/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */
/*
 *
 * Copyright (c) 2018, Intel Corporation.
 * Copyright (c) 2020 by Lawrence Livermore National Security, LLC.
 */

#include <sys/vdev_impl.h>
#include <sys/dsl_scan.h>
#include <sys/spa_impl.h>
#include <sys/metaslab_impl.h>
#include <sys/vdev_rebuild.h>
#include <sys/zio.h>
#include <sys/dmu_tx.h>
#include <sys/arc.h>
#include <sys/zap.h>

/*
 * This file contains the sequential reconstruction implementation for
 * resilvering.  This form of resilvering is internally referred to as device
 * rebuild to avoid conflating it with the traditional healing reconstruction
 * performed by the dsl scan code.
 *
 * When replacing a device, or scrubbing the pool, ZFS has historically used
 * a process called resilvering which is a form of healing reconstruction.
 * This approach has the advantage that as blocks are read from disk their
 * checksums can be immediately verified and the data repaired.  Unfortunately,
 * it also results in a random IO pattern to the disk even when extra care
 * is taken to sequentialize the IO as much as possible.  This substantially
 * increases the time required to resilver the pool and restore redundancy.
 *
 * For mirrored devices it's possible to implement an alternate sequential
 * reconstruction strategy when resilvering.  Sequential reconstruction
 * behaves like a traditional RAID rebuild and reconstructs a device in LBA
 * order without verifying the checksum.  After this phase completes a second
 * scrub phase is started to verify all of the checksums.  This two phase
 * process will take longer than the healing reconstruction described above.
 * However, it has that advantage that after the reconstruction first phase
 * completes redundancy has been restored.  At this point the pool can incur
 * another device failure without risking data loss.
 *
 * There are a few noteworthy limitations and other advantages of resilvering
 * using sequential reconstruction vs healing reconstruction.
 *
 * Limitations:
 *
 *   - Only supported for mirror vdev types.  Due to the variable stripe
 *     width used by raidz sequential reconstruction is not possible.
 *
 *   - Block checksums are not verified during sequential reconstuction.
 *     Similar to traditional RAID the parity/mirror data is reconstructed
 *     but cannot be immediately double checked.  For this reason when the
 *     last active resilver completes the pool is automatically scrubbed.
 *
 *   - Deferred resilvers using sequential reconstruction are not currently
 *     supported.  When adding another vdev to an active top-level resilver
 *     it must be restarted.
 *
 * Advantages:
 *
 *   - Sequential reconstuction is performed in LBA order which may be faster
 *     than healing reconstuction particularly when using using HDDs (or
 *     especially with SMR devices).  Only allocated capacity is resilvered.
 *
 *   - Sequential reconstruction is not constrained by ZFS block boundaries.
 *     This allows it to issue larger IOs to disk which span multiple blocks
 *     allowing all of these logical blocks to be repaired with a single IO.
 *
 *   - Unlike a healing resilver or scrub which are pool wide operations,
 *     sequential reconstruction is handled by the top-level mirror vdevs.
 *     This allows for it to be started or canceled on a top-level vdev
 *     without impacting any other top-level vdevs in the pool.
 *
 *   - Data only referenced by a pool checkpoint will be repaired because
 *     that space is reflected in the space maps.  This differs for a
 *     healing resilver or scrub which will not repair that data.
 */


/*
 * Maximum number of queued rebuild I/Os top-level vdev.  The number of
 * concurrent rebuild I/Os issued to the device is controlled by the
 * zfs_vdev_rebuild_min_active and zfs_vdev_rebuild_max_active module
 * options.
 */
unsigned int zfs_rebuild_queue_limit = 20;

/*
 * Size of rebuild reads; defaults to 1MiB and is capped at SPA_MAXBLOCKSIZE.
 */
unsigned long zfs_rebuild_max_segment = 1024 * 1024;

/*
 * For vdev_rebuild_initiate_sync() and vdev_rebuild_reset_sync().
 */
static void vdev_rebuild_thread(void *arg);

/*
 * Clear the per-vdev rebuild bytes value for a vdev tree.
 */
static void
clear_rebuild_bytes(vdev_t *vd)
{
	vdev_stat_t *vs = &vd->vdev_stat;

	for (uint64_t i = 0; i < vd->vdev_children; i++)
		clear_rebuild_bytes(vd->vdev_child[i]);

	mutex_enter(&vd->vdev_stat_lock);
	vs->vs_rebuild_processed = 0;
	mutex_exit(&vd->vdev_stat_lock);
}

/*
 * Determines whether a vdev_rebuild_thread() should be stopped.
 */
static boolean_t
vdev_rebuild_should_stop(vdev_t *vd)
{
	return (!vdev_writeable(vd) || vd->vdev_removing ||
	    vd->vdev_rebuild_exit_wanted ||
	    vd->vdev_rebuild_cancel_wanted ||
	    vd->vdev_rebuild_reset_wanted);
}

/*
 * Determine if the rebuild should be canceled.  This may happen when all
 * vdevs with MISSING DTLs are detached.
 */
static boolean_t
vdev_rebuild_should_cancel(vdev_t *vd)
{
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

	if (!vdev_resilver_needed(vd, &vrp->vrp_min_txg, &vrp->vrp_max_txg))
		return (B_TRUE);

	return (B_FALSE);
}

/*
 * The sync task for updating the on-disk state of a rebuild.  This is
 * scheduled by vdev_rebuild_range().
 */
static void
vdev_rebuild_update_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	uint64_t txg = dmu_tx_get_txg(tx);

	mutex_enter(&vd->vdev_rebuild_lock);

	if (vr->vr_scan_offset[txg & TXG_MASK] > 0) {
		vrp->vrp_last_offset = vr->vr_scan_offset[txg & TXG_MASK];
		vr->vr_scan_offset[txg & TXG_MASK] = 0;
	}

	vrp->vrp_scan_time_ms = vr->vr_prev_scan_time_ms +
	    NSEC2MSEC(gethrtime() - vr->vr_pass_start_time);

	VERIFY0(zap_update(vd->vdev_spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp, tx));

	mutex_exit(&vd->vdev_rebuild_lock);
}

/*
 * Initialize the on-disk state for a new rebuild, start the rebuild thread.
 */
static void
vdev_rebuild_initiate_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

	ASSERT(vd->vdev_rebuilding);

	spa_feature_incr(vd->vdev_spa, SPA_FEATURE_DEVICE_REBUILD, tx);

	mutex_enter(&vd->vdev_rebuild_lock);
	bzero(vrp, sizeof (uint64_t) * REBUILD_PHYS_ENTRIES);
	vrp->vrp_rebuild_state = VDEV_REBUILD_ACTIVE;
	vrp->vrp_min_txg = 0;
	vrp->vrp_max_txg = dmu_tx_get_txg(tx);
	vrp->vrp_start_time = gethrestime_sec();
	vrp->vrp_scan_time_ms = 0;
	vr->vr_prev_scan_time_ms = 0;

	/*
	 * Rebuilds are currently only used when replacing a device, in which
	 * case there must be DTL_MISSING entries.  In the future, we could
	 * allow rebuilds to be used in a way similar to a scrub.  This would
	 * be useful because it would allow us to rebuild the space used by
	 * pool checkpoints.
	 */
	VERIFY(vdev_resilver_needed(vd, &vrp->vrp_min_txg, &vrp->vrp_max_txg));

	VERIFY0(zap_update(vd->vdev_spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp, tx));

	spa_history_log_internal(spa, "rebuild", tx,
	    "vdev_id=%llu vdev_guid=%llu started",
	    (u_longlong_t)vd->vdev_id, (u_longlong_t)vd->vdev_guid);

	ASSERT3P(vd->vdev_rebuild_thread, ==, NULL);
	vd->vdev_rebuild_thread = thread_create(NULL, 0,
	    vdev_rebuild_thread, vd, 0, &p0, TS_RUN, maxclsyspri);

	mutex_exit(&vd->vdev_rebuild_lock);
}

static void
vdev_rebuild_log_notify(spa_t *spa, vdev_t *vd, char *name)
{
	nvlist_t *aux = fnvlist_alloc();

	fnvlist_add_string(aux, ZFS_EV_RESILVER_TYPE, "sequential");
	spa_event_notify(spa, vd, aux, name);
	nvlist_free(aux);
}

/*
 * Called to request that a new rebuild be started.  The feature will remain
 * active for the duration of the rebuild, then revert to the enabled state.
 */
static void
vdev_rebuild_initiate(vdev_t *vd)
{
	spa_t *spa = vd->vdev_spa;

	ASSERT(vd->vdev_top == vd);
	ASSERT(MUTEX_HELD(&vd->vdev_rebuild_lock));
	ASSERT(!vd->vdev_rebuilding);

	dmu_tx_t *tx = dmu_tx_create_dd(spa_get_dsl(spa)->dp_mos_dir);
	VERIFY0(dmu_tx_assign(tx, TXG_WAIT));

	vd->vdev_rebuilding = B_TRUE;

	dsl_sync_task_nowait(spa_get_dsl(spa), vdev_rebuild_initiate_sync,
	    (void *)(uintptr_t)vd->vdev_id, 0, ZFS_SPACE_CHECK_NONE, tx);
	dmu_tx_commit(tx);

	vdev_rebuild_log_notify(spa, vd, ESC_ZFS_RESILVER_START);
}

/*
 * Update the on-disk state to completed when a rebuild finishes.
 */
static void
vdev_rebuild_complete_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

	mutex_enter(&vd->vdev_rebuild_lock);
	vrp->vrp_rebuild_state = VDEV_REBUILD_COMPLETE;
	vrp->vrp_end_time = gethrestime_sec();

	VERIFY0(zap_update(vd->vdev_spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp, tx));

	vdev_dtl_reassess(vd,  tx->tx_txg, vrp->vrp_max_txg, B_TRUE, B_TRUE);
	spa_feature_decr(vd->vdev_spa, SPA_FEATURE_DEVICE_REBUILD, tx);

	spa_history_log_internal(spa, "rebuild",  tx,
	    "vdev_id=%llu vdev_guid=%llu complete",
	    (u_longlong_t)vd->vdev_id, (u_longlong_t)vd->vdev_guid);
	vdev_rebuild_log_notify(spa, vd, ESC_ZFS_RESILVER_FINISH);

	/* Handles detaching of spares */
	spa_async_request(spa, SPA_ASYNC_REBUILD_DONE);
	vd->vdev_rebuilding = B_FALSE;
	mutex_exit(&vd->vdev_rebuild_lock);

	spa_notify_waiters(spa);
	cv_broadcast(&vd->vdev_rebuild_cv);
}

/*
 * Update the on-disk state to canceled when a rebuild finishes.
 */
static void
vdev_rebuild_cancel_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

	mutex_enter(&vd->vdev_rebuild_lock);
	vrp->vrp_rebuild_state = VDEV_REBUILD_CANCELED;
	vrp->vrp_end_time = gethrestime_sec();

	VERIFY0(zap_update(vd->vdev_spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp, tx));

	spa_feature_decr(vd->vdev_spa, SPA_FEATURE_DEVICE_REBUILD, tx);

	spa_history_log_internal(spa, "rebuild",  tx,
	    "vdev_id=%llu vdev_guid=%llu canceled",
	    (u_longlong_t)vd->vdev_id, (u_longlong_t)vd->vdev_guid);
	vdev_rebuild_log_notify(spa, vd, ESC_ZFS_RESILVER_FINISH);

	vd->vdev_rebuild_cancel_wanted = B_FALSE;
	vd->vdev_rebuilding = B_FALSE;
	mutex_exit(&vd->vdev_rebuild_lock);

	spa_notify_waiters(spa);
	cv_broadcast(&vd->vdev_rebuild_cv);
}

/*
 * Resets the progress of a running rebuild.  This will occur when a new
 * vdev is added to rebuild.
 */
static void
vdev_rebuild_reset_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

	mutex_enter(&vd->vdev_rebuild_lock);

	ASSERT(vrp->vrp_rebuild_state == VDEV_REBUILD_ACTIVE);
	ASSERT3P(vd->vdev_rebuild_thread, ==, NULL);

	vrp->vrp_last_offset = 0;
	vrp->vrp_min_txg = 0;
	vrp->vrp_max_txg = dmu_tx_get_txg(tx);
	vrp->vrp_bytes_scanned = 0;
	vrp->vrp_bytes_issued = 0;
	vrp->vrp_bytes_rebuilt = 0;
	vrp->vrp_bytes_est = 0;
	vrp->vrp_scan_time_ms = 0;
	vr->vr_prev_scan_time_ms = 0;

	/* See vdev_rebuild_initiate_sync comment */
	VERIFY(vdev_resilver_needed(vd, &vrp->vrp_min_txg, &vrp->vrp_max_txg));

	VERIFY0(zap_update(vd->vdev_spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp, tx));

	spa_history_log_internal(spa, "rebuild",  tx,
	    "vdev_id=%llu vdev_guid=%llu reset",
	    (u_longlong_t)vd->vdev_id, (u_longlong_t)vd->vdev_guid);

	vd->vdev_rebuild_reset_wanted = B_FALSE;
	ASSERT(vd->vdev_rebuilding);

	vd->vdev_rebuild_thread = thread_create(NULL, 0,
	    vdev_rebuild_thread, vd, 0, &p0, TS_RUN, maxclsyspri);

	mutex_exit(&vd->vdev_rebuild_lock);
}

/*
 * Clear the last rebuild status.
 */
void
vdev_rebuild_clear_sync(void *arg, dmu_tx_t *tx)
{
	int vdev_id = (uintptr_t)arg;
	spa_t *spa = dmu_tx_pool(tx)->dp_spa;
	vdev_t *vd = vdev_lookup_top(spa, vdev_id);
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	objset_t *mos = spa_meta_objset(spa);

	mutex_enter(&vd->vdev_rebuild_lock);

	if (!spa_feature_is_enabled(spa, SPA_FEATURE_DEVICE_REBUILD) ||
	    vrp->vrp_rebuild_state == VDEV_REBUILD_ACTIVE) {
		mutex_exit(&vd->vdev_rebuild_lock);
		return;
	}

	clear_rebuild_bytes(vd);
	bzero(vrp, sizeof (uint64_t) * REBUILD_PHYS_ENTRIES);

	if (vd->vdev_top_zap != 0 && zap_contains(mos, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS) == 0) {
		VERIFY0(zap_update(mos, vd->vdev_top_zap,
		    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
		    REBUILD_PHYS_ENTRIES, vrp, tx));
	}

	mutex_exit(&vd->vdev_rebuild_lock);
}

/*
 * The zio_done_func_t callback for each rebuild I/O issued.  It's responsible
 * for updating the rebuild stats and limiting the number of in flight I/Os.
 */
static void
vdev_rebuild_cb(zio_t *zio)
{
	vdev_rebuild_t *vr = zio->io_private;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	vdev_t *vd = vr->vr_top_vdev;

	mutex_enter(&vd->vdev_rebuild_io_lock);
	if (zio->io_error == ENXIO && !vdev_writeable(vd)) {
		/*
		 * The I/O failed because the top-level vdev was unavailable.
		 * Attempt to roll back to the last completed offset, in order
		 * resume from the correct location if the pool is resumed.
		 * (This works because spa_sync waits on spa_txg_zio before
		 * it runs sync tasks.)
		 */
		uint64_t *off = &vr->vr_scan_offset[zio->io_txg & TXG_MASK];
		*off = MIN(*off, zio->io_offset);
	} else if (zio->io_error) {
		vrp->vrp_errors++;
	}

	abd_free(zio->io_abd);

	ASSERT3U(vd->vdev_rebuild_inflight, >, 0);
	vd->vdev_rebuild_inflight--;
	cv_broadcast(&vd->vdev_rebuild_io_cv);
	mutex_exit(&vd->vdev_rebuild_io_lock);

	spa_config_exit(vd->vdev_spa, SCL_STATE_ALL, vd);
}

/*
 * Rebuild the data in this range by constructing a special dummy block
 * pointer for the given range.  It has no relation to any existing blocks
 * in the pool.  But by disabling checksum verification and issuing a scrub
 * I/O mirrored vdevs will replicate the block using any available mirror
 * leaf vdevs.
 */
static void
vdev_rebuild_rebuild_block(vdev_rebuild_t *vr, uint64_t start, uint64_t asize,
    uint64_t txg)
{
	vdev_t *vd = vr->vr_top_vdev;
	spa_t *spa = vd->vdev_spa;
	uint64_t psize = asize;

	ASSERT(vd->vdev_ops == &vdev_mirror_ops ||
	    vd->vdev_ops == &vdev_replacing_ops ||
	    vd->vdev_ops == &vdev_spare_ops);

	blkptr_t blk, *bp = &blk;
	BP_ZERO(bp);

	DVA_SET_VDEV(&bp->blk_dva[0], vd->vdev_id);
	DVA_SET_OFFSET(&bp->blk_dva[0], start);
	DVA_SET_GANG(&bp->blk_dva[0], 0);
	DVA_SET_ASIZE(&bp->blk_dva[0], asize);

	BP_SET_BIRTH(bp, TXG_INITIAL, TXG_INITIAL);
	BP_SET_LSIZE(bp, psize);
	BP_SET_PSIZE(bp, psize);
	BP_SET_COMPRESS(bp, ZIO_COMPRESS_OFF);
	BP_SET_CHECKSUM(bp, ZIO_CHECKSUM_OFF);
	BP_SET_TYPE(bp, DMU_OT_NONE);
	BP_SET_LEVEL(bp, 0);
	BP_SET_DEDUP(bp, 0);
	BP_SET_BYTEORDER(bp, ZFS_HOST_BYTEORDER);

	/*
	 * We increment the issued bytes by the asize rather than the psize
	 * so the scanned and issued bytes may be directly compared.  This
	 * is consistent with the scrub/resilver issued reporting.
	 */
	vr->vr_pass_bytes_issued += asize;
	vr->vr_rebuild_phys.vrp_bytes_issued += asize;

	zio_nowait(zio_read(spa->spa_txg_zio[txg & TXG_MASK], spa, bp,
	    abd_alloc(psize, B_FALSE), psize, vdev_rebuild_cb, vr,
	    ZIO_PRIORITY_REBUILD, ZIO_FLAG_RAW | ZIO_FLAG_CANFAIL |
	    ZIO_FLAG_RESILVER, NULL));
}

/*
 * Issues a rebuild I/O and takes care of rate limiting the number of queued
 * rebuild I/Os.  The provided start and size must be properly aligned for the
 * top-level vdev type being rebuilt.
 */
static int
vdev_rebuild_range(vdev_rebuild_t *vr, uint64_t start, uint64_t size)
{
	uint64_t ms_id __maybe_unused = vr->vr_scan_msp->ms_id;
	vdev_t *vd = vr->vr_top_vdev;
	spa_t *spa = vd->vdev_spa;

	ASSERT3U(ms_id, ==, start >> vd->vdev_ms_shift);
	ASSERT3U(ms_id, ==, (start + size - 1) >> vd->vdev_ms_shift);

	vr->vr_pass_bytes_scanned += size;
	vr->vr_rebuild_phys.vrp_bytes_scanned += size;

	mutex_enter(&vd->vdev_rebuild_io_lock);

	/* Limit in flight rebuild I/Os */
	while (vd->vdev_rebuild_inflight >= zfs_rebuild_queue_limit)
		cv_wait(&vd->vdev_rebuild_io_cv, &vd->vdev_rebuild_io_lock);

	vd->vdev_rebuild_inflight++;
	mutex_exit(&vd->vdev_rebuild_io_lock);

	dmu_tx_t *tx = dmu_tx_create_dd(spa_get_dsl(spa)->dp_mos_dir);
	VERIFY0(dmu_tx_assign(tx, TXG_WAIT));
	uint64_t txg = dmu_tx_get_txg(tx);

	spa_config_enter(spa, SCL_STATE_ALL, vd, RW_READER);
	mutex_enter(&vd->vdev_rebuild_lock);

	/* This is the first I/O for this txg. */
	if (vr->vr_scan_offset[txg & TXG_MASK] == 0) {
		vr->vr_scan_offset[txg & TXG_MASK] = start;
		dsl_sync_task_nowait(spa_get_dsl(spa),
		    vdev_rebuild_update_sync,
		    (void *)(uintptr_t)vd->vdev_id, 2,
		    ZFS_SPACE_CHECK_RESERVED, tx);
	}

	/* When exiting write out our progress. */
	if (vdev_rebuild_should_stop(vd)) {
		mutex_enter(&vd->vdev_rebuild_io_lock);
		vd->vdev_rebuild_inflight--;
		mutex_exit(&vd->vdev_rebuild_io_lock);
		spa_config_exit(vd->vdev_spa, SCL_STATE_ALL, vd);
		mutex_exit(&vd->vdev_rebuild_lock);
		dmu_tx_commit(tx);
		return (SET_ERROR(EINTR));
	}
	mutex_exit(&vd->vdev_rebuild_lock);

	vr->vr_scan_offset[txg & TXG_MASK] = start + size;
	vdev_rebuild_rebuild_block(vr, start, size, txg);

	dmu_tx_commit(tx);

	return (0);
}

/*
 * Split range into legally-sized logical chunks given the constraints of the
 * top-level mirror vdev type.
 */
static uint64_t
vdev_rebuild_chunk_size(vdev_t *vd, uint64_t start, uint64_t size)
{
	uint64_t chunk_size, max_asize, max_segment;

	ASSERT(vd->vdev_ops == &vdev_mirror_ops ||
	    vd->vdev_ops == &vdev_replacing_ops ||
	    vd->vdev_ops == &vdev_spare_ops);

	max_segment = MIN(P2ROUNDUP(zfs_rebuild_max_segment,
	    1 << vd->vdev_ashift), SPA_MAXBLOCKSIZE);
	max_asize = vdev_psize_to_asize(vd, max_segment);
	chunk_size = MIN(size, max_asize);

	return (chunk_size);
}

/*
 * Issues rebuild I/Os for all ranges in the provided vr->vr_tree range tree.
 */
static int
vdev_rebuild_ranges(vdev_rebuild_t *vr)
{
	vdev_t *vd = vr->vr_top_vdev;
	zfs_btree_t *t = &vr->vr_scan_tree->rt_root;
	zfs_btree_index_t idx;
	int error;

	for (range_seg_t *rs = zfs_btree_first(t, &idx); rs != NULL;
	    rs = zfs_btree_next(t, &idx, &idx)) {
		uint64_t start = rs_get_start(rs, vr->vr_scan_tree);
		uint64_t size = rs_get_end(rs, vr->vr_scan_tree) - start;

		/*
		 * zfs_scan_suspend_progress can be set to disable rebuild
		 * progress for testing.  See comment in dsl_scan_sync().
		 */
		while (zfs_scan_suspend_progress &&
		    !vdev_rebuild_should_stop(vd)) {
			delay(hz);
		}

		while (size > 0) {
			uint64_t chunk_size;

			chunk_size = vdev_rebuild_chunk_size(vd, start, size);

			error = vdev_rebuild_range(vr, start, chunk_size);
			if (error != 0)
				return (error);

			size -= chunk_size;
			start += chunk_size;
		}
	}

	return (0);
}

/*
 * Calculates the estimated capacity which remains to be scanned.  Since
 * we traverse the pool in metaslab order only allocated capacity beyond
 * the vrp_last_offset need be considered.  All lower offsets must have
 * already been rebuilt and are thus already included in vrp_bytes_scanned.
 */
static void
vdev_rebuild_update_bytes_est(vdev_t *vd, uint64_t ms_id)
{
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	uint64_t bytes_est = vrp->vrp_bytes_scanned;

	if (vrp->vrp_last_offset < vd->vdev_ms[ms_id]->ms_start)
		return;

	for (uint64_t i = ms_id; i < vd->vdev_ms_count; i++) {
		metaslab_t *msp = vd->vdev_ms[i];

		mutex_enter(&msp->ms_lock);
		bytes_est += metaslab_allocated_space(msp);
		mutex_exit(&msp->ms_lock);
	}

	vrp->vrp_bytes_est = bytes_est;
}

/*
 * Load from disk the top-level vdev's rebuild information.
 */
int
vdev_rebuild_load(vdev_t *vd)
{
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	spa_t *spa = vd->vdev_spa;
	int err = 0;

	mutex_enter(&vd->vdev_rebuild_lock);
	vd->vdev_rebuilding = B_FALSE;

	if (!spa_feature_is_enabled(spa, SPA_FEATURE_DEVICE_REBUILD)) {
		bzero(vrp, sizeof (uint64_t) * REBUILD_PHYS_ENTRIES);
		mutex_exit(&vd->vdev_rebuild_lock);
		return (SET_ERROR(ENOTSUP));
	}

	ASSERT(vd->vdev_top == vd);

	err = zap_lookup(spa->spa_meta_objset, vd->vdev_top_zap,
	    VDEV_TOP_ZAP_VDEV_REBUILD_PHYS, sizeof (uint64_t),
	    REBUILD_PHYS_ENTRIES, vrp);

	/*
	 * A missing or damaged VDEV_TOP_ZAP_VDEV_REBUILD_PHYS should
	 * not prevent a pool from being imported.  Clear the rebuild
	 * status allowing a new resilver/rebuild to be started.
	 */
	if (err == ENOENT || err == EOVERFLOW || err == ECKSUM) {
		bzero(vrp, sizeof (uint64_t) * REBUILD_PHYS_ENTRIES);
	} else if (err) {
		mutex_exit(&vd->vdev_rebuild_lock);
		return (err);
	}

	vr->vr_prev_scan_time_ms = vrp->vrp_scan_time_ms;
	vr->vr_top_vdev = vd;

	mutex_exit(&vd->vdev_rebuild_lock);

	return (0);
}

/*
 * Each scan thread is responsible for rebuilding a top-level vdev.  The
 * rebuild progress in tracked on-disk in VDEV_TOP_ZAP_VDEV_REBUILD_PHYS.
 */
static void
vdev_rebuild_thread(void *arg)
{
	vdev_t *vd = arg;
	spa_t *spa = vd->vdev_spa;
	int error = 0;

	/*
	 * If there's a scrub in process request that it be stopped.  This
	 * is not required for a correct rebuild, but we do want rebuilds to
	 * emulate the resilver behavior as much as possible.
	 */
	dsl_pool_t *dsl = spa_get_dsl(spa);
	if (dsl_scan_scrubbing(dsl))
		dsl_scan_cancel(dsl);

	spa_config_enter(spa, SCL_CONFIG, FTAG, RW_READER);
	mutex_enter(&vd->vdev_rebuild_lock);

	ASSERT3P(vd->vdev_top, ==, vd);
	ASSERT3P(vd->vdev_rebuild_thread, !=, NULL);
	ASSERT(vd->vdev_rebuilding);
	ASSERT(spa_feature_is_active(spa, SPA_FEATURE_DEVICE_REBUILD));
	ASSERT3B(vd->vdev_rebuild_cancel_wanted, ==, B_FALSE);
	ASSERT3B(vd->vdev_rebuild_reset_wanted, ==, B_FALSE);

	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;
	vr->vr_top_vdev = vd;
	vr->vr_scan_msp = NULL;
	vr->vr_scan_tree = range_tree_create(NULL, RANGE_SEG64, NULL, 0, 0);
	vr->vr_pass_start_time = gethrtime();
	vr->vr_pass_bytes_scanned = 0;
	vr->vr_pass_bytes_issued = 0;

	uint64_t update_est_time = gethrtime();
	vdev_rebuild_update_bytes_est(vd, 0);

	clear_rebuild_bytes(vr->vr_top_vdev);

	mutex_exit(&vd->vdev_rebuild_lock);

	/*
	 * Systematically walk the metaslabs and issue rebuild I/Os for
	 * all ranges in the allocated space map.
	 */
	for (uint64_t i = 0; i < vd->vdev_ms_count; i++) {
		metaslab_t *msp = vd->vdev_ms[i];
		vr->vr_scan_msp = msp;

		/*
		 * Removal of vdevs from the vdev tree may eliminate the need
		 * for the rebuild, in which case it should be canceled.  The
		 * vdev_rebuild_cancel_wanted flag is set until the sync task
		 * completes.  This may be after the rebuild thread exits.
		 */
		if (vdev_rebuild_should_cancel(vd)) {
			vd->vdev_rebuild_cancel_wanted = B_TRUE;
			error = EINTR;
			break;
		}

		ASSERT0(range_tree_space(vr->vr_scan_tree));

		/*
		 * Disable any new allocations to this metaslab and wait
		 * for any writes inflight to complete.  This is needed to
		 * ensure all allocated ranges are rebuilt.
		 */
		metaslab_disable(msp);
		spa_config_exit(spa, SCL_CONFIG, FTAG);
		txg_wait_synced(dsl, 0);

		mutex_enter(&msp->ms_sync_lock);
		mutex_enter(&msp->ms_lock);

		/*
		 * When a metaslab has been allocated from read its allocated
		 * ranges from the space map object in to the vr_scan_tree.
		 * Then add inflight / unflushed ranges and remove inflight /
		 * unflushed frees.  This is the minimum range to be rebuilt.
		 */
		if (msp->ms_sm != NULL) {
			VERIFY0(space_map_load(msp->ms_sm,
			    vr->vr_scan_tree, SM_ALLOC));

			for (int i = 0; i < TXG_SIZE; i++) {
				ASSERT0(range_tree_space(
				    msp->ms_allocating[i]));
			}

			range_tree_walk(msp->ms_unflushed_allocs,
			    range_tree_add, vr->vr_scan_tree);
			range_tree_walk(msp->ms_unflushed_frees,
			    range_tree_remove, vr->vr_scan_tree);

			/*
			 * Remove ranges which have already been rebuilt based
			 * on the last offset.  This can happen when restarting
			 * a scan after exporting and re-importing the pool.
			 */
			range_tree_clear(vr->vr_scan_tree, 0,
			    vrp->vrp_last_offset);
		}

		mutex_exit(&msp->ms_lock);
		mutex_exit(&msp->ms_sync_lock);

		/*
		 * To provide an accurate estimate re-calculate the estimated
		 * size every 5 minutes to account for recent allocations and
		 * frees made space maps which have not yet been rebuilt.
		 */
		if (gethrtime() > update_est_time + SEC2NSEC(300)) {
			update_est_time = gethrtime();
			vdev_rebuild_update_bytes_est(vd, i);
		}

		/*
		 * Walk the allocated space map and issue the rebuild I/O.
		 */
		error = vdev_rebuild_ranges(vr);
		range_tree_vacate(vr->vr_scan_tree, NULL, NULL);

		spa_config_enter(spa, SCL_CONFIG, FTAG, RW_READER);
		metaslab_enable(msp, B_FALSE, B_FALSE);

		if (error != 0)
			break;
	}

	range_tree_destroy(vr->vr_scan_tree);
	spa_config_exit(spa, SCL_CONFIG, FTAG);

	/* Wait for any remaining rebuild I/O to complete */
	mutex_enter(&vd->vdev_rebuild_io_lock);
	while (vd->vdev_rebuild_inflight > 0)
		cv_wait(&vd->vdev_rebuild_io_cv, &vd->vdev_rebuild_io_lock);

	mutex_exit(&vd->vdev_rebuild_io_lock);

	spa_config_enter(spa, SCL_CONFIG, FTAG, RW_READER);

	dsl_pool_t *dp = spa_get_dsl(spa);
	dmu_tx_t *tx = dmu_tx_create_dd(dp->dp_mos_dir);
	VERIFY0(dmu_tx_assign(tx, TXG_WAIT));

	mutex_enter(&vd->vdev_rebuild_lock);
	if (error == 0) {
		/*
		 * After a successful rebuild clear the DTLs of all ranges
		 * which were missing when the rebuild was started.  These
		 * ranges must have been rebuilt as a consequence of rebuilding
		 * all allocated space.  Note that unlike a scrub or resilver
		 * the rebuild operation will reconstruct data only referenced
		 * by a pool checkpoint.  See the dsl_scan_done() comments.
		 */
		dsl_sync_task_nowait(dp, vdev_rebuild_complete_sync,
		    (void *)(uintptr_t)vd->vdev_id, 0,
		    ZFS_SPACE_CHECK_NONE, tx);
	} else if (vd->vdev_rebuild_cancel_wanted) {
		/*
		 * The rebuild operation was canceled.  This will occur when
		 * a device participating in the rebuild is detached.
		 */
		dsl_sync_task_nowait(dp, vdev_rebuild_cancel_sync,
		    (void *)(uintptr_t)vd->vdev_id, 0,
		    ZFS_SPACE_CHECK_NONE, tx);
	} else if (vd->vdev_rebuild_reset_wanted) {
		/*
		 * Reset the running rebuild without canceling and restarting
		 * it.  This will occur when a new device is attached and must
		 * participate in the rebuild.
		 */
		dsl_sync_task_nowait(dp, vdev_rebuild_reset_sync,
		    (void *)(uintptr_t)vd->vdev_id, 0,
		    ZFS_SPACE_CHECK_NONE, tx);
	} else {
		/*
		 * The rebuild operation should be suspended.  This may occur
		 * when detaching a child vdev or when exporting the pool.  The
		 * rebuild is left in the active state so it will be resumed.
		 */
		ASSERT(vrp->vrp_rebuild_state == VDEV_REBUILD_ACTIVE);
		vd->vdev_rebuilding = B_FALSE;
	}

	dmu_tx_commit(tx);

	vd->vdev_rebuild_thread = NULL;
	mutex_exit(&vd->vdev_rebuild_lock);
	spa_config_exit(spa, SCL_CONFIG, FTAG);

	cv_broadcast(&vd->vdev_rebuild_cv);

	thread_exit();
}

/*
 * Returns B_TRUE if any top-level vdev are rebuilding.
 */
boolean_t
vdev_rebuild_active(vdev_t *vd)
{
	spa_t *spa = vd->vdev_spa;
	boolean_t ret = B_FALSE;

	if (vd == spa->spa_root_vdev) {
		for (uint64_t i = 0; i < vd->vdev_children; i++) {
			ret = vdev_rebuild_active(vd->vdev_child[i]);
			if (ret)
				return (ret);
		}
	} else if (vd->vdev_top_zap != 0) {
		vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
		vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

		mutex_enter(&vd->vdev_rebuild_lock);
		ret = (vrp->vrp_rebuild_state == VDEV_REBUILD_ACTIVE);
		mutex_exit(&vd->vdev_rebuild_lock);
	}

	return (ret);
}

/*
 * Start a rebuild operation.  The rebuild may be restarted when the
 * top-level vdev is currently actively rebuilding.
 */
void
vdev_rebuild(vdev_t *vd)
{
	vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
	vdev_rebuild_phys_t *vrp __maybe_unused = &vr->vr_rebuild_phys;

	ASSERT(vd->vdev_top == vd);
	ASSERT(vdev_is_concrete(vd));
	ASSERT(!vd->vdev_removing);
	ASSERT(spa_feature_is_enabled(vd->vdev_spa,
	    SPA_FEATURE_DEVICE_REBUILD));

	mutex_enter(&vd->vdev_rebuild_lock);
	if (vd->vdev_rebuilding) {
		ASSERT3U(vrp->vrp_rebuild_state, ==, VDEV_REBUILD_ACTIVE);

		/*
		 * Signal a running rebuild operation that it should restart
		 * from the beginning because a new device was attached.  The
		 * vdev_rebuild_reset_wanted flag is set until the sync task
		 * completes.  This may be after the rebuild thread exits.
		 */
		if (!vd->vdev_rebuild_reset_wanted)
			vd->vdev_rebuild_reset_wanted = B_TRUE;
	} else {
		vdev_rebuild_initiate(vd);
	}
	mutex_exit(&vd->vdev_rebuild_lock);
}

static void
vdev_rebuild_restart_impl(vdev_t *vd)
{
	spa_t *spa = vd->vdev_spa;

	if (vd == spa->spa_root_vdev) {
		for (uint64_t i = 0; i < vd->vdev_children; i++)
			vdev_rebuild_restart_impl(vd->vdev_child[i]);

	} else if (vd->vdev_top_zap != 0) {
		vdev_rebuild_t *vr = &vd->vdev_rebuild_config;
		vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

		mutex_enter(&vd->vdev_rebuild_lock);
		if (vrp->vrp_rebuild_state == VDEV_REBUILD_ACTIVE &&
		    vdev_writeable(vd) && !vd->vdev_rebuilding) {
			ASSERT(spa_feature_is_active(spa,
			    SPA_FEATURE_DEVICE_REBUILD));
			vd->vdev_rebuilding = B_TRUE;
			vd->vdev_rebuild_thread = thread_create(NULL, 0,
			    vdev_rebuild_thread, vd, 0, &p0, TS_RUN,
			    maxclsyspri);
		}
		mutex_exit(&vd->vdev_rebuild_lock);
	}
}

/*
 * Conditionally restart all of the vdev_rebuild_thread's for a pool.  The
 * feature flag must be active and the rebuild in the active state.   This
 * cannot be used to start a new rebuild.
 */
void
vdev_rebuild_restart(spa_t *spa)
{
	ASSERT(MUTEX_HELD(&spa_namespace_lock));

	vdev_rebuild_restart_impl(spa->spa_root_vdev);
}

/*
 * Stop and wait for all of the vdev_rebuild_thread's associated with the
 * vdev tree provide to be terminated (canceled or stopped).
 */
void
vdev_rebuild_stop_wait(vdev_t *vd)
{
	spa_t *spa = vd->vdev_spa;

	ASSERT(MUTEX_HELD(&spa_namespace_lock));

	if (vd == spa->spa_root_vdev) {
		for (uint64_t i = 0; i < vd->vdev_children; i++)
			vdev_rebuild_stop_wait(vd->vdev_child[i]);

	} else if (vd->vdev_top_zap != 0) {
		ASSERT(vd == vd->vdev_top);

		mutex_enter(&vd->vdev_rebuild_lock);
		if (vd->vdev_rebuild_thread != NULL) {
			vd->vdev_rebuild_exit_wanted = B_TRUE;
			while (vd->vdev_rebuilding) {
				cv_wait(&vd->vdev_rebuild_cv,
				    &vd->vdev_rebuild_lock);
			}
			vd->vdev_rebuild_exit_wanted = B_FALSE;
		}
		mutex_exit(&vd->vdev_rebuild_lock);
	}
}

/*
 * Stop all rebuild operations but leave them in the active state so they
 * will be resumed when importing the pool.
 */
void
vdev_rebuild_stop_all(spa_t *spa)
{
	vdev_rebuild_stop_wait(spa->spa_root_vdev);
}

/*
 * Rebuild statistics reported per top-level vdev.
 */
int
vdev_rebuild_get_stats(vdev_t *tvd, vdev_rebuild_stat_t *vrs)
{
	spa_t *spa = tvd->vdev_spa;

	if (!spa_feature_is_enabled(spa, SPA_FEATURE_DEVICE_REBUILD))
		return (SET_ERROR(ENOTSUP));

	if (tvd != tvd->vdev_top || tvd->vdev_top_zap == 0)
		return (SET_ERROR(EINVAL));

	int error = zap_contains(spa_meta_objset(spa),
	    tvd->vdev_top_zap, VDEV_TOP_ZAP_VDEV_REBUILD_PHYS);

	if (error == ENOENT) {
		bzero(vrs, sizeof (vdev_rebuild_stat_t));
		vrs->vrs_state = VDEV_REBUILD_NONE;
		error = 0;
	} else if (error == 0) {
		vdev_rebuild_t *vr = &tvd->vdev_rebuild_config;
		vdev_rebuild_phys_t *vrp = &vr->vr_rebuild_phys;

		mutex_enter(&tvd->vdev_rebuild_lock);
		vrs->vrs_state = vrp->vrp_rebuild_state;
		vrs->vrs_start_time = vrp->vrp_start_time;
		vrs->vrs_end_time = vrp->vrp_end_time;
		vrs->vrs_scan_time_ms = vrp->vrp_scan_time_ms;
		vrs->vrs_bytes_scanned = vrp->vrp_bytes_scanned;
		vrs->vrs_bytes_issued = vrp->vrp_bytes_issued;
		vrs->vrs_bytes_rebuilt = vrp->vrp_bytes_rebuilt;
		vrs->vrs_bytes_est = vrp->vrp_bytes_est;
		vrs->vrs_errors = vrp->vrp_errors;
		vrs->vrs_pass_time_ms = NSEC2MSEC(gethrtime() -
		    vr->vr_pass_start_time);
		vrs->vrs_pass_bytes_scanned = vr->vr_pass_bytes_scanned;
		vrs->vrs_pass_bytes_issued = vr->vr_pass_bytes_issued;
		mutex_exit(&tvd->vdev_rebuild_lock);
	}

	return (error);
}

/* BEGIN CSTYLED */
ZFS_MODULE_PARAM(zfs, zfs_, rebuild_max_segment, ULONG, ZMOD_RW,
        "Max segment size in bytes of rebuild reads");
/* END CSTYLED */
