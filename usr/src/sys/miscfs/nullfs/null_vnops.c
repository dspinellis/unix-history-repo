/*
 * Copyright (c) 1992 The Regents of the University of California
 * All rights reserved.
 *
 * This code is derived from the null layer of
 * John Heidemann of the UCLA Ficus project and
 * the Jan-Simon Pendry's loopback file system.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)null_vnops.c	1.3 (Berkeley) %G%
 *
 * Ancestors:
 *	@(#)lofs_vnops.c	1.2 (Berkeley) 6/18/92
 *	$Id: lofs_vnops.c,v 1.11 1992/05/30 10:05:43 jsp Exp jsp $
 *	...and...
 *	@(#)null_vnodeops.c 1.20 92/07/07 UCLA Ficus project
 */

/*
 * Null Layer
 *
 * The null layer duplicates a portion of the file system
 * name space under a new name.  In this respect, it is
 * similar to the loopback file system.  It differs from
 * the loopback fs in two respects:  it is implemented using
 * a bypass operation, and it's "null-nodes" stack above
 * all lower-layer vnodes, not just over directory vnodes.
 *
 * The null layer is the minimum file system layer,
 * simply bypassing all possible operations to the lower layer
 * for processing there.  All but vop_getattr, _inactive, _reclaim,
 * and _print are bypassed.
 *
 * Vop_getattr is not bypassed so that we can change the fsid being
 * returned.  Vop_{inactive,reclaim} are bypassed so that
 * they can handle freeing null-layer specific data.
 * Vop_print is not bypassed for debugging.
 *
 * NEEDSWORK: Describe methods to invoke operations on the lower layer
 * (bypass vs. VOP).
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <lofs/lofs.h>


int null_bug_bypass = 0;   /* for debugging: enables bypass printf'ing */

/*
 * This is the 10-Apr-92 bypass routine.
 *    This version has been optimized for speed, throwing away some
 * safety checks.  It should still always work, but it's not as
 * robust to programmer errors.
 *    Define SAFETY to include some error checking code.
 *
 * In general, we map all vnodes going down and unmap them on the way back.
 * As an exception to this, vnodes can be marked "unmapped" by setting
 * the Nth bit in operation's vdesc_flags.
 *
 * Also, some BSD vnode operations have the side effect of vrele'ing
 * their arguments.  With stacking, the reference counts are held
 * by the upper node, not the lower one, so we must handle these
 * side-effects here.  This is not of concern in Sun-derived systems
 * since there are no such side-effects.
 *
 * This makes the following assumptions:
 * - only one returned vpp
 * - no INOUT vpp's (Sun's vop_open has one of these)
 * - the vnode operation vector of the first vnode should be used
 *   to determine what implementation of the op should be invoked
 * - all mapped vnodes are of our vnode-type (NEEDSWORK:
 *   problems on rmdir'ing mount points and renaming?)
 */ 
int
null_bypass(ap)
	struct nvop_generic_args *ap;
{
	register int this_vp_p;
	int error;
	struct vnode *old_vps[VDESC_MAX_VPS];
	struct vnode **vps_p[VDESC_MAX_VPS];
	struct vnode ***vppp;
	struct vnodeop_desc *descp = ap->a_desc;
	int maps, reles, i;

	if (null_bug_bypass)
		printf ("null_bypass: %s\n", descp->vdesc_name);

#ifdef SAFETY
	/*
	 * We require at least one vp.
	 */
	if (descp->vdesc_vp_offsets==NULL ||
	    descp->vdesc_vp_offsets[0]==VDESC_NO_OFFSET)
		panic ("null_bypass: no vp's in map.\n");
#endif

	/*
	 * Map the vnodes going in.
	 * Later, we'll invoke the operation based on
	 * the first mapped vnode's operation vector.
	 */
	maps = descp->vdesc_flags;
	reles = descp->vdesc_rele_flags;
	for (i=0; i<VDESC_MAX_VPS; maps>>=1, reles>>=1, i++) {
		if (descp->vdesc_vp_offsets[i]==VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		if (maps & 1)   /* skip vps that aren't to be mapped */
			continue;
		vps_p[i] = this_vp_p = 
			VOPARG_OFFSETTO(struct vnode**,descp->vdesc_vp_offsets[i],ap);
		old_vps[i] = *this_vp_p;
		*(vps_p[i]) = NULLTOLOWERVP(VTONULLNODE(*this_vp_p));
		if (reles & 1)
			VREF(*this_vp_p);
			
	};

	/*
	 * Call the operation on the lower layer
	 * with the modified argument structure.
	 */
	error = VCALL(*(vps_p[0]), descp->vdesc_offset, ap);

	/*
	 * Maintain the illusion of call-by-value
	 * by restoring vnodes in the argument structure
	 * to their original value.
	 */
	maps = descp->vdesc_flags;
	reles = descp->vdesc_rele_flags;
	for (i=0; i<VDESC_MAX_VPS; maps>>=1, i++) {
		if (descp->vdesc_vp_offsets[i]==VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		if (maps & 1)   /* skip vps that aren't to be mapped */
			continue;
		*(vps_p[i]) = old_vps[i];
		if (reles & 1)
			vrele(*(vps_p[i]));
	};

	/*
	 * Map the possible out-going vpp.
	 */
	if (descp->vdesc_vpp_offset != VDESC_NO_OFFSET &&
	    !(descp->vdesc_flags & VDESC_NOMAP_VPP) &&
	    !error) {
		vppp=VOPARG_OFFSETTO(struct vnode***,
				 descp->vdesc_vpp_offset,ap);
		error = make_null_node(old_vps[0]->v_mount, **vppp, *vppp);
	};

	return (error);
}


/*
 *  We handle getattr to change the fsid.
 */
int
null_getattr(ap)
	struct nvop_getattr_args *ap;
{
	int error;
	if (error=null_bypass(ap))
		return error;
	/* Requires that arguments be restored. */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];
	return 0;
}


#if 0
null_rename (ap)
	struct vop_rename_args *ap;
{
	USES_VOP_RENAME;
	struct vnode *fvp, *tvp;
	struct vnode *tdvp;
#if 0
	struct vnode *fsvp, *tsvp;
#endif
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename(fdvp = %x->%x)\n", ap->a_fdvp, NULLTOLOWERVP(ap->a_fdvp));
	/*printf("null_rename(tdvp = %x->%x)\n", tndp->ni_dvp, NULLTOLOWERVP(tndp->ni_dvp));*/
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source dvp\n");
#endif
	/*
	 * Switch source directory to point to lofsed vnode
	 */
	PUSHREF(fdvp, ap->a_fdvp);
	VREF(ap->a_fdvp);

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source vp\n");
#endif
	/*
	 * And source object if it is lofsed...
	 */
	fvp = ap->a_fvp;
	if (fvp && fvp->v_op == null_vnodeop_p) {
		ap->a_fvp = NULLTOLOWERVP(fvp);
		VREF(ap->a_fvp);
	} else {
		fvp = 0;
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source start vp\n");
#endif
	/*
	 * And source startdir object if it is lofsed...
	 */
	fsvp = fndp->ni_startdir;
	if (fsvp && fsvp->v_op == null_vnodeop_p) {
		fndp->ni_startdir = NULLTOLOWERVP(fsvp);
		VREF(fndp->ni_startdir);
	} else {
		fsvp = 0;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target dvp\n");
#endif
	/*
 	 * Switch target directory to point to lofsed vnode
	 */
	tdvp = ap->a_tdvp;
	if (tdvp && tdvp->v_op == null_vnodeop_p) {
		ap->a_tdvp = NULLTOLOWERVP(tdvp);
		VREF(ap->a_tdvp);
	} else {
		tdvp = 0;
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target vp\n");
#endif
	/*
	 * And target object if it is lofsed...
	 */
	tvp = ap->a_tvp;
	if (tvp && tvp->v_op == null_vnodeop_p) {
		ap->a_tvp = NULLTOLOWERVP(tvp);
		VREF(ap->a_tvp);
	} else {
		tvp = 0;
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target start vp\n");
#endif
	/*
	 * And target startdir object if it is lofsed...
	 */
	tsvp = tndp->ni_startdir;
	if (tsvp && tsvp->v_op == null_vnodeop_p) {
		tndp->ni_startdir = NULLTOLOWERVP(fsvp);
		VREF(tndp->ni_startdir);
	} else {
		tsvp = 0;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - VOP_RENAME(%x, %x, %x, %x)\n",
		ap->a_fdvp, ap->a_fvp, ap->a_tdvp, ap->a_tvp);
	vprint("ap->a_fdvp", ap->a_fdvp);
	vprint("ap->a_fvp", ap->a_fvp);
	vprint("ap->a_tdvp", ap->a_tdvp);
	if (ap->a_tvp) vprint("ap->a_tvp", ap->a_tvp);
	DELAY(16000000);
#endif

	error = VOP_RENAME(ap->a_fdvp, ap->a_fvp, ap->a_fcnp, ap->a_tdvp, ap->a_tvp, ap->a_tcnp);

	/*
	 * Put everything back...
	 */
 
#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target startdir\n");
#endif

	if (tsvp) {
		if (tndp->ni_startdir)
			vrele(tndp->ni_startdir);
		tndp->ni_startdir = tsvp;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target vp\n");
#endif

	if (tvp) {
		ap->a_tvp = tvp;
		vrele(ap->a_tvp);
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target dvp\n");
#endif

	if (tdvp) {
		ap->a_tdvp = tdvp;
		vrele(ap->a_tdvp);
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source startdir\n");
#endif

	if (fsvp) {
		if (fndp->ni_startdir)
			vrele(fndp->ni_startdir);
		fndp->ni_startdir = fsvp;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source vp\n");
#endif


	if (fvp) {
		ap->a_fvp = fvp;
		vrele(ap->a_fvp);
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source dvp\n");
#endif

	POP(fdvp, ap->a_fdvp);
	vrele(ap->a_fdvp);

	return (error);
}
#endif


int
null_inactive (ap)
	struct vop_inactive_args *ap;
{
#ifdef NULLFS_DIAGNOSTIC
	printf("null_inactive(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif
	/*
	 * Do nothing (and _don't_ bypass).
	 * Wait to vrele lowervp until reclaim,
	 * so that until then our null_node is in the
	 * cache and reusable.
	 *
	 * NEEDSWORK: Someday, consider inactive'ing
	 * the lowervp and then trying to reactivate it
	 * like they do in the name lookup cache code.
	 * That's too much work for now.
	 */
	return 0;
}

null_reclaim (ap)
	struct vop_reclaim_args *ap;
{
	USES_VOP_RECLAIM;
	struct vnode *targetvp;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_reclaim(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif
	remque(VTONULLNODE(ap->a_vp));   /* NEEDSWORK: What? */
	vrele (NULLTOLOWERVP(ap->a_vp));   /* release lower layer */
	FREE(ap->a_vp->v_data, M_TEMP);
	ap->a_vp->v_data = 0;
	return (0);
}

null_bmap (ap)
	struct vop_bmap_args *ap;
{
	USES_VOP_BMAP;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_bmap(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_BMAP(NULLTOLOWERVP(ap->a_vp), ap->a_bn, ap->a_vpp, ap->a_bnp);
}

null_strategy (ap)
	struct vop_strategy_args *ap;
{
	USES_VOP_STRATEGY;
	int error;
	struct vnode *savedvp;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_strategy(vp = %x->%x)\n", ap->a_bp->b_vp, NULLTOLOWERVP(ap->a_bp->b_vp));
#endif

	savedvp = ap->a_bp->b_vp;

	error = VOP_STRATEGY(ap->a_bp);

	ap->a_bp->b_vp = savedvp;

	return error;
}


int
null_print (ap)
	struct vop_print_args *ap;
{
	register struct vnode *vp = ap->a_vp;
	printf ("tag VT_NULLFS, vp=%x, lowervp=%x\n", vp, NULLTOLOWERVP(vp));
	return 0;
}


/*
 * Global vfs data structures
 */
/*
 * NEEDSWORK: strategy,bmap are hand coded currently.  They should
 * go away with a merged buffer/block cache.
 *
 */
int (**null_vnodeop_p)();
struct vnodeopv_entry_desc lofs_vnodeop_entries[] = {
	{ &vop_default_desc, null_bypass },

	{ &vop_getattr_desc, null_getattr },
	{ &vop_inactive_desc, null_inactive },
	{ &vop_reclaim_desc, null_reclaim },
	{ &vop_print_desc, null_print },

	{ &vop_bmap_desc, null_bmap },
	{ &vop_strategy_desc, null_strategy },

	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc lofs_vnodeop_opv_desc =
	{ &null_vnodeop_p, lofs_vnodeop_entries };
