/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)vfs_bio.c	7.40 (Berkeley) 5/8/91
 */

#include "param.h"
#include "proc.h"
#include "buf.h"
#include "vnode.h"
#include "specdev.h"
#include "mount.h"
#include "trace.h"
#include "resourcevar.h"

/*
 * Initialize buffers and hash links for buffers.
 */
bufinit()
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Find the block in the buffer pool.
 * If the buffer is not present, allocate a new buffer and load
 * its contents according to the filesystem fill routine.
 */
bread(vp, blkno, size, cred, bpp)
	struct vnode *vp;
	daddr_t blkno;
	int size;
	struct ucred *cred;
	struct buf **bpp;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

/*
 * Operates like bread, but also starts I/O on the specified
 * read-ahead block.
 */
breada(vp, blkno, size, rablkno, rabsize, cred, bpp)
	struct vnode *vp;
	daddr_t blkno; int size;
	daddr_t rablkno; int rabsize;
	struct ucred *cred;
	struct buf **bpp;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

/*
 * Synchronous write.
 * Release buffer on completion.
 */
bwrite(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

/*
 * Delayed write.
 *
 * The buffer is marked dirty, but is not queued for I/O.
 * This routine should be used when the buffer is expected
 * to be modified again soon, typically a small write that
 * partially fills a buffer.
 *
 * NB: magnetic tapes cannot be delayed; they must be
 * written in the order that the writes are requested.
 */
bdwrite(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Asynchronous write.
 * Start I/O on a buffer, but do not wait for it to complete.
 * The buffer is released when the I/O completes.
 */
bawrite(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Release a buffer.
 * Even if the buffer is dirty, no I/O is started.
 */
brelse(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Check to see if a block is currently memory resident.
 */
incore(vp, blkno)
	struct vnode *vp;
	daddr_t blkno;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Check to see if a block is currently memory resident.
 * If it is resident, return it. If it is not resident,
 * allocate a new buffer and assign it to the block.
 */
struct buf *
getblk(vp, blkno, size)
	register struct vnode *vp;
	daddr_t blkno;
	int size;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Allocate a buffer.
 * The caller will assign it to a block.
 */
struct buf *
geteblk(size)
	int size;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Expand or contract the actual memory allocated to a buffer.
 * If no memory is available, release buffer and take error exit.
 */
allocbuf(tp, size)
	register struct buf *tp;
	int size;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Find a buffer which is available for use.
 * Select something from a free list.
 * Preference is to AGE list, then LRU list.
 */
struct buf *
getnewbuf()
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Wait for I/O to complete.
 *
 * Extract and return any errors associated with the I/O.
 * If the error flag is set, but no specific error is
 * given, return EIO.
 */
biowait(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

/*
 * Mark I/O complete on a buffer.
 *
 * If a callback has been requested, e.g. the pageout
 * daemon, do so. Otherwise, awaken waiting processes.
 */
biodone(bp)
	register struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	 return;
}
