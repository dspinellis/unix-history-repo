/*
 * Copyright (c) 1994 Jan-Simon Pendry
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)union_subr.c	1.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/vnode.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/file.h>
#include "union.h" /*<miscfs/union/union.h>*/

#ifdef DIAGNOSTIC
#include <sys/proc.h>
#endif

static struct union_node *unhead;
static int unvplock;

int
union_init()
{

	unhead = 0;
	unvplock = 0;
}

/*
 * allocate a union_node/vnode pair.  the vnode is
 * referenced and locked.  the new vnode is returned
 * via (vpp).  (mp) is the mountpoint of the union filesystem,
 * (dvp) is the parent directory where the upper layer object
 * should exist (but doesn't) and (cnp) is the componentname
 * information which is partially copied to allow the upper
 * layer object to be created at a later time.  (uppervp)
 * and (lowervp) reference the upper and lower layer objects
 * being mapped.  either, but not both, can be nil.
 *
 * all union_nodes are maintained on a singly-linked
 * list.  new nodes are only allocated when they cannot
 * be found on this list.  entries on the list are
 * removed when the vfs reclaim entry is called.
 *
 * a single lock is kept for the entire list.  this is
 * needed because the getnewvnode() function can block
 * waiting for a vnode to become free, in which case there
 * may be more than one process trying to get the same
 * vnode.  this lock is only taken if we are going to
 * call getnewvnode, since the kernel itself is single-threaded.
 *
 * if an entry is found on the list, then call vget() to
 * take a reference.  this is done because there may be
 * zero references to it and so it needs to removed from
 * the vnode free list.
 */
int
union_allocvp(vpp, mp, undvp, dvp, cnp, uppervp, lowervp)
	struct vnode **vpp;
	struct mount *mp;
	struct vnode *undvp;
	struct vnode *dvp;		/* may be null */
	struct componentname *cnp;	/* may be null */
	struct vnode *uppervp;		/* may be null */
	struct vnode *lowervp;		/* may be null */
{
	int error;
	struct union_node *un;
	struct union_node **pp;
	struct vnode *xlowervp = 0;

	if (uppervp == 0 && lowervp == 0)
		panic("union: unidentifiable allocation");

	if (uppervp && lowervp && (uppervp->v_type != lowervp->v_type)) {
		xlowervp = lowervp;
		lowervp = 0;
	}

loop:
	for (un = unhead; un != 0; un = un->un_next) {
		if ((un->un_lowervp == lowervp ||
		     un->un_lowervp == 0) &&
		    (un->un_uppervp == uppervp ||
		     un->un_uppervp == 0) &&
		    (UNIONTOV(un)->v_mount == mp)) {
			if (vget(UNIONTOV(un), 0))
				goto loop;
			if (UNIONTOV(un) != undvp)
				VOP_LOCK(UNIONTOV(un));
			if (uppervp != un->un_uppervp) {
				if (un->un_uppervp)
					vrele(un->un_uppervp);
				un->un_uppervp = uppervp;
			}
			if (lowervp != un->un_lowervp) {
				if (un->un_lowervp)
					vrele(un->un_lowervp);
				un->un_lowervp = lowervp;
			}
			*vpp = UNIONTOV(un);
			return (0);
		}
	}

	/*
	 * otherwise lock the vp list while we call getnewvnode
	 * since that can block.
	 */ 
	if (unvplock & UN_LOCKED) {
		unvplock |= UN_WANT;
		sleep((caddr_t) &unvplock, PINOD);
		goto loop;
	}
	unvplock |= UN_LOCKED;

	error = getnewvnode(VT_UNION, mp, union_vnodeop_p, vpp);
	if (error)
		goto out;

	MALLOC((*vpp)->v_data, void *, sizeof(struct union_node),
		M_TEMP, M_WAITOK);

	if (uppervp)
		(*vpp)->v_type = uppervp->v_type;
	else
		(*vpp)->v_type = lowervp->v_type;
	un = VTOUNION(*vpp);
	un->un_vnode = *vpp;
	un->un_next = 0;
	un->un_uppervp = uppervp;
	un->un_lowervp = lowervp;
	un->un_flags = 0;
	if (uppervp == 0 && cnp) {
		un->un_path = malloc(cnp->cn_namelen+1, M_TEMP, M_WAITOK);
		bcopy(cnp->cn_nameptr, un->un_path, cnp->cn_namelen);
		un->un_path[cnp->cn_namelen] = '\0';
		VREF(dvp);
		un->un_dirvp = dvp;
	} else {
		un->un_path = 0;
		un->un_dirvp = 0;
	}

	/* add to union vnode list */
	for (pp = &unhead; *pp; pp = &(*pp)->un_next)
		continue;
	*pp = un;

	un->un_flags |= UN_LOCKED;

#ifdef DIAGNOSTIC
	un->un_pid = curproc->p_pid;
#endif

	if (xlowervp)
		vrele(xlowervp);

out:
	unvplock &= ~UN_LOCKED;

	if (unvplock & UN_WANT) {
		unvplock &= ~UN_WANT;
		wakeup((caddr_t) &unvplock);
	}

	return (error);
}

int
union_freevp(vp)
	struct vnode *vp;
{
	struct union_node **unpp;
	struct union_node *un = VTOUNION(vp);

	for (unpp = &unhead; *unpp != 0; unpp = &(*unpp)->un_next) {
		if (*unpp == un) {
			*unpp = un->un_next;
			break;
		}
	}

	if (un->un_path)
		FREE(un->un_path, M_TEMP);

	FREE(vp->v_data, M_TEMP);
	vp->v_data = 0;
	return (0);
}

/*
 * copyfile.  copy the vnode (fvp) to the vnode (tvp)
 * using a sequence of reads and writes.  both (fvp)
 * and (tvp) are locked on entry and exit.
 */
int
union_copyfile(p, cred, fvp, tvp)
	struct proc *p;
	struct ucred *cred;
	struct vnode *fvp;
	struct vnode *tvp;
{
	char *buf;
	struct uio uio;
	struct iovec iov;
	int error = 0;

	/*
	 * strategy:
	 * allocate a buffer of size MAXBSIZE.
	 * loop doing reads and writes, keeping track
	 * of the current uio offset.
	 * give up at the first sign of trouble.
	 */

	uio.uio_procp = p;
	uio.uio_segflg = UIO_SYSSPACE;
	uio.uio_offset = 0;

	VOP_UNLOCK(fvp);				/* XXX */
	LEASE_CHECK(fvp, p, cred, LEASE_READ);
	VOP_LOCK(fvp);					/* XXX */
	VOP_UNLOCK(tvp);				/* XXX */
	LEASE_CHECK(tvp, p, cred, LEASE_WRITE);
	VOP_LOCK(tvp);					/* XXX */

	buf = malloc(MAXBSIZE, M_TEMP, M_WAITOK);

	/* ugly loop follows... */
	do {
		off_t offset = uio.uio_offset;

		uio.uio_iov = &iov;
		uio.uio_iovcnt = 1;
		iov.iov_base = buf;
		iov.iov_len = MAXBSIZE;
		uio.uio_resid = iov.iov_len;
		uio.uio_rw = UIO_READ;
		error = VOP_READ(fvp, &uio, 0, cred);

		if (error == 0) {
			uio.uio_iov = &iov;
			uio.uio_iovcnt = 1;
			iov.iov_base = buf;
			iov.iov_len = MAXBSIZE - uio.uio_resid;
			uio.uio_offset = offset;
			uio.uio_rw = UIO_WRITE;
			uio.uio_resid = iov.iov_len;

			if (uio.uio_resid == 0)
				break;

			do {
				error = VOP_WRITE(tvp, &uio, 0, cred);
			} while ((uio.uio_resid > 0) && (error == 0));
		}

	} while (error == 0);

	free(buf, M_TEMP);
	return (error);
}

/*
 * union_vn_create: creates and opens a new shadow file
 * on the upper union layer.  this function is similar
 * in spirit to calling vn_open but it avoids calling namei().
 * the problem with calling namei is that a) it locks too many
 * things, and b) it doesn't start at the "right" directory,
 * whereas relookup is told where to start.
 */
int
union_vn_create(vpp, un, cmode, p)
	struct vnode **vpp;
	struct union_node *un;
	int cmode;
	struct proc *p;
{
	struct vnode *vp;
	struct ucred *cred = p->p_ucred;
	struct vattr vat;
	struct vattr *vap = &vat;
	int fmode = FFLAGS(O_WRONLY|O_CREAT|O_TRUNC|O_EXCL);
	int error;
	int hash;
	char *cp;
	struct componentname cn;

	*vpp = NULLVP;

	cn.cn_namelen = strlen(un->un_path);
	cn.cn_pnbuf = (caddr_t) malloc(cn.cn_namelen, M_NAMEI, M_WAITOK);
	bcopy(un->un_path, cn.cn_pnbuf, cn.cn_namelen+1);
	cn.cn_nameiop = CREATE;
	cn.cn_flags = (LOCKLEAF|LOCKPARENT|HASBUF|SAVENAME|ISLASTCN);
	cn.cn_proc = p;
	cn.cn_cred = p->p_ucred;
	cn.cn_nameptr = cn.cn_pnbuf;
	for (hash = 0, cp = cn.cn_nameptr; *cp != 0 && *cp != '/'; cp++)
		hash += (unsigned char)*cp;
	cn.cn_hash = hash;
	cn.cn_consume = 0;

	if (error = relookup(un->un_dirvp, &vp, &cn))
		return (error);
	if (vp == NULLVP) {
		VATTR_NULL(vap);
		vap->va_type = VREG;
		vap->va_mode = cmode;
		LEASE_CHECK(un->un_dirvp, p, cred, LEASE_WRITE);
		if (error = VOP_CREATE(un->un_dirvp, &vp,
		    &cn, vap))
			return (error);
	} else {
		VOP_ABORTOP(un->un_dirvp, &cn);
		if (un->un_dirvp == vp)
			vrele(un->un_dirvp);
		else
			vput(vp);
		error = EEXIST;
		goto bad;
	}

	if (vp->v_type != VREG) {
		error = EOPNOTSUPP;
		goto bad;
	}

	VOP_UNLOCK(vp);				/* XXX */
	LEASE_CHECK(vp, p, cred, LEASE_WRITE);
	VOP_LOCK(vp);				/* XXX */
	VATTR_NULL(vap);
	vap->va_size = 0;
	if (error = VOP_SETATTR(vp, vap, cred, p))
		goto bad;

	if (error = VOP_OPEN(vp, fmode, cred, p))
		goto bad;

	vp->v_writecount++;
	*vpp = vp;
	return (0);
bad:
	vput(vp);
	return (error);
}
