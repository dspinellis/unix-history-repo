/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lofs_subr.c	8.4 (Berkeley) %G%
 *
 * $Id: lofs_subr.c,v 1.11 1992/05/30 10:05:43 jsp Exp jsp $
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <miscfs/lofs/lofs.h>

#define LOG2_SIZEVNODE 7		/* log2(sizeof struct vnode) */
#define	NLOFSCACHE 16
#define	LOFS_NHASH(vp) ((((u_long)vp)>>LOG2_SIZEVNODE) & (NLOFSCACHE-1))

/*
 * Loopback cache:
 * Each cache entry holds a reference to the target vnode
 * along with a pointer to the alias vnode.  When an
 * entry is added the target vnode is VREF'd.  When the
 * alias is removed the target vnode is vrele'd.
 */

/*
 * Cache head
 */
struct lofscache {
	struct lofsnode	*ac_forw;
	struct lofsnode	*ac_back;
};

static struct lofscache lofscache[NLOFSCACHE];

/*
 * Initialise cache headers
 */
int
lofs_init()
{
	struct lofscache *ac;

	for (ac = lofscache; ac < lofscache + NLOFSCACHE; ac++)
		ac->ac_forw = ac->ac_back = (struct lofsnode *) ac;

	return (0);
}

/*
 * Compute hash list for given target vnode
 */
static struct lofscache *
lofs_hash(targetvp)
struct vnode *targetvp;
{

	return (&lofscache[LOFS_NHASH(targetvp)]);
}

/*
 * Make a new lofsnode node.
 * Vp is the alias vnode, lofsvp is the target vnode.
 * Maintain a reference to (targetvp).
 */
static void
lofs_alloc(vp, targetvp)
	struct vnode *vp;
	struct vnode *targetvp;
{
	struct lofscache *hd;
	struct lofsnode *a;

	MALLOC(a, struct lofsnode *, sizeof(struct lofsnode), M_TEMP, M_WAITOK);
	a->a_vnode = vp;
	vp->v_data = a;
	VREF(targetvp);
	a->a_lofsvp = targetvp;
	hd = lofs_hash(targetvp);
	insque(a, hd);

}


/*
 * Return alias for target vnode if already exists, else 0.
 */
static struct lofsnode *
lofs_find(mp, targetvp)
	struct mount *mp;
	struct vnode *targetvp;
{
	struct lofscache *hd;
	struct lofsnode *a;

	/*
	 * Find hash base, and then search the (two-way) linked
	 * list looking for a lofsnode structure which is referencing
	 * the target vnode.  If found, the increment the lofsnode
	 * reference count (but NOT the target vnode's VREF counter).
	 */
	hd = lofs_hash(targetvp);

	for (a = hd->ac_forw; a != (struct lofsnode *) hd; a = a->a_forw) {
		if (a->a_lofsvp == targetvp && a->a_vnode->v_mount == mp) {
			return (a);
		}
	}

	return (0);
}

static int
lofs_alias(mp, targetvp, newvpp)
	struct mount *mp;
	struct vnode *targetvp;
	struct vnode **newvpp;
{
	struct lofsnode *ap;
	struct vnode *aliasvp;

	if (targetvp->v_type != VDIR || targetvp->v_op == lofs_vnodeop_p) {
		*newvpp = targetvp;
		return (0);
	}

	ap = lofs_find(mp, targetvp);

	if (ap) {
		/*
		 * Take another reference to the alias vnode
		 */
		aliasvp = ap->a_vnode;
		VREF(aliasvp);
	} else {
		int error;

		/*
		 * Get new vnode.
		 */
		if (error = getnewvnode(VT_LOFS, mp, lofs_vnodeop_p, &aliasvp))
			return (error);

		/*
		 * Must be a directory
		 */
		aliasvp->v_type = VDIR;

		/*
		 * Make new vnode reference the lofsnode.
		 */
		lofs_alloc(aliasvp, targetvp);

		/*
		 * aliasvp is already VREF'd by getnewvnode()
		 */
	}

	vrele(targetvp);

	*newvpp = aliasvp;
	return (0);
}

/*
 * Try to find an existing lofsnode vnode refering
 * to it, otherwise make a new lofsnode vnode which
 * contains a reference to the target vnode.
 */
int
make_lofs(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	struct vnode *targetvp;

	/*
	 * (targetvp) is locked at this point.
	 */
	targetvp = *vpp;

	/*
	 * Try to find an existing reference to the target vnodes.
	 */
	return (lofs_alias(mp, targetvp, vpp));
}

