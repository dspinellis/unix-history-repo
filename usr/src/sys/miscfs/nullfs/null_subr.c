/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lofs_subr.c	1.2 (Berkeley) 6/18/92
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
#include <lofs/lofs.h>

#define LOG2_SIZEVNODE 7		/* log2(sizeof struct vnode) */
#define	NNULLNODECACHE 16
#define	NULL_NHASH(vp) ((((u_long)vp)>>LOG2_SIZEVNODE) & (NNULLNODECACHE-1))

/*
 * Null layer cache:
 * Each cache entry holds a reference to the target vnode
 * along with a pointer to the alias vnode.  When an
 * entry is added the target vnode is VREF'd.  When the
 * alias is removed the target vnode is vrele'd.
 */

/*
 * Cache head
 */
struct null_node_cache {
	struct null_node	*ac_forw;
	struct null_node	*ac_back;
};

static struct null_node_cache null_node_cache[NNULLNODECACHE];

/*
 * Initialise cache headers
 */
nullfs_init()
{
	struct null_node_cache *ac;
#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_init\n");		/* printed during system boot */
#endif

	for (ac = null_node_cache; ac < null_node_cache + NNULLNODECACHE; ac++)
		ac->ac_forw = ac->ac_back = (struct null_node *) ac;
}

/*
 * Compute hash list for given target vnode
 */
static struct null_node_cache *
null_node_hash(targetvp)
struct vnode *targetvp;
{
	return (&null_node_cache[NULL_NHASH(targetvp)]);
}

/*
 * Make a new null_node node.
 * Vp is the alias vnode, lofsvp is the target vnode.
 * Maintain a reference to (targetvp).
 */
static void
null_node_alloc(vp, targetvp)
	struct vnode *vp;
	struct vnode *targetvp;
{
	struct null_node_cache *hd;
	struct null_node *a;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_node_alloc(%x, %x)\n", vp, targetvp);
#endif

	MALLOC(a, struct null_node *, sizeof(struct null_node), M_TEMP, M_WAITOK);
	a->null_vnode = vp;
	vp->v_data = a;
	VREF(targetvp);
	a->null_lowervp = targetvp;
	hd = null_node_hash(targetvp);
	insque(a, hd);

#ifdef NULLFS_DIAGNOSTIC
	vprint("alloc vp", vp);
	vprint("alloc targetvp", targetvp);
#endif
}

#ifdef NULLFS_DIAGNOSTIC
/*
 * NEEDSWORK:  The ability to set lowervp to null here
 * implies that one can never count on lowervp staying null
 * (even if vp is locked).  This seems quite bad.  Think
 * about these things.
 */
void
null_node_flushmp (mp)
	struct mount *mp;
{
	struct null_node_cache *ac;
	int i = 0;
	struct null_node *roota;

	printf("null_node_flushmp (%x)\n", mp);

	roota = VTONULLNODE(MOUNTTONULLMOUNT(mp)->nullm_rootvp);

	for (ac = null_node_cache; ac < null_node_cache + NNULLNODECACHE; ac++) {
		struct null_node *a = ac->ac_forw;
		while (a != (struct null_node *) ac) {
			if (a != roota && a->null_vnode->v_mount == mp) {
				struct vnode *vp = a->null_lowervp;
				if (vp) {
					a->null_lowervp = 0;
					vprint("would vrele", vp);
					/*vrele(vp);*/
					i++;
				}
			}
			a = a->null_forw;
		}
	}
	if (i > 0)
		printf("null_node: vrele'd %d aliases\n", i);
}
#endif

/*
 * Return alias for target vnode if already exists, else 0.
 */
static struct null_node *
null_node_find(mp, targetvp)
	struct mount *mp;
	struct vnode *targetvp;
{
	struct null_node_cache *hd;
	struct null_node *a;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_node_find(mp = %x, target = %x)\n", mp, targetvp);
#endif

	/*
	 * Find hash base, and then search the (two-way) linked
	 * list looking for a null_node structure which is referencing
	 * the target vnode.  If found, the increment the null_node
	 * reference count (but NOT the target vnode's VREF counter).
	 */
	hd = null_node_hash(targetvp);

	for (a = hd->ac_forw; a != (struct null_node *) hd; a = a->null_forw) {
		if (a->null_lowervp == targetvp && a->null_vnode->v_mount == mp) {
#ifdef NULLFS_DIAGNOSTIC
			printf("null_node_find(%x): found (%x,%x)->%x\n",
				targetvp, mp, a->null_vnode, targetvp);
#endif
			return (a);
		}
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_node_find(%x, %x): NOT found\n", mp, targetvp);
#endif

	return (0);
}

/*
 * Try to find an existing null_node vnode refering
 * to it, otherwise make a new null_node vnode which
 * contains a reference to the target vnode.
 */
int
null_node_create(mp, targetvp, newvpp)
	struct mount *mp;
	struct vnode *targetvp;
	struct vnode **newvpp;
{
	struct null_node *ap;
	struct vnode *aliasvp;

	if (targetvp->v_type != VDIR || targetvp->v_op == null_vnodeop_p) {
		*newvpp = targetvp;
		return;
	}

	ap = null_node_find(mp, targetvp);

	if (ap) {
		/*
		 * Take another reference to the alias vnode
		 */
#ifdef NULLFS_DIAGNOSTIC
		vprint("null_node_alias: exists", ap->null_vnode);
#endif
		aliasvp = ap->null_vnode;
		VREF(aliasvp);
	} else {
		int error;

		/*
		 * Get new vnode.
		 */
#ifdef NULLFS_DIAGNOSTIC
		printf("null_node_alias: create new alias vnode\n");
#endif
		if (error = getnewvnode(VT_UFS, mp, null_vnodeop_p, &aliasvp))
			return (error);	/* XXX: VT_LOFS above */

		/*
		 * Must be a directory
		 */
		aliasvp->v_type = VDIR;

		/*
		 * Make new vnode reference the null_node.
		 */
		null_node_alloc(aliasvp, targetvp);

		/*
		 * aliasvp is already VREF'd by getnewvnode()
		 */
	}

	vrele(targetvp);

#ifdef NULLFS_DIAGNOSTIC
	vprint("null_node_alias alias", aliasvp);
	vprint("null_node_alias target", targetvp);
#endif

	*newvpp = aliasvp;
	return (0);
}
#ifdef NULLFS_DIAGNOSTIC
struct vnode *
null_checkvp(vp, fil, lno)
	struct vnode *vp;
	char *fil;
	int lno;
{
	struct null_node *a = VTONULLNODE(vp);
	if (a->null_lowervp == 0) {
		/* Should never happen */
		int i; u_long *p;
		printf("vp = %x, ZERO ptr\n", vp);
		for (p = (u_long *) a, i = 0; i < 8; i++)
			printf(" %x", p[i]);
		printf("\n");
		DELAY(2000000);
		panic("null_checkvp");
	}
	printf("nullvp %x/%d -> %x/%d [%s, %d]\n",
		a->null_vnode, a->null_vnode->v_usecount,
		a->null_lowervp, a->null_lowervp->v_usecount,
		fil, lno);
	return a->null_lowervp;
}
#endif
