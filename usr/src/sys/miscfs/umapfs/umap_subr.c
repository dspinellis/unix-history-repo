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
#include <umapfs/umap.h>

#define LOG2_SIZEVNODE 7		/* log2(sizeof struct vnode) */
#define	NUMAPNODECACHE 16
#define	UMAP_NHASH(vp) ((((u_long)vp)>>LOG2_SIZEVNODE) & (NUMAPNODECACHE-1))

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
struct umap_node_cache {
	struct umap_node	*ac_forw;
	struct umap_node	*ac_back;
};

static struct umap_node_cache umap_node_cache[NUMAPNODECACHE];

/*
 * Initialise cache headers
 */
umapfs_init()
{
	struct umap_node_cache *ac;
#ifdef UMAPFS_DIAGNOSTIC
	printf("umapfs_init\n");		/* printed during system boot */
#endif

	for (ac = umap_node_cache; ac < umap_node_cache + NUMAPNODECACHE; ac++)
		ac->ac_forw = ac->ac_back = (struct umap_node *) ac;
}

/*
 * Compute hash list for given target vnode
 */
static struct umap_node_cache *
umap_node_hash(targetvp)
struct vnode *targetvp;
{
	return (&umap_node_cache[UMAP_NHASH(targetvp)]);
}

/*
 * Make a new umap_node node.
 * Vp is the alias vnode, lofsvp is the target vnode.
 * Maintain a reference to (targetvp).
 */
static void
umap_node_alloc(vp, targetvp)
	struct vnode *vp;
	struct vnode *targetvp;
{
	struct umap_node_cache *hd;
	struct umap_node *a;

#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_node_alloc(%x, %x)\n", vp, targetvp);
#endif

	MALLOC(a, struct umap_node *, sizeof(struct umap_node), M_TEMP, M_WAITOK);
	vp->v_type = targetvp->v_type;
	a->umap_vnode = vp;
	vp->v_data = a;
	VREF(targetvp);   /* Extra VREF will be vrele'd in umap_node_create */
	a->umap_lowervp = targetvp;
	hd = umap_node_hash(targetvp);
	insque(a, hd);

#ifdef UMAPFS_DIAGNOSTIC
	vprint("umap_node_alloc vp", vp);
	vprint("umap_node_alloc targetvp", targetvp);
#endif
}

#ifdef UMAPFS_DIAGNOSTIC
/*
 * NEEDSWORK:  The ability to set lowervp to umap here
 * implies that one can never count on lowervp staying umap
 * (even if vp is locked).  This seems quite bad.  Think
 * about these things.
 */
void
umap_node_flushmp (mp)
	struct mount *mp;
{
	struct umap_node_cache *ac;
	int i = 0;
	struct umap_node *roota;

	printf("umap_node_flushmp (%x)\n", mp);

	roota = VTOUMAP(MOUNTTOUMAPMOUNT(mp)->umapm_rootvp);

	for (ac = umap_node_cache; ac < umap_node_cache + NUMAPNODECACHE; ac++) {
		struct umap_node *a = ac->ac_forw;
		while (a != (struct umap_node *) ac) {
			if (a != roota && a->umap_vnode->v_mount == mp) {
				struct vnode *vp = a->umap_lowervp;
				if (vp) {
					a->umap_lowervp = 0;
					vprint("umap_flushmp: would vrele", vp);
					/*vrele(vp);*/
					i++;
				}
			}
			a = a->umap_forw;
		}
	}
	if (i > 0)
		printf("umap_node: vrele'd %d aliases\n", i);
}
#endif

/*
 * Return alias for target vnode if already exists, else 0.
 */
static struct umap_node *
umap_node_find(mp, targetvp)
	struct mount *mp;
	struct vnode *targetvp;
{
	struct umap_node_cache *hd;
	struct umap_node *a;

#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_node_find(mp = %x, target = %x)\n", mp, targetvp);
#endif

	/*
	 * Find hash base, and then search the (two-way) linked
	 * list looking for a umap_node structure which is referencing
	 * the target vnode.  If found, the increment the umap_node
	 * reference count (but NOT the target vnode's VREF counter).
	 */
	hd = umap_node_hash(targetvp);

	for (a = hd->ac_forw; a != (struct umap_node *) hd; a = a->umap_forw) {
		if (a->umap_lowervp == targetvp && a->umap_vnode->v_mount == mp) {
#ifdef UMAPFS_DIAGNOSTIC
			printf("umap_node_find(%x): found (%x,%x)->%x\n",
				targetvp, mp, a->umap_vnode, targetvp);
#endif
			return (a);
		}
	}

#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_node_find(%x, %x): NOT found\n", mp, targetvp);
#endif

	return (0);
}

/*
 * Try to find an existing umap_node vnode refering
 * to it, otherwise make a new umap_node vnode which
 * contains a reference to the target vnode.
 */
int
umap_node_create(mp, targetvp, newvpp)
	struct mount *mp;
	struct vnode *targetvp;
	struct vnode **newvpp;
{
	struct umap_node *ap;
	struct vnode *aliasvp;

	ap = umap_node_find(mp, targetvp);

	if (ap) {
		/*
		 * Take another reference to the alias vnode
		 */
#ifdef UMAPFS_DIAGNOSTIC
		vprint("umap_node_create: exists", ap->umap_vnode);
#endif
		aliasvp = ap->umap_vnode;
		VREF(aliasvp);
	} else {
		int error;

		/*
		 * Get new vnode.
		 */
#ifdef UMAPFS_DIAGNOSTIC
		printf("umap_node_create: create new alias vnode\n");
#endif
		if (error = getnewvnode(VT_UFS, mp, umap_vnodeop_p, &aliasvp))
			return (error);	/* XXX: VT_LOFS above */

		/*
		 * Make new vnode reference the umap_node.
		 */
		umap_node_alloc(aliasvp, targetvp);

		/*
		 * aliasvp is already VREF'd by getnewvnode()
		 */
	}

	vrele(targetvp);

#ifdef UMAPFS_DIAGNOSTIC
	vprint("umap_node_create: alias", aliasvp);
	vprint("umap_node_create: target", targetvp);
#endif

	*newvpp = aliasvp;
	return (0);
}
#ifdef UMAPFS_DIAGNOSTIC
int umap_checkvp_barrier = 1;
struct vnode *
umap_checkvp(vp, fil, lno)
	struct vnode *vp;
	char *fil;
	int lno;
{
	struct umap_node *a = VTOUMAP(vp);
#if 0
	/*
	 * Can't do this check because vop_reclaim runs
	 * with funny vop vector.
	 */
	if (vp->v_op != umap_vnodeop_p) {
		printf ("umap_checkvp: on non-umap-node\n");
		while (umap_checkvp_barrier) /*WAIT*/ ;
		panic("umap_checkvp");
	};
#endif
	if (a->umap_lowervp == NULL) {
		/* Should never happen */
		int i; u_long *p;
		printf("vp = %x, ZERO ptr\n", vp);
		for (p = (u_long *) a, i = 0; i < 8; i++)
			printf(" %x", p[i]);
		printf("\n");
		/* wait for debugger */
		while (umap_checkvp_barrier) /*WAIT*/ ;
		panic("umap_checkvp");
	}
	if (a->umap_lowervp->v_usecount < 1) {
		int i; u_long *p;
		printf("vp = %x, unref'ed lowervp\n", vp);
		for (p = (u_long *) a, i = 0; i < 8; i++)
			printf(" %x", p[i]);
		printf("\n");
		/* wait for debugger */
		while (umap_checkvp_barrier) /*WAIT*/ ;
		panic ("umap with unref'ed lowervp");
	};
#if 0
	printf("umap %x/%d -> %x/%d [%s, %d]\n",
	        a->umap_vnode, a->umap_vnode->v_usecount,
		a->umap_lowervp, a->umap_lowervp->v_usecount,
		fil, lno);
#endif
	return a->umap_lowervp;
}
#endif

/* umap_mapids maps all of the ids in a credential, both user and group. */

umap_mapids(credp,usermap,unentries,groupmap,gnentries)
	struct ucred *credp;
	int * usermap, groupmap;
	int unentries,gnentries;
{
	int i,gid,uid;

	/* Find uid entry in map */

	uid = umap_findid(credp->cr_uid,usermap,unentries);

	if (uid != -1) {
		credp->cr_ruid =
		credp->cr_uid =
			(u_short)uid;
	} else 
		credp->cr_ruid = credp->cr_uid = (u_short)NOBODY;

	/* Find gid entry in map */

	gid = umap_findid(credp->cr_gid,groupmap,gnentries);

	if (gid != -1) {
		credp->cr_rgid =
		credp->cr_gid =
			(u_short)gid;
	} else 
		credp->cr_rgid = credp->cr_gid = (u_short)NULLGROUP;

	/* Now we must map each of the set of groups in the cr_groups 
		structure. */

	i = 0;
	while (credp->cr_groups[i] != 0)
	{
		gid = umap_findid(credp->cr_groups[i],groupmap,
			gnentries);
		
		if (gid != -1) 
			credp->cr_groups[i++] = (u_short)gid;
		else
			credp->cr_groups[i++] = (u_short)NULLGROUP;

	}
}

/* umap_findid is called by various routines in umap_vnodeops.c to
 * find a user or group id in a map.
 */

umap_findid(id,map,nentries)
	ushort id;
	int map[][2];
	int nentries;
{
	int i;

	/* Find uid entry in map */
	i = 0;
	while ((i<nentries) && ((u_short)(map[i][0] ) != id))
		i++;

	if ( i < nentries )
		return (map[i][1]);
	else
		return (-1);

}

/* umap_reverse_findid is called by umap_getattr() in umap_vnodeops.c to
 * find a user or group id in a map, in reverse.
 */

umap_reverse_findid(id,map,nentries)
	ushort id;
	int map[][2];
	int nentries;
{
	int i;

	/* Find uid entry in map */
	i = 0;
	while ((i<nentries) && ((u_short)(map[i][1] ) != id))
		i++;

	if ( i < nentries )
		return (map[i][0]);
	else
		return (-1);

}

