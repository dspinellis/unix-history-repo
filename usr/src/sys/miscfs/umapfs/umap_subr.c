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
 *	@(#)umap_subr.c	8.3 (Berkeley) %G%
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
#include <miscfs/umapfs/umap.h>

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
 * Return alias for target vnode if already exists, else 0.
 */
static struct vnode *
umap_node_find(mp, targetvp)
	struct mount *mp;
	struct vnode *targetvp;
{
	struct umap_node_cache *hd;
	struct umap_node *a;
	struct vnode *vp;

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

 loop:
	for (a = hd->ac_forw; a != (struct umap_node *) hd; a = a->umap_forw) {
		if (a->umap_lowervp == targetvp &&
		    a->umap_vnode->v_mount == mp) {
			vp = UMAPTOV(a);
			/*
			 * We need vget for the VXLOCK
			 * stuff, but we don't want to lock
			 * the lower node.
			 */
			if (vget(vp, 0)) {
				printf ("null_node_find: vget failed.\n");
				goto loop;
			}
			return (vp);
		}
	}

#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_node_find(%x, %x): NOT found\n", mp, targetvp);
#endif

	return (0);
}

/*
 * Make a new umap_node node.
 * Vp is the alias vnode, lofsvp is the target vnode.
 * Maintain a reference to (targetvp).
 */
static int
umap_node_alloc(mp, lowervp, vpp)
	struct mount *mp;
	struct vnode *lowervp;
	struct vnode **vpp;
{
	struct umap_node_cache *hd;
	struct umap_node *xp;
	struct vnode *othervp, *vp;
	int error;

	if (error = getnewvnode(VT_UMAP, mp, umap_vnodeop_p, vpp))
		return (error);
	vp = *vpp;

	MALLOC(xp, struct umap_node *, sizeof(struct umap_node),
	    M_TEMP, M_WAITOK);
	vp->v_type = lowervp->v_type;
	xp->umap_vnode = vp;
	vp->v_data = xp;
	xp->umap_lowervp = lowervp;
	/*
	 * Before we insert our new node onto the hash chains,
	 * check to see if someone else has beaten us to it.
	 * (We could have slept in MALLOC.)
	 */
	if (othervp = umap_node_find(lowervp)) {
		FREE(xp, M_TEMP);
		vp->v_type = VBAD;	/* node is discarded */
		vp->v_usecount = 0;	/* XXX */
		*vpp = othervp;
		return (0);
	}
	VREF(lowervp);   /* Extra VREF will be vrele'd in umap_node_create */
	hd = umap_node_hash(lowervp);
	insque(xp, hd);
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
	struct vnode *aliasvp;

	if (aliasvp = umap_node_find(mp, targetvp)) {
		/*
		 * Take another reference to the alias vnode
		 */
#ifdef UMAPFS_DIAGNOSTIC
		vprint("umap_node_create: exists", ap->umap_vnode);
#endif
		/* VREF(aliasvp); */
	} else {
		int error;

		/*
		 * Get new vnode.
		 */
#ifdef UMAPFS_DIAGNOSTIC
		printf("umap_node_create: create new alias vnode\n");
#endif
		/*
		 * Make new vnode reference the umap_node.
		 */
		if (error = umap_node_alloc(mp, targetvp, &aliasvp))
			return (error);

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
	}
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
	}
#if 0
	printf("umap %x/%d -> %x/%d [%s, %d]\n",
	        a->umap_vnode, a->umap_vnode->v_usecount,
		a->umap_lowervp, a->umap_lowervp->v_usecount,
		fil, lno);
#endif
	return (a->umap_lowervp);
}
#endif

/* umap_mapids maps all of the ids in a credential, both user and group. */

umap_mapids(v_mount,credp)
	struct mount *v_mount;
	struct ucred *credp;
{
	int i,gid,uid,unentries,gnentries,*groupmap,*usermap;

	unentries =  MOUNTTOUMAPMOUNT(v_mount)->info_nentries;
	usermap =  &(MOUNTTOUMAPMOUNT(v_mount)->info_mapdata[0][0]);
	gnentries =  MOUNTTOUMAPMOUNT(v_mount)->info_gnentries;
	groupmap =  &(MOUNTTOUMAPMOUNT(v_mount)->info_gmapdata[0][0]);

	/* Find uid entry in map */

	uid = umap_findid(credp->cr_uid,usermap,unentries);

	if (uid != -1) {
		credp->cr_uid =
			(u_short)uid;
	} else 
		credp->cr_uid = (u_short)NOBODY;

	/* Find gid entry in map */

	gid = umap_findid(credp->cr_gid,groupmap,gnentries);

	if (gid != -1) {
		credp->cr_gid =
			(u_short)gid;
	} else 
		credp->cr_gid = (u_short)NULLGROUP;

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
