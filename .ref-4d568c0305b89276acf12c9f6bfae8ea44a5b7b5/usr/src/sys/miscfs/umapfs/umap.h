/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * the UCLA Ficus project.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)umap.h	8.3 (Berkeley) %G%
 *
 * @(#)null_vnops.c       1.5 (Berkeley) 7/10/92
 */

#define MAPFILEENTRIES 64
#define GMAPFILEENTRIES 16
#define NOBODY 32767
#define NULLGROUP 65534

struct umap_args {
	char		*target;	/* Target of loopback  */
	int 		nentries;       /* # of entries in user map array */
	int 		gnentries;	/* # of entries in group map array */
	u_long 		(*mapdata)[2];	/* pointer to array of user mappings */
	u_long 		(*gmapdata)[2];	/* pointer to array of group mappings */
};

struct umap_mount {
	struct mount	*umapm_vfs;
	struct vnode	*umapm_rootvp;	/* Reference to root umap_node */
	int             info_nentries;  /* number of uid mappings */
	int		info_gnentries;	/* number of gid mappings */
	u_long		info_mapdata[MAPFILEENTRIES][2]; /* mapping data for 
	    user mapping in ficus */
	u_long		info_gmapdata[GMAPFILEENTRIES][2]; /*mapping data for 
	    group mapping in ficus */
};

#ifdef KERNEL
/*
 * A cache of vnode references
 */
struct umap_node {
	struct umap_node	*umap_forw;	/* Hash chain */
	struct umap_node	*umap_back;
	struct vnode	*umap_lowervp;	/* Aliased vnode - VREFed once */
	struct vnode	*umap_vnode;	/* Back pointer to vnode/umap_node */
};

extern int umap_node_create __P((struct mount *mp, struct vnode *target, struct vnode **vpp));
extern u_long umap_reverse_findid __P((u_long id, u_long map[][2], int nentries));
extern void umap_mapids __P((struct mount *v_mount, struct ucred *credp));

#define	MOUNTTOUMAPMOUNT(mp) ((struct umap_mount *)((mp)->mnt_data))
#define	VTOUMAP(vp) ((struct umap_node *)(vp)->v_data)
#define UMAPTOV(xp) ((xp)->umap_vnode)
#ifdef UMAPFS_DIAGNOSTIC
extern struct vnode *umap_checkvp __P((struct vnode *vp, char *fil, int lno));
#define	UMAPVPTOLOWERVP(vp) umap_checkvp((vp), __FILE__, __LINE__)
#else
#define	UMAPVPTOLOWERVP(vp) (VTOUMAP(vp)->umap_lowervp)
#endif

extern int (**umap_vnodeop_p)();
extern struct vfsops umap_vfsops;
#endif /* KERNEL */
