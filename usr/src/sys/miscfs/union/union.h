/*
 * Copyright (c) 1994 The Regents of the University of California.
 * Copyright (c) 1994 Jan-Simon Pendry.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)union.h	1.8 (Berkeley) %G%
 */

struct union_args {
	char		*target;	/* Target of loopback  */
};

struct union_mount {
	struct vnode	*um_uppervp;
	struct vnode	*um_lowervp;
	struct ucred	*um_cred;	/* Credentials of user calling mount */
	int		um_cmode;	/* cmask from mount process */
};

#ifdef KERNEL

/*
 * DEFDIRMODE is the mode bits used to create a shadow directory.
 */
#define VRWXMODE (VREAD|VWRITE|VEXEC)
#define VRWMODE (VREAD|VWRITE)
#define UN_DIRMODE ((VRWXMODE)|(VRWXMODE>>3)|(VRWXMODE>>6))
#define UN_FILEMODE ((VRWMODE)|(VRWMODE>>3)|(VRWMODE>>6))

/*
 * A cache of vnode references
 */
struct union_node {
	struct union_node	*un_next;	/* Hash chain */
	struct vnode		*un_vnode;	/* Back pointer */
	struct vnode	        *un_uppervp;	/* overlaying object */
	struct vnode	        *un_lowervp;	/* underlying object */
	struct vnode		*un_dirvp;	/* Parent dir of uppervp */
	char			*un_path;	/* saved component name */
	int			un_hash;	/* saved un_path hash value */
	int			un_openl;	/* # of opens on lowervp */
	int			un_flags;
#ifdef DIAGNOSTIC
	pid_t			un_pid;
#endif
};

#define UN_WANT 0x01
#define UN_LOCKED 0x02

extern int union_allocvp __P((struct vnode **, struct mount *,
				struct vnode *, struct vnode *,
				struct componentname *, struct vnode *,
				struct vnode *));
extern int union_copyfile __P((struct proc *, struct ucred *,
				struct vnode *, struct vnode *));
extern int union_mkshadow __P((struct union_mount *, struct vnode *,
				struct componentname *, struct vnode **));
extern int union_vn_create __P((struct vnode **, struct union_node *,
				struct proc *));
extern int union_cn_close __P((struct vnode *, int, struct ucred *,
				struct proc *));
extern void union_removed_upper __P((struct union_node *un));
extern struct vnode *union_lowervp __P((struct vnode *));

#define	MOUNTTOUNIONMOUNT(mp) ((struct union_mount *)((mp)->mnt_data))
#define	VTOUNION(vp) ((struct union_node *)(vp)->v_data)
#define	UNIONTOV(un) ((un)->un_vnode)
#define	LOWERVP(vp) (VTOUNION(vp)->un_lowervp)
#define	UPPERVP(vp) (VTOUNION(vp)->un_uppervp)
#define OTHERVP(vp) (UPPERVP(vp) ? UPPERVP(vp) : LOWERVP(vp))

extern int (**union_vnodeop_p)();
extern struct vfsops union_vfsops;
#endif /* KERNEL */
