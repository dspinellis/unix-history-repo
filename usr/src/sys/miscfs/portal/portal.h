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
 *	@(#)portal.h	8.2 (Berkeley) %G%
 *
 * $Id: portal.h,v 1.3 1992/05/30 10:05:24 jsp Exp jsp $
 */

struct portal_args {
	char		*pa_config;	/* Config file */
	int		pa_socket;	/* Socket to server */
};

struct portal_cred {
	int		pcr_flag;		/* File open mode */
	uid_t		pcr_uid;		/* From ucred */
	short		pcr_ngroups;		/* From ucred */
	gid_t		pcr_groups[NGROUPS];	/* From ucred */
};

#ifdef KERNEL

#ifndef VT_PORTAL
#define VT_PORTAL VT_UFS
#endif

struct portalmount {
	struct vnode	*pm_root;	/* Root node */
	struct file	*pm_server;	/* Held reference to server socket */
};

struct portalnode {
	int		pt_size;	/* Length of Arg */
	char		*pt_arg;	/* Arg to send to server */
	int		pt_fileid;	/* cookie */
};

#define VFSTOPORTAL(mp)	((struct portalmount *)((mp)->mnt_data))
#define	VTOPORTAL(vp) ((struct portalnode *)(vp)->v_data)

#define PORTAL_ROOTFILEID	2

extern int (**portal_vnodeop_p)();
extern struct vfsops portal_vfsops;
#endif /* KERNEL */
