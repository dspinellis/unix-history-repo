/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fstype.h	5.4 (Berkeley) %G%
 *
 * $Id: fstype.h,v 5.2.2.1 1992/02/09 15:09:57 jsp beta $
 *
 */

/*
 * File system types
 */

/*
 * Automount File System
 */
#define HAS_AFS
extern am_ops	afs_ops;	/* Automount file system (this!) */
extern am_ops	toplvl_ops;	/* Top-level automount file system */
extern am_ops	root_ops;	/* Root file system */
extern qelem	afs_srvr_list;
extern fserver *find_afs_srvr P((mntfs*));

/*
 * Direct Automount File System
 */
#define	HAS_DFS
extern am_ops	dfs_ops;	/* Direct Automount file system (this too) */

/*
 * Error File System
 */
#define HAS_EFS
extern am_ops	efs_ops;	/* Error file system */

/*
 * Inheritance File System
 */
#define HAS_IFS
extern am_ops	ifs_ops;	/* Inheritance file system */

/*
 * Loopback File System
 * LOFS is optional - you can compile without it.
 */
#ifdef OS_HAS_LOFS
/*
 * Most systems can't support this, and in
 * any case most of the functionality is
 * available with Symlink FS.  In fact,
 * lofs_ops is not yet available.
 */
#define HAS_LOFS
extern am_ops lofs_ops;
#endif

/*
 * Netw*rk File System
 * Good, slow, NFS.
 * NFS host - a whole tree
 */
#define HAS_NFS
#define	HAS_HOST
#define HAS_NFSX
extern am_ops	nfs_ops;	/* NFS */
extern am_ops	nfsx_ops;	/* NFS X */
extern am_ops	host_ops;	/* NFS host */
#ifdef HOST_EXEC
extern char	*host_helper;	/* "/usr/local/etc/amd-host" */
#endif
extern qelem	nfs_srvr_list;
extern fserver *find_nfs_srvr P((mntfs*));

/*
 * Program File System
 * PFS is optional - you can compile without it.
 * This is useful for things like RVD.
 */
#define HAS_PFS
extern am_ops	pfs_ops;	/* PFS */

/*
 * Translucent File System
 * TFS is optional - you can compile without it.
 * This is just plain cute.
 */
#ifdef notdef
extern am_ops	tfs_ops;	/* TFS */
#endif
#undef	HAS_TFS

/*
 * Un*x File System
 * Normal local disk file system.
 */
#define HAS_UFS
extern am_ops	ufs_ops;	/* Un*x file system */

/*
 * Symbolic-link file system
 * A "filesystem" which is just a symbol link.
 *
 * sfsx also checks that the target of the link exists.
 */
#define HAS_SFS
extern am_ops	sfs_ops;	/* Symlink FS */
#define HAS_SFSX
extern am_ops	sfsx_ops;	/* Symlink FS with existence check */

/*
 * Union file system
 */
#define	HAS_UNION_FS
extern am_ops	union_ops;	/* Union FS */
