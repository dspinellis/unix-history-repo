/*
 * $Id: fstype.h,v 5.2 90/06/23 22:20:30 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)fstype.h	5.1 (Berkeley) 7/19/90
 */

/*
 * File system types
 */

/*
 * Automount File System
 */
#define HAS_AFS
extern am_ops	afs_ops;	/* Automount file system (this!) */
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
extern am_ops	nfs_ops;	/* NFS */
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
 */
#define HAS_SFS
extern am_ops	sfs_ops;	/* Symlink FS */
