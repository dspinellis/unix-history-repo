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
 *	@(#)misc-hpux.h	5.4 (Berkeley) %G%
 *
 * $Id: misc-hpux.h,v 5.2.2.1 1992/02/09 15:10:24 jsp beta $
 *
 */

/*
 * These definitions are from <nfs/nfs.h>
 * Unfortunately, that file cannot be included
 * because it contains lots of structure definitions
 * that are not wanted (they produce name clashes).
 * Isn't HP-UX wonderful!
 */

/*
 * HP-UX specific definitions
 */
struct nfs_args {
	struct sockaddr_in	*addr;		/* file server address */
	fhandle_t		*fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	int			wsize;		/* write size in bytes */
	int			rsize;		/* read size in bytes */
	int			timeo;		/* initial timeout in .1 secs */
	int			retrans;	/* times to retry send */
	char			*hostname;	/* server's name */
#ifdef __hp9000s700 /* XXX for HPUX 8.0 */
	char			*fsname;	/* server's filesystem name */
#endif
};

/*
 * NFS mount option flags
 */
#define	NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	NFSMNT_WSIZE	0x002	/* set write size */
#define	NFSMNT_RSIZE	0x004	/* set read size */
#define	NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	NFSMNT_INT	0x040	/* set option to have interruptable mounts */
#define	NFSMNT_NODEVS   0x080   /* turn off device file access (default on) */
