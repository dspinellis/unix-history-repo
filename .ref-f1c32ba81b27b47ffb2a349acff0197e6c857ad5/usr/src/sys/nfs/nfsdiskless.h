/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsdiskless.h	7.1 (Berkeley) %G%
 */

/*
 * Structure that must be initialized for a diskless nfs client.
 * This structure is used by nfs_mountroot() to set up the root and swap
 * vnodes plus do a partial ifconfig(8) and route(8) so that the critical net
 * interface can communicate with the server.
 * For now it is statically initialized in swapvmunix.c, but someday a primary
 * bootstrap should fill it in.
 */
struct nfs_diskless {
	struct ifaliasreq myif;		/* Info. for partial ifconfig */
	struct sockaddr	mygateway;	/* Default gateway for "route add" */
	struct nfs_args	swap_args;	/* Mount args for swap file */
	u_char		swap_fh[NFS_FHSIZE]; /* Swap file's file handle */
	struct sockaddr	swap_saddr;	/* Address of swap server */
	char		*swap_hostnam;	/* Host name for mount pt */
	struct nfs_args	root_args;	/* Mount args for root fs */
	u_char		root_fh[NFS_FHSIZE]; /* File handle of root dir */
	struct sockaddr	root_saddr;	/* Address of root server */
	char		*root_hostnam;	/* Host name for mount pt */
};
