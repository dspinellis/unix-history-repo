/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsdiskless.h	7.5 (Berkeley) %G%
 */

/*
 * Structure that must be initialized for a diskless nfs client.
 * This structure is used by nfs_mountroot() to set up the root and swap
 * vnodes plus do a partial ifconfig(8) and route(8) so that the critical net
 * interface can communicate with the server.
 * The primary bootstrap is expected to fill in the appropriate fields before
 * starting vmunix. Whether or not the swap area is nfs mounted is determined
 * by the value in swdevt[0]. (equal to NODEV --> swap over nfs)
 * Currently only works for AF_INET protocols.
 * NB: All fields are stored in net byte order to avoid hassles with
 * client/server byte ordering differences.
 */
struct nfs_diskless {
	struct ifaliasreq myif;			/* Default interface */
	struct sockaddr_in mygateway;		/* Default gateway */
	struct nfs_args	swap_args;		/* Mount args for swap file */
	u_char		swap_fh[NFS_FHSIZE];	/* Swap file's file handle */
	struct sockaddr_in swap_saddr;		/* Address of swap server */
	char		swap_hostnam[MNAMELEN];	/* Host name for mount pt */
	int		swap_nblks;		/* Size of server swap file */
	struct ucred	swap_ucred;		/* Swap credentials */
	struct nfs_args	root_args;		/* Mount args for root fs */
	u_char		root_fh[NFS_FHSIZE];	/* File handle of root dir */
	struct sockaddr_in root_saddr;		/* Address of root server */
	char		root_hostnam[MNAMELEN];	/* Host name for mount pt */
	long		root_time;		/* Timestamp of root fs */
	char		my_hostnam[MAXHOSTNAMELEN]; /* Client host name */
};
