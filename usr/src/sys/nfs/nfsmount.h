/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfsmount.h	7.1 (Berkeley) %G%
 */

/*
 * Mount structure.
 * One allocated on every nfs mount.
 * Holds nfs specific info for mount (sockets...)
 */
struct	nfsmount {
	int	nm_flag;		/* Flags for soft/hard... */
	struct	mount *nm_mountp;	/* vfs structure for this filesystem */
	nfsv2fh_t nm_fh;		/* File handle of root dir */
	struct	mbuf *nm_sockaddr;	/* Address of server */
	struct	socket	*nm_so;		/* rpc socket */
	int	nm_timeo;		/* Timeout interval */
	int	nm_retrans;		/* # of retransmits */
	int	nm_rsize;		/* Max size of read rpc */
	int	nm_wsize;		/* Max size of write rpc */
	char	nm_path[MNAMELEN];	/* Path mounted on */
	char	nm_host[MNAMELEN];	/* Remote host name */
};

struct nfsmount *vfs_to_nfs();
