/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfsnode.h	7.11 (Berkeley) 6/28/90
 */

/*
 * The nfsnode is the nfs equivalent to ufs's inode. Any similarity
 * is purely coincidental.
 * There is a unique nfsnode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
 * An nfsnode is 'named' by its file handle. (nget/nfs_node.c)
 */

struct nfsnode {
	struct	nfsnode *n_chain[2];	/* must be first */
	nfsv2fh_t n_fh;			/* NFS File Handle */
	long	n_flag;			/* Flag for locking.. */
	struct	vnode *n_vnode;	/* vnode associated with this nfsnode */
	time_t	n_attrstamp;	/* Time stamp (sec) for attributes */
	struct	vattr n_vattr;	/* Vnode attribute cache */
	struct	sillyrename *n_sillyrename;	/* Ptr to silly rename struct */
	u_long	n_size;		/* Current size of file */
	time_t	n_mtime;	/* Prev modify time to maintain data cache consistency*/
	time_t	n_ctime;	/* Prev create time for name cache consistency*/
	int	n_error;	/* Save write error value */
	pid_t	n_lockholder;	/* holder of nfsnode lock */
	pid_t	n_lockwaiter;	/* most recent waiter for nfsnode lock */
	u_long	n_direofoffset;	/* Dir. EOF offset cache */
};

#define	n_forw		n_chain[0]
#define	n_back		n_chain[1]

#ifdef KERNEL
/*
 * Convert between nfsnode pointers and vnode pointers
 */
#define VTONFS(vp)	((struct nfsnode *)(vp)->v_data)
#define NFSTOV(np)	((struct vnode *)(np)->n_vnode)
#endif
/*
 * Flags for n_flag
 */
#define	NLOCKED		0x1	/* Lock the node for other local accesses */
#define	NWANT		0x2	/* Want above lock */
#define	NMODIFIED	0x4	/* Might have a modified buffer in bio */
#define	NWRITEERR	0x8	/* Flag write errors so close will know */
