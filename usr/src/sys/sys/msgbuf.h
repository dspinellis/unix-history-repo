/*
 * Copyright (c) 1981, 1984 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)msgbuf.h	7.5 (Berkeley) %G%
 */

#define	MSG_BSIZE	(4096 - 3 * sizeof(long))
struct	msgbuf {
#define	MSG_MAGIC	0x063061
	long	msg_magic;
	long	msg_bufx;		/* write pointer */
	long	msg_bufr;		/* read pointer */
	char	msg_bufc[MSG_BSIZE];	/* buffer */
};
#ifdef KERNEL
struct	msgbuf *msgbufp;
#endif
