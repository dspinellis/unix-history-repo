/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 *	@(#)ipc.h	7.1 (Berkeley) 5/8/90
 */

/*
 * SVID compatible ipc.h file
 */
#ifndef _IPC_
#define _IPC_

typedef	long	key_t;	/* XXX should be in types.h */

struct ipc_perm {
	ushort	cuid;	/* creator user id */
	ushort	cgid;	/* creator group id */
	ushort	uid;	/* user id */
	ushort	gid;	/* group id */
	ushort	mode;	/* r/w permission */
	ushort	seq;	/* sequence # (to generate unique msg/sem/shm id) */
	key_t	key;	/* user specified msg/sem/shm key */
};

/* common mode bits */
#define	IPC_R		00400	/* read permission */
#define	IPC_W		00200	/* write/alter permission */

/* SVID required constants (same values as system 5) */
#define	IPC_CREAT	01000	/* create entry if key does not exist */
#define	IPC_EXCL	02000	/* fail if key exists */
#define	IPC_NOWAIT	04000	/* error if request must wait */

#define	IPC_PRIVATE	(key_t)0 /* private key */

#define	IPC_RMID	0	/* remove identifier */
#define	IPC_SET		1	/* set options */
#define	IPC_STAT	2	/* get options */

#endif /* _IPC_ */
