/*
 * Copyright (c) 1991, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * @(#) $Header: remote.h,v 1.4 92/01/20 15:12:42 mccanne Exp $ (LBL)
 */

struct remote_fn {
	int (*send)();		/* send an rpc message */
	int (*recv)();		/* receive an rpc message */
	void (*close)();	/* shutdown the link layer */
	int maxdata;		/* maximum number of read/write bytes */
	int rpcsize;		/* size of rpc msg buffers */
};

/*
 * Error codes.
 */
#define EKGDB_CSUM	1	/* failed checksum */
#define EKGDB_2BIG	2	/* "giant" packet */
#define EKGDB_RUNT	3	/* short packet */
#define EKGDB_BADOP	4	/* bad op code in packet */
#define EKGDB_TIMEOUT	5	/* request timed out */
#define EKGDB_IO	6	/* generic I/O error */
