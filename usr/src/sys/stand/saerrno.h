/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)saerrno.h	7.3 (Berkeley) 6/28/90
 */

extern	int errno;	/* just like unix */

/* error codes */
#define	EADAPT	1	/* bad adaptor */
#define	ECTLR	2	/* bad controller */
#define	EUNIT	3	/* bad drive */
#define	EPART	4	/* bad partition */
#define	ERDLAB	5	/* can't read disk label */
#define	EUNLAB	6	/* unlabeled disk */
#define	ENXIO	7	/* bad device specification */
#define	EBADF	8	/* bad file descriptor */
#define	EOFFSET	9	/* relative seek not supported */
#define	ESRCH	10	/* directory search for file failed */
#define	EIO	11	/* generic error */
#define	ECMD	12	/* undefined driver command */
#define	EBSE	13	/* bad sector error */
#define	EWCK	14	/* write check error */
#define	EECC	15	/* uncorrectable ecc error */
#define	EHER	16	/* hard error */
