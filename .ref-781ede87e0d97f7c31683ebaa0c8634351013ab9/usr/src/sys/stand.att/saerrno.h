/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)saerrno.h	7.3 (Berkeley) %G%
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
