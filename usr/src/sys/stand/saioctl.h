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
 *	@(#)saioctl.h	7.4 (Berkeley) 6/28/90
 */

/* ioctl's -- for disks just now */
#define	SAIOHDR		(('d'<<8)|1)	/* next i/o includes header */
#define	SAIOCHECK	(('d'<<8)|2)	/* next i/o checks data */
#define	SAIOHCHECK	(('d'<<8)|3)	/* next i/o checks header & data */
#define	SAIONOBAD	(('d'<<8)|4)	/* inhibit bad sector forwarding */
#define	SAIODOBAD	(('d'<<8)|5)	/* enable bad sector forwarding */
#define	SAIOECCLIM	(('d'<<8)|6)	/* set limit to ecc correction, bits */
#define	SAIOECCUNL	(('d'<<8)|7)	/* use standard ecc procedures */
#define	SAIORETRIES	(('d'<<8)|8)	/* set retry count for unit */
#define	SAIODEVDATA	(('d'<<8)|9)	/* get pointer to pack label */
#define	SAIOSSI		(('d'<<8)|10)	/* set skip sector inhibit */
#define	SAIONOSSI	(('d'<<8)|11)	/* inhibit skip sector handling */
#define	SAIOSSDEV	(('d'<<8)|12)	/* is device skip sector type? */
#define	SAIODEBUG	(('d'<<8)|13)	/* enable/disable debugging */
#define	SAIOGBADINFO	(('d'<<8)|14)	/* get bad-sector table */
