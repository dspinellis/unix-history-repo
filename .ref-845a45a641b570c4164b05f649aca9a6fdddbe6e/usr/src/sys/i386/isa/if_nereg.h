/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_nereg.h	7.2 (Berkeley) %G%
 */

/*
 * NE2000 Ethernet Card registers
 */

/* The NE2000 uses a DS8390 Ethernet controller in at the beginning of
   its i/o space */
#include <i386/isa/ic/ds8390.h>

#define ne_data		0x10	/* Data Transfer port */
#define ne_reset	0x1f	/* Card Reset port */

#define	PKTSZ	3*512
#define	TBUF	(16*1024)	/* Starting location of Transmit Buffer */
#define	RBUF	(16*1024+PKTSZ)	/* Starting location of Receive Buffer */
#define	RBUFEND	(32*1024)	/* Ending location of Transmit Buffer */
