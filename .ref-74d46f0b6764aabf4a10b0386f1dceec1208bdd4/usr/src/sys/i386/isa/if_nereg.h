/*
 * NE2000 Ethernet Card registers
 * @(#)if_nereg.h	1.1 (Berkeley) %G%
 */

/* The NE2000 uses a DS8390 Ethernet controller in at the beginning of
   its i/o space */
#include "ic/ds8390.h"

#define ne_data		0x10	/* Data Transfer port */
#define ne_reset	0x1f	/* Card Reset port */

#define	PKTSZ	3*512
#define	TBUF	(16*1024)	/* Starting location of Transmit Buffer */
#define	RBUF	(16*1024+PKTSZ)	/* Starting location of Receive Buffer */
#define	RBUFEND	(32*1024)	/* Ending location of Transmit Buffer */
