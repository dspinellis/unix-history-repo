/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)icu.h	5.2 (Berkeley) %G%
 */

/*
 * AT/386 Interrupt Control constants
 * W. Jolitz 8/89
 */

#ifndef	__ICU__
#define	__ICU__

#ifndef	LOCORE
extern	unsigned short	imen;	/* interrupt mask enable */
extern	unsigned short	cpl;	/* current priority level */

extern	unsigned short ttymask; /* things that get masked with spltty() */
extern	unsigned short biomask; /* things that get masked with splbio() */

#define	INTREN(s)	imen &= ~(s)
#define	INTRDIS(s)	imen |= (s)
#endif

/* Interrupt enable bits */
#define	IRQ0	0x0001		/* highest priority - timer */
#define	IRQ1	0x0002
#define	IRQ8	0x0100
#define	IRQ9	0x0200
#define	IRQ2	IRQ9
#define	IRQ10	0x0400
#define	IRQ11	0x0800
#define	IRQ12	0x1000
#define	IRQ13	0x2000
#define	IRQ14	0x4000
#define	IRQ15	0x8000
#define	IRQ3	0x0008
#define	IRQ4	0x0010
#define	IRQ5	0x0020
#define	IRQ6	0x0040
#define	IRQ7	0x0080		/* lowest - parallel printer */

#endif	__ICU__
