/*
 * Copyright (c) 1986 MICOM-Interlan, Inc., Boxborough Mass.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)npcmd.h	6.1 (Berkeley) %G%
 *  @(#)NP Series Drivers npcmd.h	1.1
 *
 * Delta Created 9/19/85			Date Extracted 9/20/85
 * File name ../../sccs/h/s.npcmd.h
 */

/* Note that the NPUNIT macro must match that defined in "npreg.h" */

#define NPUNIT(a)	((minor(a) >> 4) & 0x0F)

#define NPRESET		0x01		/* reset the board */
#define	NPSTART		0x04		/* start board execution */
#define	NPGPANIC	0x05		/* Get panic message */
#define	NPINIT		0x06		/* initialize software on board */
#define NPSTATS 	0x07
#define	NPRCSR0		0x08		/* read CSR0 */
#define	NPRCSR1		0x09		/* read CSR1 */
#define	NPRCSR2		0x0a		/* read CSR2 */
#define	NPRCSR3		0x0b		/* read CSR3 */
#define	NPWCSR0		0x0c		/* write CSR0 */
#define	NPWCSR1		0x0d		/* write CSR1 */
#define	NPWCSR2		0x0e		/* write CSR2 */
#define	NPWCSR3		0x0f		/* write CSR3 */
#define NPPOLL  	0x10
#define NPKILL  	0x11
#define	NPSETPROT	0x12		/* set the protocol to use */
#define	NPSETBOARD	0x13		/* set board to use */
#define	NPSETNPDEB	0x14		/* set nc debuging level */
#define	NPSETADDR	0x15		/* set host address */
#define	NPNETBOOT	0x16		/* boot from the network */

/* ICP Board Requests */

#define ICPLOAD  0x02
#define ICPDUMP  0x03
#define ICPPANIC 0x05
#define ICPPOLL  0x10
