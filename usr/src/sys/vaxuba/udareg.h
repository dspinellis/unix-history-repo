/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)udareg.h	7.2 (Berkeley) 10/23/87
 */

/*
 * UDA50 registers and structures
 */

/*
 * Writing any value to udaip starts initialisation.  Reading from it
 * when the UDA is running makes the UDA look through the command ring
 * to find any new commands.  Reading udasa gives status; writing it
 * during initialisation sets things up.
 */
struct udadevice {
	u_short	udaip;		/* initialisation and polling */
	u_short	udasa;		/* status and address */
};

/*
 * Bits in UDA status register during initialisation
 */
#define	UDA_ERR		0x8000	/* error */
#define	UDA_STEP4	0x4000	/* step 4 has started */
#define	UDA_STEP3	0x2000	/* step 3 has started */
#define	UDA_STEP2	0x1000	/* step 2 has started */
#define	UDA_STEP1	0x0800	/* step 1 has started */
#define	UDA_NV		0x0400	/* no host settable interrupt vector */
#define	UDA_QB		0x0200	/* controller supports Q22 bus */
#define	UDA_DI		0x0100	/* controller implements diagnostics */
#define	UDA_IE		0x0080	/* interrupt enable */
#define	UDA_NCNRMASK	0x003f	/* in STEP1, bits 0-2=NCMDL2, 3-5=NRSPL2 */
#define	UDA_IVECMASK	0x007f	/* in STEP2, bits 0-6 are interruptvec / 4 */
#define	UDA_PI		0x0001	/* host requests adapter purge interrupts */

/*
 * Bits in UDA status register after initialisation
 */
#define	UDA_GO		0x0001	/* run */

#define	UDASR_BITS \
"\20\20ERR\17STEP4\16STEP3\15STEP2\14STEP1\13NV\12QB\11DI\10IE\1GO"

/*
 * UDA Communications Area.  Note that this structure definition
 * requires NRSP and NCMD to be defined already.
 */
struct udaca {
	short	ca_xxx1;	/* unused */
	char	ca_xxx2;	/* unused */
	char	ca_bdp;		/* BDP to purge */
	short	ca_cmdint;	/* command ring transition flag */
	short	ca_rspint;	/* response ring transition flag */
	long	ca_rspdsc[NRSP];/* response descriptors */
	long	ca_cmddsc[NCMD];/* command descriptors */
};

/*
 * Simplified routines (e.g., uddump) reprogram the UDA50 for one command
 * and one response at a time; uda1ca is like udaca except that it provides
 * exactly one command and response descriptor.
 */
struct uda1ca {
	short	ca_xxx1;
	char	ca_xxx2;
	char	ca_bdp;
	short	ca_cmdint;
	short	ca_rspint;
	long	ca_rspdsc;
	long	ca_cmddsc;
};
