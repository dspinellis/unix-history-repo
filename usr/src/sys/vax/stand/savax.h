/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)savax.h	7.3 (Berkeley) %G%
 */

/*
 * Standalone definitions peculiar to vaxen
 * The mba devices in the standalone system are addressed as 
 *	xx(unit,section)
 * where unit is
 *	8*mbanum+drive
 * The mbadrv macro gives the address of the device registers
 * for the specified unit; the mbamba macro gives the address of the
 * mba registers themselves.
 *
 * The uba devices are also addressed by giving, as unit,
 *	8*ubanum+drive
 * The ubamem macro converts a specified unibus address (ala pdp-11)
 * into a unibus memory address space address.
 */

int	cpu;		/* see <sys/cpu.h> */

#define	MAXNMBA	8
#define	MAXNUBA	8
#define	MAXNKDB 2
struct	mba_regs **mbaddr;
int	mbaact;
caddr_t	*uioaddr;
struct	uba_regs **ubaddr;

#ifdef VAX8200
caddr_t	kdbaddr[MAXNKDB];
int	nkdb;
#endif

#define	UNITTOMBA(unit)		((unit)>>3)
#define	UNITTODRIVE(unit)	((unit)&07)

#define	mbamba(unit)		(mbaddr[UNITTOMBA(unit)])
#define	mbadrv(unit) 		(&mbamba(unit)->mba_drv[UNITTODRIVE(unit)])

#define	UNITTOUBA(unit)		((unit)>>3)
#define	ubauba(unit)		(ubaddr[UNITTOUBA(unit)])

/* compute an I/O page physical address from a 16/18/22-bit bus address */
#define	ubamem(unit, off)	(uioaddr[UNITTOUBA(unit)] + ubdevreg(off))

/*
 * RM03/5 (4-byte header plus CRC) format information:
 * codes for sector header word 1
 */
#define	HDR1_FMT22	0x1000	/* standard 16 bit format */
#define	HDR1_OKSCT	0xc000	/* sector ok */
#define	HDR1_SSF	0x2000	/* skip sector flag */
