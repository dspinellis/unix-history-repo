/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)savax.h	6.3 (Berkeley) %G%
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

#define	MAXNMBA	4
#define	MAXNUBA	4
struct	mba_regs **mbaddr;
int	mbaact;
caddr_t	*umaddr;
struct	uba_regs **ubaddr;

#define	UNITTOMBA(unit)		((unit)>>3)
#define	UNITTODRIVE(unit)	((unit)&07)

#define	mbamba(unit)		(mbaddr[UNITTOMBA(unit)])
#define	mbadrv(unit) 		(&mbamba(unit)->mba_drv[UNITTODRIVE(unit)])

#define	UNITTOUBA(unit)		((unit)>>3)
#define	ubauba(unit)		(ubaddr[UNITTOUBA(unit)])
#define	ubamem(unit, off)	((umaddr[UNITTOUBA(unit)]+ubdevreg(off)))

#define	PHYSUBA0	0x20006000
#define	PHYSMBA0	0x20010000
#define	PHYSMBA1	0x20012000
#define	PHYSUMEM	0x2013e000
