/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)savax.h	7.5 (Berkeley) %G%
 */

/*
 * Standalone definitions peculiar to vaxen:
 *
 * The mba devices in the standalone system are addressed as
 *	type(mba, 0, drive, partition)		(disks)
 *	type(mba, formatter, transport, file)	(tapes)
 *
 * The mbadrv macro gives the address of the device registers
 * for the specified unit.
 *
 * The uba devices in the standalone system are addressed as
 *	type(uba, ctlr, drive, partition)	(disks)
 *	type(uba, formatter, transport, file)	(tapes)
 *
 * The ubamem macro converts a specified unibus address (ala pdp-11)
 * into a unibus memory address space address.
 */

#define	mbadrv(mba, unit) 	(&mbamba(mba)->mba_drv[unit])
/* compute an I/O page physical address from a 16/18/22-bit bus address */
#define	ubamem(uba, off)	(uioaddr[uba] + ubdevreg(off))

#define	mbamba(mba)		(mbaddr[mba])
#define	ubauba(uba)		(ubaddr[uba])

#define	MAXNMBA	8
#define	MAXNUBA	8
#define	MAXNKDB	2

struct	mba_regs **mbaddr;
int	mbaact;
int	nmba;

caddr_t	*uioaddr;
struct	uba_regs **ubaddr;
int	nuba;

#ifdef VAX8200
caddr_t	kdbaddr[MAXNKDB];
int	nkdb;
#endif

int	cpu;				/* see ../vax/cpu.h */
