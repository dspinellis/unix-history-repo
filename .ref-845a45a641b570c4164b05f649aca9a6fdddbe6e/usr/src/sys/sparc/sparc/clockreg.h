/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)clockreg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: clockreg.h,v 1.6 92/11/26 03:04:48 torek Exp $ (LBL)
 */

/*
 * Sun-4c clock Mostek TOD clock.  This includes the ``id prom''.
 */

/*
 * ID prom format.  The ``host id'' is set up by taking the machine
 * ID as the top byte and the hostid field as the remaining three.
 * The id_xxx0 field appears to contain some other number.  The id_xxx1
 * contains a bunch of 00's and a5's on my machines, suggesting it is
 * not actually used.  The checksum seems to include them, however.
 */
struct sun4c_idprom {
	u_char	id_format;		/* format identifier (= 1) */
	u_char	id_machine;		/* machine type (see cpu.h) */
	u_char	id_ether[6];		/* ethernet address */
	long	id_xxx0;		/* ??? */
	u_char	id_hostid[3];		/* ``host id'' bytes */
	u_char	id_checksum;		/* xor of everything else */
	char	id_xxx1[16];		/* ??? */
};

/*
 * Mostek MK48T02 clock.
 *
 * The clock includes 2040 bytes of RAM, the last 32 of which serve to
 * identify the kind of Sun 4c this is.
 */
struct clockreg {
	char	cl_nvram[2008];		/* `free' nonvolatile memory */
	struct	sun4c_idprom cl_idprom;	/* `id prom' */
	volatile u_char	cl_csr;		/* control register */
	volatile u_char	cl_sec;		/* seconds (0..59; BCD) */
	volatile u_char	cl_min;		/* minutes (0..59; BCD) */
	volatile u_char	cl_hour;	/* hour (0..23; BCD) */
	volatile u_char	cl_wday;	/* weekday (1..7) */
	volatile u_char	cl_mday;	/* day in month (1..31; BCD) */
	volatile u_char	cl_month;	/* month (1..12; BCD) */
	volatile u_char	cl_year;	/* year (0..99; BCD) */
};

/* bits in cl_csr */
#define	CLK_WRITE	0x80		/* want to write */
#define	CLK_READ	0x40		/* want to read (freeze clock) */

struct clockreg *clockreg;

/*
 * Sun chose the year `68' as their base count, so that
 * cl_year==0 means 1968.
 */
#define	YEAR0	68
