/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tmps.h	5.3 (Berkeley) %G%
 */

/*
 * The following structure is used
 * to keep track of the amount of variable
 * storage required by each block.
 * "Max" is the high water mark, "off"
 * the current need. Temporaries for "for"
 * loops and "with" statements are allocated
 * in the local variable area and these
 * numbers are thereby changed if necessary.
 *
 * for the compiler,
 *	low_water is the lowest number register allocated of its type
 *	next_avail is the next available register of its type
 */

#ifdef PC
#ifdef vax
    /*
     *	the number of register types.
     *	the details of how many of each kind of register there is
     *	(and what they are for) is known in tmps.c
     */
#define	NUMREGTYPES	1
#define	REG_GENERAL	0
#endif vax

#ifdef tahoe
    /*
     *	the number of register types.
     *	the details of how many of each kind of register there is
     *	(and what they are for) is known in tmps.c
     */
#define	NUMREGTYPES	1
#define	REG_GENERAL	0
#endif tahoe

#ifdef mc68000
    /*
     *	the number of register types.
     *	the details of how many of each kind of register there is
     *	(and what they are for) is known in tmps.c
     */
#define	NUMREGTYPES	2
#define	REG_DATA	0
#define	REG_ADDR	1
#endif mc68000
#endif PC

struct om {
	long	om_max;
#ifdef PC
	long	low_water[NUMREGTYPES];
#endif PC
	struct tmps {
	    long	om_off;
#ifdef PC
	    long	next_avail[NUMREGTYPES];
#endif PC
	}	curtmps;
} sizes[DSPLYSZ];

    /*
     *	an enumeration for whether a temporary can be a register.  cf. tmps.c
     */
#define NOREG 0
#define REGOK 1
