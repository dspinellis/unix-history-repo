/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)tmps.h	8.1 (Berkeley) 6/6/93
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
