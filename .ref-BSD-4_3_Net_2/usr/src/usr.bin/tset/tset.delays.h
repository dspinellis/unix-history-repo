/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)tset.delays.h	5.4 (Berkeley) 6/1/90
 */

/*
**  SYSTEM DEPENDENT TERMINAL DELAY TABLES
**
**	Evans Hall VAX
**
**	This file maintains the correspondence between the delays
**	defined in /etc/termcap and the delay algorithms on a
**	particular system.  For each type of delay, the bits used
**	for that delay must be specified (in XXbits) and a table
**	must be defined giving correspondences between delays and
**	algorithms.  Algorithms which are not fixed delays (such
**	as dependent on current column or line number) must be
**	cludged in some way at this time.
*/



/*
**  Carriage Return delays
*/

int	CRbits = CRDELAY;
struct delay	CRdelay[] =
{
	0,	CR0,
	9,	CR3,
	80,	CR1,
	160,	CR2,
	-1
};

/*
**  New Line delays
*/

int	NLbits = NLDELAY;
struct delay	NLdelay[] =
{
	0,	NL0,
	66,	NL1,		/* special M37 delay */
	100,	NL2,
	-1
};


/*
**  Back Space delays
*/

int	BSbits = BSDELAY;
struct delay	BSdelay[] =
{
	0,	BS0,
	-1
};


/*
**  TaB delays
*/

int	TBbits = TBDELAY;
struct delay	TBdelay[] =
{
	0,	TAB0,
	11,	TAB1,		/* special M37 delay */
	-1
};


/*
**  Form Feed delays
*/

int	FFbits = VTDELAY;
struct delay	FFdelay[] =
{
	0,	FF0,
	2000,	FF1,
	-1
};

#ifdef CBVIRTTERM
/*
 * Map from the universal tables in termcap to the particular numbers
 * this system uses.  The lack of standardization of terminal numbers
 * is a botch but such is life.
 */
struct vt_map {
	char stdnum;
	char localnum;
} vt_map[] = {
#ifdef	TERM_TEC
	1, TERM_TEC,
#endif
#ifdef	TERM_V61
	2, TERM_V61,
#endif
#ifdef	TERM_V10
	3, TERM_V10,
#endif
#ifdef	TERM_TEX
	4, TERM_TEX,
#endif
#ifdef	TERM_D40
	5, TERM_D40,
#endif
#ifdef	TERM_H45
	6, TERM_H45,
#endif
#ifdef	TERM_D42
	7, TERM_D42,
#endif
#ifdef TERM_C100
	8, TERM_C100,
#endif
#ifdef TERM_MIME
	9, TERM_MIME,
#endif
	0,0
};
#endif
