/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 * $Id: ansi.h,v 1.2 92/02/03 02:27:39 jnweiger Exp $ FAU
 */

#define NATTR		6

#define ATTR_DI		0	/* Dim mode */
#define ATTR_US		1	/* Underscore mode */
#define ATTR_BD		2	/* Bold mode */
#define ATTR_RV		3	/* Reverse mode */
#define ATTR_SO		4	/* Standout mode */
#define ATTR_BL		5	/* Blinking */

#define A_DI	(1<<ATTR_DI)
#define A_US	(1<<ATTR_US)
#define A_BD	(1<<ATTR_BD)
#define A_RV	(1<<ATTR_RV)
#define A_SO	(1<<ATTR_SO)
#define A_BL	(1<<ATTR_BL)
#define A_MAX	(1<<(NATTR-1))

/* Types of movement used by GotoPos() */
enum move_t {
	M_NONE,
	M_UP,
	M_CUP,
	M_DO,
	M_CDO,
	M_LE,
	M_CLE,
	M_RI,
	M_CRI,
	M_RW,
	M_CR	/* CR and rewrite */
};

#define EXPENSIVE	 1000

#define G0			 0
#define G1			 1
#define G2			 2
#define G3			 3

#define ASCII		 0

#ifdef TOPSTAT
#define STATLINE	 (0)
#else
#define STATLINE	 (screenheight-1)
#endif

