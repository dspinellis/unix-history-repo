/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "ctype.h"

int	SyntaxTable = FUNDAMENTAL;	/* Current table to use. */

char CharTable[NMAJORS][128] = {
{	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_Op|_P,	_Cl|_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,
	_W|_N,	_W|_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_Op|_P,	_P,	_Cl|_P,	_P,	_P,
	_P,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_Op|_P,	_P,	_Cl|_P,	_P,	_C	},

{	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P|_W,
	_Op|_P,	_Cl|_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,
	_W|_N,	_W|_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_Op|_P,	_P,	_Cl|_P,	_P,	_P,
	_P,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_Op|_P,	_P,	_Cl|_P,	_P,	_C	},

{	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_P,	_P,	_P,	_P,	_P|_W,	_P,	_P,	_P,
	_Op|_P,	_Cl|_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,
	_W|_N,	_W|_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_Op|_P,	_P,	_Cl|_P,	_P,	_P|_W,
	_P,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_Op|_P,	_P,	_Cl|_P,	_P,	_C	
#ifndef LISP
}
#else
},

{	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_P,	_W|_P,	_P,	_P,	_W|_P,	_W|_P,	_W|_P,	_P,
	_Op|_P,	_Cl|_P,	_W|_P,	_W|_P,	_P,	_W|_P,	_P,	_P,
	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,	_W|_N,
	_W|_N,	_W|_N,	_W|_P,	_P,	_W|_P,	_W|_P,	_W|_P,	_W|_P,
	_W|_P,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,	_W|_U,
	_W|_U,	_W|_U,	_W|_U,	_Op|_P,	_P,	_Cl|_P,	_W|_P,	_W|_P,
	_P,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,	_W|_L,
	_W|_L,	_W|_L,	_W|_L,	_Op|_W|_P,	_W|_P,	_Cl|_W|_P,	_W|_P,	_W|_C	},
#endif
};

ismword(c)
{
	return ((CharTable[curbuf->b_major])[c]&(_W));
}
