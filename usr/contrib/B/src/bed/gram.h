/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: gram.h,v 2.0 84/06/18 15:46:55 guido Exp $ */

/*
 * B editor -- Grammar symbols.
 */

/*
 * Values used in "tabl.c" but also publicly.
 */

#define Rootsymbol	00
#define Suggestion	97
#define Optional	98
#define Hole    	99


/*
 * Ditto for "lexi.c".
 */

#define LEXICAL 100

/*
 * Routines defined in "gram.c".
 */

string *noderepr();
node gram();
node suggestion();
node variable();
string symname();

/*
 * Macros for oft-used funtion.
 */

#define Fwidth(str) ((str) ? fwidth(str) : 0)

#define Fw_zero(str) (!(str) || index("\b\t", (str)[0]))
#define Fw_positive(str) ((str) && (str)[0] >= ' ')
#define Fw_negative(str) ((str) && (str)[0] == '\n')
