%{
/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ed James.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

#ifndef lint
static char sccsid[] = "@(#)lex.l	5.2 (Berkeley) 4/30/90";
#endif /* not lint */

#include "y.tab.h"
extern int	line;

%}
%%
[0-9]+			{ yylval.ival = atoi(yytext);  return(ConstOp); }
height			{ return(HeightOp); }
width			{ return(WidthOp); }
newplane		{ return(NewplaneOp); }
update			{ return(UpdateOp); }
airport			{ return(AirportOp); }
line			{ return(LineOp); }
exit			{ return(ExitOp); }
beacon			{ return(BeaconOp); }
[wedcxzaq]		{ yylval.cval = *yytext; return (DirOp); }
[ \t]+			{ }
#[^\n]*\n		{ line++; }
\n			{ line++; }
.			{ return *yytext; }
