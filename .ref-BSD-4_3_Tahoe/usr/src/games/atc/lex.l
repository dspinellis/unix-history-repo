%{
/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

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
