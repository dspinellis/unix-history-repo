%{

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */



/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)lex.l	3.8	1/24/86";
#endif

#include "y.tab.h"
#include "uwm.h"

char *dequote();
char *stash();

%}
W	[ \t]+
%%

#.*\n				{ return(COMMENT); }	/* Comments. */
{W}				{ ; }	/* Whitespace. */
[^ \"\t\n#=\^\!\|\(\)\{\}:;]+	{ yylval.sval=stash(yytext);return(STRING); }
\"[^\"\n]*\"			{ yylval.sval=stash(dequote(yytext));
				  return(STRING); }
\n				{ return(NL); }
.				{ return(*yytext); }

%%

/*
 * Remove the quotes enclosing a quoted string.
 */
char *
dequote(s)
char *s;
{
    char *p;

    ++s;
    p = rindex(s, '"');
    *p = '\0';
    return(s);
}

/*
 * Save string away in memory and return a pointer to it.  If
 * no space could be found, quit with a fatal error.
 */
char *stash(s)
char *s;
{
    char *ptr;
    char *malloc();

    ptr = malloc(strlen(s) + 1);
    if (ptr == NULL) {
        Error("out of space");
    }
    return(strcpy(ptr, s));
}
