/* yylex - scanner front-end for flex */

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Vern Paxson of Lawrence Berkeley Laboratory.
 * 
 * The United States Government has rights in this work pursuant
 * to contract no. DE-AC03-76SF00098 between the United States
 * Department of Energy and the University of California.
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
 */

#ifndef lint
static char sccsid[] = "@(#)yylex.c	5.2 (Berkeley) 6/18/90";
#endif /* not lint */

#include <ctype.h>
#include "flexdef.h"
#include "parse.h"


/* ANSI C does not guarantee that isascii() is defined */
#ifndef isascii
#define isascii(c) ((c) <= 0177)
#endif


/* yylex - scan for a regular expression token
 *
 * synopsis
 *
 *   token = yylex();
 *
 *     token - return token found
 */

int yylex()

    {
    int toktype;
    static int beglin = false;

    if ( eofseen )
	toktype = EOF;
    else
	toktype = flexscan();

    if ( toktype == EOF || toktype == 0 )
	{
	eofseen = 1;

	if ( sectnum == 1 )
	    {
	    synerr( "premature EOF" );
	    sectnum = 2;
	    toktype = SECTEND;
	    }

	else if ( sectnum == 2 )
	    {
	    sectnum = 3;
	    toktype = 0;
	    }

	else
	    toktype = 0;
	}

    if ( trace )
	{
	if ( beglin )
	    {
	    fprintf( stderr, "%d\t", num_rules + 1 );
	    beglin = 0;
	    }

	switch ( toktype )
	    {
	    case '<':
	    case '>':
	    case '^':
	    case '$':
	    case '"':
	    case '[':
	    case ']':
	    case '{':
	    case '}':
	    case '|':
	    case '(':
	    case ')':
	    case '-':
	    case '/':
	    case '\\':
	    case '?':
	    case '.':
	    case '*':
	    case '+':
	    case ',':
		(void) putc( toktype, stderr );
		break;

	    case '\n':
		(void) putc( '\n', stderr );

		if ( sectnum == 2 )
		    beglin = 1;

		break;

	    case SCDECL:
		fputs( "%s", stderr );
		break;

	    case XSCDECL:
		fputs( "%x", stderr );
		break;

	    case WHITESPACE:
		(void) putc( ' ', stderr );
		break;

	    case SECTEND:
		fputs( "%%\n", stderr );

		/* we set beglin to be true so we'll start
		 * writing out numbers as we echo rules.  flexscan() has
		 * already assigned sectnum
		 */

		if ( sectnum == 2 )
		    beglin = 1;

		break;

	    case NAME:
		fprintf( stderr, "'%s'", nmstr );
		break;

	    case CHAR:
		switch ( yylval )
		    {
		    case '<':
		    case '>':
		    case '^':
		    case '$':
		    case '"':
		    case '[':
		    case ']':
		    case '{':
		    case '}':
		    case '|':
		    case '(':
		    case ')':
		    case '-':
		    case '/':
		    case '\\':
		    case '?':
		    case '.':
		    case '*':
		    case '+':
		    case ',':
			fprintf( stderr, "\\%c", yylval );
			break;

		    default:
			if ( ! isascii( yylval ) || ! isprint( yylval ) )
			    fprintf( stderr, "\\%.3o", yylval );
			else
			    (void) putc( yylval, stderr );
			break;
		    }
			
		break;

	    case NUMBER:
		fprintf( stderr, "%d", yylval );
		break;

	    case PREVCCL:
		fprintf( stderr, "[%d]", yylval );
		break;

	    case EOF_OP:
		fprintf( stderr, "<<EOF>>" );
		break;

	    case 0:
		fprintf( stderr, "End Marker" );
		break;

	    default:
		fprintf( stderr, "*Something Weird* - tok: %d val: %d\n",
			 toktype, yylval );
		break;
	    }
	}
	    
    return ( toktype );
    }
