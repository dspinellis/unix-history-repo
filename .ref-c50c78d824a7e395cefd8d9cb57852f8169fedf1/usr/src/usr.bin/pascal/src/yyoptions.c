/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)yyoptions.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"	/* must be included for yy.h */
#include "yy.h"

/*
 * Options processes the option
 * strings which can appear in
 * comments and returns the next character.
 */
options()
{
	register c;
#ifdef PI0
	register ch;
#endif
	register char *optp;

	c = readch();
	if (c != '$')
		return (c);
	do {
		c = readch();
#		ifdef PI0
		ch = c;
#		endif
		switch (c) {
			case 'b':
				optp = &opt( 'b' );
				c = readch();
				if (!digit(c))
					return (c);
				*optp = c - '0';
				c = readch();
				break;
#		    ifdef PC
			case 'C':
				    /*
				     *	C is a replacement for t, fake it.
				     */
				c = 't';
				/* and fall through */
			case 'g':
#		    endif PC
			case 'k':
			case 'l':
			case 'n':
			case 'p':
			case 's':
			case 't':
			case 'u':
			case 'w':
			case 'z':
				optp = &opt( c );
				c = readch();
				if (c == '+') {
					*optp = 1;
					c = readch();
				} else if (c == '-') {
					*optp = 0;
					c = readch();
				} else {
					return (c);
				}
				break;
			default:
				    return (c);
			}
#ifdef PI0
		send(ROSET, ch, *optp);
#endif
	} while (c == ',');
	if ( opt( 'u' ) )
		setuflg();
	return (c);
}
