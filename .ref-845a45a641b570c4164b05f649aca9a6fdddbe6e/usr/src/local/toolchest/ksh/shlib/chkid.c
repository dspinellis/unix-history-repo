/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)chkid.c	1.1 */

/*
 *   CHKID.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   CHKID (NAME)
 *
 *        Verify the validity of NAME as a namid, and return its
 *        hash value.
 *
 *
 *
 *   See Also:  gettree(III)
 */

#ifdef KSHELL
#include       "shtype.h"
#else
#include       <ctype.h>
#endif	/* KSHELL */

#define BITSPBYTE     8
#define BITSPWORD     8*sizeof(unsigned)

/*
 *   CHKID (NAME)
 *
 *        char *NAME;
 *
 *   Check the validity of NAME as a namid, and return its hash value.
 *
 *   NAME should begin with a printable character, and should
 *   contain only alphanumerics.  If NAME does not meet these
 *   requirements, '0' is returned.
 *   The characters *, [, $ and ` cannot be used.
 */

unsigned chkid(name)
char *name;
{
	register char *cp = name;
	register unsigned i;
	register unsigned c = *cp;
	if(!isprint(c))
		return(0);
	if(expchar(c))
		return(0);
	i = c;
	while(c= *++cp)
	{
#ifdef KSHELL
		if(!isalnum(c))
#else
		if ((!isalnum (c)) && (c != '_'))
#endif	/* KSHELL */
			return(0);
		i = ((i<<1)|(i>>(BITSPWORD-1))) ^ c;
	}
	return (i | (1 << (BITSPWORD - 1)));
}

