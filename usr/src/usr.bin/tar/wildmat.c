/* Wildcard matching routines.
   Copyright (C) 1988 Free Software Foundation

This file is part of GNU Tar.

GNU Tar is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Tar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Tar; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * @(#)wildmat.c 1.3 87/11/06
 *
From: rs@mirror.TMC.COM (Rich Salz)
Newsgroups: net.sources
Subject: Small shell-style pattern matcher
Message-ID: <596@mirror.TMC.COM>
Date: 27 Nov 86 00:06:40 GMT

There have been several regular-expression subroutines and one or two
filename-globbing routines in mod.sources.  They handle lots of
complicated patterns.  This small piece of code handles the *?[]\
wildcard characters the way the standard Unix(tm) shells do, with the
addition that "[^.....]" is an inverse character class -- it matches
any character not in the range ".....".  Read the comments for more
info.

For my application, I had first ripped off a copy of the "glob" routine
from within the find source, but that code is bad news:  it recurses
on every character in the pattern.  I'm putting this replacement in the
public domain.  It's small, tight, and iterative.  Compile with -DTEST
to get a test driver.  After you're convinced it works, install in
whatever way is appropriate for you.

I would like to hear of bugs, but am not interested in additions; if I
were, I'd use the code I mentioned above.
*/
/*
**  Do shell-style pattern matching for ?, \, [], and * characters.
**  Might not be robust in face of malformed patterns; e.g., "foo[a-"
**  could cause a segmentation violation.
**
**  Written by Rich $alz, mirror!rs, Wed Nov 26 19:03:17 EST 1986.
*/

/*
 * Modified 6Nov87 by John Gilmore (hoptoad!gnu) to return a "match"
 * if the pattern is immediately followed by a "/", as well as \0.
 * This matches what "tar" does for matching whole subdirectories.
 *
 * The "*" code could be sped up by only recursing one level instead
 * of two for each trial pattern, perhaps, and not recursing at all
 * if a literal match of the next 2 chars would fail.
 */
#define TRUE		1
#define FALSE		0


static int
Star(s, p)
    register char	*s;
    register char	*p;
{
    while (wildmat(s, p) == FALSE)
	if (*++s == '\0')
	    return(FALSE);
    return(TRUE);
}


int
wildmat(s, p)
    register char	*s;
    register char	*p;
{
    register int 	 last;
    register int 	 matched;
    register int 	 reverse;

    for ( ; *p; s++, p++)
	switch (*p) {
	    case '\\':
		/* Literal match with following character; fall through. */
		p++;
	    default:
		if (*s != *p)
		    return(FALSE);
		continue;
	    case '?':
		/* Match anything. */
		if (*s == '\0')
		    return(FALSE);
		continue;
	    case '*':
		/* Trailing star matches everything. */
		return(*++p ? Star(s, p) : TRUE);
	    case '[':
		/* [^....] means inverse character class. */
		if (reverse = p[1] == '^')
		    p++;
		for (last = 0400, matched = FALSE; *++p && *p != ']'; last = *p)
		    /* This next line requires a good C compiler. */
		    if (*p == '-' ? *s <= *++p && *s >= last : *s == *p)
			matched = TRUE;
		if (matched == reverse)
		    return(FALSE);
		continue;
	}

    /* For "tar" use, matches that end at a slash also work. --hoptoad!gnu */
    return(*s == '\0' || *s == '/');
}


#ifdef	TEST
#include <stdio.h>

extern char	*gets();


main()
{
    char	 pattern[80];
    char	 text[80];

    while (TRUE) {
	printf("Enter pattern:  ");
	if (gets(pattern) == NULL)
	    break;
	while (TRUE) {
	    printf("Enter text:  ");
	    if (gets(text) == NULL)
		exit(0);
	    if (text[0] == '\0')
		/* Blank line; go back and get a new pattern. */
		break;
	    printf("      %d\n", wildmat(text, pattern));
	}
    }
    exit(0);
}
#endif	/* TEST */
