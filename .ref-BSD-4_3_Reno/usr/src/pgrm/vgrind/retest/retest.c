/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
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

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)retest.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

#include <ctype.h>

int l_onecase = 0;
char * _start;
char * _escaped;
char * convexp();
char * expmatch();
main()
{
    char reg[132];
    char *ireg;
    char str[132];
    char *match;
    char matstr[132];
    char c;

    while (1) {
	printf ("\nexpr: ");
	scanf ("%s", reg);
	ireg = convexp(reg);
	match = ireg;
	while(*match) {
	    switch (*match) {

	    case '\\':
	    case '(':
	    case ')':
	    case '|':
		printf ("%c", *match);
		break;

	    default:
		if (isalnum(*match))
		    printf("%c", *match);
		else
		    printf ("<%03o>", *match);
		break;
	    }
	    match++;
	}
	printf("\n");
	getchar();
	while(1) {
	    printf ("string: ");
	    match = str;
	    while ((c = getchar()) != '\n')
		*match++ = c;
	    *match = 0;
	    if (str[0] == '#')
		break;
	    matstr[0] = 0;
	    _start = str;
	    _escaped = 0;
	    match = expmatch (str, ireg, matstr);
	    if (match == 0)
		printf ("FAILED\n");
	    else
		printf ("match\nmatstr = %s\n", matstr);
	}

    }
}
