/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Mark Nudleman.\n\
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vecho.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * This dumb little program emulates the System V "echo" command,
 * to accomodate BSD systems which don't understand the \c escape,
 * meaning don't echo a newline.  BSD uses "echo -n".
 */

#include <stdio.h>

int putnl;

main(argc, argv)
	int argc;
	char *argv[];
{
	putnl = 1;
	while (--argc > 0)
	{
		vecho(*++argv);
		if (argc > 1)
			putchar(' ');
	}
	if (putnl)
		putchar('\n');
}

vecho(str)
	char *str;
{
	register char *s;

	for (s = str;  *s != '\0';  s++)
	{
		if (*s == '\\' && s[1] == 'c')
		{
			putnl = 0;
			return;
		}
		putchar(*s);
	}
}
