/*
 * Copyright (c) 1980, 1988 Regents of the University of California.
 * All rights reserved.
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
"@(#) Copyright (c) 1980, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tput.c	5.2 (Berkeley) %G%";
#endif /* not lint */

main()
{
	char *cp, *clbp, clbuf[100], tbuf[1024], *getenv(), *tgetstr();
	int putchar();

	if ((cp = getenv("TERM")) && tgetent(tbuf, cp) == 1) {
		clbp = clbuf;
		if (cp = tgetstr("cl", &clbp)) {
			tputs(cp, 1, putchar);
			exit(0);
		}
	}
	exit(1);
}
