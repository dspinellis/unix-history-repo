/*
 * Copyright (c) 1980 The Regents of the University of California.
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
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tc3.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * tc3 [term]
 * Dummy program to test out termlib.
 * Input two numbers and it prints out the tgoto string generated.
 */
#include <stdio.h>
char buf[1024];
char *getenv(), *tgetstr();
char *rdchar();
char *tgoto();
char *CM;
char cmbuff[30];
char *x;
char *UP;
char *tgout;

main(argc, argv) char **argv; {
	char *p;
	int rc;
	int row, col;

	if (argc < 2)
		p = getenv("TERM");
	else
		p = argv[1];
	rc = tgetent(buf,p);
	x = cmbuff;
	UP = tgetstr("up", &x);
	printf("UP = %x = ", UP); pr(UP); printf("\n");
	if (UP && *UP==0)
		UP = 0;
	CM = tgetstr("cm", &x);
	printf("CM = "); pr(CM); printf("\n");
	for (;;) {
		if (scanf("%d %d", &row, &col) < 2)
			exit(0);
		tgout = tgoto(CM, row, col);
		pr(tgout);
		printf("\n");
	}
}

pr(p)
register char *p;
{
	for (; *p; p++)
		printf("%s", rdchar(*p));
}

/*
 * rdchar: returns a readable representation of an ASCII char, using ^ notation.
 */
#include <ctype.h>
char *rdchar(c)
char c;
{
	static char ret[4];
	register char *p;

	/*
	 * Due to a bug in isprint, this prints spaces as ^`, but this is OK
	 * because we want something to show up on the screen.
	 */
	ret[0] = ((c&0377) > 0177) ? '\'' : ' ';
	c &= 0177;
	ret[1] = isprint(c) ? ' ' : '^';
	ret[2] = isprint(c) ?  c  : c^0100;
	ret[3] = 0;
	for (p=ret; *p==' '; p++)
		;
	return (p);
}
