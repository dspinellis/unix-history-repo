/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tc3.c	5.1 (Berkeley) %G%";
#endif not lint

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
