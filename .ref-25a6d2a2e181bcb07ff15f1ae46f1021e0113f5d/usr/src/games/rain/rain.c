/*
 * Copyright (c) 1980 Regents of the University of California.
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
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rain.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * rain 11/3/1980 EPS/CITHEP
 * cc rain.c -o rain -O -ltermlib
 */

#include <sys/types.h>
#include <stdio.h>
#ifdef USG
#include <termio.h>
#else
#include <sgtty.h>
#endif
#include <signal.h>

#define	cursor(c, r)	tputs(tgoto(CM, c, r), 1, fputchar)

#ifdef USG
static struct termio sg, old_tty;
#else
static struct sgttyb sg, old_tty;
#endif

int	fputchar();
char	*LL, *TE, *tgoto();

main(argc, argv)
	int argc;
	char **argv;
{
	extern short ospeed;
	extern char *UP;
	register int x, y, j;
	register char *CM, *BC, *DN, *ND, *term;
	char *TI, *tcp, *mp, tcb[100],
		*malloc(), *getenv(), *strcpy(), *tgetstr();
	long cols, lines, random();
	int xpos[5], ypos[5], onsig();

	if (!(term = getenv("TERM"))) {
		fprintf(stderr, "%s: TERM: parameter not set\n", *argv);
		exit(1);
	}
	if (!(mp = malloc((u_int)1024))) {
		fprintf(stderr, "%s: out of space.\n", *argv);
		exit(1);
	}
	if (tgetent(mp, term) <= 0) {
		fprintf(stderr, "%s: %s: unknown terminal type\n", *argv, term);
		exit(1);
	}
	tcp = tcb;
	if (!(CM = tgetstr("cm", &tcp))) {
		fprintf(stderr, "%s: terminal not capable of cursor motion\n", *argv);
		exit(1);
	}
	if (!(BC = tgetstr("bc", &tcp)))
		BC = "\b";
	if (!(DN = tgetstr("dn", &tcp)))
		DN = "\n";
	if (!(ND = tgetstr("nd", &tcp)))
		ND = " ";
	if ((cols = tgetnum("co")) == -1)
		cols = 80;
	if ((lines = tgetnum("li")) == -1)
		lines = 24;
	cols -= 4;
	lines -= 4;
	TE = tgetstr("te", &tcp);
	TI = tgetstr("ti", &tcp);
	UP = tgetstr("up", &tcp);
	if (!(LL = tgetstr("ll", &tcp))) {
		if (!(LL = malloc((u_int)10))) {
			fprintf(stderr, "%s: out of space.\n", *argv);
			exit(1);
		}
		(void)strcpy(LL, tgoto(CM, 0, 23));
	}
#ifdef USG
	ioctl(1, TCGETA, &sg);
	ospeed = sg.c_cflag&CBAUD;
#else
	gtty(1, &sg);
	ospeed = sg.sg_ospeed;
#endif
	(void)signal(SIGHUP, onsig);
	(void)signal(SIGINT, onsig);
	(void)signal(SIGQUIT, onsig);
	(void)signal(SIGSTOP, onsig);
	(void)signal(SIGTSTP, onsig);
	(void)signal(SIGTERM, onsig);
#ifdef USG
	ioctl(1, TCGETA, &old_tty);	/* save tty bits for exit */
	ioctl(1, TCGETA, &sg);
	sg.c_iflag &= ~ICRNL;
	sg.c_oflag &= ~ONLCR;
	sg.c_lflag &= ~ECHO;
	ioctl(1, TCSETAW, &sg);
#else
	gtty(1, &old_tty);		/* save tty bits for exit */
	gtty(1, &sg);
	sg.sg_flags &= ~(CRMOD|ECHO);
	stty(1, &sg);
#endif
	if (TI)
		tputs(TI, 1, fputchar);
	tputs(tgetstr("cl", &tcp), 1, fputchar);
	(void)fflush(stdout);
	for (j = 4; j >= 0; --j) {
		xpos[j] = random() % cols + 2;
		ypos[j] = random() % lines + 2;
	}
	for (j = 0;;) {
		x = random() % cols + 2;
		y = random() % lines + 2;
		cursor(x, y);
		fputchar('.');
		cursor(xpos[j], ypos[j]);
		fputchar('o');
		if (!j--)
			j = 4;
		cursor(xpos[j], ypos[j]);
		fputchar('O');
		if (!j--)
			j = 4;
		cursor(xpos[j], ypos[j] - 1);
		fputchar('-');
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputs("|.|", stdout);
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputchar('-');
		if (!j--)
			j = 4;
		cursor(xpos[j], ypos[j] - 2);
		fputchar('-');
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputs("/ \\", stdout);
		cursor(xpos[j] - 2, ypos[j]);
		fputs("| O |", stdout);
		cursor(xpos[j] - 1, ypos[j] + 1);
		fputs("\\ /", stdout);
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputchar('-');
		if (!j--)
			j = 4;
		cursor(xpos[j], ypos[j] - 2);
		fputchar(' ');
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputchar(' ');
		tputs(ND, 1, fputchar);
		fputchar(' ');
		cursor(xpos[j] - 2, ypos[j]);
		fputchar(' ');
		tputs(ND, 1, fputchar);
		fputchar(' ');
		tputs(ND, 1, fputchar);
		fputchar(' ');
		cursor(xpos[j] - 1, ypos[j] + 1);
		fputchar(' ');
		tputs(ND, 1, fputchar);
		fputchar(' ');
		tputs(DN, 1, fputchar);
		tputs(BC, 1, fputchar);
		tputs(BC, 1, fputchar);
		fputchar(' ');
		xpos[j] = x;
		ypos[j] = y;
		(void)fflush(stdout);
	}
}

static
onsig()
{
	tputs(LL, 1, fputchar);
	if (TE)
		tputs(TE, 1, fputchar);
	(void)fflush(stdout);
#ifdef USG
	ioctl(1, TCSETAW, &old_tty);
#else
	stty(1, &old_tty);
#endif
	exit(0);
}

static
fputchar(c)
	char c;
{
	putchar(c);
}
