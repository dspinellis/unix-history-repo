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
static char sccsid[] = "@(#)worms.c	5.5 (Berkeley) 6/27/88";
#endif /* not lint */

/*
 *
 *	 @@@        @@@    @@@@@@@@@@     @@@@@@@@@@@    @@@@@@@@@@@@
 *	 @@@        @@@   @@@@@@@@@@@@    @@@@@@@@@@@@   @@@@@@@@@@@@@
 *	 @@@        @@@  @@@@      @@@@   @@@@           @@@@ @@@  @@@@
 *	 @@@   @@   @@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	 @@@  @@@@  @@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	 @@@@ @@@@ @@@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	  @@@@@@@@@@@@   @@@@      @@@@   @@@            @@@  @@@   @@@
 *	   @@@@  @@@@     @@@@@@@@@@@@    @@@            @@@  @@@   @@@
 *	    @@    @@       @@@@@@@@@@     @@@            @@@  @@@   @@@
 *
 *				 Eric P. Scott
 *			  Caltech High Energy Physics
 *				 October, 1980
 *
 */
#include <sys/types.h>
#include <stdio.h>
#ifdef USG
#include <termio.h>
#else
#include <sgtty.h>
#endif
#include <signal.h>

static struct options {
	int nopts;
	int opts[3];
}
	normal[8] = {
	{ 3, { 7, 0, 1 } },
	{ 3, { 0, 1, 2 } },
	{ 3, { 1, 2, 3 } },
	{ 3, { 2, 3, 4 } },
	{ 3, { 3, 4, 5 } },
	{ 3, { 4, 5, 6 } },
	{ 3, { 5, 6, 7 } },
	{ 3, { 6, 7, 0 } }
},	upper[8] = {
	{ 1, { 1, 0, 0 } },
	{ 2, { 1, 2, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 2, { 4, 5, 0 } },
	{ 1, { 5, 0, 0 } },
	{ 2, { 1, 5, 0 } }
},
	left[8] = {
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 2, { 2, 3, 0 } },
	{ 1, { 3, 0, 0 } },
	{ 2, { 3, 7, 0 } },
	{ 1, { 7, 0, 0 } },
	{ 2, { 7, 0, 0 } }
},
	right[8] = {
	{ 1, { 7, 0, 0 } },
	{ 2, { 3, 7, 0 } },
	{ 1, { 3, 0, 0 } },
	{ 2, { 3, 4, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 2, { 6, 7, 0 } }
},
	lower[8] = {
	{ 0, { 0, 0, 0 } },
	{ 2, { 0, 1, 0 } },
	{ 1, { 1, 0, 0 } },
	{ 2, { 1, 5, 0 } },
	{ 1, { 5, 0, 0 } },
	{ 2, { 5, 6, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } }
},
	upleft[8] = {
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 1, { 3, 0, 0 } },
	{ 2, { 1, 3, 0 } },
	{ 1, { 1, 0, 0 } }
},
	upright[8] = {
	{ 2, { 3, 5, 0 } },
	{ 1, { 3, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 1, { 5, 0, 0 } }
},
	lowleft[8] = {
	{ 3, { 7, 0, 1 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 1, { 1, 0, 0 } },
	{ 2, { 1, 7, 0 } },
	{ 1, { 7, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } }
},
	lowright[8] = {
	{ 0, { 0, 0, 0 } },
	{ 1, { 7, 0, 0 } },
	{ 2, { 5, 7, 0 } },
	{ 1, { 5, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } },
	{ 0, { 0, 0, 0 } }
};

#define	cursor(c, r)	tputs(tgoto(CM, c, r), 1, fputchar)

static char	*TE;
static int	fputchar();

static char	flavor[] = {
	'O', '*', '#', '$', '%', '0'
};
static short	xinc[] = {
	1,  1,  1,  0, -1, -1, -1,  0
}, yinc[] = {
	-1,  0,  1,  1,  1,  0, -1, -1
};
static struct	worm {
	int orientation, head;
	short *xpos, *ypos;
} worm[40];

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern short ospeed;
	extern char *optarg, *UP;
	register int x, y, h, n;
	register struct worm *w;
	register struct options *op;
	register short *ip;
	register char *term;
	int CO, IN, LI, last, bottom, ch, length, number, trail, Wrap,
		onsig();
	short **ref;
	char *AL, *BC, *CM, *EI, *HO, *IC, *IM, *IP, *SR, *tcp,
		*field, tcb[100], *mp, *malloc(), *getenv(), *tgetstr(),
		*tgoto();
	long random();
#ifdef USG
	struct termio sg;
#else
	struct sgttyb sg;
#endif

	length = 16;
	number = 3;
	trail = ' ';
	field = NULL;
	while ((ch = getopt(argc, argv, "fl:n:t")) != EOF)
		switch((char)ch) {
		case 'f':
			field = "WORM";
			break;
		case 'l':
			if ((length = atoi(optarg)) < 2 || length > 1024) {
				fprintf(stderr, "%s: invalid length; range %d - %d.\n", *argv, 2, 1024);
				exit(1);
			}
			break;
		case 'n':
			if ((number = atoi(optarg)) < 1 || number > 40) {
				fprintf(stderr, "%s: invalid number of worms; range %d - %d.\n", *argv, 1, 40);
				exit(1);
			}
			break;
		case 't':
			trail = '.';
			break;
		case '?':
		default:
			fprintf(stderr, "usage: %s [-ft] [-length #] [-number #]\n", *argv);
			exit(1);
		}

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
	AL = tgetstr("al", &tcp);
	BC = tgetflag("bs") ? "\b" : tgetstr("bc", &tcp);
	if ((CO = tgetnum("co")) <= 0)
		CO = 80;
	last = CO - 1;
	EI = tgetstr("ei", &tcp);
	HO = tgetstr("ho", &tcp);
	IC = tgetstr("ic", &tcp);
	IM = tgetstr("im", &tcp);
	IN = tgetflag("in");
	IP = tgetstr("ip", &tcp);
	if ((LI = tgetnum("li")) <= 0)
		LI = 24;
	bottom = LI - 1;
	SR = tgetstr("sr", &tcp);
	TE = tgetstr("te", &tcp);
	UP = tgetstr("up", &tcp);
#ifdef USG
	ioctl(1, TCGETA, &sg);
	ospeed = sg.c_cflag&CBAUD;
#else
	gtty(1, &sg);
	ospeed = sg.sg_ospeed;
#endif
	Wrap = tgetflag("am");
	if (!(ip = (short *)malloc((u_int)(LI * CO * sizeof(short))))) {
		fprintf(stderr, "%s: out of memory\n", *argv);
		exit(1);
	}
	if (!(ref = (short **)malloc((u_int)(LI * sizeof(short *))))) {
		fprintf(stderr, "%s: out of memory\n", *argv);
		exit(1);
	}
	for (n = 0; n < LI; ++n) {
		ref[n] = ip;
		ip += CO;
	}
	for (ip = ref[0], n = LI * CO; --n >= 0;)
		*ip++ = 0;
	if (Wrap)
		ref[bottom][last] = 1;
	for (n = number, w = &worm[0]; --n >= 0; w++) {
		w->orientation = w->head = 0;
		if (!(ip = (short *)malloc((u_int)(length * sizeof(short))))) {
			fprintf(stderr, "%s: out of memory\n", *argv);
			exit(1);
		}
		w->xpos = ip;
		for (x = length; --x >= 0;)
			*ip++ = -1;
		if (!(ip = (short *)malloc((u_int)(length * sizeof(short))))) {
			fprintf(stderr, "%s: out of memory\n", *argv);
			exit(1);
		}
		w->ypos = ip;
		for (y = length; --y >= 0;)
			*ip++ = -1;
	}

	(void)signal(SIGHUP, onsig);
	(void)signal(SIGINT, onsig);
	(void)signal(SIGQUIT, onsig);
	(void)signal(SIGSTOP, onsig);
	(void)signal(SIGTSTP, onsig);
	(void)signal(SIGTERM, onsig);

	tputs(tgetstr("ti", &tcp), 1, fputchar);
	tputs(tgetstr("cl", &tcp), 1, fputchar);
	if (field) {
		register char *p = field;

		for (y = bottom; --y >= 0;) {
			for (x = CO; --x >= 0;) {
				fputchar(*p++);
				if (!*p)
					p = field;
			}
			if (!Wrap)
				fputchar('\n');
			(void)fflush(stdout);
		}
		if (Wrap) {
			if (IM && !IN) {
				for (x = last; --x > 0;) {
					fputchar(*p++);
					if (!*p)
						p = field;
				}
				y = *p++;
				if (!*p)
					p = field;
				fputchar(*p);
				if (BC)
					tputs(BC, 1, fputchar);
				else
					cursor(last - 1, bottom);
				tputs(IM, 1, fputchar);
				if (IC)
					tputs(IC, 1, fputchar);
				fputchar(y);
				if (IP)
					tputs(IP, 1, fputchar);
				tputs(EI, 1, fputchar);
			}
			else if (SR || AL) {
				if (HO)
					tputs(HO, 1, fputchar);
				else
					cursor(0, 0);
				if (SR)
					tputs(SR, 1, fputchar);
				else
					tputs(AL, LI, fputchar);
				for (x = CO; --x >= 0;) {
					fputchar(*p++);
					if (!*p)
						p = field;
				}
			}
			else for (x = last; --x >= 0;) {
				fputchar(*p++);
				if (!*p)
					p = field;
			}
		}
		else for (x = CO; --x >= 0;) {
			fputchar(*p++);
			if (!*p)
				p = field;
		}
	}
	for (;;) {
		(void)fflush(stdout);
		for (n = 0, w = &worm[0]; n < number; n++, w++) {
			if ((x = w->xpos[h = w->head]) < 0) {
				cursor(x = w->xpos[h] = 0,
				     y = w->ypos[h] = bottom);
				fputchar(flavor[n % 6]);
				ref[y][x]++;
			}
			else
				y = w->ypos[h];
			if (++h == length)
				h = 0;
			if (w->xpos[w->head = h] >= 0) {
				register int x1, y1;

				x1 = w->xpos[h];
				y1 = w->ypos[h];
				if (--ref[y1][x1] == 0) {
					cursor(x1, y1);
					if (trail)
						fputchar(trail);
				}
			}
			op = &(!x ? (!y ? upleft : (y == bottom ? lowleft : left)) : (x == last ? (!y ? upright : (y == bottom ? lowright : right)) : (!y ? upper : (y == bottom ? lower : normal))))[w->orientation];
			switch (op->nopts) {
			case 0:
				(void)fflush(stdout);
				abort();
				return;
			case 1:
				w->orientation = op->opts[0];
				break;
			default:
				w->orientation = op->opts[(int)random() % op->nopts];
			}
			cursor(x += xinc[w->orientation], y += yinc[w->orientation]);
			if (!Wrap || x != last || y != bottom)
				fputchar(flavor[n % 6]);
			ref[w->ypos[h] = y][w->xpos[h] = x]++;
		}
	}
}

static
onsig()
{
	tputs(TE, 1, fputchar);
	exit(0);
}

static
fputchar(c)
	char c;
{
	putchar(c);
}
