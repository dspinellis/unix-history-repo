/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)teach.c	5.1 (Berkeley) %G%";
#endif not lint

#include "back.h"

char	*hello[];
char	*list[];
char	*intro1[];
char	*intro2[];
char	*moves[];
char	*remove[];
char	*hits[];
char	*endgame[];
char	*doubl[];
char	*stragy[];
char	*prog[];
char	*lastch[];

extern char	ospeed;			/* tty output speed for termlib */

char *helpm[] = {
	"\nEnter a space or newline to roll, or",
	"     b   to display the board",
	"     d   to double",
	"     q   to quit\n",
	0
};

char *contin[] = {
	"",
	0
};

main (argc,argv)
int	argc;
char	**argv;

{
	register char	*s, *ts[];
	register int	i;

	signal (2,getout);
	if (gtty (0,&tty) == -1)			/* get old tty mode */
		errexit ("teachgammon(gtty)");
	old = tty.sg_flags;
#ifdef V7
	raw = ((noech = old & ~ECHO) | CBREAK);		/* set up modes */
#else
	raw = ((noech = old & ~ECHO) | RAW);		/* set up modes */
#endif
	ospeed = old.sg_ospeed;				/* for termlib */
	tflag = getcaps (getenv ("TERM"));
#ifdef V7
	while (*++argv != 0)
#else
	while (*++argv != -1)
#endif
		getarg (&argv);
	if (tflag)  {
		noech &= ~(CRMOD|XTABS);
		raw &= ~(CRMOD|XTABS);
		clear();
	}
	text (hello);
	text (list);
	i = text (contin);
	if (i == 0)
		i = 2;
	init();
	while (i)
		switch (i)  {
		
		case 1:
			leave();
		
		case 2:
			if (i = text(intro1))
				break;
			wrboard();
			if (i = text(intro2))
				break;
		
		case 3:
			if (i = text(moves))
				break;
		
		case 4:
			if (i = text(remove))
				break;
		
		case 5:
			if (i = text(hits))
				break;
		
		case 6:
			if (i = text(endgame))
				break;
		
		case 7:
			if (i = text(doubl))
				break;
		
		case 8:
			if (i = text(stragy))
				break;
		
		case 9:
			if (i = text(prog))
				break;
		
		case 10:
			if (i = text(lastch))
				break;
		}
	tutor();
}

leave()  {
	if (tflag)
		clear();
	else
		writec ('\n');
	fixtty(old);
	execl (EXEC,"backgammon",args,"n",0);
	writel ("Help! Backgammon program is missing\007!!\n");
	exit (-1);
}
