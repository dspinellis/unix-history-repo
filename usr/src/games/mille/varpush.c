/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)varpush.c	5.2 (Berkeley) %G%";
#endif not lint

# include	"mille.h"

/*
 * @(#)varpush.c	1.1 (Berkeley) 4/1/82
 */

int	read(), write();

/*
 *	push variables around via the routine func() on the file
 * channel file.  func() is either read or write.
 */
varpush(file, func)
reg int	file;
reg int	(*func)(); {

	int	temp;

	(*func)(file, (char *) &Debug, sizeof Debug);
	(*func)(file, (char *) &Finished, sizeof Finished);
	(*func)(file, (char *) &Order, sizeof Order);
	(*func)(file, (char *) &End, sizeof End);
	(*func)(file, (char *) &On_exit, sizeof On_exit);
	(*func)(file, (char *) &Handstart, sizeof Handstart);
	(*func)(file, (char *) &Numgos, sizeof Numgos);
	(*func)(file, (char *)  Numseen, sizeof Numseen);
	(*func)(file, (char *) &Play, sizeof Play);
	(*func)(file, (char *) &Window, sizeof Window);
	(*func)(file, (char *)  Deck, sizeof Deck);
	(*func)(file, (char *) &Discard, sizeof Discard);
	(*func)(file, (char *)  Player, sizeof Player);
	if (func == read) {
		read(file, (char *) &temp, sizeof temp);
		Topcard = &Deck[temp];
		if (Debug) {
			char	buf[80];
over:
			printf("Debug file:");
			gets(buf);
			if ((outf = fopen(buf, "w")) == NULL) {
				perror(buf);
				goto over;
			}
			if (strcmp(buf, "/dev/null") != 0)
				setbuf(outf, (char *)NULL);
		}
	}
	else {
		temp = Topcard - Deck;
		write(file, (char *) &temp, sizeof temp);
	}
}

