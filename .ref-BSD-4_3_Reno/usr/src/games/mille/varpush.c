/*
 * Copyright (c) 1982 Regents of the University of California.
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
static char sccsid[] = "@(#)varpush.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

# include	<paths.h>
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
#ifdef DEBUG
		if (Debug) {
			char	buf[80];
over:
			printf("Debug file:");
			gets(buf);
			if ((outf = fopen(buf, "w")) == NULL) {
				perror(buf);
				goto over;
			}
			if (strcmp(buf, _PATH_DEVNULL) != 0)
				setbuf(outf, (char *)NULL);
		}
#endif
	}
	else {
		temp = Topcard - Deck;
		write(file, (char *) &temp, sizeof temp);
	}
}
