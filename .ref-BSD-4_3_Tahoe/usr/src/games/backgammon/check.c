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
static char sccsid[] = "@(#)check.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include "back.h"

getmove ()  {
	register int	i, c;

	c = 0;
	for (;;)  {
		i = checkmove(c);

		switch (i)  {
		case -1:
			if (movokay(mvlim))  {
				if (tflag)
					curmove (20,0);
				else
					writec ('\n');
				for (i = 0; i < mvlim; i++)
					if (h[i])
						wrhit(g[i]);
				nexturn();
				if (*offopp == 15)
					cturn *= -2;
				if (tflag && pnum)
					bflag = pnum;
				return;
			}

		case -4:
		case 0:
			if (tflag)
				refresh();
			if (i != 0 && i != -4)
				break;
			if (tflag)
				curmove (20,0);
			else
				writec ('\n');
			writel (*Colorptr);
			if (i == -4)
				writel (" must make ");
			else
				writel (" can only make ");
			writec (mvlim+'0');
			writel (" move");
			if (mvlim > 1)
				writec ('s');
			writec ('.');
			writec ('\n');
			break;

		case -3:
			if (quit())
				return;
		}

		if (! tflag)
			proll ();
		else  {
			curmove (cturn == -1? 18: 19,39);
			cline ();
			c = -1;
		}
	}
}

movokay (mv)
register int	mv;

{
	register int	i, m;

	if (d0)
		swap;

	for (i = 0; i < mv; i++)  {

		if (p[i] == g[i])  {
			moverr (i);
			curmove (20,0);
			writel ("Attempt to move to same location.\n");
			return (0);
		}

		if (cturn*(g[i]-p[i]) < 0)  {
			moverr (i);
			curmove (20,0);
			writel ("Backwards move.\n");
			return (0);
		}

		if (abs(board[bar]) && p[i] != bar)  {
			moverr (i);
			curmove (20,0);
			writel ("Men still on bar.\n");
			return (0);
		}

		if ( (m = makmove(i)) )  {
			moverr (i);
			switch (m)  {

			case 1:
				writel ("Move not rolled.\n");
				break;

			case 2:
				writel ("Bad starting position.\n");
				break;

			case 3:
				writel ("Destination occupied.\n");
				break;

			case 4:
				writel ("Can't remove men yet.\n");
			}
			return (0);
		}
	}
	return (1);
}
