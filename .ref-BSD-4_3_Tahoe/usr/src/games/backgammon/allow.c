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
static char sccsid[] = "@(#)allow.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include "back.h"

movallow ()  {

	register int	i, m, iold;
	int		r;

	if (d0)
		swap;
	m = (D0 == D1? 4: 2);
	for (i = 0; i < 4; i++)
		p[i] = bar;
	i = iold = 0;
	while (i < m)  {
		if (*offptr == 15)
			break;
		h[i] = 0;
		if (board[bar])  {
			if (i == 1 || m == 4)
				g[i] = bar+cturn*D1;
			else
				g[i] = bar+cturn*D0;
			if (r = makmove(i))  {
				if (d0 || m == 4)
					break;
				swap;
				movback (i);
				if (i > iold)
					iold = i;
				for (i = 0; i < 4; i++)
					p[i] = bar;
				i = 0;
			} else
				i++;
			continue;
		}
		if ((p[i] += cturn) == home)  {
			if (i > iold)
				iold = i;
			if (m == 2 && i)  {
				movback(i);
				p[i--] = bar;
				if (p[i] != bar)
					continue;
				else
					break;
			}
			if (d0 || m == 4)
				break;
			swap;
			movback (i);
			for (i = 0; i < 4; i++)
				p[i] = bar;
			i = 0;
			continue;
		}
		if (i == 1 || m == 4)
			g[i] = p[i]+cturn*D1;
		else
			g[i] = p[i]+cturn*D0;
		if (g[i]*cturn > home)  {
			if (*offptr >= 0)
				g[i] = home;
			else
				continue;
		}
		if (board[p[i]]*cturn > 0 && (r = makmove(i)) == 0)
			i++;
	}
	movback (i);
	return (iold > i? iold: i);
}
