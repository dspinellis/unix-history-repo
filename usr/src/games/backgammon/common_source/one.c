static char sccsid[] = "	one.c	1.1	82/05/11	";

#include "back.h"

makmove (i)
register int	i;
 
{
	register int	n, d;
	int		max;

	d = d0;
	n = abs(g[i]-p[i]);
	max = (*offptr < 0? 7: last());
	if (board[p[i]]*cturn <= 0)
		return (checkd(d)+2);
	if (g[i] != home && board[g[i]]*cturn < -1)
		return (checkd(d)+3);
	if (i || D0 == D1)  {
		if (n == max? D1 < n: D1 != n)
			return (checkd(d)+1);
	} else  {
		if (n == max? D0 < n && D1 < n: D0 != n && D1 != n)
			return (checkd(d)+1);
		if (n == max? D0 < n: D0 != n)  {
			if (d0)
				return (checkd(d)+1);
			swap;
		}
	}
	if (g[i] == home && *offptr < 0)
		return (checkd(d)+4);
	h[i] = 0;
	board[p[i]] -= cturn;
	if (g[i] != home)  {
		if (board[g[i]] == -cturn)  {
			board[home] -= cturn;
			board[g[i]] = 0;
			h[i] = 1;
			if (abs(bar-g[i]) < 7)  {
				(*inopp)--;
				if (*offopp >= 0)
					*offopp -= 15;
			}
		}
		board[g[i]] += cturn;
		if (abs(home-g[i]) < 7 && abs(home-p[i]) > 6)  {
			(*inptr)++;
			if (*inptr+*offptr == 0)
				*offptr += 15;
		}
	} else {
		(*offptr)++;
		(*inptr)--;
	}
	return (0);
}

moverr (i)
register int	i;

{
	register int	j;

	if (tflag)
		curmove (20,0);
	else
		writec ('\n');
	writel ("Error:  ");
	for (j = 0; j <= i; j++)  {
		wrint (p[j]);
		writec ('-');
		wrint (g[j]);
		if (j < i)
			writec (',');
	}
	writel ("... ");
	movback (i);
}


checkd (d)
register int	d;

{
	if (d0 != d)
		swap;
	return (0);
}

last ()  {
	register int	i;

	for (i = home-6*cturn; i != home; i += cturn)
		if (board[i]*cturn > 0)
			return (abs(home-i));
}

movback (i)
register int	i;

{
	register int	j;

	for (j = i-1; j >= 0; j--)
		backone(j);
}

backone (i)
register int	i;

{
	board[p[i]] += cturn;
	if (g[i] != home)  {
		board[g[i]] -= cturn;
		if (abs(g[i]-home) < 7 && abs(p[i]-home) > 6)  {
			(*inptr)--;
			if (*inptr+*offptr < 15 && *offptr >= 0)
				*offptr -= 15;
		}
	} else  {
		(*offptr)--;
		(*inptr)++;
	}
	if (h[i])  {
		board[home] += cturn;
		board[g[i]] = -cturn;
		if (abs(bar-g[i]) < 7)  {
			(*inopp)++;
			if (*inopp+*offopp == 0)
				*offopp += 15;
		}
	}
}
