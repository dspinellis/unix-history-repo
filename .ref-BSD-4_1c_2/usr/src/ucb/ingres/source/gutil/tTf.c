# include	<sccs.h>

SCCSID(@(#)tTf.c	7.1	2/5/81)

tTf(m, n)
{
	extern char	tTany;
	extern short	*tT;

	if (!tTany)
		return (0);
	if (n < 0)
		return (tT[m]);
	else
		return ((tT[m] >> n) & 01);
}
