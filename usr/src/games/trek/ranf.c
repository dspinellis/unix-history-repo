#ifndef lint
static char sccsid[] = "@(#)ranf.c	4.2	(Berkeley)	5/27/83";
#endif not lint

# include	<stdio.h>

ranf(max)
int	max;
{
	register int	t;

	if (max <= 0)
		return (0);
	t = rand() >> 5;
	return (t % max);
}


double franf()
{
	double		t;
	t = rand() & 077777;
	return (t / 32767.0);
}
