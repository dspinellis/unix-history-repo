#ifndef lint
static char sccsid[] = "@(#)cgetc.c	1.1	(Berkeley) %G%";
#endif not lint

# include	<stdio.h>

char	cgetc(i)
int	i;
{
	return ( getchar() );
}
