#ifndef lint
static char sccsid[] = "@(#)subr.c	1.2 (CWI) 85/10/02";
#endif lint


/*
 * variuos subroutines for f.i drawing horizontal lines
 *
 * We could stuff more routines in this file which are used in various modules
 * of tbl, like reg in misc.c etc. We leave this for later.
 */

#include "defs.h"
#include "ext.h"

ctype(il, ic)
{

	if(instead[il])
		return(0);
	if(fullbot[il])
		return(0);
	il = stynum[il];
	return(style[il][ic]);
}

fspan(i, c)
{

	c++;
	return(c < ncol && ctype (i, c) == 's');
}

lspan(i, c)
{
	register int k;

	if(ctype(i, c) != 's')
		return(0);
	c++;
	if(c < ncol && ctype(i, c) == 's')
		return(0);
	for(k = 0; ctype(i, --c) == 's'; k++)
		;
	return(k);
}

ctspan(i, c)
{
	register int k;

	c++;
	for(k = 1; c < ncol && ctype(i, c) == 's'; k++)
		c++;
	return(k);
}

tohcol(ic)
{
	if(ic == 0)
		printf("\\h'|0'");
	else
		printf("\\h'(|\\n(%2su+|\\n(%2su)/2u'", reg(ic, CLEFT),
							reg(ic - 1, CRIGHT));
}

/*
 * Return true if every element in line i is horizontal
 * Also at least one must be horizontal
 */
allh(i)
{
	register int c, one, k;

	if(fullbot[i])
		return(1);
	for(one = c = 0; c < ncol; c++){
		k = thish(i, c);
		if(k == 0)
			return(0);
		if(k == 1)
			continue;
		one = 1;
	}
	return(one);
}

thish(i, c)
{
	register int t;
	register char *s;
	register struct colstr *pc;

	if(c < 0)
		return(0);
	if(i < 0)
		return(0);
	t = ctype(i, c);
	if(t == '_' || t == '-')
		return('-');
	if(t == '=')
		return('=');
	if(t == '^')
		return(1);
	if(fullbot[i])
		return(fullbot[i]);
	if(t == 's')
		return(thish (i, c - 1));
	if(t == 0)
		return(1);
	pc = &table[i][c];
	s = (t == 'a' ? pc -> rcol : pc -> col);
	if(s == 0 || (point(s) && *s == 0))
		return(1);
	if(vspen(s))
		return(1);
	if(t = barent(s))
		return(t);
	return(0);
}


prefix(small, big)
char *small, *big;
{
	register int c;
	while ((c= *small++) == *big++)
		if (c==0) return(1);
	return(c==0);
}
