/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tttermcap.c	3.12 (Berkeley) %G%";
#endif /* not lint */

#include "tt.h"

char *tgetstr();
char *tgoto();
char *malloc();

tttputc(c)
{
	ttputc(c);
}

ttxputc(c)
{
	*tt_strp++ = c;
}

struct tt_str *
tttgetstr(str)
	char *str;
{
	register struct tt_str *s;

	if ((str = tgetstr(str, &tt_strp)) == 0)
		return 0;
	if ((s = (struct tt_str *) malloc(sizeof *s)) == 0)
		return 0;
	s->ts_str = str;
	s->ts_n = tt_strp - s->ts_str - 1;
	return s;
}

struct tt_str *
ttxgetstr(str)
	char *str;
{
	register struct tt_str *s;
	char buf[100];
	char *bufp = buf;

	if (tgetstr(str, &bufp) == 0)
		return 0;
	if ((s = (struct tt_str *) malloc(sizeof *s)) == 0)
		return 0;
	s->ts_str = tt_strp;
	tputs(buf, 1, ttxputc);
	s->ts_n = tt_strp - s->ts_str;
	*tt_strp++ = 0;
	return s;
}

tttgoto(s, col, row)
	struct tt_str *s;
{
	register char *p = s->ts_str;

	ttputs(tgoto(p, col, row));
	for (p += s->ts_n; *--p == 0;)
		ttputc(0);
}

ttpgoto(s, col, row, n)
	struct tt_str *s;
{

	tputs(tgoto(s->ts_str, col, row), n, tttputc);
}

ttstrcmp(a, b)
	register struct tt_str *a, *b;
{
	int n, r;

	if (r = bcmp(a->ts_str, b->ts_str,
			(n = a->ts_n - b->ts_n) < 0 ? a->ts_n : b->ts_n))
		return r;
	return n;
}
