/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: retab.c,v 1.1 84/07/04 17:57:16 timo Exp $ */

/* 
 * retab [ [-ain] [-t N] ] [ file ]
 *
 * replace all tabs by appropriate number of spaces,
 * and replace these spaces by optimal number of tabs.
 *
 * -i: replace only initial spaces
 * -n: no spaces are replaced
 * -a: all spaces are replaced (Default)
 * -t N: tab positions are multiples of N; default N=8
 *
 */

#define	OPTIONS	"-[a|i|n] -tN"
#include	"par.h"
#include	<ctype.h>

#define	NL	'\n'
#define	TAB	'\t'
#define	BS	'\b'
#define	SP	' '

int tabwidth = 8;
int rpos, wpos;
char opt = 'a';

int
options(c, v) char *v[];	{
	char ch = v[0][1];

	VOID(c);
	switch (ch)	{
	case 'a':
	case 'i':
	case 'n':
		opt = ch;
		return 1;
	case 't':
		tabwidth = atoi(&v[0][2]);
		if (tabwidth <= 1)
			return ERR;
		return 1;
	default:
		return ERR;
	}
}

process()	{
	register int ch;

	rpos = wpos = 0;
	while ((ch = getc(ifile)) NE EOF)
	switch (ch)	{
	case NL:
		rpos = 0;
		wpos = 0;
		putchar(NL);
		break;
	case BS:
		rpos--;
		break;
	case TAB:
		rpos = ntab(rpos);
		break;
	case SP:
		rpos++;
		break;
	default:
		if (isprint(ch))
			rpos++;
		outchar(ch);
		break;
	}
}

outchar(ch) char ch;	{

	while (wpos > rpos-1)	{
		putchar(BS);
		wpos--;
	}
	if (opt EQ 'a' || (opt EQ 'i' && wpos EQ 0))
		while (rpos - wpos > 2 && ntab(wpos) < rpos)	{
			putchar(ntab(wpos) EQ wpos+2 ? SP : TAB);
			wpos = ntab(wpos);
		}
	while (wpos < rpos-1)	{
		putchar(SP);
		wpos++;
	}
	putchar(ch);
	wpos++;
}

int
ntab(n)	{
	return (n + tabwidth) / tabwidth * tabwidth;
}
