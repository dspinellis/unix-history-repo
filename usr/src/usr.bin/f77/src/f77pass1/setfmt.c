/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)setfmt.c	5.1 (Berkeley) 6/7/85";
#endif not lint

#include "defs.h"
#include "format.h"

#define GLITCH '\2'

extern char *fmtptr, *s_init;
extern int fmt_strings;

setfmt(lp)
struct Labelblock *lp;
{
int fmt_len, frmt_str_lab;
char *s, *lexline();

s = lexline(&fmt_len);
preven(ALILONG);
prlabel(asmfile, lp->labelno);
if( pars_f(s) != 0) {
	err_fmt(fmt_len);
} else {
	fprintf( asmfile, ".word %d\n", FMT_COMP ); /* precompiled format */
	fprintf( asmfile, ".word %d\n", pc );
	frmt_str_lab = newlabel();	/* to mark loc of format string */
	prcona( asmfile, frmt_str_lab );
	putshrts( syl, sizeof(struct syl)/SZSHORT*pc );
}
if( fmt_strings ) pruse( asmfile, USEINIT) ;
prlabel(asmfile, frmt_str_lab );
putstr(asmfile, s, fmt_len);
if( fmt_strings ) pruse( asmfile, USECONST) ;
flline();
}

LOCAL
putshrts( words, count )
short *words;
{
    while ( count-- > 0 )
	fprintf( asmfile, ".word %d\n", *words++ );
}

static char *fmt_seg_ptr;

LOCAL
err_fmt(fmt_len)
{
	int i;
	char *ep, *end_ptr = s_init+fmt_len;
	char fmt_seg[50];

	fmt_seg_ptr = fmt_seg;
	i = fmtptr - s_init;
	ep = fmtptr - (i<25?i:25);
	i = i + 5;
	while(i && *ep && ep<end_ptr)
	{
		if(ep==fmtptr) save_char('|',fmt_seg);
		save_char(*ep);
		ep++; i--;
	}
	*fmt_seg_ptr++ = '\0';
	errstr("Error in format:  %s", fmt_seg);
}

LOCAL
save_char(c)
int	c;
{
	c &= 0177;
	if( c == GLITCH ) {
		c = '"';
	} else if (c < ' ' || c == 0177) {
		*fmt_seg_ptr++ ='^';
		c ^= 0100;
	} 
	*fmt_seg_ptr++ = c;
}
