/*
 *
 * zle_vi.c - vi-specific functions
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define ZLE
#include "zsh.h"


static void startvichange(im)
int im;
{
	insmode = im;
	if (vichgbuf) free(vichgbuf);
	vichgbuf = zalloc(vichgbufsz = 16);
	vichgbuf[0] = c;
	vichgbufptr = 1;
	vichgflag = 1;
	viinsbegin = cs;
}

static void startvitext(im)
{
	startvichange(im);
	bindtab = mainbindtab;
	undoing = 0;
}

int vigetkey() /**/
{
int ch;

	if ((ch = getkey(0)) == -1)
		return 0;
	if (ch == 22)
		{
		if ((ch = getkey(0)) == -1)
			return 0;
		return ch;
		}
	else if (ch == 27)
		return 0;
	return ch;
}

int getvirange(wf) /**/
int wf;
{
int k2,t0,startline,endline;

	startline = findbol();
	endline = findeol();
	for (;;) {
		k2 = getkeycmd();
		if (k2 == -1) {
			feep();
			return -1;
		}
		if (zlecmds[k2].flags & ZLE_ARG)
			(*zlecmds[k2].func)();
		else
			break;
	}
	if (k2 == bindk) {
		findline(&cs,&t0);
		return (t0 == ll) ? t0 : t0+1;
	}
	if (!(zlecmds[k2].flags & ZLE_MOVEMENT)) {
		feep();
		return -1;
	}
	t0 = cs;

	virangeflag = 1;
	wordflag = wf;
	(*zlecmds[k2].func)();
	wordflag = virangeflag = 0;
	if (cs == t0) {
		feep();
		return -1;
	}
	if (startline != findbol()) {
		if (zlecmds[k2].flags & ZLE_LINEMOVE) {
			if (cs < t0) {
				cs = startline;
				t0 = findeol()+1;
			} else {
				t0 = startline;
				cs = findeol()+1;
			}
		} else {
			if (cs < startline) cs = startline;
			else if (cs >= endline) cs = endline-1;
		}
	}
	if (cs > t0) {
		k2 = cs;
		cs = t0;
		t0 = k2;
	}
	return t0;
}

void viaddnext() /**/
{
	if (cs != ll)
		cs++;
	startvitext(1);
}

void viaddeol() /**/
{
	cs = findeol();
	startvitext(1);
}

void viinsert() /**/
{
	startvitext(1);
}

void viinsertbol() /**/
{
	cs = findbol();
	startvitext(1);
}

void videlete() /**/
{
int c2;

	startvichange(1);
	if ((c2 = getvirange(0)) == -1)
		{ vichgflag = 0; return; }
	forekill(c2-cs,0);
	vichgflag = 0;
}

void vichange() /**/
{
int c2;

	startvichange(1);
	if ((c2 = getvirange(1)) == -1)
		{ vichgflag = 0; return; }
	forekill(c2-cs,0);
	bindtab = mainbindtab;
	undoing = 0;
}

void visubstitute() /**/
{
	if (mult < 0) return;
	if (findeol()-cs < mult) mult = findeol()-cs;
	if (mult) {
		foredel(mult);
		startvitext(1);
	}
}

void vichangeeol() /**/
{
	killline();
	startvitext(1);
}

void vichangewholeline() /**/
{
int cq;

	findline(&cs,&cq);
	foredel(cq-cs);
	startvitext(1);
}

void viyank() /**/
{
int c2;

	if ((c2 = getvirange(0)) == -1) return;
	cut(cs,c2-cs,0);
}

void viyankeol() /**/
{
int x = findeol();

	if (x == cs)
		feep();
	else
		cut(cs,x-cs,0);
}

void vireplace() /**/
{
	startvitext(0);
}

void vireplacechars() /**/
{
int ch;

	if (mult < 0) return;
	if (mult+cs > ll) {
		feep();
		return;
	}
	startvichange(1);
	if (ch = vigetkey()) while (mult--) line[cs++] = ch;
	vichgflag = 0;
	cs--;
}

void vicmdmode() /**/
{
	bindtab = altbindtab;
	if (cs) cs--;
	undoing = 1;
	if (vichgflag) vichgflag = 0;
}

void viopenlinebelow() /**/
{
	cs = findeol();
	spaceinline(1);
	line[cs++] = '\n';
	startvitext(1);
}

void viopenlineabove() /**/
{
	cs = findbol();
	spaceinline(1);
	line[cs] = '\n';
	startvitext(1);
}

void vioperswapcase() /**/
{
int c2;

	if ((c2 = getvirange(0)) == -1)
		return;
	while (cs < c2)
		{
		int ch = line[cs];

		if (islower(ch))
			ch = tuupper(ch);
		else if (isupper(ch))
			ch = tulower(ch);
		line[cs++] = ch;
		}
}

void virepeatchange() /**/
{
	if (!vichgbuf || bindtab == mainbindtab || vichgflag) feep();
	else ungetkeys(vichgbuf,vichgbufptr);
}

void viindent() /**/
{
int c2,endcs,t0,rmult;

	if (mult < 0) { mult = -mult; viunindent(); return; }
	rmult = mult;
	if ((c2 = getvirange(0)) == -1)
		return;
	if (cs != findbol()) { feep(); return; }
	endcs = cs+rmult;
	while (cs < c2) {
		spaceinline(rmult);
		for (t0 = 0; t0 != rmult; t0++) line[cs++] = '\t';
		cs = findeol()+1;
	}
	cs = endcs;
}

void viunindent() /**/
{
int c2,endcs,t0,rmult;

	rmult = mult;
	if (mult < 0) { mult = -mult; viindent(); return; }
	if ((c2 = getvirange(0)) == -1)
		return;
	if (cs != findbol()) { feep(); return; }
	endcs = cs;
	while (cs < c2) {
		for (t0 = 0; t0 != rmult && line[cs] == '\t'; t0++) foredel(1);
		cs = findeol()+1;
	}
	cs = endcs;
}
