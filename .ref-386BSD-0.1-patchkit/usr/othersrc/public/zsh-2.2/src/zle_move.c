/*
 *
 * zle_move.c - editor movement
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

void beginningofline() /**/
{
	if (mult < 0) { mult = -mult; endofline(); return; }
	while (mult--) {
		if (cs == 0)
			return;
		if (line[cs-1] == '\n')
			if (!--cs)
				return;
		while (cs && line[cs-1] != '\n') cs--;
	}
}

void endofline() /**/
{
	if (mult < 0) { mult = -mult; beginningofline(); return; }
	while (mult--) {
		if (cs >= ll) {
			cs = ll;
			return;
		}
		if (line[cs] == '\n')
			if (++cs == ll)
				return;
		while (cs != ll && line[cs] != '\n') cs++;
	}
}

void beginningoflinehist() /**/
{
	if (mult < 0) { mult = -mult; endoflinehist(); return; }
	while (mult) {
		if (cs == 0)
			break;
		if (line[cs-1] == '\n')
			if (!--cs)
				break;
		while (cs && line[cs-1] != '\n') cs--;
		mult--;
	}
	if (mult) {
		uphistory();
		cs = 0;
	}
}

void endoflinehist() /**/
{
	if (mult < 0) { mult = -mult; beginningoflinehist(); return; }
	while (mult) {
		if (cs >= ll) {
			cs = ll;
			break;
		}
		if (line[cs] == '\n')
			if (++cs == ll)
				break;
		while (cs != ll && line[cs] != '\n') cs++;
		mult--;
	}
	if (mult)
		downhistory();
}

void forwardchar() /**/
{
	cs += mult;
	if (cs > ll) cs = ll;
	if (cs <  0) cs = 0;
}

void backwardchar() /**/
{
	cs -= mult;
	if (cs > ll) cs = ll;
	if (cs <  0) cs = 0;
}

void setmarkcommand() /**/
{
	mark = cs;
}

void exchangepointandmark() /**/
{
int x;

	x = mark;
	mark = cs;
	cs = x;
	if (cs > ll)
		cs = ll;
}

void vigotocolumn() /**/
{
int x,y,ocs = cs;

	if (mult > 0) mult--;
	findline(&x,&y);
	if (mult >= 0) cs = x+mult; else cs = y+mult;
	if (cs < x || cs > y) {
		feep();
		cs = ocs;
	}
}

void vimatchbracket() /**/
{
int ocs = cs,dir,ct;
char oth,me;

otog:
	if (cs == ll)
		{
		feep();
		cs = ocs;
		return;
		}
	switch(me = line[cs])
		{
		case '{': dir = 1; oth = '}'; break;
		case '}': dir = -1; oth = '{'; break;
		case '(': dir = 1; oth = ')'; break;
		case ')': dir = -1; oth = '('; break;
		case '[': dir = 1; oth = ']'; break;
		case ']': dir = -1; oth = '['; break;
		default: cs++; goto otog;
		}
	ct = 1;
	while (cs >= 0 && cs < ll && ct)
		{
		cs += dir;
		if (line[cs] == oth)
			ct--;
		else if (line[cs] == me)
			ct++;
		}
	if (cs < 0 || cs >= ll)
		{
		feep();
		cs = ocs;
		}
}

void viforwardchar() /**/
{
	if (mult < 0) { mult = -mult; vibackwardchar(); return; }
	while (mult--) {
		cs++;
		if (cs >= ll || line[cs] == '\n') {
			cs--;
			break;
		}
	}
}

void vibackwardchar() /**/
{
	if (mult < 0) { mult = -mult; viforwardchar(); return; }
	while (mult--) {
		cs--;
		if (cs < 0 || line[cs] == '\n') {
			cs++;
			break;
		}
	}
}

void viendofline() /**/
{
	cs = findeol();
	if (!virangeflag && cs != 0 && line[cs-1] != '\n') cs--;
}

void vibeginningofline() /**/
{
	cs = findbol();
}


static int vfindchar,vfinddir,tailadd;

void vifindnextchar() /**/
{
	if (vfindchar = vigetkey())
		{
		vfinddir = 1;
		tailadd = 0;
		virepeatfind();
		}
}

void vifindprevchar() /**/
{
	if (vfindchar = vigetkey())
		{
		vfinddir = -1;
		tailadd = 0;
		virepeatfind();
		}
}

void vifindnextcharskip() /**/
{
	if (vfindchar = vigetkey())
		{
		vfinddir = 1;
		tailadd = -1;
		virepeatfind();
		}
}

void vifindprevcharskip() /**/
{
	if (vfindchar = vigetkey())
		{
		vfinddir = -1;
		tailadd = 1;
		virepeatfind();
		}
}

void virepeatfind() /**/
{
int ocs = cs;

	if (!vfinddir) { feep(); return; }
	if (mult < 0) { mult = -mult; virevrepeatfind(); return; }
	while (mult--)
		{
		do
			cs += vfinddir;
		while (cs >= 0 && cs < ll && line[cs] != vfindchar && line[cs] != '\n');
		if (cs < 0 || cs >= ll || line[cs] == '\n')
			{
			feep();
			cs = ocs;
			return;
			}
		}
	cs += tailadd;
	if (vfinddir == 1 && virangeflag) cs++;
}

void virevrepeatfind() /**/
{
	if (mult < 0) { mult = -mult; virepeatfind(); return; }
	vfinddir = -vfinddir;
	virepeatfind();
	vfinddir = -vfinddir;
}

void vifirstnonblank() /**/
{
	cs = findbol();
	while (cs != ll && iblank(line[cs]))
		cs++;
}

void visetmark() /**/
{
int ch;

	ch = getkey(1);
	if (ch < 'a' || ch > 'z') {
		feep();
		return;
	}
	ch -= 'a';
	vimarkcs[ch] = cs;
	vimarkline[ch] = histline;
}

void vigotomark() /**/
{
int ch;

	ch = getkey(1);
	if (ch == c) ch = 26;
	else {
		if (ch < 'a' || ch > 'z') {
			feep();
			return;
		}
		ch -= 'a';
	}
	if (!vimarkline[ch]) {
		feep();
		return;
	}
	if (curhist != vimarkline[ch]) {
		mult = vimarkline[ch];
		vifetchhistory();
		if (curhist != vimarkline[ch]) return;
	}
	cs = vimarkcs[ch];
	if (cs > ll) ch = ll;
}

void vigotomarkline() /**/
{
	vigotomark();
	cs = findbol();
}
