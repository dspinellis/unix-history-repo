/*
 *
 * zle_word.c - word-related editor functions
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


void forwardword() /**/
{
	if (mult < 0) { mult = -mult; backwardword(); return; }
	while (mult--) {
		while (cs != ll && iword(line[cs])) cs++;
		if (wordflag && !mult) return;
		while (cs != ll && !iword(line[cs])) cs++;
	}
}

void viforwardword() /**/
{
	if (mult < 0) { mult = -mult; backwardword(); return; }
	while (mult--) {
		if (iident(line[cs])) while (cs != ll && iident(line[cs])) cs++;
		else while (cs != ll && !iident(line[cs]) && !iblank(line[cs])) cs++;
		if (wordflag && !mult) return;
		while (cs != ll && iblank(line[cs])) cs++;
	}
}

void viforwardblankword() /**/
{
	if (mult < 0) { mult = -mult; vibackwardblankword(); return; }
	while (mult--) {
		while (cs != ll && !iblank(line[cs])) cs++;
		if (wordflag && !mult) return;
		while (cs != ll && iblank(line[cs])) cs++;
	}
}

void emacsforwardword() /**/
{
	if (mult < 0) { mult = -mult; emacsbackwardword(); return; }
	while (mult--)
		{
		while (cs != ll && !iword(line[cs])) cs++;
		if (wordflag && !mult) return;
		while (cs != ll && iword(line[cs])) cs++;
		}
}

void viforwardblankwordend() /**/
{
	if (mult < 0) return;
	while (mult--) {
		while (cs != ll && iblank(line[cs+1])) cs++;
		while (cs != ll && !iblank(line[cs+1])) cs++;
	}
	if (cs != ll && virangeflag) cs++;
}

void viforwardwordend() /**/
{
	if (mult < 0) { mult = -mult; backwardword(); return; }
	while (mult--) {
		if (iblank(line[cs+1])) while (cs != ll && iblank(line[cs+1])) cs++;
		if (iident(line[cs+1])) while (cs != ll && iident(line[cs+1])) cs++;
		else while (cs != ll && !iident(line[cs+1]) && !iblank(line[cs+1])) cs++;
	}
	if (cs != ll && virangeflag) cs++;
}

void backwardword() /**/
{
	if (mult < 0) { mult = -mult; forwardword(); return; }
	while (mult--) {
		while (cs && !iword(line[cs-1])) cs--;
		while (cs && iword(line[cs-1])) cs--;
	}
}

void vibackwardword() /**/
{
	if (mult < 0) { mult = -mult; backwardword(); return; }
	while (mult--) {
		while (cs && iblank(line[cs-1])) cs--;
		if (iident(line[cs-1])) while (cs && iident(line[cs-1])) cs--;
		else while (cs && !iident(line[cs-1]) && !iblank(line[cs-1])) cs--;
	}
}

void vibackwardblankword() /**/
{
	if (mult < 0) { mult = -mult; viforwardblankword(); return; }
	while (mult--) {
		while (cs && iblank(line[cs-1])) cs--;
		while (cs && !iblank(line[cs-1])) cs--;
	}
}

void emacsbackwardword() /**/
{
	if (mult < 0) { mult = -mult; emacsforwardword(); return; }
	while (mult--) {
		while (cs && !iword(line[cs-1])) cs--;
		while (cs && iword(line[cs-1])) cs--;
	}
}

void backwarddeleteword() /**/
{
int x = cs;

	if (mult < 0) { mult = -mult; deleteword(); return; }
	while (mult--) {
		while (x && !iword(line[x-1])) x--;
		while (x && iword(line[x-1])) x--;
	}
	backdel(cs-x);
}

void vibackwardkillword() /**/
{
int x = cs;

	if (mult < 0) { feep(); return; }
	while (mult--) {
		while (x > viinsbegin && !iword(line[x-1])) x--;
		while (x > viinsbegin && iword(line[x-1])) x--;
	}
	backkill(cs-x,1);
}

void backwardkillword() /**/
{
int x = cs;

	if (mult < 0) { mult = -mult; killword(); return; }
	while (mult--) {
		while (x && !iword(line[x-1])) x--;
		while (x && iword(line[x-1])) x--;
	}
	backkill(cs-x,1);
}

void upcaseword() /**/
{
int neg = mult < 0, ocs = cs;

	if (neg) mult = -mult;
	while (mult--) {
		while (cs != ll && !iword(line[cs])) cs++;
		while (cs != ll && iword(line[cs])) {
			line[cs] = tuupper(line[cs]);
			cs++;
		}
	}
	if (neg) cs = ocs;
}

void downcaseword() /**/
{
int neg = mult < 0, ocs = cs;

	if (neg) mult = -mult;
	while (mult--) {
		while (cs != ll && !iword(line[cs])) cs++;
		while (cs != ll && iword(line[cs])) {
			line[cs] = tulower(line[cs]);
			cs++;
		}
	}
	if (neg) cs = ocs;
}

void capitalizeword() /**/
{
int first;
int neg = mult < 0, ocs = cs;
	
	if (neg) mult = -mult;
	while (mult--) {
		first = 1;
		while (cs != ll && !iword(line[cs])) cs++;
		while (cs != ll && iword(line[cs])) {
			line[cs] = (first) ? tuupper(line[cs]) : tulower(line[cs]);
			first = 0;
			cs++;
		}
	}
	if (neg) cs = ocs;
}

void deleteword() /**/
{
int x = cs;

	if (mult < 0) { mult = -mult; backwarddeleteword(); return; }
	while (mult--) {
		while (x != ll && !iword(line[x])) x++;
		while (x != ll && iword(line[x])) x++;
	}
	foredel(x-cs);
}

void killword() /**/
{
int x = cs;

	if (mult < 0) { mult = -mult; backwardkillword(); return; }
	while (mult--) {
		while (x != ll && !iword(line[x])) x++;
		while (x != ll && iword(line[x])) x++;
	}
	forekill(x-cs,0);
}

void transposewords() /**/
{
int p1,p2,p3,p4,x = cs;
char *temp,*pp;
int neg = mult < 0, ocs = cs;

	if (neg) mult = -mult;
	while (mult--) {
		while (x != ll && line[x] != '\n' && !iword(line[x]))
			x++;
		if (x == ll || line[x] == '\n') {
			x = cs;
			while (x && line[x-1] != '\n' && !iword(line[x]))
				x--;
			if (!x || line[x-1] == '\n') {
				feep();
				return;
			}
		}
		for (p4 = x; p4 != ll && iword(line[p4]); p4++);
		for (p3 = p4; p3 && iword(line[p3-1]); p3--);
		if (!p3) {
			feep();
			return;
		}
		for (p2 = p3; p2 && !iword(line[p2-1]); p2--);
		if (!p2) {
			feep();
			return;
		}
		for (p1 = p2; p1 && iword(line[p1-1]); p1--);
		pp = temp = halloc(p4-p1+1);
		struncpy(&pp,line+p3,p4-p3);
		struncpy(&pp,line+p2,p3-p2);
		struncpy(&pp,line+p1,p2-p1);
		strncpy((char *) line+p1,temp,p4-p1);
		cs = p4;
	}
	if (neg) cs = ocs;
}
