/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	5.1 (Berkeley) %G%";
#endif not lint

#include "systat.h"

int     showpigs(), fetchpigs(), labelpigs();
int	initpigs(), closepigs();
WINDOW	*openpigs();
int     showswap(), fetchswap(), labelswap();
int	initswap(), closeswap();
WINDOW	*openswap();
int	showmbufs(), fetchmbufs(), labelmbufs();
int	initmbufs(), closembufs();
WINDOW	*openmbufs();
int	showiostat(), fetchiostat(), labeliostat();
int	initiostat(), closeiostat(), cmdiostat();
WINDOW	*openiostat();
int	showkre(), fetchkre(), labelkre();
int	initkre(), closekre(), cmdkre();
WINDOW	*openkre();
int	shownetstat(), fetchnetstat(), labelnetstat();
int	initnetstat(), closenetstat(), cmdnetstat();
WINDOW	*opennetstat();

struct	cmdtab cmdtab[] = {
        { "pigs",	showpigs,	fetchpigs,	labelpigs,
	  initpigs,	openpigs,	closepigs,	0,
	  CF_LOADAV },
        { "swap",	showswap,	fetchswap,	labelswap,
	  initswap,	openswap,	closeswap,	0,
	  CF_LOADAV },
        { "mbufs",	showmbufs,	fetchmbufs,	labelmbufs,
	  initmbufs,	openmbufs,	closembufs,	0,
	  CF_LOADAV },
        { "iostat",	showiostat,	fetchiostat,	labeliostat,
	  initiostat,	openiostat,	closeiostat,	cmdiostat,
	  CF_LOADAV },
        { "vmstat",	showkre,	fetchkre,	labelkre,
	  initkre,	openkre,	closekre,	cmdkre,
	  0 },
        { "netstat",	shownetstat,	fetchnetstat,	labelnetstat,
	  initnetstat,	opennetstat,	closenetstat,	cmdnetstat,
	  CF_LOADAV },
        { 0 }
};
struct  cmdtab *curcmd = &cmdtab[0];
