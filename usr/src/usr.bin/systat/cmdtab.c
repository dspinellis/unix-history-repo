#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	1.1 (Lucasfilm) %G%";
#endif

#include "systat.h"

int     showpigs(), fetchpigs(), labelpigs(), initpigs(), closepigs();
WINDOW	*openpigs();
int     showswap(), fetchswap(), labelswap(), initswap(), closeswap();
WINDOW	*openswap();
int	showmbufs(), fetchmbufs(), labelmbufs(), initmbufs(), closembufs();
WINDOW	*openmbufs();
int	showiostat(), fetchiostat(), labeliostat(), initiostat(), closeiostat();
WINDOW	*openiostat();

struct	cmdtab cmdtab[] = {
        { "pigs",	showpigs,	fetchpigs,	labelpigs,
	  initpigs,	openpigs,	closepigs },
        { "swap",	showswap,	fetchswap,	labelswap,
	  initswap,	openswap,	closeswap },
        { "mbufs",	showmbufs,	fetchmbufs,	labelmbufs,
	  initmbufs,	openmbufs,	closembufs },
        { "iostat",	showiostat,	fetchiostat,	labeliostat,
	  initiostat,	openiostat,	closeiostat },
        { 0 }
};
struct  cmdtab *curcmd = &cmdtab[0];
