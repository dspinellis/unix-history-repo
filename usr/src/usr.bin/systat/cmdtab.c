#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	1.2 (Lucasfilm) %G%";
#endif

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
int	showvmstat(), fetchvmstat(), labelvmstat();
int	initvmstat(), closevmstat();
WINDOW	*openvmstat();

struct	cmdtab cmdtab[] = {
        { "pigs",	showpigs,	fetchpigs,	labelpigs,
	  initpigs,	openpigs,	closepigs },
        { "swap",	showswap,	fetchswap,	labelswap,
	  initswap,	openswap,	closeswap },
        { "mbufs",	showmbufs,	fetchmbufs,	labelmbufs,
	  initmbufs,	openmbufs,	closembufs },
        { "iostat",	showiostat,	fetchiostat,	labeliostat,
	  initiostat,	openiostat,	closeiostat,	cmdiostat },
        { "vmstat",	showvmstat,	fetchvmstat,	labelvmstat,
	  initvmstat,	openvmstat,	closevmstat },
        { 0 }
};
struct  cmdtab *curcmd = &cmdtab[0];
