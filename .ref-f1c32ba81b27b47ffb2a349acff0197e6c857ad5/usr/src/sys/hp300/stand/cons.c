/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: cons.c 1.5 89/08/22$
 *
 *	@(#)cons.c	7.3 (Berkeley) %G%
 */

#include "sys/param.h"
#include "samachdep.h"
#include "../hp300/cons.h"

int	nodev();
#ifdef ITECONSOLE
int	iteprobe(), iteinit(), itegetchar(), iteputchar();
#endif
#ifdef DCACONSOLE
int	dcaprobe(), dcainit(), dcagetchar(), dcaputchar();
#endif
#ifdef DCMCONSOLE
int	dcmprobe(), dcminit(), dcmgetchar(), dcmputchar();
#endif

struct consdev constab[] = {
#ifdef ITECONSOLE
	{ iteprobe,	iteinit,	itegetchar,	iteputchar },
#endif
#ifdef DCACONSOLE
	{ dcaprobe,	dcainit,	dcagetchar,	dcaputchar },
#endif
#ifdef DCMCONSOLE
	{ dcmprobe,	dcminit,	dcmgetchar,	dcmputchar },
#endif
	{ 0 },
};

struct consdev *cn_tab;
int noconsole;

cninit()
{
	register struct consdev *cp;

	cn_tab = NULL;
	noconsole = 1;
	for (cp = constab; cp->cn_probe; cp++) {
		(*cp->cn_probe)(cp);
		if (cp->cn_pri > CN_DEAD &&
		    (cn_tab == NULL || cp->cn_pri > cn_tab->cn_pri))
			cn_tab = cp;
	}
	if (cn_tab) {
		(*cn_tab->cn_init)(cn_tab);
		noconsole = 0;
	}
}

cngetc()
{
	if (cn_tab)
		return((*cn_tab->cn_getc)());
	return(0);
}

cnputc(c)
	int c;
{
	if (cn_tab)
		(*cn_tab->cn_putc)(c);
}
