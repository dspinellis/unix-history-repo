/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: cons.c 1.5 89/08/22$
 *
 *	@(#)cons.c	7.2 (Berkeley) 5/25/90
 */

#include "param.h"
#include "samachdep.h"
#include "machine/cons.h"

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
