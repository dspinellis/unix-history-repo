/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)swapgeneric.c	7.11 (Berkeley) 5/9/91
 */

#include "mba.h"

#include "../include/pte.h"

#include "sys/param.h"
#include "sys/conf.h"
#include "sys/buf.h"
#include "sys/vm.h"
#include "sys/systm.h"
#include "sys/reboot.h"

#include "cons.h"
#include "../include/mtpr.h"
#include "../mba/mbareg.h"
#include "../mba/mbavar.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

/*
 * Generic configuration;  all in one
 */
dev_t	rootdev = NODEV;
dev_t	argdev = NODEV;
dev_t	dumpdev = NODEV;
int	nswap;
struct	swdevt swdevt[] = {
	{ -1,	1,	0 },
	{ 0,	0,	0 },
};
long	dumplo;
int	dmmin, dmmax, dmtext;

extern	struct mba_driver hpdriver;
extern	struct uba_driver scdriver;
extern	struct uba_driver hkdriver;
extern	struct uba_driver idcdriver;
extern	struct uba_driver hldriver;
extern	struct uba_driver udadriver;
extern	struct uba_driver kdbdriver;

struct	genericconf {
	caddr_t	gc_driver;
	char	*gc_name;
	dev_t	gc_root;
} genericconf[] = {
	{ (caddr_t)&hpdriver,	"hp",	makedev(0, 0),	},
	{ (caddr_t)&scdriver,	"up",	makedev(2, 0),	},
	{ (caddr_t)&udadriver,	"ra",	makedev(9, 0),	},
	{ (caddr_t)&idcdriver,	"rb",	makedev(11, 0),	},
	{ (caddr_t)&hldriver,	"rl",	makedev(14, 0),	},
	{ (caddr_t)&hkdriver,	"hk",	makedev(3, 0),	},
	{ (caddr_t)&hkdriver,	"rk",	makedev(3, 0),	},
	{ (caddr_t)&kdbdriver,	"kra",	makedev(16, 0), },
	{ 0 },
};

setconf()
{
#if NMBA > 0
	register struct mba_device *mi;
#endif
	register struct uba_device *ui;
	register struct genericconf *gc;
	register char *cp, *gp;
	int unit, swaponroot = 0;

	if (rootdev != NODEV)
		goto doswap;
	unit = 0;
	if (boothowto & RB_ASKNAME) {
		char name[128];
retry:
		printf("root device? ");
		gets(name);
		for (gc = genericconf; gc->gc_driver; gc++)
		    for (cp = name, gp = gc->gc_name; *cp == *gp; cp++)
			if (*++gp == 0)
				goto gotit;
		printf(
		  "use hp%%d, up%%d, ra%%d, rb%%d, rl%%d, hk%%d or kra%%d\n");
		goto retry;
gotit:
		if (*++cp < '0' || *cp > '9') {
			printf("bad/missing unit number\n");
			goto retry;
		}
		while (*cp >= '0' && *cp <= '9')
			unit = 10 * unit + *cp++ - '0';
		if (*cp == '*')
			swaponroot++;
		goto found;
	}
	for (gc = genericconf; gc->gc_driver; gc++) {
#if NMBA > 0
		for (mi = mbdinit; mi->mi_driver; mi++) {
			if (mi->mi_alive == 0)
				continue;
			if (mi->mi_unit == unit && mi->mi_driver ==
			    (struct mba_driver *)gc->gc_driver) {
				printf("root on %s%d\n",
				    mi->mi_driver->md_dname, unit);
				goto found;
			}
		}
#endif
		for (ui = ubdinit; ui->ui_driver; ui++) {
			if (ui->ui_alive == 0)
				continue;
			if (ui->ui_unit == unit && ui->ui_driver ==
			    (struct uba_driver *)gc->gc_driver) {
				printf("root on %s%d\n",
				    ui->ui_driver->ud_dname, unit);
				goto found;
			}
		}
	}
	printf("no suitable root\n");
	asm("halt");
found:
	gc->gc_root = makedev(major(gc->gc_root), unit*8);
	rootdev = gc->gc_root;
doswap:
	swdevt[0].sw_dev = argdev = dumpdev =
	    makedev(major(rootdev), minor(rootdev)+1);
	/* swap size and dumplo set during autoconfigure */
	if (swaponroot)
		rootdev = dumpdev;
}

gets(cp)
	char *cp;
{
	register char *lp;
	register c;

	lp = cp;
	for (;;) {
		cnputc(c = cngetc());
		switch (c) {
		case '\n':
		case '\r':
			*lp++ = '\0';
			return;
		case '\b':
		case '#':
		case '\177':
			lp--;
			if (lp < cp)
				lp = cp;
			continue;
		case '@':
		case 'u'&037:
			lp = cp;
			cnputc('\n');
			continue;
		default:
			*lp++ = c;
		}
	}
}
