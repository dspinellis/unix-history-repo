/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)swapgeneric.c	7.5 (Berkeley) 4/6/90
 */

#include "param.h"
#include "conf.h"
#include "buf.h"
#include "vm.h"
#include "systm.h"
#include "reboot.h"

#include "pte.h"
#include "cpu.h"
#include "cp.h"
#include "mtpr.h"

#include "../tahoevba/vbavar.h"

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

extern	struct vba_driver vddriver;
extern	struct vba_driver hdcdriver;

struct	genericconf {
	caddr_t	gc_driver;
	char	*gc_name;
	dev_t	gc_root;
} genericconf[] = {
	{ (caddr_t)&vddriver,	"dk",	makedev(1, 0),	},
	{ (caddr_t)&hdcdriver,	"hd",	makedev(2, 0),	},
	{ 0 },
};

setconf()
{
	register struct vba_device *ui;
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
				if (!*++gp)
					goto gotit;
		printf("use");
		for (gc = genericconf; gc->gc_driver; gc++)
			printf(" %s%%d", gc->gc_name);
		printf("\n");
		goto retry;
gotit:
		cp = name + 2;
		if (*cp < '0' || *cp > '9') {
			printf("bad/missing unit number\n");
			goto retry;
		}
		while (*cp >= '0' && *cp <= '9')
			unit = 10 * unit + *cp++ - '0';
		if (*cp == '*')
			swaponroot++;
	}
	for (gc = genericconf; gc->gc_driver; gc++) {
		for (ui = vbdinit; ui->ui_driver; ui++) {
			if (ui->ui_alive == 0)
				continue;
			if (ui->ui_unit == 0 && ui->ui_driver ==
			    (struct vba_driver *)gc->gc_driver) {
				printf("root on %s0\n",
				    ui->ui_driver->ud_dname);
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

gets(buf)
	char *buf;
{
	register int c;
	register char *lp;

	for (lp = buf;;) {
		printf("%c", c = cngetc()&0177);
		switch (c) {
		case '\n':
		case '\r':
			*lp = '\0';
			return;
		case '\b':
		case '\177':
			if (lp > buf) {
				--lp;
				printf("\b \b");
			}
			break;
		case '#':
			if (lp > buf)
				--lp;
			break;
		case 'r'&037:
			*lp = 0;
			printf("\n");
			printf("%s", buf);
			break;
		case '@':
		case 'u'&037:
		case 'w'&037:
			lp = buf;
			printf("%c", '\n');
			break;
		default:
			*lp++ = c;
		}
	}
	/* NOTREACHED */
}
