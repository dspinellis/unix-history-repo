/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)swapgeneric.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "conf.h"
#include "buf.h"
#include "systm.h"
#include "reboot.h"

#include "../dev/device.h"

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
int	dmmin, dmmax, dmtext;

extern	struct driver rzdriver;

struct	genericconf {
	caddr_t	gc_driver;
	char	*gc_name;
	dev_t	gc_root;
} genericconf[] = {
	{ (caddr_t)&rzdriver,	"rz",	makedev(0, 0),	},
	{ 0 },
};

setconf()
{
	register struct scsi_device *dp;
	register struct genericconf *gc;
	register char *cp, *gp;
	int unit, swaponroot = 0;
	char *root_swap;

	/*
	 * If we are running on the in memory, mini-root; then we just need
	 * to set the swap device.
	 */
	if (boothowto & RB_MINIROOT)
		root_swap = "swap";
	else {
		if (rootdev != NODEV) {
			swdevt[0].sw_dev = argdev = dumpdev =
				makedev(major(rootdev), minor(rootdev)+1);
			return;
		}
		root_swap = "root";
	}
	unit = 0;
	if (boothowto & RB_ASKNAME) {
		char name[128];
retry:
		printf("%s device? ", root_swap);
		gets(name);
		for (gc = genericconf; gc->gc_driver; gc++)
		    for (cp = name, gp = gc->gc_name; *cp == *gp; cp++)
			if (*++gp == 0)
				goto gotit;
		printf("use one of:");
		for (gc = genericconf; gc->gc_driver; gc++)
			printf(" %s%%d", gc->gc_name);
		printf("\n");
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
		for (dp = scsi_dinit; dp->sd_driver; dp++) {
			if (dp->sd_alive == 0)
				continue;
			if (dp->sd_unit == unit &&
			    dp->sd_driver == (struct driver *)gc->gc_driver) {
				printf("root on %s%d%c\n",
					dp->sd_driver->d_name, unit,
					"ab"[swaponroot]);
				goto found;
			}
		}
	}
	printf("no suitable %s\n", root_swap);
	goto retry;
found:
	if (boothowto & RB_MINIROOT) {
		swdevt[0].sw_dev = argdev = dumpdev =
			makedev(major(gc->gc_root), unit*8+1);
	} else {
		rootdev = makedev(major(gc->gc_root), unit*8);
		swdevt[0].sw_dev = argdev = dumpdev =
			makedev(major(rootdev), minor(rootdev)+1);
		/* swap size and dumplo set during autoconfigure */
		if (swaponroot)
			rootdev = dumpdev;
	}
}

gets(cp)
	char *cp;
{
	register char *lp;
	register c;
	int s;

	lp = cp;
	s = spltty();
	for (;;) {
		cnputc(c = cngetc());
		switch (c) {
		case '\r':
			cnputc('\n');
			*lp++ = '\0';
			break;
		case '\n':
			cnputc('\r');
			*lp++ = '\0';
			break;
		case '\b':
		case '\177':
			if (lp > cp) {
				lp--;
				cnputc(' ');
				cnputc('\b');
			}
			continue;
		case '#':
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
			continue;
		}
		break;
	}
	splx(s);
}
