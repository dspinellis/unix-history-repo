/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)swapgeneric.c	8.1 (Berkeley) 6/16/93
 */

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/buf.h>
#include <sys/systm.h>
#include <sys/reboot.h>

#include <pmax/dev/device.h>

/*
 * Generic configuration;  all in one
 */
dev_t	rootdev = NODEV;
dev_t	argdev = NODEV;
dev_t	dumpdev = NODEV;
int	nswap;
struct	swdevt swdevt[] = {
	{ -1,	1,	0 },
	{ NODEV,	0,	0 },
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
