/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell, and Kazumasa Utashiro of Software Research
 * Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)swapgeneric.c	7.2 (Berkeley) %G%
 */

#include <machine/fix_machine_type.h>

#include <sysparam.h>
#include <sysconf.h>
#include <sysbuf.h>
#include <syssystm.h>
#include <sysreboot.h>

#ifdef CPU_DOUBLE
#include <news3400/iop/iopvar.h>
#else /* CPU_DOUBLE */
#include <news3400/hbdev/hbvar.h>
# define	iop	hb

# define	idinit	hdinit

# define	im_driver	hm_driver
# define	im_ctlr		hm_ctlr
# define	im_alive	hm_alive
# define	im_addr		hm_addr
# define	im_intr		hm_intr
# define	im_scnum	hm_scnum
# define	im_hd		hm_hd
# define	im_hbinfo	hm_hbinfo
# define	im_tab		hm_tab

# define	ii_driver	hi_driver
# define	ii_unit		hi_unit
# define	ii_ctlr		hi_ctlr
# define	ii_slave	hi_slave
# define	ii_addr		hi_addr
# define	ii_intr		hi_intr
# define	ii_dk		hi_dk
# define	ii_flags	hi_flags
# define	ii_alive	hi_alive
# define	ii_type		hi_type
# define	ii_forw		hi_forw
# define	ii_mi		hi_mi
# define	ii_hd		hi_hd

# define	id_probe	hd_probe
# define	id_slave	hd_slave
# define	id_attach	hd_attach
# define	id_dgo		hd_dgo
# define	id_dname	hd_dname
# define	id_dinfo	hd_dinfo
# define	id_mname	hd_mname
# define	id_minfo	hd_minfo
#endif /* CPU_DOUBLE */

# include "sd.h"
# include "hd.h"
# include "fd.h"
# include "od.h"

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

# if NSDC > 0
extern	struct iop/**/_driver sdcdriver;
# endif
# if NHC > 0
extern	struct iop/**/_driver hcdriver;
# endif
# if NFC > 0
extern	struct iop/**/_driver fcdriver;
# endif
# if NOC > 0
extern	struct iop/**/_driver ocdriver;
# endif

struct	genericconf {
	caddr_t	gc_driver;
	char	*gc_name;
	dev_t	gc_root;
} genericconf[] = {
# if NSDC > 0
	{ (caddr_t)&sdcdriver,	"sd",	makedev(0, 0),	},
# endif
# if NHC > 0
	{ (caddr_t)&hcdriver,	"hd",	makedev(0, 0),	},
# endif
# if NFC > 0
	{ (caddr_t)&fcdriver,	"fh",	makedev(1, 0),	},
	{ (caddr_t)&fcdriver,	"fd",	makedev(1, 1),	},
# endif
# if NOC > 0
	{ (caddr_t)&ocdriver,	"od",	makedev(10, 0),	},
# endif
	{ 0 },
};

setconf()
{
	register struct scsi_device *dp;
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
	printf("no suitable root\n");
	goto retry;
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
