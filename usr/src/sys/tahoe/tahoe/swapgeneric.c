/*	swapgeneric.c	7.1	88/05/21	*/

#include "../machine/pte.h"

#include "param.h"
#include "conf.h"
#include "buf.h"
#include "vm.h"
#include "systm.h"
#include "reboot.h"

#include "../tahoe/cpu.h"
#include "../tahoe/cp.h"
#include "../tahoe/mtpr.h"
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

struct	genericconf {
	caddr_t	gc_driver;
	char	*gc_name;
	dev_t	gc_root;
} genericconf[] = {
	{ (caddr_t)&vddriver,	"dk",	makedev(1, 0),	},
	{ 0 },
};

setconf()
{
	register struct vba_device *ui;
	register struct genericconf *gc;
	register char *cp;
	int unit, swaponroot = 0;

	if (rootdev != NODEV)
		goto doswap;
	if (boothowto & RB_ASKNAME) {
		char name[128];
retry:
		printf("root device? ");
		gets(name);
		for (gc = genericconf; gc->gc_driver; gc++)
			if (gc->gc_name[0] == name[0] &&
			    gc->gc_name[1] == name[1])
				goto gotit;
		printf("use dk%%d\n");
		goto retry;
gotit:
		cp = name + 2;
		if (*cp < '0' || *cp > '9') {
			printf("bad/missing unit number\n");
			goto retry;
		}
		unit = 0;
		while (*cp >= '0' && *cp <= '9')
			unit = 10 * unit + *cp++ - '0';
		if (*cp == '*')
			swaponroot++;
	}
	unit = 0;
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

gets(cp)
	char *cp;
{
	register char *lp;
	register c;

	lp = cp;
	for (;;) {
		printf("%c", c = cngetc()&0177);
		switch (c) {
		case '\n':
		case '\r':
			*lp++ = '\0';
			return;
		case '\b':
		case '\177':
			if (lp > cp) {
				printf(" \b");
				lp--;
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
			printf("%c", '\n');
			continue;
		default:
			*lp++ = c;
		}
	}
}
