/*	swapgeneric.c	1.3	86/07/16	*/

#include "../machine/pte.h"

#include "param.h"
#include "conf.h"
#include "buf.h"
#include "vm.h"
#include "systm.h"
#include "reboot.h"

#include "../tahoe/cp.h"
#include "../tahoe/mtpr.h"
#include "../tahoevba/vbavar.h"

/*
 * Generic configuration;  all in one
 */
dev_t	rootdev;
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
	int unit, swaponroot = 0;

	if (rootdev != NODEV)
		goto noswap;
	if (boothowto & RB_ASKNAME) {
		char name[128];
retry:
		printf("root device? ");
		gets(name);
		for (gc = genericconf; gc->gc_driver; gc++)
			if (gc->gc_name[0] == name[0] &&
			    gc->gc_name[1] == name[1])
				goto gotit;
		goto bad;
gotit:
		if (name[3] == '*') {
			name[3] = name[4];
			swaponroot++;
		}
		if (name[2] >= '0' && name[2] <= '7' && name[3] == 0) {
			unit = name[2] - '0';
			goto found;
		}
		printf("bad/missing unit number\n");
bad:
		printf("use dk%%d\n");
		goto retry;
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
	swdevt[0].sw_dev = argdev = dumpdev =
	    makedev(major(rootdev), minor(rootdev)+1);
	/* swap size and dumplo set during autoconfigure */
	if (swaponroot)
		rootdev = dumpdev;
}

getchar()
{
	char c;
	int timo;
	extern struct cpdcb_i consin[];
	extern struct cphdr *lasthdr;
#define cpin consin[CPCONS]

	timo = 10000;
	uncache((char *)&lasthdr->cp_unit);
	while ((lasthdr->cp_unit&CPTAKE) == 0 && --timo)
		uncache(&lasthdr->cp_unit);
	cpin.cp_hdr.cp_unit = CPCONS;	/* Resets done bit */
	cpin.cp_hdr.cp_comm = CPREAD;
	cpin.cp_hdr.cp_count = 1;
	mtpr(CPMDCB, &cpin);
	while ((cpin.cp_hdr.cp_unit & CPDONE) == 0) 
		uncache(&cpin.cp_hdr.cp_unit);
	uncache(&cpin.cpi_buf[0]);
	c = cpin.cpi_buf[0] & 0x7f;
	lasthdr = (struct cphdr *)&cpin;
	if (c == '\r')
		c = '\n';
	printf("%c", c);	/* takes care of interrupts & parity */
	return (c);
}

gets(cp)
	char *cp;
{
	register char *lp;
	register c;

	lp = cp;
	for (;;) {
		c = getchar() & 0177;
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
