/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)autoconf.c	7.3 (Berkeley) %G%
 *
 * from: $Header: autoconf.c,v 1.23 92/07/10 22:35:23 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/disklabel.h>
#include <sys/device.h>
#include <sys/dkstat.h>
#include <sys/conf.h>
#include <sys/dmap.h>
#include <sys/reboot.h>
#include <sys/socket.h>
#include <sys/systm.h>

#include <net/if.h>

#include <machine/autoconf.h>
#include <machine/bsd_openprom.h>
#include <machine/cpu.h>

#ifndef	FS_NFS		/* XXX */
#define	FS_NFS	100	/* XXX */
#endif			/* XXX */

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold;		/* if 1, still working on cold-start */
int	dkn;		/* number of iostat dk numbers assigned so far */
int	cpuspeed = 10;	/* relative cpu speed */
int	fbnode;		/* node ID of ROM's console frame buffer */
int	optionsnode;	/* node ID of ROM's options */

extern struct promvec *promvec;

static	int rootnode;
int	findroot();
static	struct	bootinfo *findbootdev();

static	struct bootinfo {
	char	name[16];	/* device name */
	int	val[3];		/* up to 3 values */
	int	type;		/* FS type */
	caddr_t	data;		/* FS dependant info */
} bootinfo;


/*
 * Most configuration on the SPARC is done by matching OPENPROM Forth
 * device names with our internal names.
 */
int
matchbyname(parent, cf, aux)
	struct device *parent;
	struct cfdata *cf;
	void *aux;
{

	return (strcmp(cf->cf_driver->cd_name, *(char **)aux) == 0);
}

/*
 * Convert hex ASCII string to a value.  Returns updated pointer.
 * Depends on ASCII order (this *is* machine-dependent code, you know).
 */
static char *
str2hex(str, vp)
	register char *str;
	register int *vp;
{
	register int v, c;

	for (v = 0;; v = v * 16 + c, str++) {
		c = *(u_char *)str;
		if (c <= '9') {
			if ((c -= '0') < 0)
				break;
		} else if (c <= 'F') {
			if ((c -= 'A' - 10) < 10)
				break;
		} else if (c <= 'f') {
			if ((c -= 'a' - 10) < 10)
				break;
		} else
			break;
	}
	*vp = v;
	return (str);
}

/*
 * locore.s code calls bootstrap() just before calling main(), after double
 * mapping the kernel to high memory and setting up the trap base register.
 * We must finish mapping the kernel properly and glean any bootstrap info.
 */
void
bootstrap()
{
	register char *cp, *bp, *ep;
	register int i;
	int nmmu, ncontext, node;
#ifdef KGDB
	extern int kgdb_debug_panic;
#endif
	extern char *rindex(const char *, int);

	node = findroot();
	nmmu = getpropint(node, "mmu-npmg", 128);
	ncontext = getpropint(node, "mmu-nctx", 8);
	pmap_bootstrap(nmmu, ncontext);
#ifdef KGDB
	zs_kgdb_init();			/* XXX */
#endif
	/*
	 * On SS1s, promvec->pv_v0bootargs->ba_argv[1] contains the flags
	 * that were given after the boot command.  On SS2s, pv_v0bootargs
	 * is NULL but *promvec->pv_v2bootargs.v2_bootargs points to
	 * "vmunix -s" or whatever.
	 * ###	DO THIS BEFORE pmap_boostrap?
	 */
	if (promvec->pv_romvec_vers < 2) {
		/* Grab boot device name and values. */
		cp = (*promvec->pv_v0bootargs)->ba_argv[0];
		if (cp != NULL) {
			bp = bootinfo.name;
			ep = &bootinfo.name[sizeof(bootinfo.name)];
			while (*cp != '(' && *cp != '\0' && bp < ep - 1)
				*bp++ = *cp++;
			*bp = '\0';

			if (*cp == '(' &&
			    *(cp = str2hex(++cp, &bootinfo.val[0])) == ',' &&
			    *(cp = str2hex(++cp, &bootinfo.val[1])) == ',')
				(void)str2hex(++cp, &bootinfo.val[2]);
		}

		/* Setup pointer to boot flags */
		cp = (*promvec->pv_v0bootargs)->ba_argv[1];
		if (cp == NULL || *cp != '-')
			return;
	} else {
		/* Grab boot device name and values. */
		cp = *promvec->pv_v2bootargs.v2_bootpath;
		if (cp != NULL && (cp = rindex(cp, '/')) != NULL) {
			++cp;
			bp = bootinfo.name;
			ep = &bootinfo.name[sizeof(bootinfo.name)];
			while (*cp != '@' && *cp != '\0' && bp < ep - 1)
				*bp++ = *cp++;
			*bp = '\0';

			if (*cp == '@' &&
			    *(cp = str2hex(++cp, &bootinfo.val[0])) == ',' &&
			    *(cp = str2hex(++cp, &bootinfo.val[1])) == ',')
				(void)str2hex(++cp, &bootinfo.val[2]);
		}

		/* Setup pointer to boot flags */
		cp = *promvec->pv_v2bootargs.v2_bootargs;
		if (cp == NULL)
			return;
		while (*cp != '-')
			if (*cp++ == '\0')
				return;
	}
	for (;;) {
		switch (*++cp) {

		case '\0':
			return;

		case 'a':
			boothowto |= RB_ASKNAME;
			break;

		case 'b':
			boothowto |= RB_DFLTROOT;
			break;

		case 'd':	/* kgdb - always on zs	XXX */
#ifdef KGDB
			boothowto |= RB_KDB;	/* XXX unused */
			kgdb_debug_panic = 1;
			kgdb_connect(1);
#else
			printf("kernel not compiled with KGDB\n");
#endif
			break;

		case 's':
			boothowto |= RB_SINGLE;
			break;
		}
	}
}

/*
 * Determine mass storage and memory configuration for a machine.
 * We get the PROM's root device and make sure we understand it, then
 * attach it as `mainbus0'.  We also set up to handle the PROM `sync'
 * command.
 */
configure()
{
	register int node;
	register char *cp;
	struct romaux ra;
	void sync_crash();
#ifdef NFS
	register struct bootinfo *bi;
	extern int (*mountroot)(), nfs_mountroot();
#endif

	node = findroot();
	cp = getpropstring(node, "device_type");
	if (strcmp(cp, "cpu") != 0) {
		printf("PROM root device type = %s\n", cp);
		panic("need CPU as root");
	}
	*promvec->pv_synchook = sync_crash;
	ra.ra_node = node;
	ra.ra_name = cp = "mainbus";
	if (!config_rootfound(cp, (void *)&ra))
		panic("mainbus not configured");
	(void) spl0();
	cold = 0;
#ifdef NFS
	if (boothowto & RB_ASKNAME) {
		char ans[100];

		printf("nfs root? (y/n) [n] ");
		gets(ans);
		if (ans[0] == 'y')
			mountroot = nfs_mountroot;
	} else if ((bi = findbootdev()) != NULL && bi->type == FS_NFS) {
		mountroot = nfs_mountroot;
#ifdef LBL
		lbl_diskless_setup();
#endif /* LBL */
	}
#endif /* NFS */
#if GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	swapconf();
}

/*
 * Console `sync' command.  SunOS just does a `panic: zero' so I guess
 * no one really wants anything fancy...
 */
void
sync_crash()
{

	panic("PROM sync command");
}

char *
clockfreq(freq)
	register int freq;
{
	register char *p;
	static char buf[10];

	freq /= 1000;
	sprintf(buf, "%d", freq / 1000);
	freq %= 1000;
	if (freq) {
		freq += 1000;	/* now in 1000..1999 */
		p = buf + strlen(buf);
		sprintf(p, "%d", freq);
		*p = '.';	/* now buf = %d.%3d */
	}
	return (buf);
}

/* ARGSUSED */
static int
mbprint(aux, name)
	void *aux;
	char *name;
{
	register struct romaux *ra = aux;

	if (name)
		printf("%s at %s", ra->ra_name, name);
	if (ra->ra_paddr)
		printf(" %saddr 0x%x", ra->ra_iospace ? "io" : "",
		    (int)ra->ra_paddr);
	return (UNCONF);
}

int
findroot()
{
	register int node;

	if ((node = rootnode) == 0 && (node = nextsibling(0)) == 0)
		panic("no PROM root device");
	rootnode = node;
	return (node);
}

/*
 * Given a `first child' node number, locate the node with the given name.
 * Return the node number, or 0 if not found.
 */
int
findnode(first, name)
	int first;
	register char *name;
{
	register int node;

	for (node = first; node; node = nextsibling(node))
		if (strcmp(getpropstring(node, "name"), name) == 0)
			return (node);
	return (0);
}

/*
 * Fill in a romaux.  Returns 1 on success, 0 if the register property
 * was not the right size.
 */
int
romprop(rp, cp, node)
	register struct romaux *rp;
	const char *cp;
	register int node;
{
	register int len;
	union { char regbuf[64]; int ireg[3]; } u;
	static const char pl[] = "property length";

	len = getprop(node, "reg", (void *)u.regbuf, sizeof u.regbuf);
	if (len < 12) {
		printf("%s \"reg\" %s = %d (need 12)\n", cp, pl, len);
		return (0);
	}
	if (len > 12)
		printf("warning: %s \"reg\" %s %d > 12, excess ignored\n",
		    cp, pl, len);
	rp->ra_node = node;
	rp->ra_name = cp;
	rp->ra_iospace = u.ireg[0];
	rp->ra_paddr = (caddr_t)u.ireg[1];
	rp->ra_len = u.ireg[2];
	rp->ra_vaddr = (caddr_t)getpropint(node, "address", 0);
	len = getprop(node, "intr", (void *)&rp->ra_intr, sizeof rp->ra_intr);
	if (len == -1)
		len = 0;
	if (len & 7) {
		printf("%s \"intr\" %s = %d (need multiple of 8)\n",
		    cp, pl, len);
		len = 0;
	}
	rp->ra_nintr = len >>= 3;
	/* SPARCstation interrupts are not hardware-vectored */
	while (--len >= 0) {
		if (rp->ra_intr[len].int_vec) {
			printf("WARNING: %s interrupt %d has nonzero vector\n",
			    cp, len);
			break;
		}
	}
	return (1);
}

/*
 * Attach the mainbus.
 *
 * Our main job is to attach the CPU (the root node we got in configure())
 * and iterate down the list of `mainbus devices' (children of that node).
 * We also record the `node id' of the default frame buffer, if any.
 */
static void
mainbus_attach(parent, dev, aux)
	struct device *parent, *dev;
	void *aux;
{
	register int node0, node;
	register const char *cp, *const *ssp, *sp;
#define L1A_HACK		/* XXX hack to allow L1-A during autoconf */
#ifdef L1A_HACK
	int nzs = 0, audio = 0;
#endif
	struct romaux ra;
	static const char *const special[] = {
		/* find these first (end with empty string) */
		"memory-error", "eeprom", "counter-timer", "",

		/* ignore these (end with NULL) */
		"options", "packages", "openprom", "memory", "virtual-memory",
		"interrupt-enable", NULL
	};

	printf("\n");

	/* configure the cpu */
	node = ((struct romaux *)aux)->ra_node;
	ra.ra_node = node;
	ra.ra_name = cp = "cpu";
	ra.ra_paddr = 0;
	config_found(dev, (void *)&ra, mbprint);

	/* remember which frame buffer, if any, is to be /dev/fb */
	fbnode = getpropint(node, "fb", 0);

	/* Find the "options" node */
	node0 = firstchild(node);
	optionsnode = findnode(node0, "options");
	if (optionsnode == 0)
		panic("no options in OPENPROM");

	/*
	 * Locate and configure the ``early'' devices.  These must be
	 * configured before we can do the rest.  For instance, the
	 * EEPROM contains the Ethernet address for the LANCE chip.
	 * If the device cannot be located or configured, panic.
	 */
	for (ssp = special; *(sp = *ssp) != 0; ssp++) {
		if ((node = findnode(node0, sp)) == 0) {
			printf("could not find %s in OPENPROM\n", sp);
			panic(sp);
		}
		if (!romprop(&ra, sp, node) ||
		    !config_found(dev, (void *)&ra, mbprint))
			panic(sp);
	}

	/*
	 * Configure the rest of the devices, in PROM order.  Skip
	 * PROM entries that are not for devices, or which must be
	 * done before we get here.
	 */
	for (node = node0; node; node = nextsibling(node)) {
		cp = getpropstring(node, "name");
		for (ssp = special; (sp = *ssp) != NULL; ssp++)
			if (strcmp(cp, sp) == 0)
				break;
		if (sp == NULL && romprop(&ra, cp, node)) {
#ifdef L1A_HACK
			if (strcmp(cp, "audio") == 0)
				audio = 1;
			if (strcmp(cp, "zs") == 0)
				nzs++;
			if (audio && nzs >= 2)
				(void) splx(11 << 8);	/* XXX */
#endif
			(void) config_found(dev, (void *)&ra, mbprint);
		}
	}
}

struct cfdriver mainbuscd =
    { NULL, "mainbus", matchbyname, mainbus_attach,
      DV_DULL, sizeof(struct device) };

/*
 * findzs() is called from the zs driver (which is, at least in theory,
 * generic to any machine with a Zilog ZSCC chip).  It should return the
 * address of the corresponding zs channel.  It may not fail, and it
 * may be called before the VM code can be used.  Here we count on the
 * FORTH PROM to map in the required zs chips.
 */
void *
findzs(zs)
	int zs;
{
	register int node, addr;

	node = firstchild(findroot());
	while ((node = findnode(node, "zs")) != 0) {
		if (getpropint(node, "slave", -1) == zs) {
			if ((addr = getpropint(node, "address", 0)) == 0)
				panic("findzs: zs%d not mapped by PROM", zs);
			return ((void *)addr);
		}
		node = nextsibling(node);
	}
	panic("findzs: cannot find zs%d", zs);
	/* NOTREACHED */
}

int
makememarr(ap, max, which)
	register struct memarr *ap;
	int max, which;
{
	struct v2rmi {
		int	zero;
		int	addr;
		int	len;
	} v2rmi[200];		/* version 2 rom meminfo layout */
#define	MAXMEMINFO (sizeof(v2rmi) / sizeof(*v2rmi))
	register struct v0mlist *mp;
	register int i, node, len;
	char *prop;

	switch (i = promvec->pv_romvec_vers) {

	case 0:
		/*
		 * Version 0 PROMs use a linked list to describe these
		 * guys.
		 */
		switch (which) {

		case MEMARR_AVAILPHYS:
			mp = *promvec->pv_v0mem.v0_physavail;
			break;

		case MEMARR_TOTALPHYS:
			mp = *promvec->pv_v0mem.v0_phystot;
			break;

		default:
			panic("makememarr");
		}
		for (i = 0; mp != NULL; mp = mp->next, i++) {
			if (i >= max)
				goto overflow;
			ap->addr = (u_int)mp->addr;
			ap->len = mp->nbytes;
			ap++;
		}
		break;

	default:
		printf("makememarr: hope version %d PROM is like version 2\n",
		    i);
		/* FALLTHROUGH */

	case 2:
		/*
		 * Version 2 PROMs use a property array to describe them.
		 */
		if (max > MAXMEMINFO) {
			printf("makememarr: limited to %d\n", MAXMEMINFO);
			max = MAXMEMINFO;
		}
		if ((node = findnode(firstchild(findroot()), "memory")) == 0)
			panic("makememarr: cannot find \"memory\" node");
		switch (which) {

		case MEMARR_AVAILPHYS:
			prop = "available";
			break;

		case MEMARR_TOTALPHYS:
			prop = "reg";
			break;

		default:
			panic("makememarr");
		}
		len = getprop(node, prop, (void *)v2rmi, sizeof v2rmi) /
		    sizeof(struct v2rmi);
		for (i = 0; i < len; i++) {
			if (i >= max)
				goto overflow;
			ap->addr = v2rmi[i].addr;
			ap->len = v2rmi[i].len;
			ap++;
		}
		break;
	}

	/*
	 * Success!  (Hooray)
	 */
	if (i == 0)
		panic("makememarr: no memory found");
	return (i);

overflow:
	/*
	 * Oops, there are more things in the PROM than our caller
	 * provided space for.  Truncate any extras.
	 */
	printf("makememarr: WARNING: lost some memory\n");
	return (i);
}

/*
 * Internal form of getprop().  Returns the actual length.
 */
int
getprop(node, name, buf, bufsiz)
	int node;
	char *name;
	void *buf;
	register int bufsiz;
{
	register struct nodeops *no;
	register int len;

	no = promvec->pv_nodeops;
	len = no->no_proplen(node, name);
	if (len > bufsiz) {
		printf("node %x property %s length %d > %d\n",
		    node, name, len, bufsiz);
#ifdef DEBUG
		panic("getprop");
#else
		return (0);
#endif
	}
	no->no_getprop(node, name, buf);
	return (len);
}

/*
 * Return a string property.  There is a (small) limit on the length;
 * the string is fetched into a static buffer which is overwritten on
 * subsequent calls.
 */
char *
getpropstring(node, name)
	int node;
	char *name;
{
	register int len;
	static char stringbuf[32];

	len = getprop(node, name, (void *)stringbuf, sizeof stringbuf - 1);
	stringbuf[len] = '\0';	/* usually unnecessary */
	return (stringbuf);
}

/*
 * Fetch an integer (or pointer) property.
 * The return value is the property, or the default if there was none.
 */
int
getpropint(node, name, deflt)
	int node;
	char *name;
	int deflt;
{
	register int len;
	char intbuf[16];

	len = getprop(node, name, (void *)intbuf, sizeof intbuf);
	if (len != 4)
		return (deflt);
	return (*(int *)intbuf);
}

/*
 * OPENPROM functions.  These are here mainly to hide the OPENPROM interface
 * from the rest of the kernel.
 */
int
firstchild(node)
	int node;
{

	return (promvec->pv_nodeops->no_child(node));
}

int
nextsibling(node)
	int node;
{

	return (promvec->pv_nodeops->no_nextnode(node));
}

/* Pass a string to the FORTH PROM to be interpreted */
void
rominterpret(s)
	register char *s;
{

	if (promvec->pv_romvec_vers < 2)
		promvec->pv_fortheval.v0_eval(strlen(s), s);
	else
		promvec->pv_fortheval.v2_eval(s);
}

volatile void
romhalt()
{

	promvec->pv_halt();
	panic("PROM exit failed");
}

volatile void
romboot(str)
	char *str;
{

	promvec->pv_reboot(str);
	panic("PROM boot failed");
}

callrom()
{

#ifdef notdef		/* sun4c FORTH PROMs do this for us */
	fb_unblank();
#endif
	promvec->pv_abort();
}

/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}

#define	DOSWAP			/* Change swdevt, argdev, and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	0,0,		/* 0 = xx */
};

#define	PARTITIONMASK	0x7
#define	PARTITIONSHIFT	3

/*
 * Attempt to find the device from which we were booted.
 * If we can do so, and not instructed not to do so,
 * change rootdev to correspond to the load device.
 */
setroot()
{
#ifdef notyet
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != (u_long)B_DEVMAGIC)
		return;
	majdev = (bootdev >> B_TYPESHIFT) & B_TYPEMASK;
	if (majdev > sizeof(devname) / sizeof(devname[0]))
		return;
	adaptor = (bootdev >> B_ADAPTORSHIFT) & B_ADAPTORMASK;
	part = (bootdev >> B_PARTITIONSHIFT) & B_PARTITIONMASK;
	unit = (bootdev >> B_UNITSHIFT) & B_UNITMASK;
	/*
	 * First, find the controller type which support this device.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_driver->d_name[0] == devname[majdev][0] &&
		    hd->hp_driver->d_name[1] == devname[majdev][1])
			break;
	if (hd->hp_driver == 0)
		return;
	/*
	 * Next, find the controller of that type corresponding to
	 * the adaptor number.
	 */
	for (hc = hp_cinit; hc->hp_driver; hc++)
		if (hc->hp_alive && hc->hp_unit == adaptor &&
		    hc->hp_driver == hd->hp_cdriver)
			break;
	if (hc->hp_driver == 0)
		return;
	/*
	 * Finally, find the device in question attached to that controller.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_alive && hd->hp_slave == unit &&
		    hd->hp_cdriver == hc->hp_driver &&
		    hd->hp_ctlr == hc->hp_unit)
			break;
	if (hd->hp_driver == 0)
		return;
	mindev = hd->hp_unit;
	/*
	 * Form a new rootdev
	 */
	mindev = (mindev << PARTITIONSHIFT) + part;
	orootdev = rootdev;
	rootdev = makedev(majdev, mindev);
	/*
	 * If the original rootdev is the same as the one
	 * just calculated, don't need to adjust the swap configuration.
	 */
	if (rootdev == orootdev)
		return;

	printf("Changing root device to %c%c%d%c\n",
		devname[majdev][0], devname[majdev][1],
		mindev >> PARTITIONSHIFT, part + 'a');

#ifdef DOSWAP
	mindev &= ~PARTITIONMASK;
	for (swp = swdevt; swp->sw_dev; swp++) {
		if (majdev == major(swp->sw_dev) &&
		    mindev == (minor(swp->sw_dev) & ~PARTITIONMASK)) {
			temp = swdevt[0].sw_dev;
			swdevt[0].sw_dev = swp->sw_dev;
			swp->sw_dev = temp;
			break;
		}
	}
	if (swp->sw_dev == 0)
		return;

	/*
	 * If argdev and dumpdev were the same as the old primary swap
	 * device, move them to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
	if (temp == argdev)
		argdev = swdevt[0].sw_dev;
#endif
#endif
}

/*
 * Return pointer to device we booted from. Return NULL if we can't
 * figure this out.
 * XXX currently only works for network devices.
 */

static struct bootinfo *
findbootdev()
{
	register struct bootinfo *bi;
	register char *bp;
	register int unit, controller;
	register struct ifnet *ifp;

	bi = &bootinfo;
	bp = bi->name;
printf("findbootdev: (v%d rom) trying \"%s(%x,%x,%x)\"... ",
    promvec->pv_romvec_vers, bp, bi->val[0], bi->val[1], bi->val[2]);

	/* Try network devices first */
	unit = bi->val[0];
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (unit == ifp->if_unit && strcmp(bp, ifp->if_name) == 0) {
printf("found \"%s%d\"\n", ifp->if_name, ifp->if_unit);
			bi->type = FS_NFS;
			bi->data = (caddr_t)ifp;
			return (bi);
		}
printf("not found\n");
	return (NULL);
}

gets(cp)
	register char *cp;
{
	register char *lp;
	register int c;

	lp = cp;
	for (;;) {
		c = cngetc();
		switch (c) {
		case '\n':
		case '\r':
			printf("\n");
			*lp++ = '\0';
			return;
		case '\b':
		case '\177':
		case '#':
			if (lp > cp) {
				lp--;
				printf(" \b ");
			}
			continue;
		case '@':
		case 'u'&037:
			lp = cp;
			cnputc('\n');
			continue;
		default:
			if (c < ' ')
				continue;
			cnputc(c);
			*lp++ = c;
		}
	}
}
