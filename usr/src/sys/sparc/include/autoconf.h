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
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)autoconf.h	7.3 (Berkeley) %G%
 *
 * from: $Header: autoconf.h,v 1.10 92/11/26 02:04:32 torek Exp $ (LBL)
 */

/*
 * Autoconfiguration information.
 */

/*
 * Most devices are configured according to information kept in
 * the FORTH PROMs.  In particular, we extract the `name', `reg',
 * and `address' properties of each device attached to the mainbus;
 * other drives may also use this information.  The mainbus itself
 * (which `is' the CPU, in some sense) gets just the node, with a
 * fake name ("mainbus").
 */
#define	RA_MAXINTR	8		/* max interrupts per device */
struct romaux {
	const char *ra_name;		/* name from FORTH PROM */
	int	ra_node;		/* FORTH PROM node ID */
	int	ra_iospace;		/* register space (obio, etc) */
	void	*ra_paddr;		/* register physical address */
	int	ra_len;			/* register length */
	void	*ra_vaddr;		/* ROM mapped virtual address, or 0 */
	struct rom_intr {		/* interrupt information: */
		int	int_pri;		/* priority (IPL) */
		int	int_vec;		/* vector (always 0?) */
	} ra_intr[RA_MAXINTR];
	int	ra_nintr;		/* number of interrupt info elements */
	struct	bootpath *ra_bp;	/* used for locating boot device */
};

/*
 * The various getprop* functions obtain `properties' from the ROMs.
 * getprop() obtains a property as a byte-sequence, and returns its
 * length; the others convert or make some other guarantee.
 */
int	getprop __P((int node, char *name, void *buf, int bufsiz));
char	*getpropstring __P((int node, char *name));
int	getpropint __P((int node, char *name, int deflt));

/* Frequently used options node */
extern int optionsnode;

/*
 * The romprop function gets physical and virtual addresses from the PROM
 * and fills in a romaux.  It returns 1 on success, 0 if the physical
 * address is not available as a "reg" property.
 */
int	romprop __P((struct romaux *ra, const char *name, int node));

/*
 * The matchbyname function is useful in drivers that are matched
 * by romaux name, i.e., all `mainbus attached' devices.  It expects
 * its aux pointer to point to a pointer to the name (the address of
 * a romaux structure suffices, for instance).
 */
int	matchbyname __P((struct device *, struct cfdata *cf, void *aux));

/*
 * `clockfreq' produces a printable representation of a clock frequency
 * (this is just a frill).
 */
char	*clockfreq __P((int freq));

/*
 * mapiodev maps an I/O device to a virtual address, returning the address.
 * mapdev does the real work: you can supply a special virtual address and
 * it will use that instead of creating one, but you must only do this if
 * you get it from ../sparc/vaddrs.h.
 */
void	*mapdev __P((void *pa, int va, int size));
#define	mapiodev(pa, size)	mapdev(pa, 0, size)

/*
 * Memory description arrays.  Shared between pmap.c and autoconf.c; no
 * one else should use this (except maybe mem.c, e.g., if we fix the VM to
 * handle discontiguous physical memory).
 */
struct memarr {
	u_int	addr;
	u_int	len;
};
int	makememarr(struct memarr *, int max, int which);
#define	MEMARR_AVAILPHYS	0
#define	MEMARR_TOTALPHYS	1

/* Pass a string to the FORTH interpreter.  May fail silently. */
void	rominterpret __P((char *));

/* Openprom V2 style boot path */
struct bootpath {
	char	name[8];		/* name of this node */
	int	val[2];			/* up to two optional values */
};

struct device *bootdv;			/* found during autoconfiguration */
