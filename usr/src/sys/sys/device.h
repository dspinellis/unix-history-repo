/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software developed by the Computer Systems
 * Engineering group at Lawrence Berkeley Laboratory under DARPA
 * contract BG 91-66 and contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)device.h	7.2 (Berkeley) %G%
 *
 * from: $Header: device.h,v 1.3 92/01/15 18:25:53 torek Exp $ (LBL)
 */

/*
 * Minimal device structures.
 */
enum devtype { DV_DULL, DV_DISK, DV_TAPE, DV_TTY };

struct device {
/*	enum	devclass dv_class;	/* class */
	char	*dv_name;		/* device name */
	int	dv_unit;		/* device unit number */
	int	dv_flags;		/* flags (copied from config) */
	char	*dv_xname;		/* expanded name (name + unit) */
	struct	device *dv_parent;	/* pointer to parent device */
};

/*
 * Configuration data (i.e., data placed in ioconf.c).
 */
struct cfdata {
	struct	cfdriver *cf_driver;	/* config driver */
	short	cf_unit;		/* unit number */
	short	cf_fstate;		/* finding state (below) */
	int	*cf_loc;		/* locators (machine dependent) */
	int	cf_flags;		/* flags from config */
	short	*cf_parents;		/* potential parents */
	void	(**cf_ivstubs)();	/* config-generated vectors, if any */
};
#define FSTATE_NOTFOUND	0	/* has not been found */
#define	FSTATE_FOUND	1	/* has been found */
#define	FSTATE_STAR	2	/* duplicable leaf (unimplemented) */

typedef int (*cfmatch_t) __P((struct device *, struct cfdata *, void *));

/*
 * `configuration' driver (what the machine-independent autoconf uses).
 * As devices are found, they are applied against all the potential matches.
 * The one with the best match is taken, and a device structure (plus any
 * other data desired) is allocated.  Pointers to these are placed into
 * an array of pointers.  The array itself must be dynamic since devices
 * can be found long after the machine is up and running.
 */
struct cfdriver {
	void	**cd_devs;		/* devices found */
	char	*cd_name;		/* device name */
	cfmatch_t cd_match;		/* returns a match level */
	void	(*cd_attach) __P((struct device *, struct device *, void *));
	size_t	cd_devsize;		/* size of dev data (for malloc) */
	void	*cd_aux;		/* additional driver, if any */
	int	cd_ndevs;		/* size of cd_devs array */
};

/*
 * Configuration printing functions, and their return codes.  The second
 * argument is NULL if the device was configured; otherwise it is the name
 * of the parent device.  The return value is ignored if the device was
 * configured, so most functions can return UNCONF unconditionally.
 */
typedef int (*cfprint_t) __P((void *, char *));
#define	QUIET	0		/* print nothing */
#define	UNCONF	1		/* print " not configured\n" */
#define	UNSUPP	2		/* print " not supported\n" */

struct cfdata *config_search __P((cfmatch_t, struct device *, void *));
struct cfdata *config_rootsearch __P((cfmatch_t, char *, void *));
int config_found __P((struct device *, void *, cfprint_t));
int config_rootfound __P((char *, void *));
void config_attach __P((struct device *, struct cfdata *, void *, cfprint_t));
