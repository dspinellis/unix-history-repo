/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Computer Systems Engineering group at Lawrence Berkeley
 * Laboratory under DARPA contract BG 91-66.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)subr_autoconf.c	7.1 (Berkeley) %G%
 *
 * from: $Header: subr_autoconf.c,v 1.3 91/11/23 00:53:49 torek Exp $ (LBL)
 */

#include "sys/param.h"
#include "sys/device.h"
#include "sys/malloc.h"

/*
 * Autoconfiguration subroutines.
 */

extern struct cfdata cfdata[];		/* from ioconf.c */

#define	ROOT ((struct device *)NULL)

/*
 * Iterate over all potential children of some device, calling the given
 * function (default being the child's match function) for each one.
 * Nonzero returns are matches; the highest value returned is considered
 * the best match.  Return the `found child' if we got a match, or NULL
 * otherwise.  The `aux' pointer is simply passed on through.
 *
 * Note that this function is designed so that it can be used to apply
 * an arbitrary function to all potential children (its return value
 * can be ignored).
 */
struct cfdata *
config_search(fn, dev, aux)
	register cfmatch_t fn;
	register struct device *dev;
	register void *aux;
{
	register struct cfdata *cf, *pcf, *match = NULL;
	register short *p;
	register int pri, bestpri = 0;

	for (cf = cfdata; cf->cf_driver; cf++) {
		/*
		 * Skip cf if no longer eligible, or if a root entry.
		 * Otherwise scan through parents for one matching `dev'
		 * (and alive), and try match function.
		 */
		if (cf->cf_fstate == FSTATE_FOUND ||
		    (p = cf->cf_parents) == NULL)
			continue;
		while (*p >= 0) {
			pcf = &cfdata[*p++];
			if (pcf->cf_fstate != FSTATE_FOUND ||
			    pcf->cf_driver->cd_name != dev->dv_name ||
			    pcf->cf_unit != dev->dv_unit)
				continue;
			if (fn != NULL)
				pri = (*fn)(dev, cf, aux);
			else
				pri = (*cf->cf_driver->cd_match)(dev, cf, aux);
			if (pri > bestpri) {
				match = cf;
				bestpri = pri;
			}
		}
	}
	return (match);
}

/*
 * Find the given root device.
 * This is much like config_search, but there is no parent.
 */
struct cfdata *
config_rootsearch(fn, rootname, aux)
	register cfmatch_t fn;
	register char *rootname;
	register void *aux;
{
	register struct cfdata *cf, *match = NULL;
	register int pri, bestpri = 0;

	for (cf = cfdata; cf->cf_driver; cf++) {
		/*
		 * Look at root entries for matching name.
		 * We do not bother with found-state here
		 * since only one root should ever be searched.
		 */
		if (cf->cf_parents != NULL || cf->cf_unit != 0 ||
		    strcmp(cf->cf_driver->cd_name, rootname) != 0)
			continue;
		if (fn != NULL)
			pri = (*fn)(ROOT, cf, aux);
		else
			pri = (*cf->cf_driver->cd_match)(ROOT, cf, aux);
		if (pri > bestpri) {
			match = cf;
			bestpri = pri;
		}
	}
	return (match);
}

static char *msgs[3] = { "", " not configured\n", " unsupported\n" };

/*
 * The given `aux' argument describes a device that has been found
 * on the given parent, but not necessarily configured.  Locate the
 * configuration data for that device (using the cd_match configuration
 * driver function) and attach it, and return true.  If the device was
 * not configured, call the given `print' function and return 0.
 */
int
config_found(parent, aux, print)
	struct device *parent;
	void *aux;
	cfprint_t print;
{
	struct cfdata *cf;

	if ((cf = config_search((cfmatch_t)NULL, parent, aux)) != NULL) {
		config_attach(parent, cf, aux, print);
		return (1);
	}
	printf(msgs[(*print)(aux, parent->dv_xname)]);
	return (0);
}

/*
 * As above, but for root devices.
 */
int
config_rootfound(rootname, aux)
	char *rootname;
	void *aux;
{
	struct cfdata *cf;

	if ((cf = config_rootsearch((cfmatch_t)NULL, rootname, aux)) != NULL) {
		config_attach(ROOT, cf, aux, (cfprint_t)NULL);
		return (1);
	}
	printf("root device %s not configured\n", rootname);
	return (0);
}

/* just like sprintf(buf, "%d") except that it works from the end */
static char *
number(ep, n)
	register char *ep;
	register int n;
{

	*--ep = 0;
	while (n >= 10) {
		*--ep = (n % 10) + '0';
		n /= 10;
	}
	*--ep = n + '0';
	return (ep);
}

/*
 * Attach a found device.  Allocates memory for device variables.
 */
void
config_attach(parent, cf, aux, print)
	register struct device *parent;
	register struct cfdata *cf;
	register void *aux;
	cfprint_t print;
{
	register struct device *dev;
	register struct cfdriver *cd;
	register size_t lname, lunit;
	register char *xunit;
	char num[10];

	cd = cf->cf_driver;
	if (cd->cd_devsize < sizeof(struct device))
		panic("config_attach");
	if (cf->cf_fstate == FSTATE_NOTFOUND)
		cf->cf_fstate = FSTATE_FOUND;

	/* compute length of name and decimal expansion of unit number */
	lname = strlen(cd->cd_name);
	xunit = number(&num[sizeof num], cf->cf_unit);
	lunit = &num[sizeof num] - xunit;

	/* get memory for all device vars, plus expanded name */
	dev = (struct device *)malloc(cd->cd_devsize + lname + lunit,
	    M_DEVBUF, M_WAITOK);		/* XXX cannot wait! */
	bzero(dev, cd->cd_devsize);
	dev->dv_name = cd->cd_name;
	dev->dv_unit = cf->cf_unit;
	dev->dv_flags = cf->cf_flags;
	dev->dv_xname = (char *)dev + cd->cd_devsize;
	bcopy(dev->dv_name, dev->dv_xname, lname);
	bcopy(xunit, dev->dv_xname + lname, lunit);
	dev->dv_parent = parent;
	if (parent == ROOT)
		printf("%s (root)", dev->dv_xname);
	else {
		printf("%s at %s", dev->dv_xname, parent->dv_xname);
		(void) (*print)(aux, (char *)0);
	}

	/* put this device in the devices array */
	if (dev->dv_unit >= cd->cd_ndevs) {
		/*
		 * Need to expand the array.
		 */
		int old = cd->cd_ndevs, oldbytes, new, newbytes;
		void **nsp;

		if (old == 0) {
			nsp = malloc(MINALLOCSIZE, M_DEVBUF, M_WAITOK);	/*XXX*/
			bzero(nsp, MINALLOCSIZE);
			cd->cd_ndevs = MINALLOCSIZE / sizeof(void *);
		} else {
			new = cd->cd_ndevs;
			do {
				new *= 2;
			} while (new <= dev->dv_unit);
			cd->cd_ndevs = new;
			oldbytes = old * sizeof(void *);
			newbytes = new * sizeof(void *);
			nsp = malloc(newbytes, M_DEVBUF, M_WAITOK);	/*XXX*/
			bcopy(cd->cd_devs, nsp, oldbytes);
			bzero(&nsp[old], newbytes - oldbytes);
			free(cd->cd_devs, M_DEVBUF);
		}
		cd->cd_devs = nsp;
	}
	cd->cd_devs[dev->dv_unit] = dev;
	(*cd->cd_attach)(parent, dev, aux);
}
