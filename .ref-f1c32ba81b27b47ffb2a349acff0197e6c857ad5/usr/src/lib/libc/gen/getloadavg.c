/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getloadavg.c	6.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <nlist.h>

static struct nlist nl[] = {
	{ "_averunnable" },
#define	X_AVERUNNABLE	0
	{ "_fscale" },
#define	X_FSCALE	1
	{ "" },
};

/*
 *  getloadavg() -- Get system load averages.
 *
 *  Put `nelem' samples into `loadavg' array.
 *  Return number of samples retrieved, or -1 on error.
 */
getloadavg(loadavg, nelem)
	double loadavg[];
	int nelem;
{
	static int need_nlist = 1;
	fixpt_t	averunnable[3];
	int fscale, kmemfd, i;
	int alreadyopen;

	if ((alreadyopen = kvm_openfiles(NULL, NULL, NULL)) == -1)
		return (-1);
	/* 
	 * cache nlist 
	 */
	if (need_nlist) {
		if (kvm_nlist(nl) != 0)
			goto bad;
		need_nlist = 0;
	}
	if (kvm_read((off_t)nl[X_AVERUNNABLE].n_value, (char *)averunnable, 
	    sizeof(averunnable)) != sizeof(averunnable))
		goto bad;
	if (kvm_read( (off_t)nl[X_FSCALE].n_value, (char *)&fscale, 
	    sizeof(fscale)) != sizeof(fscale))
		goto bad;
	nelem = MIN(nelem, sizeof(averunnable) / sizeof(averunnable[0]));
	for (i = 0; i < nelem; i++)
		loadavg[i] = (double) averunnable[i] / fscale;
	if (!alreadyopen)
		kvm_close();
	return (nelem);

bad:
	if (!alreadyopen)
		kvm_close();
	return (-1);
}
