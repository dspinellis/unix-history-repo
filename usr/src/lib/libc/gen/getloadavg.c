/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getloadavg.c	6.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/kernel.h>
#include <sys/kinfo.h>
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
	struct loadavg loadinfo;
	int size, kmemfd, i;
	int alreadyopen = 1;
	int fscale;

	size = sizeof(loadinfo);
	if (getkerninfo(KINFO_LOADAVG, &loadinfo, &size, 0) < 0) {
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
		if (kvm_read((off_t)nl[X_AVERUNNABLE].n_value,
		    (char *)&loadinfo, sizeof(loadinfo)) != size)
			goto bad;
		/*
		 * Old kernel have fscale separately; if not found assume
		 * running new format.
		 */
		if (kvm_read( (off_t)nl[X_FSCALE].n_value, (char *)&fscale, 
		    sizeof(fscale)) == sizeof(fscale))
			loadinfo.fscale = fscale;
	}
	nelem = MIN(nelem, sizeof(loadinfo.ldavg) / sizeof(fixpt_t));
	for (i = 0; i < nelem; i++)
		loadavg[i] = (double) loadinfo.ldavg[i] / loadinfo.fscale;
	if (!alreadyopen)
		kvm_close();
	return (nelem);

bad:
	if (!alreadyopen)
		kvm_close();
	return (-1);
}
