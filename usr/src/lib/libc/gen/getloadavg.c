/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getloadavg.c	6.1 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>

#include <nlist.h>
#include <paths.h>

static char *kmem = _PATH_KMEM;
static char *vmunix = _PATH_UNIX;

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
	off_t lseek();
	static int need_nlist = 1;
	fixpt_t	averunnable[3];
	int fscale, kmemfd, i;

	/* nlist() is slow; cache result */
	if (need_nlist) {
		if (nlist(vmunix, nl) != 0)
			return (-1);
		if (nl[X_AVERUNNABLE].n_type == 0 || nl[X_FSCALE].n_type == 0)
			return (-1);
		need_nlist = 0;
	}

	if ((kmemfd = open(kmem, O_RDONLY, 0)) < 0)
		return (-1);
	if (lseek(kmemfd, (off_t)nl[X_AVERUNNABLE].n_value, L_SET) == -1)
		goto bad;
	if (read(kmemfd, (char *)averunnable, sizeof(averunnable)) < 0)
		goto bad;
	if (lseek(kmemfd, (off_t)nl[X_FSCALE].n_value, L_SET) == -1)
		goto bad;
	if (read(kmemfd, (char *)&fscale, sizeof(fscale)) < 0)
		goto bad;
	(void) close(kmemfd);

	nelem = MIN(nelem, sizeof(averunnable) / sizeof(averunnable[0]));
	for (i = 0; i < nelem; i++)
		loadavg[i] = (double) averunnable[i] / fscale;
	return (nelem);

bad:
	(void) close(kmemfd);
	return (-1);
}
