/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)wr_bparam.c	5.4 (Berkeley) %G%
 *
 * $Id: wr_bparam.c,v 5.2.2.1 1992/02/09 15:09:46 jsp beta $
 *
 */

#include "../fsinfo/fsinfo.h"

/*
 * Write a host/path in NFS format
 */
static int write_nfsname(ef, fp, hn)
FILE *ef;
fsmount *fp;
char *hn;
{
	int errors = 0;
	char *h = strdup(fp->f_ref->m_dk->d_host->h_hostname);
	domain_strip(h, hn);
	fprintf(ef, "%s:%s", h, fp->f_volname);
	free(h);
	return errors;
}

/*
 * Write a bootparams entry for a host
 */
static int write_boot_info(ef, hp)
FILE *ef;
host *hp;
{
	int errors = 0;
	fprintf(ef, "%s\troot=", hp->h_hostname);
	errors += write_nfsname(ef, hp->h_netroot, hp->h_hostname);
	fputs(" swap=", ef);
	errors += write_nfsname(ef, hp->h_netswap, hp->h_hostname);
	fputs("\n", ef);

	return 0;
}

/*
 * Output a bootparams file
 */
int write_bootparams(q)
qelem *q;
{
	int errors = 0;

	if (bootparams_pref) {
		FILE *ef = pref_open(bootparams_pref, "bootparams", info_hdr, "bootparams");
		if (ef) {
			host *hp;
			ITER(hp, host, q)
				if (hp->h_netroot && hp->h_netswap)
					errors += write_boot_info(ef, hp);
			errors += pref_close(ef);
		} else {
			errors++;
		}
	}

	return errors;
}
