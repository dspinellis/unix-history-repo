/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sum.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/uio.h>
#include <sys/namei.h>

#include <errno.h>
#include <kvm.h>
#include <limits.h>
#include <nlist.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <vm/vm.h>

#ifdef tahoe
#include <machine/cpu.h>
#endif

#include "extern.h"

int	pct __P((long, long));

#define	PCT(top, bot)	pct((long)(top), (long)(bot))

static struct nlist nl[] = {
#define	X_CNT		0
	{ "_cnt" },
#define X_NCHSTATS	1
	{ "_nchstats" },
#ifdef tahoe				/* XXX */
#define X_CKEYSTATS	2
	{ "_ckeystats" },
#define X_DKEYSTATS	3
	{ "_dkeystats" },
#endif
	0
};

void
dosum()
{
	long nchtotal;
	struct nchstats nchstats;
	struct vmmeter cnt;
#ifdef tahoe
	struct keystats keystats;
#endif

	knlist(nl);
	kread(nl[X_CNT].n_value, &cnt, sizeof cnt, "cnt");
	(void)printf("%9u cpu context switches\n", cnt.v_swtch);
	(void)printf("%9u device interrupts\n", cnt.v_intr);
	(void)printf("%9u software interrupts\n", cnt.v_soft);
#ifdef vax
	(void)printf("%9u pseudo-dma dz interrupts\n", cnt.v_pdma);
#endif
	(void)printf("%9u traps\n", cnt.v_trap);
	(void)printf("%9u system calls\n", cnt.v_syscall);
	(void)printf("%9u total faults taken\n", cnt.v_faults);
	(void)printf("%9u swap ins\n", cnt.v_swpin);
	(void)printf("%9u swap outs\n", cnt.v_swpout);
	(void)printf("%9u pages swapped in\n", cnt.v_pswpin / CLSIZE);
	(void)printf("%9u pages swapped out\n", cnt.v_pswpout / CLSIZE);
	(void)printf("%9u page ins\n", cnt.v_pageins);
	(void)printf("%9u page outs\n", cnt.v_pageouts);
	(void)printf("%9u pages paged in\n", cnt.v_pgpgin);
	(void)printf("%9u pages paged out\n", cnt.v_pgpgout);
	(void)printf("%9u pages reactivated\n", cnt.v_reactivated);
	(void)printf("%9u intransit blocking page faults\n", cnt.v_intrans);
	(void)printf("%9u zero fill pages created\n", cnt.v_nzfod / CLSIZE);
	(void)printf("%9u zero fill page faults\n", cnt.v_zfod / CLSIZE);
	(void)printf("%9u pages examined by the clock daemon\n", cnt.v_scan);
	(void)printf("%9u revolutions of the clock hand\n", cnt.v_rev);
	(void)printf("%9u VM object cache lookups\n", cnt.v_lookups);
	(void)printf("%9u VM object hits\n", cnt.v_hits);
	(void)printf("%9u total VM faults taken\n", cnt.v_vm_faults);
	(void)printf("%9u copy-on-write faults\n", cnt.v_cow_faults);
	(void)printf("%9u pages freed by daemon\n", cnt.v_dfree);
	(void)printf("%9u pages freed by exiting processes\n", cnt.v_pfree);
	(void)printf("%9u pages free\n", cnt.v_free_count);
	(void)printf("%9u pages wired down\n", cnt.v_wire_count);
	(void)printf("%9u pages active\n", cnt.v_active_count);
	(void)printf("%9u pages inactive\n", cnt.v_inactive_count);
	(void)printf("%9u bytes per page\n", cnt.v_page_size);
	kread(nl[X_NCHSTATS].n_value, &nchstats, sizeof nchstats, "nchstats");
	nchtotal = nchstats.ncs_goodhits + nchstats.ncs_neghits +
	    nchstats.ncs_badhits + nchstats.ncs_falsehits +
	    nchstats.ncs_miss + nchstats.ncs_long;
	(void)printf("%9ld total name lookups\n", nchtotal);
	(void)printf(
	    "%9s cache hits (%d%% pos + %d%% neg) system %d%% per-process\n",
	    "", PCT(nchstats.ncs_goodhits, nchtotal),
	    PCT(nchstats.ncs_neghits, nchtotal),
	    PCT(nchstats.ncs_pass2, nchtotal));
	(void)printf("%9s deletions %d%%, falsehits %d%%, toolong %d%%\n", "",
	    PCT(nchstats.ncs_badhits, nchtotal),
	    PCT(nchstats.ncs_falsehits, nchtotal),
	    PCT(nchstats.ncs_long, nchtotal));
#if defined(tahoe)
	kread(nl[X_CKEYSTATS].n_value, &keystats, sizeof keystats, "ckeystats");
	(void)printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "code cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
	kread(nl[X_DKEYSTATS].n_value, &keystats, sizeof keystats, "ckeystats");
	(void)printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "data cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
#endif
}

int
pct(top, bot)
	long top, bot;
{
	if (bot == 0)
		return (0);
	if (bot > (LONG_MAX / 100))
		return (top / (bot / 100));
	return ((top * 100) / bot);
}
