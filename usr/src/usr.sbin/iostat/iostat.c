/*-
 * Copyright (c) 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)iostat.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/buf.h>
#include <sys/dkstat.h>
#include <signal.h>
#include <fcntl.h>
#include <nlist.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <paths.h>
#include <kvm.h>

struct nlist nl[] = {
#define	X_DK_BUSY	0
	{ "_dk_busy" },
#define	X_DK_TIME	1
	{ "_dk_time" },
#define	X_DK_XFER	2
	{ "_dk_xfer" },
#define	X_DK_WDS	3
	{ "_dk_wds" },
#define	X_TK_NIN	4
	{ "_tk_nin" },
#define	X_TK_NOUT	5
	{ "_tk_nout" },
#define	X_DK_SEEK	6
	{ "_dk_seek" },
#define	X_CP_TIME	7
	{ "_cp_time" },
#define	X_DK_WPMS	8
	{ "_dk_wpms" },
#define	X_HZ		9
	{ "_hz" },
#define	X_PHZ		10
	{ "_phz" },
#define	X_DK_NDRIVE	11
	{ "_dk_ndrive" },
#define	X_END		11
#ifdef hp300
#define	X_HPDINIT	(X_END+1)
	{ "_hp_dinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_END+1)
	{ "_vbdinit" },
#endif
#ifdef vax
	{ "_mbdinit" },
#define X_MBDINIT	(X_END+1)
	{ "_ubdinit" },
#define X_UBDINIT	(X_END+2)
#endif
	{ 0 },
};

struct _disk {
	int	dk_busy;
	long	cp_time[CPUSTATES];
	long	*dk_time;
	long	*dk_wds;
	long	*dk_seek;
	long	*dk_xfer;
	long	tk_nin;
	long	tk_nout;
} cur, last;

double etime;
long *dk_wpms;
int dk_ndrive, *dr_select, hz, kmemfd, ndrives;
char **dr_name;

#include "names.c"				/* XXX */

#define nlread(x, v) \
	kvm_read((void *)nl[x].n_value, (void *)&(v), sizeof(v))

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int i;
	long tmp;
	int ch, hdrcnt, reps, interval, phz, ndrives;
	char **cp, *memfile, *namelist, buf[30];
	void printhdr(), read_names(), stats(), stat1(), usage(), error();

	interval = reps = 0;
	namelist = memfile = NULL;
	while ((ch = getopt(argc, argv, "c:M:N:w:")) != EOF)
		switch(ch) {
		case 'c':
			reps = atoi(optarg);
			break;
		case 'M':
			memfile = optarg;
			break;
		case 'N':
			namelist = optarg;
			break;
		case 'w':
			interval = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (kvm_openfiles(namelist, memfile, NULL) == -1) {
		error("kvm_openfiles: %s", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) == -1) {
		error("kvm_nlist: %s", kvm_geterr());
		exit(1);
	}
	if (nl[X_DK_BUSY].n_type == 0) {
		error("dk_busy not found namelist");
		exit(1);
	}
	if (nl[X_DK_NDRIVE].n_type == 0) {
		error("dk_ndrive not found in namelist");
		exit(1);
	}
	(void)nlread(X_DK_NDRIVE, dk_ndrive);
	if (dk_ndrive <= 0) {
		error("invalid dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}

	cur.dk_time = calloc(dk_ndrive, sizeof(long));
	cur.dk_wds = calloc(dk_ndrive, sizeof(long));
	cur.dk_seek = calloc(dk_ndrive, sizeof(long));
	cur.dk_xfer = calloc(dk_ndrive, sizeof(long));
	last.dk_time = calloc(dk_ndrive, sizeof(long));
	last.dk_wds = calloc(dk_ndrive, sizeof(long));
	last.dk_seek = calloc(dk_ndrive, sizeof(long));
	last.dk_xfer = calloc(dk_ndrive, sizeof(long));
	dr_select = calloc(dk_ndrive, sizeof(int));
	dr_name = calloc(dk_ndrive, sizeof(char *));
	dk_wpms = calloc(dk_ndrive, sizeof(long));

	for (i = 0; i < dk_ndrive; i++) {
		(void)sprintf(buf, "dk%d", i);
		dr_name[i] = strdup(buf);
	}
	read_names();
	(void)nlread(X_HZ, hz);
	(void)nlread(X_PHZ, phz);
	if (phz)
		hz = phz;
	(void)kvm_read((void *)nl[X_DK_WPMS].n_value, dk_wpms,
		dk_ndrive * sizeof(dk_wpms));

	/*
	 * Choose drives to be displayed.  Priority goes to (in order) drives
	 * supplied as arguments and default drives.  If everything isn't
	 * filled in and there are drives not taken care of, display the first
	 * few that fit.
	 *
	 * The backward compatibility #ifdefs permit the syntax:
	 *	iostat [ drives ] [ interval [ count ] ]
	 */
#define	BACKWARD_COMPATIBILITY
	for (ndrives = 0; *argv; ++argv) {
#ifdef	BACKWARD_COMPATIBILITY
		if (isdigit(**argv))
			break;
#endif
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], *argv))
				continue;
			dr_select[i] = 1;
			++ndrives;
		}
	}
#ifdef	BACKWARD_COMPATIBILITY
	if (*argv) {
		interval = atoi(*argv);
		if (*++argv)
			reps = atoi(*argv);
	}
#endif
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i] || dk_wpms[i] == 0)
			continue;
		for (cp = defdrives; *cp; cp++)
			if (strcmp(dr_name[i], *cp) == 0) {
				dr_select[i] = 1;
				++ndrives;
				break;
			}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		dr_select[i] = 1;
		++ndrives;
	}

	(void)signal(SIGCONT, printhdr);

	for (hdrcnt = 1;;) {
		if (!--hdrcnt) {
			printhdr();
			hdrcnt = 20;
		}
		(void)nlread(X_DK_BUSY, cur.dk_busy);
		(void)kvm_read((void *)nl[X_DK_TIME].n_value,
		    cur.dk_time, dk_ndrive * sizeof(long));
		(void)kvm_read((void *)nl[X_DK_XFER].n_value,
		    cur.dk_xfer, dk_ndrive * sizeof(long));
		(void)kvm_read((void *)nl[X_DK_WDS].n_value,
		    cur.dk_wds, dk_ndrive * sizeof(long));
		(void)kvm_read((void *)nl[X_DK_SEEK].n_value,
		    cur.dk_seek, dk_ndrive * sizeof(long));
		(void)kvm_read((void *)nl[X_TK_NIN].n_value,
		    &cur.tk_nin, sizeof(cur.tk_nin));
		(void)kvm_read((void *)nl[X_TK_NOUT].n_value,
		    &cur.tk_nout, sizeof(cur.tk_nout));
		(void)kvm_read((void *)nl[X_CP_TIME].n_value,
		    cur.cp_time, sizeof(cur.cp_time));
		for (i = 0; i < dk_ndrive; i++) {
			if (!dr_select[i])
				continue;
#define X(fld)	tmp = cur.fld[i]; cur.fld[i] -= last.fld[i]; last.fld[i] = tmp
			X(dk_xfer);
			X(dk_seek);
			X(dk_wds);
			X(dk_time);
		}
		tmp = cur.tk_nin;
		cur.tk_nin -= last.tk_nin;
		last.tk_nin = tmp;
		tmp = cur.tk_nout;
		cur.tk_nout -= last.tk_nout;
		last.tk_nout = tmp;
		etime = 0;
		for (i = 0; i < CPUSTATES; i++) {
			X(cp_time);
			etime += cur.cp_time[i];
		}
		if (etime == 0.0)
			etime = 1.0;
		etime /= (float)hz;
		(void)printf("%4.0f%5.0f",
		    cur.tk_nin / etime, cur.tk_nout / etime);
		for (i = 0; i < dk_ndrive; i++)
			if (dr_select[i])
				stats(i);
		for (i = 0; i < CPUSTATES; i++)
			stat1(i);
		(void)printf("\n");
		(void)fflush(stdout);

		if (--reps <= 0)
			break;
		if (interval)
			(void)sleep(interval);
	}
	exit(0);
}

void
printhdr()
{
	register int i;

	(void)printf("      tty");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			(void)printf("          %3.3s ", dr_name[i]);
	(void)printf("         cpu\n tin tout");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			(void)printf(" bps tps msps ");
	(void)printf(" us ni sy id\n");
}

void
stats(dn)
	int dn;
{
	double atime, words, xtime, itime;

	if (dk_wpms[dn] == 0) {
		(void)printf("%4.0f%4.0f%5.1f ", 0.0, 0.0, 0.0);
		return;
	}
	atime = cur.dk_time[dn];
	atime /= (float)hz;
	words = cur.dk_wds[dn] * 32.0;	/* number of words transferred */
	xtime = words / dk_wpms[dn];	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	(void)printf("%4.0f", words / 512 / etime);
	(void)printf("%4.0f", cur.dk_xfer[dn] / etime);
	(void)printf("%5.1f ",
	    cur.dk_seek[dn] ? itime * 1000. / cur.dk_seek[dn] : 0.0);
}

void
stat1(state)
	int state;
{
	register int i;
	double time;

	time = 0;
	for (i = 0; i < CPUSTATES; i++)
		time += cur.cp_time[i];
	if (time == 0.0)
		time = 1.0;
	(void)printf("%3.0f", 100. * cur.cp_time[state] / time);
}

void
usage()
{
	(void)fprintf(stderr,
"usage: iostat [-c count] [-M core] [-N system] [-w wait] [drives]\n");
	exit(1);
}

void
error(fmt)
	char *fmt;
{
	va_list ap;

        va_start(ap, fmt);
        (void)fprintf(stderr, "iostat: ");
        (void)vfprintf(stderr, fmt, ap);
        (void)fprintf(stderr, "\n");
        va_end(ap);
}
