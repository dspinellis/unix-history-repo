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
static char sccsid[] = "@(#)iostat.c	5.1 (Berkeley) %G%";
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
#include <string.h>
#include <paths.h>

struct nlist nl[] = {
	{ "_dk_busy" },
#define	X_DK_BUSY	0
	{ "_dk_time" },
#define	X_DK_TIME	1
	{ "_dk_xfer" },
#define	X_DK_XFER	2
	{ "_dk_wds" },
#define	X_DK_WDS	3
	{ "_tk_nin" },
#define	X_TK_NIN	4
	{ "_tk_nout" },
#define	X_TK_NOUT	5
	{ "_dk_seek" },
#define	X_DK_SEEK	6
	{ "_cp_time" },
#define	X_CP_TIME	7
	{ "_dk_wpms" },
#define	X_DK_WPMS	8
	{ "_hz" },
#define	X_HZ		9
	{ "_phz" },
#define	X_PHZ		10
	{ "_dk_ndrive" },
#define	X_DK_NDRIVE	11
#ifdef vax
	{ "_mbdinit" },
#define X_MBDINIT	(X_DK_NDRIVE+1)
	{ "_ubdinit" },
#define X_UBDINIT	(X_DK_NDRIVE+2)
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_DK_NDRIVE+1)
	{ "_vbdinit" },
#endif
	{ 0 },
};

double etime;
long *dk_wpms;
int *dr_select;
int dk_ndrive, hz, kmemfd, ndrives;
char **dr_name;

#ifdef vax
char	*defdrives[] = { "hp0", "hp1", "hp2",  0 };
#else
char	*defdrives[] = { 0 };
#endif

struct {
	int	dk_busy;
	long	cp_time[CPUSTATES];
	long	*dk_time;
	long	*dk_wds;
	long	*dk_seek;
	long	*dk_xfer;
	long	tk_nin;
	long	tk_nout;
} s, s1;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int i;
	long t;
	int ch, hdrcnt, reps, interval, phz, ndrives;
	char *arg, **cp, buf[BUFSIZ];
	void printhdr(), read_names(), stats(), stat1(), usage();

	interval = reps = 0;
	while ((ch = getopt(argc, argv, "c:i:")) != EOF)
		switch(ch) {
		case 'c':
			reps = atoi(optarg);
			break;
		case 'i':
			interval = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	nlist(_PATH_UNIX, nl);
	if (nl[X_DK_BUSY].n_type == 0) {
		(void)fprintf(stderr,
		    "iostat: dk_busy not found in %s namelist\n",
		    _PATH_UNIX);
		exit(1);
	}
	kmemfd = open(_PATH_KMEM, O_RDONLY);
	if (kmemfd < 0) {
		(void)fprintf(stderr, "iostat: cannot open %s\n", _PATH_KMEM);
		exit(1);
	}

	if (nl[X_DK_NDRIVE].n_value == 0) {
		(void)fprintf(stderr,
		    "iostat: dk_ndrive not found in %s namelist\n",
		    _PATH_UNIX);
		exit(1);
	}
	(void)lseek(kmemfd, nl[X_DK_NDRIVE].n_value, L_SET);
	(void)read(kmemfd, &dk_ndrive, sizeof(dk_ndrive));
	if (dk_ndrive <= 0) {
		(void)fprintf(stderr, "iostat: dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}

	s.dk_time = calloc(dk_ndrive, sizeof(long));
	s.dk_wds = calloc(dk_ndrive, sizeof(long));
	s.dk_seek = calloc(dk_ndrive, sizeof(long));
	s.dk_xfer = calloc(dk_ndrive, sizeof(long));
	s1.dk_time = calloc(dk_ndrive, sizeof(long));
	s1.dk_wds = calloc(dk_ndrive, sizeof(long));
	s1.dk_seek = calloc(dk_ndrive, sizeof(long));
	s1.dk_xfer = calloc(dk_ndrive, sizeof(long));
	dr_select = calloc(dk_ndrive, sizeof(int));
	dr_name = calloc(dk_ndrive, sizeof(char *));
	dk_wpms = calloc(dk_ndrive, sizeof(long));

	for (arg = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = arg;
		(void)sprintf(dr_name[i], "dk%d", i);
		arg += strlen(dr_name[i]) + 1;
	}
	read_names();
	(void)lseek(kmemfd, (long)nl[X_HZ].n_value, L_SET);
	(void)read(kmemfd, &hz, sizeof(hz));
	(void)lseek(kmemfd, (long)nl[X_PHZ].n_value, L_SET);
	(void)read(kmemfd, &phz, sizeof(phz));
	if (phz)
		hz = phz;
	(void)lseek(kmemfd, (long)nl[X_DK_WPMS].n_value, L_SET);
	(void)read(kmemfd, dk_wpms, dk_ndrive * sizeof(dk_wpms));

	/*
	 * Choose drives to be displayed.  Priority goes to (in order) drives
	 * supplied as arguments and default drives.  If everything isn't
	 * filled in and there are drives not taken care of, display the first
	 * few that fit.
	 */
	for (ndrives = 0; *argv; ++argv)
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], *argv))
				continue;
			dr_select[i] = 1;
			++ndrives;
		}
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
		(void)lseek(kmemfd, (long)nl[X_DK_BUSY].n_value, L_SET);
		(void)read(kmemfd, &s.dk_busy, sizeof(s.dk_busy));
		(void)lseek(kmemfd, (long)nl[X_DK_TIME].n_value, L_SET);
		(void)read(kmemfd, s.dk_time, dk_ndrive * sizeof(long));
		(void)lseek(kmemfd, (long)nl[X_DK_XFER].n_value, L_SET);
		(void)read(kmemfd, s.dk_xfer, dk_ndrive * sizeof(long));
		(void)lseek(kmemfd, (long)nl[X_DK_WDS].n_value, L_SET);
		(void)read(kmemfd, s.dk_wds, dk_ndrive * sizeof(long));
		(void)lseek(kmemfd, (long)nl[X_DK_SEEK].n_value, L_SET);
		(void)read(kmemfd, s.dk_seek, dk_ndrive * sizeof(long));
		(void)lseek(kmemfd, (long)nl[X_TK_NIN].n_value, L_SET);
		(void)read(kmemfd, &s.tk_nin, sizeof(s.tk_nin));
		(void)lseek(kmemfd, (long)nl[X_TK_NOUT].n_value, L_SET);
		(void)read(kmemfd, &s.tk_nout, sizeof(s.tk_nout));
		(void)lseek(kmemfd, (long)nl[X_CP_TIME].n_value, L_SET);
		(void)read(kmemfd, s.cp_time, sizeof(s.cp_time));
		for (i = 0; i < dk_ndrive; i++) {
			if (!dr_select[i])
				continue;
#define X(fld)	t = s.fld[i]; s.fld[i] -= s1.fld[i]; s1.fld[i] = t
			X(dk_xfer);
			X(dk_seek);
			X(dk_wds);
			X(dk_time);
		}
		t = s.tk_nin;
		s.tk_nin -= s1.tk_nin;
		s1.tk_nin = t;
		t = s.tk_nout;
		s.tk_nout -= s1.tk_nout;
		s1.tk_nout = t;
		etime = 0;
		for (i = 0; i < CPUSTATES; i++) {
			X(cp_time);
			etime += s.cp_time[i];
		}
		if (etime == 0.0)
			etime = 1.0;
		etime /= (float) hz;
		(void)printf("%4.0f%5.0f", s.tk_nin / etime, s.tk_nout / etime);
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
	atime = s.dk_time[dn];
	atime /= (float)hz;
	words = s.dk_wds[dn] * 32.0;	/* number of words transferred */
	xtime = words / dk_wpms[dn];	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	(void)printf("%4.0f", words / 512 / etime);
	(void)printf("%4.0f", s.dk_xfer[dn] / etime);
	(void)printf("%5.1f ",
	    s.dk_seek[dn] ? itime * 1000. / s.dk_seek[dn] : 0.0);
}

void
stat1(state)
	int state;
{
	register int i;
	double time;

	time = 0;
	for (i = 0; i < CPUSTATES; i++)
		time += s.cp_time[i];
	if (time == 0.0)
		time = 1.0;
	(void)printf("%3.0f", 100. * s.cp_time[state] / time);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: iostat [-c count] [-i interval] [drives]\n");
	exit(1);
}

#define	steal(where, var) \
	(void)lseek(kmemfd, (where), L_SET); \
	(void)read(kmemfd, &var, sizeof(var));

#ifdef vax
#include <vax/uba/ubavar.h>
#include <vax/mba/mbavar.h>

void
read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nl[X_MBDINIT].n_value;
	up = (struct uba_device *) nl[X_UBDINIT].n_value;
	if (up == 0) {
		(void)fprintf(stderr,
		    "iostat: disk init info not in namelist\n");
		exit(1);
	}
	if (mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		(void)sprintf(dr_name[mdev.mi_dk], "%c%c%d",
		    cp[0], cp[1], mdev.mi_unit);
	}
	if (up) for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		(void)sprintf(dr_name[udev.ui_dk], "%c%c%d",
		    cp[0], cp[1], udev.ui_unit);
	}
}
#endif /* vax */

#ifdef tahoe
#include <tahoe/vba/vbavar.h>

void
read_names()
{
	struct vba_device udev, *up;
	struct vba_driver udrv;
	short two_char;
	char *cp = (char *)&two_char;

	up = (struct vba_device *)nl[X_VBDINIT].n_value;
	if (up == 0) {
		(void)fprintf(stderr,
		    "iostat: disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		(void)sprintf(dr_name[udev.ui_dk], "%c%c%d",
		     cp[0], cp[1], udev.ui_unit);
	}
}
#endif /* tahoe */
