/*
 * Copyright (c) 1983, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kgmon.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/gprof.h>
#include <errno.h>
#include <kvm.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <nlist.h>
#include <ctype.h>
#include <paths.h>

#define	PROFILING_ON	0
#define	PROFILING_OFF	3

struct nlist nl[] = {
#define N_FROMS		0
	{ "_froms" },
#define	N_PROFILING	1
	{ "_profiling" },
#define	N_S_LOWPC	2
	{ "_s_lowpc" },
#define	N_S_TEXTSIZE	3
	{ "_s_textsize" },
#define	N_SBUF		4
	{ "_sbuf" },
#define N_SSIZ		5
	{ "_ssiz" },
#define	N_TOS		6
	{ "_tos" },
	0,
};

kvm_t	*kd;
int	bflag, hflag, kflag, rflag, pflag;
int	debug = 0;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch, mode, disp, openmode;
	char *system, *kmemf;
	char errbuf[_POSIX2_LINE_MAX];

	kmemf = NULL;
	system = NULL;
	while ((ch = getopt(argc, argv, "M:N:bhpr")) != EOF)
		switch((char)ch) {
		case 'M':
			kmemf = optarg;
			kflag = 1;
			break;
		case 'N':
			system = optarg;
			break;
		case 'b':
			bflag = 1;
			break;
		case 'h':
			hflag = 1;
			break;
		case 'p':
			pflag = 1;
			break;
		case 'r':
			rflag = 1;
			break;
		default:
			(void)fprintf(stderr,
			    "usage: kgmon [-bhrp] [-M core] [-N system]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

#define BACKWARD_COMPATIBILITY
#ifdef	BACKWARD_COMPATIBILITY
	if (*argv) {
		system = *argv;
		if (*++argv) {
			kmemf = *argv;
			++kflag;
		}
	}
#endif

	if (system == NULL)
		system = _PATH_UNIX;
	openmode = (bflag || hflag || pflag || rflag) ? O_RDWR : O_RDONLY;
	kd = kvm_openfiles(system, kmemf, NULL, openmode, errbuf);
	if (kd == NULL) {
		if (openmode == O_RDWR) {
			openmode = O_RDONLY;
			kd = kvm_openfiles(system, kmemf, NULL, O_RDONLY,
			    errbuf);
		}
		if (kd == NULL) {
			(void)fprintf(stderr, "kgmon: kvm_openfiles: %s\n",
			    errbuf);
			exit(2);
		}
		(void)fprintf(stderr, "kgmon: kernel opened read-only\n");
		if (rflag)
			(void)fprintf(stderr, "-r supressed\n");
		if (bflag)
			(void)fprintf(stderr, "-b supressed\n");
		if (hflag)
			(void)fprintf(stderr, "-h supressed\n");
		rflag = bflag = hflag = 0;
	}
	if (kvm_nlist(kd, nl) < 0) {
		(void)fprintf(stderr, "kgmon: %s: no namelist\n", system);
		exit(2);
	}
	if (!nl[N_PROFILING].n_value) {
		(void)fprintf(stderr,
		    "kgmon: profiling not defined in kernel.\n");
		exit(10);
	}
	mode = kfetch(N_PROFILING);
	if (hflag)
		disp = PROFILING_OFF;
	else if (bflag)
		disp = PROFILING_ON;
	else
		disp = mode;
	if (pflag) {
		if (openmode == O_RDONLY && mode == PROFILING_ON)
			(void)fprintf(stderr, "data may be inconsistent\n");
		dumpstate();
	}
	if (rflag)
		resetstate();
	if (openmode == O_RDWR)
		turnonoff(disp);
	(void)fprintf(stdout,
	    "kernel profiling is %s.\n", disp ? "off" : "running");
	exit(0);
}

dumpstate()
{
	struct rawarc rawarc;
	struct tostruct *tos;
	u_long frompc;
	u_short *froms;		/* froms is a bunch of u_shorts indexing tos */
	int i, n, ret, fd;
	int fromindex, endfrom, toindex;
	u_int fromssize, tossize;
	u_long s_textsize;
	off_t sbuf;
	char *s_lowpc;
	char buf[MAXBSIZE];

	turnonoff(PROFILING_OFF);
	fd = open("gmon.out", O_WRONLY | O_CREAT, 0666);
	if (fd < 0) {
		perror("gmon.out");
		return;
	}
	sbuf = kfetch(N_SBUF);
	for (i = kfetch(N_SSIZ); i != 0; i -= n, sbuf += n) {
		n = i < sizeof(buf) ? i : sizeof(buf);
		if ((ret = kvm_read(kd, sbuf, buf, n)) != n) {
			(void)fprintf(stderr,
			    "kgmon: read kmem: read %d, got %d: %s\n",
			    n, ret, kvm_geterr(kd));
			exit(4);
		}
		if ((ret = write(fd, buf, n)) != n) {
			(void)fprintf(stderr,
			    "kgmon: write gmon.out: wrote %d, got %d: %s\n",
			    n, ret, strerror(errno));
		}
	}
	s_textsize = kfetch(N_S_TEXTSIZE);
	fromssize = s_textsize / HASHFRACTION;
	froms = (u_short *)malloc(fromssize);
	i = kvm_read(kd, kfetch(N_FROMS), (char *)froms, fromssize);
	if (i != fromssize) {
		(void)fprintf(stderr, "kgmon: read kmem: read %u, got %d: %s",
		    fromssize, i, strerror(errno));
		exit(5);
	}
	tossize = (s_textsize * ARCDENSITY / 100) * sizeof(struct tostruct);
	tos = (struct tostruct *)malloc(tossize);
	i = kvm_read(kd, kfetch(N_TOS), (char *)tos, tossize);
	if (i != tossize) {
		(void)fprintf(stderr, "kgmon: read kmem: read %u, got %d: %s",
		    tossize, i, kvm_geterr(kd));
		exit(6);
	}
	s_lowpc = (char *)kfetch(N_S_LOWPC);
	if (debug)
		(void)fprintf(stderr, "s_lowpc 0x%x, s_textsize 0x%x\n",
		    s_lowpc, s_textsize);
	endfrom = fromssize / sizeof(*froms);
	for (fromindex = 0; fromindex < endfrom; fromindex++) {
		if (froms[fromindex] == 0)
			continue;
		frompc = (u_long)s_lowpc +
		    (fromindex * HASHFRACTION * sizeof(*froms));
		for (toindex = froms[fromindex]; toindex != 0;
		   toindex = tos[toindex].link) {
			if (debug)
			    (void)fprintf(stderr,
			    "[mcleanup] frompc 0x%x selfpc 0x%x count %d\n" ,
			    frompc, tos[toindex].selfpc, tos[toindex].count);
			rawarc.raw_frompc = frompc;
			rawarc.raw_selfpc = (u_long)tos[toindex].selfpc;
			rawarc.raw_count = tos[toindex].count;
			write(fd, (char *)&rawarc, sizeof (rawarc));
		}
	}
	close(fd);
}

int
zeroit(addr, len, zbuf, zsize)
	off_t addr;
	int len;
	char *zbuf;
	int zsize;
{
	register int n;

	while (len > 0) {
		n = len < zsize ? len : zsize;
		if (kvm_write(kd, addr, zbuf, n) != n)
			return (-1);
		addr += n;
		len -= n;
	}
	return (0);
}

resetstate()
{
	off_t sbuf, ktos;
	int ssiz, fromssize, tossize;
	u_long s_textsize;
	char zbuf[MAXBSIZE];

	turnonoff(PROFILING_OFF);
	bzero(zbuf, sizeof(zbuf));
	ssiz = kfetch(N_SSIZ) - sizeof(struct phdr);
	sbuf = kfetch(N_SBUF) + sizeof(struct phdr);
	if (zeroit(sbuf, ssiz, zbuf, sizeof(zbuf))) {
		(void)fprintf(stderr, "kgmon: sbuf write: %s\n",
		    kvm_geterr(kd));
		exit(7);
	}
	s_textsize = kfetch(N_S_TEXTSIZE);
	fromssize = s_textsize / HASHFRACTION;
	if (zeroit((off_t)kfetch(N_FROMS), fromssize, zbuf, sizeof(zbuf))) {
		(void)fprintf(stderr, "kgmon: kfroms write: %s\n",
		    kvm_geterr(kd));
		exit(8);
	}
	tossize = (s_textsize * ARCDENSITY / 100) * sizeof(struct tostruct);
	ktos = kfetch(N_TOS);
	if (zeroit((off_t)kfetch(N_TOS), tossize, zbuf, sizeof(zbuf))) {
		(void)fprintf(stderr, "kgmon: ktos write: %s\n",
		    kvm_geterr(kd));
		exit(9);
	}
}

turnonoff(onoff)
	int onoff;
{

	if (kvm_write(kd, (off_t)nl[N_PROFILING].n_value,
	    (char *)&onoff, sizeof(onoff)) != sizeof(onoff))
		(void)fprintf(stderr,
		    "kgmon: warning: can't turn profiling %s\n",
		    onoff ? "off" : "on");
}

kfetch(index)
	int index;
{
	off_t off;
	int value;

	if ((off = nl[index].n_value) == 0) {
		(void)fprintf(stderr, "kgmon: %s: not defined in kernel\n",
		    nl[index].n_name);
		exit(11);
	}
	if (kvm_read(kd, off, (char *)&value, sizeof(value)) != sizeof(value)) {
		(void)fprintf(stderr, "kgmon: kvm_read(%s): %s\n",
		    nl[index].n_name, kvm_geterr(kd));
		exit(13);
	}
	return (value);
}
