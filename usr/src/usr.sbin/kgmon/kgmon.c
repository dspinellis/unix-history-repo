/*
 * Copyright (c) 1983 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kgmon.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <machine/pte.h>

#include <sys/param.h>
#include <sys/file.h>
#include <sys/vm.h>
#include <sys/gprof.h>
#include <stdio.h>
#include <nlist.h>
#include <ctype.h>
#include "pathnames.h"

#define	PROFILING_ON	0
#define	PROFILING_OFF	3

u_long	s_textsize;
off_t	sbuf, klseek(), lseek();
int	ssiz;

struct nlist nl[] = {
#define	N_SYSMAP	0
	{ "_Sysmap" },
#define	N_SYSSIZE	1
	{ "_Syssize" },
#define N_FROMS		2
	{ "_froms" },
#define	N_PROFILING	3
	{ "_profiling" },
#define	N_S_LOWPC	4
	{ "_s_lowpc" },
#define	N_S_TEXTSIZE	5
	{ "_s_textsize" },
#define	N_SBUF		6
	{ "_sbuf" },
#define N_SSIZ		7
	{ "_ssiz" },
#define	N_TOS		8
	{ "_tos" },
	0,
};

struct	pte *Sysmap;

int	kmem;
int	bflag, hflag, kflag, rflag, pflag;
int	debug = 0;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch, mode, disp, openmode;
	char *system, *kmemf, *malloc();

	while ((ch = getopt(argc, argv, "bhpr")) != EOF)
		switch((char)ch) {
		case 'b':
			bflag++;
			break;
		case 'h':
			hflag++;
			break;
		case 'p':
			pflag++;
			break;
		case 'r':
			rflag++;
			break;
		default:
			(void)fprintf(stderr,
			    "usage: kgmon [-bhrp] [system [core]]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	kmemf = _PATH_KMEM;
	if (*argv) {
		system = *argv;
		if (*++argv) {
			kmemf = *argv;
			++kflag;
		}
	}
	else
		system = _PATH_UNIX;

	if (nlist(system, nl) < 0 || nl[0].n_type == 0) {
		(void)fprintf(stderr, "kgmon: %s: no namelist\n", system);
		exit(2);
	}
	if (!nl[N_PROFILING].n_value) {
		(void)fprintf(stderr,
		    "kgmon: profiling: not defined in kernel.\n");
		exit(10);
	}

	openmode = (bflag || hflag || pflag || rflag) ? O_RDWR : O_RDONLY;
	kmem = open(kmemf, openmode);
	if (kmem < 0) {
		openmode = O_RDONLY;
		kmem = open(kmemf, openmode);
		if (kmem < 0) {
			perror(kmemf);
			exit(3);
		}
		(void)fprintf(stderr, "%s opened read-only\n", kmemf);
		if (rflag)
			(void)fprintf(stderr, "-r supressed\n");
		if (bflag)
			(void)fprintf(stderr, "-b supressed\n");
		if (hflag)
			(void)fprintf(stderr, "-h supressed\n");
		rflag = bflag = hflag = 0;
	}
	if (kflag) {
		off_t off;

		Sysmap = (struct pte *)
		   malloc((u_int)(nl[N_SYSSIZE].n_value * sizeof(struct pte)));
		if (!Sysmap) {
			(void)fprintf(stderr,
			    "kgmon: can't get memory for Sysmap.\n");
			exit(1);
		}
		off = nl[N_SYSMAP].n_value & ~KERNBASE;
		(void)lseek(kmem, off, L_SET);
		(void)read(kmem, (char *)Sysmap,
		    (int)(nl[N_SYSSIZE].n_value * sizeof(struct pte)));
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
	turnonoff(disp);
	(void)fprintf(stdout,
	    "kernel profiling is %s.\n", disp ? "off" : "running");
}

dumpstate()
{
	extern int errno;
	struct rawarc rawarc;
	struct tostruct *tos;
	u_long frompc;
	off_t kfroms, ktos;
	u_short *froms;		/* froms is a bunch of u_shorts indexing tos */
	int i, fd, fromindex, endfrom, fromssize, tossize, toindex;
	char buf[BUFSIZ], *s_lowpc, *malloc(), *strerror();

	turnonoff(PROFILING_OFF);
	fd = creat("gmon.out", 0666);
	if (fd < 0) {
		perror("gmon.out");
		return;
	}
	ssiz = kfetch(N_SSIZ);
	sbuf = kfetch(N_SBUF);
	(void)klseek(kmem, (off_t)sbuf, L_SET);
	for (i = ssiz; i > 0; i -= BUFSIZ) {
		read(kmem, buf, i < BUFSIZ ? i : BUFSIZ);
		write(fd, buf, i < BUFSIZ ? i : BUFSIZ);
	}
	s_textsize = kfetch(N_S_TEXTSIZE);
	fromssize = s_textsize / HASHFRACTION;
	froms = (u_short *)malloc((u_int)fromssize);
	kfroms = kfetch(N_FROMS);
	(void)klseek(kmem, kfroms, L_SET);
	i = read(kmem, ((char *)(froms)), fromssize);
	if (i != fromssize) {
		(void)fprintf(stderr, "read kmem: request %d, got %d: %s",
		    fromssize, i, strerror(errno));
		exit(5);
	}
	tossize = (s_textsize * ARCDENSITY / 100) * sizeof(struct tostruct);
	tos = (struct tostruct *)malloc((u_int)tossize);
	ktos = kfetch(N_TOS);
	(void)klseek(kmem, ktos, L_SET);
	i = read(kmem, ((char *)(tos)), tossize);
	if (i != tossize) {
		(void)fprintf(stderr, "read kmem: request %d, got %d: %s",
		    tossize, i, strerror(errno));
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

resetstate()
{
	off_t kfroms, ktos;
	int i, fromssize, tossize;
	char buf[BUFSIZ];

	turnonoff(PROFILING_OFF);
	bzero(buf, BUFSIZ);
	ssiz = kfetch(N_SSIZ);
	sbuf = kfetch(N_SBUF);
	ssiz -= sizeof(struct phdr);
	sbuf += sizeof(struct phdr);
	(void)klseek(kmem, (off_t)sbuf, L_SET);
	for (i = ssiz; i > 0; i -= BUFSIZ)
		if (write(kmem, buf, i < BUFSIZ ? i : BUFSIZ) < 0) {
			perror("sbuf write");
			exit(7);
		}
	s_textsize = kfetch(N_S_TEXTSIZE);
	fromssize = s_textsize / HASHFRACTION;
	kfroms = kfetch(N_FROMS);
	(void)klseek(kmem, kfroms, L_SET);
	for (i = fromssize; i > 0; i -= BUFSIZ)
		if (write(kmem, buf, i < BUFSIZ ? i : BUFSIZ) < 0) {
			perror("kforms write");
			exit(8);
		}
	tossize = (s_textsize * ARCDENSITY / 100) * sizeof(struct tostruct);
	ktos = kfetch(N_TOS);
	(void)klseek(kmem, ktos, L_SET);
	for (i = tossize; i > 0; i -= BUFSIZ)
		if (write(kmem, buf, i < BUFSIZ ? i : BUFSIZ) < 0) {
			perror("ktos write");
			exit(9);
		}
}

turnonoff(onoff)
	int onoff;
{
	(void)klseek(kmem, (long)nl[N_PROFILING].n_value, L_SET);
	(void)write(kmem, (char *)&onoff, sizeof (onoff));
}

kfetch(index)
	int index;
{
	off_t off;
	int value;

	if ((off = nl[index].n_value) == 0) {
		printf("%s: not defined in kernel\n", nl[index].n_name);
		exit(11);
	}
	if (klseek(kmem, off, L_SET) == -1) {
		perror("lseek");
		exit(12);
	}
	if (read(kmem, (char *)&value, sizeof (value)) != sizeof (value)) {
		perror("read");
		exit(13);
	}
	return (value);
}

off_t
klseek(fd, base, off)
	int fd, off;
	off_t base;
{
	if (kflag) {	/* get kernel pte */
		base &= ~KERNBASE;
		base = ctob(Sysmap[btop(base)].pg_pfnum) + (base & PGOFSET);
	}
	return (lseek(fd, base, off));
}
