/* Copyright (c) 1982 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)kgmon.c	4.1 82/06/21";
#endif

#include <sys/param.h>
#include <sys/pte.h>
#include <stdio.h>
#include <nlist.h>
#include <ctype.h>
#include <sys/crt0.h>

#define	PROFILING_ON	0
#define PROFILING_OFF	3

/*
 * froms is actually a bunch of unsigned shorts indexing tos
 */
u_short	*froms;
struct	tostruct *tos;
char	*s_lowpc;
u_long	s_textsize;
int	ssiz;
off_t	sbuf;

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

char	*system = "/vmunix";
char	*kmemf = "/dev/kmem";
int	kmem;
int	kflg;

main(argc, argv)
	int argc;
	char *argv[];
{
	int i, j, k;
	char *cp;
	long conptr;
	int fd;
	int fromindex;
	u_long frompc;
	int toindex;
	struct rawarc rawarc;
	off_t kfroms, ktos;
	char buf[BUFSIZ];
	int debug = 0;

	argc--, argv++;
	if (argc > 0) {
		system = *argv;
		argv++, argc--;
	}
	nlist(system, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: no namelist\n", system);
		exit(1);
	}
	if (argc > 0) {
		kmemf = *argv;
		kflg++;
	}
	kmem = open(kmemf, 2);
	if (kmem < 0) {
		kmem = open(kmemf, 0);
		if (kmem < 0) {
			fprintf(stderr, "cannot open ");
			perror(kmemf);
			exit(1);
		}
		fprintf(stderr,
		    "%s opened read-only, data may be inconsistent\n",
		    kmemf);
	}
	if (kflg) {
		off_t off;

		off = nl[N_SYSMAP].n_value & 0x7fffffff;
		lseek(kmem, off, 0);
		nl[N_SYSSIZE].n_value *= 4;
		Sysmap = (struct pte *)malloc(nl[N_SYSSIZE].n_value);
		if (Sysmap == 0) {
			perror("Sysmap");
			exit(1);
		}
		read(kmem, Sysmap, nl[N_SYSSIZE].n_value);
	}
	turnonoff(PROFILING_OFF);
	fd = creat("gmon.out", 0666);
	if (fd < 0) {
		perror("gmon.out");
		return;
	}
	ssiz = kfetch(N_SSIZ);
	sbuf = kfetch(N_SBUF);
	klseek(kmem, (off_t)sbuf, 0);
	for (i = ssiz; i > 0; i -= BUFSIZ) {
		read(kmem, buf, i < BUFSIZ ? i : BUFSIZ);
		write(fd, buf, i < BUFSIZ ? i : BUFSIZ);
	}
	s_textsize = kfetch(N_S_TEXTSIZE);
	froms = (u_short *)malloc(s_textsize);
	kfroms = kfetch(N_FROMS);
	klseek(kmem, kfroms, 0);
	for (i = 0; i < s_textsize; i += BUFSIZ) {
		j = s_textsize - i;
		if (j > BUFSIZ)
			j = BUFSIZ;
		k = read(kmem, ((char *)(froms)) + i, j);
		if (j != k) {
			fprintf(stderr, "read tos: loc %d, request %d, got %d",
			    i, j, k);
			perror("");
			exit(5);
		}
	}
	tos = (struct tostruct *)malloc(s_textsize);
	ktos = kfetch(N_TOS);
	klseek(kmem, ktos, 0);
	for (i = 0; i < s_textsize; i += BUFSIZ) {
		j = s_textsize - i;
		if (j > BUFSIZ)
			j = BUFSIZ;
		k = read(kmem, ((char *)(tos)) + i, j);
		if (j != k) {
			fprintf(stderr, "read tos: loc %d, request %d, got %d",
			    i, j, k);
			perror("");
			exit(6);
		}
	}
	s_lowpc = (char *)kfetch(N_S_LOWPC);
	if (debug)
		fprintf(stderr, "s_lowpc 0x%x, s_textsize 0x%x\n",
		    s_lowpc, s_textsize);
	for (fromindex = 0; fromindex < s_textsize>>1; fromindex++) {
		if (froms[fromindex] == 0)
			continue;
		frompc = (u_long)s_lowpc + (fromindex<<1);
		for (toindex = froms[fromindex]; toindex != 0;
		   toindex = tos[toindex].link) {
			if (debug)
			    fprintf(stderr,
			    "[mcleanup] frompc 0x%x selfpc 0x%x count %d\n" ,
			    frompc, tos[toindex].selfpc, tos[toindex].count);
			rawarc.raw_frompc = frompc;
			rawarc.raw_selfpc = (u_long)tos[toindex].selfpc;
			rawarc.raw_count = tos[toindex].count;
			write(fd, &rawarc, sizeof (rawarc));
		}
	}
	close(fd);
	turnonoff(PROFILING_ON);
}

turnonoff(onoff)
	int onoff;
{
	off_t off;

	if ((off = nl[N_PROFILING].n_value) == 0) {
		printf("profiling: not defined in kernel\n");
		exit(1);
	}
	klseek(kmem, off, 0);
	write(kmem, (char *)&onoff, sizeof (onoff));
}

kfetch(index)
	int index;
{
	off_t off;
	int value;

	if ((off = nl[index].n_value) == 0) {
		printf("%s: not defined in kernel\n", nl[index].n_name);
		exit(1);
	}
	if (klseek(kmem, off, 0) == -1) {
		perror("lseek");
		exit(2);
	}
	if (read(kmem, (char *)&value, sizeof (value)) != sizeof (value)) {
		perror("read");
		exit(3);
	}
	return (value);
}

klseek(fd, base, off)
	int fd, base, off;
{

	if (kflg) {
		/* get kernel pte */
		base &= 0x7fffffff;
		base = Sysmap[base >> 9].pg_pfnum * 512 + (base & 0x1ff);
	}
	return (lseek(fd, base, off));
}
