/*-
 * Copyright (c) 1983, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if !defined(lint) && defined(LIBC_SCCS)
static char sccsid[] = "@(#)gmon.c	5.14 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/time.h>
#include <sys/gmon.h>
#include <sys/sysctl.h>

#ifdef DEBUG
#include <stdio.h>
#include <fcntl.h>
#endif

#include <unistd.h>

extern char *minbrk asm ("minbrk");

struct gmonparam _gmonparam = { GMON_PROF_OFF };

static int	ssiz;
static char	*sbuf;
static int	s_scale;
/* see profil(2) where this is describe (incorrectly) */
#define		SCALE_1_TO_1	0x10000L

#define ERR(s) write(2, s, sizeof(s))

static struct gmonhdr gmonhdr;

monstartup(lowpc, highpc)
	u_long lowpc;
	u_long highpc;
{
	register int o;
	struct clockinfo clockinfo;
	int mib[2], tsize, fsize, size;
	char *cp;
	struct gmonhdr *hdr;
	struct gmonparam *p = &_gmonparam;

	/*
	 * round lowpc and highpc to multiples of the density we're using
	 * so the rest of the scaling (here and in gprof) stays in ints.
	 */
	lowpc = ROUNDDOWN(lowpc, HISTFRACTION * sizeof(HISTCOUNTER));
	p->lowpc = lowpc;
	highpc = ROUNDUP(highpc, HISTFRACTION * sizeof(HISTCOUNTER));
	p->highpc = highpc;
	p->textsize = highpc - lowpc;
	ssiz = p->textsize / HISTFRACTION;
	fsize = p->textsize / HASHFRACTION;
	tsize = p->textsize * ARCDENSITY / 100;
	if (tsize < MINARCS)
		tsize = MINARCS;
	else if (tsize > MAXARCS)
		tsize = MAXARCS;
	p->tolimit = tsize;
	tsize *= sizeof(struct tostruct);

	cp = sbrk(ssiz + fsize + tsize);
	if (cp == (char *)-1) {
		ERR("monstartup: out of memory\n");
		return;
	}
#ifdef notdef
	bzero(cp, ssiz + fsize + tsize);
#endif
	p->tos = (struct tostruct *)cp;
	cp += tsize;
	sbuf = cp;
	cp += ssiz;
	p->froms = (u_short *)cp;

	minbrk = sbrk(0);
	p->tos[0].link = 0;

	o = highpc - lowpc;
	if (ssiz < o) {
#ifndef hp300
		s_scale = ((float)ssiz / o ) * SCALE_1_TO_1;
#else /* avoid floating point */
		int quot = o / ssiz;
		
		if (quot >= 0x10000)
			s_scale = 1;
		else if (quot >= 0x100)
			s_scale = 0x10000 / quot;
		else if (o >= 0x800000)
			s_scale = 0x1000000 / (o / (ssiz >> 8));
		else
			s_scale = 0x1000000 / ((o << 8) / ssiz);
#endif
	} else
		s_scale = SCALE_1_TO_1;

	moncontrol(1);
	size = sizeof(clockinfo);
	mib[0] = CTL_KERN;
	mib[1] = KERN_CLOCKRATE;
	if (sysctl(mib, 2, &clockinfo, &size, NULL, 0) < 0)
		/*
		 * Best guess
		 */
		clockinfo.profhz = hertz();
	else if (clockinfo.profhz == 0) {
		if (clockinfo.hz != 0)
			clockinfo.profhz = clockinfo.hz;
		else
			clockinfo.profhz = hertz();
	}
	hdr = (struct gmonhdr *)&gmonhdr;
	hdr->lpc = lowpc;
	hdr->hpc = highpc;
	hdr->ncnt = ssiz + sizeof(gmonhdr);
	hdr->version = GMONVERSION;
	hdr->profrate = clockinfo.profhz;
}

_mcleanup()
{
	int fd;
	int fromindex;
	int endfrom;
	u_long frompc;
	int toindex;
	struct rawarc rawarc;
	struct gmonparam *p = &_gmonparam;
	int log, len;
	char buf[200];

	if (p->state == GMON_PROF_ERROR)
		ERR("_mcleanup: tos overflow\n");

	moncontrol(0);
	fd = creat("gmon.out", 0666);
	if (fd < 0) {
		perror("mcount: gmon.out");
		return;
	}
#ifdef DEBUG
	log = open("gmon.log", O_CREAT|O_TRUNC|O_WRONLY, 0664);
	if (log < 0) {
		perror("mcount: gmon.log");
		return;
	}
	len = sprintf(buf, "[mcleanup1] sbuf 0x%x ssiz %d\n", sbuf, ssiz);
	write(log, buf, len);
#endif
	write(fd, (char *)&gmonhdr, sizeof(gmonhdr));
	write(fd, sbuf, ssiz);
	endfrom = p->textsize / (HASHFRACTION * sizeof(*p->froms));
	for (fromindex = 0; fromindex < endfrom; fromindex++) {
		if (p->froms[fromindex] == 0)
			continue;

		frompc = p->lowpc;
		frompc += fromindex * HASHFRACTION * sizeof(*p->froms);
		for (toindex = p->froms[fromindex]; toindex != 0;
		     toindex = p->tos[toindex].link) {
#ifdef DEBUG
			len = sprintf(buf,
			"[mcleanup2] frompc 0x%x selfpc 0x%x count %d\n" ,
				frompc, p->tos[toindex].selfpc,
				p->tos[toindex].count);
			write(log, buf, len);
#endif
			rawarc.raw_frompc = frompc;
			rawarc.raw_selfpc = p->tos[toindex].selfpc;
			rawarc.raw_count = p->tos[toindex].count;
			write(fd, &rawarc, sizeof rawarc);
		}
	}
	close(fd);
}

/*
 * Control profiling
 *	profiling is what mcount checks to see if
 *	all the data structures are ready.
 */
moncontrol(mode)
	int mode;
{
	struct gmonparam *p = &_gmonparam;

	if (mode) {
		/* start */
		profil(sbuf, ssiz, (int)p->lowpc, s_scale);
		p->state = GMON_PROF_ON;
	} else {
		/* stop */
		profil((char *)0, 0, 0, 0);
		p->state = GMON_PROF_OFF;
	}
}

/*
 * discover the tick frequency of the machine
 * if something goes wrong, we return 0, an impossible hertz.
 */
hertz()
{
	struct itimerval tim;
	
	tim.it_interval.tv_sec = 0;
	tim.it_interval.tv_usec = 1;
	tim.it_value.tv_sec = 0;
	tim.it_value.tv_usec = 0;
	setitimer(ITIMER_REAL, &tim, 0);
	setitimer(ITIMER_REAL, 0, &tim);
	if (tim.it_interval.tv_usec < 2)
		return(0);
	return (1000000 / tim.it_interval.tv_usec);
}


