/*-
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)gmon.c	5.5 (Berkeley) 5/22/91";
#endif /* not lint */

#include <unistd.h>
#include "gmon.h"

extern mcount() asm ("mcount");
extern char *minbrk asm ("minbrk");

    /*
     *	froms is actually a bunch of unsigned shorts indexing tos
     */
static int		profiling = 3;
static unsigned short	*froms;
static struct tostruct	*tos = 0;
static long		tolimit = 0;
static char		*s_lowpc = 0;
static char		*s_highpc = 0;
static unsigned long	s_textsize = 0;

static int	ssiz;
static char	*sbuf;
static int	s_scale;
    /* see profil(2) where this is describe (incorrectly) */
#define		SCALE_1_TO_1	0x10000L

char nospace_msg[] = "No space for profiling buffer(s)\n";
#define NOSPACE() (void) write(2, nospace_msg, sizeof nospace_msg - 1)

char overflow_msg[] = "mcount: tos overflow\n";
#define OVERFLOW() (void) write(2, overflow_msg, sizeof overflow_msg - 1)

monstartup(lowpc, highpc)
	char	*lowpc;
	char	*highpc;
{
	int	monsize;
	char	*buffer;
	int	o;
	struct	phdr *p;

	/*
	 * Round lowpc and highpc to multiples of the density we're using
	 * so the rest of the scaling (here and in gprof) stays in ints.
	 */
	lowpc = (char *)
	    ROUNDDOWN((unsigned)lowpc, HISTFRACTION*sizeof(HISTCOUNTER));
	s_lowpc = lowpc;
	highpc = (char *)
	    ROUNDUP((unsigned)highpc, HISTFRACTION*sizeof(HISTCOUNTER));
	s_highpc = highpc;
	s_textsize = highpc - lowpc;
	monsize = (s_textsize / HISTFRACTION) + sizeof(struct phdr);
	monsize = (monsize + 3) & ~3;
	buffer = sbrk(monsize);
	if (buffer == (char *) -1) {
		NOSPACE();
		return;
	}
	froms = (unsigned short *) sbrk(s_textsize / HASHFRACTION);
	if (froms == (unsigned short *) -1) {
		NOSPACE();
		froms = 0;
		return;
	}
	tolimit = s_textsize * ARCDENSITY / 100;
	if (tolimit < MINARCS)
		tolimit = MINARCS;
	else if (tolimit > 65534)
		tolimit = 65534;
	tos = (struct tostruct *) sbrk((tolimit*sizeof(struct tostruct)+3)&~3);
	if (tos == (struct tostruct *) -1) {
		NOSPACE();
		froms = 0;
		tos = 0;
		return;
	}
	minbrk = sbrk(0);
	tos[0].link = 0;
	sbuf = buffer;
	ssiz = monsize;
	p = (struct phdr *) buffer;
	p->lpc = lowpc;
	p->hpc = highpc;
	p->ncnt = ssiz;
	monsize -= sizeof(struct phdr);
	if (monsize <= 0)
		return;
	o = highpc - lowpc;
	if(monsize < o)
		s_scale = ((float) monsize / o) * SCALE_1_TO_1;
	else
		s_scale = SCALE_1_TO_1;
	moncontrol(1);
}

_mcleanup()
{
	int		fd;
	int		fromindex;
	int		endfrom;
	char		*frompc;
	int		toindex;
	struct rawarc	rawarc;

	moncontrol(0);
	fd = creat("gmon.out", 0666);
	if (fd < 0) {
		perror("mcount: gmon.out");
		return;
	}
	(void) write(fd, sbuf, ssiz);
	endfrom = s_textsize / (HASHFRACTION * sizeof(*froms));
	for (fromindex = 0; fromindex < endfrom; fromindex++) {
		if (froms[fromindex] == 0)
			continue;
		frompc = s_lowpc + (fromindex * HASHFRACTION * sizeof(*froms));
		for (toindex = froms[fromindex];
		     toindex != 0;
		     toindex = tos[toindex].link) {
			rawarc.raw_frompc = (unsigned long) frompc;
			rawarc.raw_selfpc = (unsigned long) tos[toindex].selfpc;
			rawarc.raw_count = tos[toindex].count;
			(void) write(fd, &rawarc, sizeof rawarc);
		}
	}
	close(fd);
}

mcount(cntpa)
	long **cntpa;
{
	register char			*selfpc;
	register unsigned short		*frompcindex;
	register struct tostruct	*top;
	register struct tostruct	*prevtop;
	register long			toindex;

	/*
	 * Find the return address for mcount,
	 * and address of counter pointer.
	 */
	selfpc = *((char **)&cntpa-3);	/* -8(fp) */
	frompcindex = (unsigned short *)(*((((long *)*((char **)&cntpa-1)))-2));

	/*
	 * Check that we are profiling
	 * and that we aren't recursively invoked.
	 */
	if (profiling)
		return;
	profiling++;

	/*
	 * Check that frompcindex is a reasonable pc value.
	 * for example:	signal catchers get called from the stack,
	 *		not from text space.  too bad.
	 */
	frompcindex = (unsigned short *)((long)frompcindex - (long)s_lowpc);
	if ((unsigned long)frompcindex > s_textsize)
		goto done;
	frompcindex =
	    &froms[((long)frompcindex) / (HASHFRACTION * sizeof(*froms))];

	toindex = *frompcindex;
	if (toindex == 0) {
		/*
		 * First time traversing this arc
		 */
		toindex = ++tos[0].link;
		if (toindex >= tolimit) {
			profiling++;	/* stop profiling */
			OVERFLOW();
			return;
		}
		*frompcindex = toindex;
		top = &tos[toindex];
		top->selfpc = selfpc;
		top->count = 1;
		top->link = 0;
		goto done;
	}
	top = &tos[toindex];
	if (top->selfpc == selfpc) {
		/*
		 * Arc at front of chain; usual case.
		 */
		top->count++;
		goto done;
	}

	/*
	 * Have to go looking down chain for it.
	 * top points to what we are looking at,
	 * prevtop points to previous top.
	 * We know it is not at the head of the chain.
	 */
	for (;;) {
		if (top->link == 0) {
			/*
			 * top is end of the chain and none of the chain
			 * had top->selfpc == selfpc,
			 * so we allocate a new tostruct
			 * and link it to the head of the chain.
			 */
			toindex = ++tos[0].link;
			if (toindex >= tolimit) {
				profiling++;	/* stop profiling */
				OVERFLOW();
				return;
			}
			top = &tos[toindex];
			top->selfpc = selfpc;
			top->count = 1;
			top->link = *frompcindex;
			*frompcindex = toindex;
			goto done;
		}
		/*
		 * Otherwise, check the next arc on the chain.
		 */
		prevtop = top;
		top = &tos[top->link];
		if (top->selfpc == selfpc) {
			/*
			 * There it is.
			 * Increment its count and
			 * move it to the head of the chain.
			 */
			top->count++;
			toindex = prevtop->link;
			prevtop->link = top->link;
			top->link = *frompcindex;
			*frompcindex = toindex;
			goto done;
		}

	}
done:
	profiling--;
	return;
}

/*
 * While profiling is enabled, we set the variable 'profiling' to indicate
 * that all the data structures are ready.
 */
moncontrol(mode)
	int mode;
{
	if (mode) {
		/* start */
		profil(sbuf + sizeof(struct phdr), ssiz - sizeof(struct phdr),
			(int)s_lowpc, s_scale);
		profiling = 0;
	} else {
		/* stop */
		profil((char *)0, 0, 0, 0);
		profiling = 3;
	}
}
