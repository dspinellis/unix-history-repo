/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)gmon.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/kinfo.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#include "gmon.h"

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

#define	MSG "No space for profiling buffer(s)\n"

monstartup(lowpc, highpc)
    char	*lowpc;
    char	*highpc;
{
    int			monsize;
    char		*buffer;
    register int	o;
    struct clockinfo	clockinfo;
    int			size;

	/*
	 *	round lowpc and highpc to multiples of the density we're using
	 *	so the rest of the scaling (here and in gprof) stays in ints.
	 */
    lowpc = (char *)
	    ROUNDDOWN((unsigned)lowpc, HISTFRACTION*sizeof(HISTCOUNTER));
    s_lowpc = lowpc;
    highpc = (char *)
	    ROUNDUP((unsigned)highpc, HISTFRACTION*sizeof(HISTCOUNTER));
    s_highpc = highpc;
    s_textsize = highpc - lowpc;
    monsize = (s_textsize / HISTFRACTION) + sizeof(struct phdr);
    buffer = sbrk( monsize );
    if ( buffer == (char *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	return;
    }
    froms = (unsigned short *) sbrk( s_textsize / HASHFRACTION );
    if ( froms == (unsigned short *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	return;
    }
    tolimit = s_textsize * ARCDENSITY / 100;
    if ( tolimit < MINARCS ) {
	tolimit = MINARCS;
    } else if ( tolimit > 65534 ) {
	tolimit = 65534;
    }
    tos = (struct tostruct *) sbrk( tolimit * sizeof( struct tostruct ) );
    if ( tos == (struct tostruct *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	tos = 0;
	return;
    }
    minbrk = sbrk(0);
    tos[0].link = 0;
    sbuf = buffer;
    ssiz = monsize;
    ( (struct phdr *) buffer ) -> lpc = lowpc;
    ( (struct phdr *) buffer ) -> hpc = highpc;
    ( (struct phdr *) buffer ) -> ncnt = ssiz;
    ( (struct phdr *) buffer ) -> version = GMONVERSION;
    monsize -= sizeof(struct phdr);
    if ( monsize <= 0 )
	return;
    o = highpc - lowpc;
    if( monsize < o )
#ifndef hp300
	s_scale = ( (float) monsize / o ) * SCALE_1_TO_1;
#else /* avoid floating point */
    {
	int quot = o / monsize;

	if (quot >= 0x10000)
		s_scale = 1;
	else if (quot >= 0x100)
		s_scale = 0x10000 / quot;
	else if (o >= 0x800000)
		s_scale = 0x1000000 / (o / (monsize >> 8));
	else
		s_scale = 0x1000000 / ((o << 8) / monsize);
    }
#endif
    else
	s_scale = SCALE_1_TO_1;
    moncontrol(1);
    size = sizeof( clockinfo );
    if ( getkerninfo( KINFO_CLOCKRATE , &clockinfo , &size , 0 ) < 0 ) {
	/*
	 * Best guess
	 */
	clockinfo.profhz = hertz();
    } else if ( clockinfo.profhz == 0 ) {
	if ( clockinfo.hz != 0 )
	    clockinfo.profhz = clockinfo.hz;
	else
	    clockinfo.profhz = hertz();
    }
    ( (struct phdr *) buffer ) -> profrate = clockinfo.profhz;
}

_mcleanup()
{
    int			fd;
    int			fromindex;
    int			endfrom;
    char		*frompc;
    int			toindex;
    struct rawarc	rawarc;

    moncontrol(0);
    fd = creat( "gmon.out" , 0666 );
    if ( fd < 0 ) {
	perror( "mcount: gmon.out" );
	return;
    }
#   ifdef DEBUG
	fprintf( stderr , "[mcleanup] sbuf 0x%x ssiz %d\n" , sbuf , ssiz );
#   endif DEBUG
    write( fd , sbuf , ssiz );
    endfrom = s_textsize / (HASHFRACTION * sizeof(*froms));
    for ( fromindex = 0 ; fromindex < endfrom ; fromindex++ ) {
	if ( froms[fromindex] == 0 ) {
	    continue;
	}
	frompc = s_lowpc + (fromindex * HASHFRACTION * sizeof(*froms));
	for (toindex=froms[fromindex]; toindex!=0; toindex=tos[toindex].link) {
#	    ifdef DEBUG
		fprintf( stderr ,
			"[mcleanup] frompc 0x%x selfpc 0x%x count %d\n" ,
			frompc , tos[toindex].selfpc , tos[toindex].count );
#	    endif DEBUG
	    rawarc.raw_frompc = (unsigned long) frompc;
	    rawarc.raw_selfpc = (unsigned long) tos[toindex].selfpc;
	    rawarc.raw_count = tos[toindex].count;
	    write( fd , &rawarc , sizeof rawarc );
	}
    }
    close( fd );
}

/*
 * Control profiling
 *	profiling is what mcount checks to see if
 *	all the data structures are ready.
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

    /*
     *	discover the tick frequency of the machine
     *	if something goes wrong, we return 0, an impossible hertz.
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
