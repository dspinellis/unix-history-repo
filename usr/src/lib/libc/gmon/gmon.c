static	char *sccsid = "@(#)gmon.c	4.8 (Berkeley) %G%";

#ifdef DEBUG
#include <stdio.h>
#endif DEBUG

#include "gmon.h"

    /*
     *	froms is actually a bunch of unsigned shorts indexing tos
     */
static unsigned short	*froms;
static struct tostruct	*tos = 0;
static long		tolimit = 0;
static char		*s_lowpc = 0;
static char		*s_highpc = 0;
static unsigned long	s_textsize = 0;
static char		*minsbrk = 0;

static int	ssiz;
static int	*sbuf;

#define	MSG "No space for monitor buffer(s)\n"

monstartup(lowpc, highpc)
    char	*lowpc;
    char	*highpc;
{
    int			monsize;
    char		*buffer;
    char		*sbrk();
    unsigned long	limit;

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
    froms = (unsigned short *) sbrk( s_textsize );
    if ( froms == (unsigned short *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	return;
    }
    limit = s_textsize * ARCDENSITY / 100;
    if ( limit < MINARCS ) {
	limit = MINARCS;
    } else if ( limit > 65534 ) {
	limit = 65534;
    }
    tos = (struct tostruct *) sbrk( limit * sizeof( struct tostruct ) );
    if ( tos == (struct tostruct *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	tos = 0;
	return;
    }
    minsbrk = sbrk(0);
    tos[0].link = 0;
	/*
	 *	tolimit is what mcount checks to see if
	 *	all the data structures are ready!!!
	 *	make sure it won't overflow.
	 */
    tolimit = limit;
    monitor( lowpc , highpc , buffer , monsize , tolimit );
}

_mcleanup()
{
    int			fd;
    int			fromindex;
    char		*frompc;
    int			toindex;
    struct rawarc	rawarc;

    fd = creat( "gmon.out" , 0666 );
    if ( fd < 0 ) {
	perror( "mcount: gmon.out" );
	return;
    }
#   ifdef DEBUG
	fprintf( stderr , "[mcleanup] sbuf 0x%x ssiz %d\n" , sbuf , ssiz );
#   endif DEBUG
    write( fd , sbuf , ssiz );
    for ( fromindex = 0 ; fromindex < s_textsize>>1 ; fromindex++ ) {
	if ( froms[fromindex] == 0 ) {
	    continue;
	}
	frompc = s_lowpc + (fromindex<<1);
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

asm(".text");
asm("#the beginning of mcount()");
asm(".data");
mcount()
{
	register char			*selfpc;	/* r11 => r5 */
	register unsigned short		*frompcindex;	/* r10 => r4 */
	register struct tostruct	*top;		/* r9  => r3 */
	register struct tostruct	*prevtop;	/* r8  => r2 */
	register long			toindex;	/* r7  => r1 */
	static int			profiling = 0;

#ifdef lint
	selfpc = (char *)0;
	frompcindex = 0;
#else not lint
	/*
	 *	find the return address for mcount,
	 *	and the return address for mcount's caller.
	 */
	asm("	.text");		/* make sure we're in text space */
	asm("	movl (sp), r11");	/* selfpc = ... (jsb frame) */
	asm("	movl 16(fp), r10");	/* frompcindex =     (calls frame) */
#endif not lint
	/*
	 *	check that we are profiling
	 *	and that we aren't recursively invoked.
	 */
	if (tolimit == 0) {
		goto out;
	}
	if (profiling) {
		goto out;
	}
	profiling = 1;
	/*
	 *	check that frompcindex is a reasonable pc value.
	 *	for example:	signal catchers get called from the stack,
	 *			not from text space.  too bad.
	 */
	frompcindex = (unsigned short *)((long)frompcindex - (long)s_lowpc);
	if ((unsigned long)frompcindex > s_textsize) {
		goto done;
	}
	frompcindex =
	    &froms[(((long)frompcindex) + sizeof(*froms) - 1) / sizeof(*froms)];
	toindex = *frompcindex;
	if (toindex == 0) {
		/*
		 *	first time traversing this arc
		 */
		toindex = ++tos[0].link;
		if (toindex >= tolimit) {
			goto overflow;
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
		 *	arc at front of chain; usual case.
		 */
		top->count++;
		goto done;
	}
	/*
	 *	have to go looking down chain for it.
	 *	top points to what we are looking at,
	 *	prevtop points to previous top.
	 *	we know it is not at the head of the chain.
	 */
	for (; /* goto done */; ) {
		if (top->link == 0) {
			/*
			 *	top is end of the chain and none of the chain
			 *	had top->selfpc == selfpc.
			 *	so we allocate a new tostruct
			 *	and link it to the head of the chain.
			 */
			toindex = ++tos[0].link;
			if (toindex >= tolimit) {
				goto overflow;
			}
			top = &tos[toindex];
			top->selfpc = selfpc;
			top->count = 1;
			top->link = *frompcindex;
			*frompcindex = toindex;
			goto done;
		}
		/*
		 *	otherwise, check the next arc on the chain.
		 */
		prevtop = top;
		top = &tos[top->link];
		if (top->selfpc == selfpc) {
			/*
			 *	there it is.
			 *	increment its count
			 *	move it to the head of the chain.
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
	profiling = 0;
	/* and fall through */
out:
	asm("	rsb");

overflow:
	tolimit = 0;
#   define	TOLIMIT	"mcount: tos overflow\n"
	write(2, TOLIMIT, sizeof(TOLIMIT));
	goto out;
}
asm(".text");
asm("#the end of mcount()");
asm(".data");

/*VARARGS1*/
monitor( lowpc , highpc , buf , bufsiz , nfunc )
    char	*lowpc;
    char	*highpc;
    int		*buf, bufsiz;
    int		nfunc;	/* not used, available for compatability only */
{
    register o;

    if ( lowpc == 0 ) {
	profil( (char *) 0 , 0 , 0 , 0 );
	_mcleanup();
	return;
    }
    sbuf = buf;
    ssiz = bufsiz;
    ( (struct phdr *) buf ) -> lpc = lowpc;
    ( (struct phdr *) buf ) -> hpc = highpc;
    ( (struct phdr *) buf ) -> ncnt = ssiz;
    o = sizeof(struct phdr);
    buf = (int *) ( ( (int) buf ) + o );
    bufsiz -= o;
    if ( bufsiz <= 0 )
	return;
    o = ( ( (char *) highpc - (char *) lowpc) );
    if( bufsiz < o )
	o = ( (float) bufsiz / o ) * 65536;
    else
	o = 65536;
    profil( buf , bufsiz , lowpc , o );
}

/*
 * This is a stub for the "brk" system call, which we want to
 * catch so that it will not deallocate our data space.
 * (of which the program is not aware)
 */
extern char *curbrk;

brk(addr)
	char *addr;
{

	if (addr < minsbrk)
		addr = minsbrk;
	asm("	chmk	$17");
	asm("	jcc	1f");
	asm("	jmp	cerror");
asm("1:");
	curbrk = addr;
	return (0);
}
