static	char *sccsid = "@(#)gmon.c	1.3 (Berkeley) %G%";

#include <stdio.h>

#include "gmcrt0.h"

    /*
     *	C start up routines, for monitoring
     *	Robert Henry, UCB, 20 Oct 81
     *
     *	We make the following (true) assumptions:
     *	1) when the kernel calls start, it does a jump to location 2,
     *	and thus avoids the register save mask.  We are NOT called
     *	with a calls!  see sys1.c:setregs().
     *	2) The only register variable that we can trust is sp,
     *	which points to the base of the kernel calling frame.
     *	Do NOT believe the documentation in exec(2) regarding the
     *	values of fp and ap.
     *	3) We can allocate as many register variables as we want,
     *	and don't have to save them for anybody.
     *	4) Because of the ways that asm's work, we can't have
     *	any automatic variables allocated on the stack, because
     *	we must catch the value of sp before any automatics are
     *	allocated.
     */

char **environ;
extern	unsigned char	etext;
asm( "#define _eprol eprol" );
extern	unsigned char	eprol;

asm( "#define _start start" );
start()
{
    struct kframe {
	int	kargc;
	char	*kargv[1];		/* size depends on kargc */
	char	kargstr[1];		/* size varies */
	char	kenvstr[1];		/* size varies */
    };
	/*
	 *	ALL REGISTER VARIABLES!!!
	 */
    register struct kframe	*kfp;		/* r11 */
    register char		**targv;
    register char		**argv;

#ifdef lint
    kfp = 0;
#else not lint
    asm( "	movl	sp,r11" );		/* catch it quick */
#endif not lint
    for ( argv = targv = &kfp -> kargv[0] ; *targv++ ; /* void */ )
	/* VOID */ ;
    if ( targv >= (char **) ( *argv ) )
	--targv;
    environ = targv;
asm("eprol:");
    _mstartup( &eprol , &etext );
    exit( main( kfp -> kargc , argv , environ ) );
}
asm( "#undef _start" );
asm( "#undef _eprol" );

exit( code )
	/* ARGSUSED */
    register int	code;	/* r11 */
{

    fflush( stdout );
    _mcleanup();
    _cleanup();
    asm( "	movl r11, r0" );
    asm( "	chmk $1" );
}

    /*
     *	froms is actually a bunch of unsigned shorts indexing tos
     */
static unsigned short	*froms;
static struct tostruct	*tos = 0;
static unsigned short	tolimit = 0;
static char		*s_lowpc = 0;
static char		*s_highpc = 0;

static int	ssiz;
static int	*sbuf;

#define	MSG "No space for monitor buffer(s)\n"

_mstartup(lowpc, highpc)
    char	*lowpc;
    char	*highpc;
{
    int		monsize;
    char	*buffer;
    int		textsize;
    char	*sbrk();

    s_lowpc = lowpc;
    s_highpc = highpc;
    textsize = ( (char *) highpc - (char *) lowpc );
    monsize = textsize + sizeof(struct phdr);
    buffer = sbrk( monsize );
    if ( buffer == (char *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	return;
    }
    froms = (unsigned short *) sbrk( textsize );
    if ( froms == (unsigned short *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	return;
    }
    tos = (struct tostruct *) sbrk(textsize);
    if ( tos == (struct tostruct *) -1 ) {
	write( 2 , MSG , sizeof(MSG) );
	froms = 0;
	tos = 0;
	return;
    }
    tolimit = textsize / sizeof(struct tostruct);
    tos[0].link = 0;
    monitor( lowpc , highpc , buffer , monsize );
}

_mcleanup()
{
    FILE	*fd;
    int		fromindex;
    char	*frompc;
    int		toindex;
    int		textsize;

    monitor( (int (*)()) 0 );
    fd = fopen( "gmon.out" , "w" );
    if ( fd == NULL ) {
	perror( "mcount: gmon.out" );
	return;
    }
#   ifdef DEBUG
	fprintf( stderr , "[mcleanup] sbuf 0x%x ssiz %d\n" , sbuf , ssiz );
#   endif DEBUG
    fwrite( sbuf , 1 , ssiz , fd );
    textsize = s_highpc - s_lowpc;
    for ( fromindex = 0 ; fromindex < textsize>>1 ; fromindex++ ) {
	if ( froms[fromindex] == 0 ) {
	    continue;
	}
	frompc = s_lowpc + (fromindex<<1);
	for (toindex=froms[fromindex]; toindex!=0; toindex=tos[toindex].link) {
#	    ifdef DEBUG
		fprintf( stderr , "[mcleanup] frompc %d selfpc %d count %d\n" ,
			frompc , tos[toindex].selfpc , tos[toindex].count );
#	    endif DEBUG
	    fwrite( &frompc, 1, sizeof frompc, fd );
	    fwrite( &tos[toindex].selfpc, 1, sizeof tos[toindex].selfpc, fd );
	    fwrite( &tos[toindex].count, 1, sizeof tos[toindex].count, fd );
	}
    }
    fclose( fd );
}

    /*
     *	This routine is massaged so that it may be jsb'ed to
     */
asm("#define _mcount mcount");
mcount()
{
    register char		*selfpc;	/* r11 */
    register unsigned short	*frompcindex;	/* r10 */
    register struct tostruct	*top;		/* r9 */
    static int			profiling = 0;

    asm( "	forgot to run ex script on gmcrt0.s" );
    asm( "#define r11 r5" );
    asm( "#define r10 r4" );
    asm( "#define r9 r3" );
#ifdef lint
    selfpc = (char *) 0;
    frompcindex = 0;
#else not lint
	/*
	 *	find the return address for mcount,
	 *	and the return address for mcount's caller.
	 */
    asm("	movl (sp), r11");	/* selfpc = ... (jsb frame) */
    asm("	movl 16(fp), r10");	/* frompcindex =     (calls frame) */
#endif not lint
	/*
	 *	check that we are profiling
	 *	and that we aren't recursively invoked.
	 */
    if ( tos == 0 ) {
	goto out;
    }
    if ( profiling ) {
	goto out;
    }
    profiling = 1;
	/*
	 *	check that frompcindex is a reasonable pc value.
	 *	for example:	signal catchers get called from the stack,
	 *			not from text space.  too bad.
	 */
    if ( (char *) frompcindex < s_lowpc || (char *) frompcindex > s_highpc ) {
	goto done;
    }
    frompcindex = &froms[ ( (long) frompcindex - (long) s_lowpc ) >> 1 ];
    if ( *frompcindex == 0 ) {
	*frompcindex = ++tos[0].link;
	if ( *frompcindex >= tolimit ) {
	    goto overflow;
	}
	top = &tos[ *frompcindex ];
	top->selfpc = selfpc;
	top->count = 0;
	top->link = 0;
    } else {
	top = &tos[ *frompcindex ];
    }
    for ( ; /* goto done */ ; top = &tos[ top -> link ] ) {
	if ( top -> selfpc == selfpc ) {
	    top -> count++;
	    goto done;
	}
	if ( top -> link == 0 ) {
	    top -> link = ++tos[0].link;
	    if ( top -> link >= tolimit )
		goto overflow;
	    top = &tos[ top -> link ];
	    top -> selfpc = selfpc;
	    top -> count = 1;
	    top -> link = 0;
	    goto done;
	}
    }
done:
    profiling = 0;
    /* and fall through */
out:
    asm( "	rsb" );
    asm( "#undef r11" );
    asm( "#undef r10" );
    asm( "#undef r9" );
    asm( "#undef _mcount");

overflow:
#   define	TOLIMIT	"mcount: tos overflow\n"
    write( 2 , TOLIMIT , sizeof( TOLIMIT ) );
    tos = 0;
    froms = 0;
    goto out;
}

monitor( lowpc , highpc , buf , bufsiz )
    char	*lowpc;
    /* VARARGS1 */
    char	*highpc;
    int		*buf, bufsiz;
{
    register o;

    if ( lowpc == 0 ) {
	profil( (char *) 0 , 0 , 0 , 0 );
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
    o = ( ( (char *) highpc - (char *) lowpc) >> 1 );
    if( bufsiz < o )
	o = ( (float) bufsiz / o ) * 32768;
    else
	o = 0177777;
    profil( buf , bufsiz , lowpc , o );
}
