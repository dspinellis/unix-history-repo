#ifndef lint
static char *sccsid ="@(#)fort.c	4.8 (Berkeley) %G%";
#endif lint

# ifndef FORT
# define FORT
/* this may force larger trees, etc. */
# endif

# include "pass2.h"
# include "fort.h"

/*	masks for unpacking longs */

# ifndef FOP
# define FOP(x) (int)((x)&0377)
# endif

# ifndef VAL
# define VAL(x) (int)(((x)>>8)&0377)
# endif

# ifndef REST
# define REST(x) (((x)>>16)&0177777)
# endif

# ifndef FIXINT
# if SZINT == SZLONG
# define FIXINT(x) ((x) == LONG || (x) == ULONG ? (x) - 1 : (x))
# else
# if SZINT == SZSHORT
# define FIXINT(x) ((x) == SHORT || (x) == USHORT ? (x) + 1 : (x))
# else
# define FIXINT(x) (x)
# endif
# endif
# endif

FILE * lrd;  /* for default reading routines */

# if !defined(NOLNREAD) && defined(FLEXNAMES)
char *
lnread()
{
	char buf[BUFSIZ];
	register char *cp = buf;
	register char *limit = &buf[BUFSIZ];

	for (;;) {
		if (fread(cp, sizeof (long), 1, lrd) !=  1)
			cerror("intermediate file read error");
		cp += sizeof (long);
		if (cp[-1] == 0)
			break;
		if (cp >= limit)
			cerror("lnread overran string buffer");
	}
	return (tstr(buf));
}
#endif

# ifndef NOLREAD
long lread(){
	static long x;
	if( fread( (char *) &x, 4, 1, lrd ) <= 0 ) cerror( "intermediate file read error" );
	return( x );
	}
# endif

# ifndef NOLOPEN
lopen( s ) char *s; {
	/* if null, opens the standard input */
	if( *s ){
		lrd = fopen( s, "r" );
		if( lrd == NULL ) cerror( "cannot open intermediate file %s", s );
		}
	else  lrd = stdin;
	}
# endif

# ifndef NOLCREAD
lcread( cp, n ) char *cp; {
	if( n > 0 ){
		if( fread( cp, 4, n, lrd ) != n ) cerror( "intermediate file read error" );
		}
	}
# endif

# ifndef NOLCCOPY
lccopy( n ) register n; {
	register i;
	static char fbuf[BUFSIZ];
	if( n > 0 ){
		if( n > BUFSIZ/4 ) cerror( "lccopy asked to copy too much" );
		if( fread( fbuf, 4, n, lrd ) != n ) cerror( "intermediate file read error" );
		for( i=4*n; fbuf[i-1] == '\0' && i>0; --i ) { /* VOID */ }
		if( i ) {
			if( fwrite( fbuf, 1, i, stdout ) != i ) cerror( "output file error" );
			}
		}
	}
# endif

/*	stack for reading nodes in postfix form */

# define NSTACKSZ 250

NODE * fstack[NSTACKSZ];
NODE ** fsp;  /* points to next free position on the stack */

unsigned int offsz;
unsigned int caloff();
mainp2( argc, argv ) char *argv[]; {
	int files;
	register long x;
	register NODE *p;

	offsz = caloff();
	files = p2init( argc, argv );
	tinit();

		
	if( files ){
		while( files < argc && argv[files][0] == '-' ) {
			++files;
			}
		if( files > argc ) return( nerrors );
		lopen( argv[files] );
		}
	else lopen( "" );

	fsp = fstack;

	for(;;){
		/* read nodes, and go to work... */
		x = lread();

	if( xdebug ) fprintf( stderr, "op=%d, val = %d, rest = 0%o\n", FOP(x), VAL(x), (int)REST(x) );
		switch( (int)FOP(x) ){  /* switch on opcode */

		case 0:
			fprintf( stderr, "null opcode ignored\n" );
			continue;
		case FTEXT:
			lccopy( VAL(x) );
			printf( "\n" );
			continue;

		case FLBRAC:
			tmpoff = baseoff = lread();
			maxtreg = VAL(x);
			if( ftnno != REST(x) ){
				/* beginning of function */
				maxoff = baseoff;
				ftnno = REST(x);
				maxtemp = 0;
				}
			else {
				if( baseoff > maxoff ) maxoff = baseoff;
				/* maxoff at end of ftn is max of autos and temps 
				   over all blocks in the function */
				}
			setregs();
			continue;

		case FRBRAC:
			SETOFF( maxoff, ALSTACK );
			eobl2();
			continue;

		case FEOF:
			return( nerrors );

		case FSWITCH:
			uerror( "switch not yet done" );
			for( x=VAL(x); x>0; --x ) lread();
			continue;

		case ICON:
			p = talloc();
			p->in.op = ICON;
			p->in.type = FIXINT(REST(x));
			p->tn.rval = 0;
			p->tn.lval = lread();
			if( VAL(x) ){
#ifndef FLEXNAMES
				lcread( p->in.name, 2 );
#else
				p->in.name = lnread();
#endif
				}
#ifndef FLEXNAMES
			else p->in.name[0] = '\0';
#else
			else p->in.name = "";
#endif

		bump:
			p->in.su = 0;
			p->in.rall = NOPREF;
			*fsp++ = p;
			if( fsp >= &fstack[NSTACKSZ] ) uerror( "expression depth exceeded" );
			continue;

		case NAME:
			p = talloc();
			p->in.op = NAME;
			p->in.type = FIXINT(REST(x));
			p->tn.rval = 0;
			if( VAL(x) ) p->tn.lval = lread();
			else p->tn.lval = 0;
#ifndef FLEXNAMES
			lcread( p->in.name, 2 );
#else
			p->in.name = lnread();
#endif
			goto bump;

		case OREG:
			p = talloc();
			p->in.op = OREG;
			p->in.type = FIXINT(REST(x));
			p->tn.rval = VAL(x);
			rbusy( p->tn.rval, PTR | p->in.type );
			p->tn.lval = lread();
#ifndef FLEXNAMES
			lcread( p->in.name, 2 );
#else
			p->in.name = lnread();
#endif
			goto bump;

		case REG:
			p = talloc();
			p->in.op = REG;
			p->in.type = FIXINT(REST(x));
			p->tn.rval = VAL(x);
			rbusy( p->tn.rval, p->in.type );
			p->tn.lval = 0;
#ifndef FLEXNAMES
			p->in.name[0] = '\0';
#else
			p->in.name = "";
#endif
			goto bump;

		case FEXPR:
			lineno = REST(x);
			if( VAL(x) ) lcread( filename, VAL(x) );
			if( fsp == fstack ) continue;  /* filename only */
			if( --fsp != fstack ) uerror( "expression poorly formed" );
			if( lflag ) lineid( lineno, filename );
			tmpoff = baseoff;
			p = fstack[0];
			if( edebug ) fwalk( p, eprint, 0 );
# ifdef MYREADER
			MYREADER(p);
# endif

			nrecur = 0;
			delay( p );
			reclaim( p, RNULL, 0 );

			allchk();
			tcheck();
			continue;

		case FLABEL:
			if( VAL(x) ){
				tlabel();
				}
			else {
				label( (int) REST(x) );
				}
			continue;

		case GOTO:
			if( VAL(x) ) {
				cbgen( 0, (int) REST(x), 'I' );  /* unconditional branch */
				continue;
				}
			/* otherwise, treat as unary */
			goto def;

		case STASG:
		case STARG:
		case STCALL:
		case UNARY STCALL:
			    /*
			     * size and alignment come from next long words
			     */
			p = talloc();
			p -> stn.stsize = lread();
			p -> stn.stalign = lread();
			goto defa;
		default:
		def:
			p = talloc();
		defa:
			p->in.op = FOP(x);
			p->in.type = FIXINT(REST(x));

			switch( optype( p->in.op ) ){

			case BITYPE:
				p->in.right = *--fsp;
				p->in.left = *--fsp;
				goto bump;

			case UTYPE:
				p->in.left = *--fsp;
				p->tn.rval = 0;
				goto bump;

			case LTYPE:
				uerror( "illegal leaf node: %d", p->in.op );
				exit( 1 );
				}
			}
		}
	}
