# define FORT
/* this forces larger trees, etc. */
# include "mfile2"
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

FILE * lrd;  /* for default reading routines */
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
	static char fbuf[128];
	if( n > 0 ){
		if( n > 32 ) cerror( "lccopy asked to copy too much" );
		if( fread( fbuf, 4, n, lrd ) != n ) cerror( "intermediate file read error" );
		for( i=4*n; fbuf[i-1] == '\0' && i>0; --i ) { /* VOID */ }
		if( i ) {
			if( fwrite( fbuf, 1, i, stdout ) != i ) cerror( "output file error" );
			}
		}
	}
# endif

/*	new opcode definitions */

# define FORTOPS 200
# define FTEXT 200
# define FEXPR 201
# define FSWITCH 202
# define FLBRAC 203
# define FRBRAC 204
# define FEOF 205
# define FARIF 206
# define LABEL 207

/*	stack for reading nodes in postfix form */

# define NSTACKSZ 250

NODE * fstack[NSTACKSZ];
NODE ** fsp;  /* points to next free position on the stack */
int ftlab1,ftlab2;

mainp2( argc, argv ) char *argv[]; {
	int files;
	register long x;
	register NODE *p;

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
			p->op = ICON;
			p->type = REST(x);
			p->rval = 0;
			p->lval = lread();
			if( VAL(x) ){
				lcread( p->name, 2 );
				}
			else p->name[0] = '\0';

		bump:
			p->su = 0;
			p->rall = NOPREF;
			*fsp++ = p;
			if( fsp >= &fstack[NSTACKSZ] ) uerror( "expression depth exceeded" );
			continue;

		case NAME:
			p = talloc();
			p->op = NAME;
			p->type = REST(x);
			p->rval = 0;
			if( VAL(x) ) p->lval = lread();
			else p->lval = 0;
			lcread( p->name, 2 );
			goto bump;

		case OREG:
			p = talloc();
			p->op = OREG;
			p->type = REST(x);
			p->rval = VAL(x);
			p->lval = lread();
			lcread( p->name, 2 );
			goto bump;

		case REG:
			p = talloc();
			p->op = REG;
			p->type = REST(x);
			p->rval = VAL(x);
			rbusy( p->rval, p->type );
			p->lval = 0;
			p->name[0] = '\0';
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

		case LABEL:
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

		default:
		def:
			p = talloc();
			p->op = FOP(x);
			p->type = REST(x);

			switch( optype( p->op ) ){

			case BITYPE:
				p->right = *--fsp;
				p->left = *--fsp;
				goto bump;

			case UTYPE:
				p->left = *--fsp;
				p->rval = 0;
				goto bump;

			case LTYPE:
				uerror( "illegal leaf node: %d", p->op );
				exit( 1 );
				}
			}
		}
	}
