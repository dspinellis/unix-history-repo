#ifndef lint
static char *sccsid ="@(#)reader.c	4.6 (Berkeley) %G%";
#endif lint

# include "pass2.h"

/*	some storage declarations */

# ifndef ONEPASS
NODE node[TREESZ];
char filename[100] = "";  /* the name of the file */
int ftnno;  /* number of current function */
int lineno;
# else
# define NOMAIN
#endif

int nrecur;
int lflag;
#ifdef FORT
int Oflag = 0;
#endif
extern int Wflag;
int edebug = 0;
int xdebug = 0;
int udebug = 0;
int vdebug = 0;

OFFSZ tmpoff;  /* offset for first temporary, in bits for current block */
OFFSZ maxoff;  /* maximum temporary offset over all blocks in current ftn, in bits */
int maxtreg;

NODE *stotree;
int stocook;

OFFSZ baseoff = 0;
OFFSZ maxtemp = 0;

p2init( argc, argv ) char *argv[];{
	/* set the values of the pass 2 arguments */

	register int c;
	register char *cp;
	register files;

	allo0();  /* free all regs */
	files = 0;

	for( c=1; c<argc; ++c ){
		if( *(cp=argv[c]) == '-' ){
			while( *++cp ){
				switch( *cp ){

				case 'X':  /* pass1 flags */
					while( *++cp ) { /* VOID */ }
					--cp;
					break;

				case 'l':  /* linenos */
					++lflag;
					break;

				case 'e':  /* expressions */
					++edebug;
					break;

				case 'o':  /* orders */
					++odebug;
					break;

				case 'r':  /* register allocation */
					++rdebug;
					break;

				case 'a':  /* rallo */
					++radebug;
					break;

				case 'v':
					++vdebug;
					break;

				case 't':  /* ttype calls */
					++tdebug;
					break;

				case 's':  /* shapes */
					++sdebug;
					break;

				case 'u':  /* Sethi-Ullman testing (machine dependent) */
					++udebug;
					break;

				case 'x':  /* general machine-dependent debugging flag */
					++xdebug;
					break;

				case 'w':
				case 'W':  /* shut up warnings */

					++Wflag;
					break;

#ifdef FORT
				case 'O':  /* optimizing */
					++Oflag;
					break;
#endif

				default:
					cerror( "bad option: %c", *cp );
					}
				}
			}
		else files = 1;  /* assumed to be a filename */
		}

	mkdope();
	setrew();
	return( files );

	}

# ifndef NOMAIN

unsigned int caloff();
unsigned int offsz;
mainp2( argc, argv ) char *argv[]; {
	register files;
	register temp;
	register c;
	register char *cp;
	register NODE *p;

	offsz = caloff();
	files = p2init( argc, argv );
	tinit();

	reread:

	if( files ){
		while( files < argc && argv[files][0] == '-' ) {
			++files;
			}
		if( files > argc ) return( nerrors );
		freopen( argv[files], "r", stdin );
		}
	while( (c=getchar()) > 0 ) switch( c ){
	case ')':
	default:
		/* copy line unchanged */
		if ( c != ')' )
			PUTCHAR( c );  /*  initial tab  */
		while( (c=getchar()) > 0 ){
			PUTCHAR(c);
			if( c == '\n' ) break;
			}
		continue;

	case BBEG:
		/* beginning of a block */
		temp = rdin(10);  /* ftnno */
		tmpoff = baseoff = (unsigned int) rdin(10); /* autooff for block gives max offset of autos in block */
		maxtreg = rdin(10);
		if( getchar() != '\n' ) cerror( "intermediate file format error");

		if( temp != ftnno ){ /* beginning of function */
			maxoff = baseoff;
			ftnno = temp;
			maxtemp = 0;
			}
		else {
			if( baseoff > maxoff ) maxoff = baseoff;
			/* maxoff at end of ftn is max of autos and temps
			   over all blocks in the function */
			}
		setregs();
		continue;

	case BEND:  /* end of block */
		SETOFF( maxoff, ALSTACK );
		eobl2();
		while( (c=getchar()) != '\n' ){
			if( c <= 0 ) cerror( "intermediate file format eof" );
			}
		continue;

	case EXPR:
		/* compile code for an expression */
		lineno = rdin( 10 );
		for( cp=filename; (*cp=getchar()) != '\n'; ++cp ) ; /* VOID, reads filename */
		*cp = '\0';
		if( lflag ) lineid( lineno, filename );

		tmpoff = baseoff;  /* expression at top level reuses temps */
		p = eread();

# ifndef BUG4
		if( edebug ) fwalk( p, eprint, 0 );
# endif

# ifdef MYREADER
		MYREADER(p);  /* do your own laundering of the input */
# endif

		nrecur = 0;
		delay( p );  /* expression statement  throws out results */
		reclaim( p, RNULL, 0 );

		allchk();
		tcheck();
		continue;

		}

	/* EOF */
	if( files ) goto reread;
	return(nerrors);

	}

# endif

# ifdef ONEPASS

p2compile( p ) NODE *p; {

	if( lflag ) lineid( lineno, filename );
	tmpoff = baseoff;  /* expression at top level reuses temps */
	/* generate code for the tree p */
# ifndef BUG4
	if( edebug ) fwalk( p, eprint, 0 );
# endif

# ifdef MYREADER
	MYREADER(p);  /* do your own laundering of the input */
# endif
	nrecur = 0;
	delay( p );  /* do the code generation */
	reclaim( p, RNULL, 0 );
	allchk();
	/* can't do tcheck here; some stuff (e.g., attributes) may be around from first pass */
	/* first pass will do it... */
	}

p2bbeg( aoff, myreg ) {
	static int myftn = -1;

	tmpoff = baseoff = (unsigned int) aoff;
	maxtreg = myreg;
	if( myftn != ftnno ){ /* beginning of function */
		maxoff = baseoff;
		myftn = ftnno;
		maxtemp = 0;
		}
	else {
		if( baseoff > maxoff ) maxoff = baseoff;
		/* maxoff at end of ftn is max of autos and temps over all blocks */
		}
	setregs();
	}

p2bend(){
	SETOFF( maxoff, ALSTACK );
	eobl2();
	}

# endif

NODE *deltrees[DELAYS];
int deli;

delay( p ) register NODE *p; {
	/* look in all legal places for COMOP's and ++ and -- ops to delay */
	/* note; don't delay ++ and -- within calls or things like
	/* getchar (in their macro forms) will start behaving strangely */
	register i;

	/* look for visible COMOPS, and rewrite repeatedly */

	while( delay1( p ) ) { /* VOID */ }

	/* look for visible, delayable ++ and -- */

	deli = 0;
	delay2( p );
	codgen( p, FOREFF );  /* do what is left */
	for( i = 0; i<deli; ++i ) codgen( deltrees[i], FOREFF );  /* do the rest */
	}

delay1( p ) register NODE *p; {  /* look for COMOPS */
	register o, ty;

	o = p->in.op;
	ty = optype( o );
	if( ty == LTYPE ) return( 0 );
	else if( ty == UTYPE ) return( delay1( p->in.left ) );

	switch( o ){

	case QUEST:
	case ANDAND:
	case OROR:
		/* don't look on RHS */
		return( delay1(p->in.left ) );

	case COMOP:  /* the meat of the routine */
		delay( p->in.left );  /* completely evaluate the LHS */
		/* rewrite the COMOP */
		{ register NODE *q;
			q = p->in.right;
			ncopy( p, p->in.right );
			q->in.op = FREE;
			}
		return( 1 );
		}

	return( delay1(p->in.left) || delay1(p->in.right ) );
	}

delay2( p ) register NODE *p; {

	/* look for delayable ++ and -- operators */

	register o, ty;
	o = p->in.op;
	ty = optype( o );

	switch( o ){

	case NOT:
	case QUEST:
	case ANDAND:
	case OROR:
	case CALL:
	case UNARY CALL:
	case STCALL:
	case UNARY STCALL:
	case FORTCALL:
	case UNARY FORTCALL:
	case COMOP:
	case CBRANCH:
		/* for the moment, don't delay past a conditional context, or
		/* inside of a call */
		return;

	case UNARY MUL:
		/* if *p++, do not rewrite */
		if( autoincr( p ) ) return;
		break;

	case INCR:
	case DECR:
		if( deltest( p ) ){
			if( deli < DELAYS ){
				register NODE *q;
				deltrees[deli++] = tcopy(p);
				q = p->in.left;
				p->in.right->in.op = FREE;  /* zap constant */
				ncopy( p, q );
				q->in.op = FREE;
				return;
				}
			}

		}

	if( ty == BITYPE ) delay2( p->in.right );
	if( ty != LTYPE ) delay2( p->in.left );
	}

codgen( p, cookie ) NODE *p; {

	/* generate the code for p;
	   order may call codgen recursively */
	/* cookie is used to describe the context */

	for(;;){
		canon(p);  /* creats OREG from * if possible and does sucomp */
		stotree = NIL;
# ifndef BUG4
		if( edebug ){
			printf( "store called on:\n" );
			fwalk( p, eprint, 0 );
			}
# endif
		store(p);
		if( stotree==NIL ) break;

		/* because it's minimal, can do w.o. stores */

		order( stotree, stocook );
		}

	order( p, cookie );

	}

# ifndef BUG4
char *cnames[] = {
	"SANY",
	"SAREG",
	"STAREG",
	"SBREG",
	"STBREG",
	"SCC",
	"SNAME",
	"SCON",
	"SFLD",
	"SOREG",
# ifdef WCARD1
	"WCARD1",
# else
	"STARNM",
# endif
# ifdef WCARD2
	"WCARD2",
# else
	"STARREG",
# endif
	"INTEMP",
	"FORARG",
	"SWADD",
	0,
	};

prcook( cookie ){

	/* print a nice-looking description of cookie */

	int i, flag;

	if( cookie & SPECIAL ){
		if( cookie == SZERO ) printf( "SZERO" );
		else if( cookie == SONE ) printf( "SONE" );
		else if( cookie == SMONE ) printf( "SMONE" );
		else if( cookie == SCCON ) printf( "SCCON" );
		else if( cookie == SSCON ) printf( "SSCON" );
		else if( cookie == SSOREG ) printf( "SSOREG" );
		else printf( "SPECIAL+%d", cookie & ~SPECIAL );
		return;
		}

	flag = 0;
	for( i=0; cnames[i]; ++i ){
		if( cookie & (1<<i) ){
			if( flag ) printf( "|" );
			++flag;
			printf( cnames[i] );
			}
		}

	}
# endif

int odebug = 0;

order(p,cook) NODE *p; {

	register o, ty, m;
	int m1;
	int cookie;
	NODE *p1, *p2;

	cookie = cook;
	rcount();
	canon(p);
	rallo( p, p->in.rall );
	goto first;
	/* by this time, p should be able to be generated without stores;
	   the only question is how */

	again:

	if ( p->in.op == FREE )
		return;		/* whole tree was done */
	cookie = cook;
	rcount();
	canon(p);
	rallo( p, p->in.rall );
	/* if any rewriting and canonicalization has put
	 * the tree (p) into a shape that cook is happy
	 * with (exclusive of FOREFF, FORREW, and INTEMP)
	 * then we are done.
	 * this allows us to call order with shapes in
	 * addition to cookies and stop short if possible.
	 */
	if( tshape(p, cook &(~(FOREFF|FORREW|INTEMP))) )return;

	first:
# ifndef BUG4
	if( odebug ){
		printf( "order( %o, ", p );
		prcook( cookie );
		printf( " )\n" );
		fwalk( p, eprint, 0 );
		}
# endif

	o = p->in.op;
	ty = optype(o);

	/* first of all, for most ops, see if it is in the table */

	/* look for ops */

	switch( m = p->in.op ){

	default:
		/* look for op in table */
		for(;;){
			if( (m = match( p, cookie ) ) == MDONE ) goto cleanup;
			else if( m == MNOPE ){
				if( !(cookie = nextcook( p, cookie ) ) ) goto nomat;
				continue;
				}
			else break;
			}
		break;

	case COMOP:
	case FORCE:
	case CBRANCH:
	case QUEST:
	case ANDAND:
	case OROR:
	case NOT:
	case UNARY CALL:
	case CALL:
	case UNARY STCALL:
	case STCALL:
	case UNARY FORTCALL:
	case FORTCALL:
		/* don't even go near the table... */
		;

		}
	/* get here to do rewriting if no match or
	   fall through from above for hard ops */

	p1 = p->in.left;
	if( ty == BITYPE ) p2 = p->in.right;
	else p2 = NIL;
	
# ifndef BUG4
	if( odebug ){
		printf( "order( %o, ", p );
		prcook( cook );
		printf( " ), cookie " );
		prcook( cookie );
		printf( ", rewrite %s\n", opst[m] );
		}
# endif
	switch( m ){
	default:
		nomat:
		cerror( "no table entry for op %s", opst[p->in.op] );

	case COMOP:
		codgen( p1, FOREFF );
		p2->in.rall = p->in.rall;
		codgen( p2, cookie );
		ncopy( p, p2 );
		p2->in.op = FREE;
		goto cleanup;

	case FORCE:
		/* recurse, letting the work be done by rallo */
		p = p->in.left;
		cook = INTAREG|INTBREG;
		goto again;

	case CBRANCH:
		o = p2->tn.lval;
		cbranch( p1, -1, o );
		p2->in.op = FREE;
		p->in.op = FREE;
		return;

	case QUEST:
		cbranch( p1, -1, m=getlab() );
		p2->in.left->in.rall = p->in.rall;
		codgen( p2->in.left, INTAREG|INTBREG );
		/* force right to compute result into same reg used by left */
		p2->in.right->in.rall = p2->in.left->tn.rval|MUSTDO;
		reclaim( p2->in.left, RNULL, 0 );
		cbgen( 0, m1 = getlab(), 'I' );
		deflab( m );
		codgen( p2->in.right, INTAREG|INTBREG );
		deflab( m1 );
		p->in.op = REG;  /* set up node describing result */
		p->tn.lval = 0;
		p->tn.rval = p2->in.right->tn.rval;
		p->in.type = p2->in.right->in.type;
		tfree( p2->in.right );
		p2->in.op = FREE;
		goto cleanup;

	case ANDAND:
	case OROR:
	case NOT:  /* logical operators */
		/* if here, must be a logical operator for 0-1 value */
		cbranch( p, -1, m=getlab() );
		p->in.op = CCODES;
		p->bn.label = m;
		order( p, INTAREG );
		goto cleanup;

	case FLD:	/* fields of funny type */
		if ( p1->in.op == UNARY MUL ){
			offstar( p1->in.left );
			goto again;
			}

	case UNARY MINUS:
#if defined(tahoe)
		order( p1, INBREG|INAREG|SOREG );
#else
		order( p1, INBREG|INAREG );
#endif
		goto again;

	case NAME:
		/* all leaves end up here ... */
		if( o == REG ) goto nomat;
		order( p, INTAREG|INTBREG );
		goto again;

	case INIT:
		uerror( "illegal initialization" );
		return;

	case UNARY FORTCALL:
		p->in.right = NIL;
	case FORTCALL:
		o = p->in.op = UNARY FORTCALL;
		if( genfcall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY CALL:
		p->in.right = NIL;
	case CALL:
		o = p->in.op = UNARY CALL;
		if( gencall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY STCALL:
		p->in.right = NIL;
	case STCALL:
		o = p->in.op = UNARY STCALL;
		if( genscall( p, cookie ) ) goto nomat;
		goto cleanup;

		/* if arguments are passed in register, care must be taken that reclaim
		/* not throw away the register which now has the result... */

	case UNARY MUL:
		if( cook == FOREFF ){
			/* do nothing */
			order( p->in.left, FOREFF );
			p->in.op = FREE;
			return;
			}
		offstar( p->in.left );
		goto again;

	case INCR:  /* INCR and DECR */
		if( setincr(p) ) goto again;

		/* x++ becomes (x += 1) -1; */

		if( cook & FOREFF ){  /* result not needed so inc or dec and be done with it */
			/* x++ => x += 1 */
			p->in.op = (p->in.op==INCR)?ASG PLUS:ASG MINUS;
			goto again;
			}

		p1 = tcopy(p);
		reclaim( p->in.left, RNULL, 0 );
		p->in.left = p1;
		p1->in.op = (p->in.op==INCR)?ASG PLUS:ASG MINUS;
		p->in.op = (p->in.op==INCR)?MINUS:PLUS;
		goto again;

	case STASG:
		if( setstr( p ) ) goto again;
		goto nomat;

	case ASG PLUS:  /* and other assignment ops */
		if( setasop(p) ) goto again;

		/* there are assumed to be no side effects in LHS */

		p2 = tcopy(p);
		p->in.op = ASSIGN;
		reclaim( p->in.right, RNULL, 0 );
		p->in.right = p2;
		canon(p);
		rallo( p, p->in.rall );

# ifndef BUG4
		if( odebug ) fwalk( p, eprint, 0 );
# endif

		order( p2->in.left, INTBREG|INTAREG );
		order( p2, INTBREG|INTAREG );
		goto again;

	case ASSIGN:
		if( setasg( p ) ) goto again;
		goto nomat;


	case BITYPE:
		if( setbin( p ) ) goto again;
		/* try to replace binary ops by =ops */
		switch(o){

		case PLUS:
		case MINUS:
		case MUL:
		case DIV:
		case MOD:
		case AND:
		case OR:
		case ER:
		case LS:
		case RS:
			p->in.op = ASG o;
			goto again;
			}
		goto nomat;

		}

	cleanup:

	/* if it is not yet in the right state, put it there */

	if( cook & FOREFF ){
		reclaim( p, RNULL, 0 );
		return;
		}

	if( p->in.op==FREE ) return;

	if( tshape( p, cook ) ) return;

	if( (m=match(p,cook) ) == MDONE ) return;

	/* we are in bad shape, try one last chance */
	if( lastchance( p, cook ) ) goto again;

	goto nomat;
	}

int callflag;
int fregs;

store( p ) register NODE *p; {

	/* find a subtree of p which should be stored */

	register o, ty;

	o = p->in.op;
	ty = optype(o);

	if( ty == LTYPE ) return;

	switch( o ){

	case UNARY CALL:
	case UNARY FORTCALL:
	case UNARY STCALL:
		++callflag;
		break;

	case UNARY MUL:
		if( asgop(p->in.left->in.op) ) stoasg( p->in.left, UNARY MUL );
		break;

	case CALL:
	case FORTCALL:
	case STCALL:
		store( p->in.left );
		stoarg( p->in.right, o );
		++callflag;
		return;

	case COMOP:
		markcall( p->in.right );
		if( p->in.right->in.su > fregs ) SETSTO( p, INTEMP );
		store( p->in.left );
		return;

	case ANDAND:
	case OROR:
	case QUEST:
		markcall( p->in.right );
		if( p->in.right->in.su > fregs ) SETSTO( p, INTEMP );
	case CBRANCH:   /* to prevent complicated expressions on the LHS from being stored */
	case NOT:
		constore( p->in.left );
		return;

		}

	if( ty == UTYPE ){
		store( p->in.left );
		return;
		}

	if( asgop( p->in.right->in.op ) ) stoasg( p->in.right, o );

	if( p->in.su>fregs ){ /* must store */
		mkadrs( p );  /* set up stotree and stocook to subtree
				 that must be stored */
		}

	store( p->in.right );
	store( p->in.left );
	}

constore( p ) register NODE *p; {

	/* store conditional expressions */
	/* the point is, avoid storing expressions in conditional
	   conditional context, since the evaluation order is predetermined */

	switch( p->in.op ) {

	case ANDAND:
	case OROR:
	case QUEST:
		markcall( p->in.right );
	case NOT:
		constore( p->in.left );
		return;

		}

	store( p );
	}

markcall( p ) register NODE *p; {  /* mark off calls below the current node */

	again:
	switch( p->in.op ){

	case UNARY CALL:
	case UNARY STCALL:
	case UNARY FORTCALL:
	case CALL:
	case STCALL:
	case FORTCALL:
		++callflag;
		return;

		}

	switch( optype( p->in.op ) ){

	case BITYPE:
		markcall( p->in.right );
	case UTYPE:
		p = p->in.left;
		/* eliminate recursion (aren't I clever...) */
		goto again;
	case LTYPE:
		return;
		}

	}

stoarg( p, calltype ) register NODE *p; {
	/* arrange to store the args */

	if( p->in.op == CM ){
		stoarg( p->in.left, calltype );
		p = p->in.right ;
		}
	if( calltype == CALL ){
		STOARG(p);
		}
	else if( calltype == STCALL ){
		STOSTARG(p);
		}
	else {
		STOFARG(p);
		}
	callflag = 0;
	store(p);
# ifndef NESTCALLS
	if( callflag ){ /* prevent two calls from being active at once  */
		SETSTO(p,INTEMP);
		store(p); /* do again to preserve bottom up nature....  */
		}
#endif
	}

int negrel[] = { NE, EQ, GT, GE, LT, LE, UGT, UGE, ULT, ULE } ;  /* negatives of relationals */

cbranch( p, true, false ) NODE *p; {
	/* evaluate p for truth value, and branch to true or false
	/* accordingly: label <0 means fall through */

	register o, lab, flab, tlab;

	lab = -1;

	switch( o=p->in.op ){

	case ULE:
	case ULT:
	case UGE:
	case UGT:
	case EQ:
	case NE:
	case LE:
	case LT:
	case GE:
	case GT:
		if( true < 0 ){
			o = p->in.op = negrel[ o-EQ ];
			true = false;
			false = -1;
			}
#ifndef NOOPT
		if( p->in.right->in.op == ICON && p->in.right->tn.lval == 0 && p->in.right->in.name[0] == '\0' ){
			switch( o ){

			case UGT:
			case ULE:
				o = p->in.op = (o==UGT)?NE:EQ;
			case EQ:
			case NE:
			case LE:
			case LT:
			case GE:
			case GT:
				if( logop(p->in.left->in.op) ){
					/* strange situation: e.g., (a!=0) == 0 */
					/* must prevent reference to p->in.left->lable, so get 0/1 */
					/* we could optimize, but why bother */
					codgen( p->in.left, INAREG|INBREG );
					}
				codgen( p->in.left, FORCC );
				cbgen( o, true, 'I' );
				break;

			case UGE:
				codgen(p->in.left, FORCC);
				cbgen( 0, true, 'I' );  /* unconditional branch */
				break;
			case ULT:
				codgen(p->in.left, FORCC);
				}
			}
		else
#endif
			{
			p->bn.label = true;
			codgen( p, FORCC );
			}
		if( false>=0 ) cbgen( 0, false, 'I' );
		reclaim( p, RNULL, 0 );
		return;

	case ANDAND:
		lab = false<0 ? getlab() : false ;
		cbranch( p->in.left, -1, lab );
		cbranch( p->in.right, true, false );
		if( false < 0 ) deflab( lab );
		p->in.op = FREE;
		return;

	case OROR:
		lab = true<0 ? getlab() : true;
		cbranch( p->in.left, lab, -1 );
		cbranch( p->in.right, true, false );
		if( true < 0 ) deflab( lab );
		p->in.op = FREE;
		return;

	case NOT:
		cbranch( p->in.left, false, true );
		p->in.op = FREE;
		break;

	case COMOP:
		codgen( p->in.left, FOREFF );
		p->in.op = FREE;
		cbranch( p->in.right, true, false );
		return;

	case QUEST:
		flab = false<0 ? getlab() : false;
		tlab = true<0 ? getlab() : true;
		cbranch( p->in.left, -1, lab = getlab() );
		cbranch( p->in.right->in.left, tlab, flab );
		deflab( lab );
		cbranch( p->in.right->in.right, true, false );
		if( true < 0 ) deflab( tlab);
		if( false < 0 ) deflab( flab );
		p->in.right->in.op = FREE;
		p->in.op = FREE;
		return;

	case ICON:
		if( p->in.type != FLOAT && p->in.type != DOUBLE ){

			if( p->tn.lval || p->in.name[0] ){
				/* addresses of C objects are never 0 */
				if( true>=0 ) cbgen( 0, true, 'I' );
				}
			else if( false>=0 ) cbgen( 0, false, 'I' );
			p->in.op = FREE;
			return;
			}
		/* fall through to default with other strange constants */

	default:
		/* get condition codes */
		codgen( p, FORCC );
		if( true >= 0 ) cbgen( NE, true, 'I' );
		if( false >= 0 ) cbgen( true >= 0 ? 0 : EQ, false, 'I' );
		reclaim( p, RNULL, 0 );
		return;

		}

	}

rcount(){ /* count recursions */
	if( ++nrecur > NRECUR ){
		cerror( "expression causes compiler loop: try simplifying" );
		}

	}

# ifndef BUG4
eprint( p, down, a, b ) NODE *p; int *a, *b; {

	*a = *b = down+1;
	while( down >= 2 ){
		printf( "\t" );
		down -= 2;
		}
	if( down-- ) printf( "    " );


	printf( "%o) %s", p, opst[p->in.op] );
	switch( p->in.op ) { /* special cases */

	case REG:
		printf( " %s", rnames[p->tn.rval] );
		break;

	case ICON:
	case NAME:
	case OREG:
		printf( " " );
		adrput( p );
		break;

	case STCALL:
	case UNARY STCALL:
	case STARG:
	case STASG:
		printf( " size=%d", p->stn.stsize );
		printf( " align=%d", p->stn.stalign );
		break;
		}

	printf( ", " );
	tprint( p->in.type );
	printf( ", " );
	if( p->in.rall == NOPREF ) printf( "NOPREF" );
	else {
		if( p->in.rall & MUSTDO ) printf( "MUSTDO " );
		else printf( "PREF " );
		printf( "%s", rnames[p->in.rall&~MUSTDO]);
		}
	printf( ", SU= %d\n", p->in.su );

	}
# endif

# ifndef NOMAIN
NODE *
eread(){

	/* call eread recursively to get subtrees, if any */

	register NODE *p;
	register i, c;
	register char *pc;
	register j;

	i = rdin( 10 );

	p = talloc();

	p->in.op = i;

	i = optype(i);

	if( i == LTYPE ) p->tn.lval = rdin( 10 );
	if( i != BITYPE ) p->tn.rval = rdin( 10 );

	p->in.type = rdin(8 );
	p->in.rall = NOPREF;  /* register allocation information */

	if( p->in.op == STASG || p->in.op == STARG || p->in.op == STCALL || p->in.op == UNARY STCALL ){
		p->stn.stsize = (rdin( 10 ) + (SZCHAR-1) )/SZCHAR;
		p->stn.stalign = rdin(10) / SZCHAR;
		if( getchar() != '\n' ) cerror( "illegal \n" );
		}
	else {   /* usual case */
		if( p->in.op == REG ) rbusy( p->tn.rval, p->in.type );  /* non usually, but sometimes justified */
#ifndef FLEXNAMES
		for( pc=p->in.name,j=0; ( c = getchar() ) != '\n'; ++j ){
			if( j < NCHNAM ) *pc++ = c;
			}
		if( j < NCHNAM ) *pc = '\0';
#else
		{ char buf[BUFSIZ];
		for( pc=buf,j=0; ( c = getchar() ) != '\n'; ++j ){
			if( j < BUFSIZ ) *pc++ = c;
			}
		if( j < BUFSIZ ) *pc = '\0';
		p->in.name = tstr(buf);
		}
#endif
		}

	/* now, recursively read descendents, if any */

	if( i != LTYPE ) p->in.left = eread();
	if( i == BITYPE ) p->in.right = eread();

	return( p );

	}

CONSZ
rdin( base ){
	register sign, c;
	CONSZ val;

	sign = 1;
	val = 0;

	while( (c=getchar()) > 0 ) {
		if( c == '-' ){
			if( val != 0 ) cerror( "illegal -");
			sign = -sign;
			continue;
			}
		if( c == '\t' ) break;
		if( c>='0' && c<='9' ) {
			val *= base;
			if( sign > 0 )
				val += c-'0';
			else
				val -= c-'0';
			continue;
			}
		cerror( "illegal character `%c' on intermediate file", c );
		break;
		}

	if( c <= 0 ) {
		cerror( "unexpected EOF");
		}
	return( val );
	}
# endif

#ifndef FIELDOPS
	/* do this if there is no special hardware support for fields */

ffld( p, down, down1, down2 ) NODE *p; int *down1, *down2; {
	 /* look for fields that are not in an lvalue context, and rewrite them... */
	register NODE *shp;
	register s, o, v, ty;

	*down1 =  asgop( p->in.op );
	*down2 = 0;

	if( !down && p->in.op == FLD ){ /* rewrite the node */

		if( !rewfld(p) ) return;

		ty = (szty(p->in.type) == 2)? LONG: INT;
		v = p->tn.rval;
		s = UPKFSZ(v);
# ifdef RTOLBYTES
		o = UPKFOFF(v);  /* amount to shift */
# else
		o = szty(p->in.type)*SZINT - s - UPKFOFF(v);  /* amount to shift */
#endif

		/* make & mask part */

		p->in.left->in.type = ty;

		p->in.op = AND;
		p->in.right = talloc();
		p->in.right->in.op = ICON;
		p->in.right->in.rall = NOPREF;
		p->in.right->in.type = ty;
		p->in.right->tn.lval = 1;
		p->in.right->tn.rval = 0;
#ifndef FLEXNAMES
		p->in.right->in.name[0] = '\0';
#else
		p->in.right->in.name = "";
#endif
		p->in.right->tn.lval <<= s;
		p->in.right->tn.lval--;

		/* now, if a shift is needed, do it */

		if( o != 0 ){
			shp = talloc();
			shp->in.op = RS;
			shp->in.rall = NOPREF;
			shp->in.type = ty;
			shp->in.left = p->in.left;
			shp->in.right = talloc();
			shp->in.right->in.op = ICON;
			shp->in.right->in.rall = NOPREF;
			shp->in.right->in.type = ty;
			shp->in.right->tn.rval = 0;
			shp->in.right->tn.lval = o;  /* amount to shift */
#ifndef FLEXNAMES
			shp->in.right->in.name[0] = '\0';
#else
			shp->in.right->in.name = "";
#endif
			p->in.left = shp;
			/* whew! */
			}
		}
	}
#endif

oreg2( p ) register NODE *p; {

	/* look for situations where we can turn * into OREG */

	NODE *q;
	register i;
	register r;
	register char *cp;
	register NODE *ql, *qr;
	CONSZ temp;

	if( p->in.op == UNARY MUL ){
		q = p->in.left;
		if( q->in.op == REG ){
			temp = q->tn.lval;
			r = q->tn.rval;
			cp = q->in.name;
			goto ormake;
			}

		if( q->in.op != PLUS && q->in.op != MINUS ) return;
		ql = q->in.left;
		qr = q->in.right;

#ifdef R2REGS

		/* look for doubly indexed expressions */

		if( q->in.op == PLUS) {
			if( (r=base(ql))>=0 && (i=offset(qr, tlen(p)))>=0) {
				makeor2(p, ql, r, i);
				return;
			} else if( (r=base(qr))>=0 && (i=offset(ql, tlen(p)))>=0) {
				makeor2(p, qr, r, i);
				return;
				}
			}


#endif

		if( (q->in.op==PLUS || q->in.op==MINUS) && qr->in.op == ICON &&
				ql->in.op==REG && szty(qr->in.type)==1) {
			temp = qr->tn.lval;
			if( q->in.op == MINUS ) temp = -temp;
			r = ql->tn.rval;
			temp += ql->tn.lval;
			cp = qr->in.name;
			if( *cp && ( q->in.op == MINUS || *ql->in.name ) ) return;
			if( !*cp ) cp = ql->in.name;

			ormake:
			if( notoff( p->in.type, r, temp, cp ) ) return;
			p->in.op = OREG;
			p->tn.rval = r;
			p->tn.lval = temp;
#ifndef FLEXNAMES
			for( i=0; i<NCHNAM; ++i )
				p->in.name[i] = *cp++;
#else
			p->in.name = cp;
#endif
			tfree(q);
			return;
			}
		}

	}

canon(p) NODE *p; {
	/* put p in canonical form */
	int oreg2(), sucomp();

#ifndef FIELDOPS
	int ffld();
	fwalk( p, ffld, 0 ); /* look for field operators */
# endif
	walkf( p, oreg2 );  /* look for and create OREG nodes */
#ifdef MYCANON
	MYCANON(p);  /* your own canonicalization routine(s) */
#endif
	walkf( p, sucomp );  /* do the Sethi-Ullman computation */

	}

