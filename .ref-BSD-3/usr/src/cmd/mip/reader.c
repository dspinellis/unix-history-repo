# include "mfile2"

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
extern int Wflag;
int edebug = 0;
int xdebug = 0;
int udebug = 0;

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

				case 'W': /*Shut up warning: etc. */
					++Wflag;
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

mainp2( argc, argv ) char *argv[]; {
	register files;
	register temp;
	register c;
	register char *cp;
	register NODE *p;

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
		/* copy line unchanged */
		while( (c=getchar()) > 0 ){
			PUTCHAR(c);
			if( c == '\n' ) break;
			}
		continue;

	case '[':
		/* beginning of a block */
		temp = rdin(10);  /* ftnno */
		tmpoff = baseoff = rdin(10); /* autooff for block gives max offset of autos in block */
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

	case ']':  /* end of block */
		SETOFF( maxoff, ALSTACK );
		eobl2();
		while( (c=getchar()) != '\n' ){
			if( c <= 0 ) cerror( "intermediate file format eof" );
			}
		continue;

	case '.':
		/* compile code for an expression */
		lineno = rdin( 10 );
		for( cp=filename; (*cp=getchar()) != '\n'; ++cp ) ; /* VOID, reads filename */
		*cp = '\0';
		if( lflag ) lineid( lineno, filename );

		tmpoff = baseoff;  /* expression at top level reuses temps */
		p = eread();

		if( edebug ) fwalk( p, eprint, 0 );

# ifdef MYREADER
		MYREADER(p);  /* do your own laundering of the input */
# endif

		nrecur = 0;
		delay( p );  /* expression statement  throws out results */
		reclaim( p, RNULL, 0 );

		allchk();
		tcheck();
		continue;

	default:
		cerror( "intermediate file format error" );

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
	if( edebug ) fwalk( p, eprint, 0 );

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
	tmpoff = baseoff = aoff;
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

	o = p->op;
	ty = optype( o );
	if( ty == LTYPE ) return( 0 );
	else if( ty == UTYPE ) return( delay1( p->left ) );

	switch( o ){

	case QUEST:
	case ANDAND:
	case OROR:
		/* don't look on RHS */
		return( delay1(p->left ) );

	case COMOP:  /* the meat of the routine */
		delay( p->left );  /* completely evaluate the LHS */
		/* rewrite the COMOP */
		{ register NODE *q;
			q = p->right;
			ncopy( p, p->right );
			q->op = FREE;
			}
		return( 1 );
		}

	return( delay1(p->left) || delay1(p->right ) );
	}

delay2( p ) register NODE *p; {

	/* look for delayable ++ and -- operators */

	register o, ty;
	o = p->op;
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
		/* for the moment, don7t delay past a conditional context, or
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
				q = p->left;
				p->right->op = FREE;  /* zap constant */
				ncopy( p, q );
				q->op = FREE;
				return;
				}
			}

		}

	if( ty == BITYPE ) delay2( p->right );
	if( ty != LTYPE ) delay2( p->left );
	}

codgen( p, cookie ) NODE *p; {

	/* generate the code for p;
	   order may call codgen recursively */
	/* cookie is used to describe the context */

	for(;;){
		canon(p);  /* creats OREG from * if possible and does sucomp */
		stotree = NIL;
		if( edebug ){
			printf( "store called on:\n" );
			fwalk( p, eprint, 0 );
			}
		store(p);
		if( stotree==NIL ) break;

		/* because it's minimal, can do w.o. stores */

		order( stotree, stocook );
		}

	order( p, cookie );

	}

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
	"STARNM",
	"STARREG",
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

int odebug = 0;

order(p,cook) NODE *p; {

	register o, ty, m;
	int m1;
	int cookie;
	NODE *p1, *p2;

	/* by this time, p should be able to be generated without stores;
	   the only question is how */

	again:

	cookie = cook;
	rcount();
	canon(p);
	rallo( p, p->rall );

	if( odebug ){
		printf( "order( %o, ", p );
		prcook( cookie );
		printf( " )\n" );
		fwalk( p, eprint, 0 );
		}

	o = p->op;
	ty = optype(o);

	/* first of all, for most ops, see if it is in the table */

	/* look for ops */

	switch( m = p->op ){

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

	p1 = p->left;
	if( ty == BITYPE ) p2 = p->right;
	else p2 = NIL;
	
	if( odebug ){
		printf( "order( %o, ", p );
		prcook( cook );
		printf( " ), cookie " );
		prcook( cookie );
		printf( ", rewrite %s\n", opst[m] );
		}
	switch( m ){
	default:
		nomat:
		cerror( "no table entry for op %s", opst[p->op] );

	case COMOP:
		codgen( p1, FOREFF );
		p2->rall = p->rall;
		codgen( p2, cookie );
		ncopy( p, p2 );
		p2->op = FREE;
		goto cleanup;

	case FORCE:
		/* recurse, letting the work be done by rallo */
		p = p->left;
		cook = INTAREG|INTBREG;
		goto again;

	case CBRANCH:
		o = p2->lval;
		cbranch( p1, -1, o );
		p2->op = FREE;
		p->op = FREE;
		return;

	case QUEST:
		cbranch( p1, -1, m=getlab() );
		p2->left->rall = p->rall;
		codgen( p2->left, INTAREG|INTBREG );
		/* force right to compute result into same reg used by left */
		p2->right->rall = p2->left->rval|MUSTDO;
		reclaim( p2->left, RNULL, 0 );
		cbgen( 0, m1 = getlab(), 'I' );
		deflab( m );
		codgen( p2->right, INTAREG|INTBREG );
		deflab( m1 );
		p->op = REG;  /* set up node describing result */
		p->lval = 0;
		p->rval = p2->right->rval;
		p->type = p2->right->type;
		tfree( p2->right );
		p2->op = FREE;
		goto cleanup;

	case ANDAND:
	case OROR:
	case NOT:  /* logical operators */
		/* if here, must be a logical operator for 0-1 value */
		cbranch( p, -1, m=getlab() );
		p->op = CCODES;
		p->label = m;
		order( p, INTAREG );
		goto cleanup;

	case FLD:	/* fields of funny type */
		if ( p1->op == UNARY MUL ){
			offstar( p1->left );
			goto again;
			}

	case UNARY MINUS:
		order( p1, INBREG|INAREG );
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
		p->right = NIL;
	case FORTCALL:
		o = p->op = UNARY FORTCALL;
		if( genfcall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY CALL:
		p->right = NIL;
	case CALL:
		o = p->op = UNARY CALL;
		if( gencall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY STCALL:
		p->right = NIL;
	case STCALL:
		o = p->op = UNARY STCALL;
		if( genscall( p, cookie ) ) goto nomat;
		goto cleanup;

		/* if arguments are passed in register, care must be taken that reclaim
		/* not throw away the register which now has the result... */

	case UNARY MUL:
		if( cook == FOREFF ){
			/* do nothing */
			order( p->left, FOREFF );
			p->op = FREE;
			return;
			}
		offstar( p->left );
		canon(p);
		if( canaddr(p) && cook != INTEMP ) goto cleanup;
		goto again;

	case INCR:  /* INCR and DECR */
		if( setincr(p) ) goto again;

		/* x++ becomes (x += 1) -1; */

		if( cook & FOREFF ){  /* result not needed so inc or dec and be done with it */
			/* x++ => x += 1 */
			p->op = (p->op==INCR)?ASG PLUS:ASG MINUS;
			goto again;
			}

		p1 = tcopy(p);
		reclaim( p->left, RNULL, 0 );
		p->left = p1;
		p1->op = (p->op==INCR)?ASG PLUS:ASG MINUS;
		p->op = (p->op==INCR)?MINUS:PLUS;
		goto again;

	case STASG:
		if( setstr( p ) ) goto again;
		goto nomat;

	case ASG PLUS:  /* and other assignment ops */
		if( setasop(p) ) goto again;

		/* there are assumed to be no side effects in LHS */

		p2 = tcopy(p);
		p->op = ASSIGN;
		reclaim( p->right, RNULL, 0 );
		p->right = p2;
		canon(p);
		rallo( p, p->rall );

		if( odebug ) fwalk( p, eprint, 0 );

		order( p2->left, INTBREG|INTAREG );
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
			p->op = ASG o;
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

	if( p->op==FREE ) return;

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

	o = p->op;
	ty = optype(o);

	if( ty == LTYPE ) return;

	switch( o ){

	case UNARY CALL:
	case UNARY FORTCALL:
	case UNARY STCALL:
		++callflag;
		break;

	case UNARY MUL:
		if( asgop(p->left->op) ) stoasg( p->left, UNARY MUL );
		break;

	case CALL:
	case FORTCALL:
	case STCALL:
		store( p->left );
		stoarg( p->right, o );
		++callflag;
		return;

	case COMOP:
		markcall( p->right );
		if( p->right->su > fregs ) SETSTO( p, INTEMP );
		store( p->left );
		return;

	case ANDAND:
	case OROR:
	case QUEST:
		markcall( p->right );
		if( p->right->su > fregs ) SETSTO( p, INTEMP );
	case CBRANCH:   /* to prevent complicated expressions on the LHS from being stored */
	case NOT:
		constore( p->left );
		return;

		}

	if( ty == UTYPE ){
		store( p->left );
		return;
		}

	if( asgop( p->right->op ) ) stoasg( p->right, o );

	if( p->su>fregs ){ /* must store */
		mkadrs( p );  /* set up stotree and stocook to subtree
				 that must be stored */
		}

	store( p->right );
	store( p->left );
	}

constore( p ) register NODE *p; {

	/* store conditional expressions */
	/* the point is, avoid storing expressions in conditional
	   conditional context, since the evaluation order is predetermined */

	switch( p->op ) {

	case ANDAND:
	case OROR:
	case QUEST:
		markcall( p->right );
	case NOT:
		constore( p->left );
		return;

		}

	store( p );
	}

markcall( p ) register NODE *p; {  /* mark off calls below the current node */

	again:
	switch( p->op ){

	case UNARY CALL:
	case UNARY STCALL:
	case UNARY FORTCALL:
	case CALL:
	case STCALL:
	case FORTCALL:
		++callflag;
		return;

		}

	switch( optype( p->op ) ){

	case BITYPE:
		markcall( p->right );
	case UTYPE:
		p = p->left;
		/* eliminate recursion (aren't I clever...) */
		goto again;
	case LTYPE:
		return;
		}

	}

stoarg( p, calltype ) register NODE *p; {
	/* arrange to store the args */

	if( p->op == CM ){
		stoarg( p->left, calltype );
		p = p->right ;
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

	switch( o=p->op ){

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
			o = p->op = negrel[ o-EQ ];
			true = false;
			false = -1;
			}
#ifndef NOOPT
		if( p->right->op == ICON && p->right->lval == 0 && p->right->name[0] == '\0' ){
			switch( o ){

			case UGT:
			case ULE:
				o = p->op = (o==UGT)?NE:EQ;
			case EQ:
			case NE:
			case LE:
			case LT:
			case GE:
			case GT:
				if( logop(p->left->op) ){
					/* strange situation: e.g., (a!=0) == 0 */
					/* must prevent reference to p->left->lable, so get 0/1 */
					/* we could optimize, but why bother */
					codgen( p->left, INAREG|INBREG );
					}
				codgen( p->left, FORCC );
				cbgen( o, true, 'I' );
				break;

			case UGE:
				cbgen( 0, true, 'I' );  /* unconditional branch */
			case ULT:
				;   /* do nothing for LT */
				}
			}
		else
#endif
			{
			p->label = true;
			codgen( p, FORCC );
			}
		if( false>=0 ) cbgen( 0, false, 'I' );
		reclaim( p, RNULL, 0 );
		return;

	case ANDAND:
		lab = false<0 ? getlab() : false ;
		cbranch( p->left, -1, lab );
		cbranch( p->right, true, false );
		if( false < 0 ) deflab( lab );
		p->op = FREE;
		return;

	case OROR:
		lab = true<0 ? getlab() : true;
		cbranch( p->left, lab, -1 );
		cbranch( p->right, true, false );
		if( true < 0 ) deflab( lab );
		p->op = FREE;
		return;

	case NOT:
		cbranch( p->left, false, true );
		p->op = FREE;
		break;

	case COMOP:
		codgen( p->left, FOREFF );
		p->op = FREE;
		cbranch( p->right, true, false );
		return;

	case QUEST:
		flab = false<0 ? getlab() : false;
		tlab = true<0 ? getlab() : true;
		cbranch( p->left, -1, lab = getlab() );
		cbranch( p->right->left, tlab, flab );
		deflab( lab );
		cbranch( p->right->right, true, false );
		if( true < 0 ) deflab( tlab);
		if( false < 0 ) deflab( flab );
		p->right->op = FREE;
		p->op = FREE;
		return;

	case ICON:
		if( p->type != FLOAT && p->type != DOUBLE ){

			if( p->lval || p->name[0] ){
				/* addresses of C objects are never 0 */
				if( true>=0 ) cbgen( 0, true, 'I' );
				}
			else if( false>=0 ) cbgen( 0, false, 'I' );
			p->op = FREE;
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

eprint( p, down, a, b ) NODE *p; int *a, *b; {

	*a = *b = down+1;
	while( down >= 2 ){
		printf( "\t" );
		down -= 2;
		}
	if( down-- ) printf( "    " );


	printf( "%o) %s", p, opst[p->op] );
	switch( p->op ) { /* special cases */

	case REG:
		printf( " %s", rnames[p->rval] );
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
		printf( " size=%d", p->stsize );
		printf( " align=%d", p->stalign );
		break;
		}

	printf( ", " );
	tprint( p->type );
	printf( ", " );
	if( p->rall == NOPREF ) printf( "NOPREF" );
	else {
		if( p->rall & MUSTDO ) printf( "MUSTDO " );
		else printf( "PREF " );
		printf( "%s", rnames[p->rall&~MUSTDO]);
		}
	printf( ", SU= %d\n", p->su );

	}

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

	p->op = i;

	i = optype(i);

	if( i == LTYPE ) p->lval = rdin( 10 );
	if( i != BITYPE ) p->rval = rdin( 10 );

	p->type = rdin(8 );
	p->rall = NOPREF;  /* register allocation information */

	if( p->op == STASG || p->op == STARG || p->op == STCALL || p->op == UNARY STCALL ){
		p->stsize = (rdin( 10 ) + (SZCHAR-1) )/SZCHAR;
		p->stalign = rdin(10) / SZCHAR;
		if( getchar() != '\n' ) cerror( "illegal \n" );
		}
	else {   /* usual case */
		if( p->op == REG ) rbusy( p->rval, p->type );  /* non usually, but sometimes justified */
		for( pc=p->name,j=0; ( c = getchar() ) != '\n'; ++j ){
			if( j < NCHNAM ) *pc++ = c;
			}
		if( j < NCHNAM ) *pc = '\0';
		}

	/* now, recursively read descendents, if any */

	if( i != LTYPE ) p->left = eread();
	if( i == BITYPE ) p->right = eread();

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

	*down1 =  asgop( p->op );
	*down2 = 0;

	if( !down && p->op == FLD ){ /* rewrite the node */

		if( !rewfld(p) ) return;

		ty = (szty(p->type) == 2)? LONG: INT;
		v = p->rval;
		s = UPKFSZ(v);
# ifdef RTOLBYTES
		o = UPKFOFF(v);  /* amount to shift */
# else
		o = szty(p->type)*SZINT - s - UPKFOFF(v);  /* amount to shift */
#endif

		/* make & mask part */

		p->left->type = ty;

		p->op = AND;
		p->right = talloc();
		p->right->op = ICON;
		p->right->rall = NOPREF;
		p->right->type = ty;
		p->right->lval = 1;
		p->right->rval = 0;
		p->right->name[0] = '\0';
		p->right->lval <<= s;
		p->right->lval--;

		/* now, if a shift is needed, do it */

		if( o != 0 ){
			shp = talloc();
			shp->op = RS;
			shp->rall = NOPREF;
			shp->type = ty;
			shp->left = p->left;
			shp->right = talloc();
			shp->right->op = ICON;
			shp->right->rall = NOPREF;
			shp->right->type = ty;
			shp->right->rval = 0;
			shp->right->lval = o;  /* amount to shift */
			shp->right->name[0] = '\0';
			p->left = shp;
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

	if( p->op == UNARY MUL ){
		q = p->left;
		if( q->op == REG ){
			temp = q->lval;
			r = q->rval;
			cp = q->name;
			goto ormake;
			}

		if( q->op != PLUS && q->op != MINUS ) return;
		ql = q->left;
		qr = q->right;

#ifdef R2REGS

		/* look for doubly indexed expressions */

		if( q->op == PLUS) {
			if( (r=base(ql))>=0 && (i=offset(qr, tlen(p)))>=0) {
				makeor2(p, ql, r, i);
				return;
			} else if( (r=base(qr))>=0 && (i=offset(ql, tlen(p)))>=0) {
				makeor2(p, qr, r, i);
				return;
				}
			}


#endif

		if( (q->op==PLUS || q->op==MINUS) && qr->op == ICON &&
				ql->op==REG && szty(qr->type)==1) {
			temp = qr->lval;
			if( q->op == MINUS ) temp = -temp;
			r = ql->rval;
			temp += ql->lval;
			cp = qr->name;
			if( *cp && ( q->op == MINUS || *ql->name ) ) return;
			if( !*cp ) cp = ql->name;

			ormake:
			if( notoff( p->type, r, temp, cp ) ) return;
			p->op = OREG;
			p->rval = r;
			p->lval = temp;
			for( i=0; i<NCHNAM; ++i )
				p->name[i] = *cp++;
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

