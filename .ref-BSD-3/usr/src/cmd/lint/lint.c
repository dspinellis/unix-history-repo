# include "mfile1"

# include "lmanifest"

# include <ctype.h>

# define VAL 0
# define EFF 1

/* these are appropriate for the -p flag */
int  SZCHAR = 8;
int  SZINT = 16;
int  SZFLOAT = 32;
int  SZDOUBLE = 64;
int  SZLONG = 32;
int  SZSHORT = 16;
int SZPOINT = 16;
int ALCHAR = 8;
int ALINT = 16;
int ALFLOAT = 32;
int ALDOUBLE = 64;
int ALLONG = 32;
int ALSHORT = 16;
int ALPOINT = 16;
int ALSTRUCT = 16;

int vflag = 1;  /* tell about unused argments */
int xflag = 0;  /* tell about unused externals */
int argflag = 0;  /* used to turn off complaints about arguments */
int libflag = 0;  /* used to generate library descriptions */
int vaflag = -1;  /* used to signal functions with a variable number of args */
int aflag = 0;  /* used th check precision of assignments */

char *flabel = "xxx";

# define LNAMES 100

struct lnm {
	short lid, flgs;
	}  lnames[LNAMES], *lnp;

contx( p, down, pl, pr ) register NODE *p; register *pl, *pr; {

	*pl = *pr = VAL;
	switch( p->op ){

	case ANDAND:
	case OROR:
	case QUEST:
		*pr = down;
		break;

	case SCONV:
	case PCONV:
	case COLON:
		*pr = *pl = down;
		break;

	case COMOP:
		*pl = EFF;
		*pr = down;

	case FORCE:
	case INIT:
	case UNARY CALL:
	case STCALL:
	case UNARY STCALL:
	case CALL:
	case UNARY FORTCALL:
	case FORTCALL:
	case CBRANCH:
		break;

	default:
		if( asgop(p->op) ) break;
		if( p->op == UNARY MUL && ( p->type == STRTY || p->type == UNIONTY) ) {
			break;  /* the compiler does this... */
			}
		if( down == EFF && hflag ) werror( "null effect" );

		}
	}

ecode( p ) NODE *p; {
	/* compile code for p */

	fwalk( p, contx, EFF );
	lnp = lnames;
	lprt( p, EFF, 0 );
	}

ejobcode( flag ){
	/* called after processing each job */
	/* flag is nonzero if errors were detected */
	register k;
	register struct symtab *p;

	for( p=stab; p< &stab[SYMTSZ]; ++p ){

		if( p->stype != TNULL ) {

			if( p->stype == STRTY || p->stype == UNIONTY ){
				if( dimtab[p->sizoff+1] < 0 ){ /* never defined */
					if( hflag ) werror( "struct/union %.7s never defined", p->sname );
					}
				}

			switch( p->sclass ){
			
			case STATIC:
				if( p->suse > 0 ){
					k = lineno;
					lineno = p->suse;
					uerror( "static variable %s unused",
						p->sname );
					lineno = k;
					break;
					}

			case EXTERN:
			case USTATIC:
				/* with the xflag, worry about externs not used */
				/* the filename may be wrong here... */
				if( xflag && p->suse >= 0 && !libflag ){
					printf( "%.7s\t%03d\t%o\t%d\t", p->sname, LDX, p->stype, 0 );
					/* we don't really know the file number; we know only the line 
						number, so we put only that out */
					printf( "\"???\"\t%d\t%s\n", p->suse, flabel );
					}
			
			case EXTDEF:
				if( p->suse < 0 ){  /* used */
					printf( "%.7s\t%03d\t%o\t%d\t", exname(p->sname), LUM, p->stype, 0 );
					fident( -p->suse );
					}
				break;
				}
			
			}

		}
	exit( 0 );
	}

fident( line ){ /* like ident, but lineno = line */
	register temp;
	temp = lineno;
	lineno = line;
	ident();
	lineno = temp;
	}

ident(){ /* write out file and line identification  */
	printf( "%s\t%d\t%s\n", ftitle, lineno, flabel );
	}

bfcode( a, n ) int a[]; {
	/* code for the beginning of a function; a is an array of
		indices in stab for the arguments; n is the number */
	/* this must also set retlab */
	register i;
	register struct symtab *cfp;
	register unsigned t;

	retlab = 1;
	cfp = &stab[curftn];

	/* if variable number of arguments, only print the ones which will be checked */
	if( vaflag > 0 ){
		if( n < vaflag ) werror( "declare the VARARGS arguments you want checked!" );
		else n = vaflag;
		}
	printf( "%.7s\t%03d\t%o\t%d\t", exname(cfp->sname), libflag?LIB:LDI,
		cfp->stype, vaflag>=0?-n:n );
	vaflag = -1;

	for( i=0; i<n; ++i ) {
		switch( t = stab[a[i]].stype ){

		case ULONG:
			break;

		case CHAR:
		case SHORT:
			t = INT;
			break;

		case UCHAR:
		case USHORT:
		case UNSIGNED:
			t = UNSIGNED;
			break;

			}

		printf( "%o\t", t );
		}
	ident();
	}

ctargs( p ) NODE *p; {
	/* count arguments; p points to at least one */
	/* the arguemnts are a tower of commasto the left */
	register c;
	c = 1; /* count the rhs */
	while( p->op == CM ){
		++c;
		p = p->left;
		}
	return( c );
	}

lpta( p ) NODE *p; {
	TWORD t;

	if( p->op == CM ){
		lpta( p->left );
		p = p->right;
		}
	switch( t = p->type ){

		case CHAR:
		case SHORT:
			t = INT;
		case LONG:
		case ULONG:
		case INT:
		case UNSIGNED:
			break;

		case UCHAR:
		case USHORT:
			t = UNSIGNED;
			break;

		case FLOAT:
			printf( "%o\t", DOUBLE );
			return;

		default:
			printf( "%o\t", p->type );
			return;
		}

	if( p->op == ICON ) printf( "%o<1\t", t );
	else printf( "%o\t", t );
	}

# define VALSET 1
# define VALUSED 2
# define VALASGOP 4
# define VALADDR 8

lprt( p, down, uses ) register NODE *p; {
	register struct symtab *q;
	register id;
	register acount;
	register down1, down2;
	register use1, use2;
	register struct lnm *np1, *np2;

	/* first, set variables which are set... */

	use1 = use2 = VALUSED;
	if( p->op == ASSIGN ) use1 = VALSET;
	else if( p->op == UNARY AND ) use1 = VALADDR;
	else if( asgop( p->op ) ){ /* =ops */
		use1 = VALUSED|VALSET;
		if( down == EFF ) use1 |= VALASGOP;
		}


	/* print the lines for lint */

	down2 = down1 = VAL;
	acount = 0;

	switch( p->op ){

	case EQ:
	case NE:
	case GT:
	case GE:
	case LT:
	case LE:
		if( p->left->type == CHAR && p->right->op==ICON && p->right->lval < 0 ){
			werror( "nonportable character comparison" );
			}
		if( (p->op==EQ || p->op==NE ) && ISUNSIGNED(p->left->type) && p->right->op == ICON ){
			if( p->right->lval < 0 && p->right->rval == NONAME && !ISUNSIGNED(p->right->type) ){
				werror( "comparison of unsigned with negative constant" );
				}
			}
		break;

	case UGE:
	case ULT:
		if( p->right->op == ICON && p->right->lval == 0 && p->right->rval == NONAME ){
			werror( "unsigned comparison with 0?" );
			break;
			}
	case UGT:
	case ULE:
		if( p->right->op == ICON && p->right->lval <= 0 && !ISUNSIGNED(p->right->type) && p->right->rval == NONAME ){
			werror( "degenerate unsigned comparison" );
			}
		break;

	case COMOP:
		down1 = EFF;

	case ANDAND:
	case OROR:
	case QUEST:
		down2 = down;
		/* go recursively left, then right  */
		np1 = lnp;
		lprt( p->left, down1, use1 );
		np2 = lnp;
		lprt( p->right, down2, use2 );
		lmerge( np1, np2, 0 );
		return;

	case SCONV:
	case PCONV:
	case COLON:
		down1 = down2 = down;
		break;

	case CALL:
	case STCALL:
	case FORTCALL:
		acount = ctargs( p->right );
	case UNARY CALL:
	case UNARY STCALL:
	case UNARY FORTCALL:
		if( p->left->op == ICON && (id=p->left->rval) != NONAME ){ /* used to be &name */
			printf( "%.7s\t%03d\t%o\t%d\t",
				exname(stab[id].sname),
				down==EFF ? LUE : LUV,
				DECREF(p->left->type), acount );
			if( acount ) lpta( p->right );
			ident();
			}
		break;

	case ICON:
		/* look for &name case */
		if( (id = p->rval) >= 0 && id != NONAME ){
			q = &stab[id];
			q->sflags |= (SREF|SSET);
			}
		return;

	case NAME:
		if( (id = p->rval) >= 0 && id != NONAME ){
			q = &stab[id];
			if( (uses&VALUSED) && !(q->sflags&SSET) ){
				if( q->sclass == AUTO || q->sclass == REGISTER ){
					if( !ISARY(q->stype ) && !ISFTN(q->stype) && q->stype!=STRTY ){
						werror( "%.7s may be used before set", q->sname );
						q->sflags |= SSET;
						}
					}
				}
			if( uses & VALASGOP ) break;  /* not a real use */
			if( uses & VALSET ) q->sflags |= SSET;
			if( uses & VALUSED ) q->sflags |= SREF;
			if( uses & VALADDR ) q->sflags |= (SREF|SSET);
			if( p->lval == 0 ){
				lnp->lid = id;
				lnp->flgs = (uses&VALADDR)?0:((uses&VALSET)?VALSET:VALUSED);
				if( ++lnp >= &lnames[LNAMES] ) --lnp;
				}
			}
		return;

		}

	/* recurse, going down the right side first if we can */

	switch( optype(p->op) ){

	case BITYPE:
		np1 = lnp;
		lprt( p->right, down2, use2 );
	case UTYPE:
		np2 = lnp;
		lprt( p->left, down1, use1 );
		}

	if( optype(p->op) == BITYPE ){
		if( p->op == ASSIGN && p->left->op == NAME ){ /* special case for a =  .. a .. */
			lmerge( np1, np2, 0 );
			}
		else lmerge( np1, np2, p->op != COLON );
		/* look for assignments to fields, and complain */
		if( p->op == ASSIGN && p->left->op == FLD && p->right->op == ICON ) fldcon( p );
		}

	}

lmerge( np1, np2, flag ) struct lnm *np1, *np2; {
	/* np1 and np2 point to lists of lnm members, for the two sides
	 * of a binary operator
	 * flag is 1 if commutation is possible, 0 otherwise
	 * lmerge returns a merged list, starting at np1, resetting lnp
	 * it also complains, if appropriate, about side effects
	 */

	register struct lnm *npx, *npy;

	for( npx = np2; npx < lnp; ++npx ){

		/* is it already there? */
		for( npy = np1; npy < np2; ++npy ){
			if( npx->lid == npy->lid ){ /* yes */
				if( npx->flgs == 0 || npx->flgs == (VALSET|VALUSED) )
					;  /* do nothing */
				else if( (npx->flgs|npy->flgs)== (VALSET|VALUSED) ||
					(npx->flgs&npy->flgs&VALSET) ){
					if( flag ) werror( "%.8s evaluation order undefined", stab[npy->lid].sname );
					}
				if( npy->flgs == 0 ) npx->flgs = 0;
				else npy->flgs |= npx->flgs;
				goto foundit;
				}
			}

		/* not there: update entry */
		np2->lid = npx->lid;
		np2->flgs = npx->flgs;
		++np2;

		foundit: ;
		}

	/* all finished: merged list is at np1 */
	lnp = np2;
	}

efcode(){
	/* code for the end of a function */
	register struct symtab *cfp;

	cfp = &stab[curftn];
	if( retstat & RETVAL ){
		printf( "%.7s\t%03d\t%o\t%d\t", exname(cfp->sname),
			LRV, DECREF( cfp->stype), 0 );
		ident();
		}
	if( !vflag ){
		vflag = argflag;
		argflag = 0;
		}
	if( retstat == RETVAL+NRETVAL )
		werror( "function %.8s has return(e); and return;", cfp->sname);
	}

aocode(p) struct symtab *p; {
	/* called when automatic p removed from stab */
	register struct symtab *cfs;
	cfs = &stab[curftn];
	if(p->suse>0 && !(p->sflags&SMOS) ){
		if( p->sclass == PARAM ){
			if( vflag ) werror( "argument %.7s unused in function %.7s",
				p->sname,
				cfs->sname );
			}
		else {
			if( p->sclass != TYPEDEF ) werror( "%.7s unused in function %.7s",
				p->sname, cfs->sname );
			}
		}

	if( p->suse < 0 && (p->sflags & (SSET|SREF|SMOS)) == SSET &&
		!ISARY(p->stype) && !ISFTN(p->stype) ){

		werror( "%.7s set but not used in function %.7s", p->sname, cfs->sname );
		}

	if( p->stype == STRTY || p->stype == UNIONTY || p->stype == ENUMTY ){
		if( dimtab[p->sizoff+1] < 0 ) werror( "structure %.7s never defined", p->sname );
		}

	}

defnam( p ) register struct symtab *p; {
	/* define the current location as the name p->sname */

	if( p->sclass == STATIC && p->slevel>1 ) return;

	if( !ISFTN( p->stype ) ){
		printf( "%.7s\t%03d\t%o\t%d\t",
			exname(p->sname), libflag?LIB:LDI, p->stype, 0 );
		ident();
		}
	}

zecode( n ){
	/* n integer words of zeros */
	OFFSZ temp;
	temp = n;
	inoff += temp*SZINT;
	;
	}

andable( p ) NODE *p; {  /* p is a NAME node; can it accept & ? */
	register r;

	if( p->op != NAME ) cerror( "andable error" );

	if( (r = p->rval) < 0 ) return(1);  /* labels are andable */

	if( stab[r].sclass == AUTO || stab[r].sclass == PARAM ) return(0); 
	return(1);
	}

NODE *
clocal(p) NODE *p; {

	/* this is called to do local transformations on
	   an expression tree preparitory to its being
	   written out in intermediate code.
	*/

	/* the major essential job is rewriting the
	   automatic variables and arguments in terms of
	   REG and OREG nodes */
	/* conversion ops which are not necessary are also clobbered here */
	/* in addition, any special features (such as rewriting
	   exclusive or) are easily handled here as well */

	register o;
	register unsigned t, tl;

	switch( o = p->op ){

	case SCONV:
	case PCONV:
		if( p->left->type==ENUMTY ){
			p->left = pconvert( p->left );
			}
		/* assume conversion takes place; type is inherited */
		t = p->type;
		tl = p->left->type;
		if( aflag && (tl==LONG||tl==ULONG) && (t!=LONG&&t!=ULONG) ){
			werror( "long assignment may lose accuracy" );
			}
		if( ISPTR(tl) && ISPTR(t) ){
			tl = DECREF(tl);
			t = DECREF(t);
			switch( ISFTN(t) + ISFTN(tl) ){

			case 0:  /* neither is a function pointer */
				if( talign(t,p->csiz) > talign(tl,p->left->csiz) ){
					if( hflag||pflag ) werror( "possible pointer alignment problem" );
					}
				break;

			case 1:
				werror( "questionable conversion of function pointer" );

			case 2:
				;
				}
			}
		p->left->type = p->type;
		p->left->cdim = p->cdim;
		p->left->csiz = p->csiz;
		p->op = FREE;
		return( p->left );

	case PVCONV:
	case PMCONV:
		if( p->right->op != ICON ) cerror( "bad conversion");
		p->op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->left, p->right ) );

		}

	return(p);
	}

NODE *
offcon( off, t, d, s ) OFFSZ off; TWORD t;{  /* make a structure offset node */
	register NODE *p;
	p = bcon(0);
	p->lval = off/SZCHAR;
	return(p);
	}

noinit(){
	/* storage class for such as "int a;" */
	return( pflag ? EXTDEF : EXTERN );
	}


cinit( p, sz ) NODE *p; { /* initialize p into size sz */
	inoff += sz;
	if( p->op == INIT ){
		if( p->left->op == ICON ) return;
		if( p->left->op == NAME && p->left->type == MOE ) return;
		}
	uerror( "illegal initialization" );
	}

char *
exname( p ) char *p; {
	/* make a name look like an external name in the local machine */
	static char aa[8];
	register int i;

	if( !pflag ) return(p);
	for( i=0; i<6; ++i ){
		if( isupper(*p ) ) aa[i] = tolower( *p );
		else aa[i] = *p;
		if( *p ) ++p;
		}
	aa[6] = '\0';
	return( aa );
	}

where(f){ /* print true location of error */
	if( f == 'u' && nerrors>1 ) --nerrors; /* don't get "too many errors" */
	fprintf( stderr, "%s, line %d: ", ftitle, lineno );
	}

	/* a number of dummy routines, unneeded by lint */

branch(n){;}
defalign(n){;}
deflab(n){;}
bycode(t,i){;}
cisreg(t){return(1);}  /* everyting is a register variable! */

fldty(p) struct symtab *p; {
	; /* all types are OK here... */
	}

fldal(t) unsigned t; { /* field alignment... */
	if( t == ENUMTY ) return( ALCHAR );  /* this should be thought through better... */
	if( ISPTR(t) ){ /* really for the benefit of honeywell (and someday IBM) */
		if( pflag ) uerror( "nonportable field type" );
		}
	else uerror( "illegal field type" );
	return(ALINT);
	}

main( argc, argv ) char *argv[]; {
	char *p;

	/* handle options */

	for( p=argv[1]; *p; ++p ){

		switch( *p ){

		case '-':
			continue;

		case 'L':  /* produced by driver program */
			flabel = p;
			goto break2;

		case '\0':
			break;

		case 'b':
			brkflag = 1;
			continue;

		case 'p':
			pflag = 1;
			continue;

		case 'c':
			cflag = 1;
			continue;

		case 's':
			/* for the moment, -s triggers -h */

		case 'h':
			hflag = 1;
			continue;

		case 'v':
			vflag = 0;
			continue;

		case 'x':
			xflag = 1;
			continue;

		case 'a':
			aflag = 1;
		case 'u':	/* done in second pass */
		case 'n':	/* done in shell script */
			continue;

		case 't':
			werror( "option %c now default: see `man 6 lint'", *p );
			continue;

		default:
			uerror( "illegal option: %c", *p );
			continue;

			}
		}

	break2:
	if( !pflag ){  /* set sizes to sizes of target machine */
# ifdef gcos
		SZCHAR = ALCHAR = 9;
# else
		SZCHAR = ALCHAR = 8;
# endif
		SZINT = ALINT = sizeof(int)*SZCHAR;
		SZFLOAT = ALFLOAT = sizeof(float)*SZCHAR;
		SZDOUBLE = ALDOUBLE = sizeof(double)*SZCHAR;
		SZLONG = ALLONG = sizeof(long)*SZCHAR;
		SZSHORT = ALSHORT = sizeof(short)*SZCHAR;
		SZPOINT = ALPOINT = sizeof(int *)*SZCHAR;
		ALSTRUCT = ALINT;
		/* now, fix some things up for various machines (I wish we had "alignof") */

# ifdef pdp11
		ALLONG = ALDOUBLE = ALFLOAT = ALINT;
#endif
# ifdef ibm
		ALSTRUCT = ALCHAR;
#endif
		}

	return( mainp1( argc, argv ) );
	}

ctype( type ) unsigned type; { /* are there any funny types? */
	return( type );
	}

commdec( i ){
	/* put out a common declaration */
	register struct symtab *p;
	p = &stab[i];
	printf( "%.7s\t%03d\t%o\t%d\t", exname(p->sname), libflag?LIB:LDC, p->stype, 0 );
	ident();
	}

isitfloat ( s ) char *s; {
	/* s is a character string;
	   if floating point is implemented, set dcon to the value of s */
	/* lint version
	*/
	dcon = atof( s );
	return( FCON );
	}

fldcon( p ) register NODE *p; {
	/* p is an assignment of a constant to a field */
	/* check to see if the assignment is going to overflow, or otherwise cause trouble */
	register s;
	CONSZ v;

	if( !hflag & !pflag ) return;

	s = UPKFSZ(p->left->rval);
	v = p->right->lval;

	switch( p->left->type ){

	case CHAR:
	case INT:
	case SHORT:
	case LONG:
	case ENUMTY:
		if( v>=0 && (v>>(s-1))==0 ) return;
		werror( "precision lost in assignment to (possibly sign-extended) field" );
	default:
		return;

	case UNSIGNED:
	case UCHAR:
	case USHORT:
	case ULONG:
		if( v<0 || (v>>s)!=0 ) werror( "precision lost in field assignment" );
		
		return;
		}

	}
