#ifndef lint
static char sccsid[] = "@(#)lint.c	1.14	(Berkeley)	%G%";
#endif lint

# include "pass1.h"

# include "lmanifest.h"

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

int nflag = 0;		/* avoid gripes about printf et al. */
int vflag = 1;		/* tell about unused argments */
int xflag = 0;		/* tell about unused externals */
int argflag = 0;	/* used to turn off complaints about arguments */
int libflag = 0;	/* used to generate library descriptions */
int vaflag = -1;	/* signal functions with a variable number of args */
int aflag = 0;		/* used to check precision of assignments */
int zflag = 0;		/* no 'structure never defined' error */
int Cflag = 0;	/* filter out certain output, for generating libraries */
char *libname = 0;	/* name of the library we're generating */

			/* flags for the "outdef" function */
# define USUAL (-101)
# define DECTY (-102)
# define NOFILE (-103)
# define SVLINE (-104)

# define LNAMES 250

struct lnm {
	short lid, flgs;
	}  lnames[LNAMES], *lnp;

contx( p, down, pl, pr ) register NODE *p; register *pl, *pr; {

	*pl = *pr = VAL;
	switch( p->in.op ){

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
		if( asgop(p->in.op) ) break;
		if( p->in.op == UNARY MUL && ( p->in.type == STRTY || p->in.type == UNIONTY || p->in.type == UNDEF) ) {
		/* struct x f( );  main( ) {  (void) f( ); }
		 * the the cast call appears as U* UNDEF
		 */
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
	strforget();
	}

ejobcode( flag ){
	/* called after processing each job */
	/* flag is nonzero if errors were detected */
	register k;
	register struct symtab *p;

	for( p=stab; p< &stab[SYMTSZ]; ++p ){

		if( p->stype != TNULL ) {

			if( p->stype == STRTY || p->stype == UNIONTY ){
				if( !zflag && dimtab[p->sizoff+1] < 0 ){
					/* never defined */
#ifndef FLEXNAMES
					if( hflag ) werror( "struct/union %.8s never defined", p->sname );
#else
					if( hflag ) werror( "struct/union %s never defined", p->sname );
#endif
					}
				}

			switch( p->sclass ){
			
			case STATIC:
				if( p->suse > 0 ){
					k = lineno;
					lineno = p->suse;
#ifndef FLEXNAMES
					uerror( "static variable %.8s unused",
#else
					uerror( "static variable %s unused",
#endif
						p->sname );
					lineno = k;
					break;
					}
				/* no statics in libraries */
				if( Cflag ) break;

			case EXTERN:
			case USTATIC:
				/* with the xflag, worry about externs not used */
				/* the filename may be wrong here... */
				if( xflag && p->suse >= 0 && !libflag ){
					outdef( p, LDX, NOFILE );
					}
			
			case EXTDEF:
				if( p->suse < 0 ){  /* used */
					outdef( p, LUM, SVLINE );
					}
				break;
				}
			
			}

		}
	exit( 0 );
	}

astype( t, i ) ATYPE *t; {
	TWORD tt;
	int j, k=0, l=0;

	if( (tt=BTYPE(t->aty))==STRTY || tt==UNIONTY ){
		if( i<0 || i>= DIMTABSZ-3 ){
			werror( "lint's little mind is blown" );
			}
		else {
			j = dimtab[i+3];
			if( j<0 || j>SYMTSZ ){
				k = dimtab[i];
				l = X_NONAME | stab[j].suse;
				}
			else {
				if( stab[j].suse <= 0 ) {
#ifndef FLEXNAMES
					werror( "no line number for %.8s",
#else
					werror( "no line number for %s",
#endif
						stab[j].sname );
					}
				else {
					k = dimtab[i];
#ifdef FLEXNAMES
					l = hashstr(stab[j].sname);
#else
					l = hashstr(stab[j].sname, LCHNM);
#endif
					}
				}
			}
		
		t->extra = k;
		t->extra1 = l;
		return( 1 );
		}
	else return( 0 );
	}

bfcode( a, n ) int a[]; {
	/* code for the beginning of a function; a is an array of
		indices in stab for the arguments; n is the number */
	/* this must also set retlab */
	register i;
	register struct symtab *cfp;
	static ATYPE t;

	strforget();
	retlab = 1;

	cfp = &stab[curftn];

	/* if creating library, don't do static functions */
	if( Cflag && cfp->sclass == STATIC ) return;

	/* if variable number of arguments, only print the ones which will be checked */
	if( vaflag >= 0 ){
		if( n < vaflag ) werror( "declare the VARARGS arguments you want checked!" );
		else n = vaflag;
		}
	fsave( ftitle );
	if( cfp->sclass == STATIC ) outdef( cfp, LST, vaflag>=0?~n:n );
	else outdef( cfp, libflag?LIB:LDI, vaflag>=0?~n:n );
	vaflag = -1;

	/* output the arguments */
	if( n ){
		for( i=0; i<n; ++i ) {
			t.aty = stab[a[i]].stype;
			t.extra = 0;
			t.extra1 = 0;
			if( !astype( &t, stab[a[i]].sizoff ) ) {
				switch( t.aty ){

				case ULONG:
					break;

				case CHAR:
				case SHORT:
					t.aty = INT;
					break;

				case UCHAR:
				case USHORT:
				case UNSIGNED:
					t.aty = UNSIGNED;
					break;

					}
				}
			fwrite( (char *)&t, sizeof(ATYPE), 1, stdout );
			}
		}
	}

ctargs( p ) NODE *p; {
	/* count arguments; p points to at least one */
	/* the arguemnts are a tower of commas to the left */
	register c;
	c = 1; /* count the rhs */
	while( p->in.op == CM ){
		++c;
		p = p->in.left;
		}
	return( c );
	}

lpta( p ) NODE *p; {
	static ATYPE t;

	if( p->in.op == CM ){
		lpta( p->in.left );
		p = p->in.right;
		}

	t.aty = p->in.type;
	t.extra = (p->in.op==ICON);
	t.extra1 = 0;

	if( !astype( &t, p->fn.csiz ) ) {
		switch( t.aty ){

			case CHAR:
			case SHORT:
				t.aty = INT;
			case LONG:
			case ULONG:
			case INT:
			case UNSIGNED:
				break;

			case UCHAR:
			case USHORT:
				t.aty = UNSIGNED;
				break;

			case FLOAT:
				t.aty = DOUBLE;
				t.extra = 0;
				break;

			default:
				t.extra = 0;
				break;
			}
		}
	fwrite( (char *)&t, sizeof(ATYPE), 1, stdout );
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
	if( p->in.op == ASSIGN ) use1 = VALSET;
	else if( p->in.op == UNARY AND ) use1 = VALADDR;
	else if( asgop( p->in.op ) ){ /* =ops */
		use1 = VALUSED|VALSET;
		if( down == EFF ) use1 |= VALASGOP;
		}


	/* print the lines for lint */

	down2 = down1 = VAL;
	acount = 0;

	switch( p->in.op ){

	case EQ:
	case NE:
	case GT:
	case GE:
	case LT:
	case LE:
		if( p->in.left->in.type == CHAR && p->in.right->in.op==ICON && p->in.right->tn.lval < 0 ){
			werror( "nonportable character comparison" );
			}
		if( (p->in.op==EQ || p->in.op==NE ) && ISUNSIGNED(p->in.left->in.type) && p->in.right->in.op == ICON ){
			if( p->in.right->tn.lval < 0 && p->in.right->tn.rval == NONAME && !ISUNSIGNED(p->in.right->in.type) ){
				werror( "comparison of unsigned with negative constant" );
				}
			}
		break;

	case UGE:
	case ULT:
		if( p->in.right->in.op == ICON && p->in.right->tn.lval == 0 && p->in.right->tn.rval == NONAME ){
			werror( "unsigned comparison with 0?" );
			break;
			}
	case UGT:
	case ULE:
		if( p->in.right->in.op == ICON && p->in.right->tn.lval <= 0 && !ISUNSIGNED(p->in.right->in.type) && p->in.right->tn.rval == NONAME ){
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
		lprt( p->in.left, down1, use1 );
		np2 = lnp;
		lprt( p->in.right, down2, use2 );
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
		acount = ctargs( p->in.right );
	case UNARY CALL:
	case UNARY STCALL:
	case UNARY FORTCALL:
		if( p->in.left->in.op == ICON && (id=p->in.left->tn.rval) != NONAME ){ /* used to be &name */
			struct symtab *sp = &stab[id];
			int lty;

			fsave( ftitle );
			if (!nflag)
				doform(p, sp, acount);
			/*
			 * if we're generating a library -C then
			 * we don't want to output references to functions
			 */
			if( Cflag ) break;
			/*  if a function used in an effects context is
			 *  cast to type  void  then consider its value
			 *  to have been disposed of properly
			 *  thus a call of type  undef  in an effects
			 *  context is construed to be used in a value
			 *  context
			 */
			if ((down == EFF) && (p->in.type != UNDEF)) {
				lty = LUE;
			} else if (down == EFF) {
				lty = LUV | LUE;
			} else {
				lty = LUV;
			}
			outdef( sp, lty, acount );
			if( acount ) {
				lpta( p->in.right );
				}
			}
		break;

	case ICON:
		/* look for &name case */
		if( (id = p->tn.rval) >= 0 && id != NONAME ){
			q = &stab[id];
			q->sflags |= (SREF|SSET);
			q->suse = -lineno;
			}
		return;

	case NAME:
		if( (id = p->tn.rval) >= 0 && id != NONAME ){
			q = &stab[id];
			if( (uses&VALUSED) && !(q->sflags&SSET) ){
				if( q->sclass == AUTO || q->sclass == REGISTER ){
					if( !ISARY(q->stype ) && !ISFTN(q->stype) && q->stype!=STRTY && q->stype!=UNIONTY ){
#ifndef FLEXNAMES
						werror( "%.8s may be used before set", q->sname );
#else
						werror( "%s may be used before set", q->sname );
#endif
						q->sflags |= SSET;
						}
					}
				}
			if( uses & VALASGOP ) break;  /* not a real use */
			if( uses & VALSET ) q->sflags |= SSET;
			if( uses & VALUSED ) q->sflags |= SREF;
			if( uses & VALADDR ) q->sflags |= (SREF|SSET);
			if( p->tn.lval == 0 ){
				lnp->lid = id;
				lnp->flgs = (uses&VALADDR)?0:((uses&VALSET)?VALSET:VALUSED);
				if( ++lnp >= &lnames[LNAMES] ) --lnp;
				}
			}
		return;

		}

	/* recurse, going down the right side first if we can */

	switch( optype(p->in.op) ){

	case BITYPE:
		np1 = lnp;
		lprt( p->in.right, down2, use2 );
	case UTYPE:
		np2 = lnp;
		lprt( p->in.left, down1, use1 );
		}

	if( optype(p->in.op) == BITYPE ){
		if( p->in.op == ASSIGN && p->in.left->in.op == NAME ){ /* special case for a =  .. a .. */
			lmerge( np1, np2, 0 );
			}
		else lmerge( np1, np2, p->in.op != COLON );
		/* look for assignments to fields, and complain */
		if( p->in.op == ASSIGN && p->in.left->in.op == FLD && p->in.right->in.op == ICON ) fldcon( p );
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
#ifndef FLEXNAMES
					if( flag ) werror( "%.8s evaluation order undefined", stab[npy->lid].sname );
#else
					if( flag ) werror( "%s evaluation order undefined", stab[npy->lid].sname );
#endif
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
	if( retstat & RETVAL && !(Cflag && cfp->sclass==STATIC) )
		outdef( cfp, LRV, DECTY );
	if( !vflag ){
		vflag = argflag;
		argflag = 0;
		}
	if( retstat == RETVAL+NRETVAL )
#ifndef FLEXNAMES
		werror( "function %.8s has return(e); and return;", cfp->sname);
#else
		werror( "function %s has return(e); and return;", cfp->sname);
#endif
	}

aocode(p) struct symtab *p; {
	/* called when automatic p removed from stab */
	register struct symtab *cfs;
	cfs = &stab[curftn];
	if(p->suse>0 && !(p->sflags&(SMOS|STAG)) ){
		if( p->sclass == PARAM ){
#ifndef FLEXNAMES
			if( vflag ) werror( "argument %.8s unused in function %.8s",
#else
			if( vflag ) werror( "argument %s unused in function %s",
#endif
				p->sname,
				cfs->sname );
			}
		else {
#ifndef FLEXNAMES
			if( p->sclass != TYPEDEF ) werror( "%.8s unused in function %.8s",
#else
			if( p->sclass != TYPEDEF ) werror( "%s unused in function %s",
#endif
				p->sname, cfs->sname );
			}
		}

	if( p->suse < 0 && (p->sflags & (SSET|SREF|SMOS)) == SSET &&
		!ISARY(p->stype) && !ISFTN(p->stype) ){

#ifndef FLEXNAMES
		werror( "%.8s set but not used in function %.8s", p->sname, cfs->sname );
#else
		werror( "%s set but not used in function %s", p->sname, cfs->sname );
#endif
		}

	if( p->stype == STRTY || p->stype == UNIONTY || p->stype == ENUMTY ){
		if( !zflag && dimtab[p->sizoff+1] < 0 )
#ifndef FLEXNAMES
			werror( "structure %.8s never defined", p->sname );
#else
			werror( "structure %s never defined", p->sname );
#endif
		}

	}

defnam( p ) register struct symtab *p; {
	/* define the current location as the name p->sname */

	if( p->sclass == STATIC && (p->slevel>1 || Cflag) ) return;

	if( !ISFTN( p->stype ) )
		if( p->sclass == STATIC ) outdef( p, LST, USUAL );
		else outdef( p, libflag?LIB:LDI, USUAL );
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

	if( p->in.op != NAME ) cerror( "andable error" );

	if( (r = p->tn.rval) < 0 ) return(1);  /* labels are andable */

	if( stab[r].sclass == AUTO || stab[r].sclass == PARAM ) return(0); 
#ifndef FLEXNAMES
	if( stab[r].sclass == REGISTER ) uerror( "can't take & of %.8s", stab[r].sname );
#else
	if( stab[r].sclass == REGISTER ) uerror( "can't take & of %s", stab[r].sname );
#endif
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
	int s;

	switch( o = p->in.op ){
	case NAME:
		{
			extern int	didstr, subscr;
			extern NODE *	strnodes[];

			if (didstr) {
				didstr = 0;
				strnodes[subscr] = p;
			}
		}
		break;

	case SCONV:
	case PCONV:
		if( p->in.left->in.type==ENUMTY ){
			p->in.left = pconvert( p->in.left );
			}
		/* assume conversion takes place; type is inherited */
		t = p->in.type;
		tl = p->in.left->in.type;
		if( aflag && (tl==LONG||tl==ULONG) && (t!=LONG&&t!=ULONG&&t!=UNDEF) ){
			werror( "long assignment may lose accuracy" );
			}
		if( aflag>=2 && (tl!=LONG&&tl!=ULONG) && (t==LONG||t==ULONG) && p->in.left->in.op != ICON ){
			werror( "assignment to long may sign-extend incorrectly" );
			}
		if( ISPTR(tl) && ISPTR(t) ){
			tl = DECREF(tl);
			t = DECREF(t);
			switch( ISFTN(t) + ISFTN(tl) ){

			case 0:  /* neither is a function pointer */
				if( talign(t,p->fn.csiz) > talign(tl,p->in.left->fn.csiz) ){
					if( hflag||pflag ) werror( "possible pointer alignment problem" );
					}
				break;

			case 1:
				werror( "questionable conversion of function pointer" );

			case 2:
				;
				}
			}
		p->in.left->in.type = p->in.type;
		p->in.left->fn.cdim = p->fn.cdim;
		p->in.left->fn.csiz = p->fn.csiz;
		p->in.op = FREE;
		return( p->in.left );

	case PVCONV:
	case PMCONV:
		if( p->in.right->in.op != ICON ) cerror( "bad conversion");
		p->in.op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->in.left, p->in.right ) );

	case RS:
	case LS:
	case ASG RS:
	case ASG LS:
		if( p->in.right->in.op != ICON )
			break;
		s = p->in.right->tn.lval;
		if( s < 0 )
			werror( "negative shift" );
		else
		if( s >= dimtab[ p->fn.csiz ] )
			werror( "shift greater than size of object" );
		break;

		}

	return(p);
	}

NODE *
offcon( off, t, d, s ) OFFSZ off; TWORD t;{  /* make a structure offset node */
	register NODE *p;
	p = bcon(0);
	p->tn.lval = off/SZCHAR;
	return(p);
	}

noinit(){
	/* storage class for such as "int a;" */
	return( pflag ? EXTDEF : EXTERN );
	}


cinit( p, sz ) NODE *p; { /* initialize p into size sz */
	register int id;

	inoff += sz;
	if( p->in.op == INIT ){
		if( p->in.left->in.op == ICON ) return;
		if( p->in.left->in.op == NAME && p->in.left->in.type == MOE ) return;
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

char *
strip(s) char *s; {
#ifndef FLEXNAMES
	static char x[LFNM+1];
#else
	static char x[BUFSIZ];
#endif
	register char *p;
	static	int	stripping = 0;

	if (stripping)
		return(s);
	stripping++;
	for( p=x; *s; ++s ){
		if( *s != '"' ){
#ifndef FLEXNAMES
/* PATCHED by ROBERT HENRY on 8Jul80 to fix 14 character file name bug */
			if( p >= &x[LFNM] )
#else
			if( p >= &x[BUFSIZ] )
#endif
				cerror( "filename too long" );
			*p++ = *s;
		}
	}
	stripping = 0;
	*p = '\0';
#ifndef FLEXNAMES
	return( x );
#else
	return( hash(x) );
#endif
	}

fsave( s ) char *s; {
	static union rec fsname;
	s = strip( s );
#ifndef FLEXNAMES
	if( strncmp( s, fsname.f.fn, LFNM ) )
#else
	if (fsname.f.fn == NULL || strcmp(s, fsname.f.fn))
#endif
		{
		/* new one */
#ifndef FLEXNAMES
		strncpy( fsname.f.fn, s, LFNM );
#else
		fsname.f.fn = s;
#endif
		fsname.f.decflag = LFN;
		fwrite( (char *)&fsname, sizeof(fsname), 1, stdout );
#ifdef FLEXNAMES
		/* if generating a library, prefix with the library name */
		/* only do this for flexnames */
		if( libname ){
			fwrite( libname, strlen(libname), 1, stdout );
			putchar( ':' );
			}
		fwrite( fsname.f.fn, strlen(fsname.f.fn)+1, 1, stdout );
#endif
		}
	}

where(f){ /* print true location of error */
	if( f == 'u' && nerrors > 1 )
		--nerrors; /* don't get "too many errors" */
	fprintf( stderr, "%s(%d): ", strip(ftitle), lineno);
	}

	/* a number of dummy routines, unneeded by lint */

branch(n){;}
defalign(n){;}
deflab(n){;}

extern char *	strchr();

#define SBUFSIZE	16
#define SCLICK		80

#ifndef size_t
#define size_t	unsigned
#endif /* !size_t */

static char *	strings[SBUFSIZE];
static NODE *	strnodes[SBUFSIZE];
static int	didstr;
static int	subscr;
static int	strapped;

bycode(t, i)
{
	extern char *	calloc();
	extern char *	realloc();

	if (nflag || strapped)
		return;
	if (i == 0)
		if (subscr < (SBUFSIZE - 1))
			++subscr;
	if (subscr >= SBUFSIZE)
		return;
	didstr = 1;
	if ((i % SCLICK) == 0) {
		strings[subscr] = (strings[subscr] == NULL) ?
			calloc((size_t) (SCLICK + 1), 1) :
			realloc(strings[subscr], (size_t) (i + SCLICK + 1));
		if (strings[subscr] == NULL) {
			strapped = 1;
			return;
		}
	}
	strings[subscr][i] = t;
}

strforget()
{
	didstr = subscr = 0;
}

static char *
typestr(t)
{
	switch (t) {
		case CHAR:		return "char";
		case UCHAR:		return "unsigned char";
		case SHORT:		return "short";
		case USHORT:		return "unsigned short";
		case INT:		return "int";
		case UNSIGNED:		return "unsigned";
		case ENUMTY:		return "enum";
		case LONG:		return "long";
		case ULONG:		return "unsigned long";
		case FLOAT:		return "float";
		case DOUBLE:		return "double";
		case STRTY:		return "struct";
		case UNIONTY:		return "union";
		case PTR|CHAR:		return "char *";
		case PTR|UCHAR:		return "unsigned char *";
		case PTR|SHORT:		return "short *";
		case PTR|USHORT:	return "unsigned short *";
		case PTR|INT:		return "int *";
		case PTR|UNSIGNED:	return "unsigned *";
		case PTR|ENUMTY:	return "enum *";
		case PTR|LONG:		return "long *";
		case PTR|ULONG:		return "unsigned long *";
		case PTR|FLOAT:		return "float *";
		case PTR|DOUBLE:	return "double *";
		case PTR|STRTY:		return "struct *";
		case PTR|UNIONTY:	return "union *";
		default:		return ISPTR(t) ?
						"pointer" : "non-scalar";
	}
}

NODE *
ntharg(p, n, acount)
NODE *		p;
register int	n;
register int	acount;
{
	if (n > acount)
		return NULL;
	p = p->in.right;
	while (n != acount) {
		p = p->in.left;
		--acount;
	}
	return (n == 1) ? p : p->in.right;
}

struct entry {
	/* If argument to print/scan is of type... */	int	argtype;
	/* ...and this length character is used... */	char	lchar;
	/* ...and one of these is control char... */	char *	cchars;
	/* ...then use this format with werror... */	char *	werror;
	/* ...(where NULL means it's hunky dory)... */
};

/*
** Portable printf.
** H&S says "%o" takes an unsigned argument;
** X3J11 says "%o" takes an int argument;
** we'll allow either here.
*/

static struct entry pprintf[] = {
	CHAR,		'\0',	"c",		NULL, /* this is deliberate */
	INT,		'\0',	"cdoxX",	NULL,
	UNSIGNED,	'\0',	"uoxX",		NULL,
	CHAR,		'\0',	"cdoxX",	NULL,
	UCHAR,		'\0',	"udoxX",	NULL, /* yes, d is okay */
	SHORT,		'\0',	"cdoxX",	NULL,
	USHORT,		'\0',	"uoxX",		NULL,
	ENUMTY,		'\0',	"duoxX",	NULL,
	LONG,		'l',	"doxX",		NULL,
	ULONG,		'l',	"uoxX",		NULL,
	FLOAT,		'\0',	"eEfgG",	NULL,
	DOUBLE,		'\0',	"eEfgG",	NULL,
	PTR|CHAR,	'\0',	"s",		NULL,
	UNDEF,		'\0',	"",		NULL
};

/*
** Berkeley printf.
** It allows %D, %O, and %U, which we deprecate.
** Since
**	sizeof (char *) == sizeof (int) &&
**	sizeof (int) == sizeof (long) &&
**	sizeof (char *) == sizeof (int *)
** you can be lax--and we tolerate *some* laxness. 
** g/lax/p to find lax table entries and code.
*/

static char	uppercase[] = "deprecated upper-case control character (%c)";
#define lax	NULL

static struct entry bprintf[] = {
	CHAR,		'\0',	"c",		NULL,	/* this is deliberate */
	INT,		'\0',	"cdoxX",	NULL,
	INT,		'\0',	"DO",		uppercase,
	UNSIGNED,	'\0',	"uoxX",		NULL,
	UNSIGNED,	'\0',	"UO",		uppercase,
	CHAR,		'\0',	"cdoxX",	NULL,
	CHAR,		'\0',	"DO",		uppercase,
	UCHAR,		'\0',	"duoxX",	NULL,	/* yes, d is okay */
	UCHAR,		'\0',	"DUO",		uppercase,
	SHORT,		'\0',	"cdoxX",	NULL,
	SHORT,		'\0',	"DO",		uppercase,
	USHORT,		'\0',	"duoxX",	NULL,	/* d okay on BSD */
	USHORT,		'\0',	"DUO",		uppercase,
	ENUMTY,		'\0',	"duoxX",	NULL,
	ENUMTY,		'\0',	"DUO",		uppercase,
	LONG,		'\0',	"doxX",		lax,
	LONG,		'\0',	"DO",		uppercase,
	LONG,		'l',	"doxX",		NULL,
	INT,		'l',	"doxX",		lax,
	ULONG,		'\0',	"uoxX",		lax,
	ULONG,		'\0',	"UO",		uppercase,
	ULONG,		'l',	"uoxX",		NULL,
	UNSIGNED,	'l',	"uoxX",		lax,
	FLOAT,		'\0',	"eEfgG",	NULL,
	DOUBLE,		'\0',	"eEfgG",	NULL,
	PTR|CHAR,	'\0',	"s",		NULL,
	UNDEF,		'\0',	NULL,		NULL,
};

/*
** Portable scanf.  'l' and 'h' are universally ignored preceding 'c' and 's',
** and 'h' is universally ignored preceding 'e' and 'f',
** but you won't find such cruft here.
*/

static struct entry pscanf[] = {
	INT,		'\0',	"dox",	NULL,
	UNSIGNED,	'\0',	"uox",	NULL,
	CHAR,		'\0',	"cs[",	NULL,
	SHORT,		'h',	"dox",	NULL,
	USHORT,		'h',	"uox",	NULL,
	LONG,		'l',	"dox",	NULL,
	ULONG,		'l',	"uox",	NULL,
	FLOAT,		'\0',	"ef",	NULL,	/* BSD doesn't handle g */
	DOUBLE,		'l',	"ef",	NULL,
	UNDEF,		'\0',	NULL,	NULL,
};

/*
** Berkeley scanf.  An upper case letter equals an l plus the lower case char,
** but this is deprecated.
** Even though sizeof (int) == sizeof (long), we'll be picky here.
*/

static struct entry bscanf[] = {
	INT,		'\0',	"dox",	NULL,
	UNSIGNED,	'\0',	"uox",	NULL,
	CHAR,		'\0',	"cs[",	NULL,
	SHORT,		'h',	"dox",	NULL,
	USHORT,		'h',	"uox",	NULL,
	LONG,		'\0',	"dox",	lax,
	LONG,		'\0',	"DOX",	uppercase,
	LONG,		'l',	"dox",	NULL,
	ULONG,		'\0',	"uox",	lax,
	ULONG,		'\0',	"UOX",	uppercase,
	ULONG,		'l',	"uox",	NULL,
	FLOAT,		'\0',	"ef",	NULL,
	DOUBLE,		'\0',	"EF",	uppercase,
	DOUBLE,		'l',	"ef",	NULL,
	UNDEF,		'\0',	NULL,	NULL,
};

static struct item {
	char *		name;		/* such as "printf" */
	int		isscan;		/* scanf/printf */
	int		fmtarg;		/* number of format argument */
	struct entry *	ptable;		/* portable checking table */
	struct entry *	btable;		/* berkeley checking table */
} items[] = {
	"printf",	0,	1,	pprintf,	bprintf,	
	"fprintf",	0,	2,	pprintf,	bprintf,
	"sprintf",	0,	2,	pprintf,	bprintf,
	"scanf",	1,	1,	pscanf,		bscanf,
	"fscanf",	1,	2,	pscanf,		bscanf,
	"sscanf",	1,	2,	pscanf,		bscanf,
	NULL,		-1,	-1,	NULL,		NULL
};

static char	pwf[]	= "possible wild format";
static char	pfacm[]	= "possible format/argument count mismatch";

static struct entry *
findlc(ep, lchar, cchar)
register struct entry *	ep;
register int		lchar;
register int		cchar;
{
	for ( ; ep->argtype != UNDEF; ++ep)
		if (ep->lchar == lchar && strchr(ep->cchars, cchar) != 0)
			return ep;
	return NULL;
}

static char *
subform(p, sp, acount)
register NODE *			p;
register struct symtab *	sp;
{
	register int		i, j, isscan;
	register NODE *		tp;
	register char *		cp;
	register struct entry *	basep;
	register struct entry *	ep;
	register struct item *	ip;
	register int		lchar;
	register int		cchar;
	register int		t;
	register int		suppressed;
	static char		errbuf[132];

	if (nflag || strapped)
		return NULL;
	cp = sp->sname;
	for (ip = items; ; ++ip)
		if (ip->name == NULL)
			return NULL;	/* not a print/scan function */
		else if (strcmp(ip->name, sp->sname) == 0)
			break;
	isscan = ip->isscan;
	i = ip->fmtarg;
	if (i > acount)
		return NULL;	/* handled in pass 2 */
	tp = ntharg(p, i, acount);
	if (tp->in.type != (PTR|CHAR))
		return NULL;	/* handled in pass 2 */
	if (tp->in.op != ICON || tp->tn.lval != 0)
		return NULL;	/* can't check it */
	for (j = 1; j <= subscr; ++j)
		if (tp == strnodes[j])
			break;
	if (j > subscr)
		return NULL;	/* oh well. . . */
	cp = strings[j];
	/*
	** cp now points to format string.
	*/
	/*
	** For now, ALWAYS use "portable" table, rather than doing this:
	**	basep = pflag ? ip->ptable : ip->btable;
	*/
	basep = ip->ptable;
	for ( ; ; ) {
		if (*cp == '\0')
			return (i == acount) ? NULL : pfacm;
		if (*cp++ != '%')
			continue;
		if (*cp == '\0')
			return "wild trailing %% in format";
		if (*cp == '%') {
			++cp;
			continue;
		}
		if (isscan) {
			suppressed = *cp == '*';
			if (suppressed)
				++cp;
			while (isdigit(*cp))
				++cp;
			if (!suppressed && ++i <= acount) {
				t = ntharg(p, i, acount)->in.type;
				if (!ISPTR(t)) {
(void) sprintf(errbuf,
	"%s argument is type (%s) rather than pointer (arg %d)",
	ip->name, typestr(t), i);
					return errbuf;
				}
				t = DECREF(t);
			}
		} else {
			int	nspace, ndash, nplus, nhash;

			suppressed = 0;
			nspace = ndash = nplus = nhash = 0;
			for ( ; ; ) {
				if (*cp == ' ')
					++nspace;
				else if (*cp == '+')
					++nplus;
				else if (*cp == '-')
					++ndash;
				else if (*cp == '#')
					++nhash;
				else	break;
				++cp;
			}
			if (nspace > 1 || ndash > 1 || nplus > 1 || nhash > 1)
				return "wild repeated flag character in format";
			if (*cp == '*') {
				++cp;
				if (++i > acount)
					break;
				t = ntharg(p, i, acount)->in.type;
				/*
				** Width other than INT or UNSIGNED is suspect.
				*/
				if (t != INT && t != UNSIGNED) {
(void) sprintf(errbuf,
	"field width argument is type (%s) rather than (int) (arg %d)",
	typestr(t), i);
					return errbuf;
				}
			} else while (isdigit(*cp))
				++cp;
			if (*cp == '.') {
				++cp;
				if (*cp == '*') {
					++cp;
					if (++i > acount)
						return pfacm;
					t = ntharg(p, i, acount)->in.type;
					if (t != INT && t != UNSIGNED) {
(void) sprintf(errbuf,
	"precision argument is type (%s) rather than (int) (arg %d)",
	typestr(t), i);
						return errbuf;
					}
				} else while (isdigit(*cp))
					++cp;
			}
			if (++i <= acount)
				t = ntharg(p, i, acount)->in.type;
		}
		if (*cp == 'h' || *cp == 'l')
			lchar = *cp++;
		else	lchar = '\0';
		if ((cchar = *cp++) == '\0')
			return pwf;
		if (i > acount)
			return (findlc(basep, lchar, cchar) == NULL) ?
				pwf : pfacm;
		if (!isscan && !pflag && ISPTR(t) &&
			strchr("douxX", cchar) != 0)
				continue;	/* lax--printf("%d", (int *)) */
		if (suppressed) {
			if (findlc(basep, lchar, cchar) == NULL)
				return pwf;
		} else for (ep = basep; ; ++ep) {
			if (ep->argtype == UNDEF) {	/* end of table */
				ep = findlc(basep, lchar, cchar);
				if (ep == NULL)
					return pwf;
(void) sprintf(errbuf, "%s: (%s) format, (%s) arg (arg %d)",
					ip->name,
					typestr(ep->argtype),
					typestr(isscan ? (t | PTR) : t), i);
				return errbuf;
			}
			if (ep->argtype == t && ep->lchar == lchar &&
				strchr(ep->cchars, cchar) != 0)
					if (ep->werror == 0)
						break;
					else {
						werror(ep->werror, cchar);
						return NULL;
					}
		}
		if (cchar != '[')
			continue;
		do {
			if (*cp == '\0')
				return "possible unmatched '[' in format";
		} while (*cp++ != ']');
	}
	/*NOTREACHED*/
}

doform(p, sp, acount)
NODE *		p;
struct symtab *	sp;
{
	char *	cp;

	if ((cp = subform(p, sp, acount)) != NULL)
		werror(cp);
}

cisreg(t) TWORD t; {return(1);}  /* everyting is a register variable! */

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

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	int	ch;

	while ((ch = getopt(argc,argv,"C:D:I:U:LX:Pabchnpuvxz")) != EOF)
		switch((char)ch) {
			case 'C':
				Cflag = 1;
				libname = optarg;
				continue;
			case 'D':	/* #define */
			case 'I':	/* include path */
			case 'U':	/* #undef */
			case 'X':	/* debugging, done in first pass */
			case 'P':	/* debugging, done in second pass */
				break;
			case 'L':
				libflag = 1;
				/*FALLTHROUGH*/
			case 'v':	/* unused arguments in functions */
				vflag = 0;
				break;
			case 'a':	/* long to int assignment */
				++aflag;
				break;
			case 'b':	/* unreached break statements */
				brkflag = 1;
				break;
			case 'c':	/* questionable casts */
				cflag = 1;
				break;
			case 'h':	/* heuristics */
				hflag = 1;
				break;
			case 'n':	/* standard library check */
				nflag = 1;
				break;
			case 'p':	/* IBM & GCOS portability */
				pflag = 1;
				break;
			case 'u':	/* 2nd pass: undefined or unused */
				break;
			case 'x':	/* unused externs */
				xflag = 1;
				break;
			case 'z':	/* use of undefined structures */
				zflag = 1;
				break;
			case '?':
			default:
				fputs("usage: lint [-C lib] [-D def] [-I include] [-U undef] [-Labchnpuvx] file ...\n",stderr);
				exit(1);
		}

	if (!pflag) {		/* set sizes to sizes of target machine */
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
# endif
# ifdef ibm
		ALSTRUCT = ALCHAR;
# endif
	}
	return(mainp1(argc,argv));
}

ctype( type ) unsigned type; { /* are there any funny types? */
	return( type );
	}

commdec( i ){
	/* put out a common declaration */
	if( stab[i].sclass == STATIC ) outdef( &stab[i], LST, USUAL );
	else outdef( &stab[i], libflag?LIB:LDC, USUAL );
	}

isitfloat ( s ) char *s; {
	/* s is a character string;
	   if floating point is implemented, set dcon to the value of s */
	/* lint version
	*/
	dcon = atof( s );
	return( DCON );
	}

fldcon( p ) register NODE *p; {
	/* p is an assignment of a constant to a field */
	/* check to see if the assignment is going to overflow, or otherwise cause trouble */
	register s;
	CONSZ v;

	if( !hflag & !pflag ) return;

	s = UPKFSZ(p->in.left->tn.rval);
	v = p->in.right->tn.lval;

	switch( p->in.left->in.type ){

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

outdef( p, lty, mode ) struct symtab *p; {
	/* output a definition for the second pass */
	/* if mode is > USUAL, it is the number of args */
	char *fname;
	TWORD t;
	int line;
	static union rec rc;

	if( mode == NOFILE ){
		fname = "???";
		line = p->suse;
		}
	else if( mode == SVLINE ){
		fname = ftitle;
		line = -p->suse;
		}
	else {
		fname = ftitle;
		line = lineno;
		}
	fsave( fname );
#ifndef FLEXNAMES
	strncpy( rc.l.name, exname(p->sname), LCHNM );
#endif
	rc.l.decflag = lty;
	t = p->stype;
	if( mode == DECTY ) t = DECREF(t);
	rc.l.type.aty = t;
	rc.l.type.extra = 0;
	rc.l.type.extra1 = 0;
	astype( &rc.l.type, p->sizoff );
	rc.l.nargs = (mode>USUAL) ? mode : 0;
	rc.l.fline = line;
	fwrite( (char *)&rc, sizeof(rc), 1, stdout );
#ifdef FLEXNAMES
	rc.l.name = exname(p->sname);
	fwrite( rc.l.name, strlen(rc.l.name)+1, 1, stdout );
#endif
	}
int proflg;
int gdebug;
