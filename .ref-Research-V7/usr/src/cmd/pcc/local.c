# include "mfile1"


/*	this file contains code which is dependent on the target machine */

NODE *
cast( p, t ) register NODE *p; TWORD t; {
	/* cast node p to type t */

	p = buildtree( CAST, block( NAME, NIL, NIL, t, 0, (int)t ), p );
	p->left->op = FREE;
	p->op = FREE;
	return( p->right );
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

	register struct symtab *q;
	register NODE *r;
	register o;
	register m, ml;

	switch( o = p->op ){

	case NAME:
		if( p->rval < 0 ) { /* already processed; ignore... */
			return(p);
			}
		q = &stab[p->rval];
		switch( q->sclass ){

		case AUTO:
		case PARAM:
			/* fake up a structure reference */
			r = block( REG, NIL, NIL, PTR+STRTY, 0, 0 );
			r->lval = 0;
			r->rval = (q->sclass==AUTO?STKREG:ARGREG);
			p = stref( block( STREF, r, p, 0, 0, 0 ) );
			break;

		case ULABEL:
		case LABEL:
		case STATIC:
			if( q->slevel == 0 ) break;
			p->lval = 0;
			p->rval = -q->offset;
			break;

		case REGISTER:
			p->op = REG;
			p->lval = 0;
			p->rval = q->offset;
			break;

			}
		break;
	case LT:
	case LE:
	case GT:
	case GE:
		if( ISPTR( p->left->type ) || ISPTR( p->right->type ) ){
			p->op += (ULT-LT);
			}
		break;

	case PCONV:
		/* do pointer conversions for char and longs */
		ml = p->left->type;
		if( ( ml==CHAR || ml==UCHAR || ml==LONG || ml==ULONG ) && p->left->op != ICON ) break;

		/* pointers all have the same representation; the type is inherited */
		p->left->type = p->type;
		p->left->cdim = p->cdim;
		p->left->csiz = p->csiz;
		p->op = FREE;
		return( p->left );

	case SCONV:
		m = (p->type == FLOAT || p->type == DOUBLE );
		ml = (p->left->type == FLOAT || p->left->type == DOUBLE );
		if( m != ml ) break;

		/* now, look for conversions downwards */

		m = p->type;
		ml = p->left->type;
		if( p->left->op == ICON ){ /* simulate the conversion here */
			CONSZ val;
			val = p->left->lval;
			switch( m ){
			case CHAR:
				p->left->lval = (char) val;
				break;
			case UCHAR:
				p->left->lval = val & 0XFF;
				break;
			case UNSIGNED:
				p->left->lval = val & 0XFFFFL;
				break;
			case INT:
				p->left->lval = (int)val;
				break;
				}
			p->left->type = m;
			}
		else {
			/* meaningful ones are conversion of int to char, int to short,
			   and short to char, and unsigned version of them */
			if( m==CHAR || m==UCHAR ){
				if( ml==LONG || ml==ULONG ) break;
				}
			else if( m==INT || m==UNSIGNED ){
				if( ml==LONG || ml==ULONG ) break;
				}
			else if( m==LONG || m==ULONG ){
				if( ml!=LONG && ml!= ULONG ) break;
				}
			}

		/* clobber conversion */
		p->op = FREE;
		return( p->left );  /* conversion gets clobbered */

	case ASSIGN:
		/* get rid of SCONV for assignments
		   from LONG -> CHAR|INT	*/
		if( p->right->op == SCONV ) {
			m = p->right->type;
			ml = p->right->left->type;
			if( ( m==LONG || m==ULONG ) &&
			    ml!=FLOAT && ml!=DOUBLE ) {
				p->right->op = FREE;
				p->right = p->right->left;
				}
			}
		break;

	case PVCONV:
	case PMCONV:
		if( p->right->op != ICON ) cerror( "bad conversion", 0);
		p->op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->left, p->right ) );

	case PLUS:
	case MINUS:
	case LS:
	case MUL:
		/* optimize address calculations with long indexes */
		if( ISPTR( p->type ) || ISARY( p->type ) ) {
			if( p->left->type==LONG || p->left->type==ULONG )
				p->left = cast( p->left, INT );
			if( p->right->type==LONG || p->right->type==ULONG )
				p->right = cast( p->right, INT );
			}
		break;

		}

	return(p);
	}

andable( p ) NODE *p; {
	return(1);  /* all names can have & taken on them */
	}

cendarg(){ /* at the end of the arguments of a ftn, set the automatic offset */
	autooff = AUTOINIT;
	}

cisreg( t ) TWORD t; { /* is an automatic variable of type t OK for a register variable */

	if( t==INT || t==UNSIGNED || ISPTR(t) ) return(1);
	return(0);
	}

NODE *
offcon( off, t, d, s ) OFFSZ off; TWORD t; {

	/* return a node, for structure references, which is suitable for
	   being added to a pointer of type t, in order to be off bits offset
	   into a structure */

	register NODE *p;

	/* t, d, and s are the type, dimension offset, and sizeoffset */
	/* in general they  are necessary for offcon, but not on H'well */

	p = bcon(0);
	p->lval = off/SZCHAR;
	return(p);

	}

static inwd	/* current bit offsed in word */;
static word	/* word being built from fields */;

incode( p, sz ) register NODE *p; {

	/* generate initialization code for assigning a constant c
		to a field of width sz */
	/* we assume that the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* we also assume sz  < SZINT */

	if((sz+inwd) > SZINT) cerror("incode: field > int");
	word |= p->lval<<inwd;
	inwd += sz;
	inoff += sz;
	if(inoff%SZINT == 0) {
		printf( "	%o\n", word);
		word = inwd = 0;
		}
	}

fincode( d, sz ) double d; {
	/* output code to initialize space of size sz to the value d */
	/* the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* on the target machine, write it out in octal! */

	register int *mi = (int *)&d;

	if( sz==SZDOUBLE )
		printf( "	%o; %o; %o; %o\n", mi[0], mi[1], mi[2], mi[3] );
	else
		printf( "	%o; %o\n", mi[0], mi[1] );
	inoff += sz;
	}

cinit( p, sz ) NODE *p; {
	/* arrange for the initialization of p into a space of
	size sz */
	/* the proper alignment has been opbtained */
	/* inoff is updated to have the proper final value */
	ecode( p );
	inoff += sz;
	}

vfdzero( n ){ /* define n bits of zeros in a vfd */

	if( n <= 0 ) return;

	inwd += n;
	inoff += n;
	if( inoff%ALINT ==0 ) {
		printf( "	%o\n", word );
		word = inwd = 0;
		}
	}


char *
exname( p ) char *p; {
	/* make a name look like an external name in the local machine */

	static char text[NCHNAM+1];

	register i;

	text[0] = '_';
	for( i=1; *p&&i<NCHNAM; ++i ){
		text[i] = *p++;
		}

	text[i] = '\0';
	text[NCHNAM] = '\0';  /* truncate */

	return( text );
	}

ctype( type ) TWORD type; { /* map types which are not defined on the local machine */
	switch( BTYPE(type) ){
	case SHORT:
		MODTYPE(type,INT);
		break;
	case USHORT:
		MODTYPE(type,UNSIGNED);
		}
	return( type );
	}

noinit() { /* curid is a variable which is defined but
	is not initialized (and not a function );
	This routine returns the stroage class for an uninitialized declaration */

	return(EXTERN);

	}

commdec( id ){ /* make a common declaration for id, if reasonable */
	register struct symtab *q;
	OFFSZ off;

	q = &stab[id];
	printf( "	.comm	%s,", exname( q->sname ) );
	off = tsize( q->stype, q->dimoff, q->sizoff );
	printf( CONFMT, off/SZCHAR );
	printf( ".\n" );
	}

isitlong( cb, ce ){ /* is lastcon to be long or short */
	/* cb is the first character of the representation, ce the last */

	if( ce == 'l' || ce == 'L' ||
		lastcon >= (1L << (SZINT-1) ) ) return (1);
	return(0);
	}


isitfloat( s ) char *s; {
	double atof();
	dcon = atof(s);
	return( FCON );
	}

ecode( p ) NODE *p; {

	/* walk the tree and write out the nodes.. */

	if( nerrors ) return;
	p2tree( p );
	p2compile( p );
	}

