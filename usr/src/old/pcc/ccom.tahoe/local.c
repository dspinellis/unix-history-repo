#ifndef lint
static char sccsid[] = "@(#)local.c	1.6 (Berkeley) %G%";
#endif

# include "pass1.h"

/*	this file contains code which is dependent on the target machine */

NODE *
clocal(p) register NODE *p; {

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
	register int o;
	register int m, ml;

	switch( o = p->in.op ){

	case NAME:
		if( p->tn.rval < 0 ) { /* already processed; ignore... */
			return(p);
			}
		q = &stab[p->tn.rval];
		switch( q->sclass ){

		case AUTO:
		case PARAM:
			/* fake up a structure reference */
			r = block( REG, NIL, NIL, PTR+STRTY, 0, 0 );
			r->tn.lval = 0;
			r->tn.rval = STKREG;
			p = stref( block( STREF, r, p, 0, 0, 0 ) );
			break;

		case ULABEL:
		case LABEL:
		case STATIC:
			if( q->slevel == 0 ) break;
			p->tn.lval = 0;
			p->tn.rval = -q->offset;
			break;

		case REGISTER:
			p->in.op = REG;
			p->tn.lval = 0;
			p->tn.rval = q->offset;
#ifdef REG_CHAR
			m = p->in.type;
			if( m==CHAR || m==SHORT )
				p->in.type = INT;
			else if( m==UCHAR || m==USHORT )
				p->in.type = UNSIGNED;
#endif
			break;

			}
		break;

	case LT:
	case LE:
	case GT:
	case GE:
		if( ISPTR( p->in.left->in.type ) || ISPTR( p->in.right->in.type ) ){
			p->in.op += (ULT-LT);
			}
		break;

	case PCONV:
		/* do pointer conversions for char and longs */
		ml = p->in.left->in.type;
		if( ( ml==CHAR || ml==UCHAR || ml==SHORT || ml==USHORT ) && p->in.left->in.op != ICON ) break;

		/* pointers all have the same representation; the type is inherited */

	inherit:
		p->in.left->in.type = p->in.type;
		p->in.left->fn.cdim = p->fn.cdim;
		p->in.left->fn.csiz = p->fn.csiz;
		p->in.op = FREE;
		return( p->in.left );

	case SCONV:
		m = p->in.type;
		ml = p->in.left->in.type;
		if(m == FLOAT || m == DOUBLE) {
			if(p->in.left->in.op==SCONV &&
			 ml == DOUBLE &&
			 p->in.left->in.left->in.type==m) {
				p->in.op = p->in.left->in.op = FREE;
				return(p->in.left->in.left);
			}
			if(p->in.left->in.op==FCON)
				goto inherit;
			break;
		}
		if(ml == FLOAT || ml == DOUBLE){
			if (p->in.left->in.op == FCON){
				p->in.left->in.op = FREE;
				p->in.op = ICON;
				p->tn.lval = p->in.left->fpn.fval;
				p->tn.rval = NONAME;
				return(p);
			}
			break;
		}
		/* now, look for conversions downwards */

		if( p->in.left->in.op == ICON ){	/* simulate the conversion here */
			CONSZ val;
			val = p->in.left->tn.lval;
			switch( m ){
			case CHAR:
				p->in.left->tn.lval = (char) val;
				break;
			case UCHAR:
				p->in.left->tn.lval = val & 0XFF;
				break;
			case USHORT:
				p->in.left->tn.lval = val & 0XFFFFL;
				break;
			case SHORT:
				p->in.left->tn.lval = (short)val;
				break;
			case UNSIGNED:
				p->in.left->tn.lval = val & 0xFFFFFFFFL;
				break;
			case INT:
				p->in.left->tn.lval = (int)val;
				break;
				}
			p->in.left->in.type = m;
		}
#ifdef notdef
		else if (tlen(p) == tlen(p->in.left))
			goto inherit;
		/* clobber conversion */
		p->in.op = FREE;
		return( p->in.left );  /* conversion gets clobbered */
#endif
		break;

	case QUEST:	/* the right side should be COLON */
		if((r = p->in.right)->in.op == SCONV) {
			p->in.right = r->in.left;
			p->in.type = r->in.left->in.type;
			r->in.left = p;
			return(r);
		}
		return(p);

	case PVCONV:
	case PMCONV:
		if( p->in.right->in.op != ICON ) cerror( "bad conversion", 0);
		p->in.op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->in.left, p->in.right ) );

	case FLD:
		/* make sure that the second pass does not make the
		   descendant of a FLD operator into a doubly indexed OREG */

		if( p->in.left->in.op == UNARY MUL
				&& (r=p->in.left->in.left)->in.op == PCONV)
			if( r->in.left->in.op == PLUS || r->in.left->in.op == MINUS ) 
				if( ISPTR(r->in.type) ) {
					if( ISUNSIGNED(p->in.left->in.type) )
						p->in.left->in.type = UNSIGNED;
					else
						p->in.left->in.type = INT;
				}
		break;
	case FORTCALL: /* arg must be FLOAT */
		if((r = p->in.right)->in.type != FLOAT)
			p->in.right = clocal(makety(r, FLOAT, 0, FLOAT));
		return(p);
	}

	/* if both sides are FLOAT, so is the op */
	if(optype(o)!=LTYPE && p->in.left->in.type==DOUBLE &&
	 (o==UNARY MINUS || optype(o)==BITYPE && p->in.right->in.type==DOUBLE)) {
		r = p->in.left;
		if(r->in.op==SCONV && r->in.left->in.type==FLOAT) {
			if(optype(o)==BITYPE) {
				r = p->in.right;
				if(r->in.op==SCONV && r->in.left->in.type==FLOAT) {
					r->in.op = FREE;
					p->in.right = r->in.left;
				} else if(r->in.op==FCON)
					r->in.type = FLOAT;
				else
					return(p);
			}
			r = p->in.left;
			p->in.left = r->in.left;
		} else if(optype(o)==BITYPE && r->in.op==FCON) {
			r = p->in.right;
			if(!(r->in.op==SCONV && r->in.left->in.type==FLOAT))
				return(p);
			p->in.right = r->in.left;
			p->in.left->in.type = FLOAT;
		} else
			return(p);
		if(p->in.type==DOUBLE) {
			p->in.type = FLOAT;
			r->in.left = p;
			return(r);
		} else {	/* usually logop */
			r->in.op = FREE;
			return(p);
		}
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

	if( t==INT || t==UNSIGNED || t==LONG || t==ULONG	/* tbl */
#ifdef REG_CHAR
		|| t==CHAR || t==UCHAR || t==SHORT || t==USHORT		/* tbl */
#endif
		|| ISPTR(t) || t == FLOAT) return (1);		/* wnj */
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
	p->tn.lval = off/SZCHAR;
	return(p);

	}


static inwd	/* current bit offsed in word */;
static CONSZ word	/* word being built from fields */;

incode( p, sz ) register NODE *p; {

	/* generate initialization code for assigning a constant c
		to a field of width sz */
	/* we assume that the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* we also assume sz  < SZINT */

	inwd += sz;
	if(inwd > SZINT) cerror("incode: field > int");
	word |= (p->tn.lval&((1L<<sz)-1)) << (SZINT-inwd);
	inoff += sz;
	if(inoff%SZINT == 0) {
		printf( "	.long	0x%X\n", word);
		word = inwd = 0;
		}
	}

fincode( d, sz ) double d; register int sz; {
	/*
	 * output code to initialize space of size sz to the value d
	 * the proper alignment has been obtained
	 * inoff is updated to have the proper final value.
	 */

	register struct sh4 {
		unsigned short sh[4];
	} *x;
	float f;

	if(sz == SZFLOAT) {	/* force rounding */
		f = d;
		d = f;
	}

	x = (struct sh4 *)&d;
	printf("	.long	0x%04x%04x", x->sh[0], x->sh[1]);
	if(sz == SZDOUBLE) {
		printf(", 0x%04x%04x", x->sh[2], x->sh[3]);
		printf(" # .double %.17g\n", d);
	} else
		printf(" # .float %.8g\n", d);
	inoff += sz;
	}

cinit( p, sz ) NODE *p; {
	NODE *l;

	/*
	 * as a favor (?) to people who want to write
	 *     int i = 9600/134.5;
	 * we will, under the proper circumstances, do
	 * a coersion here.
	 */
	switch (p->in.type) {
	case INT:
	case UNSIGNED:
		l = p->in.left;
		if (l->in.op != SCONV ||
		    (l->in.left->tn.op != DCON && l->in.left->tn.op != FCON))
			break;
		l->in.op = FREE;
		l = l->in.left;
		l->tn.lval = l->tn.op == DCON ? (long)(l->dpn.dval) :
			(long)(l->fpn.fval);
		l->tn.rval = NONAME;
		l->tn.op = ICON;
		l->tn.type = INT;
		p->in.left = l;
		break;
	}
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
		printf( "	.long	0x%X\n", word );
		word = inwd = 0;
		}
	}

char *
exname( p ) char *p; {
	/* make a name look like an external name in the local machine */

#ifndef FLEXNAMES
	static char text[NCHNAM+1];
#else
	static char text[BUFSIZ+1];
#endif

	register int i;

	text[0] = '_';
#ifndef FLEXNAMES
	for( i=1; *p&&i<NCHNAM; ++i )
#else
	for( i=1; *p; ++i )
#endif
		text[i] = *p++;

	text[i] = '\0';
#ifndef FLEXNAMES
	text[NCHNAM] = '\0';  /* truncate */
#endif

	return( text );
	}

ctype( type )TWORD type;{ /* map types which are not defined on the local machine */
	switch( BTYPE(type) ){

	case LONG:
		MODTYPE(type,INT);
		break;

	case ULONG:
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
	OFFSZ off, tsize();

	q = &stab[id];
	printf( "	.comm	%s,", exname( q->sname ) );
	off = tsize( q->stype, q->dimoff, q->sizoff );
	printf( "%d\n" /*CONFMT*/, off/SZCHAR );
	}

prtdcon(p)
	register NODE *p;
{
	register int o = p->in.op;
	int i;

	if (o != DCON && o != FCON)
		return;
	/*
	 * Clobber constants of value zero so
	 * we can generate more efficient code.
	 */
	if ((o == DCON && p->dpn.dval == 0) ||
	    (o == FCON && p->fpn.fval == 0)) {
		p->in.op = ICON;
		p->tn.rval = NONAME;
		return;
	}
	locctr(DATA);
	defalign(o == DCON ? ALDOUBLE : ALFLOAT);
	deflab(i = getlab());
	if (o == FCON)
		fincode(p->fpn.fval, SZFLOAT);
	else
		fincode(p->dpn.dval, SZDOUBLE);
	p->tn.lval = 0;
	p->tn.rval = -i;
	p->in.type = (o == DCON ? DOUBLE : FLOAT);
	p->in.op = NAME;
}

isitfloat( s ) char *s; {
	union cvt {
		double	d;
		int	n[2];
	} cvt;
	double atof();

	/* avoid floating point exception for double -> float conversions */
	dcon = cvt.d = atof(s);
	if( cvt.n[1] == 0 ){
		fcon = dcon;
		return( FCON );
		}
	return( DCON );
	}

ecode( p ) NODE *p; {

	/* walk the tree and write out the nodes.. */

	if( nerrors ) return;
	p2tree( p );
	p2compile( p );
	}

#ifndef ONEPASS
tlen(p) NODE *p; 
{
	switch(p->in.type) {
		case CHAR:
		case UCHAR:
			return(1);
			
		case SHORT:
		case USHORT:
			return(2);
			
		case DOUBLE:
			return(8);
			
		default:
			return(4);
		}
	}
#endif
