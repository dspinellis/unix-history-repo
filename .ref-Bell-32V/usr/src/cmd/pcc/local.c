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

	case PCONV:
		/* do pointer conversions for char and longs */
		ml = p->left->type;
		if( ( ml==CHAR || ml==UCHAR || ml==SHORT || ml==USHORT ) && p->left->op != ICON ) break;

		/* pointers all have the same representation; the type is inherited */

	inherit:
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
			case USHORT:
				p->left->lval = val & 0XFFFFL;
				break;
			case SHORT:
				p->left->lval = (short)val;
				break;
			case UNSIGNED:
				p->left->lval = val & 0xFFFFFFFFL;
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
				if( ml!=CHAR && ml!= UCHAR ) break;
				}
			else if( m==SHORT || m==USHORT ){
				if( ml!=CHAR && ml!=UCHAR && ml!=SHORT && ml!=USHORT ) break;
				}
			}

		/* clobber conversion */
		if( tlen(p) == tlen(p->left) ) goto inherit;
		p->op = FREE;
		return( p->left );  /* conversion gets clobbered */

	case PVCONV:
	case PMCONV:
		if( p->right->op != ICON ) cerror( "bad conversion", 0);
		p->op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->left, p->right ) );

	case RS:
	case ASG RS:
		/* convert >> to << with negative shift count */
		/* only if type of left operand is not unsigned */
		if( ISUNSIGNED(p->left->type) ) break;
		p->right = buildtree( UNARY MINUS, p->right, NIL );
		if( p->op == RS ) p->op = LS;
		else p->op = ASG LS;
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

	if( t==INT || t==UNSIGNED || t==LONG || t==ULONG	/* tbl */
		|| t==CHAR || t==UCHAR || t==SHORT || t==USHORT	/* tbl */
		|| ISPTR(t)) return(1);			/* tbl */
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


incode( p, sz ) register NODE *p; {

	/* generate initialization code for assigning a constant c
		to a field of width sz */
	/* we assume that the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* we also assume sz  < SZINT */

	inoff += sz;
	if (sz>SZSHORT) 
		printf("	.long	%d:%d\n", sz, p->lval);
	else if (sz>SZCHAR)
		printf("	.word	%d:%d\n", sz, p->lval);
	else
		printf("	.byte	%d:%d\n", sz, p->lval);
	}

fincode( d, sz ) double d; {
	/* output code to initialize space of size sz to the value d */
	/* the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* on the target machine, write it out in octal! */


	printf("	%s	0%c%.20e\n", sz == SZDOUBLE ? ".double" : ".float",
		sz == SZDOUBLE ? 'd' : 'f', d);
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
	register i;

	if( n <= 0 ) return;

	inoff += n;
	i = n;
	while (i>=SZCHAR) {
		printf("	.byte	0\n");
		i -= SZCHAR;
	}
	if (i) printf("	.byte	%d:0\n", i);
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

ctype( type ){ /* map types which are not defined on the local machine */
	switch( BTYPE(type) ){

	case LONG:
		MODTYPE(type,INT);
		break;

	case ULONG:
		MODTYPE(type,UNSIGNED);
		}
	return( type );
	}

noinit( t ) { /* curid is a variable which is defined but
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
	printf( CONFMT, off/SZCHAR );
	printf( "\n" );
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
	
#include "a.out.h"
int ddebug;
int gdebug;


outstab(p)
struct symtab *p; {
	register TWORD ptype;
	register char *pname;
	register char pclass;
	register int poffset;

	if (!gdebug) return;

	ptype = p->stype;
	pname = p->sname;
	pclass = p->sclass;
	poffset = p->offset;

	if (ISFTN(ptype)) {
		return;
	}
	
	switch (pclass) {
	
	case AUTO:
		pstab(pname, N_LSYM);
		printf("0,%d,%d\n", ptype, (-poffset)/SZCHAR);
		poffs(p);
		return;
	
	case EXTDEF:
	case EXTERN:
		pstab(pname, N_GSYM);
		printf("0,%d,0\n", ptype);
		poffs(p);
		return;
			
	case STATIC:
		pstab(pname, N_STSYM);
		if (p->slevel > 1) {
			printf("0,%d,L%d\n", ptype, poffset);
		} else {
			printf("0,%d,%s\n", ptype, exname(pname));
		}
		poffs(p);
		return;
	
	case REGISTER:
		pstab(pname, N_RSYM);
		printf("0,%d,%d\n", ptype, poffset);
		poffs(p);
		return;
	
	case MOS:
	case MOU:
		pstab(pname, N_SSYM);
		printf("0,%d,%d\n", ptype, poffset/SZCHAR);
		poffs(p);
		return;
	
	case PARAM:
		/* parameter stab entries are processed in dclargs() */
		return;
	
	default:
		if (ddebug) printf("	No .stab for %.8s\n", pname);
	}
}

pstab(name, type)
char *name;
int type; {
	register int i;
	register char c;
	if (!gdebug) return;
	printf("	.stab	");
	for(i=0; i<8; i++) 
		if (c = name[i]) printf("'%c,", c);
		else printf("0,");
	printf("0%o,", type);
}

poffs(p)
register struct symtab *p; {
	int s;
	if (!gdebug) return;
	if ((s = dimtab[p->sizoff]/SZCHAR) > 1) {
		pstab(p->sname, N_LENG);
		printf("1,0,%d\n", s);
	}
}

char NULLNAME[8];
int  labelno;
int  fdefflag;

psline() {
	static int lastlineno;
	register char *cp, *cq;
	register int i;
	
	if (!gdebug) return;

	cq = ititle;
	cp = ftitle;

	while ( *cq ) if ( *cp++ != *cq++ ) goto neq;
	if ( *cp == '\0' ) goto eq;
	
neq:	for (i=0; i<100; i++)
		ititle[i] = '\0';
	cp = ftitle;
	cq = ititle;
	while ( *cp )  
		*cq++ = *cp++;
	*cq = '\0';
	*--cq = '\0';
	for ( cp = ititle+1; *(cp-1); cp += 8 ) {
		pstab(cp, N_SOL);
		if (gdebug) printf("0,0,LL%d\n", labelno);
		}
	*cq = '"';
	printf("LL%d:\n", labelno++);

eq:	if (lineno == lastlineno) return;
	lastlineno = lineno;

	if (fdefflag) {
		pstab(NULLNAME, N_SLINE);
		printf("0,%d,LL%d\n", lineno, labelno);
		printf("LL%d:\n", labelno++);
		}
	}
	
plcstab(level) {
	if (!gdebug) return;
	pstab(NULLNAME, N_LBRAC);
	printf("0,%d,LL%d\n", level, labelno);
	printf("LL%d:\n", labelno++);
	}
	
prcstab(level) {
	if (!gdebug) return;
	pstab(NULLNAME, N_RBRAC);
	printf("0,%d,LL%d\n", level, labelno);
	printf("LL%d:\n", labelno++);
	}
	
pfstab(sname) 
char *sname; {
	if (!gdebug) return;
	pstab(sname, N_FUN);
	printf("0,%d,_%.7s\n", lineno, sname);
}

#ifndef ONEPASS
tlen(p) NODE *p; 
{
	switch(p->type) {
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
