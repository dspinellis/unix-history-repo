static char *sccsid ="@(#)local.c	1.1 (Berkeley) %G%";
# include "mfile1"

/*	this file contains code which is dependent on the target machine */

NODE *
cast( p, t ) register NODE *p; TWORD t; {
	/* cast node p to type t */

	p = buildtree( CAST, block( NAME, NIL, NIL, t, 0, (int)t ), p );
	p->in.left->in.op = FREE;
	p->in.op = FREE;
	return( p->in.right );
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
			r->tn.rval = (q->sclass==AUTO?STKREG:ARGREG);
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
			break;

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
		m = (p->in.type == FLOAT || p->in.type == DOUBLE );
		ml = (p->in.left->in.type == FLOAT || p->in.left->in.type == DOUBLE );
		if( m != ml ) break;

		/* now, look for conversions downwards */

		m = p->in.type;
		ml = p->in.left->in.type;
		if( p->in.left->in.op == ICON ){ /* simulate the conversion here */
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
		if( tlen(p) == tlen(p->in.left) ) goto inherit;
		p->in.op = FREE;
		return( p->in.left );  /* conversion gets clobbered */

	case PVCONV:
	case PMCONV:
		if( p->in.right->in.op != ICON ) cerror( "bad conversion", 0);
		p->in.op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->in.left, p->in.right ) );

	case RS:
	case ASG RS:
		/* convert >> to << with negative shift count */
		/* only if type of left operand is not unsigned */

		if( ISUNSIGNED(p->in.left->in.type) ) break;
		p->in.right = buildtree( UNARY MINUS, p->in.right, NIL );
		if( p->in.op == RS ) p->in.op = LS;
		else p->in.op = ASG LS;
		break;

	case FLD:
		/* make sure that the second pass does not make the
		   descendant of a FLD operator into a doubly indexed OREG */

		if( p->in.left->in.op == UNARY MUL
				&& (r=p->in.left->in.left)->in.op == PCONV)
			if( r->in.left->in.op == PLUS || r->in.left->in.op == MINUS ) 
				if( ISPTR(r->in.type) ) {
					if( ISUNSIGNED(p->in.left->in.type) )
						p->in.left->in.type = UCHAR;
					else
						p->in.left->in.type = CHAR;
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

#ifdef TRUST_REG_CHAR_AND_REG_SHORT
	if( t==INT || t==UNSIGNED || t==LONG || t==ULONG	/* tbl */
		|| t==CHAR || t==UCHAR || t==SHORT 		/* tbl */
		|| t==USHORT || ISPTR(t)) return(1);		/* tbl */
#else
	if( t==INT || t==UNSIGNED || t==LONG || t==ULONG	/* wnj */
		|| ISPTR(t)) return (1);			/* wnj */
#endif
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
static word	/* word being built from fields */;

incode( p, sz ) register NODE *p; {

	/* generate initialization code for assigning a constant c
		to a field of width sz */
	/* we assume that the proper alignment has been obtained */
	/* inoff is updated to have the proper final value */
	/* we also assume sz  < SZINT */

	if((sz+inwd) > SZINT) cerror("incode: field > int");
	word |= ((unsigned)(p->tn.lval<<(32-sz))) >> (32-sz-inwd);
	inwd += sz;
	inoff += sz;
	if(inoff%SZINT == 0) {
		printf( "	.long	0x%x\n", word);
		word = inwd = 0;
		}
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

	if( n <= 0 ) return;

	inwd += n;
	inoff += n;
	if( inoff%ALINT ==0 ) {
		printf( "	.long	0x%x\n", word );
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

	register i;

	text[0] = '_';
#ifndef FLEXNAMES
	for( i=1; *p&&i<NCHNAM; ++i ){
#else
	for( i=1; *p; ++i ){
#endif
		text[i] = *p++;
		}

	text[i] = '\0';
#ifndef FLEXNAMES
	text[NCHNAM] = '\0';  /* truncate */
#endif

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
	
#include <sys/types.h>
#include <a.out.h>
#include <stab.h>
extern int ddebug;
extern int gdebug;

fixarg(p)
struct symtab *p; {
		pstab(p->sname, N_PSYM);
		if (gdebug) printf("0,%d,%d\n", p->stype, argoff/SZCHAR);
		poffs(p);
}
int	stabLCSYM;

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
#ifdef LCOMM
		/* stabLCSYM is 1 during nidcl so we can get stab type right */
		pstab(pname, stabLCSYM ? N_LCSYM : N_STSYM);
#else
		pstab(pname, N_STSYM);
#endif
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
#ifndef FLEXNAMES
		if (ddebug) printf("	No .stab for %.8s\n", pname);
#else
		if (ddebug) printf("	No .stab for %s\n", pname);
#endif
		
	}
}

pstab(name, type)
char *name;
int type; {
	register int i;
	register char c;
	if (!gdebug) return;
	/* locctr(PROG);  /* .stabs must appear in .text for c2 */
#ifdef ASSTRINGS
	if ( name[0] == '\0')
		printf("\t.stabn\t");
	else
#ifndef FLEXNAMES
		printf("\t.stabs\t\"%.8s\", ", name);
#else
		printf("\t.stabs\t\"%s\", ", name);
#endif
#else
	printf("	.stab	");
	for(i=0; i<8; i++) 
		if (c = name[i]) printf("'%c,", c);
		else printf("0,");
#endif
	printf("0%o,", type);
}

#ifdef STABDOT
pstabdot(type, value)
	int	type;
	int	value;
{
	if ( ! gdebug) return;
	/* locctr(PROG);  /* .stabs must appear in .text for c2 */
	printf("\t.stabd\t");
	printf("0%o,0,0%o\n",type, value);
}
#endif

poffs(p)
register struct symtab *p; {
	int s;
	if (!gdebug) return;
	if ((s = dimtab[p->sizoff]/SZCHAR) > 1) {
		pstab(p->sname, N_LENG);
		printf("1,0,%d\n", s);
	}
}

extern char NULLNAME[8];
extern int  labelno;
extern int  fdefflag;

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
#ifndef FLEXNAMES
	for ( cp = ititle+1; *(cp-1); cp += 8 ) {
		pstab(cp, N_SOL);
		if (gdebug) printf("0,0,LL%d\n", labelno);
		}
#else
	pstab(ititle+1, N_SOL);
	if (gdebug) printf("0,0,LL%d\n", labelno);
#endif
	*cq = '"';
	printf("LL%d:\n", labelno++);

eq:	if (lineno == lastlineno) return;
	lastlineno = lineno;

	if (fdefflag) {
#ifdef STABDOT
		pstabdot(N_SLINE, lineno);
#else
		pstab(NULLNAME, N_SLINE);
		printf("0,%d,LL%d\n", lineno, labelno);
		printf("LL%d:\n", labelno++);
#endif
		}
	}
	
plcstab(level) {
	if (!gdebug) return;
#ifdef STABDOT
	pstabdot(N_LBRAC, level);
#else
	pstab(NULLNAME, N_LBRAC);
	printf("0,%d,LL%d\n", level, labelno);
	printf("LL%d:\n", labelno++);
#endif
	}
	
prcstab(level) {
	if (!gdebug) return;
#ifdef STABDOT
	pstabdot(N_RBRAC, level);
#else
	pstab(NULLNAME, N_RBRAC);
	printf("0,%d,LL%d\n", level, labelno);
	printf("LL%d:\n", labelno++);
#endif
	}
	
pfstab(sname) 
char *sname; {
	if (!gdebug) return;
	pstab(sname, N_FUN);
#ifndef FLEXNAMES
	printf("0,%d,_%.7s\n", lineno, sname);
#else
	printf("0,%d,_%s\n", lineno, sname);
#endif
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
