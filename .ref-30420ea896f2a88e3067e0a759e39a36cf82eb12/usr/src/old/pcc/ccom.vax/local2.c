static char *sccsid ="@(#)local2.c	1.1 (Berkeley) %G%";
# include "mfile2"
# include "ctype.h"
# ifdef FORT
int ftlab1, ftlab2;
# endif
/* a lot of the machine dependent parts of the second pass */

# define BITMASK(n) ((1L<<n)-1)

where(c){
	fprintf( stderr, "%s, line %d: ", filename, lineno );
	}

lineid( l, fn ) char *fn; {
	/* identify line l and file fn */
	printf( "#	line %d, file %s\n", l, fn );
	}


eobl2(){
	OFFSZ spoff;	/* offset from stack pointer */
#ifdef FORT
	spoff = maxoff;
	if( spoff >= AUTOINIT ) spoff -= AUTOINIT;
	spoff /= SZCHAR;
	SETOFF(spoff,4);
#ifndef FLEXNAMES
	printf( "	.set	.F%d,%ld\n", ftnno, spoff );
#else
	/* SHOULD BE L%d ... ftnno but must change pc/f77 */
	printf( "	.set	LF%d,%ld\n", ftnno, spoff );
#endif
#else
	extern int ftlab1, ftlab2;

	spoff = maxoff;
	if( spoff >= AUTOINIT ) spoff -= AUTOINIT;
	spoff /= SZCHAR;
	SETOFF(spoff,4);
	printf( "L%d:\n", ftlab1);
	if( spoff!=0 )
		if( spoff < 64 )
			printf( "	subl2	$%ld,sp\n", spoff);
		else
			printf( "	movab	-%ld(sp),sp\n", spoff);
	printf( "	jbr 	L%d\n", ftlab2);
#endif
	maxargs = -1;
	}

struct hoptab { int opmask; char * opstring; } ioptab[] = {

	ASG PLUS, "add",
	ASG MINUS, "sub",
	ASG MUL, "mul",
	ASG DIV, "div",
	ASG OR, "bis",
	ASG ER,	"xor",
	ASG AND, "bic",
	PLUS,	"add",
	MINUS,	"sub",
	MUL,	"mul",
	DIV,	"div",
	OR,	"bis",
	ER,	"xor",
	AND,	"bic",
	-1, ""    };

hopcode( f, o ){
	/* output the appropriate string from the above table */

	register struct hoptab *q;

	for( q = ioptab;  q->opmask>=0; ++q ){
		if( q->opmask == o ){
			printf( "%s", q->opstring );
/* tbl
			if( f == 'F' ) printf( "e" );
			else if( f == 'D' ) printf( "d" );
   tbl */
/* tbl */
			switch( f ) {
				case 'L':
				case 'W':
				case 'B':
				case 'D':
				case 'F':
					printf("%c", tolower(f));
					break;

				}
/* tbl */
			return;
			}
		}
	cerror( "no hoptab for %s", opst[o] );
	}

char *
rnames[] = {  /* keyed to register number tokens */

	"r0", "r1",
	"r2", "r3", "r4", "r5",
	"r6", "r7", "r8", "r9", "r10", "r11",
	"ap", "fp", "sp", "pc",

	};

int rstatus[] = {
	SAREG|STAREG, SAREG|STAREG,
	SAREG|STAREG, SAREG|STAREG, SAREG|STAREG, SAREG|STAREG,
	SAREG, SAREG, SAREG, SAREG, SAREG, SAREG,
	SAREG, SAREG, SAREG, SAREG,

	};

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

mixtypes(p, q) NODE *p, *q;
{
	register tp, tq;

	tp = p->in.type;
	tq = q->in.type;

	return( (tp==FLOAT || tp==DOUBLE) !=
		(tq==FLOAT || tq==DOUBLE) );
}

prtype(n) NODE *n;
{
	switch (n->in.type)
		{
		case DOUBLE:
			printf("d");
			return;

		case FLOAT:
			printf("f");
			return;

		case LONG:
		case ULONG:
		case INT:
		case UNSIGNED:
			printf("l");
			return;

		case SHORT:
		case USHORT:
			printf("w");
			return;

		case CHAR:
		case UCHAR:
			printf("b");
			return;

		default:
			if ( !ISPTR( n->in.type ) ) cerror("zzzcode- bad type");
			else {
				printf("l");
				return;
				}
		}
}

zzzcode( p, c ) register NODE *p; {
	register m;
	CONSZ val;
	switch( c ){

	case 'N':  /* logical ops, turned into 0-1 */
		/* use register given by register 1 */
		cbgen( 0, m=getlab(), 'I' );
		deflab( p->bn.label );
		printf( "	clrl	%s\n", rnames[getlr( p, '1' )->tn.rval] );
		deflab( m );
		return;

	case 'I':
	case 'P':
		cbgen( p->in.op, p->bn.label, c );
		return;

	case 'A':
		{
		register NODE *l, *r;

		if (xdebug) eprint(p, 0, &val, &val);
		r = getlr(p, 'R');
		if (optype(p->in.op) == LTYPE || p->in.op == UNARY MUL)
			{
			l = resc;
			l->in.type = (r->in.type==FLOAT || r->in.type==DOUBLE ? DOUBLE : INT);
			}
		else
			l = getlr(p, 'L');
		if (r->in.op == ICON)
			if(r->in.name[0] == '\0')
				{
				if (r->tn.lval == 0)
					{
					printf("clr");
					prtype(l);
					printf("	");
					adrput(l);
					return;
					}
				if (r->tn.lval < 0 && r->tn.lval >= -63)
					{
					printf("mneg");
					prtype(l);
					r->tn.lval = -r->tn.lval;
					goto ops;
					}
				r->in.type = (r->tn.lval < 0 ?
						(r->tn.lval >= -128 ? CHAR
						: (r->tn.lval >= -32768 ? SHORT
						: INT )) : r->in.type);
				r->in.type = (r->tn.lval >= 0 ?
						(r->tn.lval <= 63 ? INT
						: ( r->tn.lval <= 127 ? CHAR
						: (r->tn.lval <= 255 ? UCHAR
						: (r->tn.lval <= 32767 ? SHORT
						: (r->tn.lval <= 65535 ? USHORT
						: INT ))))) : r->in.type );
				}
				else
					{
					printf("moval");
					printf("	");
					acon(r);
					printf(",");
					adrput(l);
					return;
					}

		if (l->in.op == REG && l->in.type != FLOAT && l->in.type != DOUBLE)
			{
			if( tlen(l) < tlen(r) )
				{
				if (!mixtypes(l,r))
					{
					!ISUNSIGNED(l->in.type)?
						printf("cvt"):
						printf("movz");
					prtype(l);
					printf("l");
					goto ops;
					}
				else
					{
					printf("cvt");
					prtype(r);
					prtype(l);
					printf("	");
					adrput(r);
					printf(",");
					adrput(l);
					printf("cvt");
					prtype(l);
					printf("l");
					printf("	");
					adrput(l);
					printf(",");
					adrput(l);
					return;
					}
				}
			else
				{
			l->in.type = INT;
				}
			}
		if (!mixtypes(l,r))
			{
			if (tlen(l) == tlen(r))
				{
				printf("mov");
				prtype(l);
				goto ops;
				}
			else if (tlen(l) > tlen(r) && ISUNSIGNED(r->in.type))
				{
				printf("movz");
				}
			else
				{
				printf("cvt");
				}
			}
		else
			{
			printf("cvt");
			}
		prtype(r);
		prtype(l);
	ops:
		printf("	");
		adrput(r);
		printf(",");
		adrput(l);
		return;
		}

	case 'B':	/* get oreg value in temp register for left shift */
		{
		register NODE *r;
		if (xdebug) eprint(p, 0, &val, &val);
		r = p->in.right;
		if( tlen(r) == sizeof(int) && r->in.type != FLOAT )
			printf("movl");
		else {
			printf("cvt");
			prtype(r);
			printf("l");
			}
		return;
		}

	case 'C':	/* num words pushed on arg stack */
		{
		extern int gc_numbytes;
		extern int xdebug;

		if (xdebug) printf("->%d<-",gc_numbytes);

		printf("$%d", gc_numbytes/(SZLONG/SZCHAR) );
		return;
		}

	case 'D':	/* INCR and DECR */
		zzzcode(p->in.left, 'A');
		printf("\n	");

	case 'E':	/* INCR and DECR, FOREFF */
		if (p->in.right->tn.lval == 1)
			{
			printf("%s", (p->in.op == INCR ? "inc" : "dec") );
			prtype(p->in.left);
			printf("	");
			adrput(p->in.left);
			return;
			}
		printf("%s", (p->in.op == INCR ? "add" : "sub") );
		prtype(p->in.left);
		printf("2	");
		adrput(p->in.right);
		printf(",");
		adrput(p->in.left);
		return;

	case 'F':	/* register type of right operand */
		{
		register NODE *n;
		extern int xdebug;
		register int ty;

		n = getlr( p, 'R' );
		ty = n->in.type;

		if (xdebug) printf("->%d<-", ty);

		if ( ty==DOUBLE) printf("d");
		else if ( ty==FLOAT ) printf("f");
		else printf("l");
		return;
		}

	case 'L':	/* type of left operand */
	case 'R':	/* type of right operand */
		{
		register NODE *n;
		extern int xdebug;

		n = getlr ( p, c);
		if (xdebug) printf("->%d<-", n->in.type);

		prtype(n);
		return;
		}

	case 'Z':	/* complement mask for bit instr */
		printf("$%ld", ~p->in.right->tn.lval);
		return;

	case 'U':	/* 32 - n, for unsigned right shifts */
		printf("$%d", 32 - p->in.right->tn.lval );
		return;

	case 'T':	/* rounded structure length for arguments */
		{
		int size;

		size = p->stn.stsize;
		SETOFF( size, 4);
		printf("$%d", size);
		return;
		}

	case 'S':  /* structure assignment */
		{
			register NODE *l, *r;
			register size;

			if( p->in.op == STASG ){
				l = p->in.left;
				r = p->in.right;

				}
			else if( p->in.op == STARG ){  /* store an arg into a temporary */
				l = getlr( p, '3' );
				r = p->in.left;
				}
			else cerror( "STASG bad" );

			if( r->in.op == ICON ) r->in.op = NAME;
			else if( r->in.op == REG ) r->in.op = OREG;
			else if( r->in.op != OREG ) cerror( "STASG-r" );

			size = p->stn.stsize;

			if( size <= 0 || size > 65535 )
				cerror("structure size <0=0 or >65535");

			switch(size) {
				case 1:
					printf("	movb	");
					break;
				case 2:
					printf("	movw	");
					break;
				case 4:
					printf("	movl	");
					break;
				case 8:
					printf("	movq	");
					break;
				default:
					printf("	movc3	$%d,", size);
					break;
			}
			adrput(r);
			printf(",");
			adrput(l);
			printf("\n");

			if( r->in.op == NAME ) r->in.op = ICON;
			else if( r->in.op == OREG ) r->in.op = REG;

			}
		break;

	default:
		cerror( "illegal zzzcode" );
		}
	}

rmove( rt, rs, t ){
	printf( "	%s	%s,%s\n",
		(t==FLOAT ? "movf" : (t==DOUBLE ? "movd" : "movl")),
		rnames[rs], rnames[rt] );
	}

struct respref
respref[] = {
	INTAREG|INTBREG,	INTAREG|INTBREG,
	INAREG|INBREG,	INAREG|INBREG|SOREG|STARREG|STARNM|SNAME|SCON,
	INTEMP,	INTEMP,
	FORARG,	FORARG,
	INTEMP,	INTAREG|INAREG|INTBREG|INBREG|SOREG|STARREG|STARNM,
	0,	0 };

setregs(){ /* set up temporary registers */
	fregs = 6;	/* tbl- 6 free regs on VAX (0-5) */
	;
	}

szty(t){ /* size, in registers, needed to hold thing of type t */
	return( (t==DOUBLE||t==FLOAT) ? 2 : 1 );
	}

rewfld( p ) NODE *p; {
	return(1);
	}

callreg(p) NODE *p; {
	return( R0 );
	}

base( p ) register NODE *p; {
	register int o = p->in.op;

	if( (o==ICON && p->in.name[0] != '\0')) return( 100 ); /* ie no base reg */
	if( o==REG ) return( p->tn.rval );
    if( (o==PLUS || o==MINUS) && p->in.left->in.op == REG && p->in.right->in.op==ICON)
		return( p->in.left->tn.rval );
    if( o==OREG && !R2TEST(p->tn.rval) && (p->in.type==INT || p->in.type==UNSIGNED || ISPTR(p->in.type)) )
		return( p->tn.rval + 0200*1 );
	if( o==INCR && p->in.left->in.op==REG ) return( p->in.left->tn.rval + 0200*2 );
	if( o==ASG MINUS && p->in.left->in.op==REG) return( p->in.left->tn.rval + 0200*4 );
	if( o==UNARY MUL && p->in.left->in.op==INCR && p->in.left->in.left->in.op==REG
	  && (p->in.type==INT || p->in.type==UNSIGNED || ISPTR(p->in.type)) )
		return( p->in.left->in.left->tn.rval + 0200*(1+2) );
	return( -1 );
	}

offset( p, tyl ) register NODE *p; int tyl; {

	if( tyl==1 && p->in.op==REG && (p->in.type==INT || p->in.type==UNSIGNED) ) return( p->tn.rval );
	if( (p->in.op==LS && p->in.left->in.op==REG && (p->in.left->in.type==INT || p->in.left->in.type==UNSIGNED) &&
	      (p->in.right->in.op==ICON && p->in.right->in.name[0]=='\0')
	      && (1<<p->in.right->tn.lval)==tyl))
		return( p->in.left->tn.rval );
	return( -1 );
	}

makeor2( p, q, b, o) register NODE *p, *q; register int b, o; {
	register NODE *t;
	register int i;
	NODE *f;

	p->in.op = OREG;
	f = p->in.left; 	/* have to free this subtree later */

	/* init base */
	switch (q->in.op) {
		case ICON:
		case REG:
		case OREG:
			t = q;
			break;

		case MINUS:
			q->in.right->tn.lval = -q->in.right->tn.lval;
		case PLUS:
			t = q->in.right;
			break;

		case INCR:
		case ASG MINUS:
			t = q->in.left;
			break;

		case UNARY MUL:
			t = q->in.left->in.left;
			break;

		default:
			cerror("illegal makeor2");
	}

	p->tn.lval = t->tn.lval;
#ifndef FLEXNAMES
	for(i=0; i<NCHNAM; ++i)
		p->in.name[i] = t->in.name[i];
#else
	p->in.name = t->in.name;
#endif

	/* init offset */
	p->tn.rval = R2PACK( (b & 0177), o, (b>>7) );

	tfree(f);
	return;
	}

canaddr( p ) NODE *p; {
	register int o = p->in.op;

	if( o==NAME || o==REG || o==ICON || o==OREG || (o==UNARY MUL && shumul(p->in.left)) ) return(1);
	return(0);
	}

shltype( o, p ) register NODE *p; {
	return( o== REG || o == NAME || o == ICON || o == OREG || ( o==UNARY MUL && shumul(p->in.left)) );
	}

flshape( p ) register NODE *p; {
	return( p->in.op == REG || p->in.op == NAME || p->in.op == ICON ||
		(p->in.op == OREG && (!R2TEST(p->tn.rval) || tlen(p) == 1)) );
	}

shtemp( p ) register NODE *p; {
	if( p->in.op == STARG ) p = p->in.left;
	return( p->in.op==NAME || p->in.op ==ICON || p->in.op == OREG || (p->in.op==UNARY MUL && shumul(p->in.left)) );
	}

shumul( p ) register NODE *p; {
	register o;
	extern int xdebug;

	if (xdebug) {
		 printf("\nshumul:op=%d,lop=%d,rop=%d", p->in.op, p->in.left->in.op, p->in.right->in.op);
		printf(" prname=%s,plty=%d, prlval=%D\n", p->in.right->in.name, p->in.left->in.type, p->in.right->tn.lval);
		}


	o = p->in.op;
	if( o == NAME || (o == OREG && !R2TEST(p->tn.rval)) || o == ICON ) return( STARNM );

	if( ( o == INCR || o == ASG MINUS ) &&
	    ( p->in.left->in.op == REG && p->in.right->in.op == ICON ) &&
	    p->in.right->in.name[0] == '\0' )
		{
		switch (p->in.left->in.type)
			{
			case CHAR|PTR:
			case UCHAR|PTR:
				o = 1;
				break;

			case SHORT|PTR:
			case USHORT|PTR:
				o = 2;
				break;

			case INT|PTR:
			case UNSIGNED|PTR:
			case LONG|PTR:
			case ULONG|PTR:
			case FLOAT|PTR:
				o = 4;
				break;

			case DOUBLE|PTR:
				o = 8;
				break;

			default:
				if ( ISPTR(p->in.left->in.type) ) {
					o = 4;
					break;
					}
				else return(0);
			}
		return( p->in.right->tn.lval == o ? STARREG : 0);
		}

	return( 0 );
	}

adrcon( val ) CONSZ val; {
	printf( "$" );
	printf( CONFMT, val );
	}

conput( p ) register NODE *p; {
	switch( p->in.op ){

	case ICON:
		acon( p );
		return;

	case REG:
		printf( "%s", rnames[p->tn.rval] );
		return;

	default:
		cerror( "illegal conput" );
		}
	}

insput( p ) register NODE *p; {
	cerror( "insput" );
	}

upput( p ) register NODE *p; {
	cerror( "upput" );
	}

adrput( p ) register NODE *p; {
	register int r;
	/* output an address, with offsets, from p */

	if( p->in.op == FLD ){
		p = p->in.left;
		}
	switch( p->in.op ){

	case NAME:
		acon( p );
		return;

	case ICON:
		/* addressable value of the constant */
		printf( "$" );
		acon( p );
		return;

	case REG:
		printf( "%s", rnames[p->tn.rval] );
		return;

	case OREG:
		r = p->tn.rval;
		if( R2TEST(r) ){ /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if( flags & 1 ) printf("*");
			if( flags & 4 ) printf("-");
			if( p->tn.lval != 0 || p->in.name[0] != '\0' ) acon(p);
			if( R2UPK1(r) != 100) printf( "(%s)", rnames[R2UPK1(r)] );
			if( flags & 2 ) printf("+");
			printf( "[%s]", rnames[R2UPK2(r)] );
			return;
			}
		if( r == AP ){  /* in the argument region */
			if( p->tn.lval <= 0 || p->in.name[0] != '\0' ) werror( "bad arg temp" );
			printf( CONFMT, p->tn.lval );
			printf( "(ap)" );
			return;
			}
		if( p->tn.lval != 0 || p->in.name[0] != '\0') acon( p );
		printf( "(%s)", rnames[p->tn.rval] );
		return;

	case UNARY MUL:
		/* STARNM or STARREG found */
		if( tshape(p, STARNM) ) {
			printf( "*" );
			adrput( p->in.left);
			}
		else {	/* STARREG - really auto inc or dec */
			register NODE *q;

/* tbl
			p = p->in.left;
			p->in.left->in.op = OREG;
			if( p->in.op == INCR ) {
				adrput( p->in.left );
				printf( "+" );
				}
			else {
				printf( "-" );
				adrput( p->in.left );
				}
   tbl */
			printf("%s(%s)%s", (p->in.left->in.op==INCR ? "" : "-"),
				rnames[p->in.left->in.left->tn.rval], 
				(p->in.left->in.op==INCR ? "+" : "") );
			p->in.op = OREG;
			p->tn.rval = p->in.left->in.left->tn.rval;
			q = p->in.left;
			p->tn.lval = (p->in.left->in.op == INCR ? -p->in.left->in.right->tn.lval : 0);
#ifndef FLEXNAMES
			p->in.name[0] = '\0';
#else
			p->in.name = "";
#endif
			tfree(q);
		}
		return;

	default:
		cerror( "illegal address" );
		return;

		}

	}

acon( p ) register NODE *p; { /* print out a constant */

	if( p->in.name[0] == '\0' ){
		printf( CONFMT, p->tn.lval);
		}
	else if( p->tn.lval == 0 ) {
#ifndef FLEXNAMES
		printf( "%.8s", p->in.name );
#else
		printf( "%s", p->in.name );
#endif
		}
	else {
#ifndef FLEXNAMES
		printf( "%.8s+", p->in.name );
#else
		printf( "%s+", p->in.name );
#endif
		printf( CONFMT, p->tn.lval );
		}
	}

/*
aacon( p ) register NODE *p; { /* print out a constant */
/*

	if( p->in.name[0] == '\0' ){
		printf( CONFMT, p->tn.lval);
		return( 0 );
		}
	else if( p->tn.lval == 0 ) {
#ifndef FLEXNAMES
		printf( "$%.8s", p->in.name );
#else
		printf( "$%s", p->in.name );
#endif
		return( 1 );
		}
	else {
		printf( "$(" );
		printf( CONFMT, p->tn.lval );
		printf( "+" );
#ifndef FLEXNAMES
		printf( "%.8s)", p->in.name );
#else
		printf( "%s)", p->in.name );
#endif
		return(1);
		}
	}
 */

genscall( p, cookie ) register NODE *p; {
	/* structure valued call */
	return( gencall( p, cookie ) );
	}

/* tbl */
int gc_numbytes;
/* tbl */

gencall( p, cookie ) register NODE *p; {
	/* generate the call given by p */
	register NODE *p1, *ptemp;
	register temp, temp1;
	register m;

	if( p->in.right ) temp = argsize( p->in.right );
	else temp = 0;

	if( p->in.op == STCALL || p->in.op == UNARY STCALL ){
		/* set aside room for structure return */

		if( p->stn.stsize > temp ) temp1 = p->stn.stsize;
		else temp1 = temp;
		}

	if( temp > maxargs ) maxargs = temp;
	SETOFF(temp1,4);

	if( p->in.right ){ /* make temp node, put offset in, and generate args */
		ptemp = talloc();
		ptemp->in.op = OREG;
		ptemp->tn.lval = -1;
		ptemp->tn.rval = SP;
#ifndef FLEXNAMES
		ptemp->in.name[0] = '\0';
#else
		ptemp->in.name = "";
#endif
		ptemp->in.rall = NOPREF;
		ptemp->in.su = 0;
		genargs( p->in.right, ptemp );
		ptemp->in.op = FREE;
		}

	p1 = p->in.left;
	if( p1->in.op != ICON ){
		if( p1->in.op != REG ){
			if( p1->in.op != OREG || R2TEST(p1->tn.rval) ){
				if( p1->in.op != NAME ){
					order( p1, INAREG );
					}
				}
			}
		}

/*
	if( p1->in.op == REG && p->tn.rval == R5 ){
		cerror( "call register overwrite" );
		}
 */
/* tbl
	setup gc_numbytes so reference to ZC works */

	gc_numbytes = temp&(0x3ff);
/* tbl */

	p->in.op = UNARY CALL;
	m = match( p, INTAREG|INTBREG );

	/* compensate for deficiency in 'ret' instruction ... wah,kre */
	/* (plus in assignment to gc_numbytes above, for neatness only) */
	if (temp >= 1024)
		printf("	addl2	$%d,sp\n", (temp&(~0x3ff)));

/* tbl
	switch( temp ) {
	case 0:
		break;
	case 2:
		printf( "	tst	(sp)+\n" );
		break;
	case 4:
		printf( "	cmp	(sp)+,(sp)+\n" );
		break;
	default:
		printf( "	add	$%d,sp\n", temp);
		}
   tbl */
	return(m != MDONE);
	}

/* tbl */
char *
ccbranches[] = {
	"	jeql	L%d\n",
	"	jneq	L%d\n",
	"	jleq	L%d\n",
	"	jlss	L%d\n",
	"	jgeq	L%d\n",
	"	jgtr	L%d\n",
	"	jlequ	L%d\n",
	"	jlssu	L%d\n",
	"	jgequ	L%d\n",
	"	jgtru	L%d\n",
	};
/* tbl */

cbgen( o, lab, mode ) { /*   printf conditional and unconditional branches */

/* tbl */
	if( o == 0 ) printf( "	jbr	L%d\n", lab );
/* tbl */
	else {
		if( o > UGT ) cerror( "bad conditional branch: %s", opst[o] );
		printf( ccbranches[o-EQ], lab );
		}
	}

nextcook( p, cookie ) NODE *p; {
	/* we have failed to match p with cookie; try another */
	if( cookie == FORREW ) return( 0 );  /* hopeless! */
	if( !(cookie&(INTAREG|INTBREG)) ) return( INTAREG|INTBREG );
	if( !(cookie&INTEMP) && asgop(p->in.op) ) return( INTEMP|INAREG|INTAREG|INTBREG|INBREG );
	return( FORREW );
	}

lastchance( p, cook ) NODE *p; {
	/* forget it! */
	return(0);
	}

optim2( p ) register NODE *p; {
	/* do local tree transformations and optimizations */

	register NODE *r;

	switch( p->in.op ) {

	case AND:
		/* commute L and R to eliminate compliments and constants */
		if( (p->in.left->in.op==ICON&&p->in.left->in.name[0]==0) || p->in.left->in.op==COMPL ) {
			r = p->in.left;
			p->in.left = p->in.right;
			p->in.right = r;
			}
	case ASG AND:
		/* change meaning of AND to ~R&L - bic on pdp11 */
		r = p->in.right;
		if( r->in.op==ICON && r->in.name[0]==0 ) { /* compliment constant */
			r->tn.lval = ~r->tn.lval;
			}
		else if( r->in.op==COMPL ) { /* ~~A => A */
			r->in.op = FREE;
			p->in.right = r->in.left;
			}
		else { /* insert complement node */
			p->in.right = talloc();
			p->in.right->in.op = COMPL;
			p->in.right->in.rall = NOPREF;
			p->in.right->in.type = r->in.type;
			p->in.right->in.left = r;
			p->in.right->in.right = NULL;
			}
		break;

		}
	}

NODE * addroreg(l)
				/* OREG was built in clocal()
				 * for an auto or formal parameter
				 * now its address is being taken
				 * local code must unwind it
				 * back to PLUS/MINUS REG ICON
				 * according to local conventions
				 */
{
	cerror("address of OREG taken");
}



# ifndef ONEPASS
main( argc, argv ) char *argv[]; {
	return( mainp2( argc, argv ) );
	}
# endif


/* added by jwf */
struct functbl {
	int fop;
	TWORD ftype;
	char *func;
	} opfunc[] = {
	DIV,		TANY,	"udiv",
	MOD,		TANY,	"urem",
	ASG DIV,	TANY,	"udiv",
	ASG MOD,	TANY,	"urem",
	0,	0,	0 };

hardops(p)  register NODE *p; {
	/* change hard to do operators into function calls.  */
	register NODE *q;
	register struct functbl *f;
	register o;
	register TWORD t;

	o = p->in.op;
	t = p->in.type;
	if( t!=UNSIGNED && t!=ULONG ) return;

	for( f=opfunc; f->fop; f++ ) {
		if( o==f->fop ) goto convert;
		}
	return;

	/* need to rewrite tree for ASG OP */
	/* must change ASG OP to a simple OP */
	convert:
	if( asgop( o ) ) {
		q = talloc();
		switch( p->in.op ) {
			case ASG DIV:
				q->in.op = DIV;
				break;
			case ASG MOD:
				q->in.op = MOD;
				break;
		}
		q->in.rall = NOPREF;
		q->in.type = p->in.type;
		q->in.left = tcopy(p->in.left);
		q->in.right = p->in.right;
		p->in.op = ASSIGN;
		p->in.right = q;
		zappost(q->in.left); /* remove post-INCR(DECR) from new node */
		fixpre(q->in.left);	/* change pre-INCR(DECR) to +/-	*/
		p = q;

	}

	/* build comma op for args to function */
	q = talloc();
	q->in.op = CM;
	q->in.rall = NOPREF;
	q->in.type = INT;
	q->in.left = p->in.left;
	q->in.right = p->in.right;
	p->in.op = CALL;
	p->in.right = q;

	/* put function name in left node of call */
	p->in.left = q = talloc();
	q->in.op = ICON;
	q->in.rall = NOPREF;
	q->in.type = INCREF( FTN + p->in.type );
#ifndef FLEXNAMES
	strcpy( q->in.name, f->func );
#else
	q->in.name = f->func;
#endif
	q->tn.lval = 0;
	q->tn.rval = 0;

	return;

	}

zappost(p) NODE *p; {
	/* look for ++ and -- operators and remove them */

	register o, ty;
	register NODE *q;
	o = p->in.op;
	ty = optype( o );

	switch( o ){

	case INCR:
	case DECR:
			q = p->in.left;
			p->in.right->in.op = FREE;  /* zap constant */
			ncopy( p, q );
			q->in.op = FREE;
			return;

		}

	if( ty == BITYPE ) zappost( p->in.right );
	if( ty != LTYPE ) zappost( p->in.left );
}

fixpre(p) NODE *p; {

	register o, ty;
	o = p->in.op;
	ty = optype( o );

	switch( o ){

	case ASG PLUS:
			p->in.op = PLUS;
			break;
	case ASG MINUS:
			p->in.op = MINUS;
			break;
		}

	if( ty == BITYPE ) fixpre( p->in.right );
	if( ty != LTYPE ) fixpre( p->in.left );
}

myreader(p) register NODE *p; {
	walkf( p, hardops );	/* convert ops to function calls */
	canon( p );		/* expands r-vals for fileds */
	walkf( p, optim2 );
	/* jwf toff = 0;  /* stack offset swindle */
	}


