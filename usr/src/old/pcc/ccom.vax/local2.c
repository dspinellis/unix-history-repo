# ifndef lint
static char *sccsid ="@(#)local2.c	1.34 (Berkeley) %G%";
# endif

# include "pass2.h"
# include <ctype.h>

# define putstr(s)	fputs((s), stdout)

# ifdef FORT
int ftlab1, ftlab2;
# endif
/* a lot of the machine dependent parts of the second pass */

# define BITMASK(n) ((1L<<n)-1)

/*ARGSUSED*/
where(c){
	fprintf( stderr, "%s, line %d: ", filename, lineno );
	}

lineid( l, fn ) char *fn; {
	/* identify line l and file fn */
	printf( "#	line %d, file %s\n", l, fn );
	}


eobl2(){
	register OFFSZ spoff;	/* offset from stack pointer */
#ifndef FORT
	extern int ftlab1, ftlab2;
#endif

	spoff = maxoff;
	if( spoff >= AUTOINIT ) spoff -= AUTOINIT;
	spoff /= SZCHAR;
	SETOFF(spoff,4);
#ifdef FORT
#ifndef FLEXNAMES
	printf( "	.set	.F%d,%ld\n", ftnno, spoff );
#else
	/* SHOULD BE L%d ... ftnno but must change pc/f77 */
	printf( "	.set	LF%d,%ld\n", ftnno, spoff );
#endif
#else
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

	if(asgop(o))
		o = NOASG o;
	for( q = ioptab;  q->opmask>=0; ++q ){
		if( q->opmask == o ){
			printf( "%s%c", q->opstring, tolower(f));
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
			return(SZSHORT/SZCHAR);

		case DOUBLE:
			return(SZDOUBLE/SZCHAR);

		default:
			return(SZINT/SZCHAR);
		}
}

mixtypes(p, q) NODE *p, *q;
{
	register TWORD tp, tq;

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
			putchar('d');
			return;

		case FLOAT:
			putchar('f');
			return;

		case LONG:
		case ULONG:
		case INT:
		case UNSIGNED:
			putchar('l');
			return;

		case SHORT:
		case USHORT:
			putchar('w');
			return;

		case CHAR:
		case UCHAR:
			putchar('b');
			return;

		default:
			if ( !ISPTR( n->in.type ) ) cerror("zzzcode- bad type");
			else {
				putchar('l');
				return;
				}
		}
}

zzzcode( p, c ) register NODE *p; {
	register int m;
	int val;
	switch( c ){

	case 'N':  /* logical ops, turned into 0-1 */
		/* use register given by register 1 */
		cbgen( 0, m=getlab(), 'I' );
		deflab( p->bn.label );
		printf( "	clrl	%s\n", rnames[getlr( p, '1' )->tn.rval] );
		deflab( m );
		return;

	case 'P':
		cbgen( p->in.op, p->bn.label, c );
		return;

	case 'A':
	case 'V':
		sconv( p, c == 'V' );
		return;

	case 'G':	/* i *= f; asgops with int lhs and float rhs */
		{
		register NODE *l, *r, *s;
		int rt;

		l = p->in.left;
		r = p->in.right;
		s = talloc();
		rt = r->in.type;

		s->in.op = SCONV;
		s->in.left = l;
		s->in.type = rt;
		zzzcode(s, 'A');
		putstr("\n\t");

		hopcode(rt == FLOAT ? 'F' : 'D', p->in.op);
		putstr("2\t");
		adrput(r);
		putchar(',');
		adrput(resc);
		putstr("\n\t");

		s->in.op = ASSIGN;
		s->in.left = l;
		s->in.right = resc;
		s->in.type = l->in.type;
		zzzcode(s, 'A');

		s->in.op = FREE;
		return;
		}

	case 'J':	/* unsigned DIV/MOD with constant divisors */
		{
		register int ck = INAREG;
		int label1, label2;

		/* case constant <= 1 is handled by optim() in pass 1 */
		/* case constant < 0x80000000 is handled in table */
		switch( p->in.op ) {
		/* case DIV: handled in optim2() */
		case MOD:
			if( p->in.left->in.op == REG &&
			    p->in.left->tn.rval == resc->tn.rval )
				goto asgmod;
			label1 = getlab();
			expand(p, ck, "movl\tAL,A1\n\tcmpl\tA1,AR\n");
			printf("\tjlssu\tL%d\n", label1);
			expand(p, ck, "\tsubl2\tAR,A1\n");
			printf("L%d:", label1);
			break;
		case ASG DIV:
			label1 = getlab();
			label2 = getlab();
			expand(p, ck, "cmpl\tAL,AR\n");
			printf("\tjgequ\tL%d\n", label1);
			expand(p, ck, "\tmovl\t$1,AL\n");
			printf("\tjbr\tL%d\nL%d:\n", label2, label1);
			expand(p, ck, "\tclrl\tAL\n");
			printf("L%d:", label2);
			break;
		case ASG MOD:
		asgmod:
			label1 = getlab();
			expand(p, ck, "cmpl\tAL,AR\n");
			printf("\tjlssu\tL%d\n", label1);
			expand(p, ck, "\tsubl2\tAR,AL\n");
			printf("L%d:", label1);
			break;
			}
		return;
		}

	case 'B':	/* get oreg value in temp register for left shift */
		{
		register NODE *r;
		if (xdebug) eprint(p, 0, &val, &val);
		r = p->in.right;
		if( tlen(r) == SZINT/SZCHAR && r->in.type != FLOAT )
			putstr("movl");
		else {
			putstr(ISUNSIGNED(r->in.type) ? "movz" : "cvt");
			prtype(r);
			putchar('l');
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
		putchar('\n');
		putchar('\t');

	case 'E':	/* INCR and DECR, FOREFF */
		if (p->in.right->in.op == ICON && p->in.right->tn.lval == 1)
			{
			putstr(p->in.op == INCR ? "inc" : "dec");
			prtype(p->in.left);
			putchar('\t');
			adrput(p->in.left);
			return;
			}
		putstr(p->in.op == INCR ? "add" : "sub");
		prtype(p->in.left);
		putchar('2');
		putchar('\t');
		adrput(p->in.right);
		putchar(',');
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

		if ( ty==DOUBLE) putchar('d');
		else if ( ty==FLOAT ) putchar('f');
		else putchar('l');
		return;
		}

	case 'L':	/* type of left operand */
	case 'R':	/* type of right operand */
		{
		register NODE *n;
		extern int xdebug;

		n = getlr( p, c );
		if (xdebug) printf("->%d<-", n->in.type);

		prtype(n);
		return;
		}

	case 'Z':	/* AND for CC with ICON -- lval is complemented */
		{
		register NODE *l, *r;

		l = getlr( p, 'L' );
		r = getlr( p, 'R' );
		m = (1 << tlen(l) * SZCHAR) - 1;
		r->tn.lval = ~r->tn.lval;
		if( (l->in.type == CHAR || l->in.type == SHORT) &&
		    (r->tn.lval & ~m) ) {
			putstr("cvt");
			prtype(l);
			putstr("l\t");
			adrput(l);
			putchar(',');
			adrput(resc);
			putstr("\n\t");
			resc->tn.type = INT;
			l = resc;
			}
		else if( l->in.type == UCHAR || l->in.type == USHORT )
			/* remove trash left over from complementing */
			r->tn.lval &= m;
		putstr("bit");
		prtype(l);
		printf("\t$%ld", r->tn.lval);
		putchar(',');
		adrput(l);
		return;
		}

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
		stasg(p);
		break;

	default:
		cerror( "illegal zzzcode" );
		}
	}

stasg(p)
	register NODE *p;
{
	register NODE *l, *r;
	register size;

	if( p->in.op == STASG ){
		l = p->in.left;
		r = p->in.right;

		}
	else if( p->in.op == STARG ){  /* store an arg into a temporary */
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
			putstr("	movb	");
			break;
		case 2:
			putstr("	movw	");
			break;
		case 4:
			putstr("	movl	");
			break;
		case 8:
			putstr("	movq	");
			break;
		default:
			printf("	movc3	$%d,", size);
			break;
	}
	adrput(r);
	if( p->in.op == STASG ){
		putchar(',');
		adrput(l);
		putchar('\n');
		}
	else
		putstr(",(sp)\n");

	if( r->in.op == NAME ) r->in.op = ICON;
	else if( r->in.op == OREG ) r->in.op = REG;
	}

NODE *makearg( ty ) int ty; {
	register NODE *p, *q;

	/* build a -(sp) operand */
	p = talloc();
	p->in.op = REG;
	/* the type needn't be right, just consistent */
	p->in.type = INCREF(ty);
	p->tn.rval = SP;
	p->tn.lval = 0;
	q = talloc();
	q->in.op = ASG MINUS;
	q->in.type = INCREF(ty);
	q->in.left = p;
	p = talloc();
	p->in.op = ICON;
	p->in.type = INT;
	p->tn.name = "";
	p->tn.lval = szty(ty) * (SZINT/SZCHAR);
	q->in.right = p;
	p = talloc();
	p->in.op = UNARY MUL;
	p->in.left = q;
	return( p );
	}

sconv( p, forarg ) register NODE *p; {
	register NODE *l, *r;
	int m, val;

	if (xdebug) eprint(p, 0, &val, &val);
	r = getlr(p, 'R');
	if (p->in.op == ASSIGN)
		l = getlr(p, 'L');
	else if (p->in.op == SCONV) {
		m = r->in.type;
		if (forarg)
			l = makearg( m );
		else
			l = resc;
		l->in.type = m;
		r = getlr(p, 'L');
		}
	else {		/* OPLTYPE */
		m = (r->in.type==FLOAT || r->in.type==DOUBLE ? r->in.type : INT);
		if (forarg)
			l = makearg( m );
		else
			l = resc;
		l->in.type = m;
		}
	if (r->in.op == ICON)
		if (r->in.name[0] == '\0') {
			if (r->tn.lval == 0 && !forarg) {
				putstr("clr");
				prtype(l);
				putchar('\t');
				adrput(l);
				goto cleanup;
				}
			if (r->tn.lval < 0 && r->tn.lval >= -63) {
				putstr("mneg");
				prtype(l);
				r->tn.lval = -r->tn.lval;
				goto ops;
				}
			if (r->tn.lval < 0)
				r->in.type = r->tn.lval >= -128 ? CHAR
					: (r->tn.lval >= -32768 ? SHORT
					: INT);
			else if (l->in.type == FLOAT ||
			    l->in.type == DOUBLE)
				r->in.type = r->tn.lval <= 63 ? INT
					: (r->tn.lval <= 127 ? CHAR
					: (r->tn.lval <= 32767 ? SHORT
					: INT));
			else
				r->in.type = r->tn.lval <= 63 ? INT
					: (r->tn.lval <= 127 ? CHAR
					: (r->tn.lval <= 255 ? UCHAR
					: (r->tn.lval <= 32767 ? SHORT
					: (r->tn.lval <= 65535 ? USHORT
					: INT))));
			if (forarg && r->in.type == INT) {
				putstr("pushl\t");
				adrput(r);
				goto cleanup;
				}
			}
		else {
			if (forarg && tlen(r) == SZINT/SZCHAR) {
				putstr("pushl\t");
				adrput(r);
				goto cleanup;
				}
			putstr("moval\t");
			acon(r);
			putchar(',');
			adrput(l);
			goto cleanup;
			}

	if (p->in.op == SCONV &&
	    !(l->in.type == FLOAT || l->in.type == DOUBLE) &&
	    !mixtypes(l, r)) {
		/*
		 * Because registers must always contain objects
		 * of the same width as INTs, we may have to
		 * perform two conversions to get an INT.  Can
		 * the conversions be collapsed into one?
		 */
		if (m = collapsible(l, r))
			r->in.type = m;
		else {
			/*
			 * Two steps are required.
			 */
			NODE *x;

			if (forarg) {
				x = resc;
				x->in.type = l->in.type;
				}
			else {
				x = &resc[1];
				*x = *l;
				}

			if (tlen(x) > tlen(r) && ISUNSIGNED(r->in.type))
				putstr("movz");
			else
				putstr("cvt");
			prtype(r);
			prtype(x);
			putchar('\t');
			adrput(r);
			putchar(',');
			adrput(x);
			putchar('\n');
			putchar('\t');
			r = x;
			}
		l->in.type = (ISUNSIGNED(l->in.type) ? UNSIGNED : INT);
		}

	if ((r->in.type == UNSIGNED || r->in.type == ULONG) &&
	    mixtypes(l, r)) {
		int label1, label2;
		NODE *x = NULL;

#if defined(FORT) || defined(SPRECC)
		if (forarg)
#else
		if (forarg || l == resc)
#endif
			{
			/* compute in register, convert to double when done */
			x = l;
			l = resc;
			l->in.type = x->in.type;
			}

		label1 = getlab();
		label2 = getlab();

		putstr("movl\t");
		adrput(r);
		putchar(',');
		adrput(l);
		putstr("\n\tjbsc\t$31,");
		adrput(l);
		printf(",L%d\n\tcvtl", label1);
		prtype(l);
		putchar('\t');
		adrput(l);
		putchar(',');
		adrput(l);
		printf("\n\tjbr\tL%d\nL%d:\n\tcvtl", label2, label1);
		prtype(l);
		putchar('\t');
		adrput(l);
		putchar(',');
		adrput(l);
		putstr("\n\tadd");
		prtype(l);
		putstr("2\t$0");
		prtype(l);
		putstr("2.147483648e9,");
		adrput(l);
		printf("\nL%d:", label2);

		if (!forarg && (l->in.type == DOUBLE || l != resc))
			goto cleanup;
		if (x != NULL) {
			if (l == x) {
				r = &resc[1];
				*r = *l;
				}
			else {
				r = l;
				l = x;
				}
			l->in.type = DOUBLE;
			}
		putstr("\n\t");
		}

	if( (l->in.type == FLOAT || l->in.type == DOUBLE) &&
	    (r->in.type == UCHAR || r->in.type == USHORT) ) {
		/* skip unnecessary unsigned to floating conversion */
#if defined(FORT) || defined(SPRECC)
		if (forarg)
#else
		if (forarg || l == resc)
#endif
			l->in.type = DOUBLE;
		putstr("movz");
		prtype(r);
		putstr("l\t");
		adrput(r);
		putchar(',');
		adrput(resc);
		putstr("\n\t");
		if (l == resc) {
			r = &resc[1];
			*r = *l;
			}
		else
			r = resc;
		r->in.type = INT;
		}

#if defined(FORT) || defined(SPRECC)
	if (forarg && l->in.type == FLOAT)
#else
	if ((forarg || l == resc) && l->in.type == FLOAT)
#endif
		{
		/* perform an implicit conversion to double */
		l->in.type = DOUBLE;
		if (r->in.type != FLOAT &&
		    r->in.type != CHAR &&
		    r->in.type != SHORT) {
			/* trim bits from the mantissa */
			putstr("cvt");
			prtype(r);
			putstr("f\t");
			adrput(r);
			putchar(',');
			adrput(resc);
			putstr("\n\t");
			if (l == resc) {
				r = &resc[1];
				*r = *l;
				}
			else
				r = resc;
			r->in.type = FLOAT;
			}
		}

	if (!mixtypes(l,r)) {
		if (tlen(l) == tlen(r)) {
			if (forarg && tlen(l) == SZINT/SZCHAR) {
				putstr("pushl\t");
				adrput(r);
				goto cleanup;
				}
			putstr("mov");
#ifdef FORT
			if (Oflag)
				prtype(l);
			else {
				if (l->in.type == DOUBLE)
					putchar('q');
				else if(l->in.type == FLOAT)
					putchar('l');
				else
					prtype(l);
				}
#else
			prtype(l);
#endif FORT
			goto ops;
			}
		else if (tlen(l) > tlen(r) && ISUNSIGNED(r->in.type))
			putstr("movz");
		else
			putstr("cvt");
		}
	else
		putstr("cvt");
	prtype(r);
	prtype(l);
ops:
	putchar('\t');
	adrput(r);
	putchar(',');
	adrput(l);

cleanup:
	if (forarg)
		tfree(l);
	}

/*
 * collapsible(dest, src) -- if a conversion with a register destination
 *	can be accomplished in one instruction, return the type of src
 *	that will do the job correctly; otherwise return 0.  Note that
 *	a register must always end up having type INT or UNSIGNED.
 */
int
collapsible(dest, src)
NODE *dest, *src;
{
	int st = src->in.type;
	int dt = dest->in.type;
	int newt = 0;

	/*
	 * Are there side effects of evaluating src?
	 * If the derived type will not be the same size as src,
	 * we may have to use two steps.
	 */
	if (tlen(src) > tlen(dest)) {
		if (tshape(src, STARREG))
			return (0);
		if (src->in.op == OREG && R2TEST(src->tn.rval))
			return (0);
		}

	/*
	 * Can we get an object of dest's type by punning src?
	 * Praises be to great Cthulhu for little-endian machines...
	 */
	if (st == CHAR && dt == USHORT)
		/*
		 * Special case -- we must sign-extend to 16 bits.
		 */
		return (0);

	if (tlen(src) < tlen(dest))
		newt = st;
	else
		newt = dt;

	return (newt);
	}

rmove( rt, rs, t ) TWORD t; {
	printf( "	%s	%s,%s\n",
#ifdef FORT
		!Oflag ? (t==DOUBLE ? "movq" : "movl") :
#endif
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
	}

/*ARGSUSED*/
rewfld( p ) NODE *p; {
	return(1);
	}

/*ARGSUSED*/
callreg(p) NODE *p; {
	return( R0 );
	}

base( p ) register NODE *p; {
	register int o = p->in.op;

	if( o==ICON && p->tn.name[0] != '\0' ) return( 100 ); /* ie no base reg */
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
	if( o==NAME ) return( 100 + 0200*1 );
	return( -1 );
	}

offset( p, tyl ) register NODE *p; int tyl; {

	if( tyl==1 &&
	    p->in.op==REG &&
	    (p->in.type==INT || p->in.type==UNSIGNED) )
		return( p->tn.rval );
	if( p->in.op==LS &&
	    p->in.left->in.op==REG &&
	    (p->in.left->in.type==INT || p->in.left->in.type==UNSIGNED) &&
	    p->in.right->in.op==ICON &&
	    p->in.right->in.name[0]=='\0' &&
	    (1<<p->in.right->tn.lval)==tyl)
		return( p->in.left->tn.rval );
	if( tyl==2 &&
	    p->in.op==PLUS &&
	    (p->in.left->in.type==INT || p->in.left->in.type==UNSIGNED) &&
	    p->in.left->in.op==REG &&
	    p->in.right->in.op==REG &&
	    p->in.left->tn.rval==p->in.right->tn.rval )
		return( p->in.left->tn.rval );
	return( -1 );
	}

makeor2( p, q, b, o) register NODE *p, *q; register int b, o; {
	register NODE *t;
	NODE *f;

	p->in.op = OREG;
	f = p->in.left; 	/* have to free this subtree later */

	/* init base */
	switch (q->in.op) {
		case ICON:
		case REG:
		case OREG:
		case NAME:
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
	{
		register int i;
		for(i=0; i<NCHNAM; ++i)
			p->in.name[i] = t->in.name[i];
	}
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

flshape( p ) NODE *p; {
	register int o = p->in.op;

	return( o == REG || o == NAME || o == ICON ||
		(o == OREG && (!R2TEST(p->tn.rval) || tlen(p) == 1)) );
	}

/* INTEMP shapes must not contain any temporary registers */
shtemp( p ) register NODE *p; {
	int r;

	if( p->in.op == STARG ) p = p->in.left;

	switch (p->in.op) {
	case REG:
		return( !istreg(p->tn.rval) );
	case OREG:
		r = p->tn.rval;
		if( R2TEST(r) ) {
			if( istreg(R2UPK1(r)) )
				return(0);
			r = R2UPK2(r);
			}
		return( !istreg(r) );
	case UNARY MUL:
		p = p->in.left;
		return( p->in.op != UNARY MUL && shtemp(p) );
		}

	if( optype( p->in.op ) != LTYPE ) return(0);
	return(1);
	}

shumul( p ) register NODE *p; {
	register int o;
	extern int xdebug;

	if (xdebug) {
		int val;
		printf("shumul:\n");
		eprint(p, 0, &val, &val);
		}

	o = p->in.op;
	if( o == NAME || (o == OREG && !R2TEST(p->tn.rval)) || o == ICON ) return( STARNM );

	if( ( o == INCR || o == ASG MINUS ) &&
	    ( p->in.left->in.op == REG && p->in.right->in.op == ICON ) &&
	    p->in.right->in.name[0] == '\0' )
		{
		switch (p->in.type)
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
				if ( ISPTR(p->in.type) &&
				     ISPTR(DECREF(p->in.type)) ) {
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
	putchar( '$' );
	printf( CONFMT, val );
	}

conput( p ) register NODE *p; {
	switch( p->in.op ){

	case ICON:
		acon( p );
		return;

	case REG:
		putstr( rnames[p->tn.rval] );
		return;

	default:
		cerror( "illegal conput" );
		}
	}

/*ARGSUSED*/
insput( p ) NODE *p; {
	cerror( "insput" );
	}

upput( p, size ) NODE *p; int size; {
	if( size == SZLONG && p->in.op == REG ) {
		putstr( rnames[p->tn.rval + 1] );
		return;
		}
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
		putchar( '$' );
		acon( p );
		return;

	case REG:
		putstr( rnames[p->tn.rval] );
		return;

	case OREG:
		r = p->tn.rval;
		if( R2TEST(r) ){ /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if( flags & 1 ) putchar('*');
			if( flags & 4 ) putchar('-');
			if( p->tn.lval != 0 || p->in.name[0] != '\0' ) acon(p);
			if( R2UPK1(r) != 100) printf( "(%s)", rnames[R2UPK1(r)] );
			if( flags & 2 ) putchar('+');
			printf( "[%s]", rnames[R2UPK2(r)] );
			return;
			}
		if( r == AP ){  /* in the argument region */
			if( p->in.name[0] != '\0' ) werror( "bad arg temp" );
			printf( CONFMT, p->tn.lval );
			putstr( "(ap)" );
			return;
			}
		if( p->tn.lval != 0 || p->in.name[0] != '\0') acon( p );
		printf( "(%s)", rnames[p->tn.rval] );
		return;

	case UNARY MUL:
		/* STARNM or STARREG found */
		if( tshape(p, STARNM) ) {
			putchar( '*' );
			adrput( p->in.left);
			}
		else {	/* STARREG - really auto inc or dec */
			register NODE *q;

			q = p->in.left;
			if( q->in.right->tn.lval != tlen(p) )
				cerror("adrput: bad auto-increment/decrement");
			printf("%s(%s)%s", (q->in.op==INCR ? "" : "-"),
				rnames[q->in.left->tn.rval], 
				(q->in.op==INCR ? "+" : "") );
			p->in.op = OREG;
			p->tn.rval = q->in.left->tn.rval;
			p->tn.lval = (q->in.op == INCR ? -q->in.right->tn.lval : 0);
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

	if( p->in.name[0] == '\0' )
		printf( CONFMT, p->tn.lval);
	else {
#ifndef FLEXNAMES
		printf( "%.8s", p->in.name );
#else
		putstr( p->in.name );
#endif
		if( p->tn.lval != 0 ) {
			putchar( '+' );
			printf( CONFMT, p->tn.lval );
			}
		}
	}

genscall( p, cookie ) register NODE *p; {
	/* structure valued call */
	return( gencall( p, cookie ) );
	}

/* tbl */
int gc_numbytes;
/* tbl */

/*ARGSUSED*/
gencall( p, cookie ) register NODE *p; {
	/* generate the call given by p */
	register NODE *p1;
	register int temp, temp1;
	register int m;

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
		genargs( p->in.right );
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

	return(m != MDONE);
	}

/* tbl */
char *
ccbranches[] = {
	"eql",
	"neq",
	"leq",
	"lss",
	"geq",
	"gtr",
	"lequ",
	"lssu",
	"gequ",
	"gtru",
	};
/* tbl */

/*ARGSUSED*/
cbgen( o, lab, mode ) { /*   printf conditional and unconditional branches */

	if( o != 0 && ( o < EQ || o > UGT ) )
		cerror( "bad conditional branch: %s", opst[o] );
	printf( "	j%s	L%d\n", o == 0 ? "br" : ccbranches[o-EQ], lab );
	}

nextcook( p, cookie ) NODE *p; {
	/* we have failed to match p with cookie; try another */
	if( cookie == FORREW ) return( 0 );  /* hopeless! */
	if( !(cookie&(INTAREG|INTBREG)) ) return( INTAREG|INTBREG );
	if( !(cookie&INTEMP) && asgop(p->in.op) ) return( INTEMP|INAREG|INTAREG|INTBREG|INBREG );
	return( FORREW );
	}

/*ARGSUSED*/
lastchance( p, cook ) NODE *p; {
	/* forget it! */
	return(0);
	}

optim2( p ) register NODE *p; {
	/* do local tree transformations and optimizations */

	int o;
	int i, mask;
	register NODE *l, *r;

	switch( o = p->in.op ) {

	case AND:
		/* commute L and R to eliminate complements and constants */
		if( (l = p->in.left)->in.op == ICON && l->in.name[0] == 0 ||
		    l->in.op == COMPL ) {
			p->in.left = p->in.right;
			p->in.right = l;
			}
	case ASG AND:
		/* change meaning of AND to ~R&L - bic on pdp11 */
		r = p->in.right;
		if( r->in.op==ICON && r->in.name[0]==0 ) {
			/* check for degenerate operations */
			l = p->in.left;
			mask = (1 << tlen(l) * SZCHAR) - 1;
			if( ISUNSIGNED(r->in.type) ) {
				i = (~r->tn.lval & mask);
				if( i == 0 ) {
					r->in.op = FREE;
					ncopy(p, l);
					l->in.op = FREE;
					break;
					}
				else if( i == mask )
					goto zero;
				else
					r->tn.lval = i;
				break;
				}
			else if( r->tn.lval == mask &&
				 tlen(l) < SZINT/SZCHAR ) {
				r->in.op = SCONV;
				r->in.left = l;
				r->in.right = 0;
				r->in.type = ENUNSIGN(l->in.type);
				r->in.su = l->in.su > 1 ? l->in.su : 1;
				ncopy(p, r);
				p->in.left = r;
				p->in.type = INT;
				break;
				}
			/* complement constant */
			r->tn.lval = ~r->tn.lval;
			}
		else if( r->in.op==COMPL ) { /* ~~A => A */
			r->in.op = FREE;
			p->in.right = r->in.left;
			}
		else { /* insert complement node */
			p->in.right = l = talloc();
			l->in.op = COMPL;
			l->in.rall = NOPREF;
			l->in.type = r->in.type;
			l->in.left = r;
			l->in.right = NULL;
			}
		break;

	case SCONV:
		l = p->in.left;
#if defined(FORT) || defined(SPRECC)
		if( p->in.type == FLOAT || p->in.type == DOUBLE ||
		    l->in.type == FLOAT || l->in.type == DOUBLE )
			return;
#else
		if( mixtypes(p, l) ) return;
#endif
		if( l->in.op == PCONV )
			return;
		if( (l->in.op == CALL || l->in.op == UNARY CALL) &&
		    l->in.type != INT && l->in.type != UNSIGNED )
			return;

		/* Only trust it to get it right if the size is the same */
		if( tlen(p) != tlen(l) )
			return;

		/* clobber conversion */
		if( l->in.op != FLD )
			l->in.type = p->in.type;
		ncopy( p, l );
		l->in.op = FREE;

		break;

	case ASSIGN:
		/*
		 * Conversions are equivalent to assignments;
		 * when the two operations are combined,
		 * we can sometimes zap the conversion.
		 */
		r = p->in.right;
		l = p->in.left;
		if ( r->in.op == SCONV &&
		     !mixtypes(l, r) &&
		     l->in.op != FLD &&
		     tlen(l) == tlen(r) ) {
				p->in.right = r->in.left;
				r->in.op = FREE;
			}
		break;

	case ULE:
	case ULT:
	case UGE:
	case UGT:
		p->in.op -= (UGE-GE);
		if( degenerate(p) )
			break;
		p->in.op += (UGE-GE);
		break;

	case EQ:
	case NE:
	case LE:
	case LT:
	case GE:
	case GT:
		if( p->in.left->in.op == SCONV &&
		    p->in.right->in.op == SCONV ) {
			l = p->in.left;
			r = p->in.right;
			if( l->in.type == DOUBLE &&
			    l->in.left->in.type == FLOAT &&
			    r->in.left->in.type == FLOAT ) {
				/* nuke the conversions */
				p->in.left = l->in.left;
				p->in.right = r->in.left;
				l->in.op = FREE;
				r->in.op = FREE;
				}
			/* more? */
			}
		(void) degenerate(p);
		break;

	case DIV:
		if( p->in.right->in.op == ICON &&
		    p->in.right->tn.name[0] == '\0' &&
		    ISUNSIGNED(p->in.right->in.type) &&
		    (unsigned) p->in.right->tn.lval >= 0x80000000 ) {
			/* easy to do here, harder to do in zzzcode() */
			p->in.op = UGE;
			break;
			}
	case MOD:
	case ASG DIV:
	case ASG MOD:
		/*
		 * optimize DIV and MOD
		 *
		 * basically we spot UCHAR and USHORT and try to do them
		 * as signed ints...  apparently div+mul+sub is always
		 * faster than ediv for finding MOD on the VAX, when
		 * full unsigned MOD isn't needed.
		 *
		 * a curious fact: for MOD, cmp+sub and cmp+sub+cmp+sub
		 * are faster for unsigned dividend and a constant divisor
		 * in the right range (.5 to 1 of dividend's range for the
		 * first, .333+ to .5 for the second).  full unsigned is
		 * already done cmp+sub in the appropriate case; the
		 * other cases are less common and require more ambition.
		 */
		if( degenerate(p) )
			break;
		l = p->in.left;
		r = p->in.right;
		if( !ISUNSIGNED(r->in.type) ||
		    tlen(l) >= SZINT/SZCHAR ||
		    !(tlen(r) < SZINT/SZCHAR ||
		      (r->in.op == ICON && r->tn.name[0] == '\0')) )
			break;
		if( r->in.op == ICON )
			r->tn.type = INT;
		else {
			NODE *t = talloc();
			t->in.left = r;
			r = t;
			r->in.op = SCONV;
			r->in.type = INT;
			r->in.right = 0;
			p->in.right = r;
			}
		if( o == DIV || o == MOD ) {
			NODE *t = talloc();
			t->in.left = l;
			l = t;
			l->in.op = SCONV;
			l->in.type = INT;
			l->in.right = 0;
			p->in.left = l;
			}
		/* handle asgops in table */
		break;

	case RS:
	case ASG RS:
	case LS:
	case ASG LS:
		/* pick up degenerate shifts */
		l = p->in.left;
		r = p->in.right;
		if( !(r->in.op == ICON && r->tn.name[0] == '\0') )
			break;
		i = r->tn.lval;
		if( i < 0 )
			/* front end 'fixes' this? */
			if( o == LS || o == ASG LS )
				o += (RS-LS);
			else
				o += (LS-RS);
		if( (o == RS || o == ASG RS) &&
		    !ISUNSIGNED(l->in.type) )
			/* can't optimize signed right shifts */
			break;
		if( o == LS ) {
			if( i < SZINT )
				break;
			}
		else {
			if( i < tlen(l) * SZCHAR )
				break;
			}
	zero:
		if( !asgop( o ) )
			if( tshape(l, SAREG|SNAME|SCON|SOREG|STARNM) ) {
				/* no side effects */
				tfree(l);
				ncopy(p, r);
				r->in.op = FREE;
				p->tn.lval = 0;
				}
			else {
				p->in.op = COMOP;
				r->tn.lval = 0;
				}
		else {
			p->in.op = ASSIGN;
			r->tn.lval = 0;
			}
		break;
		}
	}

degenerate(p) register NODE *p; {
	int o;
	int result, i;
	int lower, upper;
	register NODE *l, *r;

	/*
	 * try to keep degenerate comparisons with constants
	 * out of the table.
	 */
	r = p->in.right;
	l = p->in.left;
	if( r->in.op != ICON ||
	    r->tn.name[0] != '\0' ||
	    tlen(l) >= tlen(r) )
		return (0);
	switch( l->in.type ) {
	case CHAR:
		lower = -(1 << SZCHAR - 1);
		upper = (1 << SZCHAR - 1) - 1;
		break;
	case UCHAR:
		lower = 0;
		upper = (1 << SZCHAR) - 1;
		break;
	case SHORT:
		lower = -(1 << SZSHORT - 1);
		upper = (1 << SZSHORT - 1) - 1;
		break;
	case USHORT:
		lower = 0;
		upper = (1 << SZSHORT) - 1;
		break;
	default:
		cerror("unsupported type in degenerate()");
		}
	i = r->tn.lval;
	switch( o = p->in.op ) {
	case DIV:
	case ASG DIV:
	case MOD:
	case ASG MOD:
		/* DIV and MOD work like EQ */
	case EQ:
	case NE:
		if( lower == 0 && (unsigned) i > upper )
			result = o == NE;
		else if( i < lower || i > upper )
			result = o == NE;
		else
			return (0);
		break;
	case LT:
	case GE:
		if( lower == 0 && (unsigned) i > upper )
			result = o == LT;
		else if( i <= lower )
			result = o != LT;
		else if( i > upper )
			result = o == LT;
		else
			return (0);
		break;
	case LE:
	case GT:
		if( lower == 0 && (unsigned) i >= upper )
			result = o == LE;
		else if( i < lower )
			result = o != LE;
		else if( i >= upper )
			result = o == LE;
		else
			return (0);
		break;
	default:
		cerror("unknown op in degenerate()");
		}
		
	if( o == MOD || o == ASG MOD ) {
		r->in.op = FREE;
		ncopy(p, l);
		l->in.op = FREE;
		}
	else if( o != ASG DIV && tshape(l, SAREG|SNAME|SCON|SOREG|STARNM) ) {
		/* no side effects */
		tfree(l);
		ncopy(p, r);
		r->in.op = FREE;
		p->tn.lval = result;
		}
	else {
		if( o == ASG DIV )
			p->in.op = ASSIGN;
		else {
			p->in.op = COMOP;
			r->tn.type = INT;
			}
		r->tn.lval = result;
		}
	if( logop(o) )
		p->in.type = INT;

	return (1);
	}

/* added by jwf */
struct functbl {
	int fop;
	TWORD ftype;
	char *func;
	} opfunc[] = {
	DIV,		TANY,	"udiv",
	MOD,		TANY,	"urem",
	ASG DIV,	TANY,	"audiv",
	ASG MOD,	TANY,	"aurem",
	0,	0,	0 };

hardops(p)  register NODE *p; {
	/* change hard to do operators into function calls.  */
	register NODE *q;
	register struct functbl *f;
	register o;
	NODE *old,*temp;

	o = p->in.op;
	if( ! (optype(o)==BITYPE &&
	       (ISUNSIGNED(p->in.left->in.type) ||
		ISUNSIGNED(p->in.right->in.type))) )
		return;

	for( f=opfunc; f->fop; f++ ) {
		if( o==f->fop ) goto convert;
		}
	return;

	convert:
	if( p->in.right->in.op == ICON && p->in.right->tn.name[0] == '\0' )
		/* 'J', 'K' in zzzcode() -- assumes DIV or MOD operations */
		/* save a subroutine call -- use at most 5 instructions */
		return;
	if( tlen(p->in.left) < SZINT/SZCHAR && tlen(p->in.right) < SZINT/SZCHAR )
		/* optim2() will modify the op into an ordinary int op */
		return;
	if( asgop( o ) ) {
		old = NIL;
		switch( p->in.left->in.op ){
		case FLD:
			q = p->in.left->in.left;
			/*
			 * rewrite (lval.fld /= rval); as
			 *  ((*temp).fld = udiv((*(temp = &lval)).fld,rval));
			 * else the compiler will evaluate lval twice.
			 */
			if( q->in.op == UNARY MUL ){
				/* first allocate a temp storage */
				temp = talloc();
				temp->in.op = OREG;
				temp->tn.rval = TMPREG;
				temp->tn.lval = BITOOR(freetemp(1));
				temp->in.type = INCREF(p->in.type);
#ifdef FLEXNAMES
				temp->in.name = "";
#else
				temp->in.name[0] = '\0';
#endif
				old = q->in.left;
				q->in.left = temp;
			}
			/* fall thru ... */

		case REG:
		case NAME:
		case OREG:
			/* change ASG OP to a simple OP */
			q = talloc();
			q->in.op = NOASG p->in.op;
			q->in.rall = NOPREF;
			q->in.type = p->in.type;
			q->in.left = tcopy(p->in.left);
			q->in.right = p->in.right;
			p->in.op = ASSIGN;
			p->in.right = q;
			p = q;
			f -= 2; /* Note: this depends on the table order */
			/* on the right side only - replace *temp with
			 *(temp = &lval), build the assignment node */
			if( old ){
				temp = q->in.left->in.left; /* the "*" node */
				q = talloc();
				q->in.op = ASSIGN;
				q->in.left = temp->in.left;
				q->in.right = old;
				q->in.type = old->in.type;
#ifdef FLEXNAMES
				q->in.name = "";
#else
				q->in.name[0] = '\0';
#endif
				temp->in.left = q;
			}
			break;

		case UNARY MUL:
			/* avoid doing side effects twice */
			q = p->in.left;
			p->in.left = q->in.left;
			q->in.op = FREE;
			break;

		default:
			cerror( "hardops: can't compute & LHS" );
			}
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

	}

zappost(p) NODE *p; {
	/* look for ++ and -- operators and remove them */

	register int o, ty;
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

	register int o, ty;
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

/*ARGSUSED*/
NODE * addroreg(l) NODE *l;
				/* OREG was built in clocal()
				 * for an auto or formal parameter
				 * now its address is being taken
				 * local code must unwind it
				 * back to PLUS/MINUS REG ICON
				 * according to local conventions
				 */
{
	cerror("address of OREG taken");
	/*NOTREACHED*/
}



# ifndef ONEPASS
main( argc, argv ) char *argv[]; {
	return( mainp2( argc, argv ) );
	}
# endif

strip(p) register NODE *p; {
	NODE *q;

	/* strip nodes off the top when no side effects occur */
	for( ; ; ) {
		switch( p->in.op ) {
		case SCONV:			/* remove lint tidbits */
			q = p->in.left;
			ncopy( p, q );
			q->in.op = FREE;
			break;
		/* could probably add a few more here */
		default:
			return;
			}
		}
	}

myreader(p) register NODE *p; {
	strip( p );		/* strip off operations with no side effects */
	canon( p );		/* expands r-vals for fields */
	walkf( p, hardops );	/* convert ops to function calls */
	walkf( p, optim2 );
	}
