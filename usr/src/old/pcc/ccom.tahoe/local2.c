# ifndef lint
static char sccsid[] = "@(#)local2.c	1.26 (Berkeley) %G%";
# endif

# include "pass2.h"
# include <ctype.h>

# define putstr(s)	fputs((s), stdout)
# define ISCHAR(p)	(p->in.type == UCHAR || p->in.type == CHAR)

# ifdef FORT
int ftlab1, ftlab2;
# endif
/* a lot of the machine dependent parts of the second pass */

# define BITMASK(n) ((1L<<n)-1)

# ifndef ONEPASS
/*ARGSUSED*/
where(c){
	fprintf( stderr, "%s, line %d: ", filename, lineno );
	}
# endif

lineid( l, fn ) char *fn; {
	/* identify line l and file fn */
	printf( "#	line %d, file %s\n", l, fn );
	}

int ent_mask;

eobl2(){
	register OFFSZ spoff;	/* offset from stack pointer */
#ifndef FORT
	extern int ftlab1, ftlab2;
#endif

	spoff = maxoff;
	spoff /= SZCHAR;
	SETOFF(spoff,4);
#ifdef FORT
#ifndef FLEXNAMES
	printf( "	.set	.F%d,%ld\n", ftnno, spoff );
#else
	/* SHOULD BE L%d ... ftnno but must change pc/f77 */
	printf( "	.set	LF%d,%ld\n", ftnno, spoff );
#endif
	printf( "	.set	LWM%d,0x%x\n", ftnno, ent_mask&0x1ffc|0x1000);
#else
	printf( "	.set	L%d,0x%x\n", ftnno, ent_mask&0x1ffc);
	printf( "L%d:\n", ftlab1);
	if( maxoff > AUTOINIT )
		printf( "	subl3	$%ld,fp,sp\n", spoff);
	printf( "	jbr 	L%d\n", ftlab2);
#endif
	ent_mask = 0;
	maxargs = -1;
	}

struct hoptab { int opmask; char * opstring; } ioptab[] = {

	PLUS,	"add",
	MINUS,	"sub",
	MUL,	"mul",
	DIV,	"div",
	MOD,	"div",
	OR,	"or",
	ER,	"xor",
	AND,	"and",
	-1,	""    };

hopcode( f, o ){
	/* output the appropriate string from the above table */

	register struct hoptab *q;

	if(asgop(o))
		o = NOASG o;
	for( q = ioptab;  q->opmask>=0; ++q ){
		if( q->opmask == o ){
			if(f == 'E')
				printf( "e%s", q->opstring);
			else
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
	"r12", "fp", "sp", "pc",
	};

/* output register name and update entry mask */
char *
rname(r)
	register int r;
{

	if (!istreg(r))
		ent_mask |= 1<<r;
	return(rnames[r]);
}

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
		printf( "	clrl	%s\n", rname(getlr( p, '1' )->tn.rval) );
		deflab( m );
		return;

	case 'P':
		cbgen( p->in.op, p->bn.label, c );
		return;

	case 'G':	/* i *= f; asgops with int lhs and float rhs */
		{
		register NODE *l, *r, *s;
		int lt, rt;

		l = p->in.left;
		r = p->in.right;
		s = talloc();
		rt = r->in.type;
		lt = l->in.type;

		if (lt != INT && lt != UNSIGNED) {
			s->in.op = SCONV;
			s->in.left = l;
			s->in.type = ISUNSIGNED(lt) ? UNSIGNED : INT;
			zzzcode(s, 'U');
			putstr("\n\t");
		}

		if (ISUNSIGNED(lt)) {
			s->in.op = SCONV;
			s->in.left = lt == UNSIGNED ? l : resc;
			s->in.type = rt;
			unsigned_to_float(s);
		} else {
			putstr("cvl");
			prtype(r);
			putchar('\t');
			adrput(lt == INT ? l : resc);
		}
		putstr("\n\t");

		hopcode(rt == FLOAT ? 'F' : 'D', p->in.op);
		putchar('\t');
		adrput(r);

		if (ISUNSIGNED(lt)) {
			putstr("\n\t");
			s->in.op = SCONV;
			s->in.left = r;		/* we need only the type */
			s->in.type = UNSIGNED;
			float_to_unsigned(s);
		} else {
			putstr("\n\tcv");
			prtype(r);
			putstr("l\t");
			if (lt == INT)
				adrput(l);
			else
				adrput(resc);
		}
		if (lt != INT) {
			putstr("\n\t");
			s->in.op = ASSIGN;
			s->in.left = l;
			s->in.right = resc;
			s->in.type = lt;
			zzzcode(s, 'U');
		}

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

	case 'B':	/* get oreg value in temp register for shift */
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

	case 'C':	/* generate 'call[fs] $bytes' */
		{
		extern int gc_numbytes;
		extern int xdebug;

		if (xdebug) printf("->%d<-",gc_numbytes);

		printf("call%c	$%d",
		 (p->in.left->in.op==ICON && gc_numbytes<60)?'f':'s',
		 gc_numbytes+4);
		/* don't change to double (here's the only place to catch it) */
		if(p->in.type == FLOAT)
			rtyflg = 1;
		return;
		}

	case 'D':	/* INCR and DECR */
		zzzcode(p->in.left, 'U');
		putstr("\n	");

	case 'E':	/* INCR and DECR, FOREFF */
 		if (p->in.right->tn.lval == 1)
			{
			putstr(p->in.op == INCR ? "inc" : "dec");
			prtype(p->in.left);
			putchar('\t');
			adrput(p->in.left);
			return;
			}
		putstr(p->in.op == INCR ? "add" : "sub");
		prtype(p->in.left);
		putstr("2	");
		adrput(p->in.right);
		putchar(',');
		adrput(p->in.left);
		return;

	case 'F':	/* masked constant for fields */
		printf(ACONFMT, (p->in.right->tn.lval&((1<<fldsz)-1))<<fldshf);
		return;

	case 'I':	/* produce value of bitfield assignment */
			/* avoid shifts -- shifts are SLOW on this machine */
			/* XXX this wouldn't be necessary if we were smarter
			       and masked BEFORE shifting XXX */
		{
		register NODE *r = p->in.right;
		if(r->in.op == ICON && r->tn.name[0] == '\0') {
			putstr("movl\t");
			printf(ACONFMT, r->tn.lval & ((1<<fldsz)-1));
			}
		else {
			putstr("andl3\t");
			printf(ACONFMT, (1 << fldsz) - 1);
			putchar(',');
			adrput(r);
			}
		putchar(',');
		adrput(resc);
		break;
		}

	case 'H':	/* opcode for shift */
		if(p->in.op == LS || p->in.op == ASG LS)
			putstr("shll");
		else if(ISUNSIGNED(p->in.left->in.type))
			putstr("shrl");
		else
			putstr("shar");
		return;

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

	case 'M': {  /* initiate ediv for mod and unsigned div */
		putstr("clrl\t");
		adrput(resc);
		putstr("\n\tmovl\t");
		adrput(p->in.left);
		putchar(',');
		upput(resc, SZLONG);
		printf("\n\tjgeq\tL%d\n\tmnegl\t$1,", m = getlab());
		adrput(resc);
		putchar('\n');
		deflab(m);
		return;
	}

	case 'T': {	/* rounded structure length for arguments */
		int size = p->stn.stsize;
		SETOFF( size, 4);
		printf("movab	-%d(sp),sp", size);
		return;
	}

	case 'S':  /* structure assignment */
		stasg(p);
		break;

#ifdef I_don_t_understand_this
	case 'X':	/* multiplication for short and char */
		if (ISUNSIGNED(p->in.left->in.type)) 
			printf("\tmovz");
		else
			printf("\tcvt");
		zzzcode(p, 'L');
		printf("l\t");
		adrput(p->in.left);
		printf(",");
		adrput(&resc[0]);
		printf("\n");
		if (ISUNSIGNED(p->in.right->in.type)) 
			printf("\tmovz");
		else
			printf("\tcvt");
		zzzcode(p, 'R');
		printf("l\t");
		adrput(p->in.right);
		printf(",");
		adrput(&resc[1]);
		printf("\n");
		return;
#endif

	case 'U':		/* SCONV */
	case 'V':		/* SCONV with FORCC */
		sconv(p, c == 'V');
		break;

	case 'W': {		/* SCONV or ASSIGN float/double => unsigned */
		NODE *src = p->in.op == SCONV ? p->in.left : p->in.right;

		putstr("ld");
		prtype(src);
		putchar('\t');
		adrput(src);
		putstr("\n\t");
		float_to_unsigned(p);
		break;
	}

	case 'Y':		/* SCONV or ASSIGN unsigned => float/double */
		unsigned_to_float(p);	/* stores into accumulator */
		putstr("\n\tst");
		prtype(p);
		putchar('\t');
		if (p->in.op == SCONV)
			adrput(resc);
		else
			adrput(p->in.left);
		rtyflg = 1;
		break;

#ifdef I_don_t_understand_this
	case 'Z':
		p = p->in.right;
		switch (p->in.type) {
		case SHORT: {
			short w = p->tn.lval;
			p->tn.lval = w;
			break;
		}
		case CHAR: {
			char c = p->tn.lval;
			p->tn.lval = c;
			break;
		}
		}
		printf("$%d", p->tn.lval);
		break;
#endif

	default:
		cerror( "illegal zzzcode" );
	}
}

#define	MOVB(dst, src, off) { \
	putstr("\tmovb\t"); upput(src, off); putchar(','); \
	upput(dst, off); putchar('\n'); \
}
#define	MOVW(dst, src, off) { \
	putstr("\tmovw\t"); upput(src, off); putchar(','); \
	upput(dst, off); putchar('\n'); \
}
#define	MOVL(dst, src, off) { \
	putstr("\tmovl\t"); upput(src, off); putchar(','); \
	upput(dst, off); putchar('\n'); \
}
/*
 * Generate code for a structure assignment.
 */
stasg(p)
	register NODE *p;
{
	register NODE *l, *r;
	register int size;

	switch (p->in.op) {
	case STASG:			/* regular assignment */
		l = p->in.left;
		r = p->in.right;
		break;
	case STARG:			/* place arg on the stack */
		l = getlr(p, '3');
		r = p->in.left;
		break;
	default:
		cerror("STASG bad");
		/*NOTREACHED*/
	}
	/*
	 * Pun source for use in code generation.
	 */
	switch (r->in.op) {
	case ICON:
		r->in.op = NAME;
		break;
	case REG:
		r->in.op = OREG;
		break;
	default:
		cerror( "STASG-r" );
		/*NOTREACHED*/
	}
	size = p->stn.stsize;
	if (size <= 0 || size > 65535)
		cerror("structure size out of range");
	/*
	 * Generate optimized code based on structure size
	 * and alignment properties....
	 */
	switch (size) {

	case 1:
		putstr("\tmovb\t");
	optimized:
		adrput(r);
		putchar(',');
		adrput(l);
		putchar('\n');
		break;

	case 2:
		if (p->stn.stalign != 2) {
			MOVB(l, r, SZCHAR);
			putstr("\tmovb\t");
		} else
			putstr("\tmovw\t");
		goto optimized;

	case 4:
		if (p->stn.stalign != 4) {
			if (p->stn.stalign != 2) {
				MOVB(l, r, 3*SZCHAR);
				MOVB(l, r, 2*SZCHAR);
				MOVB(l, r, 1*SZCHAR);
				putstr("\tmovb\t");
			} else {
				MOVW(l, r, SZSHORT);
				putstr("\tmovw\t");
			}
		} else
			putstr("\tmovl\t");
		goto optimized;

	case 6:
		if (p->stn.stalign != 2)
			goto movblk;
		MOVW(l, r, 2*SZSHORT);
		MOVW(l, r, 1*SZSHORT);
		putstr("\tmovw\t");
		goto optimized;

	case 8:
		if (p->stn.stalign == 4) {
			MOVL(l, r, SZLONG);
			putstr("\tmovl\t");
			goto optimized;
		}
		/* fall thru...*/

	default:
	movblk:
		/*
		 * Can we ever get a register conflict with R1 here?
		 */
		putstr("\tmovab\t");
		adrput(l);
		putstr(",r1\n\tmovab\t");
		adrput(r);
		printf(",r0\n\tmovl\t$%d,r2\n\tmovblk\n", size);
		rname(R2);
		break;
	}
	/*
	 * Reverse above pun for reclaim.
	 */
	if (r->in.op == NAME)
		r->in.op = ICON;
	else if (r->in.op == OREG)
		r->in.op = REG;
}

/*
 * Convert a float or double in the accumulator into an unsigned int.
 * Unlike the vax, the tahoe stores 0 into the destination
 *	on a conversion of > 2 ** 31, so we compensate.
 */
float_to_unsigned(p)
	NODE *p;
{
	register NODE *l = p->in.left;
	int label1 = getlab();
	int label2 = getlab();
	int label3 = getlab();
	NODE *src, *dst;

	if (p->in.op == SCONV) {
		src = p->in.left;
		dst = resc;
	} else {
		src = p->in.right;
		dst = p->in.left;
	}

	printf(".data\n\t.align\t2\nL%d:\n\t.long\t0x50000000", label1);
	if (src->in.type == DOUBLE)
		putstr(", 0x00000000 # .double");
	else
		putstr(" # .float");
	putstr(" 2147483648\n\t.text\n\tcmp");
	prtype(src);
	printf("\tL%d\n\tjlss\tL%d\n\tsub", label1, label2);
	prtype(src);
	printf("\tL%d\n\tcv", label1);
	prtype(src);
	putstr("l\t");
	adrput(dst);
	putstr("\n\taddl2\t$-2147483648,");
	adrput(dst);
	printf("\n\tjbr\tL%d\nL%d:\n\tcv", label3, label2);
	prtype(src);
	putstr("l\t");
	adrput(dst);
	printf("\nL%d:", label3);
}

/*
 * Convert an unsigned int into a float or double, leaving the result
 *	in the accumulator.
 */
unsigned_to_float(p)
	register NODE *p;
{
	int label1 = getlab();
	int label2 = getlab();
	NODE *src, *dst;

	if (p->in.op == SCONV) {
		src = p->in.left;
		dst = resc;
	} else {
		src = p->in.right;
		dst = p->in.left;
	}

	printf(".data\n\t.align\t2\nL%d:\n\t.long\t0x50800000", label2);
	if (p->in.type == DOUBLE)
		putstr(", 0x00000000 # .double");
	else
		putstr(" # .float");
	putstr(" 4294967296\n\t.text\n\tmovl\t");
	adrput(src);
	putchar(',');
	adrput(dst);
	putstr("\n\tcvl");
	prtype(p);
	putchar('\t');
	adrput(dst);
	printf("\n\tjgeq\tL%d\n\tadd", label1);
	prtype(p);
	printf("\tL%d\nL%d:", label2, label1);
}

/*
 * Prlen() is a cheap prtype()...
 */
static char convtab[SZINT/SZCHAR + 1] = {
	'?', 'b', 'w', '?', 'l'
};
#define	prlen(len)	putchar(convtab[len])


/*
 * Generate code for integral scalar conversions.
 * Some of this code is designed to work around a tahoe misfeature
 *	that causes sign- and zero- extension to be defeated in
 *	certain circumstances.
 * Basically if the source operand of a CVT or MOVZ instruction is
 *	shorter than the destination, and the source is a register
 *	or an immediate constant, sign- and zero- extension are
 *	ignored and the high bits of the source are copied.  (Note
 *	that zero-extension is not a problem for immediate
 *	constants.)
 */
sconv(p, forcc)
	NODE *p;
	int forcc;
{
	register NODE *src, *dst;
	register NODE *tmp;
	register int srclen, dstlen;
	int srctype, dsttype;
	int val;
	int neg = 0;

	if (p->in.op == ASSIGN) {
		src = p->in.right;
		dst = p->in.left;
		dstlen = tlen(dst);
		dsttype = dst->in.type;
	} else if (p->in.op == SCONV) {
		src = p->in.left;
		dst = resc;
		dstlen = tlen(p);
		dsttype = p->in.type;
	} else /* if (p->in.op == OPLEAF) */ {
		src = p;
		dst = resc;
		dstlen = SZINT/SZCHAR;
		dsttype = ISUNSIGNED(src->in.type) ? UNSIGNED : INT;
	}

	if (src->in.op == REG) {
		srclen = SZINT/SZCHAR;
		srctype = ISUNSIGNED(src->in.type) ? UNSIGNED : INT;
	} else {
		srclen = tlen(src);
		srctype = src->in.type;
	}

	if (src->in.op == ICON && src->tn.name[0] == '\0') {
		if (src->tn.lval == 0) {
			putstr("clr");
			prtype(dst);
			putchar('\t');
			adrput(dst);
			return;
		}
		if (dstlen < srclen) {
			switch (dsttype) {
			case CHAR:
				src->tn.lval = (char) src->tn.lval;
				break;
			case UCHAR:
				src->tn.lval = (unsigned char) src->tn.lval;
				break;
			case SHORT:
				src->tn.lval = (short) src->tn.lval;
				break;
			case USHORT:
				src->tn.lval = (unsigned short) src->tn.lval;
				break;
			}
		}
		if (dst->in.op == REG) {
			dsttype = INT;
			dstlen = SZINT/SZCHAR;
		}
		srctype = dsttype;
		srclen = dstlen;
		val = -src->tn.lval & ((1 << dstlen * SZCHAR) - 1);
		if ((unsigned) val < 64) {
			src->tn.lval = val;
			++neg;		/* MNEGx may be shorter */
		}
	}

	if (srclen < dstlen) {
		if (srctype == CHAR && dsttype == USHORT && dst->in.op == REG) {
			/* (unsigned short) c; => sign extend to 16 bits */
			putstr("cvtbl\t");
			adrput(src);
			putstr(",-(sp)\n\tmovzwl\t2(sp),");
			adrput(dst);
			putstr("\n\tmovab\t4(sp),sp");
			if (forcc) {
				/* inverted test */
				putstr("\n\tcmpl\t$0,");
				adrput(dst);
			}
			return;
		}
		genconv(ISUNSIGNED(srctype),
			srclen, dst->in.op == REG ? SZINT/SZCHAR : dstlen,
			src, dst);
		return;
	}

	if (srclen > dstlen && dst->in.op == REG) {
		/* if dst is a register, the result must look like an int */
		if (src->in.op == REG) {
			if (ISUNSIGNED(dsttype)) {
				val = (1 << dstlen * SZCHAR) - 1;
				if (src->tn.rval == dst->tn.rval)
					/* conversion in place */
					printf("andl2\t$%ld,", val);
				else {
					printf("andl3\t$%ld,", val);
					adrput(src);
					putchar(',');
				}
				adrput(dst);
				return;
			}
			/*
			 * Sign extension in register can also be
			 * accomplished by shifts, but unfortunately
			 * shifts are extremely slow, due to the lack
			 * of a barrel shifter.
			 */
			putstr("pushl\t");
			adrput(src);
			putstr("\n\tcvt");
			prlen(dstlen);
			printf("l\t%d(sp),", SZINT/SZCHAR - dstlen);
			adrput(dst);
			putstr("\n\tmovab\t4(sp),sp");
			if (forcc) {
				/* inverted test */
				putstr("\n\tcmpl\t$0,");
				adrput(dst);
			}
			return;
		}
		tmp = talloc();
		if ((src->in.op == NAME) ||
		    (src->in.op == UNARY MUL && src->in.left->in.op == ICON) ||
		    (src->in.op == OREG && !R2TEST(src->tn.rval))) {
			/* we can increment src's address & pun it */
			*tmp = *src;
			tmp->tn.lval += srclen - dstlen;
		} else {
			/* we must store src's address */
			*tmp = *dst;
			putstr("mova");
			prlen(srclen);
			putchar('\t');
			adrput(src);
			putchar(',');
			adrput(tmp);
			putstr("\n\t");
			tmp->tn.op = OREG;
			tmp->tn.lval = srclen - dstlen;
		}
		genconv(ISUNSIGNED(dsttype), dstlen, SZINT/SZCHAR, tmp, dst);
		tmp->in.op = FREE;
		return;
	}

	genconv(neg ? -1 : ISUNSIGNED(dsttype),
		srclen, dst->in.op == REG ? SZINT/SZCHAR : dstlen,
		src, dst);
}

genconv(srcflag, srclen, dstlen, src, dst)
	int srcflag;
	register int srclen, dstlen;
	NODE *src, *dst;
{
	if (srclen != dstlen) {
		if (srcflag > 0 && srclen < dstlen)
			putstr("movz");
		else
			putstr("cvt");
		prlen(srclen);
	} else if (srcflag < 0)
		putstr("mneg");
	else
		putstr("mov");
	prlen(dstlen);
	putchar('\t');
	adrput(src);
	putchar(',');
	adrput(dst);
}

rmove( rt, rs, t ) TWORD t; {
	printf( "	movl	%s,%s\n", rname(rs), rname(rt) );
	if(t==DOUBLE)
		printf( "	movl	%s,%s\n", rname(rs+1), rname(rt+1) );
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
	fregs = 6;	/* tbl- 6 free regs on Tahoe (0-5) */
	}

#ifndef szty
szty(t) TWORD t;{ /* size, in registers, needed to hold thing of type t */
	return(t==DOUBLE ? 2 : 1 );
	}
#endif

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

	if( o==ICON && p->in.name[0] != '\0' ) return( 100 ); /* ie no base reg */
	if( o==REG ) return( p->tn.rval );
    if( (o==PLUS || o==MINUS) && p->in.left->in.op == REG && p->in.right->in.op==ICON)
		return( p->in.left->tn.rval );
    if( o==OREG && !R2TEST(p->tn.rval) && (p->in.type==INT || p->in.type==UNSIGNED || ISPTR(p->in.type)) )
		return( p->tn.rval + 0200*1 );
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

#ifndef shltype
shltype( o, p ) register NODE *p; {
	return( o== REG || o == NAME || o == ICON || o == OREG || ( o==UNARY MUL && shumul(p->in.left)) );
	}
#endif

flshape( p ) NODE *p; {
	register int o = p->in.op;

	if( o==NAME || o==REG || o==ICON || o==OREG || (o==UNARY MUL && shumul(p->in.left)) ) return(1);
	return(0);
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
		 printf("\nshumul:op=%d,lop=%d,rop=%d", p->in.op, p->in.left->in.op, p->in.right->in.op);
		printf(" prname=%s,plty=%d, prlval=%D\n", p->in.right->in.name, p->in.left->in.type, p->in.right->tn.lval);
		}

	o = p->in.op;
	if(( o == NAME || (o == OREG && !R2TEST(p->tn.rval)) || o == ICON )
	 && p->in.type != PTR+DOUBLE)
		return( STARNM );

	return( 0 );
	}

special( p, shape ) register NODE *p; {
	if( shape==SIREG && p->in.op == OREG && R2TEST(p->tn.rval) ) return(1);
	else return(0);
}

adrcon( val ) CONSZ val; {
	printf(ACONFMT, val);
	}

conput( p ) register NODE *p; {
	switch( p->in.op ){

	case ICON:
		acon( p );
		return;

	case REG:
		putstr(rname(p->tn.rval));
		return;

	default:
		cerror( "illegal conput" );
		}
	}

/*ARGSUSED*/
insput( p ) NODE *p; {
	cerror( "insput" );
	}

/*
 * Output the address of the second item in the
 * pair pointed to by p.
 */
upput(p, size)
	register NODE *p;
{
	CONSZ save;

	if (p->in.op == FLD)
		p = p->in.left;
	switch (p->in.op) {

	case NAME:
	case OREG:
		save = p->tn.lval;
		p->tn.lval += size/SZCHAR;
		adrput(p);
		p->tn.lval = save;
		break;

	case REG:
		if (size == SZLONG) {
			putstr(rname(p->tn.rval+1));
			break;
		}
		/* fall thru... */

	default:
		cerror("illegal upper address op %s size %d",
		    opst[p->tn.op], size);
		/*NOTREACHED*/
	}
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
		putchar('$');
		acon( p );
		return;

	case REG:
		putstr(rname(p->tn.rval));
		if(p->in.type == DOUBLE)	/* for entry mask */
			(void) rname(p->tn.rval+1);
		return;

	case OREG:
		r = p->tn.rval;
		if( R2TEST(r) ){ /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if( flags & 1 ) putchar('*');
			if( p->tn.lval != 0 || p->in.name[0] != '\0' ) acon(p);
			if( R2UPK1(r) != 100) printf( "(%s)", rname(R2UPK1(r)) );
			printf( "[%s]", rname(R2UPK2(r)) );
			return;
			}
		if( r == FP && p->tn.lval > 0 ){  /* in the argument region */
			if( p->in.name[0] != '\0' ) werror( "bad arg temp" );
			printf( CONFMT, p->tn.lval );
			putstr( "(fp)" );
			return;
			}
		if( p->tn.lval != 0 || p->in.name[0] != '\0') acon( p );
		printf( "(%s)", rname(p->tn.rval) );
		return;

	case UNARY MUL:
		/* STARNM or STARREG found */
		if( tshape(p, STARNM) ) {
			putchar( '*' );
			adrput( p->in.left);
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

genfcall( p, cookie ) register NODE *p; {
	register NODE *p1;
	register int m;
	static char *funcops[6] = {
		"sin", "cos", "sqrt", "exp", "log", "atan"
	};

	/* generate function opcodes */
	if(p->in.op==UNARY FORTCALL && p->in.type==FLOAT &&
	 (p1 = p->in.left)->in.op==ICON &&
	 p1->tn.lval==0 && p1->in.type==INCREF(FTN|FLOAT)) {
#ifdef FLEXNAMES
		p1->in.name++;
#else
		strcpy(p1->in.name, p1->in.name[1]);
#endif
		for(m=0; m<6; m++)
			if(!strcmp(p1->in.name, funcops[m]))
				break;
		if(m >= 6)
			uerror("no opcode for fortarn function %s", p1->in.name);
	} else
		uerror("illegal type of fortarn function");
	p1 = p->in.right;
	p->in.op = FORTCALL;
	if(!canaddr(p1))
		order( p1, INAREG|INBREG|SOREG|STARREG|STARNM );
	m = match( p, INTAREG|INTBREG );
	return(m != MDONE);
}

/* tbl */
int gc_numbytes;
/* tbl */

/*ARGSUSED*/
gencall( p, cookie ) register NODE *p; {
	/* generate the call given by p */
	register NODE *p1, *ptemp;
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

/* tbl
	setup gc_numbytes so reference to ZC works */

	gc_numbytes = temp&(0x3ff);

	p->in.op = UNARY CALL;
	m = match( p, INTAREG|INTBREG );

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

	case ASG PLUS:
	case ASG MINUS:
	case ASG MUL:
	case ASG OR:
		/* simple ASG OPSIMP -- reduce range of constant rhs */
		l = p->in.left;
		r = p->in.right;
		if( tlen(l) < SZINT/SZCHAR &&
		    r->in.op==ICON && r->in.name[0]==0 ){
			mask = (1 << tlen(l) * SZCHAR) - 1;
			if( r->tn.lval & (mask & ~(mask >> 1)) )
				r->tn.lval |= ~mask;
			else
				r->tn.lval &= mask;
			}
		break;

	case AND:
	case ASG AND:
		r = p->in.right;
		if( r->in.op==ICON && r->in.name[0]==0 ) {
			/* check for degenerate operations */
			l = p->in.left;
			mask = (1 << tlen(l) * SZCHAR) - 1;
			if( o == ASG AND || ISUNSIGNED(r->in.type) ) {
				i = r->tn.lval & mask;
				if( i == mask ) {
					/* redundant mask */
					r->in.op = FREE;
					ncopy(p, l);
					l->in.op = FREE;
					break;
					}
				else if( i == 0 )
					/* all bits masked off */
					goto zero;
				r->tn.lval = i;
				if( tlen(l) < SZINT/SZCHAR ){
					/* sign extend */
					if( r->tn.lval & (mask & ~(mask >> 1)) )
						r->tn.lval |= ~mask;
					else
						r->tn.lval &= mask;
					}
				}
			else if( r->tn.lval == mask &&
				 tlen(l) < SZINT/SZCHAR ) {
				/* use movz instead of and */
				r->in.op = SCONV;
				r->in.left = l;
				r->in.right = 0;
				r->in.type = ENUNSIGN(l->in.type);
				r->in.su = l->in.su > 1 ? l->in.su : 1;
				ncopy(p, r);
				p->in.left = r;
				p->in.type = INT;
				}
			}
		break;

	case SCONV:
		l = p->in.left;
		if( p->in.type == FLOAT || p->in.type == DOUBLE ||
		    l->in.type == FLOAT || l->in.type == DOUBLE )
			return;
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
		 * as signed ints...  this may need tuning for the tahoe.
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
		/* 'J' in zzzcode() -- assumes DIV or MOD operations */
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
