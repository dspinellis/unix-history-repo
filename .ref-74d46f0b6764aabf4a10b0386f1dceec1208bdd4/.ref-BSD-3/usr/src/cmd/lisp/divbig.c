#include "global.h"

#define b 0x40000000
#define toint(p) ((int) (p))

divbig(dividend, divisor, quotient, remainder)
lispval dividend, divisor, *quotient, *remainder;
{
	register *ujp, *vip;
	int *sp(), *alloca(), d, negflag = 0, m, n, carry, rem, qhat, j;
	int borrow, negrem = 0;
	int *utop = sp(), *ubot, *vbot, *qbot;
	register lispval work; lispval export();

	/* copy dividend */
	for(work = dividend; work; work = work ->CDR)
		stack(work->I);
	ubot = sp();
	if(*ubot < 0) {		/* knuth's division alg works only for pos
					bignums				*/
		negflag ^= 1;
		negrem = 1;
		dsmult(utop-1,ubot,-1);
	}
	stack(0);
	ubot = sp();

	
	/*copy divisor */
	for(work = divisor; work; work = work->CDR)
		stack(work->I);

	vbot = sp();
	stack(0);
	if(*vbot < 0) {
		negflag ^= 1;
		dsmult(ubot-1,vbot,-1);
	}

	/* check validity of data */
	n = ubot - vbot;
	m = utop - ubot - n - 1;
	if (n == 1) {
		/* do destructive division by  a single. */
		rem = dsdiv(utop-1,ubot,*vbot);
		if(negrem)
			rem = -rem;
		if(negflag)
			dsmult(utop-1,ubot,-1);
		if(remainder)
			*remainder = inewint(rem);
		if(quotient)
			*quotient = export(utop,ubot);
		return;
	}
	if (m < 0) {
		if (remainder)
			*remainder = dividend;
		if(quotient)
			*quotient = inewint(0);
		return;
	}
	qbot = alloca(toint(utop) + toint(vbot) - 2 * toint(ubot));
d1:
	d = b /(*vbot +1);
	dsmult(utop-1,ubot,d);
	dsmult(ubot-1,vbot,d);

d2:	for(j=0,ujp=ubot; j <= m; j++,ujp++) {

	d3:	
		qhat = calqhat(ujp,vbot);
	d4:
		if((borrow = mlsb(ujp + n, ujp, ubot, -qhat)) < 0) {
			adback(ujp + n, ujp, ubot);
			qhat--;
		}
		qbot[j] = qhat;
	}
d8:	if(remainder) {
		dsdiv(utop, utop - n, d);
		if(negrem) dsmult(utop-1,utop-n,-1);
		*remainder = export(utop,utop-n);
	}
	if(quotient) {
		if(negflag)
			dsmult(qbot+m,qbot,-1);
		*quotient = export(qbot + m + 1, qbot);
	}
}
/*static*/ calqhat(ujp,v1p)
register int *ujp, *v1p;
{
asm("	movl	$0x3fffffff,r0");
asm("	cmpl	(r10),(r11)");
asm("	beql	on1");
asm("	emul	(r11),$0x40000000,4(r11),r1");
asm("	ediv	(r10),r1,r0,r5");
asm("on1:");
asm("	emul	r0,4(r10),$0,r1");
asm("	emul	r5,$0x40000000,8(r11),r3");
asm("	subl2	r3,r1");
asm("	sbwc	r4,r2");
asm("	bleq	out1");
asm("	decl	r0");
asm("out1:");
}
/*static*/ mlsb(utop,ubot,vtop,nqhat)
register int *utop, *ubot, *vtop;
register int nqhat;
{
asm("	clrl	r0");
asm("loop2:	addl2	(r11),r0");
asm("	emul	r8,-(r9),r0,r2");
asm("	extzv	$0,$30,r2,(r11)");
asm("	extv	$30,$32,r2,r0");
asm("	acbl	r10,$-4,r11,loop2");
}
/*static*/ adback(utop,ubot,vtop)
register int *utop, *ubot, *vtop;
{
asm("	clrl	r0");
asm("loop3:	addl2	-(r9),r0");
asm("	addl2	(r11),r0");
asm("	extzv	$0,$30,r0,(r11)");
asm("	extv	$30,$2,r0,r0");
asm("	acbl	r10,$-4,r11,loop3");
}
/*static*/ dsdiv(top,bot,div)
register int* bot;
{
asm("	clrl	r0");
asm("loop4:	emul	r0,$0x40000000,(r11),r1");
asm("	ediv	12(ap),r1,(r11),r0");
asm("	acbl	4(ap),$4,r11,loop4");
}
/*static*/ dsmult(top,bot,mult)
register int* top;
{
asm("	clrl	r0");
asm("loop5:	emul	12(ap),(r11),r0,r1");
asm("	extzv	$0,$30,r1,(r11)");
asm("	extv	$30,$32,r1,r0");
asm("	acbl	8(ap),$-4,r11,loop5");
asm("	movl	r1,4(r11)");
}
/*static*/ lispval export(top,bot)
register lispval bot;
{
	register r10, r9, r8, r7, r6;
asm("	movl	4(ap),r10");
asm("	movl	$0xC0000000,r4");
asm("	jmp	Bexport");
}

#define MAXINT 0x8000000L

Ihau(fix)
register int fix;
{
	register count;
	if(fix==MAXINT)
		return(32);
	if(fix < 0)
		fix = -fix;
	for(count = 0; fix; count++)
		fix /= 2;
	return(count);
}
lispval
Lhau()
{
	register count;
	register lispval handy;
	register dum1,dum2;
	register struct argent *lbot, *np;
	lispval Labsval();

	handy = lbot->val;
top:
	switch(TYPE(handy)) {
	case INT:
		count = Ihau(handy->i);
		break;
	case SDOT:
		lbot->val = Labsval();
		for(count = 0; handy->CDR!=((lispval) 0); handy = handy->CDR)
			count += 30;
		count += Ihau(handy->I);
		break;
	default:
		handy = errorh(Vermisc,"Haulong: bad argument",nil,
			       TRUE,997,handy);
		goto top;
	}
	return(inewint(count));
}
lispval
Lhaipar()
{
	int *sp();
	register lispval work;
	register n;
	register int *top = sp() - 1;
	register int *bot;
	register struct argent *lbot, *np;
	int mylen;

	/*chkarg(2);*/
	work = lbot->val;
					/* copy data onto stack */
on1:
	switch(TYPE(work)) {
	case INT:
		stack(work->i);
		break;
	case SDOT:
		for(; work!=((lispval) 0); work = work->CDR)
			stack(work->I);
		break;
	default:
		work = errorh(Vermisc,"Haipart: bad first argument",nil,
				TRUE,996,work);
		goto on1;
	}
	bot = sp();
	if(*bot < 0) {
		stack(0);
		dsmult(top,bot,-1);
		bot--;
	}
	for(; *bot==0 && bot < top; bot++);
				/* recalculate haulong internally */
	mylen = (top - bot) * 30 + Ihau(*bot);
				/* get second argument		  */
	work = lbot[1].val;
	while(TYPE(work)!=INT)
		work = errorh(Vermisc,"Haipart: 2nd arg not int",nil,
				TRUE,995,work);
	n = work->i;
	if(n >= mylen || -n >= mylen)
		goto done;
	if(n >= 0) {
				/* Here we want n most significant bits
				   so chop off mylen - n bits */
		stack(0);
		n = mylen - n;
		for(n; n >= 30; n -= 30)
			top--;
		if(top < bot)
			error("Internal error in haipart #1",FALSE);
		dsdiv(top,bot,1<<n);

	} else {
				/* here we want abs(n) low order bits */
		stack(0);
		bot = top + 1;
		for(; n <= 0; n += 30)
			bot--;
		n = 30 - n;
		*bot &= ~ (-1<<n);
	}
done:
	return(export(top + 1,bot));
}
