/* Copyright (c) 1982, Regents, University of California */
struct vl	{ long high; long low; };

/*
 * $Header $
 * exarith(mul1,mul2,add,hi,lo)
 *
 * (hi,lo) gets 64 bit product + sum of mul1*mul2+add;
 * routine returns non-zero if product is bigger than 30 bits
 */

long exarith(mul1,mul2,add,hi,lo)
long *hi, *lo;
register long add;
{
	struct vl work;
	register long rlo;

	    emul(mul1,mul2,add,&work);
	    add = work.high;
	    add <<= 2;
	    if((rlo = work.low) < 0) add += 2;
	    if(rlo & 0x40000000) add += 1;
	    *lo = rlo &0x3fffffff;
	    (*hi = add);
	    if((add==0) || (add!=-1)) return(add);
	    *lo = rlo;
	    return(0);
}
