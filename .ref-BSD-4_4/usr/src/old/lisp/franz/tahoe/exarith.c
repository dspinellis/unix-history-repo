/*
 * exarith(mul1,mul2,add,hi,lo)
 *
 * (hi,lo) gets 64 bit product + sum of mul1*mul2+add;
 * routine returns non-zero if product is bigger than 30 bits
 *
 * Copyright (c) 1982, Regents, University of California
 *
 * stolen from 68k by P. S. Housel 04/29/86, with minor mods
 *	(replaced call to emul with asm("emul..."))
 */

struct vl
       {long high;
	long low;
       };

long exarith(mul1,mul2,add,hi,lo)
long *hi, *lo;
register long add;
{
	register long rlo;
	struct vl work;

            asm("	emul	4(fp),8(fp),r12,-60(fp)");

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
