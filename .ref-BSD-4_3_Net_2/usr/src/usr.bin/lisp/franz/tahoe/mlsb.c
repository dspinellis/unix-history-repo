/* file: mlsb.c
** functions: mlsb(), adback(), dsdiv(), dsadd1(), dsrsh()
/* Copyright (c) 1982, Regents, University of California */

struct vl		/* very long? */
       {long high;
        long low;
       };

long mlsb(utop,ubot,vtop,nqhat)
long *utop; register long *ubot, *vtop;
register nqhat;
{
	register handy, carry;
	struct vl work;
	
	for(carry = 0; utop >= ubot; utop--) {

		--vtop;
		asm("addl3 r8,*4(fp),r0");
		asm("emul r10,(r11),r0,-60(fp)");

		carry = work.high;
		handy = work.low;
		*utop = handy & 0x3fffffff;
		carry <<= 2;
		if(handy & 0x80000000) carry += 2;
		if(handy & 0x40000000) carry += 1;
	}
	return(carry);
}

long adback(utop,ubot,vtop)
register long *utop, *ubot, *vtop;
{
	register handy, carry;
	carry = 0;
	for(; utop >= ubot; utop--) {
		carry += *--vtop;
		carry += *utop;
		*utop = carry & 0x3fffffff;
		/* next junk is faster version of  carry >>= 30; */
		handy = carry;
		carry = 0;
		if(handy & 0x80000000) carry -= 2;
		if(handy & 0x40000000) carry += 1;
	}
	return(carry);
}

long dsdiv(top,bot,div)
register long *top, *bot;
register long div;
{
	struct vl work; char err;
	register long handy, carry = 0;
	for(carry = 0;bot <= top; bot++) {
		handy = *bot;
		if(carry & 1) handy |= 0x40000000;
		if(carry & 2) handy |= 0x80000000;
		carry >>= 2;
		work.low = handy;
		work.high = carry;
	/*	*bot = ediv(&work,div,&err); */
	/*	carry = work.high;	     */
		asm("ediv r10,-60(fp),(r11),r8");
	}
	return(carry);
}

dsadd1(top,bot)
long *top, *bot;
{
	register long *p, work, carry = 0;

	/*
	 * this assumes canonical inputs
	 */
	for(p = top; p >= bot; p--) {
		work = *p + carry;
		*p = work & 0x3fffffff;
		carry = 0;
		if (work & 0x40000000) carry += 1;
		if (work & 0x80000000) carry -= 2;
	}
	p[1] = work;
}

long dsrsh(top,bot,ncnt,mask1)
long *top, *bot;
long ncnt, mask1;
{
	register long *p = bot;
	register r = -ncnt, l = 30+ncnt, carry = 0, work, save;
	long mask = -1 ^ mask1;

	while(p <= top) {
		save = work = *p; save &= mask; work >>= r;
		carry <<= l; work |= carry; *p++ = work;
		carry = save;
	}
	return(carry);
}
