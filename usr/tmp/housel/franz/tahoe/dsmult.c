/* Copyright (c) 1982, Regents, University of California */
/*
 * dsmult(top,bot,mul) --
 * multiply an array of longs on the stack, by mul.
 * the element top through bot (inclusive) get changed.
 * if you expect a carry out of the most significant,
 * it is up to you to provide a space for it to overflow.
 */

struct vl
       {long high;
	long low;
       };

dsmult(top,bot,mul)
long *top, *bot, mul;
{
	register long *p;
	struct vl work;
	long add = 0;

	for(p = top; p >= bot; p--) {
		asm("emul	(r12),12(fp),-64(fp),-60(fp)");
/* *p has 30 bits of info, mul has 32 yielding a 62 bit product. */
		*p = work.low & 0x3fffffff; /* the stack gets the low 30 bits */
		add = work.high;        /* we want add to get the next 32 bits.
					   on a 68k you might better be able to
					   do this by shifts and tests on the
					   carry but I don't know how to do this
					   from C, and the code generated here
					   will not be much worse.  Far less
					   bad than shifting work.low to the
					   right 30 bits just to get the top 2.
					   */
		add <<= 2;
		if(work.low < 0) add += 2;
		if(work.low & 0x40000000) add += 1;
	}
	p[1] = work.low;  /* on the final store want all 32 bits. */
}
