/* Copyright (c) 1982, Regents, University of California */

struct sdot	{ long I; struct sdot *CDR; };
struct vl	{ long high; long low; };

long dodiv(top,bottom)
long *top, *bottom; /* top least significant; bottom most */
{
	struct vl work;
	char error;
	long rem = 0, ediv();
	register long *p = bottom;

	for(;p <= top;p++)
	{
		emul(0x40000000,rem,*p,&work);
		*p = ediv(&work,1000000000,&error);
		rem = work.high;
	}
	return(rem);
}

long dsneg(top,bottom)
long *top, *bottom;
{
	register long *p = top;
	register carry = 0;
	register digit;

	while(p >= bottom)
	{
		digit = carry - *p;
		/* carry = digit >> 30; is slow on 68K */
		if(digit < 0) carry = -2;
		if(digit & 0x40000000) carry += 1;
		*p-- = digit & 0x3fffffff;
	}
}


