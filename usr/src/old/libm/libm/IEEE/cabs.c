/* 
 * Copyright (c) 1985 Regents of the University of California.
 * 
 * Use and reproduction of this software are granted  in  accordance  with
 * the terms and conditions specified in  the  Berkeley  Software  License
 * Agreement (in particular, this entails acknowledgement of the programs'
 * source, and inclusion of this notice) with the additional understanding
 * that  all  recipients  should regard themselves as participants  in  an
 * ongoing  research  project and hence should  feel  obligated  to report
 * their  experiences (good or bad) with these elementary function  codes,
 * using "sendbug 4bsd-bugs@BERKELEY", to the authors.
 */

#ifndef lint
static char sccsid[] = "@(#)cabs.c	1.3 (Berkeley) 6/29/87";
#endif not lint

/* CABS(Z)
 * RETURN THE ABSOLUTE VALUE OF THE COMPLEX NUMBER  Z = X + iY
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 11/28/84.
 * REVISED BY K.C. NG, 7/12/85.
 *
 * Required kernel function :
 *	hypot(x,y)
 *
 * Method :
 *	cabs(z) = hypot(x,y) .
 */

double cabs(z)
struct { double x, y;} z;
{
	double hypot();
	return(hypot(z.x,z.y));
}


/* HYPOT(X,Y)
 * RETURN THE SQUARE ROOT OF X^2 + Y^2  WHERE Z=X+iY
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 11/28/84; 
 * REVISED BY K.C. NG, 7/12/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	finite(x)
 *	scalb(x,N)
 *	sqrt(x)
 *
 * Method :
 *	1. replace x by |x| and y by |y|, and swap x and
 *	   y if y > x (hence x is never smaller than y).
 *	2. Hypot(x,y) is computed by:
 *	   Case I, x/y > 2
 *		
 *				       y
 *		hypot = x + -----------------------------
 *			 		    2
 *			    sqrt ( 1 + [x/y]  )  +  x/y
 *
 *	   Case II, x/y <= 2 
 *				                   y
 *		hypot = x + --------------------------------------------------
 *				          		     2 
 *				     			[x/y]   -  2
 *			   (sqrt(2)+1) + (x-y)/y + -----------------------------
 *			 		    			  2
 *			    			  sqrt ( 1 + [x/y]  )  + sqrt(2)
 *
 *
 *
 * Special cases:
 *	hypot(x,y) is INF if x or y is +INF or -INF; else
 *	hypot(x,y) is NAN if x or y is NAN.
 *
 * Accuracy:
 * 	hypot(x,y) returns the sqrt(x^2+y^2) with error less than 1 ulps (units
 *	in the last place). See Kahan's "Interval Arithmetic Options in the
 *	Proposed IEEE Floating Point Arithmetic Standard", Interval Mathematics
 *      1980, Edited by Karl L.E. Nickel, pp 99-128. (A faster but less accurate
 *	code follows in	comments.) In a test run with 500,000 random arguments
 *	on a VAX, the maximum observed error was .959 ulps.
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(VAX) || defined(TAHOE)	/* VAX D format */
/* static double */
/* r2p1hi =  2.4142135623730950345E0     , Hex  2^  2   *  .9A827999FCEF32 */
/* r2p1lo =  1.4349369327986523769E-17   , Hex  2^-55   *  .84597D89B3754B */
/* sqrt2  =  1.4142135623730950622E0     ; Hex  2^  1   *  .B504F333F9DE65 */
static long    r2p1hix[] = { 0x8279411a, 0xef3299fc};
static long    r2p1lox[] = { 0x597d2484, 0x754b89b3};
static long     sqrt2x[] = { 0x04f340b5, 0xde6533f9};
#define   r2p1hi    (*(double*)r2p1hix)
#define   r2p1lo    (*(double*)r2p1lox)
#define    sqrt2    (*(double*)sqrt2x)
#else		/* IEEE double format */
static double
r2p1hi =  2.4142135623730949234E0     , /*Hex  2^1     *  1.3504F333F9DE6 */
r2p1lo =  1.2537167179050217666E-16   , /*Hex  2^-53   *  1.21165F626CDD5 */
sqrt2  =  1.4142135623730951455E0     ; /*Hex  2^  0   *  1.6A09E667F3BCD */
#endif

double hypot(x,y)
double x, y;
{
	static double zero=0, one=1, 
		      small=1.0E-18;	/* fl(1+small)==1 */
	static ibig=30;	/* fl(1+2**(2*ibig))==1 */
	double copysign(),scalb(),logb(),sqrt(),t,r;
	int finite(), exp;

	if(finite(x))
	    if(finite(y))
	    {	
		x=copysign(x,one);
		y=copysign(y,one);
		if(y > x) 
		    { t=x; x=y; y=t; }
		if(x == zero) return(zero);
		if(y == zero) return(x);
		exp= logb(x);
		if(exp-(int)logb(y) > ibig ) 	
			/* raise inexact flag and return |x| */
		   { one+small; return(x); }

	    /* start computing sqrt(x^2 + y^2) */
		r=x-y;
		if(r>y) { 	/* x/y > 2 */
		    r=x/y;
		    r=r+sqrt(one+r*r); }
		else {		/* 1 <= x/y <= 2 */
		    r/=y; t=r*(r+2.0);
		    r+=t/(sqrt2+sqrt(2.0+t));
		    r+=r2p1lo; r+=r2p1hi; }

		r=y/r;
		return(x+r);

	    }

	    else if(y==y)   	   /* y is +-INF */
		     return(copysign(y,one));
	    else 
		     return(y);	   /* y is NaN and x is finite */

	else if(x==x) 		   /* x is +-INF */
	         return (copysign(x,one));
	else if(finite(y))
	         return(x);		   /* x is NaN, y is finite */
	else if(y!=y) return(y);  /* x and y is NaN */
	else return(copysign(y,one));   /* y is INF */
}

/* A faster but less accurate version of cabs(x,y) */
#if 0
double hypot(x,y)
double x, y;
{
	static double zero=0, one=1;
		      small=1.0E-18;	/* fl(1+small)==1 */
	static ibig=30;	/* fl(1+2**(2*ibig))==1 */
	double copysign(),scalb(),logb(),sqrt(),temp;
	int finite(), exp;

	if(finite(x))
	    if(finite(y))
	    {	
		x=copysign(x,one);
		y=copysign(y,one);
		if(y > x) 
		    { temp=x; x=y; y=temp; }
		if(x == zero) return(zero);
		if(y == zero) return(x);
		exp= logb(x);
		x=scalb(x,-exp);
		if(exp-(int)logb(y) > ibig ) 
			/* raise inexact flag and return |x| */
		   { one+small; return(scalb(x,exp)); }
		else y=scalb(y,-exp);
		return(scalb(sqrt(x*x+y*y),exp));
	    }

	    else if(y==y)   	   /* y is +-INF */
		     return(copysign(y,one));
	    else 
		     return(y);	   /* y is NaN and x is finite */

	else if(x==x) 		   /* x is +-INF */
	         return (copysign(x,one));
	else if(finite(y))
	         return(x);		   /* x is NaN, y is finite */
	else if(y!=y) return(y);  	/* x and y is NaN */
	else return(copysign(y,one));   /* y is INF */
}
#endif
