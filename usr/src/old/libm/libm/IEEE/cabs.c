From Prof. Kahan at UC at Berkeley
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
static char sccsid[] = "@(#)cabs.c	1.1 (Berkeley) %G%";
#endif not lint

/* HYPOT(X,Y) 
 * RETURN THE SQUARE ROOT OF X^2 + Y^2
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 11/28/84.
 *
 * Required kernel function :
 *	cabs(x,y)
 *
 * Method :
 *	hypot(x,y) = cabs(x,y) .
 */

double hypot(x,y)
double x, y;
{
	double cabs();
	return(cabs(x,y));
}

/* CABS(REAL,IMAG)
 * RETURN THE ABSOLUTE VALUE OF THE COMPLEX NUMBER  REAL + i*IMAG
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 11/28/84; 
 * REVISED BY K.C. NG on 2/7/85, 2/22/85, 3/7/85, 3/30/85, 4/16/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	finite(x)
 *	scalb(x,N)
 *	sqrt(x)
 *
 * Method :
 *	1. replace real by |real| and imag by |imag|, and swap real and
 *	   imag if imag > real (hence real is never smaller than imag).
 *	2. Let X=real and Y=imag, cabs(X,Y) is computed by:
 *	   Case I, X/Y > 2
 *		
 *				       Y
 *		cabs = X + -----------------------------
 *			 		    2
 *			    sqrt ( 1 + [X/Y]  )  +  X/Y
 *
 *	   Case II, X/Y <= 2 
 *				                   Y
 *		cabs = X + --------------------------------------------------
 *				          		     2 
 *				     			[X/Y]   -  2
 *			   (sqrt(2)+1) + (X-Y)/Y + -----------------------------
 *			 		    			  2
 *			    			  sqrt ( 1 + [X/Y]  )  + sqrt(2)
 *
 *
 *
 * Special cases:
 *	cabs(x,y) is INF if x or y is +INF or -INF; else
 *	cabs(x,y) is NAN if x or y is NAN.
 *
 * Accuracy:
 * 	cabs(x,y) returns the sqrt(x^2+y^2) with error less than 1 ulps (units
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

#ifdef VAX	/* VAX D format */
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

double cabs(real,imag)
double real, imag;
{
	static double zero=0, one=1, 
		      small=1.0E-18;	/* fl(1+small)==1 */
	static ibig=30;	/* fl(1+2**(2*ibig))==1 */
	double copysign(),scalb(),logb(),sqrt(),t,r;
	int finite(), exp;

	if(finite(real))
	    if(finite(imag))
	    {	
		real=copysign(real,one);
		imag=copysign(imag,one);
		if(imag > real) 
		    { t=real; real=imag; imag=t; }
		if(real == zero) return(zero);
		if(imag == zero) return(real);
		exp= logb(real);
		if(exp-(int)logb(imag) > ibig ) 	
			/* raise inexact flag and return |real| */
		   { one+small; return(real); }

	    /* start computing sqrt(real^2 + imag^2) */
		r=real-imag;
		if(r>imag) { 	/* real/imag > 2 */
		    r=real/imag;
		    r=r+sqrt(one+r*r); }
		else {		/* 1 <= real/imag <= 2 */
		    r/=imag; t=r*(r+2.0);
		    r+=t/(sqrt2+sqrt(2.0+t));
		    r+=r2p1lo; r+=r2p1hi; }

		r=imag/r;
		return(real+r);

	    }

	    else if(imag==imag)   	   /* imag is +-INF */
		     return(copysign(imag,one));
	    else 
		     return(imag);	   /* imag is NaN and x is finite */

	else if(real==real) 		   /* real is +-INF */
	         return (copysign(real,one));
	else if(finite(imag))
	         return(real);		   /* real is NaN, imag is finite */
	else if(imag!=imag) return(imag);  /* real and imag is NaN */
	else return(copysign(imag,one));   /* imag is INF */
}

/* A faster but less accurate version of cabs(real,imag) */
#if 0
double cabs(real,imag)
double real, imag;
{
	static double zero=0, one=1;
		      small=1.0E-18;	/* fl(1+small)==1 */
	static ibig=30;	/* fl(1+2**(2*ibig))==1 */
	double copysign(),scalb(),logb(),sqrt(),temp;
	int finite(), exp;

	if(finite(real))
	    if(finite(imag))
	    {	
		real=copysign(real,one);
		imag=copysign(imag,one);
		if(imag > real) 
		    { temp=real; real=imag; imag=temp; }
		if(real == zero) return(zero);
		if(imag == zero) return(real);
		exp= logb(real);
		real=scalb(real,-exp);
		if(exp-(int)logb(imag) > ibig ) 
			/* raise inexact flag and return |real| */
		   { one+small; return(scalb(real,exp)); }
		else imag=scalb(imag,-exp);
		return(scalb(sqrt(real*real+imag*imag),exp));
	    }

	    else if(imag==imag)   	   /* imag is +-INF */
		     return(copysign(imag,one));
	    else 
		     return(imag);	   /* imag is NaN and x is finite */

	else if(real==real) 		   /* real is +-INF */
	         return (copysign(real,one));
	else if(finite(imag))
	         return(real);		   /* real is NaN, imag is finite */
	else if(imag!=imag) return(imag);  /* real and imag is NaN */
	else return(copysign(imag,one));   /* imag is INF */
}
#endif
