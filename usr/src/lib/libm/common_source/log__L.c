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
static char sccsid[] =
"@(#)log__L.c	1.2 (Berkeley) 8/21/85; 1.3 (ucb.elefunt) %G%";
#endif not lint

/* log__L(Z)
 *		LOG(1+X) - 2S			       X
 * RETURN      ---------------  WHERE Z = S*S,  S = ------- , 0 <= Z <= .0294...
 *		      S				     2 + X
 *		     
 * DOUBLE PRECISION (VAX D FORMAT 56 bits or IEEE DOUBLE 53 BITS)
 * KERNEL FUNCTION FOR LOG; TO BE USED IN LOG1P, LOG, AND POW FUNCTIONS
 * CODED IN C BY K.C. NG, 1/19/85; 
 * REVISED BY K.C. Ng, 2/3/85, 4/16/85.
 *
 * Method :
 *	1. Polynomial approximation: let s = x/(2+x). 
 *	   Based on log(1+x) = log(1+s) - log(1-s)
 *		 = 2s + 2/3 s**3 + 2/5 s**5 + .....,
 *
 *	   (log(1+x) - 2s)/s is computed by
 *
 *	       z*(L1 + z*(L2 + z*(... (L7 + z*L8)...)))
 *
 *	   where z=s*s. (See the listing below for Lk's values.) The 
 *	   coefficients are obtained by a special Remez algorithm. 
 *
 * Accuracy:
 *	Assuming no rounding error, the maximum magnitude of the approximation 
 *	error (absolute) is 2**(-58.49) for IEEE double, and 2**(-63.63)
 *	for VAX D format.
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if (defined(VAX)||defined(TAHOE))	/* VAX D format (56 bits) */
/* static double */
/* L1     =  6.6666666666666703212E-1    , Hex  2^  0   *  .AAAAAAAAAAAAC5 */
/* L2     =  3.9999999999970461961E-1    , Hex  2^ -1   *  .CCCCCCCCCC2684 */
/* L3     =  2.8571428579395698188E-1    , Hex  2^ -1   *  .92492492F85782 */
/* L4     =  2.2222221233634724402E-1    , Hex  2^ -2   *  .E38E3839B7AF2C */
/* L5     =  1.8181879517064680057E-1    , Hex  2^ -2   *  .BA2EB4CC39655E */
/* L6     =  1.5382888777946145467E-1    , Hex  2^ -2   *  .9D8551E8C5781D */
/* L7     =  1.3338356561139403517E-1    , Hex  2^ -2   *  .8895B3907FCD92 */
/* L8     =  1.2500000000000000000E-1    , Hex  2^ -2   *  .80000000000000 */
static long        L1x[] = { 0xaaaa402a, 0xaac5aaaa};
static long        L2x[] = { 0xcccc3fcc, 0x2684cccc};
static long        L3x[] = { 0x49243f92, 0x578292f8};
static long        L4x[] = { 0x8e383f63, 0xaf2c39b7};
static long        L5x[] = { 0x2eb43f3a, 0x655ecc39};
static long        L6x[] = { 0x85513f1d, 0x781de8c5};
static long        L7x[] = { 0x95b33f08, 0xcd92907f};
static long        L8x[] = { 0x00003f00, 0x00000000};
#define       L1    (*(double*)L1x)
#define       L2    (*(double*)L2x)
#define       L3    (*(double*)L3x)
#define       L4    (*(double*)L4x)
#define       L5    (*(double*)L5x)
#define       L6    (*(double*)L6x)
#define       L7    (*(double*)L7x)
#define       L8    (*(double*)L8x)
#else	/* IEEE double */
static double
L1     =  6.6666666666667340202E-1    , /*Hex  2^ -1   *  1.5555555555592 */
L2     =  3.9999999999416702146E-1    , /*Hex  2^ -2   *  1.999999997FF24 */
L3     =  2.8571428742008753154E-1    , /*Hex  2^ -2   *  1.24924941E07B4 */
L4     =  2.2222198607186277597E-1    , /*Hex  2^ -3   *  1.C71C52150BEA6 */
L5     =  1.8183562745289935658E-1    , /*Hex  2^ -3   *  1.74663CC94342F */
L6     =  1.5314087275331442206E-1    , /*Hex  2^ -3   *  1.39A1EC014045B */
L7     =  1.4795612545334174692E-1    ; /*Hex  2^ -3   *  1.2F039F0085122 */
#endif

double log__L(z)
double z;
{
#if (defined(VAX)||defined(TAHOE))
    return(z*(L1+z*(L2+z*(L3+z*(L4+z*(L5+z*(L6+z*(L7+z*L8))))))));
#else	/* IEEE double */
    return(z*(L1+z*(L2+z*(L3+z*(L4+z*(L5+z*(L6+z*L7)))))));
#endif
}
