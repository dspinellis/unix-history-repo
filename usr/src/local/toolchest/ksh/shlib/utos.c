/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)utos.c	1.1 */

/*
 *   UTOS.C
 *
 *   Programmer:  D. G. Korn
 *
 *         Date:  May 3, 1982
 *
 *
 *
 *   LTOS (SINT, BASE)
 *
 *        Return a pointer to a string denoting the value of
 *        the signed long integer SINT in base BASE.
 *
 *   UTOS (USINT, BASE)
 *
 *        Return a pointer to a string denoting the value of
 *        the unsigned long integer USINT in base BASE.
 *
 *
 *
 *   See Also:  arith(III)
 */


#define BASEMAX	 (4+16*sizeof(int))
static char hexstr[BASEMAX];
extern char hdigits[];
char *utos();
char *ltos();

/*
 *   LTOS (SINT, BASE)
 *
 *        long USINT;
 *
 *        int BASE;
 *
 *   Return a pointer to a string denoting the value of SINT 
 *   in base BASE.  The string will be stored within HEXSTR.
 *   It will begin with the base followed by a single '#'.
 *   A minus sign will be prepended for negative numbers
 *
 */


char *ltos(sint,base)
long sint;
int base;
{
	register char *sp;
	register long l = (sint>=0?sint:-sint);
#ifdef pdp11
	sp = utos(l,base);
#else
	sp = utos((unsigned long)l,base);
#endif /* pdp11 */
	if(sint<0)
		*--sp = '-';
	return(sp);
}

/*
 *   UTOS (USINT, BASE)
 *
 *        unsigned USINT;
 *
 *        int BASE;
 *
 *   Return a pointer to a string denoting the value of USINT 
 *   in base BASE.  The string will be stored within HEXSTR.
 *   It will begin with the base followed by a single '#'.
 *
 */



char *utos(usint,base)
register int base;
#ifdef pdp11
 /* unsigned longs are not supported on pdp11 */
long usint;
{
	long l = usint;
#else
unsigned long usint;
{
	register unsigned long l = usint;
#endif	/* pdp11 */
	register char *cp = hexstr+(BASEMAX-1);
	if(base < 2 || base > BASEMAX)
		return(cp);
	for(*cp = 0;cp > hexstr && l;l /= base)
		*--cp = hdigits[(l%base)<<1];
	if(usint==0)
		*--cp = '0';
	if(base==10)
		return(cp);
	*--cp = '#';
	*--cp = hdigits[(base%10)<<1];
	if(base /= 10)
		*--cp = hdigits[(base%10)<<1];
	return(cp);	
}
