/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CTTOT.c 1.5 %G%";

#include "whoami.h"
#include "h00vars.h"

long	_mask[] = {	
#		ifdef DEC11
		    0xffffffff , 0xfffffffe , 0xfffffffc , 0xfffffff8 ,
		    0xfffffff0 , 0xffffffe0 , 0xffffffc0 , 0xffffff80 ,
		    0xffffff00 , 0xfffffe00 , 0xfffffc00 , 0xfffff800 ,
		    0xfffff000 , 0xffffe000 , 0xffffc000 , 0xffff8000 ,
		    0xffff0000 , 0xfffe0000 , 0xfffc0000 , 0xfff80000 ,
		    0xfff00000 , 0xffe00000 , 0xffc00000 , 0xff800000 ,
		    0xff000000 , 0xfe000000 , 0xfc000000 , 0xf8000000 ,
		    0xf0000000 , 0xe0000000 , 0xc0000000 , 0x80000000 ,
		    0x00000000
#		else
		    0xffffffff , 0xfeffffff , 0xfcffffff , 0xf8ffffff ,
		    0xf0ffffff , 0xe0ffffff , 0xc0ffffff , 0x80ffffff ,
		    0x00ffffff , 0x00feffff , 0x00fcffff , 0x00f8ffff ,
		    0x00f0ffff , 0x00e0ffff , 0x00c0ffff , 0x0080ffff ,
		    0x0000ffff , 0x0000feff , 0x0000fcff , 0x0000f8ff ,
		    0x0000f0ff , 0x0000e0ff , 0x0000c0ff , 0x000080ff ,
		    0x000000ff , 0x000000fe , 0x000000fc , 0x000000f8 ,
		    0x000000f0 , 0x000000e0 , 0x000000c0 , 0x00000080 ,
		    0x00000000
#		endif DEC11
	    };
/*
 * Constant set constructors.
 *
 * CTTOT is called from compiled Pascal.  It takes the list of ranges
 * and single elements on the stack, varargs style.
 *
 * CTTOTA is called from the px interpreter.  It takes a pointer to the
 * list of ranges and single elements.
 *
 * This was easier than changing the compiler to pass a pointer into
 * its own partially-constructed stack, while working to make px portable.
 */

long *CTTOTA();

long *
CTTOT(result, lwrbnd, uprbnd, paircnt, singcnt, data)

	long	*result;	/* pointer to final set */
	long	lwrbnd;		/* lower bound of set */
	long	uprbnd;		/* upper - lower of set */
	long	paircnt;	/* number of pairs to construct */
	long	singcnt;	/* number of singles to construct */
	long	data;		/* paircnt plus singcnt sets of data */
{
	return CTTOTA(result, lwrbnd, uprbnd, paircnt, singcnt, &data);
}

long *
CTTOTA(result, lwrbnd, uprbnd, paircnt, singcnt, dataptr)

	register long	*result;	/* pointer to final set */
	long	lwrbnd;			/* lower bound of set */
	long	uprbnd;			/* upper - lower of set */
	long	paircnt;		/* number of pairs to construct */
	long	singcnt;		/* number of singles to construct */
	register long	*dataptr;	/* ->paircnt plus singcnt data values */
{
	int		lowerbnd = lwrbnd;
	int		upperbnd = uprbnd;
	register long	*lp;
	register char	*cp;
	register long	temp;
	long		*limit;
	int		lower;
	int		lowerdiv;
	int		lowermod;
	int		upper;
	int		upperdiv;
	int		uppermod;
	int		cnt;

	limit = &result[(upperbnd + 1 + BITSPERLONG - 1) >> LG2BITSLONG];
	for (lp = result; lp < limit; )
		*lp++ = 0;
	for (cnt = 0; cnt < paircnt; cnt++) {
		upper = *dataptr++ - lowerbnd;
		if (upper < 0 || upper > upperbnd) {
			ERROR("Range upper bound of %D out of set bounds\n",
				*--dataptr);
		}
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			ERROR("Range lower bound of %D out of set bounds\n",
				*--dataptr);
		}
		if (lower > upper) {
			continue;
		}
		lowerdiv = lower >> LG2BITSLONG;
		lowermod = lower & MSKBITSLONG;
		upperdiv = upper >> LG2BITSLONG;
		uppermod = upper & MSKBITSLONG;
		temp = _mask [lowermod];
		if ( lowerdiv == upperdiv ) {
			temp &= ~_mask[ uppermod + 1 ];
		}
		result[ lowerdiv ] |= temp;
		limit = &result[ upperdiv-1 ];
		for ( lp = &result[ lowerdiv+1 ] ; lp <= limit ; lp++ ) {
			*lp |= ~0;
		}
		if ( lowerdiv != upperdiv ) {
			result[ upperdiv ] |= ~_mask[ uppermod + 1 ];
		}
	}
	for (cnt = 0, cp = (char *)result; cnt < singcnt; cnt++) {
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			ERROR("Value of %D out of set bounds\n", *--dataptr);
		}
		cp[ lower >> LG2BITSBYTE ] |= (1 << (lower & MSKBITSBYTE));
	}
	return(result);
}
