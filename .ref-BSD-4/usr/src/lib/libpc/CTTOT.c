/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CTTOT.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

long	_mask[] = {	
		    0xffffffff , 0xfffffffe , 0xfffffffc , 0xfffffff8 ,
		    0xfffffff0 , 0xffffffe0 , 0xffffffc0 , 0xffffff80 ,
		    0xffffff00 , 0xfffffe00 , 0xfffffc00 , 0xfffff800 ,
		    0xfffff000 , 0xffffe000 , 0xffffc000 , 0xffff8000 ,
		    0xffff0000 , 0xfffe0000 , 0xfffc0000 , 0xfff80000 ,
		    0xfff00000 , 0xffe00000 , 0xffc00000 , 0xff800000 ,
		    0xff000000 , 0xfe000000 , 0xfc000000 , 0xf8000000 ,
		    0xf0000000 , 0xe0000000 , 0xc0000000 , 0x80000000 ,
		    0x00000000
		 };
/*
 * Constant set constructor
 */

long *
CTTOT(result, lowerbnd, upperbnd, paircnt, singcnt, data)

	long	*result;	/* pointer to final set */
	int	lowerbnd;	/* lower bound of set */
	int	upperbnd;	/* upper - lower of set */
	int	paircnt;	/* number of pairs to construct */
	int	singcnt;	/* number of singles to construct */
	int	data;		/* paircnt plus singcnt sets of data */
{
	register int	lower;
	register int	lowerdiv;
	register int	lowermod;
	register int	upper;
	int		upperdiv;
	int		uppermod;
	register int	*dataptr;
	register long	*lp;
	long		*limit;
	long		temp;
	long		cnt;

	limit = &result[(upperbnd + 1 + BITSPERLONG - 1) / BITSPERLONG];
	for (lp = result; lp < limit; )
		*lp++ = 0;
	dataptr = &data;
	for (cnt = 0; cnt < paircnt; cnt++) {
		upper = *dataptr++ - lowerbnd;
		if (upper < 0 || upper > upperbnd) {
			ERROR(ECTUPR, *--dataptr);
			return;
		}
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			ERROR(ECTLWR, *--dataptr);
			return;
		}
		if (lower > upper) {
			continue;
		}
		lowerdiv = lower / BITSPERLONG;
		lowermod = lower % BITSPERLONG;
		upperdiv = upper / BITSPERLONG;
		uppermod = upper % BITSPERLONG;
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
	for (cnt = 0; cnt < singcnt; cnt++) {
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			ERROR(ECTSNG, *--dataptr);
			return;
		}
		lowerdiv = lower / BITSPERLONG;
		lowermod = lower % BITSPERLONG;
		result[ lowerdiv ] |= ( 1 << lowermod );
	}
	return(result);
}
