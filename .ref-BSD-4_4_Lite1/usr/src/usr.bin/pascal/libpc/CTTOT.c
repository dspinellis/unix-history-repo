/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)CTTOT.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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
