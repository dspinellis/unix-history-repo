#ifdef LIBC_SCCS
	.asciz	"@(#)modf.s	5.2 (Berkeley) 3/9/86"
#endif LIBC_SCCS

/*
 * double modf (value, iptr)
 * double value, *iptr;
 *
 * Modf returns the fractional part of "value",
 * and stores the integer part indirectly through "iptr".
 */

#include "DEFS.h"

ENTRY(modf, 0)
	emodd	4(ap),$0,$0f1.0,r2,r0
	jvs	1f			# integer overflow
	cvtld	r2,*12(ap)
	ret
1:
	subd3	r0,4(ap),*12(ap)
	ret
