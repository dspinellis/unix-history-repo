#ifdef LIBC_SCCS
	.asciz	"@(#)udiv.s	5.2 (Berkeley) 3/9/86"
#endif LIBC_SCCS

/*
 * udiv - unsigned division for vax-11
 *
 * arguments: dividend, divisor.
 * result: quotient.
 * uses r0-r2
 *
 * If 1 < divisor <= 2147483647, zero-extend the dividend
 * to 64 bits and let ediv do the work.  If the divisor is 1,
 * ediv will overflow if bit 31 of the dividend is on, so
 * just return the dividend unchanged.  If the divisor is 0,
 * do the ediv also, so it will generate the proper exception.
 * All other values of the divisor have bit 31 on: in this case
 * the quotient must be 0 if divisor > dividend, and 1 otherwise,
 * provided that the comparison is made as unsigned.
 */

#include "DEFS.h"

ASENTRY(udiv, 0)
	movl	4(ap),r0	/* dividend */
	movl	8(ap),r2	/* divisor */
	jeql	1f		/* if divisor=0, force exception */
	cmpl	r2,$1		/* if divisor <= 1 (signed), */
	jleq	2f		/*  no division is necessary */
1:
	clrl	r1		/* zero-extend the dividend */
	ediv	r2,r0,r0,r2	/* divide.  q->r0, r->r2 (discarded) */
	ret
2:	
	jeql	1f		/* if divisor=1, return dividend */
	cmpl	r0,r2		/* unsigned comparison between */
	jgequ	2f		/*  dividend and divisor */
	clrl	r0		/* dividend < divisor, return 0 */
	ret
2:	
	movl	$1,r0		/* dividend >= divisor, return 1 */
1:	
	ret

/*
 * audiv - unsigned division for vax-11
 *
 * arguments: *dividend, divisor.
 * result: quotient in r0 and *dividend.
 * uses r0-r2
 */

#include "DEFS.h"

ASENTRY(audiv, 0)
	movl	*4(ap),r0	/* dividend */
	movl	8(ap),r2	/* divisor */
	jeql	1f		/* if divisor=0, force exception */
	cmpl	r2,$1		/* if divisor <= 1 (signed), */
	jleq	2f		/*  no division is necessary */
1:
	clrl	r1		/* zero-extend the dividend */
	ediv	r2,r0,r0,r2	/* divide.  q->r0, r->r2 (discarded) */
	movl	r0,*4(ap)	/* save result */
	ret
2:
	jeql	1f		/* if divisor=1, return dividend */
	cmpl	r0,r2		/* unsigned comparison between */
	jgequ	2f		/*  dividend and divisor */
	clrl	r0		/* dividend < divisor, return 0 */
	clrl	*4(ap)		/* save result */
	ret
2:
	movl	$1,r0		/* dividend >= divisor, return 1 */
1:
	movl	r0,*4(ap)	/* save result */
	ret
