#ifdef LIBC_SCCS
	.asciz	"@(#)urem.s	5.2 (Berkeley) 3/9/86"
#endif LIBC_SCCS

/*
 * urem - unsigned remainder for vax-11
 *
 * arguments: dividend, divisor
 * result: remainder
 * uses r0-r2
 *
 * if 1 < divisor <= 2147483647, zero-extend the dividend
 * to 64 bits and let ediv do the work.  If the divisor is 1,
 * ediv will overflow if bit 31 of the dividend is on, so
 * just return 0.  If the divisor is 0, do the ediv also,
 * so it will generate the proper exception.  All other values
 * of the divisor have bit 31 on: in this case the remainder
 * must be the dividend if divisor > dividend, and the dividend
 * minus the divisor otherwise.  The comparison must be unsigned.
 */
#include "DEFS.h"

ASENTRY(urem, 0)
	movl	4(ap),r0	/* dividend */
	movl	8(ap),r2	/* divisor */
	jeql	1f		/* if divisor=0, force exception */
	cmpl	r2,$1		/* if divisor <= 1 (signed), */
	jleq	2f		/*  no division is necessary */
1:
	clrl	r1		/* zero-extend the dividend */
	ediv	r2,r0,r2,r0	/* divide.  q->r2 (discarded), r->r0 */
	ret
2:
	jneq	1f		/* if divisor=1, return 0 */
	clrl	r0		/*  (because doing the divide will overflow */
	ret			/*  if the dividend has its high bit on) */
1:
	cmpl	r0,r2		/* if dividend < divisor (unsigned) */
	jlssu	1f		/*  remainder is dividend */
	subl2	r2,r0		/*  else remainder is dividend - divisor */
1:
	ret

/*
 * aurem - unsigned remainder for vax-11
 *
 * arguments: *dividend, divisor
 * result: remainder in r0 and *dividend
 * uses r0-r2
 */
#include "DEFS.h"

ASENTRY(aurem, 0)
	movl	*4(ap),r0	/* dividend */
	movl	8(ap),r2	/* divisor */
	jeql	1f		/* if divisor=0, force exception */
	cmpl	r2,$1		/* if divisor <= 1 (signed), */
	jleq	2f		/*  no division is necessary */
1:
	clrl	r1		/* zero-extend the dividend */
	ediv	r2,r0,r2,r0	/* divide.  q->r2 (discarded), r->r0 */
	movl	r0,*4(ap)	/* save result */
	ret
2:
	jneq	1f		/* if divisor=1, return 0 */
	clrl	r0		/*  (because doing the divide will overflow */
	clrl	*4(ap)		/*  if the dividend has its high bit on) */
	ret	
1:
	cmpl	r0,r2		/* if dividend < divisor (unsigned) */
	jlssu	1f		/*  remainder is dividend */
	subl2	r2,r0		/*  else remainder is dividend - divisor */
1:
	movl	r0,*4(ap)	/* save result */
	ret
