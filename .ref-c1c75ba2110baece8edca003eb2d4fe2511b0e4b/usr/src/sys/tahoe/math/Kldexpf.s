/*	Kldexpf.s	1.3	86/01/05	*/

#include "../tahoe/SYS.h"
#include "../tahoemath/fp.h"
#include "../tahoemath/Kfp.h"

/* @(*)Kldexpf.s	4.2 (Berkeley) 12/21/80
 *	Tahoe 		2/2/83
 *
 * float Kldexpf (op_most, op_least, exp, hfs)
 *
 * Ldexp returns value*2**exp, if that result is in range.
 * If underflow occurs, it returns zero.  If overflow occurs,
 * it returns a value of appropriate sign and largest
 * possible magnitude.  In case of either overflow or underflow,
 * the external int "errno" is set to ERANGE.  Note that errno is
 * not modified if no error occurs, so if you intend to test it
 * after you use Kldexpf, you had better set it to something
 * other than ERANGE first (zero is a reasonable value to use).
 */

	.text
ENTRY(Kldexpf, R2)
	movl	4(fp),r0	/* Fetch "value" */
	movl	8(fp),r1

	andl3	$EXPMASK,r0,r2	/* r2 := shifted biased exponent */
	jeql	ld1		/* If it's zero, we're done */
	shar	$EXPSHIFT,r2,r2	/* shift to get value of exponent  */

	addl2	12(fp),r2	/* r2 := new biased exponent */
	jleq	under		/* if it's <= 0, we have an underflow */
	cmpl	r2,$256		/* Otherwise check if it's too big */
	jgeq	over		/* jump if overflow */
/*
 *	Construct the result and return
 */
	andl2	$0!EXPMASK,r0	/* clear old exponent */
	shal 	$EXPSHIFT,r2,r2	/* Put the exponent back in the result */
	orl2	r2,r0
ld1:	ret
/*
 *	Underflow
 */
under:	clrl	r0		/* Result is zero */
	clrl	r1
	orl2	$HFS_UNDF,*16(fp)
	jmp	err		/* Join general error code */
/*
 *	Overflow
 */
over:	movl	huge0,r0	/* Largest possible floating magnitude */
	movl	huge1,r1
	orl2	$HFS_OVF,*16(fp)
	orl2	$SIGNBIT,r0	/* If arg < 0, make result negative */

err:	orl2	$HFS_RANGE,*16(fp)	/* Indicate range error */
	ret

	.data
huge0:	.long	0x7fffffff
huge1:	.long	0xffffffff
