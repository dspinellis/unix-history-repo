#include "global.h"

/* exarith(a,b,c,lo,hi)
 * int a,b,c;
 * int *lo, *hi;
 * Exact arithmetic.
 * a,b and c are 32 bit 2's complement integers
 * calculates x=a*b+c to twice the precision of an int.
 * In the vax version, the 30 low bits only are returned
 * in *lo,and the next 32 bits of precision are returned in * hi.
 * this works since exarith is used either for calculating the sum of
 * two 32 bit numbers, (which is at most 33 bits), or
 * multiplying a 30 bit number by a 32 bit numbers,
 * which has a maximum precision of 62 bits.
 * If *phi is 0 or -1 then
 * x doesn't need any more than 31 bits plus sign to describe, so we
 * place the sign in the high two bits of *lo and return 0 from this
 * routine.  A non zero return indicates that x requires more than 31 bits
 * to describe.
 */
exarith(a,b,c,phi,plo)
int *phi, *plo;
{
asm("	emul	4(ap),8(ap),12(ap),r2	#r2 = a*b + c to 64 bits");
asm("	extzv	$0,$30,r2,*20(ap)	#get new lo");
asm("	extv	$30,$32,r2,r0		#get new carry");
asm("	beql	out			# hi = 0, no work necessary");
asm("	movl	r0,*16(ap)		# save hi");
asm("	mcoml	r0,r0			# Is hi = -1 (it'll fit in one word)");
asm("	bneq	out			# it doesn't");
asm("	bisl2	$0xc0000000,*20(ap)	# alter low so that it is ok.");
asm("out:	ret");
}
