/*	machdep.c	1.5	90/12/16	*/

#include "../tahoe/mem.h"
#include "../include/mtpr.h"
#include "../tahoe/SYS.h"

	.set	_scb, 0x0
	.set	HIGH, 0x1f	# mask for total disable
	.set	BERVEC, 0x80	# offset into scb of the bus error vector 
	.set	RESTVEC, 0x8	# offset into scb of the restart vector 

ENTRY(mtpr, 0)
	mtpr	8(fp),4(fp)
	ret

ENTRY(mfpr, 0)
	mfpr	4(fp),r0
	ret

ENTRY(bcopy, R2|R1|R0)
	movl	4(fp),r0
	movl	8(fp),r1
	movl	12(fp),r2
	movblk
	ret

/*
 * badaddr(addr, len)
 *	see if access addr with a len type instruction causes a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 *	r0 = 0 means good(exists); r0 =1 means does not exist.
 */
ENTRY(badaddr, R5|R4|R3|R2|R1)
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	mfpr	$SCBB,r5
	mtpr	$0,$SCBB
	movl	*$BERVEC,r2
	movl	4(fp),r3
	movl	8(fp),r4
	movab	9f,*$BERVEC
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0			# made it w/o machine checks
2:	movl	r2,*$BERVEC
	mtpr	r1,$IPL
	mtpr	r5,$SCBB
	ret

/*
 * wbadaddr(addr, len, value)
 *	see if write of value to addr with a len type instruction causes
 *	a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 *	r0 = 0 means good(exists); r0 =1 means does not exist.
 */
ENTRY(wbadaddr, R5|R4|R3|R2|R1)
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	mfpr	$SCBB,r5
	mtpr	$0,$SCBB
	movl	*$BERVEC,r2
	movl	4(fp),r3
	movl	8(fp),r4
	movab	9f,*$BERVEC
	bbc	$0,r4,1f; movb	15(fp), (r3)
1:	bbc	$1,r4,1f; movw	14(fp), (r3)
1:	bbc	$2,r4,1f; movl	12(fp), (r3)
1:	clrl	r0			# made it w/o machine checks
2:	movl	r2,*$BERVEC
	mtpr	r1,$IPL
	mtpr	r5,$SCBB
	ret

	.align	2
9:			# Here we catch buss error (if it comes)
	andl3	4(sp),$ERRCD,r0
	cmpl	r0,$APE
	jneq	1f
	halt			# Address parity error !!!
1:	cmpl	r0,$VBE
	jneq	1f
	halt			# Versabus error
1:
	movl	$1,r0		# Anything else = bad address
	movab	8(sp),sp	# discard buss error trash
	movab	2b,(sp)		# new program counter on stack.
	rei

ENTRY(movow, 0)
	movow	10(fp),*4(fp)
	ret

ENTRY(movob, 0)
	movob	11(fp),*4(fp)
	ret
