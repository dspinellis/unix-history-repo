#include	"fp.h"
#include	"fp_in_krnl.h"

	.globl	Ksfpover
Ksfpover:
	.word	0x0000
	movl	$HUGE0,r0
	movl	$HUGE1,r1
	ret

	.globl	Ksfpunder
Ksfpunder:
	.word	0x0000
	clrl	r0
	clrl	r1
	ret

	.globl	Ksfpzdiv
Ksfpzdiv:
	.word	0x0000
	divl2	$0,r1		# force divission by zero.
	ret
