/*	Kfp_exp.s	1.2	86/01/03	*/

#include "fp.h"
#include "Kfp.h"
#include "SYS.h"

ENTRY(Kfpover, 0)
	movl	$HUGE0,r0
	movl	$HUGE1,r1
	ret

ENTRY(Kfpunder, 0)
	clrl	r0
	clrl	r1
	ret

ENTRY(Kfpzdiv, 0)
	divl2	$0,r1		# force division by zero.
	ret
