/*	Ksfp_exp.s	1.2	86/01/03	*/

#include "fp.h"
#include "Kfp.h"
#include "SYS.h"

ENTRY(Ksfpover, 0)
	movl	$HUGE0,r0
	movl	$HUGE1,r1
	ret

ENTRY(Ksfpunder, 0)
	clrl	r0
	clrl	r1
	ret

ENTRY(Ksfpzdiv, 0)
	divl2	$0,r1		# force divission by zero.
	ret
