/*	signal.c	4.1	84/05/16	*/
/*
 * Almost backwards compatible signal.
 *	int (*signal(s, a))() int s, (*a)();
 */
#include <syscall.h>
#include "DEFS.h"

ENTRY(signal)
	subl2	$24,sp			# struct sigvec osv, sv;
	movl	8(ap),-24(fp)		# sv.sv_handler = a;
	clrq	-20(fp)			# sv.sv_mask = sv.sv_onstack = 0;
	pushal	-12(fp)			# &osv
	pushal	-24(fp)			# &sv
	pushl	4(ap)			# s
	moval	-4(sp),ap
	chmk	$SYS_sigvec		# sigvec(s, &sv, &osv)
	jcs	err
	movl	-12(fp),r0		# return osv.sv_handler;
	ret
err:
	.globl	_errno
	movl	r0,_errno
	mnegl	$1,r0
	ret
