/* brk.s 4.1 82/12/04 */

#include "SYS.h"

#define	SYS_brk		17

	.globl	curbrk

SYSCALL(brk)
	movl	4(ap),curbrk
	clrl	r0
	ret
