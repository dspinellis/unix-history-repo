/* Osbrk.c 4.1 82/12/04 */

#include "SYS.h"

#define	SYS_brk		17

	.globl	_end
	.globl	curbrk

	.data
curbrk:	.long	_end
	.text

ENTRY(sbrk)
	addl3	curbrk,4(ap),-(sp)
	pushl	$1
	movl	ap,r3
	movl	sp,ap
	chmk	$SYS_brk
	jcs 	err
	movl	curbrk,r0
	addl2	4(r3),curbrk
	ret
err:
	jmp	cerror
