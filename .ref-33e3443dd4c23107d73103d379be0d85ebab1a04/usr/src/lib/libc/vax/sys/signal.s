/* signal.s 5.2 84/11/04 */

#include "SYS.h"

	.globl	__sigsim
ENTRY(signal)
	blbs	__sigsim,2f
	chmk	$SYS_signal
	bcs	1f
	ret
1:
	jmp	cerror

/*
 * Must simulate signal 
 */
2:
	movq	4(ap),-(sp)
	calls	$2,_signalsim
	ret
