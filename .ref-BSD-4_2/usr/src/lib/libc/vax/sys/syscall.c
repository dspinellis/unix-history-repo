/* syscall.c 4.2 83/06/27 */

#include "SYS.h"

ENTRY(syscall)
	movl	4(ap),r0	# syscall number
	subl3	$1,(ap)+,(ap)	# one fewer arguments
	chmk	r0
	jcs	1f
	ret
1:
	jmp	cerror
