/* execv.s 4.1 82/12/04 */

#include "SYS.h"

ENTRY(execv)
	.globl	_environ
	pushl	_environ
	pushl	8(ap)
	pushl	4(ap)
	calls	$3,_execve
	jmp	cerror		# execv(file, argv)
