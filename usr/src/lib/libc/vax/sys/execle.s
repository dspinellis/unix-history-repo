/* execle.s 4.1 82/12/04 */

#include "SYS.h"

ENTRY(execle)
	movl	(ap),r0
	pushl	(ap)[r0]
	pushab	8(ap)
	pushl	4(ap)
	calls	$3,_execve
	ret		# execle(file, arg1, arg2, ..., env);
