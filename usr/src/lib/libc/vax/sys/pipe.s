/* pipe.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(pipe)
	movl	4(ap),r2
	movl	r0,(r2)+
	movl	r1,(r2)
	clrl	r0
	ret
