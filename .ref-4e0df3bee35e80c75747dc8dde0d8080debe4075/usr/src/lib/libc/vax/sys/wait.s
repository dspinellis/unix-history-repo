/* wait.s 4.1 82/12/28 */

#include "SYS.h"

SYSCALL(wait)
	tstl	4(ap)
	jeql	1f
	movl	r1,*4(ap)
1:
	ret
