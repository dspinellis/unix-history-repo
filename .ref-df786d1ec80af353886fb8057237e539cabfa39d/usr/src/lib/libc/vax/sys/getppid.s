/* getppid.s 4.1 82/12/04 */

#include "SYS.h"

PSEUDO(getppid,getpid)
	movl	r1,r0
	ret		# ppid = getppid();
