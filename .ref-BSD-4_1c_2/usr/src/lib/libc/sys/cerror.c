/* cerror.c 4.1 82/12/04 */

#include "SYS.h"

	.globl	_errno
cerror:
	movl	r0,_errno
	mnegl	$1,r0
	ret
