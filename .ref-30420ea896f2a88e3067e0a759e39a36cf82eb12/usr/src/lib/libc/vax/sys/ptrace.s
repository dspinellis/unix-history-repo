/* ptrace.s 4.2 83/06/30 */

#include "SYS.h"

ENTRY(ptrace)
	clrl	_errno
	chmk	$SYS_ptrace
	jcs	err
	ret
err:
	jmp	cerror
