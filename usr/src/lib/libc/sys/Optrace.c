/* Optrace.c 4.1 82/12/04 */

#include "SYS.h"

#define	SYS_ptrace	26

ENTRY(ptrace)
	clrl	_errno
	chmk	$SYS_ptrace
	jcs	err
	ret
err:
	jmp	cerror
