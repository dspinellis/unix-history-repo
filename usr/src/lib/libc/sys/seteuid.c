/* seteuid.c 4.1 82/12/04 */

#include "SYS.h"

ENTRY(seteuid)
	pushl	4(ap)
	pushl	$-1
	CALL(2,setreuid)
	ret
