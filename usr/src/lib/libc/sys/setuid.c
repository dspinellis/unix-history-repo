/* setuid.c 4.1 82/12/04 */

#include "SYS.h"

ENTRY(setuid)
	pushl	4(ap)
	pushl	4(ap)
	CALL(2,setreuid)
	ret
