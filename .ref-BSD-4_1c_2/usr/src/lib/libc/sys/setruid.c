/* setruid.c 4.1 82/12/04 */

#include "SYS.h"

ENTRY(setruid)
	pushl	$-1
	pushl	4(ap)
	CALL(2,setreuid)
	ret
