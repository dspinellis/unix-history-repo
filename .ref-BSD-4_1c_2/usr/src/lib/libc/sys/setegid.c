/* setegid.c 4.1 82/12/04 */

#include "SYS.h"

ENTRY(setegid)
	pushl	4(ap)
	pushl	$-1
	CALL(2,setregid)
	ret
