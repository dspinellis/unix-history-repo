/* setgid.c 4.1 82/12/04 */

#include "SYS.h"

ENTRY(setgid)
	pushl	4(ap)
	pushl	4(ap)
	CALL(2,setregid)
	ret
