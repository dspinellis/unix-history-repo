/* setregid.c 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(setregid)
	ret		# setregid(rgid, egid)
