/* setgroups.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(setgroups)
	ret		# setgroups(gidsetsize, gidset)
