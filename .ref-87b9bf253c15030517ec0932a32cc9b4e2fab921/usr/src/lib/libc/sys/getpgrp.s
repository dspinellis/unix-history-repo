/* getpgrp.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(getpgrp)
	ret		# pgrp = getpgrp(pid);
