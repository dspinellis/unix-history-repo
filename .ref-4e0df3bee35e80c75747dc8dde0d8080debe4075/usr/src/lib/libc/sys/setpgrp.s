/* setpgrp.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(setpgrp)
	ret		# setpgrp(pid, pgrp);
