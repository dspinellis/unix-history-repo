/* gethostname.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(gethostname)
	ret		# len = gethostname(buf, buflen)
