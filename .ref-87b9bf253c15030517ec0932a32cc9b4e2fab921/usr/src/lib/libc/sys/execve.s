/* execve.s 4.1 82/12/04 */

#include "SYS.h"

SYSCALL(execve)
	ret		# execve(file, argv, arge)
