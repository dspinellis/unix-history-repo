/* Oumask.c 4.1 82/12/04 */

#include "SYS.h"

#define	SYS_umask	60

SYSCALL(umask)
	ret
