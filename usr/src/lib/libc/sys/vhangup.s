/* vhangup.s 4.2 82/12/04 */

#include "SYS.h"

#define SYS_vhangup 72

SYSCALL(vhangup)
	ret
