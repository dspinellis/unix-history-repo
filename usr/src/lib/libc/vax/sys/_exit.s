/* _exit.s 4.1 82/12/04 */

#include "SYS.h"

	.align	1
PSEUDO(_exit,exit)
			# _exit(status)
