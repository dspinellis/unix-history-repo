/* exect.s 4.1 82/12/04 */

#include "SYS.h"
#include <sys/psl.h>

ENTRY(exect)
	bispsw	$PSL_T
	chmk	$SYS_execve
	jmp	cerror		# exect(file, argv, env)
