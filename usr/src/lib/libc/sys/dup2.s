/* dup2.s 4.1 82/12/04 */

#include "SYS.h"

ENTRY(dup2)
	bisb2	$0100,4(ap)		/* XXX */
	chmk	$SYS_dup		/* XXX */
	jcs	err
	ret
err:
	jmp	cerror
