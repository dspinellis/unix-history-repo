/*	abort.s	4.3	85/01/09	*/

/* C library -- abort */

#include "DEFS.h"

ENTRY(abort, 0)
	pushl	$-1
	calls	$1,_sigblock	# sigblock(~0);
	pushl	$0
	pushl	$4
	calls	$2,_signal	# signal(SIGILL, SIG_DFL);
	pushl	$-9
	calls	$1,_sigsetmask	# sigsetmask(~sigmask(SIGILL));
	halt
	clrl	r0
	ret
