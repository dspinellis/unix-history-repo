/*	signalsim.c	4.2	83/06/10	*/

/*
 * Backwards compatible signal.
 */
#include <signal.h>

int (*
signal(s, a))()
	int s, (*a)();
{
	struct sigvec osv, sv;

	sv.sv_handler = a;
	sv.sv_mask = sv.sv_onstack = 0;
	if (sigvec(s, &sv, &osv) < 0)
		return (BADSIG);
	return (osv.sv_handler);
}
