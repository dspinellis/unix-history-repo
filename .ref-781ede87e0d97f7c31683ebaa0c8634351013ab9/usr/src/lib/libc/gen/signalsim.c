/*	signalsim.c	4.4	84/11/04	*/

/*
 * Almost backwards compatible signal.
 */
#include <signal.h>

enum {NATIVE = 0x0, UNKNOWN = 0x1, SIMULATE = 0x3} _sigsim = UNKNOWN;

int (*
signalsim(s, a))()
	int s, (*a)();
{
	struct sigvec osv, sv;
	int badcall();

	switch (_sigsim) {
	case UNKNOWN:
		sv.sv_handler = badcall;
		sv.sv_mask = sv.sv_onstack = 0;
		if (sigvec(SIGSYS, &sv, &osv) < 0) {
			_sigsim = SIMULATE;
		} else {
			_sigsim = NATIVE;
			signal(SIGSYS, osv.sv_handler);
		}
		/* fall through */
	case NATIVE:
	case SIMULATE:
		sv.sv_handler = a;
		sv.sv_mask = sv.sv_onstack = 0;
		if (sigvec(s, &sv, &osv) < 0)
			return (BADSIG);
		return (osv.sv_handler);
	}
}

/*
 * Signal call will trap to here if not implemented
 */
badcall()
{
	_sigsim = SIMULATE;
}
