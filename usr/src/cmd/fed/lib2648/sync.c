/*
 * Make the screen & screen mode look like what it's supposed to.
 *
 * There are two basic things to do here, put the _pen
 * in the right place, and make the line drawing mode be right.
 * We don't sync the cursor here, only when there's user input & it's on.
 */
#include "2648.h"

sync()
{
	if (_supx != _penx || _supy != _peny) {
		escseq(ESCP);
		outchar('a');
		motion(_supx, _supy);
	}
	if (_supsmode != _actsmode) {
		escseq(ESCM);
		switch (_actsmode = _supsmode) {
		case MX:
			outchar('3');
			break;
		case MC:
			outchar('1');
			break;
		case MS:
			outchar('2');
			break;
		}
		outchar('a');
	}
}
