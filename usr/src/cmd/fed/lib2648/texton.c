#include "2648.h"

texton()
{
	sync();
	escseq(TEXT);
}

textoff()
{
	sync();

	/*
	 * The following is needed because going into text mode
	 * leaves the pen where the cursor last was.
	 */
	_penx = -40; _peny = 40;
	escseq(ESCP);
	outchar('a');
	motion(_supx, _supy);
	_penx = _supx; _peny = _supy;
}
