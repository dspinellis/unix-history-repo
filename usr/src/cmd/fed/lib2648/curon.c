#include "2648.h"

curon()
{
	if (_cursoron)
		return;
	sync();
	escseq(ESCD);
	outchar('k');
	_cursoron = 1;
}

curoff()
{
	if (!_cursoron)
		return;
	sync();
	escseq(ESCD);
	outchar('l');
	_cursoron = 0;
}
