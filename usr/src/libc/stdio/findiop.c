#include <stdio.h>

FILE *
_findiop()
{
	extern FILE *_lastbuf;
	register FILE *iop;

	for(iop = _iob; iop->_flag & (_IOREAD|_IOWRT|_IORW); iop++)
		if (iop >= _lastbuf)
			return(NULL);

	return(iop);
}
