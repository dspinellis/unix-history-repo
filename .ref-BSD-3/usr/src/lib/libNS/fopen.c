/* Copyright (c) 1979 Regents of the University of California */
#include	<nstdio.h>

struct _iobuf *fopen(file, mode)
register char *mode;
{
	register f;
	register struct _iobuf *iop;
	int unixmode, a_flag, noclob;

	for (iop = _iob; iop->_flag&(_IOREAD|_IOWRT); iop++)
		if (iop >= _iob + _NFILE)
			return(NULL);
	iop->_flag = 0;
	iop->_delta = 0;
	a_flag = 0;
	noclob = 0;
	for(;*mode;mode++) {
		switch(*mode) {

		case 'w':
			iop->_flag |= _IOWRT;
			break;

		case 'r':
			iop->_flag |= _IOREAD;
			break;

		case 'a':
			a_flag = 1;
			noclob = 1;
			iop->_flag |= _IOWRT ;
			break;

		case '+':
			if (iop->_flag & _IOREAD)
				noclob = 1;
			iop->_flag |= (_IOREAD | _IOWRT);
			break;
		}
	}
	if((unixmode = (iop->_flag & 3) - 1) < 0) {
		unixmode = 0;
		iop->_flag = 1;
	}
	if ((iop->_flag & _IOWRT) && noclob==0 ) {
		f = creat(file, 0644);
		if((iop->_flag &  _IOREAD) && (f>0)) {
			close(f);
			f = open(file,2);
		}
	}
	else
		if ((0 >(f = open(file, unixmode))) && (noclob || !(iop->_flag&_IOREAD))) {
			f = creat(file, 0644);
			close(f);
			f = open(file,unixmode);
		}

	if (f < 0)
		return(NULL);
	if (a_flag)
		lseek(f, (long) 0, 2);
	iop->_file = f;
	return(iop);
}
