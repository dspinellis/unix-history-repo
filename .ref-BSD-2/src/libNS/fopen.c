/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>

struct _iobuf *fopen(file, mode)
register char *mode;
{
	register f;
	register struct _iobuf *iop;
	int unixmode, a_flag;

	for (iop = _iob; iop->_flag&(_IOREAD|_IOWRT); iop++)
		if (iop >= _iob + _NFILE)
			return(NULL);
	iop->_flag = 0;
	iop->_delta = 0;
	a_flag = 0;
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
			iop->_flag |= _IOWRT ;
		}
	}
	if((unixmode = (iop->_flag & 3) - 1) < 0) {
		unixmode = 0;
		iop->_flag = 1;
	}
	if ((iop->_flag & _IOWRT) && a_flag==0 ) {
		f = creat(file, 0644);
		if((iop->_flag &  _IOREAD) && (f>0)) {
			close(f);
			f = open(file,2);
		}
	}
	else
		if ((0 >(f = open(file, unixmode))) && (a_flag || !(iop->_flag&_IOREAD))) {
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
