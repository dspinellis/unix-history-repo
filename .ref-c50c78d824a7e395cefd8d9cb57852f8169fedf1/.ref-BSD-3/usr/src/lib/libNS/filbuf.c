/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>
_filbuf(iop)
register struct _iobuf *iop;
{
	static char smallbuf[_NFILE];
	register n;
	char *malloc();

	if ((iop->_flag&_IOREAD) == 0)
		_error("Reading bad file\n");
	if (iop->_flag&_IOSTRG)
		return(-1);
tryagain:
	if (iop->_base==NULL) {
		if (iop->_flag&_IONBF) {
			iop->_base = &smallbuf[fileno(iop)];
			goto tryagain;
		}
		if ((iop->_base = malloc(BUFSIZ)) == NULL) {
			iop->_flag |= _IONBF;
			goto tryagain;
		}
		iop->_flag |= _IOMYBUF;
	}
	if((iop->_flag & _IODIRT) && !(iop->_flag & _IONBF) && (iop->_flag & _IOWRT)) {
		if (iop->_delta)
			if (lseek(iop->_file,(long)  -iop->_delta, 1) < 0) {
				_error("Seek error in filbuf\n");
				iop->_flag |= _IOERR;
			}
		if( 0 < (n = iop->_ptr - iop->_base))
			if( n != write(iop->_file, iop->_base, n) )
				iop->_flag |= _IOERR;
		iop->_flag &= ~_IODIRT;
	}
	iop->_ptr = iop->_base;
	iop->_cnt = read(fileno(iop), iop->_ptr, iop->_flag&_IONBF?1:BUFSIZ);
	iop->_delta = iop->_cnt;
	if (--iop->_cnt < 0) {
		if (iop->_cnt == -1)
			iop->_flag |= _IOEOF;
		else
			iop->_flag |= _IOERR;
		iop->_cnt = 0;
		return(-1);
	}
	return(*iop->_ptr++&0377);
}
