#include	<stdio.h>
#include	<errno.h>

FILE *
fopen(file, mode)
char *file;
register char *mode;
{
	extern int errno;
	register f;
	register FILE *iop;
	extern FILE *_lastbuf;

	for (iop = _iob; iop->_flag&(_IOREAD|_IOWRT); iop++)
		if (iop >= _lastbuf)
			return(NULL);
	if (*mode=='w')
		f = creat(file, 0666);
	else if (*mode=='a') {
		if ((f = open(file, 1)) < 0) {
			if (errno == ENOENT)
				f = creat(file, 0666);
		}
		if (f >= 0)
			lseek(f, 0L, 2);
	} else
		f = open(file, 0);
	if (f < 0)
		return(NULL);
	iop->_cnt = 0;
	iop->_file = f;
	if (*mode != 'r')
		iop->_flag |= _IOWRT;
	else
		iop->_flag |= _IOREAD;
	return(iop);
}
