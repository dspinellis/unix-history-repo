#include <stdio.h>

FILE *
freopen(file, mode, iop)
char *file;
register char *mode;
register FILE *iop;
{
	register f;

	fclose(iop);
	if (*mode=='w')
		f = creat(file, 0666);
	else if (*mode=='a') {
		if ((f = open(file, 1)) < 0)
			f = creat(file, 0666);
		lseek(f, 0L, 2);
	} else
		f = open(file, 0);
	if (f < 0)
		return(NULL);
	iop->_file = f;
	if (*mode != 'r')
		iop->_flag |= _IOWRT;
	else
		iop->_flag |= _IOREAD;
	return(iop);
}
