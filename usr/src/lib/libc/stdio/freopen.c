/* @(#)freopen.c	4.2 (Berkeley) 3/9/81 */
#include	<stdio.h>
#include	<errno.h>

FILE *
freopen(file, mode, iop)
char *file;
register char *mode;
register FILE *iop;
{
	extern int errno;
	register f, rw;

	rw = mode[1] == '+';

	fclose(iop);
	if (*mode=='w') {
		f = creat(file, 0666);
		if (rw && f>=0) {
			close(f);
			f = open(file, 2);
		}
	} else if (*mode=='a') {
		if ((f = open(file, rw? 2: 1)) < 0) {
			if (errno == ENOENT) {
				f = creat(file, 0666);
				if (rw && f>=0) {
					close(f);
					f = open(file, 2);
				}
			}
		}
		if (f >= 0)
			lseek(f, 0L, 2);
	} else
		f = open(file, rw? 2: 0);
	if (f < 0)
		return(NULL);
	iop->_cnt = 0;
	iop->_file = f;
	if (rw)
		iop->_flag |= _IORW;
	else if (*mode != 'r')
		iop->_flag |= _IOWRT;
	else
		iop->_flag |= _IOREAD;
	return(iop);
}
