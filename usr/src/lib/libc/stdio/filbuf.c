/* @(#)filbuf.c	4.4 (Berkeley) %G% */
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
char	*malloc();

_filbuf(iop)
register FILE *iop;
{
	int size;
	struct stat stbuf;
	static char smallbuf[_NFILE];
	extern char _sibuf[];

	if (iop->_flag & _IORW)
		iop->_flag |= _IOREAD;

	if ((iop->_flag&_IOREAD) == 0)
		return(EOF);
	if (iop->_flag&_IOSTRG)
		return(EOF);
tryagain:
	if (iop->_base==NULL) {
		if (iop->_flag&_IONBF) {
			iop->_base = &smallbuf[fileno(iop)];
			goto tryagain;
		}
		if (fstat(fileno(iop), &stbuf) < 0 || stbuf.st_blksize <= NULL)
			size = BUFSIZ;
		else
			size = stbuf.st_blksize;
		if (iop == stdin)
			iop->_base = _sibuf;
		else {
			if ((iop->_base = malloc(size)) == NULL) {
				iop->_flag |= _IONBF;
				goto tryagain;
			}
			iop->_flag |= _IOMYBUF;
		}
		iop->_bufsiz = size;
	}
	if (iop == stdin && (stdout->_flag&_IOLBF))
		fflush(stdout);
	iop->_cnt = read(fileno(iop), iop->_base,
		iop->_flag & _IONBF ? 1 : iop->_bufsiz);
	iop->_ptr = iop->_base;
	if (--iop->_cnt < 0) {
		if (iop->_cnt == -1) {
			iop->_flag |= _IOEOF;
			if (iop->_flag & _IORW)
				iop->_flag &= ~_IOREAD;
		} else
			iop->_flag |= _IOERR;
		iop->_cnt = 0;
		return(-1);
	}
	return(*iop->_ptr++&0377);
}
