/* @(#)setbuffer.c	4.4 (Berkeley) %G% */
#include	<stdio.h>

setbuffer(iop, buf, size)
	register struct _iobuf *iop;
	char *buf;
	int size;
{
	if (iop->_base != NULL && iop->_flag&_IOMYBUF)
		free(iop->_base);
	iop->_flag &= ~(_IOMYBUF|_IONBF|_IOLBF);
	if ((iop->_base = buf) == NULL) {
		iop->_flag |= _IONBF;
		iop->_bufsiz = NULL;
	} else {
		iop->_ptr = iop->_base;
		iop->_bufsiz = size;
	}
	iop->_cnt = 0;
}

/*
 * set line buffering for either stdout or stderr
 */
setlinebuf(iop)
	register struct _iobuf *iop;
{
	char *buf;
	extern char *malloc();

	fflush(iop);
	setbuffer(iop, NULL, 0);
	buf = malloc(BUFSIZ);
	if (buf != NULL) {
		setbuffer(iop, buf, BUFSIZ);
		iop->_flag |= _IOLBF|_IOMYBUF;
	}
}
