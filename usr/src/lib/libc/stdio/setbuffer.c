/* @(#)setbuffer.c	4.2 (Berkeley) 2/27/83 */
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
	static char _sebuf[BUFSIZ];
	extern char _sobuf[];

	if (iop != stdout && iop != stderr)
		return;
	fflush(iop);
	setbuffer(iop, NULL, 0);
	setbuffer(iop, iop == stderr ? _sebuf : _sobuf, BUFSIZ);
	iop->_flag |= _IOLBF;
}
