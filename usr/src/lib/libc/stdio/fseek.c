#ifndef lint
static char sccsid[] = "@(#)fseek.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Seek for standard library.  Coordinates with buffering.
 */

#include	<stdio.h>

long lseek();

long
fseek(iop, offset, ptrname)
	register FILE *iop;
	long offset;
{
	register resync, c;
	long p, curpos = -1;

	iop->_flag &= ~_IOEOF;
	if (iop->_flag&_IOREAD) {
		if (ptrname<2 && iop->_base &&
			!(iop->_flag&_IONBF)) {
			c = iop->_cnt;
			p = offset;
			if (ptrname==0) {
				curpos = lseek(fileno(iop), 0L, 1);
				if (curpos == -1)
					return (-1);
				p += c - curpos;
			} else
				offset -= c;
			if(!(iop->_flag&_IORW) && c>0&&p<=c
			    && p>=iop->_base-iop->_ptr){
				iop->_ptr += (int)p;
				iop->_cnt -= (int)p;
				if (curpos == -1)
					curpos = lseek(fileno(iop), 0L, 1);
				return (curpos == -1? -1: curpos - iop->_cnt);
			}
			resync = offset&01;
		} else 
			resync = 0;
		if (iop->_flag & _IORW) {
			iop->_ptr = iop->_base;
			iop->_flag &= ~_IOREAD;
			resync = 0;
		}
		p = lseek(fileno(iop), offset-resync, ptrname);
		iop->_cnt = 0;
		if (resync && getc(iop) != EOF && p != -1)
			p++;
	}
	else if (iop->_flag & (_IOWRT|_IORW)) {
		fflush(iop);
		if (iop->_flag & _IORW) {
			iop->_cnt = 0;
			iop->_flag &= ~_IOWRT;
			iop->_ptr = iop->_base;
		}
		p = lseek(fileno(iop), offset, ptrname);
	}
	return(p);
}
