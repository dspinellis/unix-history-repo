/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)stdio.c	1.1 */
/*
 * adapted for ksh from System V stdio library
 */
#include <stdio.h>

#ifndef _IOLBF
#define _IOLBF	0400
#endif



extern long lseek();
extern int fflush();

#ifdef VENIX
#define unsigned
#endif	/* VENIX */

#ifdef BSD
#define unsigned
#endif	/* BSD */

extern unsigned char _sobuf[];

int
fseek(iop, offset, ptrname)
register FILE *iop;
long	offset;
register int	ptrname;
{
	register int c;
	long	p;

	iop->_flag &= ~_IOEOF;
	if(iop->_flag & _IOREAD)
	{
		if(ptrname < 2 && iop->_base && !(iop->_flag&_IONBF))
		{
			c = iop->_cnt;
			p = offset;
			if(ptrname == 0)
				p += (long)c-lseek(fileno(iop), 0L, 1);
			else
				offset -= (long)c;
			if(!(iop->_flag&_IORW) && c > 0 && p <= c &&
					p >= iop->_base - iop->_ptr)
			{
				iop->_ptr += (int)p;
				iop->_cnt -= (int)p;
				return(0);
			}
		}
		if(iop->_flag & _IORW)
		{
			iop->_ptr = iop->_base;
			iop->_flag &= ~_IOREAD;
		}
		p = lseek(fileno(iop), offset, ptrname);
		iop->_cnt = 0;
	}
	else if(iop->_flag & (_IOWRT | _IORW))
	{
		(void) fflush(iop);
		if(iop->_flag & _IORW)
		{
			iop->_cnt = 0;
			iop->_flag &= ~_IOWRT;
			iop->_ptr = iop->_base;
		}
		p = lseek(fileno(iop), offset, ptrname);
	}
	return((p == -1)? -1: 0);
}

void
setbuf(iop, buf)
register FILE *iop;
char	*buf;
{
	register int fno = fileno(iop);  /* file number */

	if(iop->_base != NULL && iop->_flag & _IOMYBUF)
		free((char*)iop->_base);
	iop->_flag &= ~(_IOMYBUF | _IONBF | _IOLBF);
	if((iop->_base = (unsigned char*)buf) == NULL)
	{
		iop->_flag |= _IONBF;
	}
	iop->_ptr = iop->_base;
	iop->_cnt = 0;
}

char	*malloc();

_flsbuf(c, iop)
register FILE *iop;
{
	register unsigned char *base;
	register n, rn;
	char c1;

	if (iop->_flag & _IORW)
	{
		iop->_flag |= _IOWRT;
		iop->_flag &= ~(_IOEOF|_IOREAD);
	}

	if ((iop->_flag&_IOWRT)==0)
		return(EOF);
tryagain:
	if (iop->_flag&_IONBF)
	{
		c1 = c;
		rn = 1;
		n = write(fileno(iop), &c1, rn);
		iop->_cnt = 0;
	}
	else
	{
		if ((base=iop->_base)==NULL)
		{
			if (iop==stdout)
			{
				if (isatty(fileno(stdout)))
					iop->_flag |= _IOLBF;
				iop->_base = _sobuf;
				iop->_ptr = _sobuf;
				goto tryagain;
			}
			if ((iop->_base=base=(unsigned char*)malloc(BUFSIZ)) == NULL)
			{
				iop->_flag |= _IONBF;
				goto tryagain;
			}
			iop->_flag |= _IOMYBUF;
			rn = n = 0;
		}
		else if ((rn = n = iop->_ptr - base) > 0)
		{
			iop->_ptr = base;
			n = write(fileno(iop), base, n);
		}
		iop->_cnt = BUFSIZ-1;
		*base++ = c;
		iop->_ptr = base;
	}
	if (rn != n)
	{
		iop->_flag |= _IOERR;
		return(EOF);
	}
	return(c);
}

fflush(iop)
register FILE *iop;
{
	register unsigned char *base;
	register n;

	if ((iop->_flag&(_IONBF|_IOWRT))==_IOWRT
	 && (base=iop->_base)!=NULL && (n=iop->_ptr-base)>0)
	{
		iop->_ptr = base;
		iop->_cnt = (iop->_flag&(_IOLBF|_IONBF)) ? 0 : BUFSIZ;
		if (write(fileno(iop), base, n)!=n)
		{
			iop->_flag |= _IOERR;
			return(EOF);
		}
	}
	return(0);
}

/*
 * Flush buffers on exit
 */

exit(n)
{
	register FILE *iop;

	for (iop = _iob; iop < _iob+_NFILE; iop++)
		fclose(iop);
	_exit(n);
}

fclose(iop)
	register FILE *iop;
{
	register int r;

	r = EOF;
	if (iop->_flag&(_IOREAD|_IOWRT|_IORW))
	{
		r = fflush(iop);
		if (close(fileno(iop)) < 0)
			r = EOF;
		if (iop->_flag&_IOMYBUF)
			free(iop->_base);
	}
	iop->_cnt = 0;
	iop->_base = NULL;
	iop->_ptr = NULL;
	iop->_flag = 0;
	iop->_file = 0;
	return(r);
}

#ifndef INT16
/*
 * special version of fread for to save space
 * only works if count is 1
 * code active in io.c when INT16 is on
 */

fread(ptr,size,count,iop)
register char *ptr;
int size,count;
register FILE *iop;
{
	register int c;
	do
	{
		if((c=getc(iop))>=0)
			*ptr++ = c;
		else
			return(0);
	}
	while(--size);
	return(1);
}
#endif	/* INT16 */

#ifndef VSH
#ifndef ESH
int	_filbuf(iop)
register FILE *iop;
{
	unsigned char cc;

	if (iop->_flag & _IORW)
		iop->_flag |= _IOREAD;

	if ((iop->_flag&_IOREAD) == 0)
		return(EOF);
	p_flush();
	if((iop->_flag&_IONBF)||iop->_base==NULL)
	{
		/* unbuffered reads needed for pipes */
		iop->_cnt = read(fileno(iop),(char*)(&cc),1);
		if(iop->_cnt>0)
			{
				iop->_cnt--;
				return(cc);
			}
		goto skip;
	}
	iop->_cnt = read(fileno(iop), (char*)iop->_base, BUFSIZ);
	iop->_ptr = iop->_base;
skip:
	if (--iop->_cnt < 0)
	{
		if (iop->_cnt == -1)
		{
			iop->_flag |= _IOEOF;
			if (iop->_flag & _IORW)
				iop->_flag &= ~_IOREAD;
		}
		else
			iop->_flag |= _IOERR;
		iop->_cnt = 0;
		return(EOF);
	}
	return(*iop->_ptr++&0377);
}
#endif /* ESH */
#endif /* VSH */
