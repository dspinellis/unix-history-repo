/*
char id_douio[] = "@(#)douio.c	1.1";
 *
 * unformatted external i/o
 */

#include "fio.h"

char *eor = "eor/uio";
char *uio = "uio";

do_us(number,ptr,len) ftnint *number; ftnlen len; char *ptr;  /* sequential */
{
	if(reading)
	{
		recpos += *number * len;
		if (recpos > reclen)
			err(errflag,110,eor);

		if (fread(ptr,(int)len,(int)(*number),cf) != *number)
			return(due_err(uio));
	}
	else
	{
		reclen += *number * len;
		fwrite(ptr,(int)len,(int)(*number),cf);
	}
	return(OK);
}

do_uio(number,ptr,len) ftnint *number; ftnlen len; char *ptr;
{
	if(sequential)
		return(do_us(number,ptr,len));
	else
		return(do_ud(number,ptr,len));
}

do_ud(number,ptr,len) ftnint *number; ftnlen len; char *ptr;  /* direct */
{
	recpos += *number * len;
	if(recpos > curunit->url && curunit->url!=1)
		err(errflag,110,eor);
	if(reading)
	{
		if (fread(ptr, (int)len, (int)(*number), cf) != *number)
			return(due_err(uio));
	}
	else
		fwrite(ptr,(int)len,(int)(*number),cf);
	return(OK);
}

due_err(s) char *s;
{
	if(feof(cf))
		err(endflag,EOF,s)
	else
	{	clearerr(cf);
		err(errflag,errno,s)
	}
}
