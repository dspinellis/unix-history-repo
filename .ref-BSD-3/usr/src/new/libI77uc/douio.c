/*
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
		if(recpos>reclen)
		{
			err(errflag,110,eor);
		}
		if (fread(ptr,(int)len,(int)(*number),cf) != *number)
		{	if(feof(cf)) err(endflag,EOF,uio)
			else
			{	clearerr(cf);
				err(errflag,errno,uio)
			}
		}
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
	else	return(do_ud(number,ptr,len));
}

do_ud(number,ptr,len) ftnint *number; ftnlen len; char *ptr;  /* direct */
{
	recpos += *number * len;
	if(recpos > curunit->url && curunit->url!=1)
		err(errflag,110,eor);
	if(reading)
	{
		if(fread(ptr,(int)len,(int)(*number),cf) != *number)
		{	if(feof(cf)) err(endflag,EOF,uio)
			else
			{	clearerr(cf);
				err(errflag,errno,uio)
			}
		}
	}
	else fwrite(ptr,(int)len,(int)(*number),cf);
	return(OK);
}
