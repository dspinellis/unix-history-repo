#include "fio.h"
int reclen;
do_us(number,ptr,len) ftnint *number; ftnlen len; char *ptr;
{
	if(reading)
	{
		recpos += *number * len;
		if(recpos>reclen)
		{
			err(elist->cierr,110,"eof/uio");
		}
		fread(ptr,(int)len,(int)(*number),cf);
		return(0);
	}
	else
	{
		reclen += *number * len;
		fwrite(ptr,(int)len,(int)(*number),cf);
		return(0);
	}
}
do_uio(number,ptr,len) ftnint *number; ftnlen len; char *ptr;
{
	if(sequential)
		return(do_us(number,ptr,len));
	else	return(do_ud(number,ptr,len));
}
do_ud(number,ptr,len) ftnint *number; ftnlen len; char *ptr;
{
	recpos += *number * len;
	if(recpos > curunit->url && curunit->url!=1)
		err(elist->cierr,110,"eof/uio");
	if(reading)
	{
		if(fread(ptr,(int)len,(int)(*number),cf)
			!= *number)
			err(elist->cierr,errno,"eof/uio")
		else return(0);
	}
	fwrite(ptr,(int)len,(int)(*number),cf);
	return(0);
}
