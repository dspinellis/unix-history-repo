/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)douio.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * unformatted external i/o
 */

#include "fio.h"

LOCAL char *eor = "eor/uio";
LOCAL char *uio = "uio";

LOCAL
do_us(number,ptr,len) ftnint *number; ftnlen len; char *ptr;  /* sequential */
{
	if(reading)
	{
		recpos += *number * len;
		if (recpos > reclen) {
			recpos -= *number * len;
			e_rsue(); /* in case tries another read */
			err(errflag,F_EREREC,eor);
		}

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

LOCAL
do_ud(number,ptr,len) ftnint *number; ftnlen len; char *ptr;  /* direct */
{
	recpos += *number * len;
	if(recpos > curunit->url && curunit->url!=1)
		err(errflag,F_EREREC,eor);
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
