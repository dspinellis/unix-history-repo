/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)util.c	5.1	6/7/85
 */

/*
 * utility routines
 */

#include "fio.h"

extern short	ccntrl_, blzero_;

nowreading(x) unit *x;
{
	return(now_acc(x,"r"));
}

nowwriting(x) unit *x;
{
	return(now_acc(x,"a"));
}

LOCAL now_acc(x,mode)
unit *x; char *mode;
{
	long loc;

	if (!x->ufnm)
	{
		errno = EBADF;
		return(NO);
	}
	if (x->useek)
		loc=ftell(x->ufd);
	if (freopen(x->ufnm,mode,x->ufd))
	{
		if (x->useek)
			fseek(x->ufd,loc,0);
		x->uwrt = (*mode=='a');
		return(YES);
	}
	if (x->ufd = fopen(x->ufnm, (*mode=='a')? "r":"a"))
		if (x->useek)
			fseek(x->ufd,loc,0);
	return(NO);
}

g_char(a,alen,b) char *a,*b; ftnlen alen;
{	char *x=a+alen-1, *y=b+alen-1;
	while (x >= a  &&  *x == ' ') {x--; y--;}
	*(y+1) = '\0';
	while (x >= a) *y-- = *x--;
}

b_char(from, to, tolen) char *from, *to; ftnlen tolen;
{	int i=0;
	while (*from && i < tolen) {
		*to++ = *from++;
		i++;
	}
	while (i++ < tolen)
		*to++ = ' ';
}

char
last_char(f) FILE *f;
{
	fseek(f,-2L,1);
	if(ftell(f)) return(getc(f));
	else return('\n');
}
