/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)msqrt.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <mp.h>
msqrt(a,b,r) MINT *a,*b,*r;
{	MINT x,junk,y;
	int j;
	x.len=junk.len=y.len=0;
	if(a->len<0) fatal("msqrt: neg arg");
	if(a->len==0)
	{	b->len=0;
		r->len=0;
		return(0);
	}
	if(a->len%2==1) x.len=(1+a->len)/2;
	else x.len=1+a->len/2;
	x.val=xalloc(x.len,"msqrt");
	for(j=0;j<x.len;x.val[j++]=0);
	if(a->len%2==1) x.val[x.len-1]=0400;
	else x.val[x.len-1]=1;
	xfree(b);
	xfree(r);
loop:
	mdiv(a,&x,&y,&junk);
	xfree(&junk);
	madd(&x,&y,&y);
	sdiv(&y,2,&y,(short *)&j);
	if(mcmp(&x,&y)>0)
	{	xfree(&x);
		move(&y,&x);
		xfree(&y);
		goto loop;
	}
	xfree(&y);
	move(&x,b);
	mult(&x,&x,&x);
	msub(a,&x,r);
	xfree(&x);
	return(r->len);
}
