#ifndef lint
static char sccsid[] = "@(#)pow.c	5.2 (Berkeley) %G%";
#endif not lint

#include <mp.h>
pow(a,b,c,d) MINT *a,*b,*c,*d;
{	int i,j,n;
	MINT x,y;
	x.len=y.len=0;
	xfree(d);
	d->len=1;
	d->val=xalloc(1,"pow");
	*d->val=1;
	for(j=0;j<b->len;j++)
	{	n=b->val[b->len-j-1];
		for(i=0;i<15;i++)
		{	mult(d,d,&x);
			mdiv(&x,c,&y,d);
			if((n=n<<1)&0100000)
			{	mult(a,d,&x);
				mdiv(&x,c,&y,d);
			}
		}
	}
	xfree(&x);
	xfree(&y);
	return;
}
rpow(a,n,b) MINT *a,*b;
{	MINT x,y;
	int i;
	x.len=1;
	x.val=xalloc(1,"rpow");
	*x.val=n;
	y.len=n*a->len+4;
	y.val=xalloc(y.len,"rpow2");
	for(i=0;i<y.len;i++) y.val[i]=0;
	y.val[y.len-1]=010000;
	xfree(b);
	pow(a,&x,&y,b);
	xfree(&x);
	xfree(&y);
	return;
}
