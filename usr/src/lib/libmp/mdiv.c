/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mdiv.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <mp.h>
mdiv(a,b,q,r) MINT *a,*b,*q,*r;
{	MINT x,y;
	int sign;
	sign=1;
	x.val=a->val;
	y.val=b->val;
	x.len=a->len;
	if(x.len<0) {sign= -1; x.len= -x.len;}
	y.len=b->len;
	if(y.len<0) {sign= -sign; y.len= -y.len;}
	xfree(q);
	xfree(r);
	m_div(&x,&y,q,r);
	if(sign==-1)
	{	q->len= -q->len;
		r->len = - r->len;
	}
	return;
}
m_dsb(q,n,a,b) short *a,*b;
{	long int x,qx;
	int borrow,j;
	short u;
	qx=q;
	borrow=0;
	for(j=0;j<n;j++)
	{	x=borrow-a[j]*qx+b[j];
		b[j]=x&077777;
		borrow=x>>15;
	}
	x=borrow+b[j];
	b[j]=x&077777;
	if(x>>15 ==0) { return(0);}
	borrow=0;
	for(j=0;j<n;j++)
	{	u=a[j]+b[j]+borrow;
		if(u<0) borrow=1;
		else borrow=0;
		b[j]=u&077777;
	}
	{ return(1);}
}
m_trq(v1,v2,u1,u2,u3)
{	long int d;
	long int x1;
	if(u1==v1) d=077777;
	else d=(u1*0100000L+u2)/v1;
	while(1)
	{	x1=u1*0100000L+u2-v1*d;
		x1=x1*0100000L+u3-v2*d;
		if(x1<0) d=d-1;
		else {return(d);}
	}
}
m_div(a,b,q,r) MINT *a,*b,*q,*r;
{	MINT u,v,x,w;
	short d,*qval;
	int qq,n,v1,v2,j;
	u.len=v.len=x.len=w.len=0;
	if(b->len==0) { fatal("mdiv divide by zero"); return;}
	if(b->len==1)
	{	r->val=xalloc(1,"m_div1");
		sdiv(a,b->val[0],q,r->val);
		if(r->val[0]==0)
		{	shfree(r->val);
			r->len=0;
		}
		else r->len=1;
		return;
	}
	if(a->len < b->len)
	{	q->len=0;
		r->len=a->len;
		r->val=xalloc(r->len,"m_div2");
		for(qq=0;qq<r->len;qq++) r->val[qq]=a->val[qq];
		return;
	}
	x.len=1;
	x.val = &d;
	n=b->len;
	d=0100000L/(b->val[n-1]+1L);
	mult(a,&x,&u); /*subtle: relies on fact that mult allocates extra space */
	mult(b,&x,&v);
	v1=v.val[n-1];
	v2=v.val[n-2];
	qval=xalloc(a->len-n+1,"m_div3");
	for(j=a->len-n;j>=0;j--)
	{	qq=m_trq(v1,v2,u.val[j+n],u.val[j+n-1],u.val[j+n-2]);
		if(m_dsb(qq,n,v.val,&(u.val[j]))) qq -= 1;
		qval[j]=qq;
	}
	x.len=n;
	x.val=u.val;
	mcan(&x);
	sdiv(&x,d,&w,(short *)&qq);
	r->len=w.len;
	r->val=w.val;
	q->val=qval;
	qq=a->len-n+1;
	if(qq>0 && qval[qq-1]==0) qq -= 1;
	q->len=qq;
	if(qq==0) shfree(qval);
	if(x.len!=0) xfree(&u);
	xfree(&v);
	return;
}
