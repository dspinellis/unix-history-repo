#ifndef lint
static char sccsid[] = "@(#)0.alloc.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"
int routbeg;

extern int debug;
struct coreblk	{struct coreblk *nxtblk;
			int blksize;
			int nxtfree;
			int *blk;
			};

long space;
challoc(n)
int n;
	{
	int i;
	i = malloc(n);
	if(i) { space += n; return(i); }
	fprintf(stderr,"alloc out of space\n");
	fprintf(stderr,"total space alloc'ed = %D\n",space);
	fprintf(stderr,"%d more bytes requested\n",n);
	exit(1);
	}


chfree(p,n)
int *p,n;
	{
	ASSERT(p,chfree);
	space -= n;
	free(p);
	}


struct coreblk *tcore, *gcore;
int tblksize=12, gblksize=300;


balloc(n,p,size)		/* allocate n bytes from coreblk *p */
int n,size;		/* use specifies where called */
struct coreblk **p;
	{
	int i;
	struct coreblk *q;
	n = (n+sizeof(i)-1)/sizeof(i);	/* convert bytes to wds to ensure ints always at wd boundaries */
	for (q = *p; ; q = q->nxtblk)
		{
		if (!q)
			{
			q = morespace(n,p,size);
			break;
			}
		if (q-> blksize - q->nxtfree >= n)  break;
		}
	i = q->nxtfree;
	q ->nxtfree += n;
	return( &(q->blk)[i]);
	}

talloc(n)		/* allocate from line-by-line storage area */
int n;
	{return(balloc(n,&tcore,tblksize)); }

galloc(n)		/* allocate from graph storage area */
int n;
	{
	return(balloc(n,&gcore,gblksize));
	}

reuse(p)		/* set nxtfree so coreblk can be reused */
struct coreblk *p;
	{
	for (; p; p=p->nxtblk)  p->nxtfree = 0;  
	}

bfree(p)		/* free coreblk p */
struct coreblk *p;
	{
	if (!p) return;
	bfree(p->nxtblk);
	p->nxtblk = 0;
	free(p);
	}


morespace(n,p,size)		/* get at least n more wds for coreblk *p */
int n,size;
struct coreblk **p;
	{struct coreblk *q;
	int t,i;

	t = n<size?size:n;
	q = malloc(i=t*sizeof(*(q->blk))+sizeof(*q));
	if(!q){
		error(": alloc out of space","","");
		fprintf(stderr,"space = %D\n",space);
		fprintf(stderr,"%d more bytes requested\n",n);
		exit(1);
		}
	space += i;
	q->nxtblk = *p;
	*p = q;
	q -> blksize = t;
	q-> nxtfree = 0;
	q->blk = q + 1;
	return(q);
	}




freegraf()
	{
	bfree(gcore);
	gcore = 0;


	}









error(mess1, mess2, mess3)
char *mess1, *mess2, *mess3;
	{
	static lastbeg;
	if (lastbeg != routbeg)
		{
		fprintf(stderr,"routine beginning on line %d:\n",routbeg);
		lastbeg = routbeg;
		}
	fprintf(stderr,"error %s %s %s\n",mess1, mess2, mess3);
	}


faterr(mess1, mess2, mess3)
char *mess1, *mess2, *mess3;
	{
	error(mess1, mess2, mess3);
	exit(1);
	}


strerr(mess1, mess2, mess3)
char *mess1, *mess2, *mess3;
	{
	error("struct error: ",mess1, mess2);
	}
