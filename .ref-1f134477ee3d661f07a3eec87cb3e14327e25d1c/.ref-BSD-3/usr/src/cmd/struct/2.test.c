#include <stdio.h>
#
/* for testing only */
#include "def.h"
#include "2.def.h"

testaft()
	{
	int i;
	for (i = 0; i < nodenum; ++i)
		fprintf(stderr,"ntoaft[%d] = %d, ntobef[%d] = %d\n",i,ntoaft[i],i,ntobef[i]);
	fprintf(stderr,"\n");
	for (i = 0; i < accessnum; ++i)
		fprintf(stderr,"after[%d] = %d\n",i,after[i]);
	}

testhead(head)
VERT *head;
	{
	VERT v;
	for (v = 0; v < nodenum; ++v)
		fprintf(stderr,"head[%d] = %d\n",v,head[v]);
	}

testdom(dom)
VERT *dom;
	{
	VERT v;
	for (v = 0; v < nodenum; ++v)
		fprintf(stderr,"dom[%d] = %d\n",v,dom[v]);
	}


testtree()
	{
	VERT v;
	int i;
	for (v = 0; v < nodenum; ++v)
		{
		fprintf(stderr,"%d: RSIB %d, ",v,RSIB(v));
		for (i = 0; i < CHILDNUM(v); ++i)
			fprintf(stderr," %d",LCHILD(v,i));
		fprintf(stderr,"\n");
		}
	}
