#ifndef lint
static char sccsid[] = "@(#)0.parts.c	4.1	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#include "def.h"

char *typename[TYPENUM]	= {"STLNVX",	"IFVX",		"DOVX",		"IOVX",	"FMTVX",
			"COMPVX",	"ASVX",		"ASGOVX",	"LOOPVX",	"WHIVX",
			"UNTVX",	"ITERVX",	"THENVX",	"STOPVX",	"RETVX",
			"DUMVX",	"GOVX",		"BRKVX",	"NXTVX",	"SWCHVX",
			"ACASVX",	"ICASVX"
	};
int hascom[TYPENUM]	= {2,		2,		2,		2,		2,
			2,		2,		2,		0,		0,
			0,		0,		2,		0,		0,
			0,		0,		0,		0,		2,
			2,		0
			};

int nonarcs[TYPENUM]  	= {FIXED+3,   	FIXED+4,	FIXED+2,	FIXED+3, 	FIXED+2,
			FIXED+2,	FIXED+2,	FIXED+2,	FIXED+1,	FIXED+1,
			FIXED+1,	FIXED+4,	FIXED+3,	FIXED,		FIXED,
			FIXED+2,	FIXED+1,	FIXED + 1,	FIXED + 1,	FIXED+3,
			FIXED+4,	FIXED+2
			};

int childper[TYPENUM]	= {0,	2,	1,	0,	0,
			0,	0,	0,	1,	1,
			1,	1,	1,	0,	0,
			1,	0,	0,	0,	1,
			2,	1
			};

int arcsper[TYPENUM]	= {1,		2,		2,	3,	0,
			-(FIXED+1),	1,	-(FIXED+1),	1,	1,
			1,		1,		2,	0,	0,
			-FIXED,		1,	1,		1,	-(FIXED+1),
			2,		1
			};

VERT *arc(v,i)
VERT v;
int i;
	{
	ASSERT(DEFINED(v),arc);
	ASSERT(0 <= i && i < ARCNUM(v), arc);
	return(&graph[v][nonarcs[NTYPE(v)] + i ]);
	}

VERT *lchild(v,i)
VERT v; int i;
	{
	ASSERT(DEFINED(v),lchild);
	ASSERT(0 <= i && i < childper[NTYPE(v)],lchild);
	return(&graph[v][nonarcs[NTYPE(v)]-i-1]);
	}

int *vxpart(v,type,j)
VERT v;
int type,j;
	{
	ASSERT((NTYPE(v) == type) && (0 <= j) && (j < nonarcs[type] - FIXED), vxpart);
	return(&graph[v][FIXED+j]);
	}

int *expres(v)
VERT v;
	{
	int ty;
	ty = NTYPE(v);
	ASSERT(ty == COMPVX || ty == ASGOVX || ty == ASVX || ty == SWCHVX || ty == ICASVX,expres);
	return(&graph[v][FIXED]);
	}

int *negpart(v)
VERT v;
	{
	ASSERT(NTYPE(v) == IFVX || NTYPE(v) == ACASVX,negpart);
	return(&graph[v][FIXED+1]);
	}

int *predic(v)
VERT v;
	{
	ASSERT(NTYPE(v) == IFVX || NTYPE(v) == ACASVX, predic);
	return(&graph[v][FIXED]);
	}

int *level(v)
VERT v;
	{
	ASSERT(NTYPE(v) == GOVX || NTYPE(v) == BRKVX || NTYPE(v) == NXTVX, level);
	return(&graph[v][FIXED]);
	}
int *stlfmt(v,n)
VERT v;
int n;
	{
	ASSERT(NTYPE(v) == STLNVX || NTYPE(v) == FMTVX,stlfmt);
	return(&graph[v][FIXED + n]);
	}

create(type,arcnum)
int type, arcnum;
	{
	int i, *temp, wds;
	if (nodenum >= maxnode)
		{
		maxnode += 100;
		temp=realloc(graph,maxnode*sizeof(*graph));
		free(graph);
		graph=temp;
		}
	wds = nonarcs[type] + arcnum;
	graph[nodenum] = galloc(sizeof(*graph) * wds);
	for (i = 0; i < wds; i++)  graph[nodenum][i] = 0;
	NTYPE(nodenum) = type;
	if (arcsper[type] < 0)
		ARCNUM(nodenum) = arcnum;
	
	return(nodenum++);
	}

