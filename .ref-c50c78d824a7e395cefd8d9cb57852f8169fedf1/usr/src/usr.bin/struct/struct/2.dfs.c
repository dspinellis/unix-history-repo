#ifndef lint
static char sccsid[] = "@(#)2.dfs.c	4.1	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#
/* depth-first search used to identify back edges, unreachable nodes;
	each node v entered by back edge replaced by
	LOOPVX ->ITERVX -> v,
	so that back edges entering v now enter the ITERVX,
	and other edges entering v now enter the LOOPVX.
	Nodes are numbered according to depth-first search:
		before numbering- ntobef[v] = i => node numbered v is i'th
			node in order of first visit during the search;
		after numbering- ntoaft[v] = i => node numbered v is i'th
			node visited in order of last visit during the search.
			Also, in this case after[i] = v.
*/

#include "def.h"
#include "2.def.h"

#define MAXINS 3	/* spacing needed between numbers generated during depth first search */

int *status;
int befcount, aftcount;
/* following defines used to mark back edges and nodes entered by back edges */
#define UNPROCESSED	0
#define STACKED	1
#define FINISHED	2
#define MARK(v)	{REACH(v) = 1; }	/* mark node v */
#define UNMARK(v)	{REACH(v) = 0; }
#define MARKED(v)	(REACH(v))
#define MKEDGE(e)	{if (e >= -1) e = -(e+3); }	/* mark edge e */
#define UNMKEDGE(e)	{if (e < -1) e = -(e+3); }
#define BACKEDGE(e)	(e < -1)


dfs(v)		/* depth first search */
VERT v;
	{
	int i; VERT w;
	accessnum = 0;
	status = challoc(sizeof(*status) * nodenum);
	for (w = 0; w < nodenum; ++w)
		{
		status[w] = UNPROCESSED;
		UNMARK(w);
		}
	search(v);
	chreach();
	chfree(status, sizeof(*status) * nodenum);
	addloop();
	after = challoc(sizeof(*after) * accessnum);
	for (i = 0; i < accessnum; ++i)
		after[i] = UNDEFINED;
	ntoaft = challoc(sizeof(*ntoaft) * nodenum);
	ntobef = challoc(sizeof(*ntobef) * nodenum);
	for (w = 0; w < nodenum; ++w)
		ntobef[w] = ntoaft[w] = UNDEFINED;
	befcount = 0;
	aftcount = 0;
	repsearch(v);
	}


search(v)
	/* using depth first search, mark back edges using MKEDGE, and nodes entered by back
	edges using MARK */
VERT v;
	{
	VERT adj; int i;
	status[v] = STACKED;
	for(i = 0; i < ARCNUM(v); ++i)
		{
		adj = ARC(v,i);
		if (!DEFINED(adj)) continue;
		else if (status[adj] == UNPROCESSED)
			search(adj);
		else if (status[adj] == STACKED)
			{
			MARK(adj);		/* mark adj as entered by back edge */
			MKEDGE(ARC(v,i));	/* mark edge ARC(v,i) as being back edge */
			}
		}
	status[v] = FINISHED;
	++accessnum;
	}

chreach()		/* look for unreachable nodes */
	{
	VERT v;
	LOGICAL unreach;
	unreach = FALSE;
	for (v = 0; v < nodenum; ++v)
		if (status[v] == UNPROCESSED && NTYPE(v) != FMTVX
			&& NTYPE(v) != STOPVX && NTYPE(v) != RETVX)
			{
			unreach = TRUE;
			if (debug)
				fprintf(stderr,"node %d unreachable\n",v);
			}
	if (unreach)
		error(": unreachable statements - ","will be ignored","");
	}


addloop()	/* add LOOPVX, ITERVX at nodes entered by back edges, and adjust edges */
	{
	VERT v, adj;
	int j, oldnum;
	for (v = 0, oldnum = nodenum; v < oldnum; ++v)	/* insloop increases nodenum */
		if (MARKED(v))
			{
			UNMARK(v);	/* remove mark indicating v entered by back edge */
			if (NTYPE(v) != ITERVX)			/* DO loops already have ITERVX */
				 insloop(v);  /* add LOOPVX, ITERVX since v entered by back edge*/
			}
	/* arcs which used to enter v now enter LOOPVX; must make back edges enter ITERVX */
	for (v = 0; v < nodenum; ++v)
		for (j = 0; j < ARCNUM(v); ++j)
			{
			if (BACKEDGE(ARC(v,j)))
				{
				UNMKEDGE(ARC(v,j));		/* return edge to normal value */
				adj = ARC(v,j);
				if (NTYPE(adj) == ITERVX) continue;
				ASSERT(NTYPE(adj) == LOOPVX,addloop);
				ARC(v,j) = ARC(adj,0);	/* change arc to point to ITERVX */
				ASSERT(NTYPE(ARC(v,j)) == ITERVX,addloop);
				}
			}
	}

insloop(v)		/* insert LOOPVX, ITERVX at node number v */
VERT v;
	{
	VERT loo, iter;
	loo = create(LOOPVX, 1);
	iter = create(ITERVX,1);
	accessnum += 2;
	/* want LOOPVX to take on node number v, so that arcs other than back arcs
		entering v will enter the LOOPVX automatically */
	exchange(&graph[v], &graph[loo]);
	exchange(&v, &loo);
	ARC(loo,0) = iter;
	ARC(iter,0) = v;
	FATH(iter) = UNDEFINED;	/* will be defined later along with FATH for DOVX */
	}

exchange(p1,p2)		/* exchange values of p1,p2 */
int *p1,*p2;
	{
	int temp;
	temp = *p1;
	*p1 = *p2;
	*p2 = temp;
	}


repsearch(v)		/* repeat df search in order to fill in after, ntoaft, ntobef tables */
VERT v;
	{
	VERT adj; int i,temp;
	ntobef[v] = befcount;
	++befcount;
	for(i = 0; i < ARCNUM(v); ++i)
		{
		adj = ARC(v,i);
		if (DEFINED(adj) && ntobef[adj] == UNDEFINED)
			repsearch(adj);
		}
	++aftcount;
	temp = accessnum - aftcount;
	after[temp] = v;
	ntoaft[v] = temp;
	}
