/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)2.inarc.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#
/* find forward in-arcs for each node, pretending that arcs which jump into a loop 
	jump to the head of the largest such loop instead, based on the
	depth first search tree */
#include "def.h"
#include "2.def.h"

getinarc(inarc,head)		/* construct array "inarc" containing in arcs for each node */
struct list **inarc;
VERT *head;
	{
	VERT v,adj,x;
	int i, j;

	for (v=0; v < nodenum; ++v) inarc[v] = 0;

	/* fill in inarc nodes */

	for (i = 0; i < accessnum; ++i)
		{
		v = after[i];
		for (j = 0; j < ARCNUM(v); ++j)
			{
			adj = ARC(v,j);
			if (!DEFINED(adj))
				continue;
			if (ntoaft[adj] > ntoaft[v])		/* not a back edge */
				/* if edge jumps into loop, pretend jumps to head of
					largest loop jumped into */
				{
				x = maxentry(v,adj,head);
				if (!DEFINED(x)) x = adj;
				else x = FATH(x);

				inarc[x] = consls(v,inarc[x]);	/* insert v in list inarc[x] */
				}
			}
		}
	}



maxentry(x,y,head)	/* return z if z is ITERVX of largest loop containing y but not x, UNDEFINED otherwise */
VERT x,y, *head;
	{
	if (head[y] == UNDEFINED)  return(UNDEFINED);
	if (loomem(x,head[y], head)) return (UNDEFINED);
	y = head[y];
	while (head[y] != UNDEFINED)
		{
		if (loomem(x,head[y],head))  return(y);
		y = head[y];
		}
	return(y);
	}



loomem(x,y,head)			/* return TRUE if x is in loop headed by y, FALSE otherwise */
VERT x,y, *head;
	{
	VERT w;
	if (!DEFINED(y)) return(TRUE);
	ASSERT(NTYPE(y) == ITERVX, loomem);
	for (w = (NTYPE(x) == ITERVX) ? x : head[x]; DEFINED(w); w = head[w])
		if (w == y)  return (TRUE);
	return(FALSE);
	}
