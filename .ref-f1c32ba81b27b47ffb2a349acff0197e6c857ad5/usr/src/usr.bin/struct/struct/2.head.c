/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)2.head.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#
/*
set head[v] to ITERVX heading smallest loop containing v, for each v
*/
#include "def.h"
#include "2.def.h"

/* define ANC(v,w) true if v == w or v is ancestor of w */
#define ANC(v,w)	(ntobef[v] <= ntobef[w] && ntoaft[v] <= ntoaft[w])	/* reflexive ancestor */


gethead(head)
VERT *head;
	{
	VERT v, w, adj; int i, j;
	/* search nodes in reverse of after numbering so that all paths from
		a node to an ancestor are searched before the node */
	/* at any point, the current value of head allows chains of nodes
		to be reached from any node v by taking head[v], head[head[v]], etc.
		until an UNDEFINED value is reached.  Upon searching each arc, 
		the appropriate chains must be merged to avoid losing information.
		For example, from one path out of a node v it may be known that
		 v is in a loop headed by z, while from another
		it may be known that v is in a loop headed by w.
		Thus, head[v] must be set to whichever of z,w is the closer ancestor,
		and the fact that this node is in a loop headed by the other must be
		recorded in head. 	*/
	for (v = 0; v < nodenum; ++v)
		head[v] = UNDEFINED;
	for (i = accessnum -1; i >= 0; --i)
		{
		v = after[i];
		for (j = 0; j < ARCNUM(v); ++j)
			{
			adj = ARC(v,j);
			if (!DEFINED(adj)) continue;
			if (ntoaft[adj] < i)		/* back edge */
				merge(v,adj,head);
			else if (ANC(v,adj))		/* not back edge or cross edge */
				{
				/* need to do only tree edges - must not do edge (v,adj)
					when head[adj] is not ANC of v */
				if (DEFINED(head[adj]) && ANC(head[adj],v))
					merge(v,head[adj],head);
				}
			else				/* cross edge */
				{
				w = lowanc(adj,v,head);
				if (DEFINED(w))
					merge(w,v,head);
				}
			}
		if (NTYPE(v) == LOOPVX || NTYPE(v) == DOVX)
			head[ARC(v,0)] = head[v];	/* head of ITERVX must be different ITERVX */
		}
	}


lowanc(y,z,head)		/* find the first node in chain of y which is anc of z, if it exists */
VERT y,z, *head;
	{
	while (y != -1 && !ANC(y,z))
		y = head[y];
	return(y);
	}


merge(w,y,head)		/* merge chains of w and y according to ANC relation */
VERT w,y, *head;
	{
	VERT t, min;
	if (w == y) return;

	if (ANC(w,y))		/* set t to min of w,y */
		{
		t = y;
		 y = head[y];
		}
	else
		{
		t = w;
		 w = head[w];
		}

	while (w != -1 && y != -1)		/* construct chain at t by adding min of remaining elts */
		{
		if (ANC(w,y))
			{
			min = y;
			y = head[y];
			}
		else
			{
			min = w;
			w = head[w];
			}
		if (t != min)
			{
			head[t] = min;
			t = min;
			}
		}
	if (w == -1)  min = y;  else  min = w;
	if (t != min)  head[t] = min;

	}
