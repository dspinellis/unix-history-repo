#ifndef lint
static char sccsid[] = "@(#)3.reach.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#
/*
set REACH[v] = w if w is only node outside subtree of v which is reached from within
	subtree of v, REACH[v] = UNDEFINED otherwise
*/
#include "def.h"

/* strategy in obtaining REACH(v) for each node v:
Since only need to know whether there is exactly one exit from subtree of v,
need keep track only of 2 farthest exits from each subtree rather than all exits.
The first may be the unique exit, while the second is used when the children
of a node has the same first exit.
To obtain 2 farthest exits of v, look at 2 farthest exits of children of v and
the nodes entered by arcs from v.  Farthest exits are identified by numbering
the nodes from -2 to -(accessnum-2) starting at the bottom left corner of tree
using procedure number().  The farthest exit from the subtree of v is the one
with the least number according NUM to this numbering.  If a node w is an exit from the
subtree of v, then NUM(w) < NUM(v).  The negative numbers allow NUM(v) to be stored
in the same location as REACH(v).  REACH(w) may already be set when an arc (v,w) to a child
is searched, but the negative numbering is consistent, i.e. NUM(v) < NUM(w) in this case
as in other cases where w is not an exit from the subtree of v.
*/

struct pair {
	int smallest;
	int second;
	};


getreach()		/* obtain REACH(v) for each node v */
	{
	VERT v;
	struct pair *pr;
	for (v = 0; v < nodenum; ++v)
		REACH(v) = UNDEFINED;
	number(START);
	for (v = START; DEFINED(v); v = RSIB(v))
		{
		pr = exits(v);	/* need to free the space for pr */
		chfree(pr,sizeof(*pr));
		}
	}


exits(v)	/* set REACH(v) = w if w is only node outside subtree of v which is reached from within
			subtree of v, leave REACH(v) UNDEFINED otherwise */
VERT v;
	{
	struct pair *vpair, *chpair;
	VERT w,t;
	int i;
	vpair = challoc(sizeof(*vpair));
	vpair ->smallest = vpair ->second = UNDEFINED;
	for (i = 0; i < CHILDNUM(v); ++i)
		{
		w = LCHILD(v,i);
		if (!DEFINED(w)) continue;
		for (t = w; DEFINED(t); t = RSIB(t))
			{
			chpair = exits(t);

			/* set vpair->smallest,second to two smallest of vpair->smallest,second,
				chpair->smallest,second */
			if (inspr(chpair->smallest,vpair))
				inspr(chpair->second,vpair);
			chfree(chpair, sizeof(*chpair));
			}
		}
	for (i = 0; i < ARCNUM(v); ++i)
		{
		w = ARC(v,i);
		if (!DEFINED(w)) continue;
			inspr(w,vpair);
		}
	/* throw out nodes in subtree of  v */
	if (NUM(vpair->second) >= NUM(v))
		{
		vpair->second = UNDEFINED;
		if (NUM(vpair->smallest) >= NUM(v))
			vpair->smallest = UNDEFINED;
		}
	if (vpair->second == UNDEFINED)
		 REACH(v) = vpair->smallest;	/* vpair->smallest possibly UNDEFINED */
	else
		REACH(v) = UNDEFINED;
	return(vpair);
	}


	/* number nodes from -2 to -(accessnum+2) starting at bottom left corner of tree */
number(v)
VERT v;
	{
	int i;
	VERT w;
	static int count;
	for (i = 0; i < CHILDNUM(v); ++i)
		{
		w = LCHILD(v,i);
		if (DEFINED(w))
			number(w);
		}
	SETNUM(v,count-2);
	--count;
	if (DEFINED(RSIB(v)))
		number(RSIB(v));
	}


NUM(v)
VERT v;
	{
	if (!DEFINED(v)) return(UNDEFINED);
	return(REACH(v));
	}

SETNUM(v,count)
VERT v; int count;
	{
	/* this reuses REACH to save space;
	/* appears to be no conflict with setting true value of REACH later */
	REACH(v) = count;
	}


LOGICAL inspr(w,pr)		/* insert w in order in pr, return TRUE if <= smaller of pr */
					/* don't insert duplicates */
VERT w;
struct pair *pr;
	{
	if (w == pr-> smallest) return(TRUE);
	if (NUM(w) < NUM(pr->smallest))
		{
		pr->second = pr->smallest;
		pr->smallest = w;
		return(TRUE);
		}
	if (w == pr->second) return(FALSE);
	if (NUM(w) < NUM(pr->second))
		pr->second = w;
	return(FALSE);
	}
