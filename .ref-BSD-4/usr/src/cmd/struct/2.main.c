#include <stdio.h>
#include "def.h"
#include "2.def.h"

VERT *after;
int *ntobef, *ntoaft;
build()
	{
	VERT v, *dom, *head;
	int type;
	struct list **inarc;
	dfs(START);
	if (routerr) return;
	for (v = 0; v < nodenum; ++v)
		{
		type = NTYPE(v);
		if (type == LOOPVX || type == DOVX)
			FATH(ARC(v,0)) = v;
		}

	head = challoc(sizeof(*head) * nodenum);
	if (progress) fprintf(stderr,"	gethead:\n");
	gethead(head);	/* sets head[v] to ITERVX heading smallest loop containing v or UNDEFINED */

	if (routerr) return;
	inarc = challoc(nodenum * sizeof(*inarc));
	if (progress) fprintf(stderr,"	getinarc:\n");
	getinarc(inarc,head);		/* sets inarc[v] to list of forward arcs entering v */

	dom = challoc(nodenum * sizeof(*dom));
	if (progress) fprintf(stderr,"	getdom:\n");
	getdom(inarc,dom);	/* sets dom[v] to immediate dominator of v or UNDEFINED */
	if (routerr) return;
	if (progress) fprintf(stderr,"	gettree:\n");
	gettree(inarc, dom, head);
	if (routerr) return;

	chfree(head, nodenum * sizeof(*head)); head = 0;
	chfree(dom,nodenum * sizeof(*dom)); dom = 0;
	for (v = 0; v < nodenum; ++v)
		{
		freelst(inarc[v]);
		inarc[v] = 0;
		}
	chfree(inarc,sizeof(*inarc) * nodenum); inarc = 0;
	chfree(ntoaft,sizeof(*ntoaft) * nodenum); ntoaft = 0;
	chfree(ntobef,sizeof(*ntobef) * nodenum); ntobef = 0;
	chfree(after, sizeof(*after) * accessnum); after = 0;
	}
