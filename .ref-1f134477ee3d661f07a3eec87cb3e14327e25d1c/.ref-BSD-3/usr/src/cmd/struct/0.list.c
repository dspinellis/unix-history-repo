#include <stdio.h>
#include "def.h"

struct list *consls(v,ls)		/* make list */
VERT v;
struct list *ls;
	{
	struct list *temp;
	temp = challoc(sizeof(*temp));
	temp->elt = v;
	temp->nxtlist = ls;
	return(temp);
	}

struct list *append(v,ls)		/* return ls . v */
VERT v;
struct list *ls;
	{
	struct list *temp;
	if (!ls) return(consls(v,0));
	for (temp = ls; temp -> nxtlist; temp = temp->nxtlist)
		;
	temp->nxtlist = consls(v,0);
	return(ls);
	}


freelst(ls)
struct list *ls;
	{
	if (!ls) return;
	if (ls->nxtlist)
		freelst(ls->nxtlist);
	chfree(ls,sizeof(*ls));
	}


oneelt(ls)		/* return w if w is only elt of ls, UNDEFINED otherwise */
struct list *ls;
	{
	if (!ls) return(UNDEFINED);
	if (ls->nxtlist) return(UNDEFINED);
	return(ls->elt);
	}


lslen(ls)		/* return number of elements in list ls */
struct list *ls;
	{
	int count;
	struct list *lp;
	count = 0;
	for (lp = ls; lp; lp = lp->nxtlist)
		++count;
	return(count);
	}


prlst(ls)
struct list *ls;
	{
	struct list *lp;
	for (lp = ls; lp; lp = lp->nxtlist)
		printf("%d,",lp->elt);
	fprintf(stderr,"\n");
	}
