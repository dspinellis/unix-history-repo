#ifndef lint
static char *sccsid ="bldds.c	(CWI)	1.1	85/03/01";
#endif
/* BuiLD Data Structures */

#include "ideal.h"
#include "y.tab.h"

NOADPTR rbuildnoadtree (corrputnode)
PUTPTR corrputnode;
{
	NOADPTR nuroot, putsons, boxsons;
	dprintf "building noad tree for %s\n", idprint (corrputnode->name));
	nuroot = noadgen (
			corrputnode,
			buildvarlist (corrputnode->parm->stmtlist),
			buildvarlist ((findbox (corrputnode->parm->name,FALSE))->stmtlist)
		);
	putsons = walkputlist (corrputnode->parm->stmtlist, nuroot);
	boxsons = walkputlist ((findbox (corrputnode->parm->name,FALSE))->stmtlist, nuroot);
	if (putsons == NULL) {
		nuroot->son = boxsons;
	} else {
		NOADPTR temp;
		for (temp = putsons;
			temp->brother != NULL;
			temp = temp->brother)
			;
		temp->brother = boxsons;
		nuroot->son = putsons;
	}
	return (nuroot);
}

NOADPTR buildnoadtree (corrputnode)
PUTPTR corrputnode;
{
	NOADPTR retval;
	if (when_bug & 02) bug_on;
	retval = rbuildnoadtree (corrputnode);
	bug_off;
	return (retval);
}

VARPTR buildvarlist (stmtlist)
STMTPTR stmtlist;
{
	VARPTR curlist;
	NAMEPTR namewalk;
	curlist = NULL;
	stmtlist = nextstmt (VAR, stmtlist);
	while (stmtlist) {
		/* make room for each local variable, and
		/* make each independent (x = 1*x) */
		for (namewalk = ((NAMEPTR) stmtlist->stmt);
			namewalk;
			namewalk = namewalk->next) {
			VARPTR newre, newim;
			newre = vargen (namewalk->name, TRUE, (DEPPTR) NULL);
			newre->deplist = depgen (newre, 1.0);
			newim = vargen (namewalk->name, FALSE, (DEPPTR) NULL);
			newim->deplist = depgen (newim, 1.0);
			newre->next = newim;
			newim->next = curlist;
			curlist = newre;
		}
		stmtlist = nextstmt (VAR, stmtlist->next);
	}
	return (curlist);
}

NOADPTR walkputlist (stmtlist, parent)
STMTPTR stmtlist;
NOADPTR parent;
{
	NOADPTR headnoad, curnoad, prevnoad;
	stmtlist = nextstmt (PUT, stmtlist);
	if (!stmtlist)
		return (NULL);
	headnoad = prevnoad = rbuildnoadtree ((PUTPTR) stmtlist->stmt);
	prevnoad->father = parent;
	stmtlist = nextstmt (PUT, stmtlist->next);
	while (stmtlist) {
		curnoad = rbuildnoadtree ((PUTPTR) stmtlist->stmt);
		curnoad->father = parent;
		prevnoad->brother = curnoad;
		prevnoad = curnoad;
		stmtlist = nextstmt (PUT, stmtlist->next);
	}
	return (headnoad);
}
