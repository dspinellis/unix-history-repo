#ifndef lint
static char *sccsid ="opaque.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

LINEPTR opqact (opqstmt, noadtree, linelist)
STMTPTR opqstmt;
NOADPTR noadtree;
LINEPTR linelist;
{
	STMTPTR bdstmt;
	LINEPTR inlines, outlines, both;
	LINENODE nuline;
	LINEPTR prevline;

	if (when_bug & 0100) bug_on;
	prevline = &nuline;
	prevline->next = NULL;
	both = linelist;
	if ((bdstmt = nextstmt (BDLIST, noadtree->defnode->parm->stmtlist))
		|| (bdstmt = nextstmt (BDLIST,findbox (noadtree->defnode->parm->name,FALSE)->stmtlist))) {
		EDGENODE edgelist;
		EXPRPTR bdwalk, lastbd;
		INTLPTR prevtx, curvtx, postvtx;
		EDGEPTR edgewalk;
		EDGEPTR forfreeing;
		lastbd = (EXPRPTR) tail ((BOXPTR) bdstmt->stmt);
		lastbd->next = exprgen (((EXPRPTR) bdstmt->stmt)->expr);
		edgewalk = &edgelist;
		prevtx = expreval (((EXPRPTR) bdstmt->stmt)->expr, noadtree);
		for (bdwalk = ((EXPRPTR) bdstmt->stmt)->next;
			bdwalk;
			bdwalk = bdwalk->next) {
			curvtx = expreval (bdwalk->expr, noadtree);
			if (((INTLPTR) bdwalk->expr)->oper == '^') {
				bdwalk = bdwalk->next;
				if (!bdwalk) {
					fprintf (stderr, "ideal: arc point may not begin boundary specification\n");
					return (linelist);
				}
				postvtx = expreval (bdwalk->expr, noadtree);
				edgewalk->next = edgearc (
					Re(prevtx),
					Im(prevtx),
					Re(curvtx),
					Im(curvtx),
					Re(postvtx),
					Im(postvtx)
				);
			} else {
				postvtx = curvtx;
				edgewalk->next = edgeline (
					Re(prevtx),
					Im(prevtx),
					Re(postvtx),
					Im(postvtx)
				);
			}
			prevtx = postvtx;
			edgewalk = edgewalk->next;
		}
		edgewalk->next = edgelist.next;
		lastbd->next = NULL;
		opqpoly (
			edgelist.next->next,
			linelist,
			&inlines,
			&outlines,
			&both
		);
		forfreeing = edgelist.next->next;
		edgelist.next->next = NULL;
		linefree (forfreeing);
/*
	} else if (noadtree->defnode->parm->name == lookup ("circle")
		|| noadtree->defnode->parm->name == lookup ("hole")) {
		z0 = varfind (lookup ("center"), noadtree);
		r = varfind (lookup ("radius"), noadtree);
		if (!known (z0) || !known (r)) {
			fprintf (stderr, "ideal: indeterminate opaque circle\n");
			return (linelist);
		}
		opqcirc (
			Re(z0), Im(z0), Re(r),
			linelist,
			&inlines,
			&outlines,
			&both
		);
		intlfree (z0);
		intlfree (r);
	} else if (noadtree->defnode->parm->name == lookup ("sector")) {
		z0 = varfind (lookup ("center"), noadtree);
		r = varfind (lookup ("radius"), noadtree);
		z1 = varfind (lookup("start"), noadtree);
		z2 = varfind (lookup ("end"), noadtree);
		t1 = varfind (lookup ("startang"), noadtree);
		t2 = varfind (lookup ("endang"), noadtree);
		if (!known(z0) || !known(r) || !known(z1) || !known(z2)) {
			fprintf (stderr, "ideal: intederminate opaque sector\n");
				return (linelist);
		}
		opqsect (
			Re(z0), Im(z0), Re(r),
			Re(z1), Im(z1),
			Re(z2), Im(z2),
			Re(t1), Re(t2),
			linelist,
			&inlines,
			&outlines,
			&both
		);
		intlfree (z0);
		intlfree (r);
		intlfree (z1);
		intlfree (z2);
		intlfree (t1);
		intlfree (t2);
	} else if (noadtree->defnode->parm->name == lookup ("segment")) {
		z0 = varfind (lookup ("center"), noadtree);
		r = varfind (lookup ("radius"), noadtree);
		z1 = varfind (lookup ("start"), noadtree);
		z2 = varfind (lookup ("end"), noadtree);
		if (!known(z0) || !known(r) || !known(z1) || !known(z2)) {
			fprintf (stderr, "ideal: indeterminate opaque segment\n");
			return (linelist);
		}
		opqseg (
			Re(z0), Im(z0), Re(r),
			Re(z1), Im(z1),
			Re(z2), Im(z2),
			linelist,
			&inlines,
			&outlines,
			&both
		);
		intlfree (z0);
		intlfree (r);
		intlfree (z1);
		intlfree (z2);
*/
	} else {
		fprintf(stderr, "ideal: no boundary list\n");
	}
	if (((MISCPTR) opqstmt->stmt)->info == INTERIOR) {
		prevline->next = outlines;
		linefree (inlines);
	} else {
		prevline->next = inlines;
		linefree (outlines);
	}
	if (both) {
		while (prevline->next)
			prevline = prevline->next;
		prevline->next = both;
	}
	linelist = lineclean (nuline.next);
	bug_off;
	return (linelist);
} /* opqact */

void opqinsert (code, alpha, opqlist)
int code;
float alpha;
OPQPTR *opqlist;
{
	OPQNODE head;
	OPQPTR walk, prev, new;
	walk = &head;
	walk->alpha = -INFINITY;
	walk->next = *opqlist;
	prev = NULL;
	while (walk->next && walk->next->alpha < alpha + EPSILON) {
		prev = walk;	
		walk = walk->next;
	}
	if (walk->alpha < alpha - EPSILON) {
		new = opqgen(code, alpha);
		new->next = walk->next;
		walk->next = new;
	} else {
		if (walk->code == EXT0 || walk->code == INFL0)
			if (code == EXT1 || code == INFL1) {
				walk->code = IGNORE;
			}
		else if (walk->code == EXT1 || walk->code == INFL1)
			if (code == EXT0 || code == INFL0) {
				walk->code = IGNORE;
			}
		else if (walk->code == SIMPLE && code != INHERIT)
			walk->code = code;
	}
	*opqlist = head.next;
}

LINEPTR lineclean (linelist)
LINEPTR linelist;
{
	/* clean short lines from linelist */
	LINEPTR prevline, linewalk;
	LINENODE nuhead;
	prevline = &nuhead;
	prevline->next = linewalk = linelist;
	while (linewalk) {
		if ((linewalk->kind == LINE)
			&& (fabs(linewalk->x0 - linewalk->x1) < EPSILON)
			&& (fabs(linewalk->y0 - linewalk->y1) < EPSILON)) {
			dprintf "Removing chopped line\n");
			prevline->next = linewalk->next;
			tryfree(linewalk);
			linewalk = prevline->next;
		} else {
			prevline = linewalk;
			linewalk = linewalk->next;
		}
	}
	linelist = nuhead.next;
	return (linelist);
}

/*
/*void tangent (x, y, dx, dy, x1, y1, x2, y2)
/*float x, y, dx, dy;
/*float *x1, *y1, *x2, *y2;
/*{
/*	*x1 = x + dx;
/*	*x2 = x - dx;
/*	*y1 = y + dy;
/*	*y2 = y - dy;
/*}
*/

/*
/*void halfplane (x1,y1, x2,y2, linelist, inlines, outlines, both)
/*float x1, y1, x2, y2;
/*LINEPTR linelist;
/*LINEPTR *inlines, *outlines, *both;
/*{
/*	LINEPTR edges;
/*	float perpx, perpy;
/*	float ulx, uly, urx, ury, llx, lly, lrx, lry;
/*	dprintf "halfplane (%f,%f) (%f,%f)\n", x1, y1, x2, y2);
/*	perpx = 10.0*(y1 - y2)/hypot ((x2 - x1), (y2 - y1));
/*	perpy = 10.0*(x2 - x1)/hypot ((x2 - x1), (y2 - y1));
/*	lrx = x1 - 10.0*(x2 - x1);
/*	lry = y1 - 10.0*(y2 - y1);
/*	urx = x1 + 10.0*(x2 - x1);
/*	ury = y1 + 10.0*(y2 - y1);
/*	ulx = urx + perpx;
/*	uly = ury + perpy;
/*	llx = lrx + perpx;
/*	lly = lry + perpy;
/*	dprintf "perpx %f perpy %f\n", perpx, perpy);
/*	if (dbg)
/*		fprintf (stderr, "(%f,%f)\n (%f,%f)\n (%f,%f)\n (%f,%f)\n",
/*			lrx, lry,
/*			urx, ury,
/*			ulx, uly,
/*			llx, lly
/*		);
/*	edges = linegen (lrx, lry, urx, ury);
/*	edges->next = linegen (urx, ury, ulx, uly);
/*	edges->next->next = linegen (ulx, uly, llx, lly);
/*	edges->next->next->next = linegen (llx, lly, lrx, lry);
/*	edges->next->next->next->next = edges;
/*	opqpoly (
/*		edges,
/*		linelist,
/*		inlines,
/*		outlines,
/*		both
/*	);
/*	edges->next->next->next->next = NULL;
/*	linefree (edges);
/*}
*/

/*
/*void triangle (x1,y1, x2,y2, x3,y3, linelist, inlines, outlines, both)
/*float x1, y1, x2, y2, x3, y3;
/*LINEPTR linelist;
/*LINEPTR *inlines, *outlines, *both;
/*{
/*	LINEPTR edges;
/*	edges = linegen (x1, y1, x2, y2);
/*	edges->next = linegen (x2, y2, x3, y3);
/*	edges->next->next = linegen (x3, y3, x1, y1);
/*	edges->next->next->next = edges;
/*	opqpoly (
/*		edges,
/*		linelist,
/*		inlines,
/*		outlines,
/*		both
/*	);
/*	edges->next->next->next = NULL;
/*	linefree (edges);
/*}
*/
