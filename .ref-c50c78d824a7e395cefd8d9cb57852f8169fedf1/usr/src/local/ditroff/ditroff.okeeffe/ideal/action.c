#ifndef lint
static char *sccsid ="action.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

LINEPTR rbuild (noadtree, linelist)
NOADPTR noadtree;
LINEPTR linelist;
{
	STMTPTR slist[2];
	STMTPTR curstmt, opqstmt;
	NOADPTR nextnoad;
	int i;
	slist[0] = noadtree->defnode->parm->stmtlist;
	slist[1] = findbox (noadtree->defnode->parm->name,FALSE)->stmtlist;
	nextnoad = noadtree->son;
	if ((opqstmt = nextstmt (OPAQUE, slist[0])) || (opqstmt = nextstmt (OPAQUE, slist[1])))
		linelist = opqact (opqstmt, noadtree, linelist);
	if (noadtree->defnode->parm->name == lookup ("circle"))
		linelist = circact (noadtree, linelist);
	else if (noadtree->defnode->parm->name == lookup ("arc"))
		linelist = arcact (noadtree, linelist);
	for (i = 0; i < 2; i ++)
	for (curstmt = slist[i]; curstmt; curstmt = curstmt->next) {
	switch (curstmt->kind) {
		case '=':
			break;
		case CONN:
			linelist = connact (
				(EXPRPTR) curstmt->stmt,
				noadtree,
				linelist
			);
			break;
		case USING:
			linelist = penact (
				(PENPTR) curstmt->stmt,
				noadtree,
				linelist
			);
			break;
		case PUT:
			if (((PUTPTR)curstmt->stmt)->p_or_c == PUT)
				linelist = rbuild (nextnoad, linelist);
			else if (((PUTPTR)curstmt->stmt)->p_or_c == CONSTRUCT)
				nextnoad->linelist = rbuild (nextnoad, nextnoad->linelist);
			else impossible ("rbuild (PUT)");
			nextnoad = nextnoad->brother;
			break;
                        case DRAW:
			linelist = drawact (
				(MISCPTR) curstmt->stmt,
				noadtree,
				linelist
			);
			break;
		case STRING:
			linelist = stract (
				(STRPTR) curstmt->stmt,
				noadtree,
				linelist
			);
			break;
		case SPLINE:
			linelist = splact (
				(EXPRPTR) curstmt->stmt,
				noadtree,
				linelist
			);
			break;
		case OPAQUE:
			break;
		case BDLIST:
			break;
		case VAR:
			break;
		default:
			impossible ("rbuild");
		}
	}
	return (linelist);
}

LINEPTR build (noadtree, linelist)
NOADPTR noadtree;
LINEPTR linelist;
{
	LINEPTR retval;
	if (when_bug & 040) bug_on;
	retval = rbuild (noadtree, linelist);
	bug_off;
	return (retval);
}

LINEPTR connact (connstmt, noadtree, linelist)
EXPRPTR connstmt;
NOADPTR noadtree;
LINEPTR linelist;
{
	INTLPTR frompt, topt;
	LINEPTR newline;
	EXPRPTR linwalk;
	newline = NULL;
	frompt = expreval (connstmt->expr, noadtree);
	linwalk = connstmt->next;
	while (linwalk) {
		topt = expreval (linwalk->expr, noadtree);
		if (!known(frompt) || !known(topt)) {
			fprintf (stderr, "ideal: indeterminate endpoints\n   >>>conn ignored\n");
		} else {
			newline = linegen (
				Re(frompt), Im(frompt),
				Re(topt), Im(topt)
			);
			dprintf "Adding line (%f,%f) to (%f,%f)\n",
				Re(frompt), Im(frompt),
				Re(topt), Im(topt)
			);
			newline->next = linelist;
			linelist = newline;
		}
		intlfree (frompt);
		frompt = topt;
		linwalk = linwalk->next;
	}
	intlfree (topt);
	return (newline);
}

LINEPTR penact (penstmt, noadtree, linelist)
PENPTR penstmt;
NOADPTR noadtree;
LINEPTR linelist;
{
	INTLPTR frompt, topt, copies;
	LINEPTR newline;
	frompt = expreval (penstmt->from, noadtree);
	topt = expreval (penstmt->to, noadtree);
	copies = expreval (penstmt->copies, noadtree);
	newline = linelist;
	if (!known(frompt) || !known(topt) || !known(copies)) {
		fprintf (stderr, "ideal: indeterminate pen\n   >>>pen ignored\n");
	} else {
		PUTNODE dummyput;
		NOADPTR pennoad;
		STMTPTR ostmthead;
		int i;

		dprintf "Drawing pen from (%f,%f) to (%f,%f)\n",
			Re(frompt),
			Im(frompt),
			Re(topt),
			Im(topt)
		);

		/* add key statements to beginning of parameter section */
		dummyput.name = 0;
		dummyput.parm = penstmt->pen;
		ostmthead = dummyput.parm->stmtlist;
		dummyput.parm->stmtlist = stmtgen (
			'=',
			(char *) intlgen (
				'=',
				(EXPR) extlgen (namegen (lookup ("$pencnt"))),
				(EXPR) fextlgen (0.0)
			)
		);
		dummyput.parm->stmtlist->next = stmtgen (
			'=',
			(char *) intlgen (
				'=',
				penstmt->start,
				bracket (
					(EXPR) intlgen (
						'/',
						(EXPR) extlgen(namegen(lookup("$pencnt"))),
						(EXPR) copies
					),
					(EXPR) frompt,
					(EXPR) topt
				)
			)
		);
		dummyput.parm->stmtlist->next->next = stmtgen (
			'=',
			(char *) intlgen (
				'=',
				penstmt->end,
				bracket (
					(EXPR) intlgen (
						'/',
						(EXPR) intlgen (
							'+',
							(EXPR) extlgen(namegen(lookup("$pencnt"))),
							(EXPR) fextlgen(1.0)
						),
						(EXPR) copies
					),
					(EXPR) frompt,
					(EXPR) topt
				)
			)
		);
		dummyput.parm->stmtlist->next->next->next = stmtgen (
			VAR,
			(char *) namegen (lookup ("$pencnt"))
		);
		dummyput.parm->stmtlist->next->next->next->next = ostmthead;
		/* make N copies */
		for (i = 0; i < Re(copies); i ++) {
			((EXTLPTR) ((INTLPTR) dummyput.parm->stmtlist->stmt)->right)->info.const = i;
			pennoad = buildnoadtree (&dummyput);
			pennoad->father = noadtree;
			eqneval (pennoad);
			nl_eval ();
			newline = build (pennoad, newline);
			depvarkill ();
			noadfree (pennoad);
		}
	if (dummyput.parm->stmtlist->next->next->next->next != ostmthead)
		impossible ("penact");
	dummyput.parm->stmtlist->next->next->next->next = NULL;
	/* will have to let garbage collector get dummyput.parm */
	penstmt->pen->stmtlist = ostmthead;
	}
	intlfree (frompt);
	intlfree (topt);
	intlfree (copies);
	return (newline);
}

LINEPTR drawact (noadname, noadtree, linelist)
MISCPTR noadname;
NOADPTR noadtree;
LINEPTR linelist;
{
	NOADPTR noadwalk;
	LINEPTR nuline;
	for (noadwalk = noadtree->son;
		noadwalk && (noadwalk->defnode->name != noadname->info);
		noadwalk = noadwalk->brother)
		dprintf "%s %s",
			idprint(noadwalk->defnode->name),
			idprint(noadname->info)
		);
		;
	if (noadwalk) {
		((LINEPTR) tail ((BOXPTR) noadwalk->linelist))->next = linelist;
		nuline = noadwalk->linelist;
		noadwalk->linelist = NULL;
		return (nuline);

	} else {
		fprintf (stderr, "ideal: can't find %s to draw it\n",
			idprint (noadname->info)
		);
		return (linelist);
	}
}

LINEPTR stract (strstmt, noadtree, linelist)
STRPTR strstmt;
NOADPTR noadtree;
LINEPTR linelist;
{
	LINEPTR newline;
	INTLPTR atpt;
	atpt = expreval (strstmt->at, noadtree);
	if (!known(atpt)){
		fprintf (stderr, "ideal: indeterminate string location\n   >>>string ignored\n");
		newline = linelist;
	} else {
		dprintf "Adding string %s\n",
			strstmt->string);
		newline = textgen (
			strstmt->command,
			strstmt->string,
			Re(atpt),
			Im(atpt)
		);
		newline->next = linelist;
	}
	intlfree (atpt);
	return (newline);
}

LINEPTR circact (noadtree, linelist)
NOADPTR noadtree;
LINEPTR linelist;
{
	LINEPTR newline;
	INTLPTR radius, center;
	radius = varfind (lookup ("radius"), noadtree);
	center = varfind (lookup ("center"), noadtree);
	if (!known(radius) || !known(center)) {
		fprintf (stderr, "ideal: indeterminate circle\n   >>>ignored\n");
		newline = linelist;
	} else {
		float rad;
		rad = Re(radius);
		dprintf "Adding circle %f %f %f\n",
			Re(center),
			Im(center),
			rad
		);
		newline = circgen (
			Re(center),
			Im(center),
			rad
		);
		newline->next = linelist;
	}
	intlfree (radius);
	intlfree (center);
	return (newline);
}

LINEPTR arcact (noadtree, linelist)
NOADPTR noadtree;
LINEPTR linelist;
{
	LINEPTR newline;
	INTLPTR start, center, end;
	INTLPTR temp;
	float radius;
	float startang, midang, endang;
	center = varfind (lookup ("center"), noadtree);
	start = varfind (lookup ("start"), noadtree);
	end = varfind (lookup ("end"), noadtree);
	temp = varfind (lookup ("startang"), noadtree);
	startang = Re(temp);
	tryfree(temp);
	temp = varfind (lookup ("midang"), noadtree);
	midang = Re(temp);
	if (fabs(midang - startang) < EPSILON)
		midang = startang;
	tryfree(temp);
	temp = varfind (lookup ("endang"), noadtree);
	endang = Re(temp);
	tryfree(temp);
	if (!radflag) {
		dtor(startang);
		dtor(midang);
		dtor(endang);
	}
	startang = rprin (startang);
	midang = rprin (midang);
	endang = rprin (endang);
	if (fabs(startang - midang) < EPSILON
		&& startang > endang)
		endang += 2*PI;
	radius = ((DEPPTR) (varfind (lookup ("radius"), noadtree))->left)->coeff;
	if (!known(center) || !known(start) || !known(end)) {
		fprintf (stderr, "ideal: indeterminate arc\n   >>>ignored\n");
		newline = linelist;
	} else {
		angorder (&startang, midang, &endang);
		dprintf "Adding arc %f %f %f %f %f\n",
			Re(center),
			Im(center),
			radius,
			startang,
			endang
		);
		newline = angularc (
			Re(center),
			Im(center),
			radius,
			startang,
			endang
		);
		newline->next = linelist;
	}
	intlfree (center);
	intlfree (start);
	intlfree (end);
	return (newline);
}

LINEPTR splact (splstmt, noadtree, linelist)
EXPRPTR splstmt;
NOADPTR noadtree;
LINEPTR linelist;
{
	EXPRNODE knotlist;
	EXPRPTR knotwalk;
	EXPRPTR splwalk;
	LINEPTR nuline;

	if (when_bug & 0200) bug_on;
	knotwalk = &knotlist;
	knotwalk->next = NULL;
	for (splwalk = splstmt;
		splwalk;
		splwalk = splwalk->next
	) {
		knotwalk->next = exprgen (
			(EXPR) expreval (
				splwalk->expr,
				noadtree
			)
		);
		if (!known(((INTLPTR) knotwalk->next->expr))) {
			fprintf (stderr, "ideal: unknown spline knot\n   >>>spline ignored\n");
			return (linelist);
		}
		knotwalk = knotwalk->next;
		dprintf "spline knot: %f %f\n",
			Re(((INTLPTR) knotwalk->expr)),
			Im(((INTLPTR) knotwalk->expr))
		);
	}
	nuline = splgen (knotlist.next);
	nuline->next = linelist;
	return (nuline);
}
