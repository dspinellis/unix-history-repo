#ifndef lint
static char *sccsid ="memut.c	(CWI)	1.1	85/03/01";
#endif
/* memory utilities */

#include "ideal.h"
#include "y.tab.h"

char *fooalloc;
#define	tryalloc(new,kind)	\
	if (!(new =(kind *) malloc(sizeof (kind)))) {\
	emergency ();\
	if (!(new =(kind *) malloc(sizeof (kind)))) {\
		fprintf (stderr, "ideal: Out of space\n");\
		exit (1);\
		}\
	};\
	for (fooalloc = (char *) new;\
		fooalloc < ((char *) new) + sizeof (kind);\
		fooalloc ++)\
		*fooalloc = '\0';

STMTPTR stmtgen (kind, stmt)
int kind;
char *stmt;
{
	register STMTPTR newguy;
	tryalloc(newguy,STMTNODE);
	newguy->kind = kind;
	newguy->stmt = stmt;
	return (newguy);
}

BOXPTR boxgen (name,stmtlist)
int name;
STMTPTR stmtlist;
{
	register BOXPTR newguy;
	STMTPTR bdstmt;
	tryalloc(newguy,BOXNODE);
	newguy->name = name;
	/* the stmts are in reverse order (check the yacc grammar) */
	newguy->stmtlist = reverse (stmtlist);
	if (bdstmt = nextstmt (BDLIST, stmtlist))
		bdstmt->stmt = (char *) reverse ((STMTPTR) bdstmt->stmt);
	return (newguy);
}

NAMEPTR namegen (name)
int name;
{
	register NAMEPTR newguy;
	tryalloc(newguy,NAMENODE);
	newguy->name = name;
	return (newguy);
}

EXPRPTR exprgen (expr)
EXPR expr;
{
	register EXPRPTR newguy;
	tryalloc(newguy,EXPRNODE);
	newguy->expr = expr;
	return (newguy);
}

PUTPTR putgen (name, parm, p_or_c)
int name;
BOXPTR parm;
int p_or_c;
{
	register PUTPTR newguy;
	tryalloc(newguy,PUTNODE);
	newguy->name = name;
	newguy->parm = parm;
	newguy->p_or_c = p_or_c;
	return (newguy);
}

PENPTR pengen (from, to, copies, start, end, pen)
EXPR from,
	to,
	copies,
	start,
	end;
BOXPTR pen;
{
	register PENPTR newguy;
	tryalloc(newguy,PEN_NODE);
	newguy->from = from;
	newguy->to = to;
	newguy->copies = copies;
	newguy->start = start;
	newguy->end = end;
	newguy->pen = pen;
	return (newguy);
}

MISCPTR miscgen (info)
int info;
{
	register MISCPTR newguy;
	tryalloc(newguy,MISCNODE);
	newguy->info = info;
	return (newguy);
}

INTLPTR intlgen (oper, left, right)
int oper;
EXPR left,
right;
{
	register INTLPTR newguy;
	tryalloc(newguy,EXPRINTL);
	newguy->leaf = FALSE;
	newguy->oper = oper;
	newguy->left = left;
	newguy->right = right;
	return (newguy);
}

INTLPTR commagen (real, imag)
float real,
	imag;
{
	register INTLPTR newguy;
	tryalloc(newguy,EXPRINTL);
	newguy->leaf = FALSE;
	newguy->oper = ';';
	newguy->left = (EXPR) depgen ((VARPTR) NULL, real);
	newguy->right = (EXPR) depgen ((VARPTR) NULL, imag);
	return (newguy);
}

EXTLPTR extlgen (path)
NAMEPTR path;
{
	register EXTLPTR newguy;
	tryalloc(newguy,EXPREXTL);
	newguy->leaf = TRUE;
	newguy->info.path = path;
	newguy->kind = PATH;
	return (newguy);
}

EXTLPTR fextlgen (value)
float value;
{
	register EXTLPTR newguy;
	tryalloc(newguy,EXPREXTL);
	newguy->leaf = TRUE;
	newguy->info.const = value;
	newguy->kind = CONST;
	return (newguy);
}

NOADPTR noadgen (defnode, edgevarlist, boxvarlist)
PUTPTR defnode;
VARPTR edgevarlist;
VARPTR boxvarlist;
{
	register NOADPTR newguy;
	tryalloc(newguy,NOAD);
	newguy->defnode = defnode;
	newguy->edgevarlist = edgevarlist;
	newguy->boxvarlist = boxvarlist;
	return (newguy);
}

VARPTR vargen (name, re, deplist)
int name;
boolean re;
DEPPTR deplist;
{
	register VARPTR newguy;
	tryalloc(newguy,VARNODE);
	newguy->re_name = re?name:-name;
	newguy->deplist = deplist;
	return (newguy);
}

static DEPPTR depavh = NULL;
static DEPPTR depavt = NULL;

DEPPTR depgen (var, coeff)
VARPTR var;
float coeff;
{
	register DEPPTR newguy;
	if (depavh) {
		newguy = depavh;
		depavh = depavh->next;
		if (!depavh)
			depavt = NULL;
		newguy->next = NULL;
	} else
		tryalloc(newguy,DEPNODE);
	newguy->var = var;
	newguy->coeff = coeff;
	return (newguy);
}

LINEPTR linegen (x0, y0, x1, y1)
float x0,
	y0,
	x1,
	y1;
{
	register LINEPTR newguy;
	tryalloc(newguy,LINENODE);
	newguy->kind = LINE;
	newguy->x0 = x0;
	newguy->y0 = y0;
	newguy->x1 = x1;
	newguy->y1 = y1;
	return (newguy);
}

EDGEPTR edgeline (x0, y0, x1, y1)
float x0,
	y0,
	x1,
	y1;
{
	EDGEPTR newguy;
	tryalloc(newguy,EDGENODE);
	newguy->fax = (ARCPTR) NULL;
	newguy->sx = x0;
	newguy->sy = y0;
	newguy->ex = x1;
	newguy->ey = y1;
	newguy->stx = newguy->sx + 0.2*(newguy->ex - newguy->sx);
	newguy->sty = newguy->sy + 0.2*(newguy->ey - newguy->sy);
	newguy->etx = newguy->ex + 0.2*(newguy->sx - newguy->ex);
	newguy->ety = newguy->ey + 0.2*(newguy->sy - newguy->ey);
	dprintf "opaque polygon edge: %f,%f -- %f,%f\n",
		x0,y0, x1,y1
	);
	return (newguy);
}

LINEPTR circgen (x0, y0, r)
float x0,
	y0,
	r;
{
	register CIRCPTR newguy;
	tryalloc(newguy,CIRCNODE);
	newguy->kind = CIRCLE;
	newguy->x0 = x0;
	newguy->y0 = y0;
	newguy->r = r;
	return ((LINEPTR) newguy);
}

/*
LINEPTR arcgen (x0, y0, x1, y1, x2, y2, theta1, theta2, radius)
float x0,
	y0,
	x1,
	y1,
	x2,
	y2,
	theta1,
	theta2,
	radius;
{
	register ARCPTR newguy;
	tryalloc(newguy,ARCNODE);
	newguy->kind = ARC;
	newguy->x0 = x0;
	newguy->y0 = y0;
	newguy->x1 = x1;
	newguy->y1 = y1;
	newguy->x2 = x2;
	newguy->y2 = y2;
	newguy->theta1 = theta1;
	newguy->theta2 = theta2;
	newguy->radius = radius;
	return ((LINEPTR) newguy);
}
*/

LINEPTR angularc (x0, y0, radius, theta1, theta2)
float x0,
	y0,
	theta1,
	theta2,
	radius;
{
	/* theta1 and theta2 should be in radians */
	register ARCPTR newguy;
	tryalloc(newguy,ARCNODE);
	radius = fabs(radius);
	newguy->kind = ARC;
	newguy->x0 = x0;
	newguy->y0 = y0;
	newguy->x1 = x0 + cos (theta1)*radius;
	newguy->y1 = y0 + sin (theta1)*radius;
	newguy->x2 = x0 + cos (theta2)*radius;
	newguy->y2 = y0 + sin (theta2)*radius;
	theta1 = rprin (theta1);
	theta2 = rprin (theta2);
	while (theta2 - theta1 < EPSILON)
		theta2 += 2*PI;
	if (fabs(theta2 - theta1) > PI)
		radius *= -1;
	newguy->theta1 = theta1;
	newguy->theta2 = theta2;
	newguy->radius = radius;
	return ((LINEPTR) newguy);
}

LINEPTR pointarc (x1,y1, x2,y2, x3,y3)
float x1,y1, x2,y2, x3,y3;
{
	float A, B, C, D, E, F;
	float denom, x, y;
	float startang, midang, endang;
	A = -2.0*(x2 - x1);
	B = -2.0*(y2 - y1);
	C = -2.0*(x3 - x2);
	D = -2.0*(y3 - y2);
	denom = A*D - B*C;
	if (fabs(denom) < EPSILON) {
		dprintf "pointarc: (%f,%f) (%f,%f) (%f,%f) collinear\n", x1,y1, x2,y2, x3,y3);
		return (linegen (x1,y1, x3,y3));
	}
	E = x1*x1 + y1*y1 - x2*x2 - y2*y2;
	F = x2*x2 + y2*y2 - x3*x3 - y3*y3;
	x = E*D - F*B;
	x /= denom;
	y = A*F - C*E;
	y /= denom;
	startang = rprin(atan2 (y1-y, x1-x));
	midang = rprin(atan2 (y2-y, x2-x));
	endang = rprin(atan2 (y3-y, x3-x));
	angorder (&startang, midang, &endang);
	dprintf "pointarc: (%f,%f) (%f,%f) (%f,%f)\n", x1,y1, x2,y2, x3,y3);
	dprintf "pointarc: (%f,%f) %f\n", x, y, hypot(x1-x,y1-y));
	dprintf "pointarc: /_%f -- /_%f\n", startang, endang);
	return (angularc (x, y, hypot(x1-x,y1-y), startang, endang));
}

EDGEPTR edgearc (x1,y1, x2,y2, x3,y3)
float x1,y1, x2,y2, x3,y3;
{
	EDGEPTR newguy;
	tryalloc(newguy,EDGENODE);
	newguy->fax = (ARCPTR) pointarc (x1,y1, x2,y2, x3,y3);
	if (newguy->fax->kind == LINE) {
		newguy->sx = newguy->etx = x1;
		newguy->sy = newguy->ety = y1;
		newguy->ex = newguy->stx = x3;
		newguy->ey = newguy->sty = y3;
		tryfree(newguy->fax);
		newguy->fax = NULL;
		newguy->flipped = FALSE;
	} else if (newguy->fax->kind == ARC) {
		ARCPTR temp;
		temp = newguy->fax;
		newguy->sx = x1;
		newguy->sy = y1;
		newguy->ex = x3;
		newguy->ey = y3;
		newguy->stx = temp->x0 + fabs(temp->radius)*cos(temp->theta1 + 0.2);
		newguy->sty = temp->y0 + fabs(temp->radius)*sin(temp->theta1 + 0.2);
		newguy->etx = temp->x0 + fabs(temp->radius)*cos(temp->theta2 - 0.2);
		newguy->ety = temp->y0 + fabs(temp->radius)*sin(temp->theta2 - 0.2);
		if ((fabs(newguy->sx - temp->x1) > EPSILON)
			|| (fabs(newguy->sy - temp->y1) > EPSILON)) {
			newguy->flipped = TRUE;
			fexch(&newguy->stx,&newguy->etx);
			fexch(&newguy->sty,&newguy->ety);
		} else
			newguy->flipped = FALSE;
		dprintf "edgearc: (%f,%f) --> (%f,%f)\n",
			newguy->sx, newguy->sy,
			newguy->ex, newguy->ey
		);
		dprintf "edgearc: st (%f,%f); et (%f,%f)\n",
			newguy->stx, newguy->sty,
			newguy->etx, newguy->ety
		);
		dprintf "edgearc: %sflipped\n", newguy->flipped?"":"UN");
	} else impossible ("edgearc");
	return (newguy);
}

LINEPTR textgen (command, string, x0, y0)
int command;
char *string;
float x0,
	y0;
{
	register TEXTPTR newguy;
	tryalloc(newguy,TEXTNODE);
	newguy->kind = STRING;
	newguy->command = command;
	newguy->string = string;
	newguy->x0 = x0;
	newguy->y0 = y0;
	return ((LINEPTR) newguy);
}

LINEPTR splgen (knotlist)
EXPRPTR knotlist;
{
	register SPLPTR newguy;
	tryalloc(newguy,SPLNODE);
	newguy->kind = SPLINE;
	newguy->knotlist = knotlist;
	return ((LINEPTR) newguy);
}

STRPTR strgen (command, string, at)
int command;
char *string;
EXPR at;
{
	register STRPTR newguy;
	tryalloc(newguy,STRNODE);
	newguy->command = command;
	newguy->string = string;
	newguy->at = at;
	return (newguy);
}


EQNPTR eqngen (eqn, noad)
EXPR eqn;
NOADPTR noad;
{
	register EQNPTR newguy;
	tryalloc(newguy,EQNNODE);
	newguy->eqn = eqn;
	newguy->noad = noad;
	return (newguy);
}
OPQPTR opqgen (code, alpha)
int code;
float alpha;
{
	OPQPTR newguy;
	tryalloc(newguy,OPQNODE);
	newguy->code = code;
	newguy->alpha = alpha;
	return (newguy);
}

void depfree (doomed)
DEPPTR doomed;
{
	register DEPPTR doomwalk;
	if (!doomed || doomed == depavt)
		return;
	if (!depavh) {
		depavt = depavh = doomed;
		while (depavt->next)
			depavt = depavt->next;
		return;
	}
	doomwalk = doomed;
	while (doomwalk->next) {
		if (doomwalk->next == depavt)
			return;
		doomwalk = doomwalk->next;
	}
	depavt->next = doomed;
	depavt = doomwalk;
}

void nextfree (doomed)
DEPPTR doomed;
{
	register DEPPTR walk;
	while (doomed) {
		walk = doomed->next;
		tryfree(doomed);
		doomed = walk;
	}
}

void namefree (doomed)
NAMEPTR doomed;
{
	nextfree ((DEPPTR) doomed);
}

void exprlsfree (doomed)
EXPRPTR doomed;
{
	register EXPRPTR walk;
	while (doomed) {
		walk = doomed->next;
		exprfree (doomed->expr);
		tryfree(doomed);
		doomed = walk;
	}
}

void linefree (doomed)
LINEPTR doomed;
{
	nextfree ((DEPPTR) doomed);
}

void intlfree (doomed)
INTLPTR doomed;
{
	depfree ((DEPPTR) doomed->left);
	depfree ((DEPPTR) doomed->right);
	tryfree(doomed);
}

void noadfree (doomed)
NOADPTR doomed;
{
	if (!doomed)
		return;
	noadfree (doomed->son);
	noadfree (doomed->brother);
	varfree (doomed->edgevarlist);
	varfree (doomed->boxvarlist);
	linefree(doomed->linelist);
	tryfree(doomed);
}

void varfree (doomed)
VARPTR doomed;
{
	if (!doomed)
		return;
	varfree (doomed->next);
	depfree (doomed->deplist);
	tryfree(doomed);
}


void exprfree (doomed)
EXPR doomed;
{
	if (!doomed)
		return;
	if (!((EXTLPTR) doomed)->leaf) {
		/* convention for functions (name in left, arg list hanging
		/* off right) will ream you if not careful
		/* This also depends on the allocator not complaining if
		/* you free things twice with no intervening allocation.
		/* (see processing of alpha[x,y] in idyac.y) */
		if (((INTLPTR) doomed)->oper == NAME) {
			exprfree (((EXPRPTR)((INTLPTR) doomed)->right)->expr);
			tryfree(((INTLPTR) doomed)->right);
		} else if (((INTLPTR) doomed)->oper == ';') {
			depfree ((DEPPTR)((INTLPTR) doomed)->left);
			depfree ((DEPPTR)((INTLPTR) doomed)->right);
		} else {
			exprfree (((INTLPTR) doomed)->left);
			exprfree (((INTLPTR) doomed)->right);
		}
	}
	tryfree(doomed);
}



void boxfree (doomed)
BOXPTR doomed;
{
	register STMTPTR curstmt, nextstmt;
	for (curstmt = doomed->stmtlist;
		curstmt;
		curstmt = nextstmt) {
		switch (curstmt->kind) {
		case '=':
			exprfree ((EXPR) curstmt->stmt);
			break;
		case CONN:
			exprlsfree ((EXPRPTR) curstmt->stmt);
			break;
		case USING:
			exprfree (((PENPTR) curstmt->stmt)->from);
			exprfree (((PENPTR) curstmt->stmt)->to);
			exprfree (((PENPTR) curstmt->stmt)->copies);
			exprfree (((PENPTR) curstmt->stmt)->start);
			exprfree (((PENPTR) curstmt->stmt)->end);
			boxfree (((PENPTR) curstmt->stmt)->pen);
			tryfree(curstmt->stmt);
			break;
		case PUT:
			boxfree (((PUTPTR) curstmt->stmt)->parm);
			tryfree(curstmt->stmt);
			break;
		case DRAW:
			tryfree(curstmt->stmt);
			break;
		case STRING:
/* if using malloc to get string space, can use the real free here */
			free(((STRPTR) curstmt->stmt)->string);
			exprfree (((STRPTR) curstmt->stmt)->at);
			tryfree(curstmt->stmt);
			break;
		case SPLINE:
			exprlsfree ((EXPRPTR) curstmt->stmt);
			break;
		case OPAQUE:
			tryfree(curstmt->stmt);
			break;
		case BDLIST:
			exprlsfree ((EXPRPTR) curstmt->stmt);
			break;
		case VAR:
			namefree ((NAMEPTR) curstmt->stmt);
			break;
	}
	nextstmt = curstmt->next;
	tryfree(curstmt);
	}
}

void emergency ()
{
	nextfree (depavh);
	depavh = depavt = NULL;
}
