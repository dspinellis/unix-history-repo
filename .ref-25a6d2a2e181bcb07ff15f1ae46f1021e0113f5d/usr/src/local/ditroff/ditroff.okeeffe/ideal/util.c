#ifndef lint
static char *sccsid ="util.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

#define SYMSIZE	250

static char *symtab[SYMSIZE]	= {
	"(unnamed)"
};
static int syminstal	= 1;

int lookup(identifier)
char *identifier;
{
	register int i;
	i = 0;
	symtab[syminstal] = identifier;
	while (strcmp(identifier, symtab[i]))
		i++;
	if (i == syminstal) {
		dprintf "installing NAME: %s\n", identifier);
		if (i < SYMSIZE) {
			if (!(symtab[i] = malloc((unsigned) (1+strlen(identifier))))) {
				fprintf(stderr,"ideal: memory overflow in lookup\n");
				exit(1);
			}
		}
		else {
			fprintf(stderr,"ideal: too many identifiers\n");
			exit(1);
		}
		strcpy(symtab[syminstal++], identifier);
	}
	return(i);
}

char *idprint(idnum)
int idnum;
{
	if ((idnum > -1) && (idnum < syminstal))
		return(symtab[idnum]);
	else {
		fprintf(stderr,"ideal: invalid identifier index: %d\n", idnum);
		return(NULL);
	}
}

extern BOXPTR boxlist;

BOXPTR findbox (sought, alarm)
int sought,
	alarm;
{
	BOXPTR bxwalk;
	for (bxwalk = boxlist;
		bxwalk && (bxwalk->name != sought);
		bxwalk = bxwalk->next)
		;
	if (!bxwalk) {
		if (!alarm)
			fprintf (stderr, "ideal: undefined box:  %s\n", idprint (sought));
		return (boxgen (sought, (STMTPTR) NULL));
	} else
		return (bxwalk);
}


INTLPTR varfind (name, givennoad)
int name;
NOADPTR givennoad;
{
	/* finds simple variable 'name' from 'givennoad' */
	register VARPTR varwalk;
	if (!givennoad) {
		fprintf (stderr, "ideal: no such variable, %s\n  >>>Returning 0.0\n",
			idprint (name)
		);
		return (commagen (0.0, 0.0));
	}
	dprintf "Looking for %s in %s\n", idprint (name), idprint (givennoad->defnode->name));
	for (varwalk = givennoad->edgevarlist;
		varwalk && THENAME(varwalk) != name;
		varwalk = varwalk->next)
		;
	if (!varwalk)
		for (varwalk = givennoad->boxvarlist;
			varwalk && THENAME(varwalk) != name;
			varwalk = varwalk->next)
			;
	if (!varwalk)
		return (varfind (name, givennoad->father));
	else
		return (intlgen (
			';',
			(EXPR) depadd ((DEPPTR) NULL, 0.0, varwalk->deplist, 1.0),
			(EXPR) depadd ((DEPPTR) NULL, 0.0, varwalk->next->deplist, 1.0)
			));
}

INTLPTR pathfind (ptname, givennoad)
NAMEPTR ptname;
NOADPTR givennoad;
{
	/* finds compound variable 'ptname' from 'givennoad' */
	int i;
	STMTPTR putwalk;
	NOADPTR noadwalk;
	if (!ptname->next)
		return (varfind (ptname->name, givennoad));
	for (i = 0; i < 2; i++) {
		noadwalk = givennoad->son;
		for (putwalk = nextstmt (PUT, givennoad->defnode->parm->stmtlist);
			putwalk && ((PUTPTR) putwalk->stmt)->name != ptname->name;
			putwalk = nextstmt (PUT, putwalk->next))
			noadwalk = noadwalk->brother;
		if (!putwalk)
			for (putwalk = nextstmt (PUT, findbox (givennoad->defnode->parm->name,FALSE)->stmtlist);
				putwalk && ((PUTPTR) putwalk->stmt)->name != ptname->name;
				putwalk = nextstmt (PUT, putwalk->next))
				noadwalk = noadwalk->brother;
		if (putwalk) {
			dprintf "found %s, now looking for %s\n",
				idprint (ptname->name),
				idprint (ptname->next->name)
			);
			return (pathfind (ptname->next, noadwalk));
		}
		if (!(givennoad = givennoad->father)) {
			dprintf "reached root of noad tree\n");
			break;
		} else {
			dprintf "looking for %s at father of %s\n",
				idprint (ptname->name),
				idprint (givennoad->defnode->name)
			);
		}
	}
	fprintf (stderr, "ideal: invalid variable path name beginning %s\n  >>>Returning 0.0\n",
		idprint (ptname->name));
	return (commagen (0.0, 0.0));
}

BOXPTR tail (head)
BOXPTR head;
{
	while (head->next)
		head = head->next;
	return (head);
}

void forget (sought)
int sought;
{
	BOXPTR bxwalk;
	BOXPTR prevbox;
	prevbox = NULL;
	for (bxwalk = boxlist;
		bxwalk && (bxwalk->name != sought);
		bxwalk = bxwalk->next)
		prevbox = bxwalk;
	if (bxwalk) {
		if (prevbox) {
			prevbox->next = bxwalk->next;
		} else {
			boxlist = bxwalk->next;
		}
		boxfree (bxwalk);
	}
}

void exprprint (exprn)
EXPR exprn;
{
	INTLPTR intl;
	EXTLPTR extl;
	if (!exprn)
		return;
	if (((EXTLPTR) exprn)->leaf) {
		extl = (EXTLPTR) exprn;
		switch (extl->kind) {
		case PATH:
			{
			NAMEPTR pathwalk;
			for (pathwalk = extl->info.path;
				pathwalk->next;
				pathwalk = pathwalk->next)
				fprintf (stderr, "%s.", idprint (pathwalk->name));
			fprintf (stderr, "%s", idprint (pathwalk->name));
			}
			break;
		case CONST:
			fprintf (stderr, "%f", extl->info.const);
			break;
		}
	} else {
		intl = (INTLPTR) exprn;
		switch (intl->oper) {
		case NAME:
			fprintf (stderr, "%s(", idprint ((int) intl->left));
			exprprint (((EXPRPTR) intl->right)->expr);
			fprintf (stderr, ")");
			break;
		case '=':
		case '~':
			exprprint ((EXPR) intl->right);
			break;
		case ',':
			fprintf (stderr, "(");
			exprprint ((EXPR) intl->left);
			fprintf (stderr, ",");
			exprprint ((EXPR) intl->right);
			fprintf (stderr, ")");
			break;
		case ';':
			fprintf (stderr, "(");
			depprint ((DEPPTR) intl->left);
			fprintf (stderr, ",");
			depprint ((DEPPTR) intl->right);
			fprintf (stderr, ")");
			break;
		default:
			fprintf (stderr, "(");
			exprprint ((EXPR) intl->left);
			fprintf (stderr, " %c ", intl->oper);
			exprprint ((EXPR) intl->right);
			fprintf (stderr, ")");
			break;
		}
	}
}

STMTPTR nextstmt (kind, curstmt)
int kind;
STMTPTR curstmt;
{
	register STMTPTR stmtwalk;
	stmtwalk = curstmt;
	for (stmtwalk;
		stmtwalk && (stmtwalk->kind != kind);
		stmtwalk = stmtwalk->next)
		;
	return (stmtwalk);
}

EXPR bracket (alpha, x, y)
EXPR alpha,
	x,
	y;
{
	return (
		(EXPR) intlgen (
			'+',
			x,
			(EXPR) intlgen (
				'*',
				alpha,
				(EXPR) intlgen (
					'-',
					y,
					x
				)
			)
		)
	);
}

void depprint (depnd)
DEPPTR depnd;
{
	for (depnd;
		depnd->next;
		depnd = depnd->next)
		fprintf (stderr, "%f %s(%s) + ",
			depnd->coeff,
			ISREAL(depnd->var)?"re":"im",
			idprint (THENAME(depnd->var))
		);
	fprintf (stderr, "%f", depnd->coeff);
	if (depnd->var)
		fprintf (stderr, " %s(%s)",
			ISREAL(depnd->var)?"re":"im",
			idprint (THENAME(depnd->var))
		);
}

void dexch (a,b)
double *a;
double *b;
{
	double temp;
	temp = *a;
	*a = *b;
	*b = temp;
}

void fexch (a,b)
float *a;
float *b;
{
	float temp;
	temp = *a;
	*a = *b;
	*b = temp;
}

float rprin (angle)
float angle;
{
	while (angle < 0.0)
		angle += 2*PI;
	while (angle > 2*PI + EPSILON)
		angle -= 2*PI;
	return (angle);
}

/*
float dprin (angle)
float angle;
{
	while (angle < 0.0)
		angle += 360;
	while (angle > 2*PI)
		angle -= 360;
	return (angle);
}
*/

void angorder (startang, midang, endang)
float *startang;
float midang;
float *endang;
{
	if (
		((*endang < midang) && (midang < *startang))
	||	((*startang < *endang) && (*endang < midang))
	||	((midang < *startang) && (*startang < *endang))
	)
		fexch (startang, endang);
}

STMTPTR reverse (stmtlist)
STMTPTR stmtlist;
{
	STMTPTR curstmt, prevstmt, temp;
	prevstmt = curstmt = stmtlist;
	temp = NULL;
	while (curstmt) {
		curstmt = prevstmt->next;
		prevstmt->next = temp;
		temp = prevstmt;
		prevstmt = curstmt;
	}
	return (temp);
}

void impossible (msg)
char *msg;
{
	fprintf (stderr, "ideal: %s: can't happen\n", msg);
	exit (1);
}
