#ifndef lint
static char *sccsid ="exprn.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

void nonlinerr (funcname)
char *funcname;
{
	fprintf (stderr, "ideal: %s() of unknown\n   >>>Returning 1.0\n", funcname);
}

static DEPPTR depvarlist = NULL;
static boolean incon_warn = TRUE;
static boolean nl_warn;
boolean nl_fail;
static EQNPTR nl_eqns = NULL;

INTLPTR expreval (exprn, givennoad)
EXPR exprn;
NOADPTR givennoad;
{
	/* This routine returns an INTLPTR whose operator
	   is ';'--a promoted commanode containing the
	   dependency list representing the real part in
	   its left field, the imag part in its right */
	register INTLPTR intl;
	register EXTLPTR extl;
	if (!exprn)
		return (commagen (0.0, 0.0));
	if (((EXTLPTR)exprn)->leaf) {
		extl = (EXTLPTR) exprn;
		dprintf "At a leaf of kind %d\n", extl->kind);
		switch (extl->kind) {
		case PATH:
			return (pathfind (extl->info.path, givennoad));
			break;
		case CONST:
			return (commagen (extl->info.const, 0.0));
			break;
		}
	}
	intl = (INTLPTR) exprn;
	if (intl->oper == NAME) {
		dprintf "Looking for a function named %s\n", idprint ((int) intl->left));
	} else {
		dprintf "At an internal node with operator %c\n", intl->oper);
	}
	switch (intl->oper) {
	INTLPTR lefttemp, righttemp, temp, temp2;
	DEPPTR drek, drek2;
	float repart, impart, modulus;
	case NAME:
		if (((int) intl->left) == lookup ("re")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			depfree ((DEPPTR) temp->right);
			temp->right = (EXPR) depgen ((VARPTR) NULL, 0.0);
			return (temp);
		} else if (((int) intl->left) == lookup ("im")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			depfree ((DEPPTR) temp->left);
			temp->left = temp->right;
			temp->right = (EXPR) depgen ((VARPTR) NULL, 0.0);
			return (temp);
		} else if (((int) intl->left) == lookup ("conj")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			temp2 = intlgen (
				';',
				(EXPR) depadd (
					(DEPPTR) NULL, 0.0,
					(DEPPTR) temp->left, 1.0
				),
				(EXPR) depadd (
					(DEPPTR) NULL, 0.0,
					(DEPPTR) temp->right, -1.0
				)
			);
			intlfree (temp);
			return (temp2);
		} else if (((int) intl->left) == lookup ("abs")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("abs");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0, 0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				intlfree (temp);
				return (commagen (hypot (repart, impart), 0.0));
			}
		} else if (((int) intl->left) == lookup ("cis")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("cis");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0, 0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				if (!radflag) {
					dtor(repart);
					dtor(impart);
				}
				intlfree (temp);
				if (impart > EPSILON)
					fprintf (stderr, "ideal: cis of complex value\n   >>>Ignoring imaginary part\n");
				return (commagen (cos (repart), sin (repart)));
			}
		} else if (((int) intl->left) == lookup ("int")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("int");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0,0.0));
			} else {
				double intpart;
				repart = Re(temp);
				impart = Im(temp);
				intlfree (temp);
				if (impart > EPSILON)
					fprintf (stderr, "ideal: int of complex value\n   >>>Ignoring imaginary part\n");
				modf (repart, &intpart);
				return (commagen ((float) intpart, 0.0));
			}
		} else if (((int) intl->left) == lookup ("atan2")
			|| ((int) intl->left) == lookup ("angle")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("angle");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0,0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				intlfree (temp);
				repart = atan2 (impart, repart);
				if (!radflag)
					rtod(repart);
				return (commagen (repart, 0.0));
			}
		} else if (((int) intl->left) == lookup ("E")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("E");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0, 0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				if (impart > EPSILON)
					fprintf (stderr, "ideal: E of complex value\n   >>>Ignoring imaginary part\n");
				repart *= 2*PI;
				return (commagen (cos (repart), sin (repart)));
			}
		} else if (((int) intl->left) == lookup ("unit")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("unit");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0, 0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				intlfree (temp);
				if ((modulus = hypot (repart, impart)) < EPSILON)
					return (commagen (0.0, 0.0));
				else return (
					commagen (
						repart/modulus,
						impart/modulus
					)
				);
			}
		} else if (((int) intl->left) == lookup ("sqrt")) {
			temp = expreval (((EXPRPTR) intl->right)->expr, givennoad);
			if (!known(temp)) {
				if (nl_warn)
					nonlinerr ("sqrt");
				nl_fail = !nl_warn;
				intlfree (temp);
				return (commagen (1.0, 0.0));
			} else {
				repart = Re(temp);
				impart = Im(temp);
				intlfree (temp);
				if ((modulus = hypot (repart, impart)) < EPSILON)
					return (commagen (0.0, 0.0));
				else {
					float theta;
					modulus = sqrt (modulus);
					theta = 0.5*atan2 (impart,repart);
					return (
						commagen (
							modulus*cos(theta),
							modulus*sin(theta)
						)
					);
				};
			}
		} else {
			fprintf (stderr, "ideal: unknown function name:  %s\n   >>>Returning 1.0\n", idprint ((int) intl->left));
			return (commagen (1.0, 0.0));
		}
		break;
	case '~':
		incon_warn = FALSE;
		/* FALL THROUGH TO '=' case */
	case '=':
		{
		DEPPTR depvarwalk;
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		if (nl_fail) {
			dprintf "Non-linear equation: failure\n");
			intlfree (lefttemp);
			intlfree (righttemp);
			return (commagen (0.0,0.0));
		}
		for (depvarwalk = depvarlist;
			depvarwalk;
			depvarwalk = depvarwalk->next) {
			lefttemp->left = (EXPR) depsubst (
				(DEPPTR) lefttemp->left,
				(DEPPTR) depvarwalk->var->deplist,
				depvarwalk->var
			);
			lefttemp->right = (EXPR) depsubst (
				(DEPPTR) lefttemp->right,
				(DEPPTR) depvarwalk->var->deplist,
				depvarwalk->var
			);
			righttemp->left = (EXPR) depsubst (
				(DEPPTR) righttemp->left,
				(DEPPTR) depvarwalk->var->deplist,
				depvarwalk->var
			);
			righttemp->right = (EXPR) depsubst (
				(DEPPTR) righttemp->right,
				(DEPPTR) depvarwalk->var->deplist,
				depvarwalk->var
			);
		}
		dprintf "equating real parts...\n");
		drek = depadd (
			(DEPPTR) lefttemp->left, 1.0,
			(DEPPTR) righttemp->left, -1.0
		);
		eqndo (drek, exprn, givennoad);
		depfree (drek);
		if (depvarlist) {
			/* trick: at most one variable became
			/* dependent by the above processing,
			/* so only it must be replaced in the
			/* equation on the imaginary parts */
			lefttemp->right = (EXPR) depsubst (
				(DEPPTR) lefttemp->right,
				(DEPPTR) depvarlist->var->deplist,
				depvarlist->var
			);
			righttemp->right = (EXPR) depsubst (
				(DEPPTR) righttemp->right,
				(DEPPTR) depvarlist->var->deplist,
				depvarlist->var
			);
		}
		dprintf "equating imag parts...\n");
		drek = depadd (
			(DEPPTR) lefttemp->right, 1.0,
			(DEPPTR) righttemp->right, -1.0
		);
		eqndo (drek, exprn, givennoad);
		depfree (drek);
		intlfree (lefttemp);
		return (righttemp);
		}
		break;
	case '+':
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		drek = depadd (
			(DEPPTR) lefttemp->left, 1.0,
			(DEPPTR) righttemp->left, 1.0
		);
		drek2 = depadd (
			(DEPPTR) lefttemp->right, 1.0,
			(DEPPTR) righttemp->right, 1.0
		);
		intlfree (lefttemp);
		intlfree (righttemp);
		return (intlgen (';', (EXPR) drek, (EXPR) drek2));
		break;
	case '-':
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		drek = depadd (
			(DEPPTR) lefttemp->left, 1.0,
			(DEPPTR) righttemp->left, -1.0
		);
		drek2 = depadd (
			(DEPPTR) lefttemp->right, 1.0,
			(DEPPTR) righttemp->right, -1.0
		);
		intlfree (lefttemp);
		intlfree (righttemp);
		return (intlgen (';', (EXPR) drek, (EXPR) drek2));
		break;
	case '*':
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		if (known(lefttemp)) {
			repart = ((DEPPTR) lefttemp->left)->coeff;
			impart = ((DEPPTR) lefttemp->right)->coeff;
			intlfree (lefttemp);
			drek = depadd (
				(DEPPTR) righttemp->left, repart,
				(DEPPTR) righttemp->right, -impart
			);
			drek2 = depadd (
				(DEPPTR) righttemp->left, impart,
				(DEPPTR) righttemp->right, repart
			);
			intlfree (righttemp);
			return (intlgen (';', (EXPR) drek, (EXPR) drek2));
		} else if (known(righttemp)) {
			repart = ((DEPPTR) righttemp->left)->coeff;
			impart = ((DEPPTR) righttemp->right)->coeff;
			intlfree (righttemp);
			drek = depadd (
				(DEPPTR) lefttemp->left, repart,
				(DEPPTR) lefttemp->right, -impart
			);
			drek2 = depadd (
				(DEPPTR) lefttemp->left, impart,
				(DEPPTR) lefttemp->right, repart
			);
			intlfree (lefttemp);
			return (intlgen (';', (EXPR) drek, (EXPR) drek2));
		} else {
			if (nl_warn)
				fprintf (stderr, "ideal: multiplication of two unknowns\n   >>>Returning 1.0\n");
			nl_fail = !nl_warn;
			intlfree (lefttemp);
			intlfree (righttemp);
			return (commagen (1.0, 0.0));
		}
		break;
	case '/':
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		if (!known(righttemp)) {
			if (nl_warn)
				fprintf (stderr, "ideal: division by an unknown\n   >>>Returning 1.0\n");
			nl_fail = !nl_warn;
			intlfree (lefttemp);
			intlfree (righttemp);
			return (commagen (1.0, 0.0));
		} else {
			repart = ((DEPPTR) righttemp->left)->coeff;
			impart = - ((DEPPTR) righttemp->right)->coeff;
			modulus = repart*repart + impart*impart;
			intlfree (righttemp);
			if (modulus < EPSILON*EPSILON) {
				fprintf (stderr, "ideal: division by zero\n   >>>Returning 1.0\n");
				intlfree (lefttemp);
				return (commagen (1.0, 0.0));
			} else {
				drek = depadd (
					(DEPPTR) lefttemp->left, repart/modulus,
					(DEPPTR) lefttemp->right, -impart/modulus
				);
				drek2 = depadd (
					(DEPPTR) lefttemp->left, impart/modulus,
					(DEPPTR) lefttemp->right, repart/modulus
				);
				intlfree (lefttemp);
				return (intlgen (';', (EXPR) drek, (EXPR) drek2));
			}
		}
		break;
	case ',':
		lefttemp = expreval (intl->left, givennoad);
		righttemp = expreval (intl->right, givennoad);
		depfree((DEPPTR) lefttemp->right);
		depfree((DEPPTR) righttemp->right);
		temp = intlgen (
			';',
			(EXPR) lefttemp->left,
			(EXPR) righttemp->left
		);
		tryfree(lefttemp);
		tryfree(righttemp);
		return (temp);
		break;
	case ';':
		drek = depadd (
			(DEPPTR) intl->left, 1.0,
			(DEPPTR) NULL, 0.0
		);
		drek2 = depadd (
			(DEPPTR) intl->right, 1.0,
			(DEPPTR) NULL, 0.0
		);
		return (intlgen (';', (EXPR) drek, (EXPR) drek2));
	case '^':
		return (expreval (intl->right, givennoad));
	default:
		fprintf (stderr, "ideal: unknown operator: %c\n   >>>Returning 1.0\n", intl->oper);
		return (commagen (1.0, 0.0));
		break;
	}
}

void eqndo (deplist, eqn, givennoad)
DEPPTR deplist;
EXPR eqn;
NOADPTR givennoad;
{
	/* when called, equation system says deplist == 0 */
	if (!deplist->next && !deplist->var) {
		if (fabs (deplist->coeff) > EPSILON) {
			if (incon_warn) {
				fprintf (stderr, "ideal: inconsistent equation in %s named %s\n",
					idprint (givennoad->defnode->parm->name),
					idprint (givennoad->defnode->name)
				);
				exprprint (((INTLPTR) eqn)->left);
				fprintf (stderr, "=");
				exprprint (eqn);
				fprintf (stderr, "\n");
			}
			dprintf "Inconsistent equation\n");
		} else
			dprintf "Redundant equation\n");
	}
	else {
		DEPPTR curmax;
		float maxcoeff;
		DEPPTR depvarwalk;
		DEPPTR listwalk;
		maxcoeff = -1;
		/* find variable whose coefficient is largest in absolute value */
		for (listwalk = deplist;
			listwalk;
			listwalk = listwalk->next)
			if (listwalk->var && (maxcoeff < fabs (listwalk->coeff))) {
				maxcoeff = fabs (listwalk->coeff);
				curmax = listwalk;
			}
		/* get that variable represented in terms of the others */
		listwalk = depadd (
			curmax->var->deplist, 1.0,
			deplist, -1.0/curmax->coeff
		);
		depfree (curmax->var->deplist);
		curmax->var->deplist = listwalk;
		/* put it on a list of dependent variables
		/* replace occurrences of it in other dependent variables */
		if (!depvarlist) {
			depvarlist = depgen (curmax->var, 0.0);
		}
		else {
			DEPPTR newhead;
			for (depvarwalk = depvarlist;
				depvarwalk;
				depvarwalk = depvarwalk->next) {
				depvarwalk->var->deplist = depsubst (
					depvarwalk->var->deplist,
					curmax->var->deplist,
					curmax->var
				);
			}
			newhead = depgen (curmax->var, 0.0);
			newhead->next = depvarlist;
			depvarlist = newhead;
		}
	}
}

void depvarclean ()
{
	/* clean known variables out of the dependent variable list */
	DEPPTR prevdep, depvarwalk;
	DEPNODE nuhead;
	prevdep = &nuhead;
	prevdep->next = depvarwalk = depvarlist;
	while (depvarwalk) {
		if (!depvarwalk->var->deplist->var) {
			dprintf "Removing %s(%s) = %f from dependent variable list\n",
				ISREAL(depvarwalk->var)?"re":"im",
				idprint (THENAME(depvarwalk->var)),
				depvarwalk->var->deplist->coeff);
			prevdep->next = depvarwalk->next;
			tryfree(depvarwalk);
			depvarwalk = prevdep->next;
		} else {
			prevdep = depvarwalk;
			depvarwalk = depvarwalk->next;
		}
	}
	depvarlist = nuhead.next;
}

void reqneval (noadtree)
NOADPTR noadtree;
{
	STMTPTR slist[2];
	STMTPTR eqnwalk;
	int i;
	if (!noadtree)
		return;
	nl_warn = FALSE;
	slist[0] = noadtree->defnode->parm->stmtlist;
	slist[1] = findbox (noadtree->defnode->parm->name,FALSE)->stmtlist;
	for (i = 0; i < 2; i ++)
		for (eqnwalk = nextstmt ('=', slist[i]);
			eqnwalk;
			eqnwalk = nextstmt ('=', eqnwalk->next)) {
			INTLPTR junk;
			nl_fail = FALSE;
			junk = expreval ((EXPR) eqnwalk->stmt, noadtree);
			intlfree (junk);
			if (nl_fail) {
				EQNPTR nueqn;
				nueqn = eqngen (
					(EXPR) eqnwalk->stmt,
					noadtree
				);
				nueqn->next = nl_eqns;
				nl_eqns = nueqn;
				nl_fail = FALSE;
			}
			depvarclean ();
			incon_warn = TRUE;
		}
	reqneval (noadtree->son);
	reqneval (noadtree->brother);
}

void eqneval (noadtree)
NOADPTR noadtree;
{
	if (when_bug & 04) bug_on;
	reqneval (noadtree);
	bug_off;
}

void nl_eval ()
{
	static boolean nl_succ;
	INTLPTR junk;
	{
	EQNPTR nl_prev, nl_curr, nl_temp;
	if (when_bug & 010) bug_on;
	nl_prev = nl_curr = nl_eqns;
	nl_temp = NULL;
	while (nl_curr) {
		nl_curr = nl_prev->next;
		nl_prev->next = nl_temp;
		nl_temp = nl_prev;
		nl_prev = nl_curr;
	}
	nl_eqns = nl_temp;
	nl_succ = TRUE;
	}
	while (nl_eqns && nl_succ) {
		EQNPTR prev_eqn, nl_walk;
		EQNNODE dummy_eqn;
		dprintf "Retrying nonlinear equations\n");
		prev_eqn = &dummy_eqn;
		prev_eqn->next = nl_walk = nl_eqns;
		nl_succ = FALSE;
		while (nl_walk) {
			nl_fail = FALSE;
			junk = expreval (nl_walk->eqn, nl_walk->noad);
			intlfree (junk);
			depvarclean ();
			if (!nl_fail) {
				prev_eqn->next = nl_walk->next;
				tryfree(nl_walk);
				nl_walk = prev_eqn->next;
				nl_succ = TRUE;
			} else {
				prev_eqn = nl_walk;
				nl_walk = nl_walk->next;
			}
		}
		nl_eqns = dummy_eqn.next;
	}
	if (nl_eqns) {
		EQNPTR nl_walk, nl_next;
		dprintf "Nonlinear failure\n");
		nl_warn = TRUE;
		for (nl_walk = nl_eqns;
			nl_walk;
			nl_walk = nl_next) {
			junk = expreval (nl_walk->eqn, nl_walk->noad);
			intlfree (junk);
			depvarclean ();
			nl_next = nl_walk->next;
			tryfree(nl_walk);
		}
	}
	bug_off;
}

void depvarkill ()
{
	/* remove all unknown variables from depvarlist ...
	   no chance for them to be determined now  */
	if (!depvarlist)
		return;
	if (when_bug & 020)
		fprintf (stderr, "killing depvarlist\n");
	depfree (depvarlist);
	depvarlist = NULL;
}
