#ifndef lint
static char sccsid[] = "@(#)for.c	3.1 (CWI) 85/07/30";
#endif lint

#include <stdio.h>
#include "pic.h"
#include "y.tab.h"

#define	SLOP	1.001

typedef struct {
	char	*var;	/* index variable */
	float	to;	/* limit */
	float	by;
	int	op;	/* operator */
	char	*str;	/* string to push back */
} For;

For	forstk[10];	/* stack of for loops */
For	*forp = forstk;	/* pointer to current top */

forloop(var, from, to, op, by, str)	/* set up a for loop */
	char *var;
	float from, to, by;
	int op;
	char *str;
{
	dprintf("# for %s from %g to %g by %c %g \n",
		var, from, to, op, by);
	if (++forp >= forstk+10)
		fatal("for loop nested too deep");
	forp->var = var;
	forp->to = to;
	forp->op = op;
	forp->by = by;
	forp->str = str;
	setfval(var, from);
	nextfor();
	unput('\n');
}

nextfor()	/* do one iteration of a for loop */
{
	/* BUG:  this should depend on op and direction */
	if (getfval(forp->var) > SLOP * forp->to) {	/* loop is done */
		free(forp->str);
		if (--forp < forstk)
			fatal("forstk popped too far");
	} else {		/* another iteration */
		pushsrc(String, "\nEndfor\n");
		pushsrc(String, forp->str);
	}
}

endfor()	/* end one iteration of for loop */
{
	struct symtab *p = lookup(forp->var);

	switch (forp->op) {
	case '+':
	case ' ':
		p->s_val.f += forp->by;
		break;
	case '-':
		p->s_val.f -= forp->by;
		break;
	case '*':
		p->s_val.f *= forp->by;
		break;
	case '/':
		p->s_val.f /= forp->by;
		break;
	}
	nextfor();
}

char *ifstat(expr, thenpart, elsepart)
	double expr;
	char *thenpart, *elsepart;
{
	dprintf("if %g then <%s> else <%s>\n", expr, thenpart, elsepart? elsepart : "");
	if (expr) {
		unput('\n');
		pushsrc(Free, thenpart);
		pushsrc(String, thenpart);
		unput('\n');
  		if (elsepart)
			free(elsepart);
		return thenpart;	/* to be freed later */
	} else {
		free(thenpart);
		if (elsepart) {
			unput('\n');
			pushsrc(Free, elsepart);
			pushsrc(String, elsepart);
			unput('\n');
		}
		return elsepart;
	}
}
