#include "sh.h"

/*
 * Shell:
 *	"set" and associated routines
 *
 *----------
 *	doset - performs the logic of the set command
 *	operate - performs arithmetic function of the set command
 *	unset - undefines variables
 *	xfree - frees argument storage if it is dynamic
 *	savestr - salts away a copy of the argument string in a safe place
 *	putn  - returns a pointer to a string representing the argument integer
 *	getn  - returns the number which the string argument represents or 0
 *----------
 *	value - returns the value of a variable which is argument
 *	adrof - returns a pointer to the structure for the variable argument
 *	set   - sets the argument variable to the second argument value
 */

/*
 * doset performs the logic of the set command.
 * command syntax is
 *	set [ rOPvalue ] *
 * current ops are += -= ++ -- &= |= *= /= %= and =
 *
 * Magic operator @ actually does an unset.  Good for use conditionally!
 */
doset(v)
	register char *v[];
{
	register int *rp;
	register char *p;
	char *vp, c, op;

	v++;
	p = *v++;
	if (p == 0) {
		prvars();
		return;
	}
	do {
		if (digit(*p)) {
			vp = p;
			p++;
		} else
			for (vp = p; letter(*p); p++)
				continue;
		if (vp == p) {
			bferr(": No variable");
			return;
		}
		op = *p;
		*p++ = 0;
		switch (op) {
			case 0:
				p--;
			case '=':
				set(vp, savestr(p));
				continue;
			case '@':
				if (*p != 0) {
					bferr(": Garbage after @");
					return;
				}
				unsetv(vp);
				continue;
		}
		c = *p++;
		if (c != '=') {
			if (!any(op, "+-") || c != op || *p) {
				bferr(": Missing =");
				return;
			}
			*--p = '1';
		}
		set(vp, operate(op, vp, p));
	} while (p = *v++);
}

operate(op, vp, p)
	char op, *vp, *p;
{
	register int vl, vr;

	vl = getn(value(vp));
	vr = getn(p);
	switch (op & 0177) {
		case '+':
			vl =+ vr;
			break;
		case '-':
			vl =- vr;
			break;
		case '&':
			vl =& vr;
			break;
		case '|':
			vl =| vr;
			break;
		case '/':
			if (vr == 0)
				bferr(": Divide check");
			else
				vl =/ vr;
			break;
		case '%':
			if (vr == 0)
				bferr(": Mod check");
			else
				vl =% vr;
			break;
		case '*':
			vl =* vr;
			break;
		default:
			bferr(": Bad operator");
	}
	return (putn(vl));
}

/*
 * xfree frees possibly non-dynamic storage.
 * It insures thats its argument is in the heap
 * before freeing it.
 */
xfree(cp)
	char *cp;
{
	extern char end[];

	cp = (cp + 1) &~ 1;
	if (cp >= end && cp < &cp)
		cfree(cp);
}

savestr(s)
	char *s;
{

	if (s == 0)
		s = "";
	return (strcpy(calloc(1, strlen(s) + 1), s));
}

/* static */char *putp;
/*
 * putn takes an integer and returns a pointer to
 * its string representation.
 * putp indexes the number as it is formed.
 */
putn(n)
{
	static char number[7];

/*
	sprintf(number, "%d", n);
*/
	putp = number;
	if (n < 0) {
		n = -n;
		*putp++ = '-';
	}
	putn1(n);
	*putp = 0;
	return (savestr(number));
}

putn1(n)
	int n;
{
	if (n > 9)
		putn1(n / 10);
	*putp++ = n % 10 + '0';
}

/*
 * getn is used by doset to get numbers from strings.
 * null string is considered to be 0 (ala SNOBOL).
 */
getn(cp)
	register char *cp;
{
	register n;
	int sign, base;

	sign = 0;
	base = 10;
	if (*cp == '-') {
		sign++;
		cp++;
		if (!digit(*cp))
			goto badnum;
	} else if (*cp == '0')
		base = 8;
	n = 0;
	while (digit(*cp))
		n = n * base + *cp++ - '0';
	if (*cp)
		goto badnum;
	return (sign ? -n : n);
badnum:
	bferr(": Badly formed number");
	return (0);
}

/*
 * value takes a string name of shell
 * variable and returns a pointer to its value
 */
char *
value(var)
	char *var;
{
	return (value1(var, &shvhed));
}

char *
value1(var, head)
	char *var;
	struct shvar *head;
{
	register struct shvar *vp;

	vp = adrof1(var, head);
	return (vp == 0 ? "" : vp->value);
}

/* static */ struct shvar *shprev;
/*
 * adrof takes a variable name and returns
 * a pointer to its structure or 0.
 * A side effect is to make shprev point to the
 * structure before this one for obvious reasons.
 */
struct shvar *
adrof(var)
	char *var;
{
	register struct shvar *vp;

	if (digit(var[0])) {
		vp = rgadrof(var[0]);
		if (vp == 0) {
			bferr(": Bad parameter");
			vp = &shvhed;
		}
		return (vp);
	}
	return (adrof1(var, &shvhed));
}

struct shvar *
adrof1(var, head)
	char *var;
	struct shvar *head;
{
	register struct shvar *vp;
	int cmp;

	shprev = head;
	for (vp = shprev->next; vp != 0; vp = vp->next) {
		cmp = strcmp(vp->name, var);
		if (cmp == 0)
			return (vp);
		else if (cmp > 0)
			return (0);
		shprev = vp;
	}
	return (0);
}

/*
 * set sets the variable argument to
 * the given value.
 * The caller is responsible for putting value
 * in a safe place!
 */
set(var, value)
	char *var, *value;
{

	set1(var, value, &shvhed);
}

set1(var, value, head)
	char *var, *value;
	struct shvar *head;
{
	register struct shvar *vp;

	vp = adrof1(var, head);
	if (vp == 0) {
		vp = calloc(1, sizeof *vp);
		vp->name = savestr(var);
		vp->next = shprev->next;
		shprev->next = vp;
	}
	if (value == 0)
		value = "";
	else
		xfree(vp->value);
	vp->value = value;
}

unset(v)
	register char *v[];
{

	unset1(v, &shvhed);
}

unset1(v, head)
	register char *v[];
	struct shvar *head;
{
	register char *var;

	v++;
	while (var = *v++)
		unsetv1(var, head);
}

unsetv(var)
	char *var;
{

	unsetv1(var, &shvhed);
}

unsetv1(var, head)
	char *var;
	struct shvar *head;
{
	register struct shvar *vp;

	vp = adrof1(var, head);
	if (vp == 0) {
		bferr2(var, ": Undefined");
		return;
	}
	vp = shprev->next;
	shprev->next = vp->next;
	xfree(vp->value);
	xfree(vp);
}
