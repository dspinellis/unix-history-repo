/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_temp.h"

/*
 * Set command.
 */
char	optname[ONMSZ];

set()
{
	register char *cp;
	register struct option *op;
	register int c;
	bool no;

	setnoaddr();
	if (skipend()) {
		if (peekchar() != EOF)
			ignchar();
		propts();
		return;
	}
	do {
		cp = optname;
		do {
			if (cp < &optname[ONMSZ - 2])
				*cp++ = getchar();
		} while (isalpha(peekchar()));
		*cp = 0;
		cp = optname;
		if (eq("all", cp)) {
			if (inopen)
				pofix();
			prall();
			goto next;
		}
		no = 0;
		if (cp[0] == 'n' && cp[1] == 'o') {
			cp += 2;
			no++;
		}
		for (op = options; op < &options[NOPTS]; op++)
			if (eq(op->oname, cp) || op->oabbrev && eq(op->oabbrev, cp))
				break;
		if (op->oname == 0)
			serror("%s: No such option@- 'set all' gives all option values", cp);
		c = skipwh();
		if (peekchar() == '?') {
			ignchar();
printone:
			propt(op);
			noonl();
			goto next;
		}
		if (op->otype == ONOFF) {
			op->ovalue = 1 - no;
			goto next;
		}
		if (no)
			serror("Option %s is not a toggle", op->oname);
		if (c != 0 || setend())
			goto printone;
		if (getchar() != '=')
			serror("Missing =@in assignment to option %s", op->oname);
		switch (op->otype) {

		case NUMERIC:
			if (!isdigit(peekchar()))
				error("Digits required@after = when assigning numeric option");
			op->ovalue = getnum();
			if (value(TABSTOP) <= 0)
				value(TABSTOP) = TABS;
			break;

		case STRING:
		case OTERM:
			cp = optname;
			while (!setend()) {
				if (cp >= &optname[ONMSZ])
					error("String too long@in option assignment");
				*cp++ = getchar();
			}
			*cp = 0;
			if (op->otype == OTERM) {
				if (inopen)
					error("Can't change type of terminal from within open/visual");
				setterm(optname);
			} else {
				CP(op->osvalue, optname);
				op->odefault = 1;
			}
			break;
		}
next:
		flush();
	} while (!skipend());
	eol();
}

setend()
{

	return (iswhite(peekchar()) || endcmd(peekchar()));
}

prall()
{
	register int incr = (NOPTS + 2) / 3;
	register int rows = incr;
	register struct option *op = options;

	for (; rows; rows--, op++) {
		propt(op);
		tab(24);
		propt(&op[incr]);
		if (&op[2*incr] < &options[NOPTS]) {
			tab(48);
			propt(&op[2 * incr]);
		}
		putNFL();
	}
}

propts()
{
	register struct option *op;

	for (op = options; op < &options[NOPTS]; op++) {
#ifdef V6
		if (op == &options[TERM])
#else
		if (op == &options[TTYTYPE])
#endif
			continue;
		switch (op->otype) {

		case ONOFF:
		case NUMERIC:
			if (op->ovalue == op->odefault)
				continue;
			break;

		case STRING:
			if (op->odefault == 0)
				continue;
			break;
		}
		propt(op);
		putchar(' ');
	}
	noonl();
	flush();
}

propt(op)
	register struct option *op;
{
	register char *name;
	
	name = op->oname;

	switch (op->otype) {

	case ONOFF:
		printf("%s%s", op->ovalue ? "" : "no", name);
		break;

	case NUMERIC:
		printf("%s=%d", name, op->ovalue);
		break;

	case STRING:
	case OTERM:
		printf("%s=%s", name, op->osvalue);
		break;
	}
}
