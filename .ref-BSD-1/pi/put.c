#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "opcode.h"
#include "0.h"

int	*obufp	{ obuf };

/*
 * If DEBUG is defined, include the table
 * of the printing opcode names.
 */
#ifdef DEBUG
char	*otext[] {
#include "OPnames.h"
};
#endif

/*
 * Put is responsible for the interpreter equivalent of code
 * generation.  Since the interpreter is specifically designed
 * for Pascal, little work is required here.
 */
put(a)
{
	register int *p, i;
	register char *cp;
	int n, subop, suboppr, op, oldlc, w;
	char *string;
	static int casewrd;

	/*
	 * It would be nice to do some more
	 * optimizations here.  The work
	 * done to collapse offsets in lval
	 * should be done here, the IFEQ etc
	 * relational operators could be used
	 * etc.
	 */
	oldlc = lc;
	if (cgenflg)
		/*
		 * code disabled - do nothing
		 */
		return (oldlc);
	p = &a;
	n = *p++;
	suboppr = subop = (*p>>8) & 0377;
	op = *p & 0377;
	string = 0;
#ifdef DEBUG
	if ((cp = otext[op]) == NIL) {
		printf("op= %o\n", op);
		panic("put");
	}
#endif
	switch (op) {
/*****
		case O_LINO:
			if (line == codeline)
				return (oldlc);
			codeline = line;
*****/
		case O_PUSH:
		case O_POP:
			if (p[1] == 0)
				return (oldlc);
		case O_NEW:
		case O_DISPOSE:
		case O_AS:
		case O_IND:
		case O_OFF:
		case O_CON2:
		case O_INX2:
		case O_INX4:
		case O_CARD:
		case O_ADDT:
		case O_SUBT:
		case O_MULT:
		case O_IN:
		case O_CASE1OP:
		case O_CASE2OP:
		case O_CASE4OP:
		case O_PACK:
		case O_UNPACK:
		case O_RANG2:
		case O_RSNG2:
		case O_RANG42:
		case O_RSNG42:
			if (p[1] < 128 && p[1] >= -128) {
				suboppr = subop = p[1];
				p++;
				n--;
				if (op == O_CON2)
					op = O_CON1;
			}
			break;
		default:
			if (op >= O_REL2 && op <= O_REL84) {
				if ((i = (subop >> 1) * 5 ) >= 30)
					i =- 30;
				else
					i =+ 2;
#ifdef DEBUG
				string = &"IFEQ\0IFNE\0IFLT\0IFGT\0IFLE\0IFGE"[i];
#endif
				suboppr = 0;
			}
			break;
		case O_IF:
		case O_TRA:
/*****
			codeline = 0;
*****/
		case O_CALL:
		case O_FOR1U:
		case O_FOR2U:
		case O_FOR4U:
		case O_FOR1D:
		case O_FOR2D:
		case O_FOR4D:
			p[1] =- lc + 2;
			break;
		case O_WRIT82:
#ifdef DEBUG
			string = &"22\024\042\044"[subop*3];
#endif
			suboppr = 0;
			break;
		case O_CONG:
			i = p[1];
			cp = p[2];
#ifdef DEBUG
			if (opt('c'))
				printf("%5d\tCONG:%d\t%s\n", lc, i, cp);
#endif
			if (i <= 127)
				word(O_CON | i << 8);
			else {
				word(O_CON);
				word(i);
			}
			while (i > 0) {
				w = *cp ? *cp++ : ' ';
				w =| (*cp ? *cp++ : ' ') << 8;
				word(w);
				i =- 2;
			}
			return (oldlc);
		case O_CONC:
#ifdef DEBUG
			(string = "'x'")[1] = p[1];
#endif
			suboppr = 0;
			op = O_CON1;
			subop = p[1];
			goto around;
		case O_CON1:
			suboppr = subop = p[1];
around:
			n--;
			break;
		case O_CASEBEG:
			casewrd = 0;
			return (oldlc);
		case O_CASEEND:
			if (lc & 1) {
				lc--;
				word(casewrd);
			}
			return (oldlc);
		case O_CASE1:
#ifdef DEBUG
			if (opt('c'))
				printf("%5d\tCASE1\t%d\n", lc, p[2]);
#endif
			lc++;
			if (lc & 1)
				casewrd = p[2];
			else {
				lc =- 2;
				word(casewrd | p[2] << 8);
			}
			return (oldlc);
		case O_CASE2:
#ifdef DEBUG
			if (opt('c'))
				printf("%5d\tCASE2\t%d\n", lc, p[2]);
#endif
			word(p[2]);
			return (oldlc);
		case O_CASE4:
#ifdef DEBUG
			if (opt('c'))
				printf("%5d\tCASE4\t%d %d\n", lc, p[1], p[2]);
#endif
			word(p[1]);
			word(p[2]);
			return (oldlc);
	}
#ifdef DEBUG
	if (opt('c')) {
		printf("%5d\t%s", lc, cp);
		if (suboppr)
			printf(":%d", suboppr);
		if (string)
			printf("\t%s",string);
		if (n > 1)
			putchar('\t');
		for (i=1; i<n; i++)
			printf("%d ", p[i]);
		putchar('\n');
	}
#endif
	if (op != NIL)
		word(op | subop << 8);
	for (i=1; i<n; i++)
		word(p[i]);
	return (oldlc);
}

/*
 * Putspace puts out a table
 * of nothing to leave space
 * for the case branch table e.g.
 */
putspace(n)
	int n;
{
	register i;
#ifdef DEBUG
	if (opt('c'))
		printf("%5d\t.=.+%d\n", lc, i);
#endif
	for (i = even(n); i > 0; i =- 2)
		word(0);
}

/*
 * Patch repairs the branch
 * at location loc to come
 * to the current location.
 */
patch(loc)
{

	patchfil(loc, lc-loc-2);
}

/*
 * Patchfil makes loc+2 have value
 * as its contents.
 */
patchfil(loc, value)
	char *loc;
	int value;
{
	register i;

	if (cgenflg < 0)
		return;
	if (loc > lc)
		panic("patchfil");
#ifdef DEBUG
	if (opt('c'))
		printf("\tpatch %u %d\n", loc, value);
#endif
	i = (loc + 2 - (lc & ~0777))/2;
	if (i >= 0 && i < 512)
		obuf[i] = value;
	else {
		seek(ofil, loc+2, 0);
		write(ofil, &value, 2);
		seek(ofil, 0, 2);
	}
}

/*
 * Put the word o into the code
 */
word(o)
	int o;
{

	*obufp = o;
	obufp++;
	lc =+ 2;
	if (obufp >= obuf+256)
		pflush();
}

extern	char *obj;
/*
 * Flush the code buffer
 */
pflush()
{
	register i;

	i = (obufp - obuf) * 2;
	if (i != 0 && write(ofil, obuf, i) != i)
		perror(obj), pexit(DIED);
	obufp = obuf;
}

/*
 * Getlab - returns the location counter.
 * included here for the eventual code generator.
 */
getlab()
{

	return (lc);
}

/*
 * Putlab - lay down a label.
 */
putlab(l)
	int l;
{

	return (l);
}
