#ifndef lint
static char sccsid[] = "@(#)format.c	5.3 (Berkeley) 6/29/90";
#endif

/*
 * adb - formats
 */

#include "defs.h"
#include <ctype.h>
#include <vis.h>

extern char BADMOD[];
extern char NOFORK[];

/* symbol desirability in exform() */
enum { IFEXACT, ALWAYS, NEVER } wantsym;

char	*exform();

/*
 * Execute the given format `ecount' times.
 */
scanform(forcesym, fmt, space, ptype)
	int forcesym;
	char *fmt;
	int space, ptype;
{
	register char *p;
	register int c, n;
	register expr_t ntimes = ecount;
	addr_t savdot, newdot;

	if (ntimes == 0)
		return;
	for (wantsym = forcesym ? ALWAYS : IFEXACT;; wantsym = IFEXACT) {
		p = fmt;
		savdot = dot;
		while (p != NULL) {	/* loop over format items */
			n = 0;		/* get optional count */
			while (isdigit(c = *p++))
				n = n * 10 + c - '0';
			if (c == 0)	/* end of format */
				break;
			p = exform(n ? n : 1, p - (c != '\\'), space, ptype);
		}
		dotinc = (newdot = dot) - savdot;
		dot = savdot;
		if (errflag != NULL && (long)ntimes < 0) {
			errflag = NULL;
			break;
		}
		checkerr();
		if (--ntimes == 0)
			break;
		dot = newdot;
	}
}

/*
 * Print a halfword or a word from dot.
 */
showdot(fullword, space, ptype)
	int fullword, space, ptype;
{
	char c = fullword ? '4' : '2';

	wantsym = NEVER;
	(void) exform(1, &c, space, ptype);
}

/*
 * The following are used inside exform().
 *
 * The various FT_ values specify the type of the object accessed
 * by some format character.  FT_DULL indicates that no object is
 * accessed (or that it is done in some peculiar way).
 * The fsize array holds the size (in bytes)
 * of each of those types; the fmttypes[] array lists the type for
 * each character.  To save space, since there are many characters
 * for some of the types, they are stored as strings.
 */
enum { FT_DULL, FT_CHAR, FT_HW, FT_FW, FT_ADDR, FT_FLT, FT_DBL, FT_TM };
	/* these may have to be turned into `#define's */

static char fsize[] = {		/* ordered by enumeration above! */
	0, sizeof(char), sizeof(hword_t), sizeof(expr_t),
	sizeof(addr_t), sizeof(float), sizeof(double), sizeof(time_t)
};

static struct fmttypes {
	char	*ft_chars;
	int	ft_type;
} fmttypes[] = {
	{ "\t\" +-NRST^inrst", FT_DULL },
	{ "1BCbc", FT_CHAR },
	{ "2doquvxz", FT_HW },
	{ "4DOQUVXZ", FT_FW },
	{ "p", FT_ADDR },
	{ "f", FT_FLT },
	{ "F", FT_DBL },
	{ "Y", FT_TM },
	0
};

/*
 * Execute a single format item `fcount' times; set
 * dotinc and move dot.  Return the address of the next
 * format item, or NULL upon error reading an object.
 *
 * I must apologise for the length of this routine, but
 * it is bloated mainly with type correctness.
 */
char *
exform(fcount, fmt, space, ptype)
	int fcount;
	char *fmt;
	int space, ptype;
{
	register struct fmttypes *ftp;
	register int sz;
	register char *p, *s, fmtchar;
	addr_t savdot, off;
	struct nlist *sp;
	union {
		char c;
		hword_t hw;
		expr_t fw;
		float f;
		double d;
		time_t tm;
		addr_t a;
	} obj;

	while (fcount > 0) {
		/*
		 * First decode the type to be used with the expression.
		 * If address, print dot as a symbol, save it in var 0,
		 * and bypass all the nonsense.
		 */
		p = fmt;
		fmtchar = *p++;

		/* address: special */
		if (fmtchar == 'a') {
			pdot();
			wantsym = NEVER;	/* well, hardly ever */
			var[0] = dot;
			return (p);
		}

		for (ftp = fmttypes; (s = ftp->ft_chars) != NULL; ftp++)
			while (*s != 0)
				if (*s++ == fmtchar)
					goto found;
		error(BADMOD);
		/* NOTREACHED */
found:

		/* plop out a symbol, if desired */
		if (wantsym == ALWAYS)
			pdot();
		else if (wantsym == IFEXACT &&
		    (sp = findsym(dot, ptype, &off)) != NULL && off == 0)
			adbprintf("\n%s:%16t", sp->n_un.n_name); /* \n ??? */
		wantsym = NEVER;

		/*
		 * Now read the sort of object we decided fmtchar represents,
		 * or compute it from the expression given for dot.
		 */
		sz = fsize[ftp->ft_type];
		if (space != SP_NONE) {
			/* can just read into the union */
			if (sz != 0)
				(void) adbread(space, dot, &obj, sz);
			else
				obj.fw = edot;
		} else {
			/* must decode type in order to assign, alas */
			switch (ftp->ft_type) {

			case FT_CHAR:
				obj.c = edot;
				break;

			case FT_HW:
				obj.hw = edot;
				break;

			case FT_FW:
				obj.fw = edot;
				break;

			case FT_DULL:
			case FT_ADDR:
				obj.a = dot;
				break;

			case FT_FLT:
			case FT_DBL:
				obj.fw = 0;
				etofloat(edot, &obj.c, ftp->ft_type == FT_DBL);
				break;

			case FT_TM:
				obj.fw = 0;
				obj.tm = edot;
				break;

			default:
				panic("exform 1");
				/* NOTREACHED */
			}
		}

		/* if we could not read the object, stop now. */
		if (errflag)
			return (NULL);
		if (mkfault)
			error((char *)NULL);

		/*
		 * Now copy the value read (or assigned) to var[0].
		 * Here some of the types are collapsed: since the
		 * idea is to be able to get the value back later
		 * by reading var[0] and going through the type
		 * decoding above, it sometimes suffices to record
		 * as many bits as fit in an expr_t (see expr.c).
		 *
		 * Note that double precision numbers generally lose
		 * bits, since sizeof(double) can be > sizeof(expr_t).
		 */
		switch (ftp->ft_type) {

		case FT_CHAR:
			var[0] = obj.c;
			break;

		case FT_HW:
			var[0] = obj.hw;
			break;

		case FT_FW:
		case FT_FLT:
		case FT_DBL:
		case FT_TM:
			var[0] = obj.fw;
			break;

		case FT_DULL:
		case FT_ADDR:
			var[0] = obj.a;
			break;

		default:
			panic("exform 2");
			/* NOTREACHED */
		}

		/* set the size, if this object has a size */
		if (sz)
			dotinc = sz;

		/* finally, do the command */
		if (charpos() == 0)
			adbprintf("%16m");
		switch (fmtchar) {
			/*
			 * Many of the formats translate to a %-8 or %-16
			 * edition of themselves; we use a single string,
			 * and modify the format part, for these.
			 */
			static char cfmt[] = "%-*?";

		case ' ':
		case '\t':
			dotinc = 0;
			break;

		case 't':
		case 'T':
			adbprintf("%*t", fcount);
			return (p);

		case 'r':
		case 'R':
			adbprintf("%*m", fcount);
			return (p);

		case 'p':
			psymoff("%R", obj.a, ptype, maxoff, "%16t");
			break;

		case 'c':
			printc(obj.c);
			break;

		case 'C':
			printesc(obj.c);
			break;

		case 'b':
		case 'B':
			adbprintf("%-8O", (expr_t)(u_char)obj.c);
			break;

		case 's':
		case 'S':
			savdot = dot;
			for (;;) {
				if (adbread(space, dot, &obj.c, 1) != 1 ||
				    iserr() || obj.c == 0)
					break;
				dot = inkdot(1);
				if (fmtchar == 'S')
					printesc(obj.c);
				else
					printc(obj.c);
				endline();
			}
			dotinc = dot - savdot + 1;
			dot = savdot;
			break;

		case '1':
			adbprintf("%-8R", (expr_t)(u_char)obj.c);
			break;

		case '2':
			fmtchar = 'r';
			/* FALLTHROUGH */

		case 'v':
		case 'u': case 'd':
		case 'o': case 'q':
		case 'x': case 'z':
			cfmt[3] = fmtchar;
			adbprintf(cfmt, 8, obj.hw);
			break;

		case '4':
			fmtchar = 'R';
			/* FALLTHROUGH */

		case 'V':
		case 'U': case 'D':
		case 'O': case 'Q':
		case 'X': case 'Z':
			cfmt[3] = fmtchar;
			adbprintf(cfmt, 16, obj.fw);
			break;

		case 'Y':
			adbprintf("%-24Y", obj.tm);
			break;

		case 'i':
			printins(space);	/* also sets dotinc */
			printc('\n');
			break;

		case 'f':
			s = checkfloat((caddr_t)&obj.f, 0);
			if (s != NULL)
				adbprintf("%-16s", s);
			else
				adbprintf("%-16.9f", obj.f);
			break;

		case 'F':
			s = checkfloat((caddr_t)&obj.d, 1);
			if (s != NULL)
				adbprintf("%-32s", s);
			else
				adbprintf("%-32.18f", obj.d);
			break;

		case 'n':
		case 'N':
			printc('\n');
			dotinc = 0;
			break;

		case '"':
			while (*p != 0 && *p != '"')
				printc(*p++);
			if (*p)
				p++;
			dotinc = 0;
			break;

		case '^':
			dot = inkdot(-dotinc * fcount);
			return (p);

		case '+':
			dot = inkdot(fcount);
			return (p);

		case '-':
			dot = inkdot(-fcount);
			return (p);

		default:
			panic("exform 3");
			/* NOTREACHED */
		}
		if (space != SP_NONE)
			dot = inkdot(dotinc);
		fcount--;
		endline();
	}
	return (p);
}

/*
 * Print dot in its canonical format.
 */
pdot()
{

	psymoff("%R", dot, SP_INSTR, maxoff, ":%16t");
}

/*
 * Print character c using ASCII escape conventions.
 */
printesc(c)
	register int c;

{
	char visbuf[5];

	vis(visbuf, (char)c, VIS_TAB | VIS_NL | VIS_NOSLASH, 0);
	adbprintf("%s", visbuf);
}
