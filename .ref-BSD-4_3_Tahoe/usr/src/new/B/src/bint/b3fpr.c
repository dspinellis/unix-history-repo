/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3fpr.c,v 1.4 85/08/22 16:58:15 timo Exp $
*/

/* B formula/predicate invocation */
#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b3err.h"
#include "b3sem.h"
#include "b3sou.h"

#define Other 0
#define Nume 1

#define In 1
#define Not_in 2
#ifdef EXT_COMMAND
#define Char_ready 3
#endif

/*
 * Table defining all predefined functions (but not propositions).
 */

#ifdef EXT_COMMAND

extern value e_getchar();
extern value e_screensize();
extern outcome e_ch_ready();

#endif EXT_COMMAND

struct funtab {
	string f_name; literal f_adic, f_kind;
	value	(*f_fun)();
	bool f_extended;
} funtab[] = {
	{"~",  Mon, Nume, approximate},
	{"+",  Mon, Nume, copy},
	{"+",  Dya, Nume, sum},
	{"-",  Mon, Nume, negated},
	{"-",  Dya, Nume, diff},
	{"*/", Mon, Nume, numerator},
	{"/*", Mon, Nume, denominator},

	{"*",  Dya, Nume, prod},
	{"/",  Dya, Nume, quot},
	{"**", Dya, Nume, power},

	{"^",  Dya, Other, concat},
	{"^^", Dya, Other, repeat},
	{"<<", Dya, Other, adjleft},
	{"><", Dya, Other, centre},
	{">>", Dya, Other, adjright},

	{"#",  Mon, Other, size},
	{"#",  Dya, Other, size2},

	{"pi", Zer, Other, pi},
	{"e",  Zer, Other, e},

	{"abs",    Mon, Nume, absval},
	{"sign",   Mon, Nume, signum},
	{"floor",  Mon, Nume, floorf},
	{"ceiling",Mon, Nume, ceilf},
	{"round",  Mon, Nume, round1},
	{"round",  Dya, Nume, round2},
	{"mod",    Dya, Nume, mod},
	{"root",   Mon, Nume, root1},
	{"root",   Dya, Nume, root2},

	{"sin", Mon, Nume, sin1},
	{"cos", Mon, Nume, cos1},
	{"tan", Mon, Nume, tan1},
	{"atan",Mon, Nume, atn1},
	{"atan",Dya, Nume, atn2},
	{"exp", Mon, Nume, exp1},
	{"log", Mon, Nume, log1},
	{"log", Dya, Nume, log2},

	{"keys", Mon, Other, keys},
	{"th'of",Dya, Other, th_of},
	{"min",  Mon, Other, min1},
	{"min",  Dya, Other, min2},
	{"max",  Mon, Other, max1},
	{"max",  Dya, Other, max2},

#ifdef EXT_COMMAND
	/* Extended group: */

	{"get'char", Zer, Other, e_getchar, Yes},
	{"screen'size", Zer, Other, e_screensize, Yes},
#endif

	{"", Dya, Other, NULL} /*sentinel*/
};

Visible Procedure initfpr() {
	struct funtab *fp; value r, f, pname;
	extern bool extcmds; /* Flag set by -E option */
	for (fp= funtab; *(fp->f_name) != '\0'; ++fp) {
#ifdef EXT_COMMAND
		if (fp->f_extended && !extcmds) continue;
#endif
		/* Define function */
		r= mk_text(fp->f_name);
		f= mk_fun(fp->f_adic, (intlet) (fp-funtab), NilTree, Yes);
		pname= permkey(r, fp->f_adic);
		def_unit(pname, f);
		release(f); release(r); release(pname);
	}

	defprd("in", Dya, In);
	defprd("not'in", Dya, Not_in);
#ifdef EXT_COMMAND
	if (extcmds) defprd("char'ready", Zer, Char_ready);
#endif
}

Hidden Procedure defprd(repr, adic, pre) string repr; literal adic; intlet pre; {
	value r= mk_text(repr), p= mk_prd(adic, pre, NilTree, Yes), pname;
	pname= permkey(r, adic);
	def_unit(pname, p);
	release(p); release(r); release(pname);
}

/* returns if a given test/yield exists *without faults* */
Hidden bool is_funprd(t, f, adicity, func) value t, *f; literal adicity; bool func; {
	value *aa;
	if (!is_unit(t, adicity, &aa)) return No;
	if (still_ok) {
		if (func) {
			if (!Is_function(*aa)) return No;
		} else {
			if (!Is_predicate(*aa)) return No;
		}
		*f= *aa; return Yes;
	} else return No;
}

Visible bool is_zerfun(t, f) value t, *f; {
	return is_funprd(t, f, Zer, Yes);
}

Visible bool is_monfun(t, f) value t, *f; {
	return is_funprd(t, f, Mon, Yes);
}

Visible bool is_dyafun(t, f) value t, *f; {
	return is_funprd(t, f, Dya, Yes);
}

Visible bool is_zerprd(t, p) value t, *p; {
	return is_funprd(t, p, Zer, No);
}

Visible bool is_monprd(t, p) value t, *p; {
	return is_funprd(t, p, Mon, No);
}

Visible bool is_dyaprd(t, p) value t, *p; {
	return is_funprd(t, p, Dya, No);
}

Visible value pre_fun(nd1, pre, nd2) value nd1, nd2; intlet pre; {
	struct funtab *fp= &funtab[pre]; literal adic= fp->f_adic;
	if (fp->f_kind == Nume && adic != Zer) { /* check types */
		if (adic == Dya && !Is_number(nd1)) {
			error3(MESSMAKE(fp->f_name), Vnil,
				MESS(4500, " has a non-numeric left operand"));
			return Vnil;
		} else if (!Is_number(nd2)) {
			error3(MESSMAKE(fp->f_name), Vnil,
				MESS(4501, " has a non-numeric right operand"));
			return Vnil;
		}
	}
	switch (adic) {
		case Zer: return((*fp->f_fun)());
		case Mon: return((*fp->f_fun)(nd2));
		case Dya: return((*fp->f_fun)(nd1, nd2));
		default: syserr(MESS(3300, "pre-defined fpr wrong"));
			 /*NOTREACHED*/
	}
}

Visible outcome pre_prop(nd1, pre, nd2) value nd1, nd2; intlet pre; {
	switch (pre) {
	case In: return in(nd1, nd2);
	case Not_in: return !in(nd1, nd2);
#ifdef EXT_COMMAND
	case Char_ready: return e_ch_ready();
#endif
	default:
		syserr(MESS(3301, "predicate not covered by proposition"));
		/*NOTREACHED*/
	}
}
