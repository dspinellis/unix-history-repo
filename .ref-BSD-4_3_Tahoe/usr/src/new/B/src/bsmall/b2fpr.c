/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2fpr.c,v 1.1 84/06/28 00:49:12 timo Exp $ */

/* B formula/predicate invocation */
#include "b.h"
#include "b1obj.h"
#include "b2fil.h"
#include "b2env.h"
#include "b2sem.h"
#include "b2syn.h"
#include "b2sou.h"

#define Other 0
#define Nume 1

#define In ('[')
#define Not_in (']')

/*
 * Table defining all predefined functions (not propositions).
 */

struct funtab {
	char	*f_name;
	char	f_lopri, f_hipri;
	char	f_adic;
	char	f_flag;
	value	(*f_fun)();
} funtab[] = {
	{"~",  8, 8, Mon, Nume, approximate},
	{"+",  8, 8, Mon, Nume, copy},
	{"+",  2, 2, Dya, Nume, sum},
	{"-",  5, 5, Mon, Nume, negated},
	{"-",  2, 2, Dya, Nume, diff},
	{"*/", 1, 8, Mon, Nume, numerator},
	{"/*", 1, 8, Mon, Nume, denominator},

	{"*",  4, 4, Dya, Nume, prod},
	{"/",  3, 4, Dya, Nume, quot},
	{"**", 6, 7, Dya, Nume, power},

	{"^",  2, 2, Dya, Other, concat},
	{"^^", 1, 8, Dya, Other, repeat},
	{"<<", 1, 8, Dya, Other, adjleft},
	{"><", 1, 8, Dya, Other, centre},
	{">>", 1, 8, Dya, Other, adjright},

	{"#",  7, 7, Mon, Other, size},
	{"#",  7, 8, Dya, Other, size2},

	{"pi", 8, 8, Zer, Other, pi},
	{"e",  8, 8, Zer, Other, e},

	{"abs",    1, 8, Mon, Nume, absval},
	{"sign",   1, 8, Mon, Nume, signum},
	{"floor",  1, 8, Mon, Nume, floorf},
	{"ceiling",1, 8, Mon, Nume, ceilf},
	{"round",  1, 8, Mon, Nume, round1},
	{"round",  1, 8, Dya, Nume, round2},
	{"mod",    1, 8, Dya, Nume, mod},
	{"root",   1, 8, Mon, Nume, root1},
	{"root",   1, 8, Dya, Nume, root2},

	{"sin", 1, 8, Mon, Nume, sin1},
	{"cos", 1, 8, Mon, Nume, cos1},
	{"tan", 1, 8, Mon, Nume, tan1},
	{"atan",1, 8, Mon, Nume, atn1},
	{"atan",1, 8, Dya, Other, atn2},
	{"exp", 1, 8, Mon, Nume, exp1},
	{"log", 1, 8, Mon, Nume, log1},
	{"log", 1, 8, Dya, Other, log2},

	{"keys", 1, 8, Mon, Other, keys},
	{"th'of",1, 8, Dya, Other, th_of},
	{"min",  1, 8, Mon, Other, min1},
	{"min",  1, 8, Dya, Other, min2},
	{"max",  1, 8, Mon, Other, max1},
	{"max",  1, 8, Dya, Other, max2},

	{"", 0, 0, Dya, Other, NULL} /*sentinel*/
};

Visible Procedure initfprs() {
	struct funtab *fp; value r, f;
	for (fp = funtab; fp->f_lopri != 0; ++fp) {
		/* Define function */
		r= mk_text(fp->f_name);
		f= mk_fun(fp->f_lopri, fp->f_hipri, fp->f_adic,
			Pre, (txptr)(fp-funtab), /*NON-PORTABLE: remove the cast*/
			(txptr)Dummy, (value)Dummy, (bool)Dummy);
		def_unit(f, r, fp->f_adic == Zer ? FZR
			      :fp->f_adic == Mon ? FMN : FDY);
		release(f); release(r);
	}

	defprd("in", Dya, Pre, In);
	defprd("not'in", Dya, Pre, Not_in);
}

Hidden Procedure defprd(repr, adic, def, fux) string repr; literal adic, def, fux; {
	literal ad= adic == Zer ? FZR : adic == Mon ? FMN : FDY;
	value r= mk_text(repr), p= mk_prd(adic, def, (txptr) fux /*nasty*/, (txptr)Dummy, (value)Dummy, (bool)Dummy);
	def_unit(p, r, ad);
	release(p); release(r);
}

Hidden bool is_funprd(t, f, adicity, func) value t, *f; literal adicity; bool func; {
	value *aa, *sl= lookup(t);
	if (sl != Pnil) return No;
	if (!is_unit(t, adicity, &aa)) return No;
	if (func) {
		if (!Is_function(*aa)) return No;
	} else {
		if (!Is_predicate(*aa)) return No;
	}
	*f= *aa; return Yes;
}

Visible bool is_zerfun(t, f) value t, *f; {
	return is_funprd(t, f, FZR, Yes);
}

Visible bool is_monfun(t, f) value t, *f; {
	return is_funprd(t, f, FMN, Yes);
}

Visible bool is_dyafun(t, f) value t, *f; {
	return is_funprd(t, f, FDY, Yes);
}

Visible bool is_zerprd(t, p) value t, *p; {
	return is_funprd(t, p, FZR, No);
}

Visible bool is_monprd(t, p) value t, *p; {
	return is_funprd(t, p, FMN, No);
}

Visible bool is_dyaprd(t, p) value t, *p; {
	return is_funprd(t, p, FDY, No);
}

char torbuf[3];
#define Tor *tb++= Char(tx++)
#define Rot *tb= '\0'

Visible value montor() {
	txptr tb= torbuf; value r, f;
	switch (Char(tx)) {
	case '~': Tor; break;
	case '+': Tor; break;
	case '-': Tor; break;
	case '*': Tor;
		if (Char(tx) != '/') pprerr("function * is not monadic", "");
		Tor; break;
	case '/': Tor;
		if (Char(tx) != '*') pprerr("function / is not monadic", "");
		Tor; break;
	case '#': Tor; break;
	default:  syserr("unhandled Montormark");
	}
	Rot;
	r= mk_text(torbuf);
	f= unit_info(r, FMN);
	release(r);
	return f;
}

Visible value dyator() {
	txptr tb= torbuf; value r, f;
	switch (Char(tx)) {
	case '+': Tor; break;
	case '-': Tor; break;
	case '*': Tor;
		{txptr tx0= tx;
		loop:	if (Char(tx++) != '*') {tx= tx0; break;}
			if (Char(tx++) != '/') {tx= tx0; Tor; break;}
			goto loop;
		}
	case '/': Tor; break;
	case '^': Tor; if (Char(tx) == '^') Tor; break;
	case '<': Tor;
		if (Char(tx) != '<') pprerr("order-relator instead of function", "");
		Tor; break;
	case '>': Tor;
		if (Char(tx) != '<' && Char(tx) != '>')
			pprerr("order-relator instead of function", "");
		Tor; break;
	case '#': Tor; break;
	default:  syserr("unhandled Dyatormark");
	}
	Rot;
	r= mk_text(torbuf);
	f= unit_info(r, FDY);
	release(r);
	return f;
}

Visible value formula(nd1, tor, nd2) value nd1, tor, nd2; {
	funprd *t;
	struct funtab *fp;
	if (!Is_function(tor)) syserr("formula called with non-function");
	if (!xeq) return (value) Dummy;
	t= Funprd(tor);
	if (!(t->adic==Zer ? nd2==Vnil : (t->adic==Mon) == (nd1==Vnil)))
		syserr("invoked formula has other adicity than invoker");
	if (t->def == Use) {
		value r;
		udfpr(nd1, t, nd2, Ret);
		r= resval; resval= Vnil;
		return r;
	}
	fp= &funtab[(int)(t->fux)];
	if (fp->f_flag == Nume && t->adic != Zer) { /* check types */
		if (t->adic == Dya && !Is_number(nd1)) {
			error("left operand not a number");
			return Vnil;
		} else if (!Is_number(nd2)) {
			error("right operand not a number");
			return Vnil;
		}
	}
	if (t->adic == Zer) return((*fp->f_fun)());
	else if (fp->f_adic == Mon) return((*fp->f_fun)(nd2));
	else return((*fp->f_fun)(nd1, nd2));
}

Visible outcome proposition(nd1, pred, nd2) value nd1, pred, nd2; {
	funprd *p;
	if (!Is_predicate(pred)) syserr("proposition called with non-predicate");
	if (!xeq) return (outcome) Dummy;
	p= Funprd(pred);
	if (!(p->adic==Zer ? nd2==Vnil : (p->adic==Mon) == (nd1==Vnil)))
		syserr("invoked proposition has other adicity than invoker");
	if (p->def == Use) {
		outcome o;
		udfpr(nd1, p, nd2, Rep);
		o= resout; resout= Und;
		return o;
	}
	switch (p->fux) {
	case In:
		return in(nd1, nd2);
	case Not_in:
		return !in(nd1, nd2);
	default:
		syserr("predicate not covered by proposition");
		return (outcome) Dummy;
	}
}
