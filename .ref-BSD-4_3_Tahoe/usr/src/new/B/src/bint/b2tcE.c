/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tcE.c,v 1.4 85/08/22 16:56:55 timo Exp $
*/

/* process type unification errors */

#include "b.h"
#include "b1obj.h"
#include "b2tcP.h"
#include "b2tcE.h"
#include "b2tcU.h"

/* 
 * The variables from the users line are inserted in var_list.
 * This is used to produce the right variable names
 * in the error message.
 * Call start_vars() when a new error context is established
 * with the setting of curline.
 */

Hidden value var_list;

Visible Procedure start_vars() {
	var_list = mk_elt();
}

Visible Procedure add_var(tvar) polytype tvar; {
	insert(tvar, &var_list);
}

Hidden bool in_vars(t) polytype t; {
	return in(t, var_list);
}

Visible Procedure end_vars() {
	release(var_list);
}

/* t_repr(u) is used to print polytypes when an error
 * has occurred.
 * Because the errors are printed AFTER unification, the variable 
 * polytypes in question have changed to the error-type.
 * To print the real types in error, the table has to be 
 * saved in reprtable.
 * The routines are called in unify().
 */

Hidden value reprtable;
extern value typeof; 		/* defined in b2tcP.c */

Visible Procedure setreprtable() {
	reprtable = copy(typeof);
}

Visible Procedure delreprtable() {
	release(reprtable);
}

/* miscellaneous procs */

Hidden value conc(v, w) value v, w; {
	value c;
	c = concat(v, w);
	release(v); release(w);
	return c;
}

Hidden bool newvar(u) polytype u; {
	value u1;
	char ch;
	u1 = curtail(ident(u), one);
	ch = charval(u1);
	release(u1);
	return (bool) ('0' <= ch && ch <= '9');
}

#define Known(tu) (!t_is_var(kind(tu)) && !t_is_error(kind(tu)))

Hidden bool knowntype(u) polytype u; {
	value tu;
	tu = u;
	while (t_is_var(kind(tu)) && in_keys(ident(tu), reprtable))
		tu = *adrassoc(reprtable, ident(tu));
	return Known(tu);
}

Hidden bool outervar = Yes;

Hidden value t_repr(u) polytype u; {
	typekind u_kind;
	value c;
	
	u_kind = kind(u);
	if (t_is_number(u_kind)) {
		return mk_text("0");
	}
	else if (t_is_text(u_kind)) {
		return mk_text("''");
	}
	else if (t_is_tn(u_kind)) {
		return mk_text("'' or 0");
	}
	else if (t_is_compound(u_kind)) {
		intlet k, len = nsubtypes(u);
		c = mk_text("(");
		for (k = 0; k < len - 1; k++) {
			c = conc(c, t_repr(subtype(u, k)));
			c = conc(c, mk_text(", "));
		}
		c = conc(c, t_repr(subtype(u, k)));
		return conc(c, mk_text(")"));
	}
	else if (t_is_error(u_kind)) {
		return mk_text(" ");
	}
	else if (t_is_var(u_kind)) {
		value tu;
		tu = u;
		while (t_is_var(kind(tu)) && in_keys(ident(tu), reprtable))
			tu = *adrassoc(reprtable, ident(tu));
		if (in_vars(u)) {
			if (Known(tu)) {
				if (outervar) {
					outervar = No;
					c = conc(t_repr(tu), mk_text(" for "));
					outervar = Yes;
					return conc(c, copy(ident(u)));
				}
				else
					return t_repr(tu);
			}
			else {
				return copy(ident(u));
			}
		}
		else if (Known(tu))
			return t_repr(tu);
		else if (newvar(u))
			return mk_text(" ");
		else
			return copy(ident(u));
	}
	else if (t_is_table(u_kind)) {
		if (knowntype(keytype(u))) {
			if (knowntype(asctype(u))) {
				c = conc(mk_text("{["),
					t_repr(keytype(u)));
				c = conc(c, mk_text("]:"));
				c = conc(c, t_repr(asctype(u)));
				return conc(c, mk_text("}"));
			}
			else {
				c = conc(mk_text("table with type "),
					t_repr(keytype(u)));
				return conc(c, mk_text(" keys"));
			}
		}
		else if (knowntype(asctype(u))) {
			c = conc(mk_text("table with type "),
				t_repr(asctype(u)));
			return conc(c, mk_text(" associates"));
		}
		else {
			return mk_text("table");
		}
	}
	else if (t_is_list(u_kind)) {
		if (knowntype(asctype(u))) {
			c = conc(mk_text("{"), t_repr(asctype(u)));
			return conc(c, mk_text("}"));
		}
		else {
			return mk_text("list");
		}
	}
	else if (t_is_lt(u_kind)) {
		if (knowntype(asctype(u)))
			return conc(mk_text("list or table of "),
				    t_repr(asctype(u)));
		else
			return mk_text("{}");
	}
	else if (t_is_tlt(u_kind)) {
		if (knowntype(asctype(u)))
			return conc(mk_text("text list or table of "),
				    t_repr(asctype(u)));
		else
			return mk_text("text list or table");
	}
	else {
		syserr(MESS(4300, "unknown polytype in t_repr"));
		return mk_text("***");
	}
}

/* now, the real error messages */

Visible Procedure badtyperr(a, b) polytype a, b; {
	value t;

/*error4("incompatible types: ", ta, ", and ", tb); */

	t = conc(t_repr(a), mk_text(" and "));
	t = conc(t, t_repr(b));
	error2(MESS(4301, "incompatible types "), t);
	release(t);
}

Visible Procedure cyctyperr(a) polytype a; {
	value vcyc;
	
	vcyc = Vnil;
	if (in_vars(a))
		vcyc = ident(a);
	else {
		value n, m, nvars, v;
		n = copy(one);
		nvars = size(var_list);
		while (compare(n, nvars) <= 0) {
			v = th_of(n, var_list);
			if (equal_vars(v, a) || contains(v, a)) {
				vcyc = ident(v);
				break;
			}
			m = n;
			n = sum(n, one);
			release(m); release(v);
		}
		release(n); release(nvars);
		if (vcyc EQ Vnil) {
			error2(MESS(4302, "unknown cyclic type"), ident(a));
			syserr(MESS(4303, "unknown cyclic type"));
			return;
		}
	}
	error3(MESS(4304, "(sub)type of "), vcyc,
		MESS(4305, " contains itself"));
}
