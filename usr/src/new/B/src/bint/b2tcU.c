/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tcU.c,v 1.4 85/08/22 16:57:11 timo Exp $
*/

/* unification of polytypes */

#include "b.h"
#include "b1obj.h"
#include "b2tcP.h"
#include "b2tcU.h"
#include "b2tcE.h"

Hidden bool bad;
Hidden bool cycling;
Hidden bool badcycle;

Visible Procedure unify(a, b, pu)
polytype a, b, *pu;
{
	bad = No;
	cycling = No;
	setreprtable();
	u_unify(a, b, pu);
	if (bad) badtyperr(a, b);
	delreprtable();
}

Hidden Procedure u_unify(a, b, pu)
polytype a, b, *pu;
{
	typekind a_kind, b_kind;
	polytype res;
	
	a_kind = kind(a);
	b_kind = kind(b);
	
	if (are_same_types(a, b)) {
		*pu = p_copy(a);
	}
	else if (t_is_var(a_kind) || t_is_var(b_kind)) {
		substitute_for(a, b, pu);
	}
	else if (have_same_structure(a, b)) {
		unify_subtypes(a, b, pu);
	}
	else if (has_number(a_kind) && has_number(b_kind)) {
		*pu = mkt_number();
	}
	else if (has_text(a_kind) && has_text(b_kind)) {
		*pu = mkt_text();
	}
	else if (has_text(a_kind) && t_is_tlt(b_kind)) {
		u_unify(asctype(b), (res = mkt_text()), pu);
		p_release(res);
	}
	else if (has_text(b_kind) && t_is_tlt(a_kind)) {
		u_unify(asctype(a), (res = mkt_text()), pu);
		p_release(res);
	}
	else if ((t_is_list(a_kind) && has_lt(b_kind))
		 ||
		 (t_is_list(b_kind) && has_lt(a_kind))
	)
	{
		u_unify(asctype(a), asctype(b), &res);
		*pu = mkt_list(res);
	}
	else if (t_is_table(a_kind) && has_lt(b_kind)) {
		u_unify(asctype(a), asctype(b), &res);
		*pu = mkt_table(p_copy(keytype(a)), res);
	}
	else if (t_is_table(b_kind) && has_lt(a_kind)) {
		u_unify(asctype(a), asctype(b), &res);
		*pu = mkt_table(p_copy(keytype(b)), res);
	}
	else if ((t_is_tlt(a_kind) && t_is_lt(b_kind))
		 || 
		 (t_is_lt(a_kind) && t_is_tlt(b_kind)))
	{
		u_unify(asctype(a), asctype(b), &res);
		*pu = mkt_lt(res);
	}
	else if (t_is_error(a_kind) || t_is_error(b_kind)) {
		*pu = mkt_error();
	}
	else {
		*pu = mkt_error();
		if (cycling)
			badcycle = Yes;
		else
			bad = Yes;
	}
}

Hidden Procedure unify_subtypes(a, b, pu)
polytype a, b, *pu;
{
	polytype sa, sb, s;
	intlet nsub, is;
	
	nsub = nsubtypes(a);
	*pu = mkt_polytype(kind(a), nsub);
	for (is = 0; is < nsub; is++) {
		sa = subtype(a, is);
		sb = subtype(b, is);
		u_unify(sa, sb, &s);
		putsubtype(s, *pu, is);
	}
}

Forward bool contains();
Forward bool equal_vars();

Hidden Procedure substitute_for(a, b, pu)
polytype a, b, *pu;
{
	typekind a_kind, b_kind;
	polytype ta, tb;
	bool ta_is_a, tb_is_b;
	
	a_kind = kind(a);
	b_kind = kind(b);
	
	if (t_is_var(a_kind) && table_has_type_of(a)) {
		ta = type_of(a);
		ta_is_a = No;
	}
	else {
		ta = a;
		ta_is_a = Yes;
	}
	if (t_is_var(b_kind) && table_has_type_of(b)) {
		tb = type_of(b);
		tb_is_b = No;
	}
	else {
		tb = b;
		tb_is_b = Yes;
	}
	
	if (!(ta_is_a && tb_is_b))
		u_unify(ta, tb, pu);
	else if (!t_is_var(a_kind))
		*pu = p_copy(a);
	else
		*pu = p_copy(b);
	
	if (t_is_var(a_kind)) {
		if (contains(*pu, bottom_var(a)))
			textify(a, pu);
	}
	if (t_is_var(b_kind)) {
		if (contains(*pu, bottom_var(b)))
			textify(b, pu);
	}
	
	if (t_is_var(a_kind) && !are_same_types(*pu, a))
		repl_type_of(a, *pu);
	if (t_is_var(b_kind) && !are_same_types(*pu, b))
		repl_type_of(b, *pu);
}

Hidden Procedure textify(a, pu)
polytype a, *pu;
{
	polytype ttext, text_hopefully;
	
	ttext = mkt_text();
	cycling = Yes;
	badcycle = No;
	u_unify(*pu, ttext, &text_hopefully);
	if (badcycle EQ No) {
		p_release(text_hopefully);
		u_unify(a, ttext, &text_hopefully);
	}
	if (badcycle EQ No) {
		*pu = ttext;
	}
	else {
		*pu = mkt_error();
		cyctyperr(a);
		p_release(ttext);
	}
	p_release(text_hopefully);
	cycling = No;
}

Visible bool contains(u, a) polytype u, a; {
	bool result;
	
	result = No;
	if (t_is_var(kind(u))) {
		if (table_has_type_of(u)) {
			result = contains(type_of(u), a);
		}
	}
	else {
		polytype s;
		intlet is, nsub;
		nsub = nsubtypes(u);
		for (is = 0; is < nsub; is++) {
			s = subtype(u, is);
			if (equal_vars(s, a) || contains(s, a)) {
				result = Yes;
				break;
			}
		}
	}
	return (result);
}

Visible bool equal_vars(s, a) polytype s, a; {
	return (are_same_types(bottom_var(s), a));
}
