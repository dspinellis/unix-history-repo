/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tcP.c,v 1.4 85/08/22 16:57:02 timo Exp $
*/

/* polytype representation */

#include "b.h"
#include "b1obj.h"
#include "b2tcP.h"

/* A polytype is a compound with two fields.
 * The first field is a B text, and holds the typekind.
 * If the typekind is 'Variable', the second field is 
 *   a B text, holding the identifier of the variable;
 * otherwise, the second field is a compound of sub(poly)types,
 *   indexed from 0 to one less then the number of subtypes.
 */

#define Kin	0
#define Sub	1
#define Id	Sub
#define Asc	0
#define Key	1

#define Kind(u)		((typekind) *Field((value) (u), Kin))
#define Psubtypes(u)	(Field((value) (u), Sub))
#define Ident(u)	(*Field((value) (u), Id))

typekind var_kind;
typekind num_kind;
typekind tex_kind;
typekind lis_kind;
typekind tab_kind;
typekind com_kind;
typekind t_n_kind;
typekind l_t_kind;
typekind tlt_kind;
typekind err_kind;

polytype num_type;
polytype tex_type;
polytype err_type;
polytype t_n_type;

/* Making, setting and accessing (the fields of) polytypes */

Visible polytype mkt_polytype(k, nsub) typekind k; intlet nsub; {
	value u;
	
	u = mk_compound(2);
	*Field(u, Kin)= copy((value) k);
	*Field(u, Sub)= mk_compound(nsub);
	return ((polytype) u);
}

Procedure putsubtype(sub, u, isub) polytype sub, u; intlet isub; {
	*Field(*Psubtypes(u), isub)= (value) sub;
}

typekind kind(u) polytype u; {
	return (Kind(u));
}

intlet nsubtypes(u) polytype u; {
	return (Nfields(*Psubtypes(u)));
}

polytype subtype(u, i) polytype u; intlet i; {
	return ((polytype) *Field(*Psubtypes(u), i));
}

polytype asctype(u) polytype u; {
	return (subtype(u, Asc));
}

polytype keytype(u) polytype u; {
	return (subtype(u, Key));
}

value ident(u) polytype u; {
	return (Ident(u));
}

/* making new polytypes */

polytype mkt_number() {
	return(p_copy(num_type));
}

polytype mkt_text() {
	return(p_copy(tex_type));
}

polytype mkt_tn() {
	return(p_copy(t_n_type));
}

polytype mkt_error() {
	return(p_copy(err_type));
}

polytype mkt_list(s) polytype s; {
	polytype u;
	
	u = mkt_polytype(lis_kind, 1);
	putsubtype(s, u, Asc);
	return (u);
}

polytype mkt_table(k, a) polytype k, a; {
	polytype u;
	
	u = mkt_polytype(tab_kind, 2);
	putsubtype(a, u, Asc);
	putsubtype(k, u, Key);
	return (u);
}

polytype mkt_lt(s) polytype s; {
	polytype u;
	
	u = mkt_polytype(l_t_kind, 1);
	putsubtype(s, u, Asc);
	return (u);
}

polytype mkt_tlt(s) polytype s; {
	polytype u;
	
	u = mkt_polytype(tlt_kind, 1);
	putsubtype(s, u, Asc);
	return (u);
}

polytype mkt_compound(nsub) intlet nsub; {
	return mkt_polytype(com_kind, nsub);
}

polytype mkt_var(id) value id; {
	polytype u;
	
	u = mk_compound(2);
	*Field(u, Kin)= copy((value) var_kind);
	*Field(u, Id)= id;
	return (u);
}

Hidden value nnewvar;

polytype mkt_newvar() {
	value v;
	v = sum(nnewvar, one);
	release(nnewvar);
	nnewvar = v;
	return mkt_var(convert(nnewvar, No, No));
}

polytype p_copy(u) polytype u; {
	return((polytype) copy((polytype) u));
}

Procedure p_release(u) polytype u; {
	release((polytype) u);
}

/* predicates */

bool are_same_types(u, v) polytype u, v; {
	if (compare((value) Kind(u), (value) Kind(v)) NE 0)
		return (No);
	else if (t_is_var(Kind(u)))
		return (compare(Ident(u), Ident(v)) EQ 0);
	else
		return (
			(nsubtypes(u) EQ nsubtypes(v))
			&&
			(compare(*Psubtypes(u), *Psubtypes(v)) EQ 0)
		);
}

bool have_same_structure(u, v) polytype u, v; {
	return(
		(compare((value) Kind(u), (value) Kind(v)) EQ 0)
		&&
		nsubtypes(u) EQ nsubtypes(v)
	);
}

bool t_is_number(kind) typekind kind; {
	return (compare((value) kind, (value) num_kind) EQ 0 ? Yes : No);
}

bool t_is_text(kind) typekind kind; {
	return (compare((value) kind, (value) tex_kind) EQ 0 ? Yes : No);
}

bool t_is_tn(kind) typekind kind; {
	return (compare((value) kind, (value) t_n_kind) EQ 0 ? Yes : No);
}

bool t_is_error(kind) typekind kind; {
	return (compare((value) kind, (value) err_kind) EQ 0 ? Yes : No);
}

bool t_is_list(kind) typekind kind; {
	return (compare((value) kind, (value) lis_kind) EQ 0 ? Yes : No);
}

bool t_is_table(kind) typekind kind; {
	return (compare((value) kind, (value) tab_kind) EQ 0 ? Yes : No);
}

bool t_is_lt(kind) typekind kind; {
	return (compare((value) kind, (value) l_t_kind) EQ 0 ? Yes : No);
}

bool t_is_tlt(kind) typekind kind; {
	return (compare((value) kind, (value) tlt_kind) EQ 0 ? Yes : No);
}

bool t_is_compound(kind) typekind kind; {
	return (compare((value) kind, (value) com_kind) EQ 0 ? Yes : No);
}

bool t_is_var(kind) typekind kind; {
	return (compare((value) kind, (value) var_kind) EQ 0 ? Yes : No);
}

bool has_number(kind) typekind kind; {
	if (compare(kind, num_kind) EQ 0 || compare(kind, t_n_kind) EQ 0)
		return (Yes);
	else
		return (No);
}

bool has_text(kind) typekind kind; {
	if (compare(kind, tex_kind) EQ 0 || compare(kind, t_n_kind) EQ 0)
		return (Yes);
	else
		return (No);
}

bool has_lt(kind) typekind kind; {
	if (compare(kind, l_t_kind) EQ 0 || compare(kind, tlt_kind) EQ 0)
		return (Yes);
	else
		return (No);
}

/* The table "typeof" maps the identifiers of the variables (B texts)
 * to polytypes.
 */
 
value typeof;

Procedure repl_type_of(u, p) polytype u, p; {
	replace((value) p, &typeof, Ident(u));
}

bool table_has_type_of(u) polytype u; {
	return(in_keys(Ident(u), typeof));
}

polytype type_of(u) polytype u; {
	return((polytype) *adrassoc(typeof, Ident(u)));
}

polytype bottom_var(u) polytype u; {
	polytype b;

	if (!t_is_var(Kind(u)))
		return (u);
	/* Kind(u) == Variable */
	while (table_has_type_of(u)) {
		b = type_of(u);
		if (t_is_var(Kind(b)))
			u = b;
		else
			break;
	}
	/* Kind(u) == Variable && !table_has_type_of(u)*/
	return (u);
}

Visible Procedure usetypetable(t) value t; {
	typeof = t;
}

Visible Procedure deltypetable() {
	release(typeof);
}

/* init */

Visible Procedure initpol() {
	num_kind = mk_text("Number");
	num_type = mkt_polytype(num_kind, 0);
	tex_kind = mk_text("Text");
	tex_type = mkt_polytype(tex_kind, 0);
	t_n_kind = mk_text("TN");
	t_n_type = mkt_polytype(t_n_kind, 0);
	err_kind = mk_text("Error");
	err_type = mkt_polytype(err_kind, 0);
	
	lis_kind = mk_text("List");
	tab_kind = mk_text("Table");
	com_kind = mk_text("Compound");
	l_t_kind = mk_text("LT");
	tlt_kind = mk_text("TLT");
	var_kind = mk_text("Variable");
	
	nnewvar = zero;
}
