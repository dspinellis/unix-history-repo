/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1tab.c,v 1.1 84/06/28 00:48:58 timo Exp $ */

/* B tables */
#include "b.h"
#include "b1obj.h"
#include "B1tlt.h"

Visible value* key(v, k) value v; intlet k; { /* k in {0..size-1}; no copy */
	return Key(v, k);
}

Visible value* assoc(v, k) value v; intlet k; { /* k in {0..size-1}; no copy */
	return Assoc(v, k);
}

Visible value keys(ta) value ta; {
	value li= grab_lis(Length(ta)), *le, *te= (value *)Ats(ta);
	int k, len= Length(ta);
	if(!Is_table(ta)) error("in keys t, t is not a table");
	le= (value *)Ats(li);
	Overall { *le++= copy(Cts(*te++)); }
	return li;
}

Visible value key_elem(t, i) value t; intlet i; { /*The key of the i-th entry*/
	return *Key(t, i);
}

/* adrassoc returns a pointer to the associate, rather than
   the associate itself, so that the caller can decide if a copy
   should be taken or not. If the key is not found, Pnil is returned. */
Visible value* adrassoc(t, ke) value t, ke; {
	intlet where;
	if (t->type != Tab && t->type != ELT) error("selection on non-table");
	return found(key_elem, t, ke, &where) ? Assoc(t, where) : Pnil;
}

Visible Procedure uniq_assoc(ta, ke) value ta, ke; {
	intlet k;
	if (found(key_elem, ta, ke, &k)) {
		uniql(Ats(ta)+k);
		uniql(Assoc(ta,k));
	} else syserr("uniq_assoc called for non-existent table entry");
}

Visible Procedure replace(v, ta, ke) value *ta, ke, v; {
	intlet len= Length(*ta); value *tp, *tq;
	intlet k, kk;
	uniql(ta);
	if ((*ta)->type == ELT) (*ta)->type = Tab;
	else if ((*ta)->type != Tab) error("replacing in non-table");
	if (found(key_elem, *ta, ke, &k)) {
		value *a;
		uniql(Ats(*ta)+k);
		a= Assoc(*ta, k);
		uniql(a);
		release(*a);
		*a= copy(v);
		return;
	} else {
		xtndlt(ta, 1);
		tq= Ats(*ta)+len; tp= tq-1;
		for (kk= len; kk > k; kk--) *tq--= *tp--;
		*tq= grab_com(2);
		Cts(*tq)= copy(ke);
		Dts(*tq)= copy(v);
	}
}

Visible bool in_keys(ke, tl) value ke, tl; {
	intlet dummy;
	if (tl->type == ELT) return No;
	if (tl->type != Tab) syserr("in_keys applied to non-table");
	return found(key_elem, tl, ke, &dummy);
}

Visible Procedure delete(tl, ke) value *tl, ke; {
	intlet len, k; value *tp;
	if ((*tl)->type == ELT) syserr("deleting table entry from empty table");
	if ((*tl)->type != Tab) syserr("deleting table entry from non-table");
	tp= Ats(*tl); len= Length(*tl);
	if (!found(key_elem, *tl, ke, &k))
		syserr("deleting non-existent table entry");
	if (Unique(*tl)) {
		release(*(tp+=k));
		for (k= k; k < len; k++) {*tp= *(tp+1); tp++;}
		xtndlt(tl, -1);
	} else {
		intlet kk; value *tq= Ats(*tl);
		release(*tl);
		*tl= grab_tab(--len);
		tp= Ats(*tl);
		for (kk= 0; kk < len; kk++) {
			*tp++= copy (*tq++);
			if (kk == k) tq++;
		}
	}
}

