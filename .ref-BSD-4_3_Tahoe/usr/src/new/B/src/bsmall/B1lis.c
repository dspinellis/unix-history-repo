/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1lis.c,v 1.1 84/06/28 00:48:55 timo Exp $ */

/* B lists */
#include "b.h"
#include "b1obj.h"
#include "B1tlt.h"
#include "b0con.h"

Visible value list_elem(l, i) value l; intlet i; {
	return List_elem(l, i);
}

Visible insert(v, ll) value v, *ll; {
	intlet len= Length(*ll); register value *lp, *lq;
	intlet k; register intlet kk;
	if (!Is_list(*ll)) error("inserting in non-list");
	VOID found(list_elem, *ll, v, &k);
	if (Unique(*ll) && !Is_ELT(*ll)) {
		xtndlt(ll, 1);
		lq= Ats(*ll)+len; lp= lq-1;
		for (kk= len; kk > k; kk--) *lq--= *lp--;
		*lq= copy(v);
	} else {
		lp= Ats(*ll);
		release(*ll);
		*ll= grab_lis(++len);
		lq= Ats(*ll);
		for (kk= 0; kk < len; kk++) *lq++= copy (kk == k ? v : *lp++);
	}
}

Visible remove(v, ll) value v; value *ll; {
	register value *lp, *lq;
	intlet len; intlet k;
	if (!Is_list(*ll)) error("removing from non-list");
	lp= Ats(*ll); len= Length(*ll);
	if (len == 0) error("removing from empty list");
	if (!found(list_elem, *ll, v, &k))
		error("removing non-existing list entry");
	/* lp[k] = v */
	if (Unique(*ll)) {
		release(*(lp+=k));
		for (k= k; k < len; k++) {*lp= *(lp+1); lp++;}
		xtndlt(ll, -1);
	} else {
		intlet kk= k;
		lq= Ats(*ll);
		release(*ll);
		*ll= grab_lis(--len);
		lp= Ats(*ll);
		Overall {
			*lp++= copy (*lq++);
			if (k == kk) lq++;
		}
	}
}

Visible value mk_numrange(a, z) value a, z; {
	value l= mk_elt(), m= copy(a), n;

	while (compare(m, z)<=0) {
		insert(m, &l);
		m= sum(n=m, one);
		release(n);
	}
	release(m);
	return l;
}

Visible value mk_charrange(av, zv) value av, zv; {
	char a= charval(av), z= charval(zv);
	value l= grab_lis((intlet) (z-a+1)); register value *ep= Ats(l);
	char m[2];
	m[1]= '\0';
	for (m[0]= a; m[0] <= z; m[0]++) {
		*ep++= mk_text(m);
	}
	return l;
}
