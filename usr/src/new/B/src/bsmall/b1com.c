/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b1com.c,v 1.1 84/06/28 00:49:03 timo Exp $ */

/************************************************************************/
/* B compounds                                                          */
/* plus Hows Funs and other odd types that don't fit anywhere else      */
/*                                                                      */
/* A compound is modelled as a sequence of len values, its fields.      */
/*                                                                      */
/************************************************************************/

#include "b.h"
#include "b1obj.h"

Visible value* field(v, k) value v; intlet k; { /* k in {0..size-1}; no copy */
	return (value *) Field(v, k);
}

Visible Procedure put_in_field(v, c, i) value v, *c; intlet i; {
	/*Note that no copy of v is made: the caller must do this*/
	*(Ats(*c)+i)= v;
}

/* Other types */
Visible loc mk_simploc(id, en) basidf id; env en; {
	loc l= grab_sim();
	(*Ats(l))= copy(id); (*(Ats(l)+1))= (value) en;
	return l;
}

Visible loc mk_trimloc(R, B, C) loc R; intlet B, C; {
	loc l= grab_tri(); trimloc *ll= (trimloc *)Ats(l);
	ll->R= copy(R); ll->B= B; ll->C= C;
	return l;
}

Visible loc mk_tbseloc(R, K) loc R; value K; {
	loc l= grab_tse(); tbseloc *ll= (tbseloc *)Ats(l);
	ll->R= copy(R); ll->K= copy(K);
	return l;
}

Visible fun mk_fun(L, H, adic, def, fux, lux, reftab, filed)
 intlet L, H; literal adic, def; txptr fux, lux; value reftab; bool filed; {
	fun f= grab_fun(); funprd *ff= (funprd *)Ats(f);
	ff->L= L; ff->H= H; ff->adic= adic; ff->def= def; ff->fux= fux;
	ff->lux= lux; ff->reftab= reftab; ff->filed= filed;
	return f;
}

Visible prd mk_prd(adic, def, fux, lux, reftab, filed)
 literal adic, def; txptr fux, lux; value reftab; bool filed; {
	prd p= grab_prd(); funprd *pp= (funprd *)Ats(p);
	pp->adic= adic; pp->def= def; pp->fux= fux;
	pp->lux= lux; pp->reftab= reftab; pp->filed= filed;
	return p;
}

Visible value mk_how(fux, lux, reftab, filed)
 txptr fux, lux; value reftab; bool filed; {
	value h= grab_how(); how *hh= (how *)Ats(h);
	hh->fux= fux; hh->lux= lux; hh->reftab= reftab; hh->filed= filed;
	return h;
}

Visible value mk_ref(rp, rlino) txptr rp; intlet rlino; {
	value r= grab_ref();
	((ref *)Ats(r))->rp= rp;
	((ref *)Ats(r))->rlino= rlino;
	return r;
}

Visible value mk_per(v) value v; {
	value p= grab_per();
	*Ats(p)= copy(v);
	return p;
}
