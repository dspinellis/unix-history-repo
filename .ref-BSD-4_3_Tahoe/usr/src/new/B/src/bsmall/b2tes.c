/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2tes.c,v 1.2 84/07/05 15:01:46 timo Exp $ */

/* B test testing */
#include "b.h"
#include "b1obj.h"
#include "b2key.h"
#include "b2env.h"
#include "b2syn.h"
#include "b2sem.h"

#define Tnil ((string) 0)

Forward outcome ttest(), stest(), comparison(), quant();

Visible outcome test(q) txptr q; {
	return ttest(q, Tnil);
}

Hidden outcome ttest(q, ti) txptr q; string ti; {
	txptr fa, ta, fo, to;
	bool a= find(AND, q, &fa, &ta), o= find(OR, q, &fo, &to);
	Skipsp(tx);
	if (ti != Tnil && (a || o))
	pprerr("use ( and ) to make AND and/or OR unambiguous after ", ti);
	if (a && o) parerr("AND and OR intermixed, use ( and )", "");
	if (atkw(NOT)) return !ttest(q, "NOT");
	else if (atkw(SOME)) return quant(q, No, No, "SOME");
	else if (atkw(EACH)) return quant(q, Yes, Yes, "EACH");
	else if (atkw(NO)) return quant(q, Yes, No, "NO");
	else if (a) {
	testa:	if (!stest(fa)) return No;
		tx= ta; if (find(AND, q, &fa, &ta)) goto testa;
		return ttest(q, Tnil);
	} else if (o) {
	testo:	if (stest(fo)) return Yes;
		tx= to; if (find(AND, q, &fo, &to)) goto testo;
		return ttest(q, Tnil);
	}
	return stest(q);
}

Hidden outcome stest(q) txptr q; {
	bool o, lt, eq, gt; txptr tx0; value v;
	Skipsp(tx); tx0= tx;
	nothing(q, "test");
	if (Char(tx) == '(') {
		txptr tx1= ++tx, f, t;
		req(")", q, &f, &t);
		tx= t; Skipsp(tx);
		if (tx < q) {tx= tx0; goto exex;}
		tx= tx1; o= test(f); tx= t;
		return o;
	}
	if (Letter(Char(tx))) {
		value t= tag(); prd p;
		Skipsp(tx);
		if (tx == q) /* test consists of tag */ {
			value *aa= lookup(t);
			if (aa != Pnil && Is_refinement(*aa)) {
				release(t);
				ref_et(*aa, Rep);
				o= resout; resout= Und;
				return o;
			} else if (is_zerprd(t, &p)) {
				release(t);
				upto(q, "zeroadic test");
				return proposition(Vnil, p, Vnil);
			} else
			pprerr(
		      "tag is neither refined-test nor zeroadic-predicate", "");
		}
		if (is_monprd(t, &p)) {
			release(t);
			t= obasexpr(q);
			upto(q, "monadic test");
			o= proposition(Vnil, p, t); release(t);
			return o;
		}
		release(t); tx= tx0;
	}
 exex:	v= obasexpr(q); Skipsp(tx);
	if (relop(&lt, &eq, &gt)) {
		value w;
	nextv:	w= obasexpr(q);
		if (!comparison(v, w, lt, eq, gt)) {
			release(v); release(w);
			return No;
		}
		release(v); v= w;
		Skipsp(tx);
		if (relop(&lt, &eq, &gt)) goto nextv;
		release(v);
		upto(q, "comparison");
		return Yes;
	}
	if (Letter(Char(tx))) {
		value t= tag(); prd p;
		if (!is_dyaprd(t, &p))
		pprerr("tag following expression is not a predicate", "");
		release(t); t= obasexpr(q);
		upto(q, "dyadic test");
		o= proposition(v, p, t);
		release(v); release(t);
		return o;
	}
	parerr("something unexpected following expression in test", "");
	return (bool) Dummy;
}

Visible bool relop(lt, eq, gt) bool *lt, *eq, *gt; {
	txptr tx0= tx;
	*lt= *eq= *gt= No;
	Skipsp(tx);
	switch (Char(tx++)) {
	case '<':
		if (Char(tx) == '<') break;
		*lt= Yes;
		if (Char(tx) == '=') {
			tx++; *eq= Yes;
		} else if (Char(tx) == '>') {
			tx++; *gt= Yes;
		}
		break;
	case '=':
		*eq= Yes; break;
	case '>':
		if (Char(tx) == '<' || Char(tx) == '>') break;
		*gt= Yes;
		if (Char(tx) == '=') {
			tx++; *eq= Yes;
		}
		break;
	default:
		break;
	}
	if (*lt || *eq || *gt) return Yes;
	tx= tx0; return No;
}

Visible outcome comparison(v, w, lt, eq, gt) value v, w; bool lt, eq, gt; {
	relation c= compare(v, w);
	return c < 0 ? lt : c == 0 ? eq : gt;
}

Hidden outcome quant(q, all, each, qt) txptr q; bool all, each; string qt; {
/* it is assumed that xeq == Yes */
	env e0= curnv; bool in= No, par= No, go_on= Yes; loc l; value v, w;
	txptr ftx, ttx, utx, vtx;
	reqkw(HAS, &utx, &vtx);
	if (vtx > q) parerr("HAS follows colon", "");
		/* as in: SOME i IN x: SHOW i HAS a */
	if (find(IN_quant, vtx, &ftx, &ttx)) in= Yes;
	if (find(PARSING, vtx, &ftx, &ttx)) par= Yes;
	if (!in && !par) parerr("neither IN nor PARSING found", "");
	if (in && par) parerr("you're kidding; both IN and PARSING", "");
	l= targ(ftx);
	if (!(Is_simploc(l) && !par || Is_compound(l)))
		pprerr("inappropriate identifier after ", qt);
	bind(l);
	tx= ttx; v= expr(utx);
	if (par) {
		if (!Is_text(v))
			error("in i1, ... , in PARSING t, t is not a text");
		part(Length(l), l, 0, v, 0, utx, vtx, &go_on, each, q, qt);
	} else {
		value k, k1, len;
		if (!Is_tlt(v))
		  error("in SOME/EACH/NO i IN t, t is not a text, list or table");
		len= size(v);
		k= copy(one);
		while (go_on && compare(k, len) <= 0) {
			tx= utx;
			w= th_of(k, v);
			put(w, l); release(w);
			tx= vtx;
			go_on= each == (ttest(q, qt) == Succ);
			k= sum(k1= k, one); release(k1);
		}
		release(k); release(len);
	}
	release(v); release(l); restore_env(e0);
	return go_on == all ? Succ : Fail;
}

Hidden part(n, l, f, v, B, utx, vtx, go_on, each, q, qt)
	intlet n; loc l, v; intlet f, B;
	txptr utx, vtx; bool *go_on, each; txptr q; string qt; {
	intlet r= length(v)-B, k; value w;
	for (k= n == 1 ? r : 0; *go_on && k <= r; k++) {
		tx= utx;
		w= trim(v, B, r-k);
		put(w, *field(l, f)); release(w);
		if (n == 1) {
			tx= vtx;
			*go_on= each == (ttest(q, qt) == Succ);
		} else part(n-1, l, f+1, v, B+k, utx, vtx, go_on, each, q, qt);
	}
}
