/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2uni.c,v 1.1 84/06/28 00:49:27 timo Exp $ */

/* B units */
#include "b.h"
#include "b1obj.h"
#include "b1mem.h" /* for ptr */
#include "b2fil.h"
#include "b2env.h"
#include "b2scr.h"
#include "b2err.h"
#include "b2key.h"
#include "b2syn.h"
#include "b2sou.h"
#include "b2sem.h"

Forward loc fopnd(), fop(), basfop();

value resval; outcome resout;
bool terminated;
value global;
value formlist, sharelist; envtab reftab;
bool forming;

Visible Procedure get_unit(filed) bool filed; {
	bool xeq0= xeq, hu= No, yu= No, tu= No;
	txptr fux= tx, lux;
	value u; literal adic;
	if ((hu= atkw(HOW_TO)) || (yu= atkw(YIELD)) || (tu= atkw(TEST))) {
		lino= 1; uname= aster;
		if (cur_ilev != 0) parerr("unit starts with indentation", "");
		cntxt= In_unit;
		Skipsp(tx);
		formlist= mk_elt();
		if (hu) {
			txptr utx, vtx; value f;
			uname= keyword(ceol); utype= FHW;
			req(":", ceol, &utx, &vtx);
			Skipsp(tx);
			while (tx < utx) {
				if (Cap(Char(tx))) goto nxt_kw;
				if (!Letter(Char(tx)))
			      parerr("no formal parameter where expected", "");
				f= tag();
				if (in(f, formlist))
				pprerr("multiple use of formal parameter", "");
				insert(f, &formlist);
				release(f);
				Skipsp(tx);
			nxt_kw:	if (tx < utx) {
					release(keyword(utx));
					Skipsp(tx);
				}
			}
			tx= vtx;
		} else {
			ytu_heading(&uname, &adic, ceol, Yes);
			utype= adic == Zer ? FZR : adic == Mon ? FMN : FDY;
		}
		xeq= No;
		sharelist= mk_elt();
		unicomm_suite();
		Mark_unit_end(tx);
		reftab= mk_elt();
		ref_suite();
		lux= tx+1;
		adjust_unit(&fux, &lux, &reftab);
		u= hu ? mk_how(fux, lux, reftab, filed) :
		   yu ? mk_fun(1, 8, adic, Use, fux, lux, reftab, filed)
		      : mk_prd(adic, Use, fux, lux, reftab, filed);
		def_unit(u, uname, utype);
		release(sharelist); release(u); release(formlist); release(uname);
		xeq= xeq0;
	} else parerr("no HOW'TO, YIELD or TEST where expected", "");
}

Visible Procedure ytu_heading(name, adic, wtx, form)
 value *name; literal *adic; txptr wtx; bool form; {
	/* xeq == No */
	intlet ad= 0; value t1= Vnil, t2= Vnil, t3= Vnil;
	forming= form; /*should be a parameter to fopnd()*/
	Skipsp(tx);
	if (Montormark(Char(tx)))
	parerr("user defined functions or predicates must be tags", "");
	if (Letter(Char(tx))) *name= t1= tag();
	else if (Char(tx) == '(') {
		if (fopnd(wtx) == Vnil) /* ignore */;
	} else parerr("something unexpected instead of formal formula", "");
	Skipsp(tx);
	if (Char(tx) == ':') goto postff;
	if (Dyatormark(Char(tx)))
	parerr("user defined functions or predicates must be tags", "");
	if (Letter(Char(tx))) {
		t2= tag();
		if (t1 == Vnil) *name= t2;
	} else if (Char(tx) == '(') {
		if (t1 == Vnil) parerr("no function name where expected", "");
		if (fopnd(wtx) == Vnil) /* ignore */;
	} else parerr("no function name or formal operand where expected", "");
	ad= 1;
	Skipsp(tx);
	if (Char(tx) == ':') {
		if (t1 == Vnil) nothing(tx, "second formal operand");
		goto postff;
	}
	if (t2 == Vnil)
	parerr("something unexpected following monadic formal formula", "");
	*name= t2;
	if (forming && t1 != Vnil) insert(t1, &formlist);
	if (Letter(Char(tx))) {
		t3= tag();
		if (forming) insert(t3, &formlist);
	} else if (Char(tx) == '(') {
		if (fopnd(wtx) == Vnil) /* ignore */;
	} else parerr("no formal operand where expected", "");
	ad= 2;
	Skipsp(tx);
	if (Char(tx) != ':')
	parerr("something unexpected following dyadic formal formula", "");
postff:	if (t1 != Vnil && t1 != *name) release(t1);
	if (t2 != Vnil && t2 != *name) release(t2);
	if (t3 != Vnil) release(t3);
	*adic= ad == 0 ? Zer : ad == 1 ? Mon : Dya;
	tx++;
}

Hidden value mk_formal(ftx) txptr ftx; { /* Move */
	value f= grab_for(); formal *fp= Formal(f);
	sv_context(&(fp->con)); fp->ftx= ftx;
	return f;
}

Visible bool udc() {
	value un, *aa; context ic, hc; envchain nw_envchain;
	txptr tx0= tx, uux, vux, wux; bool formals= No;
	if (!Cap(Char(tx))) return No;
	if (!xeq) {
		tx= ceol;
		if (skipping) parerr("X", ""); /* to prevent skipping= No; */
		return Yes;
	}
	un= keyword(ceol);
 debug("udc^ called");
	sv_context(&ic);
	if (!is_unit(un, FHW, &aa)) {
		release(un);
		tx= tx0;
		return No;
	}
	if (!Is_howto(*aa)) syserr("no howto associated with keyword");
	curnv= &nw_envchain;
	curnv->tab= mk_elt(); curnv->inv_env= Enil;
	cntxt= In_unit; resexp= Voi; uname= un; utype= FHW;
	cur_ilev= 0; lino= 1;
	tx= (How_to(*aa))->fux;
	terminated= No;
 debug("ready to howto");
	findceol();
	wux= ceol; req(":", wux, &uux, &vux);
	if (!atkw(HOW_TO) || (compare(uname= keyword(uux), un) != 0))
		syserr("out of phase in udc");
	release(un);
	Skipsp(tx);
	while (tx < uux) {
		txptr ftx, ttx, fux, tux;
		value fp, ap, kw;
		kw= findkw(uux, &fux, &tux);
		if (Letter(Char(tx))) fp= bastarg(fux);
		else if (tx < fux) {
			release(kw);
			parerr("no formal parameter where expected", "");
		} else fp= Vnil;
		sv_context(&hc); set_context(&ic); 
		if (fux == uux) ftx= ttx= ceol;
		else reqkw(strval(kw), &ftx, &ttx); /*dangerous use of strval*/
		release(kw);
		if (fp != Vnil) {
			Skipsp(tx);
			nothing(ftx, "actual parameter");
			ap= mk_formal(ftx); formals= Yes;
		} else {
			Skipsp(tx);
			if (tx < ftx)
				parerr("actual parameter without formal", "");
		}
		tx= ttx; 
		sv_context(&ic); set_context(&hc);
		if (fp != Vnil) {
			put(ap, fp); release(fp); release(ap);
		}
		tx= tux; Skipsp(tx);
	}
	tx= vux;
	add_reftab((How_to(*aa))->reftab);
	if (formals) curnv->inv_env= ic.curnv;
	unicomm_suite(); terminated= No;
	release(curnv->tab); release(uname);
	set_context(&ic);
	return Yes;
}

Visible value eva_formal(f) value f; {
	value v; formal *ff= Formal(f); context cc;
	if (!Is_formal(f)) syserr("eva_formal has wrong argument");
	sv_context(&cc); if (cntxt != In_formal) how_context= cc;
	set_context(&ff->con); cntxt= In_formal;
	v= expr(ff->ftx);
	set_context(&cc);
	return v;
}

Visible loc loc_formal(f) value f; {
	loc l; formal *ff= Formal(f); context cc;
	if (!Is_formal(f)) syserr("loc_formal has wrong argument");
	sv_context(&cc); if (cntxt != In_formal) how_context= cc;
	set_context(&ff->con); cntxt= In_formal;
	l= targ(ff->ftx);
	set_context(&cc);
	return l;
}

Visible bool ref_com() {
/* if !xeq, ref_com always returns Yes unless skipping */
	value rn, *aa, rname; context ic;
	txptr tx0= tx, wux; 
	if (!Cap(Char(tx))) return No;
debug("ref_com^ called");
	if (!xeq) {
		tx= ceol;
		if (skipping) parerr("X", ""); /* to prevent skipping= No; */
		return Yes;
	}
	rn= keyword(ceol);
	aa= lookup(rn);
	if (aa == Pnil) {
		release(rn);
		tx= tx0;
		return No;
	}
	if (!Is_refinement(*aa)) syserr("no refinement associated with keyword");
	upto(ceol, "refined-command");
	sv_context(&ic);
	cntxt= In_unit; resexp= Voi;
	cur_ilev= 0;
	lino= (Refinement(*aa))->rlino;
	tx= (Refinement(*aa))->rp;
	terminated= No;
debug("ready to execute refinement");
	findceol();
	wux= ceol;
	if (compare(rname= keyword(wux), rn) != 0)
		syserr("out of phase in ref_com");
	thought(':');
	comm_suite(); terminated= No;
	release(rn); release(rname);
	set_context(&ic);
	return Yes;
}

Visible Procedure udfpr(nd1, fpr, nd2, re) value nd1, nd2; funprd *fpr; literal re; {
	context ic; envchain nw_envchain; value f;
	txptr uux, vux, wux; 
debug("udfpr^ called");
	sv_context(&ic);
	curnv= &nw_envchain;
	curnv->tab= mk_elt(); curnv->inv_env= Enil;
	cntxt= In_unit; resexp= re; uname= aster;
	cur_ilev= 0; lino= 1;
	tx= fpr->fux;
	resval= Vnil; resout= Und; terminated= No;
debug("ready to Yield/Test");
	findceol();
	wux= ceol; req(":", wux, &uux, &vux);
	if (!atkw(YIELD) && !atkw(TEST)) syserr("out of phase in udfpr");
	Skipsp(tx);
	switch (fpr->adic) {
	case Zer:
		uname= tag(); utype= FZR;
		break;
	case Mon:
		uname= tag(); utype= FMN;
		put(nd2, f= fopnd(uux)); release(f);
		break;
	case Dya:
		put(nd1, f= fopnd(uux)); release(f);
		uname= tag(); utype= FDY;
		put(nd2, f= fopnd(uux)); release(f);
		break;
	}
	thought(':');
	tx= vux;
	add_reftab(fpr->reftab);
	unicomm_suite(); terminated= No;
	if (xeq) {
		if (re == Ret && resval == Vnil)
			error("command-suite of YIELD-unit returns no value");
		if (re == Rep && resout == Und)
			error("command-suite of TEST-unit reports no outcome");
	}
	terminated= No;
	release(curnv->tab); release(uname);
	set_context(&ic);
}

#define NET 8

Visible Procedure ref_et(rfv, re) value rfv; literal re; {
	context ic; value bndtglist, rname; env ee; bool prmnv_saved= No;
	envtab svperm_envtab= Vnil, et0, envtabs[NET], *et, *etp; intlet etl;
	txptr uux, vux, wux; 
 debug("ref_et^ called");
	if (!Is_refinement(rfv)) syserr("ref_et called with non-refinement");
	sv_context(&ic);
	ee= curnv; etl= 0;
	while (ee != Enil) {
		if (ee == prmnv) break;
		etl++;
		ee= ee->inv_env;
	}
	if (etl <= NET) et= envtabs;
	else et= (envtab *) getmem((unsigned)etl*sizeof(value));
	ee= curnv; etp= et;
	while (ee != Enil) {
		if (ee == prmnv) {
			if (prmnvtab == Vnil) {
				/* the original permanent environment */
				prmnvtab= prmnv->tab;
				prmnv->tab= copy(prmnvtab);
			} else svperm_envtab= copy(prmnv->tab);
			prmnv_saved= Yes;
			break;
		}
		*etp++= copy(ee->tab);
		ee= ee->inv_env;
	}
	if (resexp == Voi && !prmnv_saved) {
		/* possible access through SHARE */
		if (prmnvtab == Vnil) {
			prmnvtab= prmnv->tab;
			prmnv->tab= copy(prmnvtab);
		} else svperm_envtab= copy(prmnv->tab);
		prmnv_saved= Yes;
	}
	bndtglist= mk_elt(); bndtgs= &bndtglist;
	cntxt= In_unit; resexp= re;
	cur_ilev= 0;
	lino= (Refinement(rfv))->rlino;
	tx= (Refinement(rfv))->rp;
	resval= Vnil; resout= Und; terminated= No;
 debug("ready to eval/test refinement");
	findceol();
	wux= ceol; req(":", wux, &uux, &vux);
	rname= tag(); thought(':');
	comm_suite();
	if (xeq) {
		if (re == Ret && resval == Vnil)
			error("refinement returns no value");
		if (re == Rep && resout == Und)
			error("refinement reports no outcome");
	}
	terminated= No;
	release (rname);
	ee= curnv; etp= et;
	while (ee != Enil) {
		if (ee == prmnv) break;
		if (ee == curnv) et0= ee->tab; else release(ee->tab);
		ee->tab= *etp++;
		ee= ee->inv_env;
	}
	if (prmnv_saved) {
		release(prmnv->tab);
		if (svperm_envtab == Vnil) {
			prmnv->tab= prmnvtab;
			prmnvtab= Vnil;
		} else prmnv->tab= svperm_envtab;
	}
	set_context(&ic);
	if (curnv != prmnv) {
		if (re == Rep) extbnd_tags(bndtglist, &(curnv->tab), et0);
		release(et0);
	}
	release(bndtglist);
	if (etl > NET) freemem((ptr) et);
}

Hidden loc fopnd(q) txptr q; {
	txptr ttx;
	Skipsp(tx);
	if (tx >= q) syserr("fopnd called when it should not be");
	if (Letter(Char(tx))) {
		ttx= tx+1; while(Tagmark(Char(ttx))) ttx++;
	} else if (Char(tx) == '(') {
		txptr tx0= tx++, ftx;
		req(")", q, &ftx, &ttx);
		tx= tx0;
	} else syserr("fopnd does not see formal operand");
	return basfop(ttx);
}

Hidden loc fop(q) txptr q; {
	value c=Vnil; loc l; txptr i, j; intlet len, k;
	if ((len= 1+count(",", q)) == 1) return basfop(q);
	if (xeq) c= mk_compound(len);
	k_Overfields {
		if (!Lastfield(k)) req(",", q, &i, &j);
		else i= q;
		l= basfop(i);
		if (xeq) put_in_field(l, &c, k);
		if (!Lastfield(k)) tx= j;
	}
	return (loc) c;
}

Hidden loc basfop(q) txptr q; {
	loc l= Vnil; txptr i, j;
	Skipsp(tx);
	nothing(q, "formal operand");
	if (Char(tx) == '(') {
		tx++; req(")", q, &i, &j);
		l= fop(i); tx= j;
	} else if (Letter(Char(tx))) {
		value t= tag();
		if (forming && !xeq) insert(t, &formlist);
		else l= local_loc(t);
		release(t);
	} else parerr("no formal operand where expected", "");
	return l;
}

Hidden Procedure unicomm_suite() {
	if (ateol()) {
		while (ilev(Yes) > 0 && atkw(SHARE)) {
			findceol();
			share(ceol);
			To_eol(tx);
		}
		veli();
		if (cur_ilev > 0) {
			cur_ilev= 0;
			comm_suite();
		}
	} else command();
}

Hidden Procedure share(q) txptr q; {
	intlet n, k;
	Skipsp(tx);
	n= 1+count(",", q);
	for (k= 0; k < n; k++) {
		txptr i, j;
		if (k < n-1) req(",", q, &i, &j);
		else i= q;
		sharebas(i);
		if (k < n-1) need(",");
	}
	upto(q, "SHAREd identifier");
}

#define SH_IN_USE "SHAREd identifier is already in use as formal parameter or operand"

Hidden Procedure sharebas(q) txptr q; {
	Skipsp(tx);
	nothing(q, "SHAREd identifier");
	if (Char(tx) == '(') {
		txptr i, j;
		tx++; req(")", q, &i, &j);
		share(i); tx= j;
	} else if (Letter(Char(tx))) {
		value t= tag();
		if (!xeq) {
			if (in(t, formlist)) pprerr(SH_IN_USE, "");
			insert(t, &sharelist);
		} else if (resexp == Voi) { /*ie we're in a HOW'TO*/
			loc l; value *aa= lookup(t);
			if (aa == Pnil) {
				put(global, l= local_loc(t));
				release(l);
			}
		} else { /*we're in a TEST or YIELD*/
			loc l= global_loc(t);
			value g= content(l);
			release(l);
			put(g, l= local_loc(t));
			release(l); release(g);
			/* can this be achieved by scratch-pad copying? */
		}
		release(t);
		upto(q, "SHAREd identifier");
	} else parerr("no identifier where expected", "");
}

#define REF_IN_USE "refinement-tag is already in use as formal parameter or operand"

Hidden Procedure ref_suite() {
	txptr rp; intlet rlino; value r, kt, *aa;
rref:	if (ilev(Yes) > 0) parerr("indentation where not allowed", "");
	findceol();
	if (Cap(Char(tx)) && !atkw(SELECT)) {
		kt= findkw(lcol(), &rp, &tx);
		Skipsp(tx);
		if (Char(tx) != ':') {
			release(kt);
			veli(); return;
		}
		rlino= lino;
	} else if (Letter(Char(tx))) {
		rp= tx;
		while(Tagmark(Char(tx))) tx++;
		Skipsp(tx);
		if (Char(tx) != ':') {
			veli(); return;
		}
		tx= rp; rlino= lino; kt= tag();
		if (in(kt, formlist)) pprerr(REF_IN_USE, "");
		if (in(kt, sharelist)) pprerr(
		"refinement-tag is already in use as SHAREd identifier", "");
	} else {
		veli(); return;
	}
	if (in_env(reftab, kt, &aa)) error("redefinition of refinement");
	thought(':');
	r= mk_ref(rp, rlino);
	e_replace(r, &reftab, kt);
	comm_suite();
	if (!Eol(tx)) syserr("comm_suite does not leave tx at Eol");
	Mark_unit_end(tx);
	release(r); release(kt);
	goto rref;
}

Hidden Procedure add_reftab(rt) envtab rt; {
	int k, len;
	if (!Is_table(rt)) syserr("add_reftab called with non_table");
	len= length(rt);
	k_Over_len {
		e_replace(*assoc(rt, k), &(curnv->tab), *key(rt, k));
	}
}

Visible Procedure inithow() {
	aster= mk_text("***");
	global= grab_glo();
}

Hidden Procedure adjust_unit(fux, lux, reftb) txptr *fux, *lux; value *reftb; {
	/* The text of the unit still resides in the text buffer.
	   It is moved to an allocated area and the text pointers
	   are adjusted accordingly. */
	txptr tm, ta; int adj, k, len= length(*reftb);

	ta= (txptr) getmem((unsigned)(*lux-*fux)*sizeof(*tx));
	tm= *fux; adj= ta-tm;
	while (tm <= tx) *ta++= *tm++;
	*fux+= adj; *lux+= adj;
	k_Over_len {
		Refinement(*assoc(*reftb, k))->rp+= adj; /*Change*/
	}
}
