/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1val.c,v 1.1 84/06/28 00:49:01 timo Exp $ */

/* General operations for objects */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1mem.h"
#include "b2scr.h" /* TEMPORARY for at_nwl */
#include "b2sem.h" /* TEMPORARY for grab */
#ifndef SMALLNUMBERS
#include "b1num.h" /* for ccopy, rrelease, grab, grab_num, grab_rat, grab_approx */
#else
#include "B1num.h" /* For grab */
#endif


#define LL (len < 200 ? 1 : 8)
#define Len (len == 0 ? 0 : ((len-1)/LL+1)*LL)
#define Adj(s) (unsigned) (sizeof(*Vnil)-sizeof(Vnil->cts)+(s))

#define Grabber() {if(len>Maxintlet)syserr("big grabber");}
#define Regrabber() {if(len>Maxintlet)syserr("big regrabber");}

value etxt, elis, etab, elt;

long gr= 0;

Visible Procedure prgr() {at_nwl=No;printf(" gr:%ld",gr);gr=0;}

Hidden value grab(type, len) literal type; intlet len; {
	unsigned syze; value v;
	Grabber();
	switch (type) {
	case Num:
#ifdef SMALLNUMBERS
		syze= sizeof(number);
#else
		if (len >= 0) syze= Len*sizeof(digit);		/* Integral */
		else if (len == -1) syze= sizeof(double);	/* Approximate */
		else syze= 2*sizeof(value);			/* Rational */
#endif
		break;
	case Tex: syze= (len+1)*sizeof(char); break; /* one extra for the '\0' */
	case Com: syze= len*sizeof(value); break;
	case ELT: syze= (len= 0); break;
	case Lis:
	case Tab: syze= Len*sizeof(value); break;
	case Sim: syze= sizeof(simploc); break;
	case Tri: syze= sizeof(trimloc); break;
	case Tse: syze= sizeof(tbseloc); break;
	case How: syze= sizeof(how); break;
	case For: syze= sizeof(formal); break;
	case Glo: syze= 0; break;
	case Per: syze= sizeof(value); break;
	case Fun:
	case Prd: syze= sizeof(funprd); break;
	case Ref: syze= sizeof(ref); break;
	default:
		printf("\ngrabtype{%c}\n", type);
		syserr("grab called with unknown type");
	}
	v= (value) getmem(Adj(syze));
	v->type= type; v->len= len; v->refcnt= 1;
gr+=1;
	return v;
}

#ifdef SMALLNUMBERS
Visible value grab_num(len) intlet len; { return grab(Num, len); }
#else
Visible value grab_num(len) register int len; {
	integer v;
	register int i;

	v = (integer) grab(Num, len);
	for (i = Length(v)-1; i >= 0; --i) Digit(v, i) = 0;
	return (value) v;
}

Visible value grab_rat() {
	return (value) grab(Num, -2);
}

Visible value grab_approx() {
	return (value) grab(Num, -1);
}

Visible value regrab_num(v, len) value v; register int len; {
	register unsigned syze;

	syze = Len * sizeof(digit);
	regetmem(&v, Adj(syze));
	Length(v) = len;
	return v;
}
#endif

Visible value grab_tex(len) intlet len; {
	if (len == 0) return copy(etxt);
	return grab(Tex, len);
}

Visible value grab_com(len) intlet len; { return grab(Com, len); }

Visible value grab_elt() { return copy(elt); }

Visible value grab_lis(len) intlet len; {
	if (len == 0) return copy(elis);
	return grab(Lis, len);
}

Visible value grab_tab(len) intlet len; {
	if (len == 0) return copy(etab);
	return grab(Tab, len);
}

Visible value grab_sim() { return grab(Sim, 0); }

Visible value grab_tri() { return grab(Tri, 0); }

Visible value grab_tse() { return grab(Tse, 0); }

Visible value grab_how() { return grab(How, 0); }

Visible value grab_for() { return grab(For, 0); }

Visible value grab_glo() { return grab(Glo, 0); }

Visible value grab_per() { return grab(Per, 0); }

Visible value grab_fun() { return grab(Fun, 0); }

Visible value grab_prd() { return grab(Prd, 0); }

Visible value grab_ref() { return grab(Ref, 0); }

Visible value copy(v) value v; {
	if (v != Vnil && v->refcnt < Maxintlet) (v->refcnt)++;
 gr+=1;
	return v;
}

Visible Procedure release(v) value v; {
	intlet *r= &(v->refcnt);
	if (v == Vnil) return;
	if (*r == 0) syserr("releasing unreferenced value");
 if(bugs){printf("releasing: "); if (Type(v) == Num) bugs= No; wri(v,No,No,No); bugs= Yes; line();}
	if (*r < Maxintlet && --(*r) == 0) rrelease(v);
 gr-=1;
}

Hidden value ccopy(v) value v; {
	literal type= v->type; intlet len= Length(v), k; value w;
	w= grab(type, len);
	switch (type) {
	case Num:
#ifdef SMALLNUMBERS
		Numerator(w)= Numerator(v);
		Denominator(w)= Denominator(v);
#else
		if (Integral(v)) {
			register int i;
			for (i = len-1; i >= 0; --i)
				Digit((integer)w, i) = Digit((integer)v, i);
		} else if (Approximate(v))
			Realval((real)w) = Realval((real)v);
		else if (Rational(v)) {
			Numerator((rational)w) =
				(integer) copy(Numerator((rational)v));
			Denominator((rational)w) =
				(integer) copy(Denominator((rational)v));
		}
#endif
		break;
	case Tex:
		strcpy(Str(w), Str(v));
		break;
	case Com:
	case Lis:
	case Tab:
	case ELT:
		{value *vp= Ats(v), *wp= Ats(w);
			Overall *wp++= copy(*vp++);
		} break;
	case Sim:
		{simploc *vv= (simploc *)Ats(v), *ww= (simploc *)Ats(w);
			ww->i= copy(vv->i); ww->e= vv->e; /* No copy */
		} break;
	case Tri:
		{trimloc *vv= (trimloc *)Ats(v), *ww= (trimloc *)Ats(w);
			ww->R= copy(vv->R); ww->B= vv->B; ww->C= vv->C;
		} break;
	case Tse:
		{tbseloc *vv= (tbseloc *)Ats(v), *ww= (tbseloc *)Ats(w);
			ww->R= copy(vv->R); ww->K= copy(vv->K);
		} break;
	case How:
		*((how *)Ats(w)) = *((how *)Ats(v));
		break;
	case For:
		*((formal *)Ats(w)) = *((formal *)Ats(v));
		break;
	case Glo:
		break;
	case Per:
		*Ats(w)= copy(*Ats(v));
		break;
	case Fun:
	case Prd:
		*((funprd *)Ats(w)) = *((funprd *)Ats(v));
		break;
	case Ref:
		*((ref *)Ats(w)) = *((ref *)Ats(v));
		break;
	default:
		syserr("ccopy called with unknown type");
	}
	return w;
}

Hidden Procedure rrelease(v) value v; {
	literal type= v->type; intlet len= Length(v), k;
	switch (type) {
	case Num:
#ifndef SMALLNUMBERS
		if (Rational(v)) {
			release(Numerator((rational)v));
			release(Denominator((rational)v));
		}
		break;
#endif
	case Tex:
		break;
	case Com:
	case Lis:
	case Tab:
	case ELT:
		{value *vp= Ats(v);
			Overall release(*vp++);
		} break;
	case Sim:
		{simploc *vv= (simploc *)Ats(v);
			release(vv->i); /* No release of vv->e */
		} break;
	case Tri:
		{trimloc *vv= (trimloc *)Ats(v);
			release(vv->R);
		} break;
	case Tse:
		{tbseloc *vv= (tbseloc *)Ats(v);
			release(vv->R); release(vv->K);
		} break;
	case How:
		{how *vv= (how *)Ats(v);
			freemem((ptr) vv->fux);
			release(vv->reftab);
		} break;
	case For:
	case Glo:
		break;
	case Per:
		release(*Ats(v));
		break;
	case Fun:
	case Prd:
		{funprd *vv= (funprd *)Ats(v);
			if (vv->def == Use) {
				freemem((ptr) vv->fux);
				release(vv->reftab);
			}
		} break;
	case Ref:
		break;
	default:
		syserr("release called with unknown type");
	}
	v->type= '\0'; freemem((ptr) v);
}

Visible Procedure uniql(ll) value *ll; {
	if (*ll != Vnil && (*ll)->refcnt > 1) {
		value c= ccopy(*ll);
		release(*ll);
		*ll= c;
	}
}

Visible Procedure xtndtex(a, d) value *a; intlet d; {
	intlet len= Length(*a)+d;
	Regrabber();
	regetmem(a, Adj((len+1)*sizeof(char)));
	(*a)->len= len;
}

Visible Procedure xtndlt(a, d) value *a; intlet d; {
	intlet len= Length(*a); intlet l1= Len, l2;
	len+= d; l2= Len;
	if (l1 != l2) {
		Regrabber();
		regetmem(a, Adj(l2*sizeof(value)));
	}
	(*a)->len= len;
}

Visible Procedure initmem() {
	etxt= grab(Tex, 0);
	elis= grab(Lis, 0);
	etab= grab(Tab, 0);
	elt=  grab(ELT, 0);
 notel= grab_lis(0); noting= No;
}
