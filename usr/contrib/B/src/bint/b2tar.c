/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tar.c,v 1.4 85/08/22 16:56:49 timo Exp $
*/

#include "b.h"
#include "b1obj.h"
#include "b2par.h"
#include "b2syn.h"
#include "b2nod.h"
#include "b3err.h"

Forward parsetree singtarg();

Visible parsetree targ(q) txptr q; {
	return collateral(q, singtarg);
}

Hidden parsetree singtarg(q) txptr q; {
	parsetree v; value t;
	skipsp(&tx);
	if (nothing(q, "target")) return NilTree;
	if (open_sign()) v= compound(q, targ);
	else if (is_tag(&t)) v= node2(TAG, t);
	else {
		parerr(MESS(2500, "no target where expected"));
		tx= q;
		return NilTree;
	}
	selection(q, &v);
	tar_trimmed_text(q, &v);
	upto(q, "target");
	return v;
}
