/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sta.c,v 1.4 85/08/22 16:59:30 timo Exp $
*/

/* Stacks used by the interpreter */

/* Scratch-pad copying.

   One of the hairiest details of B is scratch-pad copying and its
   interaction with formal parameters (to HOW'TO units).
   Via formal parameters one can peek and poke into the local environment
   of the HOW'TO's in the call chain.  When a parameter is changed from
   within an expression- or test-refinement, the scratch-pad copying
   prescribes that the whole chain of local environments is restored
   to its original state when the refinement exits.  Example:

    >>> HOW'TO X fp:
	    WRITE fp, ref, fp /
	ref:
	    PUT fp+1 IN fp
	    RETURN fp
    >>> HOW'TO Y fp:
	    X fp
    >>> HOW'TO Z:
	    PUT 1 IN t
	    Y t
	    WRITE t
    >>> Z
    1 2 1
    1

   It is clear that the scratch-pad copying for the call of ref in X
   must save the local environments of Y and Z, and restore them when
   ref exits.
   For similar reasons we must save the permanent environment.
   All this also interacts with the practice of 'locating' a target.
   All targets eventually refer to (one or more) basic targets.
   The location of a basic target is represented as a pair (env, key)
   where 'env' is the address of the environment in which the target
   resides and 'key' is the target's name (for permanent targets) or
   its number (for local targets).  When we consider the PUT fp+1 IN fp
   line in unit X above, we can see that the (local) environment
   for the location returned by 'fp' is the local environment of Z.
   Therefore this whole chain must still be intact.
   There can be even trickier cases, where a location is saved for a
   long time on the execution stack while the environment it refers to
   is subject to scratch-pad copying and restoring; when the location
   is finally popped off the stack, it must still refer to the correct
   environment.

   Another detail to consider is that for the permanent environment,
   we need access to the 'real' permanent environment, i.e., its value
   before any scratch-pad copying occurred.  (Example:

    >>> YIELD f:
	    SHARE x
	    PUT x+1 IN x
	    READ t EG 0
	    RETURN t
    >>> PUT 0 IN x
    >>> WRITE x, f, x
    ??? x
    0, 0, 0
    >>> 

   Even though at the time the READ is called, x has been given the value
   1 temporarily, the value of x used in the evaluation of the input
   expression is the original value, 0.)

   A final detail to be observed is the passing back of 'bound tags'
   when a refined test is called.

   The chosen implementation is as follows:
   - Environments are saved in a linked list of structures (envchain) with
     two fields: tab, the actual environment (a table or compound) and
     inv_env, the link to the previous entry in the list.
   - The routines newenvchain and popenvchain push and pop such lists.
   - There is one list for the permanent environment, whose head is prmnv,
     and one list for the current environment, whose head is usually curnv.
     The last element of both lists is actually the same, because at the
     immediate command level the current environment is the permanent
     environment.  When we are evaluating or locating a formal parameter,
     'curnv' points somewhere in the middle of its chain, to the local
     environment of the caller.
     The two lists are manipulated separately:
   - Prmnv is pushed (with a copy of itself) for each scratch-pad copy,
     and popped whe a scratch-pad is thrown away.
   - Curnv is pushed for each unit invocation, with the new local
     environment, and popped when the unit exits.
   - When a scratch-pad copy is required, the chain headed by curnv
     is walked until a local environment is found without HOW'TO formal
     parameters, and a compound containing copies of all the local
     environments thus found is saved on the general-purpose value stack.
     This value is popped off that stack again and the local environments
     in the chain are restored when the scratch-pad copy has to be thrown
     away.  (Thus we work on the real thing and save and restore a copy
     of it, while the DP prescribes that the system work on a copy.
     The effect is the same, of course.)
   - There is a third list for bound tags whose treatment is left as an
     exercise for the reader.
   - When a formal parameter is called, the current value of 'curnv' must
     be saved somewhere, so that it can be restored later; in this case
     it doesn't follow the stack-wise discipline of the chain.
   - Finally note thate that when a YIELD unit is called during the
     evaluation of a formal parameter, the chain of local environments
     "splices" temorarily, because the new local environment is linked
     to curnv which is not the end of the chain.  No problem!

   All this nonsense can be avoided when a copy-restore parameter mechanism
   is used instead: then there are no accesses to other local environments
   that the current, except a transfer between two "adjacent" ones at call
   and return time.  Maybe ABC will have such a parameter mechanism...

*/

#include "b.h"
#include "b1mem.h"
#include "b1obj.h"
#include "b2nod.h"
#include "b3env.h"
#include "b3err.h"
#include "b3int.h"
#include "b3sem.h"
#include "b3sou.h" /* for permkey() and get_pname() */
#include "b3sta.h"

/* Fundamental registers: (shared only between this file and b3int.c) */

Visible parsetree pc; /* 'Program counter', current parsetree node */
Visible parsetree next; /* Next parsetree node (changed by jumps) */
Visible bool report; /* 'Condition code register', outcome of last test */

Visible bool noloc; /* Set while evaluating (as opposed to locating)
			formal parameters of HOW'TOs */

Hidden env boundtags; /* Holds bound tags chain */

/* Value stack: */

/* The run-time value stack grows upward, sp points to the next free entry.
   Allocated stack space lies between st_base and st_top.
   In the current invocation, the stack pointer (sp) must lie between
   st_bottom and st_top.
   Stack overflow is corrected by growing st_top, underflow is a fatal
   error (generated code is wrong).
*/

Hidden value *st_base, *st_bottom, *st_top, *sp;
Visible int call_level; /* While run() can be called recursively */

#define EmptyStack() (sp == st_bottom)
#define BotOffset() (st_bottom - st_base)
#define SetBotOffset(n) (st_bottom= st_base + (n))

#define INCREMENT 100

Hidden Procedure st_grow(incr) int incr; {
	if (!st_base) { /* First time ever */
		st_bottom= sp= st_base=
			(value*) getmem((unsigned) incr * sizeof(value *));
		st_top= st_base + incr;
	}
	else {
		int syze= (st_top - st_base) + incr;
		int n_bottom= BotOffset();
		int n_sp= sp - st_base;
		regetmem((ptr*) &st_base, (unsigned) syze * sizeof(value *));
		sp = st_base + n_sp;
		SetBotOffset(n_bottom);
		st_top= st_base + syze;
	}
}

Visible value pop() {
	if (sp <= st_bottom) {
		syserr(MESS(4100, "stack underflow"));
		return Vnil;
	}
	return *--sp;
}

Visible Procedure push(v) value v; {
	if (sp >= st_top) st_grow(INCREMENT);
	*sp++ = (v);
}

/* - - - */

/* Various call types, used as index in array: */

#define C_prmnv 0
#define C_immexp 1
#define C_immcmd 2
#define C_read 3

#define C_howto 4
#define C_yield 5
#define C_test 6

#define C_refcmd 7
#define C_refexp 8
#define C_reftest 9

#define C_formal 10


/* What can happen to a thing: */

#define Old 'o'
#define Cpy 'c'
#define New 'n'
#define Non '-'

typedef struct {
	literal do_cur;
	literal do_prm;
	literal do_bnd;
	literal do_for;
	literal do_cntxt;
	literal do_resexp;
} dorecord;


/* Table encoding what to save/restore for various call/return types: */
/* (Special cases are handled elsewhere.) */

Hidden dorecord doo[] = {
	/*		 cur  prm  bnd  for  cntxt    resexp */

	/* prmnv */	{Old, Old, Old, Old, In_prmnv, Voi},
	/* imm expr */	{Old, Old, Old, Old, In_command, Voi},
	/* imm cmd */	{Old, Old, Old, Old, In_command, Voi},
	/* READ EG */	{Non, Non, Non, Non, In_read, Voi},

	/* HOW-TO */	{New, Old, Non, New, In_unit, Voi},
	/* YIELD */	{New, Cpy, Non, Non, In_unit, Ret},
	/* TEST */	{New, Cpy, Non, Non, In_unit, Rep},

	/* REF-CMD */	{Old, Old, Old, Old, In_unit, Voi},
	/* ref-expr */	{Cpy, Cpy, Non, Old, In_unit, Ret},
	/* ref-test */	{Cpy, Cpy, New, Old, In_unit, Rep},

	/* formal */	{Non, Old, Non, Non, In_formal, Voi},
};

#define MAXTYPE ((sizeof doo) / (sizeof doo[0]))

#define Checksum(type) (12345 - (type)) /* Reversible */


#define Ipush(n) push(MkSmallInt(n))
#define Ipop() SmallIntVal(pop())


Hidden env newenv(tab, inv_env) envtab tab; env inv_env; {
	env e= (env) getmem(sizeof(envchain));
	e->tab= tab; /* Eats a reference to tab! */
	e->inv_env= inv_env;
	return e;
}


Hidden Procedure popenv(pe) env *pe; {
	env e= *pe;
	*pe= e->inv_env;
	release(e->tab);
	freemem((ptr) e);
}


Forward value save_curnv_chain();

Hidden Procedure call(type, new_pc) intlet type; parsetree new_pc; {
	if (type < 0 || type >= MAXTYPE) syserr(MESS(4101, "bad call type"));
	if (tracing) tr_call();

	/* Push other stacks */

	if (doo[type].do_bnd != Old) {
		boundtags= newenv(
			(doo[type].do_bnd == New) ? mk_elt() : Vnil,
			boundtags);
		bndtgs= &boundtags->tab;
	}
	switch (doo[type].do_cur) {

	case New:
		curnv= newenv(Vnil, curnv);
		break;

	case Cpy:
		push(save_curnv_chain());
		break;

	case Non:
		push(mk_int((double) ((int) curnv)));
			/* PORTABILITY?!?! */
		break;

	}
	if (doo[type].do_prm != Old) {
		prmnv= newenv(
			(doo[type].do_prm == Cpy) ? copy(prmnv->tab) : Vnil,
			prmnv);
	}

	/* Push those things that depend on the call type: */

	if (doo[type].do_for != Old) {
		/* Formal parameter context and unit name/type */
		/* FP removed */
		push(uname); uname= Vnil;
	}

	/* Push miscellaneous context info: */
	push(curline);
	push(curlino);
	Ipush(noloc); noloc= No;
	Ipush(resexp); resexp= doo[type].do_resexp;
	Ipush(cntxt); cntxt= doo[type].do_cntxt;
	resval= Vnil;

	/* Push vital data: */
	push(next);
	Ipush(BotOffset()); ++call_level;
	Ipush(Checksum(type)); /* Kind of checksum */

	/* Set st_bottom and jump: */
	st_bottom= sp;
	next= new_pc;
}


Visible Procedure ret() {
	int type; value rv= resval; literal re= resexp;
	value oldcurnvtab= Vnil, oldbtl= Vnil;

	if (tracing) tr_ret();
	if (cntxt == In_formal && still_ok) { rv= pop(); re= Ret; }

	/* Clear stack: */
	while (!EmptyStack()) release(pop());

	/* Pop type and hope it's good: */
	st_bottom= st_base; /* Trick to allow popping the return info */
	type= Checksum(Ipop());
	if (type < 0 || type >= MAXTYPE) syserr(MESS(4102, "stack clobbered"));

	/* Pop vital data: */
	SetBotOffset(Ipop()); --call_level;
	next= pop();

	/* Pop context info: */
	cntxt= Ipop();
	resexp= Ipop();
	noloc= Ipop();
	curlino= pop();
	curline= pop();

	/* Variable part: */
	if (doo[type].do_for != Old) {
		release(uname); uname= pop();
		/* FP removed */
	}
	if (doo[type].do_prm != Old)
		popenv(&prmnv);
	switch (doo[type].do_cur) {

	case Cpy:
		oldcurnvtab= copy(curnv->tab);
		rest_curnv_chain(pop());
		break;

	case New:
		oldcurnvtab= copy(curnv->tab);
		popenv(&curnv);
		break;

	case Non:
		{ value v= pop();
		  curnv= (env) intval(v);
		 release(v);
		}
		break;

	}
	if (doo[type].do_bnd != Old) {
		oldbtl= copy(*bndtgs);
		popenv(&boundtags);
		bndtgs= &boundtags->tab;
	}

	/* Fiddle bound tags */
	if (oldbtl != Vnil) {
		extbnd_tags(oldbtl, oldcurnvtab);
		release(oldbtl);
	}
	if (oldcurnvtab != Vnil) release(oldcurnvtab);
	if (call_level == 0) re_env(); /* Resets bndtgs */

	/* Push return value (if any): */
	if (re == Ret && still_ok) push(rv);
}

/* - - - */

Visible Procedure call_formal(name, number, targ)
 value name, number; bool targ; {
	value *aa= envassoc(curnv->tab, number); formal *ff= Formal(*aa);
	literal ct;
	if (aa == Pnil || !Is_formal(*aa)) syserr(MESS(4103, "formal gone"));
	if (cntxt != In_formal) {
		release(how_context.uname);
		sv_context(&how_context); /* for error messages */
	}
	call(C_formal, ff->fp);

	/* The following should be different, but for now... */
	curnv= ff->con.curnv;
	release(uname); uname= copy(ff->con.uname);
	curline= ff->con.cur_line; curlino= ff->con.cur_lino;
	ct= cntxt; cntxt= ff->con.cntxt;
	release(act_context.uname);
	sv_context(&act_context); cntxt= ct; /* for error messages */

	if (!targ) noloc= Yes;
	else if (!Thread2(next)) error(MESS(4104, "expression used as target"));
}

Visible Procedure call_refinement(name, def, test)
 value name; parsetree def; bool test; {
	call(test ? C_reftest : C_refexp,
		*Branch(Refinement(def)->rp, REF_START));
}

#define YOU_TEST MESS(4105, "You haven't told me how to TEST ")
#define YOU_YIELD MESS(4106, "You haven't told me how to YIELD ")

Hidden Procedure udfpr(nd1, name, nd2, isfunc)
 value nd1, name, nd2; bool isfunc; {
	value *aa;
	parsetree u; int k, nlocals; funprd *fpr;
	int adicity= nd1 ? Dya : nd2 ? Mon : Zer;
	if (!is_unit(name, adicity, &aa)
		|| !(isfunc ? Is_function(*aa) : Is_predicate(*aa))) {
		error3(isfunc ? YOU_YIELD : YOU_TEST, name, 0);
		return;
	}
	fpr= Funprd(*aa);
	if (!(fpr->adic==Zer ? nd2==Vnil : (fpr->adic==Mon) == (nd1==Vnil)))
		syserr(MESS(4107, "invoked unit has other adicity than invoker"));
	if (fpr->pre != Use) syserr(MESS(4108, "udfpr with predefined unit"));

	u= fpr->unit;
	if (fpr->unparsed) fix_nodes(&u, &fpr->code);
	if (!still_ok) { rem_unit(u); return; }
	fpr->unparsed= No;
	nlocals= intval(*Branch(u, FPR_NLOCALS));
	call(isfunc ? C_yield : C_test, fpr->code);
	curnv->tab= mk_compound(nlocals);
	for (k= 0; k < nlocals; ++k) *Field(curnv->tab, k)= Vnil;
	release(uname); uname= get_pname(u);
	if (nd1 != Vnil) push(copy(nd1));
	if (nd2 != Vnil) push(copy(nd2));
}

Visible Procedure formula(nd1, name, nd2, tor) value nd1, name, nd2, tor; {
	if (tor == Vnil) udfpr(nd1, name, nd2, Yes);
	else {
		if (!Is_function(tor))
			syserr(MESS(4109, "formula called with non-function"));
		push(pre_fun(nd1, Funprd(tor)->pre, nd2));
	}
}

Visible Procedure proposition(nd1, name, nd2, pred) value nd1, name, nd2, pred; {
	if (pred == Vnil) udfpr(nd1, name, nd2, No);
	else {
		if (!Is_predicate(pred))
			syserr(MESS(4110, "proposition called with non-predicate"));
		report= pre_prop(nd1, Funprd(pred)->pre, nd2);
	}
}

Visible Procedure v_mystery(name, number) value name, number; {
	value *aa; fun f;
	aa= envassoc(curnv->tab, Is_compound(curnv->tab) ? number : name);
	if (aa != Pnil) push(copy(*aa));
	else if (is_zerfun(name, &f)) {
		if (Funprd(f)->pre == Use) f= Vnil;
		formula(Vnil, name, Vnil, f);
	}
	else error3(0, name, MESS(4111, " has not yet received a value"));
}

Hidden value mk_formal(pt) parsetree pt; {
	value f= grab_for(); formal *ff= Formal(f);
	sv_context(&ff->con); ff->fp= pt;
	return f;
}

Visible Procedure x_user_command(name, actuals, def)
 value name; parsetree actuals; value def;
{
	how *h; parsetree u; value *aa;
	value v, formals; int k, len;
	if (def != Vnil) {
		if (!Is_refinement(def)) syserr(MESS(4112, "bad def in x_user_command"));
		call(C_refcmd, *Branch(Refinement(def)->rp, REF_START));
		return;
	}
	if (!is_unit(name, How, &aa)) {
		error3(MESS(4113, "You haven't told me HOW'TO "), name, 0);
		return;
	}
	u= (h= How_to(*aa))->unit;
	if (h->unparsed) fix_nodes(&u, &h->code);
	if (!still_ok) { rem_unit(u); return; }
	h->unparsed= No;
	formals= *Branch(u, HOW_FORMALS);
	len= intval(*Branch(u, HOW_NLOCALS)); k= 0;
	v= mk_compound(len);
	while (actuals != Vnil && formals != Vnil) { /* Save actuals */
		if (*Branch(actuals, ACT_EXPR) != Vnil) {
			if (k >= len) syserr(MESS(4114, "too many actuals"));
			*Field(v, k++)= mk_formal(*Branch(actuals, ACT_START));
		}
		actuals= *Branch(actuals, ACT_NEXT);
		formals= *Branch(formals, FML_NEXT);
	}
	for (; k < len; ++k) { *Field(v, k)= Vnil; }

	call(C_howto, h->code);
	
	curnv->tab= v;
	release(uname); uname= permkey(name, How);
}

Visible Procedure endsta() {
	if (st_base) {
		freemem((ptr) st_base);
		st_base= Pnil;		
	}
}

Hidden value save_curnv_chain() {
	value pad;
	value c, f;
	formal *ff;
	int cnt, k;

	/* Count how many */
	c= curnv->tab;
	for (cnt= 0; ; ) {
		if (!Is_compound(c)) break;
		++cnt;
		f= *Field(c, 0);
		if (!Is_formal(f)) break;
		ff= Formal(f);
		c= ff->con.curnv->tab;
	}

	pad= mk_compound(cnt);

	/* Do the copy */
	c= curnv->tab;
	for (k= 0; ; ) {
		if (!Is_compound(c)) break;
		*Field(pad, k)= copy(c);
		if (++k >= cnt) break;
		f= *Field(c, 0);
		if (!Is_formal(f)) break;
		ff= Formal(f);
		c= ff->con.curnv->tab;
	}
	if (k != cnt)
		syserr(MESS(4115, "save_curnv_chain: phase error"));

	return pad;
}

Hidden rest_curnv_chain(pad) value pad; {
	int k, cnt;
	value f, *c= &curnv->tab;
	formal *ff;

	if (pad == Vnil || !Is_compound(pad))
		syserr(MESS(4116, "rest_curnv_chain: bad pad"));
	cnt= Nfields(pad);
	for (k= 0; ; ) {
		if (!Is_compound(*c)) break;
		release(*c);
		*c= copy(*Field(pad, k));
		if (++k >= cnt) break;
		f= *Field(*c, 0);
		if (!Is_formal(f)) break;
		ff= Formal(f);
		c= &ff->con.curnv->tab;
	}
	if (k != cnt)
		syserr(MESS(4117, "rest_curnv_chain: phase error"));
	release(pad);
}
