/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2cmd.c,v 1.4 85/08/22 16:54:17 timo Exp $
*/

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b2par.h" 
#include "b2key.h"
#include "b2syn.h"
#include "b2nod.h"
#include "b3env.h"
#include "b3err.h"
#include "b3ext.h"

/* ******************************************************************** */
/*		command_suite						*/
/* ******************************************************************** */

Forward parsetree cmd_seq();

Visible parsetree cmd_suite(cil, first) intlet cil; bool first; {
	if (ateol())
		return cmd_seq(cil, first);
	else {
		parsetree v; value c; intlet l= lino;
		suite_command(&v, &c);
		return node5(SUITE, mk_integer(l), v, c, NilTree);
	}
}

Hidden parsetree cmd_seq(cil, first) intlet cil; bool first; {
	value c; intlet level, l;
	level= ilev(); l= lino;
	if (is_comment(&c)) 
		return node5(SUITE, mk_integer(l), NilTree, c,
				cmd_seq(cil, first));
	if ((level == cil && !first) || (level > cil && first)) {
		parsetree v;
		findceol();
		suite_command(&v, &c);
		return node5(SUITE, mk_integer(l), v, c, cmd_seq(level, No));
	}
	veli();
	return NilTree;
}

Visible Procedure suite_command(v, c) parsetree *v; value *c; {
	*v= NilTree; *c= Vnil;
	if (!(control_command(v) || simple_command(v, c))) 
		parerr(MESS(2000, "no command where expected"));
}

/* ******************************************************************** */
/*		is_comment, tail_line					*/
/* ******************************************************************** */

Visible bool is_comment(v) value *v; {
	txptr tx0= tx;
	skipsp(&tx);
	if (comment_sign()) {
		while (Space(Char(tx0-1))) tx0--;
		while (!Eol(tx)) tx++;
		*v= cr_text(tx0, tx);
		return Yes;
	}
	tx= tx0;
	return No;
}

Visible value tail_line() {
	value v;
	if (is_comment(&v)) return v;
	if (!ateol()) parerr(MESS(2001, "something unexpected following this line"));
	return Vnil;
}

/* ******************************************************************** */
/*		simple_command						*/
/*									*/
/* ******************************************************************** */

Forward bool bas_com(), term_com(), udr_com();

Visible bool simple_command(v, c) parsetree *v; value *c; {
	return bas_com(v) || term_com(v) || udr_com(v)
		? (*c= tail_line(), Yes) : No;
}

/* ******************************************************************** */
/*		basic_command						*/
/* ******************************************************************** */

Forward value cr_newlines();

Hidden bool bas_com(v) parsetree *v; {
	txptr ftx, ttx; parsetree e, t;
	if (check_keyword()) {
			*v= node2(CHECK, test(ceol));
	} else if (choose_keyword()) {
			req(K_FROM_choose, ceol, &ftx, &ttx);
			t= targ(ftx); tx= ttx;
			*v= node3(CHOOSE, t, expr(ceol));
	} else if (delete_keyword()) {
			*v= node2(DELETE, targ(ceol));
	} else if (draw_keyword()) {
			*v= node2(DRAW, targ(ceol));
	} else if (insert_keyword()) {
			req(K_IN_insert, ceol, &ftx, &ttx);
			e= expr(ftx); tx= ttx;
			*v= node3(INSERT, e, targ(ceol));
	} else if (put_keyword()) {
			req(K_IN_put, ceol, &ftx, &ttx);
			e= expr(ftx); tx= ttx;
			*v= node3(PUT, e, targ(ceol));
	} else if (read_keyword()) {
			if (find(K_RAW, ceol, &ftx, &ttx)) {
				*v= node2(READ_RAW, targ(ftx)); tx= ttx;
				upto(ceol, K_RAW);
			} else {
				req(K_EG, ceol, &ftx, &ttx);
				t= targ(ftx); tx= ttx;
				*v= node3(READ, t, expr(ceol));
			}
	} else if (remove_keyword()) {
			req(K_FROM_remove, ceol, &ftx, &ttx);
			e= expr(ftx); tx= ttx;
			*v= node3(REMOVE, e, targ(ceol));
	} else if (setrandom_keyword()) {
			*v= node2(SET_RANDOM, expr(ceol));
	} else if (write_keyword()) {
			intlet b_cnt= 0, a_cnt= 0;
			skipsp(&tx);
			if (Ceol(tx))
				parerr(MESS(2002, "no parameter where expected"));
			while (nwl_sign()) {b_cnt++; skipsp(&tx); }
			if (Ceol(tx)) e= NilTree;
			else {
				ftx= ceol;
				while (Space(Char(ftx-1)) || Char(ftx-1) == '/')
					if (Char(--ftx) == '/') a_cnt++;
				skipsp(&tx);
				e= ftx > tx ? expr(ftx) : NilTree;
			}
			*v= node4(WRITE,
				  cr_newlines(b_cnt), e, cr_newlines(a_cnt));
			tx= ceol;
	} else return No;
	return Yes;
}

Hidden value cr_newlines(cnt) intlet cnt; {
	value v, t= mk_text("/"), n= mk_integer(cnt);
	v= repeat(t, n);
	release(t); release(n);
	return v;
}

/* ******************************************************************** */
/*		terminating_command					*/
/* ******************************************************************** */

Visible bool term_com(v) parsetree *v; {
	if (fail_keyword()) {
		upto(ceol, K_FAIL);
		*v= node1(FAIL);
	} else if (quit_keyword()) {
		upto(ceol, K_QUIT);
		*v= node1(QUIT);
	} else if (return_keyword())
		*v= node2(RETURN, expr(ceol));
	else if (report_keyword())
		*v= node2(REPORT, test(ceol));
	else if (succeed_keyword()) {
		upto(ceol, K_SUCCEED);
		*v= node1(SUCCEED);
	} else return No;
	return Yes;
}

/* ******************************************************************** */
/*		user_defined_command; refined_command			*/
/* ******************************************************************** */

Forward value hu_actuals();
#ifdef EXT_COMMAND
Forward bool extended_command();
#endif

Hidden bool udr_com(v) parsetree *v; {
	value w;
	if (is_keyword(&w)) {
#ifdef EXT_COMMAND
		if (extended_command(w, v))
			return Yes;
#endif
		if (!in(w, kwlist)) {
			*v= node4(USER_COMMAND,
				copy(w), hu_actuals(ceol, w), Vnil);
			return Yes;
		}
		release(w);
	}
	return No;
}

Hidden value hu_actuals(q, kw) txptr q; value kw; {
	parsetree e; value v, w;
	txptr ftx;
	skipsp(&tx);
	if (!findkw(q, &ftx)) ftx= q;
	e= Text(ftx) ? expr(ftx) : NilTree;
	v= Text(q) ? hu_actuals(q, keyword()) : Vnil;
	w= node5(ACTUAL, kw, e, v, Vnil);
	return w;
}

#ifdef EXT_COMMAND

/* ******************************************************************** */
/*		extended_command					*/
/* ******************************************************************** */

Hidden bool extended_command(w, v) value w, *v; {
	string name, arg; ext *e; int i; value args[MAXEARGS], a;
	txptr ftx, ttx;
	extern bool extcmds; /* Flag set in main by -E option */
	if (!extcmds) return No;
	name= strval(w);
	for (e= extensions; e->e_name != 0; ++e) {
		if (strcmp(e->e_name, name) == 0) break;
	}
	if (e->e_name == 0) return No;
	for (i= 0; i < MAXEARGS && (arg= e->e_args[i]) != 0; ++i) {
		if (arg[1] != '\0') req(arg+1, ceol, &ftx, &ttx);
		else ftx= ceol;
		switch (arg[0]) {
		case 'e': args[i]= expr(ftx); break;
		case 't': args[i]= targ(ftx); break;
		default: psyserr(MESS(2003, "bad entry in extended_command table"));
		}
		if (arg[1] != '\0') tx= ttx;
	}
	if (i == 0) arg= e->e_name;
	else {
		arg= e->e_args[i-1];
		if (arg[1] != '\0') ++arg;
		else switch (arg[0]) {
		case 'e': arg= "expression"; break;
		case 't': arg= "target"; break;
		}
	}
	upto(ceol, arg);
	if (i == 0) a= Vnil;
	else {
		a= mk_compound(i);
		while (--i >= 0) *Field(a, i)= args[i];
	}
	*v= node3(EXTENDED_COMMAND, w, a);
	return Yes;
}

#endif EXT_COMMAND

/* ******************************************************************** */
/*		control_command						*/
/* ******************************************************************** */

Forward parsetree alt_suite();

Visible bool control_command(v) parsetree *v; {
	parsetree e, t; value c;
	txptr ftx, ttx, utx, vtx;
	skipsp(&tx);
	if (if_keyword()) {
			req(":", ceol, &utx, &vtx);
			t= test(utx); tx= vtx;
			if (!is_comment(&c)) c= Vnil;
			*v= node4(IF, t, c, cmd_suite(cur_ilev, Yes));
	} else if (select_keyword()) {
			need(":");
			c= tail_line();
			*v= node3(SELECT, c, alt_suite());
	} else if (while_keyword()) {
			req(":", ceol, &utx, &vtx);
			t= test(utx); tx= vtx;
			if (!is_comment(&c)) c= Vnil;
			*v= node4(WHILE, t, c, cmd_suite(cur_ilev, Yes));
	} else if (for_keyword()) {
			req(":", ceol, &utx, &vtx);
			req(K_IN_for, ceol, &ftx, &ttx);
			if (ttx > utx) {
				parerr(MESS(2004, "IN after colon"));
				ftx= utx= tx; ttx= vtx= ceol;
			}
			idf_cntxt= In_ranger;
			t= idf(ftx); tx= ttx;
			e= expr(utx); tx= vtx;
			if (!is_comment(&c)) c= Vnil;
			*v= node5(FOR, t, e, c, cmd_suite(cur_ilev, Yes));
	} else return No;
	return Yes;
}

/* ******************************************************************** */
/*		alternative_suite					*/
/* ******************************************************************** */

Forward parsetree alt_seq();

Hidden parsetree alt_suite() {
	parsetree v; bool empty= Yes;
	v= alt_seq(&empty, cur_ilev, Yes, No);
	if (empty) parerr(MESS(2005, "no alternative suite where expected"));
	return v;
}

Hidden parsetree 
alt_seq(empty, cil, first, else_encountered) 
	bool *empty, first, else_encountered; intlet cil;
{
	value c; intlet level, l;
	level= ilev(); l= lino;
	if (is_comment(&c)) 
		return node6(TEST_SUITE, mk_integer(l), NilTree, c, NilTree,
				alt_seq(empty, cil, first, else_encountered));
	if ((level == cil && !first) || (level > cil && first)) {
		parsetree v, s; txptr ftx, ttx;
		if (else_encountered)
			parerr(MESS(2006, "after ELSE no more alternatives allowed"));
		findceol();
		req(":", ceol, &ftx, &ttx);
		*empty= No;
		if (else_keyword()) {
			upto(ftx, K_ELSE); tx= ttx;
			if (!is_comment(&c)) c= Vnil;
			s= cmd_suite(level, Yes);
			release(alt_seq(empty, level, No, Yes));
			return node4(ELSE, mk_integer(l), c, s);
		}
		v= test(ftx); tx= ttx;
		if (!is_comment(&c)) c= Vnil;
		s= cmd_suite(level, Yes);
		return node6(TEST_SUITE, mk_integer(l), v, c, s,
				alt_seq(empty, level, No, else_encountered));
	}
	veli();
	return NilTree;
}
