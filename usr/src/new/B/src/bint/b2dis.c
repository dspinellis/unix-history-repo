/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2dis.c,v 1.4 85/08/22 16:54:27 timo Exp $
*/

#include "b.h"
#include "b1obj.h"
#include "b2par.h"
#include "b2nod.h"

FILE *d_file;

#define Indent	"    "

Hidden intlet ilevel= 0;

Hidden Procedure set_ilevel() {
	intlet i;
	for (i= 0; i<ilevel; i++) fprintf(d_file, Indent);
}

Hidden bool new_line= Yes, in_comment= No;

Hidden Procedure put_char(c) char c; {
	if (new_line && !in_comment) set_ilevel();
	putc(c, d_file);
	new_line= No;
}

Hidden Procedure put_string(s) string s; {
	if (new_line && !in_comment) set_ilevel();
	fprintf(d_file, "%s", s);
	new_line= No;
}

Hidden Procedure put_newline() {
	putc('\n', d_file);
	new_line= Yes;
}

#define Putspace	put_char(' ')

/* ******************************************************************** */

Hidden bool displ_one_line, stop_displ;

Visible Procedure display(f, v, one_line) FILE *f; parsetree v; bool one_line; {
	d_file= f;
	ilevel= 0;
	displ_one_line= one_line;
	stop_displ= No;
	new_line= !one_line;
	displ(v);
	if (!new_line) put_newline();
}

/* ******************************************************************** */

char *text[] = {
	/* HOW_TO */		"HOW'TO #h1:#c2#b34",
	/* YIELD */		"YIELD 2:#c3#b45",
	/* TEST */		"TEST 2:#c3#b45",
	/* REFINEMENT */	"0:#c1#b23",
	/* SUITE */		"1#c23",

	/* PUT */		"PUT 0 IN 1",
	/* INSERT */		"INSERT 0 IN 1",
	/* REMOVE */		"REMOVE 0 FROM 1",
	/* CHOOSE */		"CHOOSE 0 FROM 1",
	/* DRAW */		"DRAW 0",
	/* SET_RANDOM */	"SET'RANDOM 0",
	/* DELETE */		"DELETE 0",
	/* CHECK */		"CHECK 0",
	/* SHARE */		"SHARE 0",

	/* WRITE */		"WRITE #j",
	/* READ */		"READ 0 EG 1",
	/* READ_RAW */		"READ 0 RAW",

	/* IF */		"IF 0:#c1#b2",
	/* WHILE */		"WHILE 0:#c1#b2",
	/* FOR */		"FOR 0 IN 1:#c2#b3",

	/* SELECT */		"SELECT:#c0#b1",
	/* TEST_SUITE */	"1#d:#c2#b34",
	/* ELSE */		"ELSE:#c1#b2",

	/* QUIT */		"QUIT",
	/* RETURN */		"RETURN 0",
	/* REPORT */		"REPORT 0",
	/* SUCCEED */		"SUCCEED",
	/* FAIL */		"FAIL",

	/* USER_COMMAND */	"#h1",
	/* EXTENDED_COMMAND */	"0 ...",

	/* TAG */		"0",
	/* COMPOUND */		"(0)",
	/* COLLATERAL */	"#a0",
	/* SELECTION */ 	"0[1]",
	/* BEHEAD */		"0@1",
	/* CURTAIL */		"0|1",
	/* UNPARSED */		"1",
	/* MONF */		"#l",
	/* DYAF */		"#k",
	/* NUMBER */		"1",
	/* TEXT_DIS */		"#e",
	/* TEXT_LIT */		"#f",
	/* TEXT_CONV */ 	"`0`1",
	/* ELT_DIS */		"{}",
	/* LIST_DIS */		"{#i0}",
	/* RANGE_DIS */ 	"{0..1}",
	/* TAB_DIS */		"{#g0}",
	/* AND */		"0 AND 1",
	/* OR */		"0 OR 1",
	/* NOT */		"NOT 0",
	/* SOME_IN */		"SOME 0 IN 1 HAS 2",
	/* EACH_IN */		"EACH 0 IN 1 HAS 2",
	/* NO_IN */		"NO 0 IN 1 HAS 2",
	/* SOME_PARSING */	"SOME 0 PARSING 1 HAS 2",
	/* EACH_PARSING */	"EACH 0 PARSING 1 HAS 2",
	/* NO_PARSING */	"NO 0 PARSING 1 HAS 2",
	/* MONPRD */		"0 1",
	/* DYAPRD */		"0 1 2",
	/* LESS_THAN */ 	"0 < 1",
	/* AT_MOST */		"0 <= 1",
	/* GREATER_THAN */	"0 > 1",
	/* AT_LEAST */		"0 >= 1",
	/* EQUAL */		"0 = 1",
	/* UNEQUAL */		"0 <> 1",
	/* Nonode */		"",

	/* TAGformal */ 	"0",
	/* TAGlocal */		"0",
	/* TAGglobal */ 	"0",
	/* TAGmystery */	"0",
	/* TAGrefinement */	"0",
	/* TAGzerfun */ 	"0",
	/* TAGzerprd */ 	"0",
};

#define Is_digit(d) ((d) >= '0' && (d) <= '9')
#define Fld(v, t) *Branch(v, (*(t) - '0') + First_fieldnr)

Hidden Procedure displ(v) value v; {
	if (Is_text(v)) put_string(strval(v));
	else if (Is_parsetree(v)) {
		string t= text[nodetype(v)];
		while (*t) {
			if (Is_digit(*t)) displ(Fld(v, t));
			else if (*t == '#') {
				special(v, &t);
				if (stop_displ) return;
			} else put_char(*t);
			t++;
		}
	}
}

Hidden Procedure special(v, t) parsetree v; string *t; {
	(*t)++;
	switch (**t) {
		case 'a':       d_collateral(Fld(v, ++*t)); break;
		case 'b':       indent(Fld(v, ++*t)); break;
		case 'c':       d_comment(Fld(v, ++*t)); break;
		case 'd':       /* test suite */
				(*t)++;
				if (!new_line) /* there was a command */
					put_char(**t);
				break;
		case 'e':       d_textdis(v); break;
		case 'f':       d_textlit(v); break;
		case 'g':       d_tabdis(Fld(v, ++*t)); break;
		case 'h':       d_actfor_compound(Fld(v, ++*t)); break;
		case 'i':       d_listdis(Fld(v, ++*t)); break;
		case 'j':       d_write(v); break;
		case 'k':       d_dyaf(v); break;
		case 'l':       d_monf(v); break;
	}
}

Hidden Procedure indent(v) parsetree v; {
	if (displ_one_line) { stop_displ= Yes; return; }
	ilevel++;
	displ(v);
	ilevel--;
}

Hidden bool no_space_before_comment(v) value v; {
	value c, t; bool b;
	c= curtail(v, one); t= mk_text("\\");
	b= compare(c, t) == 0;
	release(c); release(t);
	return b;
}


Hidden Procedure d_comment(v) value v; {
	if ( v != Vnil) {
		in_comment= Yes;
		if (!new_line && no_space_before_comment(v)) Putspace;
		displ(v);
		in_comment= No;
	}
	if (!new_line) put_newline();
}

Hidden value quote= Vnil;

Hidden Procedure d_textdis(v) parsetree v; {
	value s_quote= quote;
	quote= *Branch(v, XDIS_QUOTE);
	displ(quote);
	displ(*Branch(v, XDIS_NEXT));
	displ(quote);
	quote= s_quote;
}

Hidden Procedure d_textlit(v) parsetree v; {
	value w;
	displ(w= *Branch(v, XLIT_TEXT));
	if (character(w)) {
		value c= mk_text("`");
		if (compare(quote, w) == 0 || compare(c, w) == 0) displ(w);
		release(c);
	}
	displ(*Branch(v, XLIT_NEXT));
}

Hidden Procedure d_tabdis(v) value v; {
	intlet k, len= Nfields(v);
	k_Over_len {
		if (k>0) put_string("; ");
		put_char('[');
		displ(*Field(v, k));
		put_string("]: ");
		displ(*Field(v, ++k));
	}
}

Hidden Procedure d_collateral(v) value v; {
	intlet k, len= Nfields(v);
	k_Over_len {
		if (k>0) put_string(", ");
		displ(*Field(v, k));
	}
}

Hidden Procedure d_listdis(v) value v; {
	intlet k, len= Nfields(v);
	k_Over_len {
		if (k>0) put_string("; ");
		displ(*Field(v, k));
	}
}

Hidden Procedure d_actfor_compound(v) value v; {
	while (v != Vnil) {
		displ(*Branch(v, ACT_KEYW));
		if (*Branch(v, ACT_EXPR) != Vnil) {
			Putspace;
			displ(*Branch(v, ACT_EXPR));
		}
		v= *Branch(v, ACT_NEXT);
		if (v != Vnil) Putspace;
	}
}

Hidden Procedure d_write(v) parsetree v; {
	value l_lines, e, r_lines;
	l_lines= *Branch(v, WRT_L_LINES);
	e= *Branch(v, WRT_EXPR);
	r_lines= *Branch(v, WRT_R_LINES);
	displ(l_lines);
	if (e != NilTree) {
		value n= size(l_lines);
		if (intval(n) > 0) Putspace;
		release(n);
		displ(e);
		n= size(r_lines);
		if (intval(n) > 0) Putspace;
		release(n);
	}
	displ(r_lines);
}

Hidden Procedure d_dyaf(v) parsetree v; {
	parsetree l, r; value name;
	l= *Branch(v, DYA_LEFT);
	r= *Branch(v, DYA_RIGHT);
	name= *Branch(v, DYA_NAME);
	displ(l);
	if (is_b_tag(name) || nodetype(r) == MONF) {
		Putspace;
		displ(name);
		Putspace;
	}
	else displ(name);
	displ(r);
}

Hidden Procedure d_monf(v) parsetree v; {
	parsetree r; value name;
	name= *Branch(v, MON_NAME);
	r= *Branch(v, MON_RIGHT);
	displ(name);
	if (is_b_tag(name)) {
		switch (nodetype(r)) {
			case MONF:
				if (!is_b_tag(*Branch(r, MON_NAME))) break;
			case SELECTION:
			case BEHEAD:
			case CURTAIL:
			case TAG:
			case TAGformal:
			case TAGlocal:
			case TAGglobal:
			case TAGmystery:
			case TAGrefinement:
			case TAGzerfun:
			case TAGzerprd:
			case NUMBER:
			case TEXT_DIS:
				Putspace;
				break;
			default:
				break;
		}
	}
	displ(r);
}
