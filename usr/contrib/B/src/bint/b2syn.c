/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2syn.c,v 1.4 85/08/22 16:56:25 timo Exp $
*/

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b2key.h"
#include "b2syn.h"
#include "b3scr.h"
#include "b3env.h"
#include "b3err.h"

#define TABSIZE 8 /* Number of spaces assumed for a tab on a file.
		     (Some editors insist on emitting tabs wherever
		     they can, and always assume 8 spaces for a tab.
		     Even when the editor can be instructed not to
		     do this, beginning users won't know about this,
		     so we'll always assume the default tab size.
		     Advanced users who used to instruct their editor
		     to set tab stops every 4 spaces will have to
		     unlearn this habit.  But that's the price for
		     over-cleverness :-)
		     The indent increment is still 4 spaces!
		     When the B interpreter outputs text, it never uses
		     tabs but always emits 4 spaces for each indent level.
		     Note that the B editor also has a #defined constant
		     which sets the number of spaces for a tab on a file.
		     Finally the B editor *displays* indents as 3 spaces,
		     but *writes* them to the file as 4, so a neat
		     lay-out on the screen may look a bit garbled
		     when the file is printed.  Sorry.  */

Visible txptr tx, ceol;

Visible Procedure skipsp(tx) txptr *tx; {
	while(Space(Char(*tx))) (*tx)++;
}

Visible bool keymark(c) char c; {
	return Cap(c) || Dig(c) || c=='\'' || c=='"';
}

Hidden bool tagmark(c) char c; {
	return Letter(c) || Dig(c) || c=='\'' || c=='"';
}

Hidden bool keytagmark(c) char c; {
	return keymark(c) || Letter(c);
}

Visible bool is_expr(c) char c; {
	return Letter(c) || c == '(' || Dig(c) || c == '.' || 
		c == '\'' || c == '"' || c == '{' || 
		c=='~' || 
		c=='*' || c=='/' || c=='+' || c=='-' || c=='#';
}

/* ******************************************************************** */
/*		cr_text							*/
/* ******************************************************************** */

Visible value cr_text(p, q) txptr p, q; {
	/* Messes with the input line, which is a bit nasty,
	   but considered preferable to copying to a separate buffer */
	value t;
	char save= Char(q);
	Char(q)= '\0';
	t= mk_text(p);
	Char(q)= save;
	return t;
}

/* ******************************************************************** */
/*		find, findceol, req, findrel				*/
/* ******************************************************************** */

#define Ptr	  	(*ftx)
#define Nokeymark 	'+'

Hidden bool E_number(tx) txptr tx; {
	return Char(tx) == 'E' && Dig(Char(tx+1)) && 
		(Dig(Char(tx-1)) || Char(tx-1) == '.');
}

Hidden bool search(find_kw, s, q, ftx, ttx)
	bool find_kw; string s; txptr q, *ftx, *ttx; {

	intlet parcnt= 0; bool outs= Yes, kw= No; char aq, lc= Nokeymark;
	while (Ptr < q) {
		if (outs) {
			if (parcnt == 0) {
				if (find_kw) {
					if (Cap(Char(Ptr)) && !E_number(Ptr))
						return Yes;
				} else if (Char(Ptr) == *s) {
					string t= s+1;
					*ttx= Ptr+1;
					while (*t && *ttx < q) {
						if (*t != Char(*ttx)) break;
						else { t++; (*ttx)++; }
					}
					if (*t);
					else if (Cap(*s) &&
						 (kw || keymark(Char(*ttx))));
					else return Yes;
				}
			}
			switch (Char(Ptr)) {
				case '(': case '{': case '[':
					parcnt++; break;
				case ')': case '}': case ']':	
					if (parcnt > 0) parcnt--; break;
				case '\'': case '"':
					if (!keytagmark(lc)) {
						outs= No; aq= Char(Ptr);
					}
					break;
				default:
					break;
			}
			lc= Char(Ptr); kw= kw ? keymark(lc) : Cap(lc);
		} else {
			if (Char(Ptr) == aq)
				{ outs= Yes; kw= No; lc= Nokeymark; }
			else if (Char(Ptr) == '`') {
				Ptr++;
				if (!search(No, "`", q, &Ptr, ttx)) return No;
			}
		}
		Ptr++;
	}
	return No;
}

/* ********************************************************************	*/

Visible bool find(s, q, ftx, ttx) string s; txptr q, *ftx, *ttx; {
	return search(No, s, q, (*ftx= tx, ftx), ttx);
}

Forward txptr lcol();

Visible Procedure findceol() {
	txptr q= lcol(), ttx;
	if (!find("\\", q, &ceol, &ttx)) ceol= q;
}

Visible Procedure req(s, q, ftx, ttx) string s; txptr q, *ftx, *ttx; {
	if (!find(s, q, ftx, ttx)) {
		parerr2(MESS(2400, "cannot find expected "), MESSMAKE(s));
		*ftx= tx; *ttx= q;
	}
}

Hidden bool relsearch(s, q, ftx) string s; txptr q, *ftx; {
	txptr ttx;
	Ptr= tx;
	while (search(No, s, q, &Ptr, &ttx))
		switch (Char(Ptr)) {
			case '<': if (Char(Ptr+1) == '<') Ptr= ++ttx;
				  else if (Char(Ptr-1) == '>') Ptr= ttx;
				  else return Yes;
				  break;
			case '>': if (Char(Ptr+1) == '<') Ptr= ++ttx;
				  else if (Char(Ptr+1) == '>') Ptr= ++ttx;
				  else return Yes;
				  break;
			case '=': return Yes;
			default:  return No;
		}
	return No;
}

Visible bool findrel(q, ftx) txptr q, *ftx; {
	txptr ttx;
	Ptr= q;
	if (relsearch("<", Ptr, &ttx)) Ptr= ttx;
	if (relsearch(">", Ptr, &ttx)) Ptr= ttx;
	if (relsearch("=", Ptr, &ttx)) Ptr= ttx;
	return Ptr < q;
}

/* ******************************************************************** */
/*		tag, keyword, findkw					*/
/* ******************************************************************** */

Visible bool is_tag(v) value *v; {
	if (!Letter(Char(tx))) return No;
	*v= tag();
	return Yes;
}

Visible value tag() {
	txptr tx0= tx;
	if (!Letter(Char(tx))) parerr(MESS(2401, "no tag where expected"));
	while (tagmark(Char(tx))) tx++;
	return cr_text(tx0, tx);
}

Visible bool is_keyword(v) value *v; {
	if (!Cap(Char(tx))) return No;
	*v= keyword();
	return Yes;
}
	
Visible value keyword() {
	txptr tx0= tx;
	if (!Cap(Char(tx))) parerr(MESS(2402, "no keyword where expected"));
	while (keymark(Char(tx))) tx++;
	return cr_text(tx0, tx);
}

Visible bool findkw(q, ftx) txptr q, *ftx; {
	txptr ttx;
	Ptr= tx;
	return search(Yes, "", q, &Ptr, &ttx);
}

/* ******************************************************************** */
/*		upto, nothing, ateol, atkw, need			*/
/* ******************************************************************** */

Visible Procedure upto(q, s) txptr q; string s; {
	skipsp(&tx);
	if (Text(q)) {
		parerr2(MESS(2403, "something unexpected following "),
			MESSMAKE(s));
		tx= q;
	}
}

Visible bool nothing(q, s) txptr q; string s; {
	if (!Text(q)) {
		if (Char(tx-1) == ' ') tx--;
		parerr2(MESS(2404, "nothing instead of expected "),
			MESSMAKE(s));
		return Yes;
	}
	return No;
}

Hidden bool looked_ahead= No;
Visible intlet cur_ilev;

Visible bool ateol() {
	if (looked_ahead) return Yes;
	skipsp(&tx);
	return Eol(tx);
}

Visible bool atkw(s) string s; {
	txptr tx0= tx;
	while (*s) if (*s++ != Char(tx0++)) return No;
	if (keymark(Char(tx0))) return No;
	tx= tx0;
	return Yes;
}

Visible Procedure need(s) string s; {
	string t= s;
	skipsp(&tx);
	while (*t)
		if (*t++ != Char(tx++)) {
			tx--;
			parerr2(MESS(2405, "according to the syntax I expected "),
				MESSMAKE(s));
			return;
		}
}

/* ******************************************************************** */
/*		buffer handling						*/
/* ******************************************************************** */

Visible txptr first_col;

Visible txptr fcol() { /* the first position of the current line */
	return first_col;
}

Hidden txptr lcol() { /* the position beyond the last character of the line */
	txptr ax= tx;
	while (!Eol(ax)) ax++;
	return ax;
}

Visible intlet ilev() {
	intlet i;
	if (looked_ahead) {
		looked_ahead= No;
		return cur_ilev;
	} else {
		first_col= tx= getline();
		looked_ahead= No;
		lino++;
		f_lino++;
		i= 0;
		while (Space(Char(tx)))
			if (Char(tx++) == ' ') i++;
			else i= (i/TABSIZE+1)*TABSIZE;
		if (Char(tx) == '\\') return cur_ilev= 0;
		if (Char(tx) == '\n') return cur_ilev= 0;
		if (i%4 == 2)
			parerr(MESS(2406, "cannot make out indentation; use tab to indent"));
		return cur_ilev= (i+1)/4; /* small deviation accepted */
	}
}

Visible Procedure veli() { /* After a look-ahead call of ilev */
	looked_ahead= Yes;
}

Visible Procedure first_ilev() { /* initialise read buffer for new input */
	looked_ahead= No;
	VOID ilev();
	findceol();
}

/* ********************************************************************	*/

/* The reserved keywords that a user command may not begin with: */

Visible value kwlist;

Hidden string kwtab[] = {
	K_SHARE, K_CHECK, K_CHOOSE, K_DELETE, K_DRAW, K_FAIL, K_FOR,
	K_HOW_TO, K_IF, K_INSERT, K_PUT, K_QUIT, K_READ, K_REMOVE,
	K_REPORT, K_RETURN, K_SELECT, K_SET_RANDOM, K_SUCCEED,
	K_TEST, K_WHILE, K_WRITE, K_YIELD, K_ELSE,
	""
};

Visible Procedure initsyn() {
	value v;
	string *kw;
	kwlist= mk_elt();
	for (kw= kwtab; **kw != '\0'; kw++) {
		insert(v= mk_text(*kw), &kwlist);
		release(v);
	}
}

Visible Procedure endsyn() {
	release(kwlist); kwlist= Vnil;
}

/* ******************************************************************** */
/*		signs							*/
/* ********************************************************************	*/

Visible string textsign;

Hidden bool la_denum(tx0) txptr tx0; {
	char l, r;
	switch (l= Char(++tx0)) {
		case '/':	r= '*'; break;
		case '*':	r= '/'; break;
		default:	return Yes;
	}
	do if (Char(++tx0) != r) return No; while (Char(++tx0) == l);
	return Yes;
}

#ifdef NOT_USED
Visible bool colon_sign() {
	return Char(tx) == ':' ? (tx++, Yes) : No;
}
#endif

Visible bool comment_sign() {
	return Char(tx) == '\\' ? (tx++, Yes) : No;
}

Visible bool nwl_sign() {
	return Char(tx) == '/' && !la_denum(tx-1) ? (tx++, Yes) : No;
}

Visible bool open_sign() {
	return Char(tx) == '(' ? (tx++, Yes) : No;
}

#ifdef NOT_USED
Visible bool close_sign() {	
	return Char(tx) == ')' ? (tx++, Yes) : No;
}
#endif

#ifdef NOT_USED
Visible bool comma_sign() {
	return Char(tx) == ',' ? (tx++, Yes) : No;
}
#endif

Visible bool point_sign() {
	return Char(tx) == '.' ? (tx++, Yes) : No;
}

Visible bool apostrophe_sign() {
	return Char(tx) == '\'' ? (tx++, textsign= "'", Yes) : No;
}

Visible bool quote_sign() {
	return Char(tx) == '"' ? (tx++, textsign= "\"", Yes) : No;
}

Visible bool conv_sign() {
	return Char(tx) == '`' ? (tx++, Yes) : No;
}

Visible bool curlyopen_sign() {
	return Char(tx) == '{' ? (tx++, Yes) : No;
}

Visible bool curlyclose_sign() {
	return Char(tx) == '}' ? (tx++, Yes) : No;
}

Visible bool sub_sign() {
	return Char(tx) == '[' ? (tx++, Yes) : No;
}

#ifdef NOT_USED
Visible bool bus_sign() {
	return Char(tx) == ']' ? (tx++, Yes) : No;
}
#endif

Visible bool behead_sign() {
	return Char(tx) == '@' ? (tx++, textsign= "@", Yes) : No;
}

Visible bool curtl_sign() {
	return Char(tx) == '|' ? (tx++, textsign= "|", Yes) : No;
}

Visible bool about_sign() {
	return Char(tx) == '~' ? (tx++, textsign= "~", Yes) : No;
}

Visible bool plus_sign() {
	return Char(tx) == '+' ? (tx++, textsign= "+", Yes) : No;
}

Visible bool minus_sign() {
	return Char(tx) == '-' ? (tx++, textsign= "-", Yes) : No;
}

Visible bool times_sign() {
	return Char(tx) == '*' && la_denum(tx)
		? (tx++, textsign= "*", Yes) : No;
}

Visible bool over_sign() {
	return Char(tx) == '/' && la_denum(tx)
		? (tx++, textsign= "/", Yes) : No;
}

Visible bool power_sign() {
	return Char(tx) == '*' && Char(tx+1) == '*' && la_denum(tx+1)
		? (tx+= 2, textsign= "**", Yes) : No;
}

Visible bool numtor_sign() {
	return Char(tx) == '*' && Char(tx+1) == '/' && la_denum(tx+1)
		? (tx+= 2, textsign= "*/", Yes) : No;
}

Visible bool denomtor_sign() {
	return Char(tx) == '/' && Char(tx+1) == '*' && la_denum(tx+1)
		? (tx+= 2, textsign= "/*", Yes) : No;
}

Visible bool join_sign() {
	return Char(tx) == '^' && Char(tx+1) != '^'
		? (tx++, textsign= "^", Yes) : No;
}

Visible bool reptext_sign() {
	return Char(tx) == '^' && Char(tx+1) == '^'
		? (tx+= 2, textsign= "^^", Yes) : No;
}

Visible bool leftadj_sign() {
	return Char(tx) == '<' && Char(tx+1) == '<'
		? (tx+= 2, textsign= "<<", Yes) : No;
}

Visible bool center_sign() {
	return Char(tx) == '>' && Char(tx+1) == '<' 
		? (tx+= 2, textsign= "><", Yes) : No;
}

Visible bool rightadj_sign() {
	return Char(tx) == '>' && Char(tx+1) == '>' 
		? (tx+= 2, textsign= ">>", Yes) : No;
}

Visible bool number_sign() {		
	return Char(tx) == '#' ? (tx++, textsign= "#", Yes) : No;
}

Visible bool less_than_sign() {
	return Char(tx) == '<' && Char(tx+1) != '=' &&
		Char(tx+1) != '>' && Char(tx+1) != '<'
		? (tx++, Yes) : No;
}

Visible bool at_most_sign() {	
	return Char(tx) == '<' && Char(tx+1) == '=' ? (tx+= 2, Yes) : No;
}

Visible bool equals_sign() {
	return Char(tx) == '=' ? (tx++, Yes) : No;
}

Visible bool unequal_sign() {
	return Char(tx) == '<' && Char(tx+1) == '>' ? (tx+= 2, Yes) : No;
}

Visible bool at_least_sign() {
	return Char(tx) == '>' && Char(tx+1) == '=' ? (tx+= 2, Yes) : No;
}

Visible bool greater_than_sign() {
	return Char(tx) == '>' && Char(tx+1) != '='
		&& Char(tx+1) != '>' && Char(tx+1) != '<'
		? (tx++, Yes) : No;
}

Visible bool dyamon_sign() {
	return plus_sign() || minus_sign() || number_sign();
}

Visible bool dya_sign() {
	return times_sign() || over_sign() || power_sign() || 
		join_sign() || reptext_sign() || 
		leftadj_sign() || center_sign() || rightadj_sign();
}

Visible bool mon_sign() {
	return about_sign() || numtor_sign() || denomtor_sign();
}

Visible bool trim_sign() {
	return behead_sign() || curtl_sign();
}

Visible bool check_keyword()		{ return atkw(K_CHECK); }
Visible bool choose_keyword()		{ return atkw(K_CHOOSE); }
Visible bool delete_keyword() 		{ return atkw(K_DELETE); }
Visible bool draw_keyword() 		{ return atkw(K_DRAW); }
Visible bool insert_keyword() 		{ return atkw(K_INSERT); }
Visible bool put_keyword() 		{ return atkw(K_PUT); }
Visible bool read_keyword() 		{ return atkw(K_READ); }
Visible bool remove_keyword() 		{ return atkw(K_REMOVE); }
Visible bool setrandom_keyword() 	{ return atkw(K_SET_RANDOM); }
Visible bool write_keyword() 		{ return atkw(K_WRITE); }
Visible bool fail_keyword()		{ return atkw(K_FAIL); }
Visible bool quit_keyword() 		{ return atkw(K_QUIT); }
Visible bool return_keyword() 		{ return atkw(K_RETURN); }
Visible bool report_keyword() 		{ return atkw(K_REPORT); }
Visible bool succeed_keyword() 		{ return atkw(K_SUCCEED); }
Visible bool if_keyword() 		{ return atkw(K_IF); }
Visible bool select_keyword() 		{ return atkw(K_SELECT); }
Visible bool while_keyword() 		{ return atkw(K_WHILE); }
Visible bool for_keyword() 		{ return atkw(K_FOR); }
Visible bool else_keyword() 		{ return atkw(K_ELSE); }
#ifdef NOT_USED
Visible bool and_keyword() 		{ return atkw(K_AND); }
Visible bool or_keyword() 		{ return atkw(K_OR); }
#endif
Visible bool not_keyword() 		{ return atkw(K_NOT); }
Visible bool some_keyword() 		{ return atkw(K_SOME); }
Visible bool each_keyword() 		{ return atkw(K_EACH); }
Visible bool no_keyword() 		{ return atkw(K_NO); }
Visible bool how_to_keyword() 		{ return atkw(K_HOW_TO); }
Visible bool yield_keyword() 		{ return atkw(K_YIELD); }
Visible bool test_keyword() 		{ return atkw(K_TEST); }
Visible bool share_keyword() 		{ return atkw(K_SHARE); }
