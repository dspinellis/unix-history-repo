/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2syn.c,v 1.1 84/06/28 00:49:21 timo Exp $ */

/* General parsing routines for B interpreter */
#include "b.h"
#include "b1obj.h"
#include "b0con.h" /*for CLEAR_EOF*/
#include "b2env.h"
#include "b2scr.h"
#include "b2syn.h"

Visible Procedure upto(q, ff) txptr q; string ff; {
	Skipsp(tx);
	if (tx < q) parerr("something unexpected following ", ff);
}

Visible Procedure nothing(q, xp) txptr q; string xp; {
	if (tx >= q) {
		if (Char(tx-1) == ' ') tx--;
		parerr("nothing instead of expected ", xp);
	}
}

Visible bool ateol() {
	Skipsp(tx);
	if (Ceol(tx)) {
		To_eol(tx);
		return Yes;
	}
	return No;
}

#define Where_inside(r, t) \
	register txptr ttx= tx; char lc= '+', q; \
	register intlet parcnt= 0; register bool outs= Yes; bool kw= No; \
	while (r) \
	if (outs) { \
		if (parcnt == 0 && (t))
#define Otherwise \
		if (Char(ttx) == '(' || Char(ttx) == '[' || Char(ttx) == '{') \
			parcnt++; \
		else if (Char(ttx) == ')' || Char(ttx) == ']' || Char(ttx) == '}') { \
			if (parcnt > 0) parcnt--; \
		} else if ((Char(ttx) == '\'' || Char(ttx) == '"') && !Keytagmark(lc)) { \
			outs= No; q= Char(ttx); \
		} \
		lc= Char(ttx++); kw= kw ? Keymark(lc) : Cap(lc); \
	} else { \
		if (Char(ttx) == q) { \
			outs= Yes; kw= No; lc= '+'; \
		} else if (!outs && Char(ttx) == '`') { \
			txptr tx0= tx, yx, zx; \
			tx= ttx+1; \
			req("`", lcol(), &yx, &zx); \
			ttx= yx; tx= tx0; \
		} \
		ttx++; \
	}

Visible Procedure findceol() {
	Where_inside (!Eol(ttx), Char(ttx) == '\\') {
		ceol= ttx;
		return;
	} Otherwise ceol= ttx;
}

Visible bool atkw(ss) register string ss; {
	register txptr tp= tx;
	while (*ss) if (*ss++ != Char(tp++)) return No;
	if (Keymark(Char(tp))) return No;
	tx= tp;
	return Yes;
}

Visible Procedure need(ss) string ss; {
	register string sp= ss;
	Skipsp(tx);
	while (*sp) if (*sp++ != Char(tx++))
		pprerr("according to the syntax I expected ", ss);
}

Visible Procedure thought(c) register char c; {
	Skipsp(tx);
	if (Char(tx++) != c) syserr("I'm confused; can't trust me own eyes");
}

Visible Procedure reqkw(ss, ptx, qtx) string ss; txptr *ptx, *qtx; {
	Where_inside (!Eol(ttx), Char(ttx) == *ss && !kw) {
		string sp= ss+1;
		*qtx= (*ptx= ttx)+1;
		while (*sp) if (*sp++ != Char((*qtx)++)) goto isnt;
		if (Keymark(Char(*qtx))) goto isnt;
		return;
	}
isnt:	Otherwise parerr("cannot find expected ", ss);
}

Visible Procedure req(ss, utx, ptx, qtx) string ss; txptr utx, *ptx, *qtx; {
	Where_inside (ttx < utx && !Eol(ttx), Char(ttx) == *ss) {
		string sp= ss+1;
		*qtx= (*ptx= ttx)+1;
		while (*sp && *qtx < utx) if (*sp++ != Char((*qtx)++)) goto isnt;
		return;
	}
isnt:	Otherwise parerr("cannot find expected ", ss);
}

Visible bool find(ss, utx, ptx, qtx) string ss; txptr utx, *ptx, *qtx; {
	Where_inside (ttx < utx, Char(ttx) == *ss && !(kw && Cap(*ss))) {
		string sp= ss+1;
		*qtx= (*ptx= ttx)+1;
		while (*sp && *qtx < utx) if (*sp++ != Char((*qtx)++)) goto isnt;
		if (Cap(*ss) && Keymark(Char(*qtx))) goto isnt;
		return Yes;
	}
isnt:	Otherwise return No;
}

Visible intlet count(ss, utx) string ss; txptr utx; {
	intlet cnt= 0;
	Where_inside (ttx < utx, Char(ttx) == *ss) {
		string sp= ss+1; txptr tp= ttx+1;
		while (*sp && tp < utx) if (*sp++ != Char(tp++)) goto isnt;
		cnt++;
	}
isnt:	Otherwise return cnt;
}

#define TAGBUFSIZE 100
char tagbuf[TAGBUFSIZE];
txptr tagbufend= &tagbuf[TAGBUFSIZE];

Visible value tag() {
	txptr tp= tagbuf; value res= Vnil;
	Skipsp(tx);
	if (!Letter(Char(tx))) return Vnil;
	while (Tagmark(Char(tx))) {
		*tp++= Char(tx++);
		if (tp+1 >= tagbufend) {
			*tp= '\0';
			concat_to(&res, tagbuf);
			tp= tagbuf;
		}
	}
	*tp= '\0';
	concat_to(&res, tagbuf);
	return(res);
}

Visible value findkw(u, f, t) txptr u, *f, *t; {
	txptr sp= tx, kp= tagbuf; value word= Vnil;
	while (sp < u && !Cap(Char(sp))) sp++;
	*f= sp;
	while (sp < u && Keymark(Char(sp))) {
		*kp++= Char(sp++);
		if (kp+1 >= tagbufend) {
			*kp= '\0';
			concat_to(&word, tagbuf);
			kp= tagbuf;
		}
	}
	*kp= '\0';
	concat_to(&word, tagbuf);
	*t= sp; /* if no keyword is found, f and t are set to u */
	return(word);
}

Visible value keyword(u) txptr u; {
	txptr f;
	Skipsp(tx);
	if (!Cap(Char(tx))) parerr("no keyword where expected", "");
	return findkw(u, &f, &tx);
}

/* Stream handling */
/* Txbuf holds streams of incoming characters from a file or the keyboard */
/* The current stream is marked by txstart and txend,			  */
/* with tx pointing somewhere in the middle				  */
/* The main stream is for immediate commands, but new ones are created	  */
/* for reading units, and for the read command (when this is implemented) */

#define TXBUFSIZE (1<<13)
char txbuf[TXBUFSIZE];
txptr txbufstart= &txbuf[1], txstart, txend, txbufend= &txbuf[TXBUFSIZE];

intlet alino;

#define Interactive (interactive && sv_ifile == ifile)

Visible txptr fcol() { /* the first position of the current line */
	txptr ax= tx;
	while (!Eol(ax-1) && Char(ax-1) != Eotc) ax--;
	return(ax);
}

Visible txptr lcol() { /* the position beyond the last character of the line */
	txptr ax= tx;
	while (!Eol(ax)) ax++;
	return(ax);
}

Visible Procedure getline() {
	intlet k; bool got;
	if (Eof0) {
		*txend++= Eouc; *txend= Eotc;
		Eof= Yes;
		return;
	}
	alino++;
	got= No;
	while (!got) {
		if (Interactive) {
			if (outeractive) {
				line();
				at_nwl= No;
			}
			fprintf(stderr, cmd_prompt);
		}
		got= Yes;
		while ((k= getc(ifile)) != EOF && k != '\n') {
			*txend++= k;
			if (txend > txbufend-5) syserr("text buffer overflow");
		}
		if (k == EOF && Interactive) {
			if (filtered) bye(0); /* Editor has died */
			fprintf(stderr, "\r*** use QUIT to end session\n");
			CLEAR_EOF;
			if (outeractive) at_nwl= Yes;
			got= No;
		}
	}
	if (Interactive && outeractive && k == '\n') at_nwl= Yes;
	*txend++= '\n'; *txend= Eotc;
	Eof0= k == EOF;
}

Visible intlet ilev(new) bool new; {
	register intlet i;
	lino++;
	if (Char(tx) == Eouc) {
		++tx; /* veli() */
		if(!new)debug("ilev saw Eouc and returns since new == No");
		if (!new) return cur_ilev= 0;
		debug("ilev saw Eouc but proceeds since new == Yes");
	} else if (Char(tx++) != '\n')
		syserr("ilev called when not at end of line");
	if(Char(tx-1)!=Eouc)debug("ilev saw no Eouc");
	if (Char(tx) == Eotc) getline();
	i= 0;
	while (Char(tx) == ' ' || Char(tx) == '\t') {
		if (Char(tx) == ' ') i++;
		else i= (i/4+1)*4;
		tx++;
	}
	if (Char(tx) == '\n') return cur_ilev= 0;
	if (i%4 == 2)
		parerr("cannot make out indentation; use tab to indent", "");
	return cur_ilev= (i+1)/4; /* small deviation accepted */
}

Visible Procedure veli() {
	/* resets tx after look-ahead call of ilev */
	debug("calling veli");
	while (Char(--tx) != '\n' && Char(tx) != Eouc);
	lino--;
	debug("leaving veli");
}

Visible Procedure inistreams() {
	txstart= txbufstart;
	start_stream();
}

Visible Procedure re_streams() {
	if (Char(tx+1) == Eotc) inistreams();
}

Visible Procedure open_stream() {
	txstart= txend+2;
	start_stream();
}

Hidden Procedure start_stream() {
	*(txend= txstart)= Eotc;
	tx= txend-1;
	*tx= Eouc;
}

Visible Procedure close_stream(otx, otxstart) txptr otx, otxstart; {
	txend= txstart-2;
	tx= otx;
	txstart= otxstart;
}
