/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3scr.c,v 1.4 85/08/22 16:58:54 timo Exp $
*/

/* B input/output handling */

#include "b.h"
#include "b0fea.h"
#include "b1mem.h"
#include "b1obj.h"
#include "b0con.h" /*for CLEAR_EOF*/
#include "b2nod.h"
#include "b2syn.h"
#include "b2par.h"
#include "b3scr.h"
#include "b3err.h"
#include "b3fil.h"
#include "b3typ.h"
#include "b3env.h"
#include "b3sem.h"
#include "b3int.h"
#ifdef SETJMP
#include <setjmp.h>
#endif

Visible bool interactive;
Visible bool rd_interactive;
Visible value iname= Vnil;	/* input name */
Visible bool filtered= No;
Visible bool outeractive;
#ifdef SETJMP
Visible bool awaiting_input= No;
Visible jmp_buf read_interrupt;
#endif
Visible bool at_nwl= Yes;	/*Yes if currently at the start of an output line*/
Hidden bool woa, wnwl;		/*was outeractive, was at_nwl */
Hidden bool last_was_text= No;	/*Yes if last value written was a text*/

Visible bool Eof;
FILE *ofile= stdout;
FILE *ifile;	 	/* input file */
FILE *sv_ifile;		/* copy of ifile for restoring after reading unit */

/******************************* Output *******************************/

#ifndef INTEGRATION

Hidden Procedure putch(c) char c; {
	if (still_ok) {
		putc(c, ofile);
		if (c == '\n') at_nwl= Yes;
		else at_nwl= No;
	}
}

#else

Hidden int ocol;	/* Current output column */

Hidden Procedure putch(c) char c; {
	if (still_ok) {
		putc(c, ofile);
		if (c == '\n') { at_nwl= Yes; ocol= 0; }
		else {
			if (at_nwl) { ocol= 0; at_nwl= No;}
			++ocol;
		}
	}
}

#endif

Visible Procedure newline() {
	putch('\n');
	fflush(stdout);
}

Hidden Procedure line() {
	if (!at_nwl) newline();
}

Visible Procedure wri_space() {
	putch(' ');
}

Visible Procedure writ(v) value v; {
	wri(v, Yes, Yes, No);
	fflush(stdout);
}

#define Putch_sp() {if (!perm) putch(' ');}

Hidden int intsize(v) value v; {
	value s= size(v); int len=0;
	if (large(s)) error(MESS(3800, "value too big to output"));
	else len= intval(s);
	release(s);
	return len;
}

Hidden bool lwt;

Visible Procedure wri(v, coll, outer, perm) value v; bool coll, outer, perm; {
	if (outer && !at_nwl && (!Is_text(v) || !last_was_text)
		  && (!Is_compound(v) || !coll)) putch(' ');
	lwt= No;
	if (Is_number(v)) {
		if (perm) printnum(ofile, v);
		else {
			string cp= convnum(v);
			while(*cp && still_ok) putch(*cp++);
		}
	} else if (Is_text(v)) {
#ifndef INTEGRATION
		wrtext(putch, v, outer ? '\0' : '"');
#else
		value ch; char c; int k, len= Length(v);
#define QUOTE '"'
		if (!outer) putch(QUOTE);
		for (k=0; k<len && still_ok; k++) {
			ch= thof(k+1, v);
			putch(c= charval(ch));
			if (!outer && (c == QUOTE || c == '`'))
				putch(c);
			release(ch);
		}
		if (!outer) putch(QUOTE);
#endif
		lwt= outer;
	} else if (Is_compound(v)) {
		intlet k, len= Nfields(v);
		outer&= coll;
		if (!coll) putch('(');
		for (k=0; k<len && still_ok; k++) {
			wri(*Field(v, k), No, outer, perm);
			if (!Lastfield(k)) {
				if (!outer){
					putch(',');
					Putch_sp();
				}
			}
		}
		if (!coll) putch(')');
	} else if (Is_list(v) || Is_ELT(v)) {
		value ve; int k, len= intsize(v);
		putch('{');
		for (k=0; k<len && still_ok; k++) {
			wri(ve= thof(k+1, v), No, No, perm);
			release(ve);
			if (!Last(k)) {
				putch(';');
				Putch_sp();
			}
		}
		putch('}');
	} else if (Is_table(v)) {
		int k, len= intsize(v);
		putch('{');
		for (k=0; k<len && still_ok; k++) {
			putch('['); wri(*key(v, k), Yes, No, perm);
			putch(']'); putch(':'); Putch_sp();
			wri(*assoc(v, k), No, No, perm);
			if (!Last(k)) {
				putch(';');
				Putch_sp();
			}
		}
		putch('}');
	} else {
		if (bugs || testing) { putch('?'); putch(Type(v)); putch('?'); }
		else syserr(MESS(3801, "writing value of unknown type"));
	}
	last_was_text= lwt;
#ifdef IBMPC
	if (interrupted) clearerr(ofile);
#endif
}

/***************************** Input ****************************************/

Hidden char cmbuf[CMBUFSIZE]; /* for commands */
Hidden char rdbuf[RDBUFSIZE]; /* for READ EG/RAW */

#ifndef INTEGRATION
Visible string cmd_prompt= ">>> "; /* commands  */
Visible string eg_prompt=  "?\b";  /* READ EG   */
Visible string raw_prompt= "?\b";  /* READ RAW  */
Visible string qn_prompt=  "?\b";  /* questions */
#else
Hidden literal cmd_prompt= '>'; /* commands  */
Hidden literal eg_prompt=  'E';  /* READ EG   */
Hidden literal raw_prompt= 'R';  /* READ RAW  */
Hidden literal qn_prompt= 'Y';  /* questions */
Visible literal unit_prompt= ':'; /* units */
Visible literal tar_prompt= '='; /* targets */
#endif

/* Read a line; EOF only allowed if not interactive, in which case eof set */
/* Returns the line input                                                  */
/* This is the only place where a long jump is necessary                   */
/* In other places, interrupts are just like procedure calls, and checks   */
/* of still_ok and interrupted suffice: eventually the stack unwinds to the*/
/* main loop in imm_command(). Here though, an interrupt must actually     */
/* terminate the read. Hence the bool awaiting_input indicating if the     */
/* long jump is necessary or not                                           */

#ifndef INTEGRATION

Hidden txptr read_line(should_prompt, prompt, cmd, eof, eof_message)
 bool should_prompt, cmd, *eof; string prompt, eof_message; {
	txptr buf, rp, bufend; intlet k; bool got= No;
	FILE *f;
	*eof= No;
	if (cmd) { buf= cmbuf; bufend= &cmbuf[CMBUFSIZE-2]; }
	else     { buf= rdbuf; bufend= &rdbuf[RDBUFSIZE-2]; }
#ifdef SETJMP
	if (setjmp(read_interrupt) != 0) {
		awaiting_input= No;
		return buf;
	}
#endif
	while (!got) {
		rp= buf;
#ifdef SETJMP
		awaiting_input= Yes;
#endif
		if (should_prompt) {
			if (cmd) {
				if (outeractive) {
					line();
					at_nwl= No;
				}
			}
			fprintf(stderr, prompt); fflush(stderr);
			f= stdin;
		} else {
			f= ifile;
		}
		while ((k= getc(f)) != EOF && k != '\n') {
			*rp++= k;
			if (rp >= bufend) syserr(MESS(3802, "buffer overflow"));
		}
#ifdef SETJMP
		awaiting_input= No;
#endif
		got= Yes; *rp++= '\n'; *rp= '\0';
		if (k == EOF) {
			if (should_prompt) {
				if (filtered) {
					bye(0); /*Editor has died*/
				} else {
					fprintf(stderr, "\r*** %s\n", eof_message);
					CLEAR_EOF;
					if (outeractive) at_nwl= Yes;
					got= No;
				}
			} else *eof= Yes;
		}
	}
	if (should_prompt && outeractive && k == '\n') at_nwl= Yes;
	return buf;
}

#else INTEGRATION

Hidden intlet
rd_fileline(nbuf, file, nbufend)
	string nbuf, nbufend;
	FILE *file;
{
	intlet k;
	while ((k= getc(file)) != EOF && k != '\n') {
		*nbuf++= k;
		if (nbuf >= nbufend)
			syserr(MESS(3803, "buffer overflow rd_fileline()"));
	}
	*nbuf++= '\n'; *nbuf= '\0';
	return k;
}

Hidden intlet
rd_bufline(nbuf, obuf, nbufend)
	string nbuf, *obuf, nbufend;
{
	while (**obuf && **obuf != '\n') {
		*nbuf++= **obuf; ++*obuf;
		if (nbuf >= nbufend)
			syserr(MESS(3804, "buffer overflow rd_bufline()"));
	}
	*nbuf++= '\n'; *nbuf= '\0';
	if (**obuf)  { ++*obuf; return '\n';}
	else return EOF;
}

Hidden string edcmdbuf;

Hidden txptr
read_line(should_prompt, prompt, cmd, eof, eof_message)
	bool should_prompt, cmd, *eof; literal prompt; string eof_message;
{
	txptr buf, rp, bufend; intlet k, indent= 0; bool got= No;
	static string pedcmdbuf;
	if (prompt == eg_prompt || prompt == raw_prompt) indent= ocol;
	*eof= No;
	if (cmd) { buf= cmbuf; bufend= &cmbuf[CMBUFSIZE-2]; }
	else	 { buf= rdbuf; bufend= &rdbuf[RDBUFSIZE-2]; }
#ifdef SETJMP
	if (setjmp(read_interrupt) != 0) {
		awaiting_input= No;
		return buf;
	}
#endif
	while (!got) {
		rp= buf; got= Yes;
#ifdef SETJMP
		awaiting_input= Yes;
#endif
		if (!should_prompt) {
			k= rd_fileline(rp, ifile, bufend);
			if (k == EOF) *eof= Yes;
		} else {
			if (!edcmdbuf) {
				if (cmd && outeractive) { line(); at_nwl= No; }
				btop(&edcmdbuf, 0, prompt, indent);
				pedcmdbuf= edcmdbuf;
			}
			k= rd_bufline(rp, &pedcmdbuf, bufend);
			if (k == EOF) {
				freemem((ptr) edcmdbuf);
				edcmdbuf= (string) NULL;
				if (prompt != '>') got= No;
			} 
		}
#ifdef SETJMP
		awaiting_input= No;
#endif
	}

	if (should_prompt && outeractive && k == '\n') at_nwl= Yes;
	return buf;
}

#endif INTEGRATION

/* Rather over-fancy routine to ask the user a question */
/* Will anybody discover that you're only given 4 chances? */

Hidden char USE_YES_OR_NO[]=
 "Answer with yes or no (or use interrupt to duck the question)";

Hidden char LAST_CHANCE[]=
 "This is your last chance. Take it. I really don't know what you want.\n\
    So answer the question";

Hidden char NO_THEN[]=
 "Well, I shall assume that your refusal to answer the question means no!";

Visible bool is_intended(m) string m; {
	char answer; intlet try; txptr tp; bool eof;
	if (!interactive) return Yes;
	if (outeractive) line();
	for (try= 1; try<=4; try++){
		if (try == 1 || try == 3) fprintf(stderr, "*** %s\n", m);
		tp= read_line(Yes, qn_prompt, No, &eof, USE_YES_OR_NO);
		skipsp(&tp);
		answer= Char(tp);
		if (answer == 'y' || answer == 'Y') return Yes;
		if (answer == 'n' || answer == 'N') return No;
		if (outeractive) line();
		fprintf(stderr, "*** %s\n",
			try == 1 ? "Please answer with yes or no" :
			try == 2 ? "Just yes or no, please" :
			try == 3 ? LAST_CHANCE :
			NO_THEN);
	} /* end for */
	return No;
}

/* Read_eg uses evaluation but it shouldn't.
   Wait for a more general mechanism. */

Visible Procedure read_eg(l, t) loc l; btype t; {
	context c; parsetree code;
	parsetree r= NilTree; value rv= Vnil; btype rt= Vnil;
	envtab svprmnvtab= Vnil;
	txptr fcol_save= first_col, tx_save= tx;
	do {
		still_ok= Yes;
		sv_context(&c);
		if (cntxt != In_read) {
			release(read_context.uname);
			sv_context(&read_context);
		}
		svprmnvtab= prmnvtab == Vnil ? Vnil : prmnv->tab;
		/* save scratch-pad copy because of following setprmnv() */
		setprmnv();
		cntxt= In_read;
		first_col= tx= read_line(rd_interactive, eg_prompt, No,
			&Eof, "use interrupt to abort READ command");
		if (still_ok && Eof)
			error(MESS(3805, "End of file encountered during READ command"));
		if (!rd_interactive) f_lino++;
		if (still_ok) {
			findceol();
			r= expr(ceol);
			if (still_ok) fix_nodes(&r, &code);
			rv= evalthread(code); release(r);
			rt= still_ok ? valtype(rv) : Vnil;
			if (svprmnvtab != Vnil) {
				prmnvtab= prmnv->tab;
				prmnv->tab= svprmnvtab;
			}
			set_context(&c);
			if (still_ok) must_agree(t, rt,
	MESS(3806, "type of expression does not agree with that of EG sample"));
			release(rt);
		}
		if (!still_ok && rd_interactive && !interrupted)
			fprintf(stderr, "*** Please try again\n");
	} while (!interrupted && !still_ok && rd_interactive);
	if (still_ok) put(rv, l);
	first_col= fcol_save;
	tx= tx_save;
	release(rv);
}

Visible Procedure read_raw(l) loc l; {
	value r; bool eof;
	txptr line= read_line(rd_interactive, raw_prompt, No, &eof, 
			"use interrupt to abort READ t RAW");
	if (still_ok && eof) error(MESS(3807, "End of file encountered during READ t RAW"));
	if (!rd_interactive) f_lino++;
	if (still_ok) {
		txptr rp= line;
		while (*rp != '\n') rp++;
		*rp= '\0';
		r= mk_text(line);
		put(r, l);
		release(r);
	}
}

Visible txptr getline() {
	bool should_prompt=
		interactive && sv_ifile == ifile;
	return read_line(should_prompt, cmd_prompt, Yes, &Eof,
			"use QUIT to end session");
}

/******************************* Files ******************************/

Visible Procedure redirect(of) FILE *of; {
	ofile= of;
	if (of == stdout) {
		outeractive= woa;
		at_nwl= wnwl;
	} else {
		woa= outeractive; outeractive= No;
		wnwl= at_nwl; at_nwl= Yes;
	}
}

Visible Procedure vs_ifile() {
	ifile= sv_ifile;
}

Visible Procedure re_screen() {
	sv_ifile= ifile;
	interactive= f_interactive(ifile) || (ifile == stdin && filtered);
	Eof= No;
}

/* initscr is a reserved name of CURSES */
Visible Procedure init_scr() {
	outeractive= f_interactive(stdout) || filtered;
	rd_interactive= f_interactive(stdin) || filtered;
	rdbuf[0]= '\n'; tx= rdbuf;
}

Visible Procedure
endscr()
{
#ifdef INTEGRATION
	if (edcmdbuf) {
		freemem((ptr) edcmdbuf);
		edcmdbuf= (string) NULL;
	}
#endif
}
