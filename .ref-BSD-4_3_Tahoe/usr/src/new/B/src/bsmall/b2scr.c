/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2scr.c,v 1.2 84/07/17 09:51:48 frank Exp $ */

/* B screen handling */
#include "b.h"
#include "b1obj.h"
#include "b0con.h" /*for CLEAR_EOF*/
#include "b2scr.h"
#include "b2fil.h"

bool interactive, read_interactive, outeractive, filtered;
bool at_nwl= Yes; /*Yes if currently at the start of an output line*/
bool Eof, Eof0;
FILE *ofile= stdout; bool woa, wnwl;
FILE *ifile;	 	/* input file */
FILE *sv_ifile;		/* copy of ifile for restoring after switching to read unit */
value iname= Vnil;	/* input name */

jmp_buf reading[MAX_NMB_ACT_READS];
intlet active_reads; 

string cmd_prompt= ">>> "; /* commands  */
string eg_prompt=  "?\b";  /* READ EG   */
string raw_prompt= "?\b";  /* READ RAW  */
string qn_prompt=  "?\b";  /* questions */


#define USE_YES_OR_NO \
"\r*** Answer with yes or no (or use interrupt to duck the question)\n"

#define LAST_CHANCE \
"*** This is your last chance. Take it. I really don't know what you want if\n\
    %s\n\
    So answer the question\n"

#define NO_THEN \
"*** Well, I shall assume that your refusal to answer the question means no!\n"

Visible bool is_intended(m) string m; {
	char answer; intlet try, k;
	if (!interactive) return Yes;
	if (outeractive) line();
	fprintf(stderr, "*** %s\n", m);
	for (try= 1; try<=4; try++){
		answer= '?';
		if (outeractive) at_nwl= No;
		fprintf(stderr, qn_prompt);
		while ((k= getchar()) != EOF && k != '\n') {
			if ((k == ' ' || k == '\t'));
			else if (answer == '?') answer= k;
		}
		if (k == EOF) {
			if (filtered) bye(0); /* Editor has died */
			CLEAR_EOF;
		}
		if (k == EOF && try < 4) {
			fprintf(stderr, USE_YES_OR_NO);
			if (outeractive) at_nwl= Yes;
		} else {
			if (outeractive && k == '\n') at_nwl= Yes;
			if (answer == 'y' || answer == 'Y') return Yes;
			if (answer == 'n' || answer == 'N') return No;
			if (outeractive) line();
			fprintf(stderr,
				try == 1 ? "*** Please answer with yes or no\n" :
				try == 2 ? "*** Just yes or no, please\n" :
				try == 3 ? LAST_CHANCE :
				NO_THEN, m);
		}
	} /* end for */
	return No;
}

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

Hidden Procedure putch(c) char c; {
	putc(c, ofile);
	if (c == '\n') at_nwl= Yes;
	else at_nwl= No;
}

Visible Procedure newline() {
	putch('\n');
	fflush(stdout);
}

Visible Procedure line() {
	if (!at_nwl) newline();
}

Visible Procedure wri_space() { /* Experiment: no space before outer strings */
	if (!at_nwl) putch(' ');
}

Visible Procedure writ(v) value v; {
	wri(v, Yes, Yes, No);
	fflush(stdout);
}

#define Putch_sp() {if (!perm) putch(' ');}

bool lwt, last_was_text= No;

Visible Procedure wri(v, coll, outer, perm) value v; bool coll, outer, perm; {
	if (outer && !at_nwl && (!Is_text(v) || !last_was_text)
		  && (!Is_compound(v) || !coll)) putch(' ');
	lwt= No;
	if (Is_number(v)) {
		if (perm) printnum(ofile, v);
		else {
			string cp= convnum(v);
			while(*cp) putch(*cp++);
		}
	} else if (Is_text(v)) {
		value ch; char c;
		value s= size(v); int k, len;
		if (large(s)) error("text too long to output");
		else { len= intval(s); release(s); }
		lwt= outer;
		if (!outer) putch('\'');
		k_Over_len {
			ch= thof(k+1, v);
			putch(c= charval(ch));
			if (!outer && (c == '\'' || c == '`'))
				putch(c);
			release(ch);
		}
		if (!outer) putch('\'');
	} else if (Is_compound(v)) {
		intlet k, len= Nfields(v);
		outer&= coll;
		if (!coll) putch('(');
		k_Overfields {
			wri(*field(v, k), No, outer, perm);
			if (!Lastfield(k)) {
				if (!outer){
					putch(',');
					Putch_sp();
				}
			}
		}
		if (!coll) putch(')');
	} else if (Is_list(v) || Is_ELT(v)) {
		value ve; value s= size(v); int k, len;
		if (large(s)) error("list too big to output");
		else { len= intval(s); release(s); }
		putch('{');
		k_Over_len {
			wri(ve= thof(k+1, v), No, No, perm);
			release(ve);
			if (!Last(k)) {
				putch(';');
				Putch_sp();
			}
		}
		putch('}');
	} else if (Is_table(v)) {
		value s= size(v); int k, len;
		if (large(s)) error("table too big to output");
		else { len= intval(s); release(s); }
		putch('{');
		k_Over_len {
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
		if (bugs) { putch('?'); putch(Type(v)); putch('?'); }
		else syserr("writing value of unknown type");
	}
	last_was_text= lwt;
}

Visible Procedure vs_ifile() {
	ifile= sv_ifile;
}

Visible Procedure re_files() {
	if (interactive && sv_ifile != ifile) {
		if (ifile != stdin) fclose(ifile);
		vs_ifile();
		Eof= Eof0= No;
	}
}

Visible Procedure initscr() {
	read_interactive= f_interactive(stdin) || filtered;
	outeractive= f_interactive(stdout) || filtered;
}

Visible Procedure re_screen() {
	sv_ifile= ifile;
	interactive= f_interactive(ifile) || (ifile == stdin && filtered);
}
