/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2err.c,v 1.1 84/06/28 00:49:07 timo Exp $ */

/* B error message handling */
#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b2err.h"
#include "b2scr.h"
#include "b2env.h"
#include "b2sem.h" /* for xeq */
#include "b2syn.h"
#include "b2sig.h"
#include "b2fil.h"

jmp_buf main_loop;
bool skipping;


#define Interactive ((cntxt == In_read || active_reads > 0) \
				? read_interactive : interactive)
#define Errout (Interactive ? stderr : stdout)
#define Skip() {if (skipping) proceed();}

Hidden Procedure Line() {
	if (interactive || Errout == stdout) line();
}

Hidden Procedure errmess(m) string m; {
	fprintf(Errout, m);
}

Hidden Procedure core_dump() {
	errmess("*** Core-dump for inspection purposes: ");
	fflush(stdout);
	dump();
}

Visible Procedure bye(ex) int ex; {
	at_nwl= Yes;
	putprmnv();
	exit(ex);
}

Hidden Procedure prname(name) value name; {
	value ch; int k, len;
	if (Is_text(name)) {
		len= length(name);
		k_Over_len {
			ch= thof(k+1, name);
			putc(charval(ch), Errout);
			release(ch);
		}
	} else errmess("???");
}

value erruname= Vnil; literal errutype; intlet errlino= 0;

Hidden intlet display_line(ax) txptr ax; {
	/*displays the line that tx is in, and returns the column number that
	  ax is at.
	*/
	txptr lx= fcol(); intlet ap= -1, p= 0; char c;
	while (!Eol(lx) && Char(lx) != Eotc) {
		if (lx == ax) ap= p;
		c= *lx++;
		if (c == '\t') {
			do { putc(' ', Errout); } while (((++p)%4)!=0);
		} else { putc(c, Errout); p++; }
	}
	putc('\n', Errout);
	if (ap < 0) return p;
	return ap;
}

Hidden Procedure prline(at) bool at; {
	txptr ax= tx; intlet p, ap;
	if (cntxt == In_read || cntxt == In_value) errmess(" in your ");
	else if (cntxt != In_formal)
		fprintf(Errout, " in line %d of your ", lino);
	switch (cntxt) {
	case In_command: errmess("command"); break;
	case In_read: errmess("expression to be read"); break;
	case In_value: errmess("edited value"); break;
	case In_unit:errmess("unit "); prname(uname);
		erruname= uname; errutype= utype; errlino= lino;
		break;
	case In_formal: break;
	case In_prmnv: errmess("permanent environment"); break;
	default: errmess("???\n"); return;
	}
	errmess("\n    ");
	if (!at) do ax--; while (Space(Char(ax)));
	ap= display_line(ax)+4;
	for (p= 0; p < ap; p++) putc(' ', Errout);
	putc('^', Errout);
	putc('\n', Errout);
}

Hidden Procedure show_where(at, wia) bool at, wia; {
	context cc;
	if (cntxt == In_formal) {
		sv_context(&cc);
		set_context(&how_context);
		prline(No);
		set_context(&cc);
		errmess("originating from your command");
	}
	prline(at);
	if (!Interactive || !wia) {
		fprintf(Errout,
	"(detected after reading %d input line%s of your input file ",
		alino, alino == 1 ? "" : "s");
		if (iname == Vnil) errmess("standard input)");
		else {
			prname(iname);
			errmess(")");
		}
		putc('\n', Errout);
	}
}

Visible Procedure syserr(m) string m; {
	Line();
	errmess("*** Sorry, B system malfunction");
	show_where(Yes, Yes);
	fprintf(Errout, "*** The problem is: %s\n", m);
	errmess("*** Please save pertinent data for inspection by B guru\n");
	core_dump();
}

Visible Procedure memexh() {
	Line();
	errmess("*** Sorry, memory exhausted");
	show_where(Yes, Yes);
	errmess("*** Get your boss to buy a larger computer\n");
	core_dump();
}

Hidden Procedure fix_files() {
	if (ifile != stdin) fclose(ifile);
	if (f_interactive(stdin) || filtered) {
		interactive= Yes;
		release(iname);
		iname = Vnil;
		ifile = stdin;
		Eof= Eof0= No;
	}
}

Hidden Procedure proceed() {
	if (cntxt == In_prmnv) exit(-1);
	else if (cntxt == In_read && read_interactive) {
		errmess("*** please enter a valid expression instead\n");
		longjmp(reading[active_reads-1], 1);
	} else if (active_reads > 0 && read_interactive) {
		errmess("*** please enter a suitable expression instead\n");
		longjmp(reading[active_reads-1], 1);
	} else {
		if (cntxt == In_value) fix_files();
		longjmp(main_loop, 1);
	}
}

Hidden Procedure message(m1, m2, m3, at) string m1, m2, m3; bool at; {
	Skip(); Line();
	errmess(m1);
	show_where(at, Yes);
	fprintf(Errout, "*** The problem is: %s%s\n", m2, m3);
	proceed();
}

Visible Procedure error(m) string m; {
	message("*** Cannot cope with problem", m, "", No);
}

Visible Procedure parerr(m, ss) string m, ss; {
	message("*** There's something I don't understand", m, ss, Yes);
}

Visible Procedure pprerr(m, ss) string m, ss; {
	message("*** There's something I don't understand", m, ss, No);
}
 
Visible Procedure checkerr() {
	Line();
	errmess("*** Your check failed");
	show_where(No, Yes);
	longjmp(main_loop, 1);
}

Visible Procedure int_signal(in_read) bool in_read; {
	if (cntxt == In_prmnv) exit(-1);
	else if (interactive) {
		interrupt(in_read);
		if (!in_read) accept_int();
		longjmp(main_loop, 1);
	} else {
		fix_files();
		if (interactive) {
			interrupt(in_read);
			if (!in_read) accept_int();
			process();
		} else bye(1);
	}
}

Hidden Procedure interrupt(in_read) bool in_read; {
	if (!in_read) at_nwl= No;
	Line();
	errmess(in_read ? "*** READ aborted\n" : "*** interrupted\n");
	if (filtered) errmess("\177");
	if (cntxt == In_read || active_reads > 0) set_context(&read_context);
	/* show_where(No, was_interactive); */
}

bool bugs= No;
bool tracing= No;

Visible Procedure debug(m) string m; {
	if (bugs) {
		Line();
		fprintf(Errout, "*** Debugging (xeq = %s, cur_ilev = %d)",
			xeq ? "Yes" : "No", cur_ilev);
		show_where(Yes, Yes);
		fprintf(Errout, "*** %s\n", m);
	}
}

Visible Procedure trace() {
	VOID display_line(tx);
}
