/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
 $Header: b3mai.c,v 1.4 85/08/22 17:15:36 timo Exp $
 */


/* B driver for interpreter */

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b1mem.h"
#include "b2nod.h"
#include "b2syn.h"
#include "b2par.h"
#include "b3env.h"
#include "b3scr.h"
#include "b3err.h"
#include "b3fil.h"
#include "b3sig.h"
#include "b3sem.h"
#include "b3sou.h"

value evalthread();

Hidden bool call_error, in_process;

#ifdef INTEGRATION
bool dflag= No; /* -d: debugging output wanted */
bool slowterminal= No;
bool hushbaby= No;
#endif INTEGRATION

Visible bool timing; /* Set if timing output wanted */
Visible bool extcmds; /* Set if must recognize extended commands */

main(argc, argv) int argc; string argv[]; {
#ifdef START_MESSAGE
	fprintf(stderr, "Interactive B version %s\n%s\n", rcsid,
	    "Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985.");
#endif
	in_process= No; call_error= No;
	call(argc, argv);
	if (call_error) exit(-1);
	in_process= Yes;
	init();
	call(argc, argv);
	bye(0);
}

#define Cllerr stderr

Hidden string pname;	 /* program name */

Hidden Procedure erm(m, n, argc, pargc, pargv) string m, n; int argc, pargc; string pargv[]; {
	fprintf(Cllerr,
 "*** There is something I don't quite get in your call of %s\n", pname);
	show_call(argc, pargc, pargv);
	fprintf(Cllerr, "*** The problem is: %s %s\n", m, n);
	if (in_process) bye(-1);
	call_error= Yes;
}

Hidden Procedure call(pargc, pargv) int pargc; string pargv[]; {
	int argc; string *argv;

	pname = pargv[0];
	argc = pargc-1;
	argv = pargv+1;
	while (argc >= 0)
	if (argc > 0 && argv[0][0] == '-' && argv[0][1] != '\0') {
		if (argv[0][1] == 'q') { if (in_process) bye(0);
#ifndef INTEGRATION
		} else if (argv[0][1] == 'i') {
			filtered= Yes;
			cmd_prompt= "\001>";
			eg_prompt=  "\001E";
			raw_prompt= "\001R";
			qn_prompt=  "\001Y";
#endif
		} else if (argv[0][1] == 'T') {
			timing = Yes;
		} else if (argv[0][1] == 'E') {
			extcmds = Yes;
#ifdef INTEGRATION
#ifndef NDEBUG
		} else if (argv[0][1] == 'd') {
			dflag= Yes;
#endif NDEBUG
#endif INTEGRATION
		} else erm("I never learned about the option", argv[0], argc, pargc, pargv);
		argc -= 1;
		argv += 1;
	} else {
		if (argc == 0 || (argv[0][0] == '-' && argv[0][1] == '\0')) {
			release(iname);
			iname = Vnil;
			ifile = stdin;
		} else {
			release(iname);
			iname = mk_text(*argv);
			ifile = fopen(*argv, "r");
		}
		if (ifile != NULL) { if (in_process) process();
		} else erm("can't open input file", *argv, argc, pargc, pargv);
		if (ifile != NULL && ifile != stdin) fclose(ifile);
		++argv; --argc;
	}
}

Hidden Procedure show_call(eargc, pargc, pargv)
 int eargc, pargc; string pargv[]; {
	int argc= pargc; string *argv= pargv;
	intlet p, pos= 4;
	fprintf(Cllerr, "    ");
	while (argc > 0) {
		fprintf(Cllerr, *argv);
		pos+= strlen(*argv);
		if (argc == eargc) p= pos-1;
		++argv; --argc;
		if (argc > 0) {
			putc(' ', Cllerr);
			pos++;
		}
	}
	putc('\n', Cllerr);
	for (pos= 0; pos < p; pos++) putc(' ', Cllerr);
	fprintf(Cllerr, "^\n");
}

#ifdef STATMEM
#ifndef IBMPC
#undef STATMEM
#endif
#endif

#ifdef ebug
#ifdef IBMPC
#define PCLEAK
#ifndef STATMEM
#define STATMEM
#endif STATMEM
#endif IBMPC
#endif ebug

#ifdef IBMPC
Visible unsigned _stack= 6000; /* Default stack size */
#endif

#ifdef STATMEM
Hidden long alloccnt= 0;
#endif

/* Quick hack to print memory statistics */
Visible Procedure memstat(where) string where; {
#ifdef STATMEM
	long sizmem();
	fprintf(stderr, "*** %s: sizmem=%ld, sizmalloc=%ld.\n",
			where, sizmem(), alloccnt);
#endif
}

Visible char*
qmalloc(syze)
	unsigned syze;
{
#ifdef STATMEM
	char *p;
	long before, sizmem();
	before= sizmem();
	p= malloc(syze);
	alloccnt += (before - sizmem());
	return p;
#else
	return malloc(syze);
#endif
}

Hidden Procedure init() {
#ifdef STATMEM
	allmem();
	memstat("before init");
#endif
	set_file_names();
#ifdef INTEGRATION
	initgram(); /* set refcnt to infinity */
	initsugg(); /* set refcnt to infinity */
	memstat("after gram/sugg");
#endif
#ifdef PCLEAK
	initsou();
	initfpr();
#endif
	initmem();
	initenv();
	initnum();
	initsyn();
#ifndef PCLEAK
	initsou();
	initfpr();
#endif
	init_scr();
	initerr();
	initsig();
	initint();
#ifdef TYPE_CHECK
	initpol();
	inittyp();
#endif
#ifdef INTEGRATION
	initfile();
	initkeys();
#ifdef unix
	initunix();
#endif
	initterm();
	initbtop();
#endif
	end_init();
	setprmnv();
	getprmnv();
	memstat("after init");
	showtime("after initialization");
}

Visible Procedure
endall()
{
	endsou();
	endsyn();
	endnum();
	endenv();
	endsta();
#ifdef INTEGRATION
	endscr();
	endterm();
	/* enddemo(); ? */
	endbtop();
#ifdef unix
	endunix();
#endif
	enderro();
	endsugg();
#endif INTEGRATION
}

/* ******************************************************************** */
/*		immediate command					*/
/* ******************************************************************** */

Hidden bool sa_expr(e) parsetree *e; {
	return is_expr(Char(tx)) ? (*e= expr(ceol), Yes) : No;
}

Hidden Procedure special() {
	switch(Char(tx++)) {
		case ':':       skipsp(&tx);
				if (Char(tx) == ':') lst_uhds();
				else edit_unit();
				break;
		case '=':       skipsp(&tx);
				if (Char(tx) == '=') lst_ttgs();
				else edit_target();
				break;
		case '!':       system(tx); break;
			/* Obey the rest of the line as an OS command */
		default:	syserr(MESS(3700, "special"));
	}
}

Visible Procedure imm_command() {
	parsetree codeseq= NilTree;
	parsetree c= NilTree, d= NilTree, e= NilTree; value v; int level;
	cntxt= In_command; still_ok= Yes; interrupted= No;
	terminated= No;
	resexp= Voi; lino= 0;
	level= ilev();
	if (!still_ok) return;
	if (level > 0) parerr(MESS(3701, "outer indentation not zero"));
	else if (findceol(), Ceol(tx));
	else if (Char(tx) == ':' || Char(tx) == '=' || Char(tx) == '!')
		if (interactive) special();
		else parerr(MESS(3702, "special commands only interactively"));
	else if (sa_expr(&e)) {
		if (still_ok) fix_nodes(&e, &codeseq);
		showtime("after fix_nodes");
		curline= e; curlino= one;
		v= evalthread(codeseq);
		if (still_ok) { wri(v, Yes, No, No); newline(); }
		release(v); release(e);
		showtime("after evaluation");
	} else if (unit_keyword()) {
		create_unit();
	} else if (quit_keyword()) terminated= Yes;
	else if (term_com(&c)) {
		release(c);
		parerr(MESS(3703, "terminating commands only allowed in units and refinements"));
	} else if (control_command(&c) || simple_command(&c, &d)) {
			/* control_command MUST come before simple above */
		if (still_ok) fix_nodes(&c, &codeseq);
		showtime("after fix_nodes");
		curline= c; curlino= one;
		execthread(codeseq);
		release(c); release(d);
		showtime("after execution");
	} else parerr(MESS(3704, "I don't recognise this as a command"));
}

Hidden Procedure process() {
	re_screen();
	re_env();
	f_lino= 0;
	while (!Eof && !terminated) {
#ifdef EXT_COMMAND
		e_done();
#endif
		imm_command();
		if (!interactive && !still_ok) bye(1);
	}
}
