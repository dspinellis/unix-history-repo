/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3err.c,v 1.4 85/08/22 16:57:50 timo Exp $
*/

/* B error message handling */

/* There are two kinds of errors:
	1) parsing, when the line in error is in a buffer
	2) execution, when the line in error is a parse-tree, and must
	   therefore be reconstructed.
*/

/* All error messages are collected in a file, both to save data space
   and to ease translation to other languages.	The English version
   of the database can be recreated from the program sources by scanning
   for the pattern "MESS(".  This is a macro whose first argument is
   the message number and whose second number is the message string;
   this macro expands to only the message number which is passed to
   the error routines.	The error routines then dig the message from
   the error message file, or just print the number if the file can't be
   opened.  There is also a way to pass a message that is determined
   at runtime.
*/

#include "b.h"
#include "b0fea.h"
#include "b0fil.h"
#include "b1obj.h"
#include "b2syn.h"
#include "b3env.h"
#include "b3fil.h"
#include "b3err.h"
#include "b3scr.h"
#include "b3sig.h"
#include "b3sou.h"

Visible bool still_ok, interrupted;

Visible parsetree curline= Vnil;
Visible value curlino;
Visible context how_context, act_context;

FILE *errfile;	/* The first thing a visible routine must do is set this */
		/* usually by calling line()				 */

#define Interactive (errfile == stderr)

/*********************************************************************/

/* While we are reading the Messages file, we build an index.
   probe[k] contains the first message number found in block k.
   blocks are BUFSIZ in size. */

#define FILESIZE 12916 /* Approximated current size of Messages file */
#define MAXPROBE (10 + FILESIZE/BUFSIZ) /* Allow some growth */

Hidden short probe[MAXPROBE];
Hidden int nprobes= 1;

Hidden FILE *messfp;
Hidden string savedmess;

Visible int MESSMAKE(mess) string mess; {
	savedmess= mess;
	return -1;
}

Visible string getmess(nr) int nr; {
	int last, c; char *cp= NULL;
	static char buf[80]; bool new; int block; long ftell();
	char *filename;
	if (nr == 0) return "";
	if (nr < 0) { return savedmess; }
	if (messfp == NULL)
		messfp= fopen(messfile, "r");
	if (messfp) {
		for (block= nprobes-1; block > 0; --block) {
			if (probe[block] <= nr)
				break;
		}
		new= block == nprobes-1;
		fseek(messfp, (long)block*BUFSIZ, 0);
		last= 0;
		while (last < nr) {
			if (new) block= ftell(messfp) / BUFSIZ;
			if (fgets(buf, sizeof buf, messfp) == NULL) break;
			last= atoi(buf);
			if (last <= 0)
				continue;
			if (new && block >= nprobes && nprobes < MAXPROBE) {
				probe[block]= last;
				nprobes= block+1;
			}
		}
		if (last == nr) {
			cp= index(buf, '\n');
			if (cp != NULL) *cp = '\0'; /* strip terminating \n */
			cp= buf;
			cp= index(buf, '\t');
			if (cp != NULL) return cp+1;
		}
	}
	sprintf(buf, " (error %d) ", nr);
	return buf;
}

Hidden Procedure prmess(nr) int nr; {
	errmess(getmess(nr));
}

/*********************************************************************/

Hidden Procedure putch(c) char c; {
	putc(c, errfile);
}

Hidden Procedure line() {
#ifdef EXT_COMMAND
	e_done();
#endif
	fflush(stdout);
	if (cntxt == In_read) {
		if (rd_interactive) {
			errfile= stderr; at_nwl= Yes;
		} else errfile= stdout;
	} else if (interactive) errfile= stderr;
	       else errfile= stdout;
	if (!at_nwl) putch('\n');
	at_nwl= Yes;
}

Hidden Procedure errmess(m) string m; {
	fputs(m, errfile);
}

#ifdef NOT_USED
Hidden Procedure core_dump() {
	errmess("*** Core-dump for inspection purposes: ");
	fflush(stdout);
	dump();
}
#endif

Hidden Procedure prname(name) value name; {
	if (Is_text(name)) {
		still_ok= Yes;
		writ(name);
		still_ok= No;
	}
}

Visible value erruname= Vnil;
Visible intlet errlino= 0;

Hidden intlet pr_line(at) bool at; {
	/*prints the line that tx is in, with an arrow pointing to the column
	  that tx is at.
	*/
	txptr lx= fcol(); intlet ap= -1, p= 0; char c; txptr ax= tx;
	if (!at) do ax--; while (Space(Char(ax)));
	while (!Eol(lx) && Char(lx) != Eotc) {
		if (lx == ax) ap= p;
		c= *lx++;
		if (c == '\t') {
			do { putch(' '); } while (((++p)%4)!=0);
		} else { putch(c); p++; }
	}
	putch('\n');
	if (ap < 0) ap= p;
	for (p= 0; p < ap+4; p++) putch(' ');
	errmess("^\n");
}

Hidden bool sh_lino(lino) intlet lino; {
	switch (cntxt) {
		case In_command:
		case In_read:
		case In_edval:
		case In_tarval:
		case In_prmnv:	return No;
		case In_unit:	return lino != 1;
		default:	return Yes;
	}
}


Hidden Procedure show_line(in_node, at, node, line_no)
 bool in_node, at; parsetree node; int line_no;
 {
	if (sh_lino(line_no))
		fprintf(errfile, " in line %d of your ", line_no);
	else
		errmess(" in your ");
	switch (cntxt) {
		case In_command:	errmess("command"); break;
		case In_read:		errmess("expression to be read"); break;
		case In_edval:		errmess("edited value"); break;
		case In_tarval: 	errmess("target value"); break;
		case In_unit:		errmess("unit ");
					release(erruname);
					if (Is_text(uname)) {
						value name; literal type;
						p_name_type(uname, &name, &type);
						prname(name); release(name);
						erruname= copy(uname);
						errlino= line_no;
					} else erruname= Vnil;
					break;
		case In_prmnv:		errmess("permanent environment"); break;
		default:		errmess("???\n"); return;
	}
	errmess("\n");
	if (!in_node || node != Vnil) errmess("    ");
	if (in_node) display(errfile, node, Yes);
	else pr_line(at);
}

Hidden bool unit_file() {
	value *aa;
	return cntxt == In_unit && Is_text(uname) && p_exists(uname, &aa);
}

Hidden Procedure show_where(in_node, at, node)
	bool in_node, at; parsetree node; {

	int line_no= in_node ? intval(curlino) : lino;
	if (cntxt == In_formal) { /*can only happen when in_node*/
		context cc;
		sv_context(&cc);
		set_context(&how_context);
		copy(uname);
		show_line(Yes, Yes, curline, intval(curlino));
		errmess("*** originating");
		set_context(&act_context);
		copy(uname);
		show_line(Yes, Yes, curline, intval(curlino));
		set_context(&cc);
	} else
		show_line(in_node, at, node, line_no);
	if (!Interactive && !unit_file()) {
		fprintf(errfile,
		  "*** (detected after reading %d input line%s of your input file ",
		    f_lino, f_lino == 1 ? "" : "s");
		if (iname == Vnil) errmess("standard input");
		else prname(iname);
		errmess(")\n");
	}
}

Hidden Procedure fatal(m, in_node) int m; bool in_node; {
	line();
	errmess("*** Sorry, B system malfunction");
	show_where(in_node, Yes, curline);
	errmess("*** The problem is: ");
	prmess(m); errmess("\n");
	errmess("*** Please save pertinent data for inspection by B guru\n");
	bye(-1);
}

Visible Procedure syserr(m) int m; {
	fatal(m, Yes);
}

#ifdef EXT_COMMAND
Visible Procedure psyserr(m) int m; {
	fatal(m, No);
}
#endif

Visible Procedure memexh() {
	line();
	errmess("*** Sorry, memory exhausted");
/* show_where(Yes, Yes); don't know if in node or not; to fix */ errmess("\n");
	errmess("*** Get your boss to buy a larger computer\n");
	bye(-1);
}

Hidden Procedure fix_files() {
	if (ifile != stdin) fclose(ifile);
	if (f_interactive(stdin) || filtered) {
		interactive= Yes;
		release(iname);
		iname = Vnil;
		ifile = stdin;
		sv_ifile= ifile;
		Eof= No;
	}
}

Hidden Procedure message(m1, m2, v, m3, in_node, at)
 string m1; int m2, m3; value v; bool in_node, at; {
	still_ok= No;
	line();
	errmess(m1);
	show_where(in_node, at, curline);
	errmess("*** The problem is: ");
	prmess(m2);
	if (v != Vnil) errmess(strval(v));
	prmess(m3);
	errmess("\n");
	at_nwl=Yes;
}

Visible Procedure pprerr(m) int m; {
	if (still_ok)
	message("*** There's something I don't understand", m, Vnil, 0, No, No);
}

Visible Procedure pprerr2(tag, m) value tag; int m; {
	if (still_ok)
	message("*** There's something I don't understand", 0, tag, m, No, No);
}

Visible Procedure parerr2(m, ss) int m, ss; {
	if (still_ok)
	message("*** There's something I don't understand", m, Vnil, ss, No, Yes);
}

Visible Procedure parerr(m) int m; {
	parerr2(m, 0);
}

Visible Procedure fixerr3(m1, v, m2) value v; int m1, m2; {
	if (still_ok)
	message("*** There's something I can't resolve", m1, v, m2, Yes, Yes);
}

Visible Procedure fixerr2(v, m) value v; int m; {
	fixerr3(0, v, m);
}

Visible Procedure fixerr(m) int m; {
	fixerr3(0, Vnil, m);
}

Visible Procedure error3(m1, v, m2) value v; int m1, m2; {
	message("*** Can't cope with problem", m1, v, m2, Yes, No);
}

Visible Procedure error2(m, v) int m; value v; {
	error3(m, v, 0);
}

Visible Procedure error(m) int m; {
	error3(m, Vnil, 0);
}

Visible Procedure checkerr() {
	still_ok= No;
	line();
	errmess("*** Your check failed");
	show_where(Yes, No, curline);
	at_nwl= Yes;
}

#ifdef SIGNAL

Visible Procedure int_signal() {
	interrupted= Yes; still_ok= No;
	if (cntxt == In_prmnv) exit(-1);
	if (!interactive) fix_files();
	if (!interactive) bye(1);
	line(); fflush(stdout);
	errmess("*** interrupted\n");
#ifndef INTEGRATION
	if (filtered) errmess("\177");
#endif
	if (cntxt == In_read) {
		set_context(&read_context);
		copy(uname);
	}
	at_nwl= Yes;
}

#endif SIGNAL

Visible bool bugs= No, testing= No, tracing= No;

#ifdef NOT_USED
Visible Procedure debug(m) string m; {
	if (bugs) {
		line();
		errmess("*** Debugging ");
		show_where(Yes, Yes, curline);
		fprintf(errfile, "*** %s\n", m);
		at_nwl= Yes;
	}
}
#endif

#ifdef EXT_COMMAND

/* User-callable error message */
Visible Procedure e_error(mesg) value mesg; {
	value v= convert(mesg, Yes, Yes);
	message("*** Halted", 0, v, 0, Yes, No);
	release(v);
}

#endif

Visible Procedure bye(ex) int ex; {
#ifdef EXT_COMMAND
	e_done();
#endif
	at_nwl= Yes;
	putprmnv();
	endall();
	if (ex == 0) {
		term_mem();
		endmem();
	}
#ifdef IBMPC
	memstat("at end");
#endif IBMPC
	exit(ex);
}

Visible Procedure initerr() {
	still_ok= Yes; interrupted= No; curline= Vnil; curlino= zero;
}


#define HZ 60 /* 4.2BSD: not line frequency but historical constant */

showtime(whence)
	string whence;
{
#ifdef TIMING
	static long total[2];
	long buf[4];
	extern bool timing; /* Set in b3mai.c by -T option */

	if (!timing) return;
	times(buf);
	line();
	fprintf(errfile, "*** Times %s: user %.2f sys %.2f (total %.2f %.2f)\n",
		whence,
		(float)(buf[0]-total[0])/HZ, (float)(buf[1]-total[1])/HZ,
		(float)total[0]/HZ, (float)total[1]/HZ
	);
	total[0]= buf[0]; total[1]= buf[1];
#endif TIMING
}
