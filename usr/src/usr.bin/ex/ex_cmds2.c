/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_cmds2.c	7.6 (Berkeley) %G%";
#endif not lint

#include "ex.h"
#include "ex_argv.h"
#include "ex_temp.h"
#include "ex_tty.h"
#include "ex_vis.h"

extern bool	pflag, nflag;		/* mjm: extern; also in ex_cmds.c */
extern int	poffset;		/* mjm: extern; also in ex_cmds.c */

/*
 * Subroutines for major command loop.
 */

/*
 * Is there a single letter indicating a named buffer next?
 */
cmdreg()
{
	register int c = 0;
	register int wh = skipwh();

	if (wh && isalpha(peekchar()))
		c = ex_getchar();
	return (c);
}

/*
 * Tell whether the character ends a command
 */
endcmd(ch)
	int ch;
{
	switch (ch) {
	
	case '\n':
	case EOF:
		endline = 1;
		return (1);
	
	case '|':
	case '"':
		endline = 0;
		return (1);
	}
	return (0);
}

/*
 * Insist on the end of the command.
 */
eol()
{

	if (!skipend())
		error("Extra chars|Extra characters at end of command");
	ignnEOF();
}

/*
 * Print out the message in the error message file at str,
 * with i an integer argument to printf.
 */
/*VARARGS2*/
error(str, i)
#ifndef EXSTRINGS
	char *str;
#else
# ifdef lint
	char *str;
# else
	int str;
# endif
#endif
	int i;
{

	error0();
	merror(str, i);
	if (writing) {
		serror(" [Warning - %s is incomplete]", file);
		writing = 0;
	}
	error1(str);
}

/*
 * Rewind the argument list.
 */
erewind()
{

	argc = argc0;
	argv = argv0;
	args = args0;
	if (argc > 1 && !hush) {
		ex_printf(mesg("%d files@to edit"), argc);
		if (inopen)
			ex_putchar(' ');
		else
			putNFL();
	}
}

/*
 * Guts of the pre-printing error processing.
 * If in visual and catching errors, then we dont mung up the internals,
 * just fixing up the echo area for the print.
 * Otherwise we reset a number of externals, and discard unused input.
 */
error0()
{

	if (vcatch) {
		if (splitw == 0)
			fixech();
		if (!SO || !SE)
			dingdong();
		return;
	}
	if (input) {
		input = strend(input) - 1;
		if (*input == '\n')
			setlastchar('\n');
		input = 0;
	}
	setoutt();
	flush();
	resetflav();
	if (!SO || !SE)
		dingdong();
	if (inopen) {
		/*
		 * We are coming out of open/visual ungracefully.
		 * Restore COLUMNS, undo, and fix tty mode.
		 */
		COLUMNS = OCOLUMNS;
		undvis();
		ostop(normf);
		/* ostop should be doing this
		putpad(VE);
		putpad(KE);
		*/
		putnl();
	}
	inopen = 0;
	holdcm = 0;
}

/*
 * Post error printing processing.
 * Close the i/o file if left open.
 * If catching in visual then throw to the visual catch,
 * else if a child after a fork, then exit.
 * Otherwise, in the normal command mode error case,
 * finish state reset, and throw to top.
 */
error1(str)
	char *str;
{
	bool die;

	if (io > 0) {
		close(io);
		io = -1;
	}
	die = (getpid() != ppid);	/* Only children die */
	inappend = inglobal = 0;
	globp = vglobp = vmacp = 0;
	if (vcatch && !die) {
		inopen = 1;
		vcatch = 0;
		if (str)
			noonl();
		fixol();
		longjmp(vreslab,1);
	}
	if (str && !vcatch)
		putNFL();
	if (die)
		ex_exit(1);
	lseek(0, 0L, 2);
	if (inglobal)
		setlastchar('\n');
	while (lastchar() != '\n' && lastchar() != EOF)
		ignchar();
	ungetchar(0);
	endline = 1;
	reset();
}

fixol()
{
	if (Outchar != vputchar) {
		flush();
		if (state == ONEOPEN || state == HARDOPEN)
			outline = destline = 0;
		Outchar = vputchar;
		vcontin(1);
	} else {
		if (destcol)
			vclreol();
		vclean();
	}
}

/*
 * Does an ! character follow in the command stream?
 */
exclam()
{

	if (peekchar() == '!') {
		ignchar();
		return (1);
	}
	return (0);
}

/*
 * Make an argument list for e.g. next.
 */
makargs()
{

	glob(&frob);
	argc0 = frob.argc0;
	argv0 = frob.argv;
	args0 = argv0[0];
	erewind();
}

/*
 * Advance to next file in argument list.
 */
next()
{
	extern short isalt;	/* defined in ex_io.c */

	if (argc == 0)
		error("No more files@to edit");
	morargc = argc;
	isalt = (strcmp(altfile, args)==0) + 1;
	if (savedfile[0])
		CP(altfile, savedfile);
	CP(savedfile, args);
	argc--;
	args = argv ? *++argv : strend(args) + 1;
}

/*
 * Eat trailing flags and offsets after a command,
 * saving for possible later post-command prints.
 */
newline()
{
	register int c;

	resetflav();
	for (;;) {
		c = ex_getchar();
		switch (c) {

		case '^':
		case '-':
			poffset--;
			break;

		case '+':
			poffset++;
			break;

		case 'l':
			listf++;
			break;

		case '#':
			nflag++;
			break;

		case 'p':
			listf = 0;
			break;

		case ' ':
		case '\t':
			continue;

		case '"':
			comment();
			setflav();
			return;

		default:
			if (!endcmd(c))
serror("Extra chars|Extra characters at end of \"%s\" command", Command);
			if (c == EOF)
				ungetchar(c);
			setflav();
			return;
		}
		pflag++;
	}
}

/*
 * Before quit or respec of arg list, check that there are
 * no more files in the arg list.
 */
nomore()
{

	if (argc == 0 || morargc == argc)
		return;
	morargc = argc;
	merror("%d more file", argc);
	serror("%s@to edit", plural((long) argc));
}

/*
 * Before edit of new file check that either an ! follows
 * or the file has not been changed.
 */
quickly()
{

	if (exclam())
		return (1);
	if (chng && dol > zero) {
/*
		chng = 0;
*/
		xchng = 0;
		serror("No write@since last change (:%s! overrides)", Command);
	}
	return (0);
}

/*
 * Reset the flavor of the output to print mode with no numbering.
 */
resetflav()
{

	if (inopen)
		return;
	listf = 0;
	nflag = 0;
	pflag = 0;
	poffset = 0;
	setflav();
}

/*
 * Print an error message with a %s type argument to printf.
 * Message text comes from error message file.
 */
serror(str, cp)
#ifdef lint
	register char *str;
#else
	register int str;
#endif
	char *cp;
{

	error0();
	smerror(str, cp);
	error1(str);
}

/*
 * Set the flavor of the output based on the flags given
 * and the number and list options to either number or not number lines
 * and either use normally decoded (ARPAnet standard) characters or list mode,
 * where end of lines are marked and tabs print as ^I.
 */
setflav()
{

	if (inopen)
		return;
	ignorf(setnumb(nflag || value(NUMBER)));
	ignorf(setlist(listf || value(LIST)));
	setoutt();
}

/*
 * Skip white space and tell whether command ends then.
 */
skipend()
{

	pastwh();
	return (endcmd(peekchar()) && peekchar() != '"');
}

/*
 * Set the command name for non-word commands.
 */
tailspec(c)
	int c;
{
	static char foocmd[2];

	foocmd[0] = c;
	Command = foocmd;
}

/*
 * Try to read off the rest of the command word.
 * If alphabetics follow, then this is not the command we seek.
 */
tail(comm)
	char *comm;
{

	tailprim(comm, 1, 0);
}

tail2of(comm)
	char *comm;
{

	tailprim(comm, 2, 0);
}

char	tcommand[20];

tailprim(comm, i, notinvis)
	register char *comm;
	int i;
	bool notinvis;
{
	register char *cp;
	register int c;

	Command = comm;
	for (cp = tcommand; i > 0; i--)
		*cp++ = *comm++;
	while (*comm && peekchar() == *comm)
		*cp++ = ex_getchar(), comm++;
	c = peekchar();
	if (notinvis || isalpha(c)) {
		/*
		 * Of the trailing lp funny business, only dl and dp
		 * survive the move from ed to ex.
		 */
		if (tcommand[0] == 'd' && any(c, "lp"))
			goto ret;
		if (tcommand[0] == 's' && any(c, "gcr"))
			goto ret;
		while (cp < &tcommand[19] && isalpha(peekchar()))
			*cp++ = ex_getchar();
		*cp = 0;
		if (notinvis)
			serror("What?|%s: No such command from open/visual", tcommand);
		else
			serror("What?|%s: Not an editor command", tcommand);
	}
ret:
	*cp = 0;
}

/*
 * Continue after a : command from open/visual.
 */
vcontin(ask)
	bool ask;
{

	if (vcnt > 0)
		vcnt = -vcnt;
	if (inopen) {
		if (state != VISUAL) {
			/*
			 * We don't know what a shell command may have left on
			 * the screen, so we move the cursor to the right place
			 * and then put out a newline.  But this makes an extra
			 * blank line most of the time so we only do it for :sh
			 * since the prompt gets left on the screen.
			 *
			 * BUG: :!echo longer than current line \\c
			 * will screw it up, but be reasonable!
			 */
			if (state == CRTOPEN) {
				termreset();
				vgoto(WECHO, 0);
			}
			if (!ask) {
				putch('\r');
				putch('\n');
			}
			return;
		}
		if (ask) {
			merror("[Hit return to continue] ");
			flush();
		}
#ifndef CBREAK
		vraw();
#endif
		if (ask) {
#ifdef EATQS
			/*
			 * Gobble ^Q/^S since the tty driver should be eating
			 * them (as far as the user can see)
			 */
			while (peekkey() == CTRL('Q') || peekkey() == CTRL('S'))
				ignore(getkey());
#endif
			if(getkey() == ':') {
				/* Ugh. Extra newlines, but no other way */
				putch('\n');
				outline = WECHO;
				ungetkey(':');
			}
		}
		vclrech(1);
		if (Peek_key != ':') {
			putpad(TI);
			tostart();
			/* replaced by ostart.
			putpad(VS);
			putpad(KS);
			*/
		}
	}
}

/*
 * Put out a newline (before a shell escape)
 * if in open/visual.
 */
vnfl()
{

	if (inopen) {
		if (state != VISUAL && state != CRTOPEN && destline <= WECHO)
			vclean();
		else
			vmoveitup(1, 0);
		vgoto(WECHO, 0);
		vclrbyte(vtube[WECHO], WCOLS);
		tostop();
		/* replaced by the ostop above
		putpad(VE);
		putpad(KE);
		*/
	}
	flush();
}
