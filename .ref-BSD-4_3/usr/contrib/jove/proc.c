/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"

#include <signal.h>

private char
	*errfmt = "^\\{\",\\}\\([^:\"( \t]*\\)\\{\"\\, line ,:,(\\} *\\([0-9][0-9]*\\)[:)]";

struct error {
	Buffer		*er_buf;	/* Buffer error is in */
	Line		*er_mess,	/* Actual error message */
			*er_text;	/* Actual error */
	int		er_char;	/* char pos of error */
	struct error	*er_prev,	/* List of errors */
			*er_next;
};

struct error	*cur_error = 0,
		*errorlist = 0;
Buffer		*perr_buf = 0;	/* Buffer with error messages */

int	WtOnMk = 1;		/* Write the modified files when we make */

/* Add an error to the end of the list of errors.  This is used for
   parse-{C,LINT}-errors and for the spell-buffer command */

private struct error *
AddError(laste, errline, buf, line, charpos)
struct error	*laste;
Line	*errline,
	*line;
Buffer	*buf;
{
	struct error	*new = (struct error *) emalloc(sizeof *new);

	new->er_prev = laste;
	if (laste)
		laste->er_next = new;
	else {
		if (errorlist)		/* Free up old errors */
			ErrFree();
		cur_error = errorlist = new;
	}
	laste = new;
	new->er_next = 0;
	new->er_buf = buf;
	new->er_text = line;
	new->er_char = charpos;
	new->er_mess = errline;

	return new;
}

ParseAll()
{
	ErrParse(errfmt);
}

XParse()
{
	char	*sstr;

	sstr = ask(errfmt, ProcFmt);
	ErrParse(sstr);
}

/* Parse for {C,LINT} errors (or anything that matches fmtstr) in the
   current buffer.  Set up for the next-error command.  This is neat
   because this will work for any kind of output that prints a file
   name and a line number on the same line. */

ErrParse(fmtstr)
char	*fmtstr;
{
	Bufpos	*bp;
	char	fname[FILESIZE],
		lineno[10],
		REbuf[128],
		*REalts[10];
	int	lnum,
		last_lnum = -1;
	struct error	*ep = 0;
	Buffer	*buf,
		*lastb = 0;
	Line	*err_line;	

	ErrFree();		/* This is important! */
	ToFirst();
	perr_buf = curbuf;
	REcompile(fmtstr, 1, REbuf, REalts);
	/* Find a line with a number on it. */
	while (bp = docompiled(FORWARD, REbuf, REalts)) {
		SetDot(bp);
		putmatch(1, fname, sizeof fname);
		putmatch(2, lineno, sizeof lineno);
		buf = do_find((Window *) 0, fname, 1);
		if (buf != lastb) {
			lastb = buf;
			last_lnum = -1;		/* signals new file */
			err_line = buf->b_first;
		}
		lnum = chr_to_int(lineno, 10, 0);
		if (lnum == last_lnum)	/* one error per line is nicer */
			continue;
		if (last_lnum == -1)
			last_lnum = 1;	/* that's where we really are */
		err_line = next_line(err_line, lnum - last_lnum);
		ep = AddError(ep, curline, buf, err_line, 0);
		last_lnum = lnum;
	}
	if (cur_error != 0)
		ShowErr();
	exp = 1;
}

/* Free up all the errors */

ErrFree()
{
	register struct error	*ep;

	for (ep = errorlist; ep != 0; ep = ep->er_next)
		free((char *) ep);
	errorlist = cur_error = 0;
}

/* Internal next error sets cur_error to the next error, taking the
   argument count, supplied by the user, into consideration. */

private char	errbounds[] = "You're at the %s error.",
		noerrs[] = "No errors!";

private
toerror(forward)
{
	register int	i;
	register struct error	*e = cur_error;

	if (e == 0)
		complain(noerrs);
	if ((forward && (e->er_next == 0)) ||
	    (!forward && (e->er_prev == 0)))
		complain(errbounds, forward ? "last" : "first");

	for (i = 0; i < exp; i++) {
		if ((e = forward ? e->er_next : e->er_prev) == 0)
			break;
		cur_error = e;
	}
}

NextError()
{
	ToError(1);
}

PrevError()
{
	ToError(0);
}

private
okay_error()
{
	return ((inlist(perr_buf->b_first, cur_error->er_mess)) &&
		(inlist(cur_error->er_buf->b_first, cur_error->er_text)));
}

/* Go the the next error, if there is one.  Put the error buffer in
   one window and the buffer with the error in another window.
   It checks to make sure that the error actually exists. */

ToError(forward)
{
	do {
		toerror(forward);
		exp = 1;
	} while (!okay_error());
	ShowErr();
}

int	EWSize = 20;	/* percentage of screen the error window
			   should be */

/* Show the current error, i.e. put the line containing the error message
   in one window, and the buffer containing the actual error in another
   window. */

ShowErr()
{
	Window	*err_wind,
		*buf_wind;
	int	w_size;		/* size of window */

	if (cur_error == 0)
		complain(noerrs);
	if (!okay_error()) {
		rbell();
		return;
	}
	err_wind = windbp(perr_buf);
	buf_wind = windbp(cur_error->er_buf);

	if (err_wind && !buf_wind) {
		SetWind(err_wind);
		pop_wind(cur_error->er_buf->b_name, 0, -1);
		buf_wind = curwind;
	} else if (!err_wind && buf_wind) {
		SetWind(buf_wind);
		pop_wind(perr_buf->b_name, 0, -1);
		err_wind = curwind;
	} else if (!err_wind && !buf_wind) {
		pop_wind(perr_buf->b_name, 0, -1);
		err_wind = curwind;
		pop_wind(cur_error->er_buf->b_name, 0, -1);
		buf_wind = curwind;
	}

	/* Put the current error message at the top of its Window */
	SetWind(err_wind);
	SetLine(cur_error->er_mess);
	SetTop(curwind, (curwind->w_line = cur_error->er_mess));
	w_size = (ILI * EWSize) / 100;
	if (w_size >= 1)
		WindSize(curwind, w_size - (curwind->w_height - 1));

	/* now go to the the line with the error in the other window */
	SetWind(buf_wind);
	DotTo(cur_error->er_text, cur_error->er_char);
}

char	ShcomBuf[128] = {0};

/* Make a buffer name given the command `command', i.e. "fgrep -n foo *.c"
   will return the buffer name "fgrep".  */

char *
MakeName(command)
char	*command;
{
	static char	bufname[50];
	register char	*cp = bufname,
			c;

	while ((c = *command++) && (c == ' ' || c == '\t'))
		;
	do
		*cp++ = c;
	while ((c = *command++) && (c != ' ' && c != '\t'));
	*cp = 0;
	strcpy(bufname, basename(bufname));

	return bufname;
}

/* Run make, first writing all the modified buffers (if the WtOnMk flag is
   non-zero), parse the errors, and go the first error. */

char	make_cmd[128] = "make";

MakeErrors()
{
	Window	*old = curwind;
	int	status,
		compilation;
	
	if (WtOnMk)
		put_bufs(0);
	/* When we're not doing make or cc (i.e., the last command
	   was probably a grep or something) and the user just types
	   C-X C-E, he probably (possibly, hopefully, usually (in my
	   case)) doesn't want to do the grep again but rather wants
	   to do a make again; so we ring the bell and insert the
	   default command and let the person decide. */

	compilation = (sindex("make", make_cmd) || sindex("cc", make_cmd));
	if (exp_p || !compilation) {
		if (!compilation) {
			rbell();
			Inputp = make_cmd;	/* insert the default for the
						   user */
		}
		null_ncpy(make_cmd, ask(make_cmd, "Compilation command: "),
				sizeof (make_cmd) - 1);
	}
	status = UnixToBuf(MakeName(make_cmd), YES, EWSize, YES, Shell, basename(Shell), ShFlags, make_cmd, 0);
	com_finish(status, make_cmd);

	ErrParse(errfmt);

	if (!cur_error)
		SetWind(old);
}

#ifdef SPELL

SpelBuffer()
{
	char	*Spell = "Spell",
		com[100];
	Window	*savewp = curwind;

	put_bufs(0);
	sprintf(com, "spell %s", curbuf->b_fname);
	(void) UnixToBuf(Spell, YES, EWSize, YES, Shell, basename(Shell), ShFlags, com, 0);
	message("[Delete the irrelevant words and then type C-X C-C]");
	Recur();
	SetWind(savewp);
	SpelParse(Spell);
}

SpelWords()
{
	char	*buftospel;
	Buffer	*wordsb = curbuf;

	if ((buftospel = ask_buf((Buffer *) 0)) == 0)
		return;
	SetBuf(do_select(curwind, buftospel));
	SpelParse(wordsb->b_name);
}

SpelParse(bname)
char	*bname;
{
	Buffer	*buftospel,
		*wordsb;
	char	wordspel[100];
	Bufpos	*bp;
	struct error	*ep = 0;

	ErrFree();		/* This is important! */

	buftospel = curbuf;
	wordsb = buf_exists(bname);
	perr_buf = wordsb;	/* This is important (buffer containing
				   error messages) */
	SetBuf(wordsb);
	ToFirst();
	f_mess("Finding misspelled words ... ");
	while (!lastp(curline)) {
		sprintf(wordspel, "\\<%s\\>", linebuf);
		SetBuf(buftospel);
		ToFirst();
		while (bp = dosearch(wordspel, 1, 1)) {
			SetDot(bp);
			ep = AddError(ep, wordsb->b_dot, buftospel,
					  curline, curchar);
		}
		SetBuf(wordsb);
		line_move(FORWARD, NO);
	}
	add_mess("Done.");
	SetBuf(buftospel);
	ShowErr();
}

#endif SPELL

ShToBuf()
{
	char	bufname[100];

	strcpy(bufname, ask((char *) 0, "Buffer: "));
	DoShell(bufname, ask(ShcomBuf, "Command: "));
}

ShellCom()
{
	null_ncpy(ShcomBuf, ask(ShcomBuf, ProcFmt), (sizeof ShcomBuf) - 1);
	DoShell(MakeName(ShcomBuf), ShcomBuf);
}

/* Run the shell command into `bufname'.  Empty the buffer except when we
   give a numeric argument, in which case it inserts the output at the
   current position in the buffer.  */

private
DoShell(bufname, command)
char	*bufname,
	*command;
{
	Window	*savewp = curwind;
	int	status;

	exp = 1;
	status = UnixToBuf(bufname, YES, 0, !exp_p, Shell, basename(Shell),
			   ShFlags, command, 0);
	com_finish(status, command);
	SetWind(savewp);
}

private
com_finish(status, com)
char	*com;
{
	s_mess("\"%s\" completed %ssuccessfully.", com, status ? "un" : NullStr);
}

dowait(pid, status)
int	pid,
	*status;
{
#ifndef IPROCS

	int	rpid;

	while ((rpid = wait(status)) != pid)
		;
#else

#ifdef BSD4_2
#   include <sys/wait.h>
#else
#   include <wait.h>
#endif

	union wait	w;
	int	rpid;

	for (;;) {
#ifndef VMUNIX
		rpid = wait2(&w.w_status, 0);
#else
		rpid = wait3(&w, 0, (struct rusage *) 0);
#endif
		if (rpid == pid) {
			if (status)
				*status = w.w_status;
			break;
		} else
			kill_off(rpid, w);
	}
#endif IPROCS
}

/* Run the command to bufname, erase the buffer if clobber is non-zero,
   and redisplay if disp is non-zero.  Leaves current buffer in `bufname'
   and leaves any windows it creates lying around.  It's up to the caller
   to fix everything up after we're done.  (Usually there's nothing to
   fix up.) */

/* VARARGS3 */

UnixToBuf(bufname, disp, wsize, clobber, cmd, args)
char	*bufname,
	*cmd;
{
	int	p[2],
		pid;
	extern int	ninbuf;
	Buffer	*bp;

	if (clobber && (bp = buf_exists(bufname)) != 0 &&
			bp->b_type != B_PROCESS && bp->b_type != B_IPROCESS)
		complain("Command would over-write buffer %s.", bufname);
	if (disp) {
		message("Starting up...");
		pop_wind(bufname, clobber, clobber ? B_PROCESS : B_FILE);
		wsize = (LI * wsize) / 100;
		if (wsize >= 1 && !one_windp())
			WindSize(curwind, wsize - (curwind->w_height - 1));
		redisplay();
	}
	exp = 1;
	dopipe(p);
	pid = fork();
	if (pid == -1) {
		pclose(p);
		complain("[Fork failed]");
	}
	if (pid == 0) {
		(void) close(0);
		(void) open("/dev/null", 0);
		(void) close(1);
		(void) close(2);
		(void) dup(p[1]);
		(void) dup(p[1]);
		pclose(p);
		execv(cmd, (char **) &args);
		(void) write(1, "Execl failed.\n", 14);
		_exit(1);
	} else {
		int	status;
		int	(*oldint)() = signal(SIGINT, SIG_IGN);
		char	*mess;
		File	*fp;

#ifdef IPROCS
		sighold(SIGCHLD);
#endif

		(void) close(p[1]);
		fp = fd_open(cmd, F_READ, p[0], iobuff, LBSIZE);
		while (inIOread = 1, f_gets(fp, genbuf, LBSIZE) != EOF) {
			inIOread = 0;
			ins_str(genbuf, YES);
			LineInsert();
			if (disp != 0 && fp->f_cnt <= 0) {
#ifdef LOAD_AV
			    {
			    	double	theavg;

				get_la(&theavg);
				if (theavg < 2.0)
					mess = "Screaming along...";
				else if (theavg < 5.0)
					mess = "Chugging along...";
				else
					mess = "Crawling along...";
			    }
#else
				mess = "Chugging along...";
#endif LOAD_AV
				message(mess);
				redisplay();
			}
		}
		if (disp)
			DrawMesg(NO);
		close_file(fp);
		(void) signal(SIGINT, oldint);
		dowait(pid, &status);
#ifdef IPROCS
		sigrelse(SIGCHLD);
#endif
		return status;
	}
	return 0;
}

#ifdef BSD4_2

private int	SigMask = 0;

sighold(sig)
{
	(void) sigblock(SigMask |= (1 << (sig - 1)));
}

sigrelse(sig)
{
	(void) sigsetmask(SigMask &= ~(1 << (sig - 1)));
}

#endif

FilterRegion()
{
	char	*cmd = ask((char *) 0, ": %f (through command) ", ProcFmt);

	RegToUnix(curbuf, cmd);
}

/* Send the current region to CMD and insert the output from the
   command into OUT_BUF. */

RegToUnix(outbuf, cmd)
Buffer	*outbuf;
char	*cmd;
{
	Mark	*m = CurMark();
	char	*tname = mktemp("/tmp/jfilterXXXXXX"),
		combuf[130];
	Window	*save_wind = curwind;
	int	status;
	File	*fp;

    CATCH
	fp = open_file(tname, iobuff, F_WRITE, COMPLAIN, QUIET);
	putreg(fp, m->m_line, m->m_char, curline, curchar, YES);
	DelReg();
	sprintf(combuf, "%s < %s", cmd, tname);
	status = UnixToBuf(outbuf->b_name, NO, 0, outbuf->b_type == B_SCRATCH,
			   Shell, basename(Shell), ShFlags, combuf, 0);
    ONERROR
	;	/* Do nothing ... but fall through and delete the tmp
		   file. */
    ENDCATCH
	f_close(fp);
	(void) unlink(tname);
	SetWind(save_wind);
	com_finish(status, combuf);
}
