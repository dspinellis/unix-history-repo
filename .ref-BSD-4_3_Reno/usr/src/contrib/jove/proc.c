/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"
#include "fp.h"
#include "re.h"
#include "termcap.h"
#include "disp.h"

#include <signal.h>
#ifdef	STDARGS
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#ifdef MSDOS
# include <io.h>
# include <process.h>
#endif

private void
	DoShell proto((char *, char *)),
	com_finish proto((int, char *)),
	toerror proto((int, int));

#ifdef	MSDOS
private void
	closepipe proto((void));
#endif

private int
	okay_error proto((void));

#if defined(MSDOS)
private int
	openforpipe proto((void)),
	reopenforpipe proto((void));
#endif

private struct error
	*AddError proto((struct error *, Line *, Buffer *, Line *, int));

long	SigMask = 0;

/* This disgusting RE search string parses output from the GREP
   family, from the pdp11 compiler, pcc, and lint.  Jay (HACK)
   Fenlasen changed this to work for the lint errors. */
char	ErrFmtStr[256] = "^\\{\",\\}\\([^:\"( \t]*\\)\\{\"\\, line ,:,(\\} *\\([0-9][0-9]*\\)[:)]\
\\|::  *\\([^(]*\\)(\\([0-9]*\\))$\
\\|( \\([^(]*\\)(\\([0-9]*\\)) ),";

struct error {
	Buffer		*er_buf;	/* Buffer error is in */
	Line		*er_mess,	/* Actual error message */
			*er_text;	/* Actual error */
	int		er_char;	/* char pos of error */
	struct error	*er_prev,	/* List of errors */
			*er_next;
};

private struct error	*cur_error = NULL,
		*errorlist = NULL;
Buffer		*perr_buf = NULL;	/* Buffer with error messages */

int	WtOnMk = 1;		/* Write the modified files when we make */

/* Add an error to the end of the list of errors.  This is used for
   parse-{C,LINT}-errors and for the spell-buffer command */

private struct error *
AddError(laste, errline, buf, line, charpos)
struct error	*laste;
Line	*errline,
	*line;
Buffer	*buf;
int	charpos;
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

void
get_FL_info(fname, lineno)
char	*fname,
	*lineno;
{
	putmatch(1, fname, (size_t)FILESIZE);
	putmatch(2, lineno, (size_t)FILESIZE);

	/* error had lineno followed fname, so switch the two */
	if (!isdigit(lineno[0])) {
		char	tmp[FILESIZE];

		strcpy(tmp, lineno);
		strcpy(lineno, fname);
		strcpy(fname, tmp);
	}
}

/* Free up all the errors */

void
ErrFree()
{
	register struct error	*ep;

	for (ep = errorlist; ep != 0; ep = ep->er_next)
		free((char *) ep);
	errorlist = cur_error = 0;
}

/* Parse errors of the form specified in ErrFmtStr in the current
   buffer.  Do a show error of the first error.  This is neat because this
   will work for any kind of output that prints a file name and a line
   number on the same line. */

void
ErrParse()
{
	struct RE_block	re_blk;
	Bufpos	*bp;
	char	fname[FILESIZE],
		lineno[FILESIZE];
	int	lnum,
		last_lnum = -1;
	struct error	*ep = 0;
	Buffer	*buf,
		*lastb = 0;
	Line	*err_line;

	ErrFree();		/* This is important! */
	ToFirst();
	perr_buf = curbuf;
	REcompile(ErrFmtStr, YES, &re_blk);
	/* Find a line with a number on it. */
	while ((bp = docompiled(FORWARD, &re_blk)) != NULL) {
		SetDot(bp);
		get_FL_info(fname, lineno);
		buf = do_find((Window *) 0, fname, YES);
		if (buf != lastb) {
			lastb = buf;
			last_lnum = -1;		/* signals new file */
			err_line = buf->b_first;
		}
		(void) chr_to_int(lineno, 10, NO, &lnum);
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
}

/* Internal next error sets cur_error to the next error, taking the
   argument count, supplied by the user, into consideration. */

private char	errbounds[] = "You're at the %s error.",
		noerrs[] = "No errors!";

private void
toerror(forward, num)
int	forward,
	num;
{
	register struct error	*e = cur_error;

	if (e == 0)
		complain(noerrs);
	if ((forward && (e->er_next == 0)) ||
	    (!forward && (e->er_prev == 0)))
		complain(errbounds, forward ? "last" : "first");

	while (--num >= 0) {
		if ((e = forward ? e->er_next : e->er_prev) == 0)
			break;
		cur_error = e;
	}
}

void
NextError()
{
	ToError(1);
}

void
PrevError()
{
	ToError(0);
}

private int
okay_error()
{
	return ((inlist(perr_buf->b_first, cur_error->er_mess)) &&
		(inlist(cur_error->er_buf->b_first, cur_error->er_text)));
}

/* Go the the next error, if there is one.  Put the error buffer in
   one window and the buffer with the error in another window.
   It checks to make sure that the error actually exists. */

void
ToError(forward)
int	forward;
{
	do {
		toerror(forward, arg_value());
	} while (!okay_error());
	ShowErr();
}

int	EWSize = 20;	/* percentage of screen the error window
			   should be */

private void
set_wsize(wsize)
int	wsize;
{
	wsize = (LI * wsize) / 100;
	if (wsize >= 1 && !one_windp())
		WindSize(curwind, wsize - (curwind->w_height - 1));
}

/* Show the current error, i.e. put the line containing the error message
   in one window, and the buffer containing the actual error in another
   window. */

void
ShowErr()
{
	Window	*err_wind,
		*buf_wind;

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
		pop_wind(cur_error->er_buf->b_name, NO, -1);
		buf_wind = curwind;
	} else if (!err_wind && buf_wind) {
		SetWind(buf_wind);
		pop_wind(perr_buf->b_name, NO, -1);
		err_wind = curwind;
	} else if (!err_wind && !buf_wind) {
		pop_wind(perr_buf->b_name, NO, -1);
		err_wind = curwind;
		pop_wind(cur_error->er_buf->b_name, NO, -1);
		buf_wind = curwind;
	}

	/* Put the current error message at the top of its Window */
	SetWind(err_wind);
	SetLine(cur_error->er_mess);
	SetTop(curwind, (curwind->w_line = cur_error->er_mess));
	set_wsize(EWSize);

	/* now go to the the line with the error in the other window */
	SetWind(buf_wind);
	DotTo(cur_error->er_text, cur_error->er_char);
}

char	ShcomBuf[LBSIZE];

/* Make a buffer name given the command `command', i.e. "fgrep -n foo *.c"
   will return the buffer name "fgrep".  */

char *
MakeName(command)
char	*command;
{
	static char	bnm[50];
	register char	*cp = bnm,
			c;

	while ((c = *command++) != '\0' && (c == ' ' || c == '\t'))
		;
	do
		*cp++ = c;
	while ((c = *command++) != '\0' && (c != ' ' && c != '\t'));
	*cp = 0;
	strcpy(bnm, basename(bnm));

	return bnm;
}

/* Run make, first writing all the modified buffers (if the WtOnMk flag is
   non-zero), parse the errors, and go the first error. */

private char	make_cmd[LBSIZE] = "make";

void
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
	if (is_an_arg() || !compilation) {
		if (!compilation) {
			rbell();
			Inputp = make_cmd;	/* insert the default for the user */
		}
		null_ncpy(make_cmd, ask(make_cmd, "Compilation command: "),
				sizeof (make_cmd) - 1);
	}
	status = UnixToBuf(MakeName(make_cmd), YES, EWSize, YES, Shell, ShFlags, make_cmd, (char *) 0);
	com_finish(status, make_cmd);

	ErrParse();

	if (!cur_error)
		SetWind(old);
}

#ifdef SPELL

private void
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
		swritef(wordspel, "\\<%s\\>", linebuf);
		SetBuf(buftospel);
		ToFirst();
		while ((bp = dosearch(wordspel, 1, 1)) != NULL) {
			SetDot(bp);
			ep = AddError(ep, wordsb->b_dot, buftospel,
					  curline, curchar);
		}
		SetBuf(wordsb);
		line_move(FORWARD, 1, NO);
	}
	add_mess("Done.");
	SetBuf(buftospel);
	ShowErr();
}

void
SpelBuffer()
{
	char	*Spell = "Spell",
		com[100];
	Window	*savewp = curwind;

	put_bufs(0);
	swritef(com, "spell %s", curbuf->b_fname);
	(void) UnixToBuf(Spell, YES, EWSize, YES, Shell, ShFlags, com, (char *) 0);
	message("[Delete the irrelevant words and then type C-X C-C]");
	ToFirst();
	Recur();
	SetWind(savewp);
	SpelParse(Spell);
}

void
SpelWords()
{
	char	*buftospel;
	Buffer	*wordsb = curbuf;

	if ((buftospel = ask_buf((Buffer *) 0)) == 0)
		return;
	SetBuf(do_select(curwind, buftospel));
	SpelParse(wordsb->b_name);
}

#endif /* SPELL */

void
ShToBuf()
{
	char	bnm[128],
		cmd[128];

	strcpy(bnm, ask((char *) 0, "Buffer: "));
	strcpy(cmd, ask(ShcomBuf, "Command: "));
	DoShell(bnm, cmd);
}

void
ShellCom()
{
	null_ncpy(ShcomBuf, ask(ShcomBuf, ProcFmt), (sizeof ShcomBuf) - 1);
	DoShell(MakeName(ShcomBuf), ShcomBuf);
}

void
ShNoBuf()
{
	int	status;

	null_ncpy(ShcomBuf, ask(ShcomBuf, ProcFmt), (sizeof ShcomBuf) - 1);
	status = UnixToBuf((char *) 0, NO, 0, NO, Shell, ShFlags, ShcomBuf,
		curbuf->b_fname, curbuf->b_fname, (char *) 0);
	com_finish(status, ShcomBuf);
}

void
Shtypeout()
{
	int	status;

	null_ncpy(ShcomBuf, ask(ShcomBuf, ProcFmt), (sizeof ShcomBuf) - 1);
	status = UnixToBuf((char *) 0, YES, 0, NO, Shell, ShFlags, ShcomBuf,
		curbuf->b_fname, curbuf->b_fname, (char *) 0);
	if (status == 0)
		Typeout("[%s: completed successfully]", ShcomBuf);
	else
		Typeout("[%s: exited (%d)]", ShcomBuf, status);
	TOstop();
}

/* Run the shell command into `bnm'.  Empty the buffer except when we
   give a numeric argument, in which case it inserts the output at the
   current position in the buffer.  */

private void
DoShell(bnm, command)
char	*bnm,
	*command;
{
	Window	*savewp = curwind;
	int	status;

	status = UnixToBuf(bnm, YES, 0, !is_an_arg(), Shell, ShFlags,
		command, curbuf->b_fname, curbuf->b_fname, (char *) 0);
	com_finish(status, command);
	SetWind(savewp);
}

private void
com_finish(status, cmd)
int	status;
char	*cmd;
{
	s_mess("[%s: ", cmd);
	if (status == 0)
		add_mess("completed successfully");
	else
		add_mess("exited (%d)", status);
	add_mess("]");
}

#ifndef MSDOS
void
dowait(pid, status)
int	pid,
	*status;
{
# ifndef IPROCS

	int	rpid;

	while ((rpid = wait(status)) != pid)
		;
# else

# include "wait.h"

	union wait	w;
	int	rpid;

	for (;;) {
#  ifndef WAIT3
		rpid = wait2(&w.w_status, 0);
#  else
		rpid = wait3(&w, 0, (struct rusage *) 0);
#  endif
		if (rpid == -1)
			break;
		else if (rpid == pid) {
			if (status)
				*status = w.w_status;
			break;
		} else
			kill_off(rpid, w);
	}
# endif /* IPROCS */
}
#endif /* MSDOS */

/* Run the command to bnm, erase the buffer if clobber is non-zero,
   and redisplay if disp is non-zero.  Leaves current buffer in `bnm'
   and leaves any windows it creates lying around.  It's up to the caller
   to fix everything up after we're done.  (Usually there's nothing to
   fix up.) */

#ifdef	STDARGS
	int
UnixToBuf(char *bnm, int disp, int wsize, int clobber, ...)
#else
	/*VARARGS4*/ int
UnixToBuf(bnm, disp, wsize, clobber, va_alist)
	char	*bnm;
	int	disp;
	int	wsize;
	int	clobber;
	va_dcl
#endif
{
#ifndef MSDOS
	int	p[2],
		pid,
		status,
#else /* MSDOS */
	int	p0,
		oldo,
		olde,
		retcode,
#endif /* MSDOS */
		eof;
	va_list	ap;
	char	*argv[32],
		*mess;
	File	*fp;
	SIGRESULT	(*old_int) proto((int));

	va_init(ap, clobber);
	make_argv(argv, ap);
	va_end(ap);
	if (bnm != 0 && clobber == YES)
		isprocbuf(bnm);
	if (disp) {
		if (bnm != 0)
			message("Starting up...");
		else {
			TOstart(argv[0], TRUE);
			Typeout("Starting up...");
		}
		if (bnm != 0) {
			pop_wind(bnm, clobber, clobber ? B_PROCESS : B_FILE);
			set_wsize(wsize);
			redisplay();
		}
	}
	/* Now I will attempt to describe how I deal with signals during
	   the execution of the shell command.  My desire was to be able
	   to interrupt the shell command AS SOON AS the window pops up.
	   So, if we have JOB_CONTROL (i.e., the new signal mechanism) I
	   hold SIGINT, meaning if we interrupt now, we will eventually
	   see the interrupt, but not before we are ready for it.  We
	   fork, the child releases the interrupt, it then sees the
	   interrupt, and so exits.  Meanwhile the parent ignores the
	   signal, so if there was a pending one, it's now lost.

	   With no JOB_CONTROL, the best behavior you can expect is, when
	   you type ^] too very quickly after the window pops up, it may
	   be ignored.  The behavior BEFORE was that it would interrupt
	   JOVE and then you would have to continue JOVE and wait a
	   little while longer before trying again.  Now that is fixed,
	   in that you just have to type it twice. */

#ifndef MSDOS
# ifdef IPROCS
	SigHold(SIGCHLD);
# endif
# ifdef JOB_CONTROL
	SigHold(SIGINT);
# else
	old_int = signal(SIGINT, SIG_IGN),
# endif
	dopipe(p);
	pid = vfork();
	if (pid == -1) {
		pclose(p);
		complain("[Fork failed]");
	}
	if (pid == 0) {
# ifdef BSD_SIGS
		/*
		 * We want to release SIGCHLD and SIGINT in the child, but
		 * we can't use SigRelse because that would change Jove's
		 * copy of the SigMask variable (because we're in a
		 * vfork).  So we simply set set the mask directly.  There
		 * are several other forks in Jove, but this is the only
		 * one we execute often enough to make it worth using a
		 * vfork.
		 */
		(void) signal(SIGINT, SIG_DFL);
		(void) sigsetmask(SigMask & ~(sigmask(SIGCHLD)|sigmask(SIGINT)));
# else /* BSD_SIGS */
# ifdef IPROCS
		SigRelse(SIGCHLD);   /* don't know if this matters */
# endif /* IPROCS */
		(void) signal(SIGINT, SIG_DFL);
# ifdef JOB_CONTROL
		SigRelse(SIGINT);
# endif
# endif /* BSD_SIGS */
		(void) close(0);
		(void) open("/dev/null", 0);
		(void) close(1);
		(void) close(2);
		(void) dup(p[1]);
		(void) dup(p[1]);
		pclose(p);
		execv(argv[0], (const char **) &argv[1]);
		(void) write(1, "Execl failed.\n", (size_t) 14);
		_exit(1);
	}
# ifdef JOB_CONTROL
	old_int = signal(SIGINT, SIG_IGN);
# endif
	(void) close(p[1]);
	fp = fd_open(argv[1], F_READ, p[0], iobuff, LBSIZE);
#else /* MSDOS */
	if ((p0 = openforpipe()) < 0)
	   complain("cannot make pipe for filter");

	oldo = dup(1);
	olde = dup(2);
	close(1);
	close(2);
	dup(p0);
	dup(1);
	close(p0);
	retcode = spawnv(0, argv[0], &argv[1]);
	p0 = reopenforpipe();
	close(1);
	close(2);
	dup(oldo);
	dup(olde);
	close(oldo);
	close(olde);

	if (retcode < 0)
		complain("[Spawn failed]");

	fp = fd_open(argv[1], F_READ, p0, iobuff, LBSIZE);
#endif /* MSDOS */
	do {
#ifndef MSDOS
		inIOread = 1;
#endif
		eof = f_gets(fp, genbuf, (size_t)LBSIZE);
#ifndef MSDOS
		inIOread = 0;
#endif
		if (bnm != 0) {
			ins_str(genbuf, YES);
			if (!eof)
				LineInsert(1);
		} else if (disp == YES)
			Typeout("%s", genbuf);
		if (bnm != 0 && disp != 0 && fp->f_cnt <= 0) {
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
#endif /* LOAD_AV */
			if (bnm != 0) {
				message(mess);
				redisplay();
			}
		}
	} while (!eof);
	if (disp)
		DrawMesg(NO);
	close_file(fp);
#ifndef MSDOS
	dowait(pid, &status);
# ifdef JOB_CONTROL
	(void) SigRelse(SIGINT);
# endif
#else /* MSDOS */
	closepipe();
#endif /* MSDOS */
	(void) signal(SIGINT, old_int);
#ifndef MSDOS
# ifdef IPROCS
	SigRelse(SIGCHLD);
# endif
	return status;
#else /* MSDOS */
	getCWD();
	return retcode;
#endif /* MSDOS */
}

/* Send the current region to CMD and insert the output from the
   command into OUT_BUF. */

void
RegToUnix(outbuf, cmd)
Buffer	*outbuf;
char	*cmd;
{
	Mark	*m = CurMark();
#ifndef MSDOS
	static char     tnambuf[20];
	char    *tname,
		combuf[128];
#endif /* MSDOS */
	Window	*save_wind = curwind;
	int	status,
		err = NO;
#ifdef MSDOS
	int p0, oldi;
#endif /* MSDOS */
	File	*fp;

#ifndef MSDOS
	strcpy (tnambuf, "/tmp/jfilterXXXXXX");
	tname = mktemp(tnambuf);
	fp = open_file(tname, iobuff, F_WRITE, YES, YES);
#else /* MSDOS */
	p0 = openforpipe();
#endif /* MSDOS */
    CATCH
#ifdef MSDOS
	fp = fd_open(cmd, F_WRITE, p0, iobuff, LBSIZE);
#endif /* MSDOS */
	putreg(fp, m->m_line, m->m_char, curline, curchar, YES);
	DelReg();
#ifndef MSDOS
	swritef(combuf, "%s < %s", cmd, tname);
#else /* MSDOS */
	f_close(fp);
	p0 = reopenforpipe();
    oldi = dup(0);
	close(0);
	dup(p0);
	close(p0);
#endif /* MSDOS */
	status = UnixToBuf(outbuf->b_name, NO, 0, outbuf->b_type == B_SCRATCH,
#ifndef MSDOS
			   Shell, ShFlags, combuf, (char *) 0
#else /* MSDOS */
			   Shell, ShFlags, cmd, (char *) 0
#endif /* MSDOS */
			   );
    ONERROR
	err = YES;
    ENDCATCH
#ifndef MSDOS
	f_close(fp);
	(void) unlink(tname);
#else /* MSDOS */
	close(0);
	open("con", 0);	/* dup(oldi);	*/
	close(oldi);
	closepipe();
#endif /* MSDOS */
	SetWind(save_wind);
	if (err == NO)
#ifndef MSDOS
		com_finish(status, combuf);
#else
		com_finish(status, cmd);
#endif
}

void
FilterRegion()
{
	static char	FltComBuf[LBSIZE];

	null_ncpy(FltComBuf, ask(FltComBuf, ": %f (through command) "),
		(sizeof FltComBuf) - 1);
	RegToUnix(curbuf, FltComBuf);
}

void
isprocbuf(bnm)
char	*bnm;
{
	Buffer	*bp;

	if ((bp = buf_exists(bnm)) != 0 && bp->b_type != B_PROCESS)
		confirm("Over-write buffer %s?", bnm);
}

#ifdef MSDOS
/*	msdos specific hacks to allow for pipes */

#include <dos.h>
#include <fcntl.h>
#include <sys/stat.h>

static char pipeiname[64];
static char pipeoname[64];
static int  pipehandle;

private int
openforpipe()
{
   swritef(pipeiname, "%s/%s", TmpFilePath, "Jove-I");
   swritef(pipeoname, "%s/%s", TmpFilePath, "Jove-O");

   return(pipehandle = creat(pipeoname, S_IWRITE|S_IREAD));
}

private int
reopenforpipe()
{
   close(pipehandle);
   unlink(pipeiname);
   rename(pipeoname, pipeiname);
   if ((pipehandle = open(pipeiname, 0)) >= 0)
      return(pipehandle);
   closepipe();
   return(-1);
}

private void
closepipe()
{
   unlink(pipeoname);
   unlink(pipeiname);
}

char
switchar()
{
  union REGS regs;

  regs.h.ah = 0x37;
  regs.h.al = 0;
  intdos(&regs, &regs);
  return(regs.h.dl);
}
#endif /* MSDOS */
