/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "re.h"
#include "ctype.h"
#include "disp.h"
#if defined(IPROCS)
# include "fp.h"
# include "iproc.h"
#endif

#ifdef	STDARGS
# include <stdargs.h>
#else
# include <varargs.h>
#endif

#ifdef IPROCS

private void
	proc_rec proto ((Process *, char *)),
	proc_close proto ((Process *)),
	proc_kill proto((Process *, int)),
	SendData proto ((int));

private int
	proc_child proto((int));

#ifdef PIPEPROCS
#   include "iproc-pipes.c"
#else
#   include "iproc-ptys.c"
#endif

char	proc_prompt[128] = "% ";

char *
pstate(p)
Process	*p;
{
	switch (proc_state(p)) {
	case NEW:
		return "New";

	case STOPPED:
		return "Stopped";

	case RUNNING:
		return "Running";

	case DEAD:
		if (p->p_howdied == EXITED) {
			if (p->p_reason == 0)
				return "Done";
			return sprint("Exit %d", p->p_reason);
		}
		return sprint("Killed %d", p->p_reason);

	default:
		return "Unknown state";
	}
}

void
KillProcs()
{
	register Process	*p;
	register int	killem = -1;		/* -1 means undetermined */
	register char	*yorn;

	for (p = procs; p != 0; p = p->p_next)
		if (!isdead(p)) {
			if (killem == -1) {
				yorn = ask("y", "Should I kill your i-processes? ");
				killem = (CharUpcase(*yorn) == 'Y');
			}
			if (killem)
				proc_kill(p, SIGKILL);
		}
}

void
pbuftiedp(b)
register Buffer	*b;
{
	register Process	*p = b->b_process;

	if (!isdead(p))
		complain("Process %s, attached to %b, is %s.",
			 proc_cmd(p), b, pstate(p));
}

char	dbx_parse_fmt[128] = "line \\([0-9]*\\) in \\{file,\\} *\"\\([^\"]*\\)\"";

void
DBXpoutput()
{
	if (curbuf->b_process == 0)
		complain("[Must be in a process buffer to enable dbx mode]");
	curbuf->b_process->p_dbx_mode = !curbuf->b_process->p_dbx_mode;
	UpdModLine = YES;
}

private void
watch_input(m)
Mark	*m;
{
	Bufpos	save;
	char	fname[FILESIZE],
		lineno[FILESIZE];
	int	lnum;
	Window	*savew = curwind;
	Buffer	*buf;

	DOTsave(&save);
	ToMark(m);
	if (dosearch(dbx_parse_fmt, FORWARD, YES) != NULL) {
		get_FL_info(fname, lineno);
		buf = do_find((Window *) 0, fname, YES);
		pop_wind(buf->b_name, NO, -1);
		lnum = atoi(lineno);
		SetLine(next_line(buf->b_first, lnum - 1));
		SetWind(savew);
	}
	SetDot(&save);
}

/* Process receive: receives the characters in buf, and appends them to
   the buffer associated with p. */

private void
proc_rec(p, buf)
register Process	*p;
char	*buf;
{
	Buffer	*saveb = curbuf;
	register Window	*w;
	register Mark	*savepoint;
	int	sameplace = NO,
		do_disp = NO;

	if (curwind->w_bufp == p->p_buffer)
		w = curwind;
	else
		w = windbp(p->p_buffer);	/* Is this window visible? */
	if (w != 0)
		do_disp = (in_window(w, p->p_mark->m_line) != -1);
	SetBuf(p->p_buffer);
	savepoint = MakeMark(curline, curchar, M_FLOATER);
	ToMark(p->p_mark);		/* where output last stopped */
	if (savepoint->m_line == curline && savepoint->m_char == curchar)
		sameplace = YES;
	ins_str(buf, YES);
	if (do_disp == YES && p->p_dbx_mode == YES)
		watch_input(p->p_mark);
	MarkSet(p->p_mark, curline, curchar);
	if (!sameplace)
		ToMark(savepoint);	/* back to where we were */
	DelMark(savepoint);
	/* redisplay now, instead of right after the ins_str, so that
	   we don't get a bouncing effect if point is not the same as
	   the process output position */
	if (do_disp) {
		w->w_line = curline;
		w->w_char = curchar;
		redisplay();
	}
	SetBuf(saveb);
}

private void
proc_kill(p, sig)
register Process	*p;
int	sig;
{
	if (isdead(p))
		return;
	if (killpg(p->p_pid, sig) == -1)
		s_mess("Cannot kill %s!", proc_buf(p));
}

/* Free process CHILD.  Do all the necessary cleaning up (closing fd's,
   etc.). */

private void
free_proc(child)
Process	*child;
{
	register Process	*p,
				*prev = 0;

	if (!isdead(child))
		return;
	for (p = procs; p != child; prev = p, p = p->p_next)
		;
	if (prev == 0)
		procs = child->p_next;
	else
		prev->p_next = child->p_next;
	proc_close(child);		/* if not already closed */

	/* It's possible that the buffer has been given another process
	   between the time CHILD dies and CHILD's death is noticed (via
	   list-processes).  So we only set it the buffer's process to
	   0 if CHILD is still the controlling process. */
	if (child->p_buffer->b_process == child) {
		child->p_buffer->b_process = 0;
	}
	{
		Buffer	*old = curbuf;

		SetBuf(child->p_buffer);
		DelMark(child->p_mark);
		SetBuf(old);
	}
	free((char *) child->p_name);
	free((char *) child);
}

void
ProcList()
{
	register Process	*p,
				*next;
	char	*fmt = "%-15s  %-15s  %-8s %s",
		pidstr[16];

	if (procs == 0) {
		message("[No subprocesses]");
		return;
	}
	TOstart("Process list", TRUE);

	Typeout(fmt, "Buffer", "Status", "Pid ", "Command");
	Typeout(fmt, "------", "------", "--- ", "-------");
	for (p = procs; p != 0; p = next) {
		next = p->p_next;
		swritef(pidstr, "%d", p->p_pid);
		Typeout(fmt, proc_buf(p), pstate(p), pidstr, p->p_name);
		if (isdead(p)) {
			free_proc(p);
			UpdModLine = YES;
		}
	}
	TOstop();
}

private void
do_rtp(mp)
register Mark	*mp;
{
	register Process	*p = curbuf->b_process;
	Line	*line1 = curline,
		*line2 = mp->m_line;
	int	char1 = curchar,
		char2 = mp->m_char;
	char	*gp;
	size_t	nbytes;

	if (isdead(p) || p->p_buffer != curbuf)
		return;

	(void) fixorder(&line1, &char1, &line2, &char2);
	while (line1 != line2->l_next) {
		gp = ltobuf(line1, genbuf) + char1;
		if (line1 == line2)
			gp[char2] = '\0';
		else
			strcat(gp, "\n");
		if ((nbytes = strlen(gp)) != 0)
			proc_write(p, gp, nbytes);
		line1 = line1->l_next;
		char1 = 0;
	}
}

void
ProcNewline()
{
#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
	SendData(YES);
}

void
ProcSendData()
{
#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
	SendData(NO);
}

private void
SendData(newlinep)
int	newlinep;
{
	register Process	*p = curbuf->b_process;
	register char	*lp,
			*gp;	/* JF fix for better prompt handling */

	if (isdead(p))
		return;
	/* If the process mark was involved in a big deletion, because
	   the user hit ^W or something, then let's do some magic with
	   the process mark.  Problem is that if the user yanks back the
	   text he deleted, the mark stays at the beginning of the region,
	   and so the next time SendData() is called the entire region
	   will be sent.  That's not good.  So, to deal with that we reset
	   the mark to the last line, after skipping over the prompt, etc. */
	if (p->p_mark->m_flags & M_BIG_DELETE) {
		Bufpos	bp;

		p->p_mark->m_flags &= ~M_BIG_DELETE;

		DOTsave(&bp);
		ToLast();
		Bol();
		/* While we're looking at a prompt, and while we're
		   moving forward.  This is for people who accidently
		   set their process-prompt to ">*" which will always
		   match! */
		while ((LookingAt(proc_prompt, linebuf, curchar)) &&
		       (REeom > curchar))
			curchar = REeom;
		MarkSet(p->p_mark, curline, curchar);
		SetDot(&bp);
	}

	if (lastp(curline)) {
		Eol();
		if (newlinep)
			LineInsert(1);
		do_rtp(p->p_mark);
		MarkSet(p->p_mark, curline, curchar);
	} else {
		/* Either we're looking at a prompt, or we're not, in
		   which case we want to strip off the beginning of the
		   line anything that looks like what the prompt at the
		   end of the file is.  In other words, if "(dbx) stop in
		   ProcessNewline" is the line we're on, and the last
		   line in the buffer is "(dbx) ", then we strip off the
		   leading "(dbx) " from this line, because we know it's
		   part of the prompt.  But this only happens if "(dbx) "
		   isn't one of the process prompts ... follow what I'm
		   saying? */
		Bol();
		if (LookingAt(proc_prompt, linebuf, curchar)) {
			do
				curchar = REeom;
			while ((LookingAt(proc_prompt, linebuf, curchar)) &&
			       (REeom > curchar));
			strcpy(genbuf, linebuf + curchar);
			Eof();
			ins_str(genbuf, NO);
		} else {
			strcpy(genbuf, linebuf + curchar);
			Eof();
			gp = genbuf;
			lp = linebuf;
			while (*lp == *gp && *lp != '\0') {
				lp += 1;
				gp += 1;
			}
			ins_str(gp, NO);
		}
	}
}

void
ShellProc()
{
	char	*shbuf = "*shell*";
	register Buffer	*b;

	b = buf_exists(shbuf);
	if (b == 0 || isdead(b->b_process))
		proc_strt(shbuf, NO, Shell, "-i", (char *) 0);
	pop_wind(shbuf, NO, -1);
}

void
Iprocess()
{
	register char	*command;
	char	scratch[64],
		*bnm;
	int	cnt = 1;
	Buffer	*bp;

	command = ask(ShcomBuf, ProcFmt);
	null_ncpy(ShcomBuf, command, (sizeof ShcomBuf) - 1);
	bnm = MakeName(command);
	strcpy(scratch, bnm);
	while ((bp = buf_exists(scratch)) != NIL && !isdead(bp->b_process))
		swritef(scratch, "%s.%d", bnm, cnt++);
	proc_strt(scratch, YES, Shell, ShFlags, command, (char *) 0);
}

private SIGRESULT
proc_child(junk)
int	junk;	/* needed for signal handler; not used */
{
	union wait	w;
	register int	pid;

	for (;;) {
#ifndef WAIT3
		pid = wait2(&w.w_status, (WNOHANG | WUNTRACED));
#else
		pid = wait3(&w, (WNOHANG | WUNTRACED), (struct rusage *) 0);
#endif
		if (pid <= 0)
			break;
		kill_off(pid, w);
	}
	SIGRETURN;
}

void
kill_off(pid, w)
register int	pid;
union wait	w;
{
	register Process	*child;

	if ((child = proc_pid(pid)) == 0)
		return;

	UpdModLine = YES;		/* we're changing state ... */
	if (WIFSTOPPED(w))
		child->p_state = STOPPED;
	else {
		child->p_state = DEAD;
		if (WIFEXITED(w))
			child->p_howdied = EXITED;
		else if (WIFSIGNALED(w)) {
			child->p_reason = w_termsignum(w);
			child->p_howdied = KILLED;
		}
		{
			Buffer	*save = curbuf;
			char	mesg[128];

			/* insert status message now */
			swritef(mesg, "[Process %s: %s]\n",
				proc_cmd(child),
				pstate(child));
			SetBuf(child->p_buffer);
			ins_str(mesg, NO);
			SetBuf(save);
			redisplay();
		}
	}
}

#endif /* IPROCS */
