/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "termcap.h"
#include "ctype.h"
#include "chars.h"
#include "disp.h"
#include "re.h"

#if defined(JOB_CONTROL) || defined(IPROCS)
# include <signal.h>
#endif

#ifdef MAC
# include "mac.h"
#else
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
#endif

#ifdef MSDOS
# include <process.h>
#endif

private void
	DefAutoExec proto((struct data_obj *(*proc)()));

private int
	match proto((char **, char *));

int	InJoverc = 0;

/* Auto execute code */

#define NEXECS	20

private struct {
	char	*a_pattern;
	data_obj	*a_cmd;
} AutoExecs[NEXECS];	/* must be initialized by system to 0 */

private int	ExecIndex = 0;

/* Command auto-execute. */

void
CAutoExec()
{
	DefAutoExec(findcom);
}

/* Macro auto-execute. */

void
MAutoExec()
{
	DefAutoExec(findmac);
}

private void
DefAutoExec(proc)
#if defined(MAC) || defined(IBMPC)
data_obj	*(*proc)();
#else
data_obj	*(*proc) proto((char *));
#endif
{
	data_obj	*d;
	char	*pattern;
	int	i;

	if (ExecIndex >= NEXECS)
		complain("Too many auto-executes, max %d.", NEXECS);
	if ((d = (*proc)(ProcFmt)) == 0)
		return;
	pattern = do_ask("\r\n", (int (*) proto((int))) 0, (char *) 0, ": %f %s ",
		d->Name);
	for (i = 0; i < ExecIndex; i++) {
	    if (AutoExecs[i].a_cmd == d) {
		char    *ipat = AutoExecs[i].a_pattern;

		if ((pattern == NULL || ipat == NULL)?
		    (pattern == ipat) : (strcmp(pattern, ipat) == 0))
			return;		/* eliminate duplicates */
	    }
	}
	AutoExecs[ExecIndex].a_pattern = copystr(pattern);
	AutoExecs[ExecIndex].a_cmd = d;
	ExecIndex += 1;
}

/* DoAutoExec: NEW and OLD are file names, and if NEW and OLD aren't the
   same kind of file (i.e., match the same pattern) or OLD is 0 and it
   matches, OR if the pattern is 0 (none was specified) then, we execute
   the command associated with that kind of file. */

void
DoAutoExec(new, old)
register char	*new,
		*old;
{
	register int	i;

	set_arg_value(1);
	for (i = 0; i < ExecIndex; i++)
		if ((AutoExecs[i].a_pattern == 0) ||
		    ((new != 0 && LookingAt(AutoExecs[i].a_pattern, new, 0)) &&
		     (old == 0 || !LookingAt(AutoExecs[i].a_pattern, old, 0))))
			ExecCmd(AutoExecs[i].a_cmd);
}

int
addgetc()
{
	int	c;

	if (!InJoverc) {
		Asking = strlen(mesgbuf);
		c = getch();
		Asking = 0;
		add_mess("%p ", c);
	} else {
		c = getch();
		if (c == '\n')
			return EOF;	/* this isn't part of the sequence */
		else if (c == '\\') {
			if ((c = getch()) == LF)
				complain("[Premature end of line]");
		} else if (c == '^') {
			if ((c = getch()) == '?')
				c = RUBOUT;
			else if (isalpha(c) || strchr("@[\\]^_", c))
				c = CTL(c);
			else
				complain("[Unknown control character]");
		}
	}
	return c;
}

void
Extend()
{
	data_obj	*d;

	if ((d = findcom(": ")) != NULL)
		ExecCmd(d);
}

/* Read a positive integer from CP.  It must be in base BASE, and
   complains if it isn't.  If allints is nonzero, all the characters
   in the string must be integers or we return -1; otherwise we stop
   reading at the first nondigit. */

int
chr_to_int(cp, base, allints, result)
register char	*cp;
int	base,
	allints;
register int	*result;
{
	register int	c;
	int	value = 0,
		sign;

	if ((c = *cp) == '-') {
		sign = -1;
		cp += 1;
	} else
		sign = 1;
	while ((c = *cp++) != '\0') {
		if (!isdigit(c)) {
			if (allints == YES)
				return INT_BAD;
			break;
		}
		c = c - '0';
		if (c >= base)
			complain("You must specify in base %d.", base);
		value = value * base + c;
	}
	*result = value * sign;
	return INT_OKAY;
}

int
ask_int(prompt, base)
char	*prompt;
int	base;
{
	char	*val = ask((char *) 0, prompt);
	int	value;

	if (chr_to_int(val, base, YES, &value) == INT_BAD)
		complain("That's not a number!");
	return value;
}

void
vpr_aux(vp, buf)
register const struct variable	*vp;
char	*buf;
{
	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
		swritef(buf, "%d", *((int *) vp->v_value));
		break;

	case V_BASE8:
		swritef(buf, "%o", *((int *) vp->v_value));
		break;

	case V_BOOL:
		swritef(buf, (*((int *) vp->v_value)) ? "on" : "off");
		break;

	case V_STRING:
	case V_FILENAME:
		swritef(buf, "%s", vp->v_value);
		break;

	case V_CHAR:
		swritef(buf, "%p", *((int *) vp->v_value));
		break;
	}
}

void
PrVar()
{
	struct variable	*vp;
	char	prbuf[256];

	if ((vp = (struct variable *) findvar(ProcFmt)) == 0)
		return;
	vpr_aux(vp, prbuf);
	s_mess(": %f %s => %s", vp->Name, prbuf);
}

void
SetVar()
{
	struct variable	*vp;
	char	*prompt;

	if ((vp = (struct variable *) findvar(ProcFmt)) == 0)
		return;
	prompt = sprint(": %f %s ", vp->Name);

	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
	case V_BASE8:
	    {
		int	value;

		value = ask_int(prompt, ((vp->v_flags & V_TYPEMASK) == V_BASE10)
					  ? 10 : 8);
		*((int *) vp->v_value) = value;
		break;
	    }

	case V_BOOL:
	    {
		char	*def = *((int *) vp->v_value) ? "off" : "on",
			*on_off;
		int	value;

		on_off = ask(def, prompt);
		if (casecmp(on_off, "on") == 0)
			value = ON;
		else if (casecmp(on_off, "off") == 0)
			value = OFF;
		else
			complain("Boolean variables must be ON or OFF.");
		*((int *) vp->v_value) = value;
#ifdef MAC
		MarkVar(vp,-1,0);	/* mark the menu item */
#endif
		s_mess("%s%s", prompt, value ? "on" : "off");
		break;
	    }

	case V_FILENAME:
	    {
		char	fbuf[FILESIZE];

		(void) ask_file(prompt, (char *) vp->v_value, fbuf);
		strcpy((char *) vp->v_value, fbuf);
		break;
	    }

	case V_STRING:
	    {
		char	*str;

		/* Do_ask() so you can set string to "" if you so desire. */
		str = do_ask("\r\n", (int (*) proto((int))) 0, (char *) vp->v_value,
			prompt);
		if (str == 0)
			str = NullStr;
		strcpy(vp->v_value, str);
		/* ... and hope there is enough room. */
		break;
	    }
	case V_CHAR:
		f_mess(prompt);
		*((int *) vp->v_value) = addgetc();
		break;

	}
	if (vp->v_flags & V_MODELINE)
		UpdModLine = YES;
	if (vp->v_flags & V_CLRSCREEN) {
#ifdef IBMPC
		setcolor(Fgcolor, Bgcolor);
#endif /* IBMPC */
		ClAndRedraw();
	}
	if (vp->v_flags & V_TTY_RESET)
		tty_reset();
}

/* Command completion - possible is an array of strings, prompt is
   the prompt to use, and flags are ... well read jove.h.

   If flags are RET_STATE, and the user hits <return> what they typed
   so far is in the Minibuf string. */

private char	**Possible;
private int	comp_value,
		comp_flags;

int
aux_complete(c)
int	c;
{
	int	command,
		i;

	if (comp_flags & CASEIND) {
		char	*lp;

		for (lp = linebuf; *lp != '\0'; lp++)
#if (defined(IBMPC) || defined(MAC))
			lower(lp);
#else
			if (isupper(*lp))
				*lp = tolower(*lp);
#endif
	}
	switch (c) {
	case EOF:
		comp_value = -1;
		return 0;

	case '\r':
	case '\n':
		command = match(Possible, linebuf);
		if (command >= 0) {
			comp_value = command;
			return 0;	/* tells ask to stop */
		}
		if (eolp() && bolp()) {
			comp_value = NULLSTRING;
			return 0;
		}
		if (comp_flags & RET_STATE) {
			comp_value = command;
			return 0;
		}
		if (InJoverc)
			complain("[\"%s\" unknown]", linebuf);
		rbell();
		break;

	case '\t':
	case ' ':
	    {
		int	minmatch = 1000,
			maxmatch = 0,
			numfound = 0,
			lastmatch = -1,
			len = strlen(linebuf);

		for (i = 0; Possible[i] != 0; i++) {
			int	this_len;

			this_len = numcomp(Possible[i], linebuf);
			maxmatch = max(maxmatch, this_len);
			if (this_len >= len) {
				if (numfound)
					minmatch = min(minmatch, numcomp(Possible[lastmatch], Possible[i]));
				else
					minmatch = strlen(Possible[i]);
				numfound += 1;
				lastmatch = i;
				if (strcmp(linebuf, Possible[i]) == 0)
					break;
			}
		}

		if (numfound == 0) {
			rbell();
			if (InJoverc)
				complain("[\"%s\" unknown]", linebuf);
			/* If we're not in the .joverc then
			   let's do something helpful for the
			   user. */
			if (maxmatch < len) {
				char	*cp;

				cp = linebuf + maxmatch;
				*cp = 0;
				Eol();
			}
			break;
		}
		if (c != '\t' && numfound == 1) {
			comp_value = lastmatch;
			return 0;
		}
		null_ncpy(linebuf, Possible[lastmatch], (size_t) minmatch);
		Eol();
		if (minmatch == len)	/* No difference */
			rbell();
		break;
	    }

	case '?':
		{
		int	len;

		if (InJoverc)
			complain((char *) 0);
		/* kludge: in case we're using UseBuffers, in which case
		   linebuf gets written all over */
		strcpy(Minibuf, linebuf);
		len = strlen(Minibuf);
		TOstart("Completion", TRUE);	/* for now ... */
		for (i = 0; Possible[i]; i++)
			if (numcomp(Possible[i], Minibuf) >= len) {
				Typeout(Possible[i]);
				if (TOabort != 0)
					break;
			}

		TOstop();
		}
		break;
	}
	return !FALSE;
}

int
complete(possible, prompt, flags)
register char	*possible[];
char	*prompt;
int	flags;
{
	/* protect static "Possible" from being overwritten due to recursion */
	if (InRealAsk)
		complain((char *) NULL);

	Possible = possible;
	comp_flags = flags;
	(void) do_ask("\r\n \t?", aux_complete, NullStr, prompt);
	return comp_value;
}

private int
match(choices, what)
register char	**choices,
		*what;
{
	register size_t	len;
	int	i,
		found = 0,
		save,
		exactmatch = -1;

	len = strlen(what);
	if (len == 0)
		return NULLSTRING;
	for (i = 0; choices[i]; i++) {
		if (strncmp(what, choices[i], len) == 0) {
			if (strcmp(what, choices[i]) == 0)
				exactmatch = i;
			save = i;
			found += 1;	/* found one */
		}
	}

	if (found == 0)
		save = ORIGINAL;
	else if (found > 1) {
		if (exactmatch != -1)
			save = exactmatch;
		else
			save = AMBIGUOUS;
	}

	return save;
}

void
Source()
{
	extern char	*getenv();
	char	*com,
		buf[FILESIZE];

#ifndef MSDOS
	swritef(buf, "%s/.joverc", HomeDir);
#else /* MSDOS */
	if (com = getenv("JOVERC"))
		strcpy(buf, com);
	else
		strcpy(buf, Joverc);
#endif /* MSDOS */
	com = ask_file((char *) 0, buf, buf);
	if (joverc(buf) == 0)
		complain(IOerr("read", com));
}

void
BufPos()
{
	register Line	*lp = curbuf->b_first;
	register int	i,
			dotline;
	long	dotchar,
		nchars;

	for (i = nchars = 0; lp != 0; i++, lp = lp->l_next) {
		if (lp == curline) {
			dotchar = nchars + curchar;
			dotline = i + 1;
		}
		nchars += length(lp) + (lp->l_next != 0); /* include the NL */
	}

	s_mess("[\"%s\" line %d/%d, char %D/%D (%d%%), cursor = %d/%d]",
	       filename(curbuf), dotline, i, dotchar, nchars,
	       (nchars == 0) ? 100 : (int) (((long) dotchar * 100) / nchars),
	       calc_pos(linebuf, curchar),
	       calc_pos(linebuf, (int)strlen(linebuf)));
}

#define IF_UNBOUND	(-1)
#define IF_TRUE		1
#define IF_FALSE	(!IF_TRUE)

#ifndef MAC
private int
do_if(cmd)
char	*cmd;
{
#ifdef MSDOS
	int status;
#else
	int	pid,
		status;
#endif /* MSDOS */
#ifndef MSDOS

	switch (pid = fork()) {
	case -1:
		complain("[Fork failed: if]");
		/*NOTREACHED*/

	case 0:
	    {
#endif /* MSDOS */
		char	*args[12],
			*cp = cmd,
			**ap = args;

		*ap++ = cmd;
		for (;;) {
			if ((cp = strchr(cp, ' ')) == 0)
				break;
			*cp++ = '\0';
			*ap++ = cp;
		}
		*ap = 0;

#ifndef MSDOS
		close(0);	/*	we want reads to fail */
		/* close(1);	 but not writes or ioctl's
		close(2);    */
#else /* MSDOS */
	if ((status = spawnvp(0, args[0], args)) < 0)
		complain("[Spawn failed: if]");
#endif /* MSDOS */

#ifndef MSDOS
		(void) execvp(args[0], (const char **)args);
		_exit(-10);	/* signals exec error (see below) */
	    }
	}
#ifdef IPROCS
	SigHold(SIGCHLD);
#endif
	dowait(pid, &status);
#ifdef IPROCS
	SigRelse(SIGCHLD);
#endif
	if (status == -10)
		complain("[Exec failed]");
	if (status < 0)
		complain("[Exit %d]", status);
#endif /* MSDOS */
	return (status == 0);	/* 0 means successful */
}
#endif /* MAC */

int
joverc(file)
char	*file;
{
	char	buf[LBSIZE],
		lbuf[LBSIZE];
	int	lnum = 0,
		eof = FALSE;
	jmp_buf	savejmp;
	int	IfStatus = IF_UNBOUND;
	File	*fp;

	fp = open_file(file, buf, F_READ, NO, YES);
	if (fp == NIL)
		return NO;	/* joverc returns an integer */

	/* Catch any errors, here, and do the right thing with them,
	   and then restore the error handle to whoever did a setjmp
	   last. */

	InJoverc += 1;
	push_env(savejmp);
	if (setjmp(mainjmp)) {
		Buffer	*savebuf = curbuf;

		SetBuf(do_select((Window *) 0, "RC errors"));
		ins_str(sprint("%s:%d:%s\t%s\n", pr_name(file, YES), lnum, lbuf, mesgbuf), NO);
		unmodify();
		SetBuf(savebuf);
		Asking = 0;
	}
	if (!eof) do {
		eof = (f_gets(fp, lbuf, sizeof lbuf) == EOF);
		lnum += 1;
		if (lbuf[0] == '#')		/* a comment */
			continue;
#ifndef MAC
		if (casencmp(lbuf, "if", (size_t)2) == 0) {
			char	cmd[128];

			if (IfStatus != IF_UNBOUND)
				complain("[Cannot have nested if's]");
			if (LookingAt("if[ \t]*\\(.*\\)$", lbuf, 0) == 0)
				complain("[If syntax error]");
			putmatch(1, cmd, sizeof cmd);
			IfStatus = do_if(cmd) ? IF_TRUE : IF_FALSE;
			continue;
		} else if (casencmp(lbuf, "else", (size_t)4) == 0) {
			if (IfStatus == IF_UNBOUND)
				complain("[Unexpected `else']");
			IfStatus = !IfStatus;
			continue;
		} else if (casencmp(lbuf, "endif", (size_t)5) == 0) {
			if (IfStatus == IF_UNBOUND)
				complain("[Unexpected `endif']");
			IfStatus = IF_UNBOUND;
			continue;
		}
#endif
		if (IfStatus == IF_FALSE)
			continue;
		(void) strcat(lbuf, "\n");
		Inputp = lbuf;
		while (*Inputp == ' ' || *Inputp == '\t')
			Inputp += 1;	/* skip white space */
		Extend();
	} while (!eof);

	f_close(fp);
	pop_env(savejmp);
	Inputp = 0;
	Asking = 0;
	InJoverc -= 1;
	if (IfStatus != IF_UNBOUND)
		complain("[Missing endif]");
	return 1;
}
