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

#ifdef	IPROCS
# include <signal.h>
#endif

#ifdef	MAC
# include "mac.h"
#else
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
#endif

#ifdef	MSDOS
# include <process.h>
#endif

private void
	DefAutoExec proto((struct data_obj *(*proc) ptrproto((const char *))));

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
data_obj	*(*proc) ptrproto((const char *));
{
	data_obj	*d;
	char	*pattern;
	int	i;

	if (ExecIndex >= NEXECS)
		complain("Too many auto-executes, max %d.", NEXECS);
	if ((d = (*proc)(ProcFmt)) == NULL)
		return;
	pattern = do_ask("\r\n", (bool (*) ptrproto((int))) NULL, (char *) NULL, ": %f %s ",
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
   same kind of file (i.e., match the same pattern) or OLD is NULL and it
   matches, OR if the pattern is NULL (none was specified) then, we execute
   the command associated with that kind of file. */

void
DoAutoExec(new, old)
register char	*new,
		*old;
{
	register int	i;

	set_arg_value(1);
	for (i = 0; i < ExecIndex; i++)
		if ((AutoExecs[i].a_pattern == NULL) ||
		    ((new != NULL && LookingAt(AutoExecs[i].a_pattern, new, 0)) &&
		     (old == NULL || !LookingAt(AutoExecs[i].a_pattern, old, 0))))
			ExecCmd(AutoExecs[i].a_cmd);
}

int
addgetc()
{
	int	c;

	if (!InJoverc) {
		Asking = YES;
		AskingWidth = strlen(mesgbuf);
		c = getch();
		Asking = NO;
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
			else if (jisalpha(c) || strchr("@[\\]^_", c))
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
		if (!jisdigit(c)) {
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
	char	*val = ask((char *)NULL, prompt);
	int	value;

	if (chr_to_int(val, base, YES, &value) == INT_BAD)
		complain("That's not a number!");
	return value;
}

void
vpr_aux(vp, buf, size)
register const struct variable	*vp;
char	*buf;
size_t	size;
{
	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
		swritef(buf, size, "%d", *((int *) vp->v_value));
		break;

	case V_BASE8:
		swritef(buf, size, "%o", *((int *) vp->v_value));
		break;

	case V_BOOL:
		swritef(buf, size, (*((int *) vp->v_value)) ? "on" : "off");
		break;

	case V_STRING:
	case V_FILENAME:
		swritef(buf, size, "%s", vp->v_value);
		break;

	case V_CHAR:
		swritef(buf, size, "%p", *((int *) vp->v_value));
		break;
	}
}

void
PrVar()
{
	struct variable	*vp;
	char	prbuf[256];

	if ((vp = (struct variable *) findvar(ProcFmt)) == NULL)
		return;
	vpr_aux(vp, prbuf, sizeof(prbuf));
	s_mess(": %f %s => %s", vp->Name, prbuf);
}

void
SetVar()
{
	struct variable	*vp;
	char	prompt[128];

	if ((vp = (struct variable *) findvar(ProcFmt)) == NULL)
		return;
	swritef(prompt, sizeof(prompt), ": %f %s ", vp->Name);

	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
	case V_BASE8:
		*((int *) vp->v_value) = ask_int(prompt,
		    ((vp->v_flags & V_TYPEMASK) == V_BASE10)? 10 : 8);
		break;

	case V_BOOL:
	    {
		char	*def = *((bool *) vp->v_value) ? "off" : "on",
			*on_off;
		bool	value;

		on_off = ask(def, prompt);
		if (casecmp(on_off, "on") == 0)
			value = ON;
		else if (casecmp(on_off, "off") == 0)
			value = OFF;
		else {
			complain("Boolean variables must be ON or OFF.");
			/* NOTREACHED */
		}
		*((bool *) vp->v_value) = value;
#ifdef	MAC
		MarkVar(vp,-1,0);	/* mark the menu item */
#endif
		s_mess("%s%s", prompt, value ? "on" : "off");
		break;
	    }

	case V_FILENAME:
	    {
		char	fbuf[FILESIZE];
		size_t	pl = strlen(prompt);

		swritef(&prompt[pl], sizeof(prompt)-pl, "(default %s) ",
			(char *)vp->v_value);
		(void) ask_file(prompt, (char *) vp->v_value, fbuf);
		strcpy((char *) vp->v_value, fbuf);
		break;
	    }

	case V_STRING:
	    {
		char	*str;

		/* Do_ask() so you can set string to "" if you so desire. */
		str = do_ask("\r\n", (bool (*) ptrproto((int))) NULL,
			(char *) vp->v_value, prompt);
		if (str == NULL)
			str = NullStr;
		strcpy((char *) vp->v_value, str);
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
#ifdef	IBMPC
		setcolor(Fgcolor, Bgcolor);
#endif	/* IBMPC */
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

private bool
aux_complete(c)
int	c;
{
	int	command,
		i;

	if (comp_flags & CASEIND) {
		char	*lp;

		for (lp = linebuf; *lp != '\0'; lp++)
			if (jisupper(*lp))
				*lp = jtolower(*lp);
	}
	switch (c) {
	case EOF:
		comp_value = -1;
		return FALSE;

	case '\r':
	case '\n':
		command = match(Possible, linebuf);
		if (command >= 0) {
			comp_value = command;
			return FALSE;	/* tells ask to stop */
		}
		if (eolp() && bolp()) {
			comp_value = NULLSTRING;
			return FALSE;
		}
		if (comp_flags & RET_STATE) {
			comp_value = command;
			return FALSE;
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
				*cp = '\0';
				Eol();
			}
			break;
		}
		if (c != '\t' && numfound == 1) {
			comp_value = lastmatch;
			return FALSE;
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
			complain((char *)NULL);
		/* kludge: in case we're using UseBuffers, in which case
		   linebuf gets written all over */
		strcpy(Minibuf, linebuf);
		len = strlen(Minibuf);
		TOstart("Completion", TRUE);	/* for now ... */
		for (i = 0; Possible[i]; i++)
			if (numcomp(Possible[i], Minibuf) >= len) {
				Typeout(Possible[i]);
				if (TOabort)
					break;
			}

		TOstop();
		}
		break;
	}
	return TRUE;
}

int
complete(possible, prompt, flags)
register char	*possible[];
const char	*prompt;
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
		save = ORIGINAL,
		exactmatch = -1;

	len = strlen(what);
	if (len == 0)
		return NULLSTRING;
	for (i = 0; choices[i]; i++) {
		if (strncmp(what, choices[i], len) == 0) {
			if (choices[i][len] == '\0')
				exactmatch = i;
			save = i;
			found += 1;	/* found one */
		}
	}

	if (found > 1) {
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
	char	*com,
		buf[FILESIZE];

#ifndef	MSDOS
	swritef(buf, sizeof(buf), "%s/.joverc", HomeDir);
#else	/* MSDOS */
	if (com = getenv("JOVERC"))
		strcpy(buf, com);
	else
		strcpy(buf, Joverc);
#endif	/* MSDOS */
	com = ask_file((char *)NULL, buf, buf);
	if (!joverc(buf))
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

	for (i = nchars = 0; lp != NULL; i++, lp = lp->l_next) {
		if (lp == curline) {
			dotchar = nchars + curchar;
			dotline = i + 1;
		}
		nchars += length(lp) + (lp->l_next != NULL); /* include the NL */
	}

	s_mess("[\"%s\" line %d/%d, char %D/%D (%d%%), cursor = %d/%d]",
	       filename(curbuf), dotline, i, dotchar, nchars,
	       (nchars == 0) ? 100 : (int) (((long) dotchar * 100) / nchars),
	       calc_pos(linebuf, curchar),
	       calc_pos(linebuf, (int)strlen(linebuf)));
}

#ifndef	MAC
private int
do_if(cmd)
char	*cmd;
{
	int status;
	char	*args[12];

	{
		char	*cp = cmd,
			**ap = args;

		for (;;) {
			while (*cp == ' ')
			    cp++;
			*ap++ = cp;
			if ((cp = strchr(cp, ' ')) == NULL)
				break;
			*cp++ = '\0';
		}
		ap[-1] = NULL;
	}

#ifndef	MSDOS
	{
		int	pid;

		switch (pid = fork()) {
		case -1:
			complain("[Fork failed: if: %s]", strerror(errno));
			/*NOTREACHED*/

		case 0:
			close(0);	/*	we want reads to fail */
			/* close(1);	 but not writes or ioctl's
			close(2);    */
			(void) execvp(args[0], (const char **)args);
			_exit(-10);	/* signals exec error (see below) */
			/*NOTREACHED*/
		}
#ifdef	IPROCS
		SigHold(SIGCHLD);
#endif
		dowait(pid, &status);
#ifdef	IPROCS
		SigRelse(SIGCHLD);
#endif
		if (status == -10)
			complain("[Exec failed]");
		if (status < 0)
			complain("[Exit %d]", status);
	}
#else	/* MSDOS */
	if ((status = spawnvp(0, args[0], args)) < 0)
		complain("[Spawn failed: if]");
#endif	/* MSDOS */

	return (status == 0);	/* 0 means successful */
}
#endif	/* MAC */

bool
joverc(file)
char	*file;
{
	char	buf[LBSIZE],
		lbuf[LBSIZE];

	jmp_buf	savejmp;
	volatile int	lnum = 0;
	File	*volatile fp;
	volatile bool	eof;
	volatile unsigned int	/* bitstrings */
			finger = 1,
			skipping = 0,
			inelse = 0;

	fp = open_file(file, buf, F_READ, NO, YES);
	if (fp == NULL)
		return NO;	/* joverc returns an integer */

	/* Catch any errors, here, and do the right thing with them,
	   and then restore the error handle to whoever did a setjmp
	   last. */

	InJoverc += 1;
	push_env(savejmp);
	if (setjmp(mainjmp)) {
		Buffer	*savebuf = curbuf;

		SetBuf(do_select((Window *)NULL, "RC errors"));
		ins_str(sprint("%s:%d:%s", pr_name(file, YES), lnum, lbuf), NO);
		ins_str(sprint("\t%s\n", mesgbuf), NO);
		unmodify();
		SetBuf(savebuf);
		Asking = NO;
	}
	do {
		/* This peculiar delayed EOF testing allows the last line to
		 * end without a NL.  We add NL later, so we leave room for it.
		 */
		eof = f_gets(fp, lbuf, sizeof(lbuf)-1);
		lnum += 1;
		Inputp = lbuf;
		while (*Inputp == ' ' || *Inputp == '\t')
			Inputp += 1;	/* skip white space */
		if (*Inputp == '#') {
			/* a comment */
#ifndef	MAC
		} else if (casencmp(Inputp, "if", (size_t)2) == 0) {
			finger <<= 1;
			if (finger == 0)
				complain("[`if' nested too deeply]");
			if (LookingAt("ifenv\\>[ \t]*\\<\\([^ \t][^ \t]*\\)\\>[ \t]\\(.*\\)$", Inputp, 0)) {
				if (skipping == 0) {
					char	envname[128],
						envpat[128],
						*envval;

					putmatch(1, envname, sizeof envname);
					putmatch(2, envpat, sizeof envpat);
					envval = getenv(envname);
					if (envval==NULL || !LookingAt(envpat, envval, 0))
						skipping |= finger;
				}
			} else if (LookingAt("if\\>[ \t]*\\(.*\\)$", Inputp, 0)) {
				char	cmd[128];

				putmatch(1, cmd, sizeof cmd);
				if (skipping == 0 && !do_if(cmd))
					skipping |= finger;
			} else {
				complain("[`if' syntax error]");
			}
		} else if (casecmp(Inputp, "else") == 0) {
			if (finger == 1 || (inelse & finger))
				complain("[Unexpected `else']");
			inelse |= finger;
			skipping ^= finger;
		} else if (casecmp(Inputp, "endif") == 0) {
			if (finger == 1)
				complain("[Unexpected `endif']");
			inelse &= ~finger;
			skipping &= ~finger;
			finger >>= 1;
#endif
		} else if (skipping == 0) {
			(void) strcat(Inputp, "\n");
			Extend();
			if (Inputp) {
				while (*Inputp == ' ' || *Inputp == '\t')
					Inputp += 1;	/* skip white space */
				if (*Inputp!='\0' && *Inputp!='\n')
					complain("[junk at end of line]");
			}
		}
	} while (!eof);

	f_close(fp);
	pop_env(savejmp);
	Inputp = NULL;
	Asking = NO;
	InJoverc -= 1;
	if (finger != 1)
		complain("[Missing endif]");
	return YES;
}
