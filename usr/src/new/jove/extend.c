/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"
#include "ctype.h"
#ifdef JOB_CONTROL
#	include <signal.h>
#endif

#ifdef MAC
#	include "mac.h"
#else
#	include <varargs.h>
#endif

#ifdef MSDOS
#include <process.h>
#endif

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private	void
	fb_aux(data_obj *, data_obj **, char *, char *),
	find_binds(data_obj *, char *),
	vpr_aux(struct variable *, char *);
#else
private	void
	fb_aux(),
	find_binds(),
	vpr_aux();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif


int	InJoverc = 0;

extern int	getch(),
		getchar();

/* Auto execute code */

#define NEXECS	20

private struct {
	char	*a_pattern;
	data_obj	*a_cmd;
} AutoExecs[NEXECS] = {0};

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

/* VARARGS0 */

void
DefAutoExec(proc)
#ifdef LINT_ARGS
data_obj	*(*proc)(char *);
#else
data_obj	*(*proc)();
#endif
{
	data_obj	*d;
	char	*pattern;
	int	i;

	if (ExecIndex >= NEXECS)
		complain("Too many auto-executes, max %d.", NEXECS);
	if ((d = (*proc)(ProcFmt)) == 0)
		return;
	pattern = do_ask("\r\n", (int (*)()) 0, (char *) 0, ": %f %s ", d->Name);
	if (pattern != 0)
	    for (i = 0; i < ExecIndex; i++)
		if ((AutoExecs[i].a_cmd == d) &&
		    (strcmp(pattern, AutoExecs[i].a_pattern) == 0))
		    	return;		/* eliminate duplicates */
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

void
BindAKey()
{
	BindSomething(findcom);
}

void
BindMac()
{
	BindSomething(findmac);
}

extern void	EscPrefix(),
		CtlxPrefix(),
		MiscPrefix();

data_obj **
IsPrefix(cp)
data_obj	*cp;
{
#ifdef MAC
	void (*proc)();
#else
	int	(*proc)();
#endif
	
	if (cp == 0 || (cp->Type & TYPEMASK) != FUNCTION)
		return 0;
	proc = ((struct cmd *) cp)->c_proc;
	if (proc == EscPrefix)
		return pref1map;
	if (proc == CtlxPrefix)
		return pref2map;
	if (proc == MiscPrefix)
		return miscmap;
	return 0;
}

void
UnbindC()
{
	char	*keys;
	data_obj	**map = mainmap;

	keys = ask((char *) 0, ProcFmt);
	for (;;) {
		if (keys[1] == '\0')
			break;
		if ((map = IsPrefix(map[*keys])) == 0)
			break;
		keys += 1;
	}
	if (keys[1] != 0)
		complain("That's not a legitimate key sequence.");
	map[keys[0]] = 0;
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
			else if (isalpha(c) || index("@[\\]^_", c))
				c = CTL(c);
			else
				complain("[Unknown control character]");
		}
	}
	return c;
}

void
BindWMap(map, lastkey, cmd)
data_obj	**map,
		*cmd;
{
	data_obj	**nextmap;
	int	c;

	c = addgetc();
	if (c == EOF) {
		if (lastkey == EOF)
			complain("[Empty key sequence]");
		complain("[Premature end of key sequence]");
	} else {
		if (nextmap = IsPrefix(map[c]))
			BindWMap(nextmap, c, cmd);
		else {
			map[c] = cmd;
#ifdef MAC
			((struct cmd *) cmd)->c_key = c;	/* see about_j() in mac.c */
			if(map == mainmap) ((struct cmd *) cmd)->c_map = F_MAINMAP;
			else if(map == pref1map) ((struct cmd *) cmd)->c_map = F_PREF1MAP;
			else if(map == pref2map) ((struct cmd *) cmd)->c_map = F_PREF2MAP;
#endif
		}
	}
}

/* VARARGS0 */

void
BindSomething(proc)
#ifdef LINT_ARGS
data_obj	*(*proc)(char *);
#else
data_obj	*(*proc)();
#endif
{
	data_obj	*d;

	if ((d = (*proc)(ProcFmt)) == 0)
		return;
	s_mess(": %f %s ", d->Name);
	BindWMap(mainmap, EOF, d);
}

/* Describe key */

void
DescWMap(map, key)
data_obj	**map;
{
	data_obj	*cp = map[key],
			**prefp;

	if (cp == 0)
		add_mess("is unbound.");
	else if (prefp = IsPrefix(cp))
		DescWMap(prefp, addgetc());
	else
		add_mess("is bound to %s.", cp->Name);
}

void
KeyDesc()
{
	s_mess(ProcFmt);
	DescWMap(mainmap, addgetc());
}

void
DescCom()
{
	data_obj	*dp;
	char	pattern[100],
		doc_type[40],
		*the_type,
		*file = CmdDb;
	File	*fp;
	int	is_var;

	if (!strcmp(LastCmd->Name, "describe-variable")) {
		dp = (data_obj *) findvar(ProcFmt);
		the_type = "Variable";
		is_var = YES;
	} else {
		dp = (data_obj *) findcom(ProcFmt);
		the_type = "Command";
		is_var = NO;
	}
	if (dp == 0)
		return;
	fp = open_file(file, iobuff, F_READ, COMPLAIN, QUIET);
	Placur(ILI, 0);
	flusho();
	sprintf(pattern, "^:entry \"%s\" \"\\([^\"]*\\)\"", dp->Name);
	TOstart("Help", TRUE);
	for (;;) {
		if (f_gets(fp, genbuf, LBSIZE) == EOF) {
			Typeout("There is no documentation for \"%s\".", dp->Name);
			goto outahere;
		}
		if ((strncmp(genbuf, ":entry", 6) == 0) &&
		    (LookingAt(pattern, genbuf, 0))) {
			char	type[64];

			putmatch(1, type, sizeof type);
			if (strcmp(type, the_type) == 0)
				break;
		}
	}
	/* found it ... let's print it */
	putmatch(1, doc_type, sizeof doc_type);
	if (is_var == YES)
		Typeout(dp->Name);
	else {
		char	binding[128];

		find_binds(dp, binding);
		if (blnkp(binding))
			Typeout("To invoke %s, type \"ESC X %s<cr>\".",
				dp->Name,
				dp->Name);
		else
			Typeout("Type \"%s\" to invoke %s.", binding, dp->Name);
	}
	Typeout("");
	while (f_gets(fp, genbuf, LBSIZE) != EOF)
		if (strncmp(genbuf, ":entry", 6) == 0)
			goto outahere;
		else
			Typeout("%s", genbuf);
outahere:
	f_close(fp);
	TOstop();
}

void
DescBindings()
{
	extern void	Typeout();

	TOstart("Key Bindings", TRUE);
	DescMap(mainmap, NullStr);
	TOstop();
}

extern int specialmap;

void
DescMap(map, pref)
data_obj	**map;
char	*pref;
{
	int	c1,
		c2 = 0,
		numbetween;
	char	keydescbuf[40];
	data_obj	**prefp;

#ifdef IBMPC
	specialmap = (map == miscmap);
#endif

	for (c1 = 0; c1 < NCHARS && c2 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (map[c1] == 0)
			continue;
		while (++c2 < NCHARS && map[c1] == map[c2])
			;
		c2 -= 1;
		numbetween = c2 - c1;
		if (numbetween == 1)
			sprintf(keydescbuf, "%s {%p,%p}", pref, c1, c2);
		else if (numbetween == 0)
			sprintf(keydescbuf, "%s %p", pref, c1);
		else
			sprintf(keydescbuf, "%s [%p-%p]", pref, c1, c2);
		if ((prefp = IsPrefix(map[c1])) && (prefp != map))
			DescMap(prefp, keydescbuf);
		else
			Typeout("%-18s%s", keydescbuf, map[c1]->Name);
	}
}

private void
find_binds(dp, buf)
data_obj	*dp;
char	*buf;
{
	char	*endp;

	buf[0] = '\0';
	fb_aux(dp, mainmap, (char *) 0, buf);
	endp = buf + strlen(buf) - 2;
	if ((endp > buf) && (strcmp(endp, ", ") == 0))
		*endp = '\0';
}

private void
fb_aux(cp, map, prefix, buf)
register data_obj	*cp,
			**map;
char	*buf,
	*prefix;
{
	int	c1,
		c2;
	char	*bufp = buf + strlen(buf),
		prefbuf[20];
	data_obj	**prefp;

#ifdef IBMPC
	specialmap = (map == miscmap);
#endif	

	for (c1 = c2 = 0; c1 < NCHARS && c2 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (map[c1] == cp) {
			while (++c2 < NCHARS && map[c1] == map[c2])
				;
			c2 -= 1;
			if (prefix)
				sprintf(bufp, "%s ", prefix);
			bufp += strlen(bufp);
			switch (c2 - c1) {
			case 0:
				sprintf(bufp, "%p, ", c1);
				break;
	
			case 1:
				sprintf(bufp, "{%p,%p}, ", c1, c2);
				break;
	
			default:
				sprintf(bufp, "[%p-%p], ", c1, c2);
				break;
			}
		}
		if ((prefp = IsPrefix(map[c1])) && (prefp != map))  {
			sprintf(prefbuf, "%p", c1);
			fb_aux(cp, prefp, prefbuf, bufp);
		}
		bufp += strlen(bufp);
	}
}

void
Apropos()
{
	register struct cmd	*cp;
	register struct macro	*m;
	register struct variable	*v;
	char	*ans;
	int	anyfs = NO,
		anyvs = NO,
		anyms = NO;
	char	buf[256];

	ans = ask((char *) 0, ": %f (keyword) ");
	TOstart("Help", TRUE);
	for (cp = commands; cp->Name != 0; cp++)
		if (sindex(ans, cp->Name)) {
			if (anyfs == 0) {
				Typeout("Commands");
				Typeout("--------");
			}
			find_binds((data_obj *) cp, buf);
			if (buf[0])
				Typeout(": %-35s(%s)", cp->Name, buf);
			else
				Typeout(": %s", cp->Name);
			anyfs = YES;
		}
	if (anyfs)
		Typeout(NullStr);
	for (v = variables; v->Name != 0; v++)
		if (sindex(ans, v->Name)) {
			if (anyvs == 0) {
				Typeout("Variables");
				Typeout("---------");
			}
			anyvs = YES;
			vpr_aux(v, buf);
			Typeout(": set %-26s%s", v->Name, buf);
		}
	if (anyvs)
		Typeout(NullStr);
	for (m = macros; m != 0; m = m->m_nextm)
		if (sindex(ans, m->Name)) {
			if (anyms == 0) {
				Typeout("Macros");
				Typeout("------");
			}
			anyms = YES;
			find_binds((data_obj *) m, buf);
			if (buf[0])
				Typeout(": %-35s(%s)", m->Name, buf);
			else
				Typeout(": %-35s%s", "execute-macro", m->Name);
		}
	TOstop();
}

void
Extend()
{
	data_obj	*d;

	if (d = findcom(": "))
		ExecCmd(d);
}

/* Read a positive integer from CP.  It must be in base BASE, and
   complains if it isn't.  If allints is nonzero, all the characters
   in the string must be integers or we return -1; otherwise we stop
   reading at the first nondigit. */

int
chr_to_int(cp, base, allints, result)
register char	*cp;
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
	while (c = *cp++) {
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

private void
vpr_aux(vp, buf)
register struct variable	*vp;
char	*buf;
{
	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
		sprintf(buf, "%d", *(vp->v_value));
		break;

	case V_BASE8:
		sprintf(buf, "%o", *(vp->v_value));
		break;

	case V_BOOL:
		sprintf(buf, (*(vp->v_value)) ? "on" : "off");
		break;

	case V_STRING:
	case V_FILENAME:
		sprintf(buf, "%s", (char *) vp->v_value);
		break;

	case V_CHAR:
		sprintf(buf, "%p", *(vp->v_value));
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
		*(vp->v_value) = value;
	    	break;
	    }

	case V_BOOL:
	    {
	    	char	*def = *(vp->v_value) ? "off" : "on",
	    		*on_off;
	    	int	value;

	    	on_off = ask(def, prompt);
		if (casecmp(on_off, "on") == 0)
			value = ON;
	    	else if (casecmp(on_off, "off") == 0)
	    		value = OFF;
	    	else
	    		complain("Boolean variables must be ON or OFF.");
	    	*(vp->v_value) = value;
#ifdef MAC
		MarkVar(vp,-1,0);	/* mark the menu item */
#endif
	    	s_mess("%s%s", prompt, value ? "on" : "off");
	    	break;
	    }

	case V_FILENAME:
	    {
		char	fbuf[FILESIZE];

	    	sprintf(&prompt[strlen(prompt)], "(default %s) ", vp->v_value);
	    	(void) ask_file(prompt, (char *) vp->v_value, fbuf);
		strcpy((char *) vp->v_value, fbuf);
	    	break;
	    }

	case V_STRING:
	    {
		char	*str;

	    	/* Do_ask() so you can set string to "" if you so desire. */
	    	str = do_ask("\r\n", (int (*)()) 0, (char *) vp->v_value, prompt);
	    	if (str == 0)
			str = NullStr;
	    	strcpy((char *) vp->v_value, str);
		/* ... and hope there is enough room. */
	    	break;
	    }
	case V_CHAR:
		f_mess(prompt);
	    	*(vp->v_value) = addgetc();
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
{
	int	command,
		length,
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
			length = strlen(linebuf);

		for (i = 0; Possible[i] != 0; i++) {
			int	this_len;

			this_len = numcomp(Possible[i], linebuf);
			maxmatch = max(maxmatch, this_len);
			if (this_len >= length) {
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
			if (maxmatch < length) {
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
		null_ncpy(linebuf, Possible[lastmatch], minmatch);
	    	Eol();
		if (minmatch == length)	/* No difference */
			rbell();
		break;
	    }

	case '?':
		if (InJoverc)
			complain((char *) 0);
		/* kludge: in case we're using UseBuffers, in which case
		   linebuf gets written all over */
		strcpy(Minibuf, linebuf);
		length = strlen(Minibuf);
		TOstart("Completion", TRUE);	/* for now ... */
		for (i = 0; Possible[i]; i++)
			if (numcomp(Possible[i], Minibuf) >= length) {
				Typeout(Possible[i]);
				if (TOabort != 0)
					break;
			}

		TOstop();
		break;
	}
	return !FALSE;
}

int
complete(possible, prompt, flags)
register char	*possible[];
char	*prompt;
{
	Possible = possible;
	comp_flags = flags;
	(void) do_ask("\r\n \t?", aux_complete, NullStr, prompt);
	return comp_value;
}

int
match(choices, what)
register char	**choices,
		*what;
{
	register int	len;
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
	char	*com, *getenv(),
		buf[FILESIZE];

#ifndef MSDOS
	sprintf(buf, "%s/.joverc", getenv("HOME"));
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
	       calc_pos(linebuf, strlen(linebuf)));
}

#define IF_UNBOUND	-1
#define IF_TRUE		1
#define IF_FALSE	!IF_TRUE

#ifndef MAC
int
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

	case 0:
	    {
#endif /* MSDOS */
		char	*args[12],
			*cp = cmd,
			**ap = args;

	    	*ap++ = cmd;
	    	for (;;) {
			if ((cp = index(cp, ' ')) == 0)
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
	    	(void) execvp(args[0], args);
		_exit(-10);	/* signals exec error (see below) */
	    }
	}
#ifdef IPROCS
	sighold(SIGCHLD);
#endif
	dowait(pid, &status);
#ifdef IPROCS
	sigrelse(SIGCHLD);
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

	fp = open_file(file, buf, F_READ, !COMPLAIN, QUIET);
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
		if (casencmp(lbuf, "if", 2) == 0) {
			char	cmd[128];

			if (IfStatus != IF_UNBOUND)
				complain("[Cannot have nested if's]");
			if (LookingAt("if[ \t]*\\(.*\\)$", lbuf, 0) == 0)
				complain("[If syntax error]");
			putmatch(1, cmd, sizeof cmd);
			IfStatus = do_if(cmd) ? IF_TRUE : IF_FALSE;
			continue;
		} else if (casencmp(lbuf, "else", 4) == 0) {
			if (IfStatus == IF_UNBOUND)
				complain("[Unexpected `else']");
			IfStatus = !IfStatus;
			continue;
		} else if (casencmp(lbuf, "endif", 5) == 0) {
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
