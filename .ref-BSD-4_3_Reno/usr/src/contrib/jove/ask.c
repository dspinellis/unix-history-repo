/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "termcap.h"
#include "ctype.h"
#include "chars.h"
#include "disp.h"
#include "fp.h"
#include "scandir.h"

#include <signal.h>

#if defined(MAC)
# include "mac.h"
#else	/* !MAC */
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
# if defined(F_COMPLETION)
#  include <sys/stat.h>
# endif
#endif	/* !MAC */

private Buffer * get_minibuf proto((void));
private char * real_ask proto((char *, int (*) proto((int)), char *, char *));

private int
	f_complete proto((int)),
	bad_extension proto((char *)),
	isdir proto((char *));
private void
	fill_in proto((char **, int)),
	EVexpand proto((void));

int	AbortChar = CTL('G'),
	DoEVexpand = NO;	/* should we expand evironment variables? */

int	Asking = NO;
char	Minibuf[LBSIZE];
private Line	*CurAskPtr = 0;	/* points at some line in mini-buffer */
private Buffer	*AskBuffer = 0;	/* Askbuffer points to actual structure */

/* The way the mini-buffer works is this:  The first line of the mini-buffer
   is where the user does his stuff.  The rest of the buffer contains
   strings that the user often wants to use, for instance, file names, or
   common search strings, etc.  If he types C-N or C-P while in ask(), we
   bump the point up or down a line and extract the contents (we make sure
   is somewhere in the mini-buffer). */

static Buffer *
get_minibuf()
{
	if (AskBuffer) {		/* make sure ut still exists */
		register Buffer	*b;

		for (b = world; b != 0; b = b->b_next)
			if (b == AskBuffer)
				return b;
	}
	AskBuffer = do_select((Window *) 0, "*minibuf*");
	AskBuffer->b_type = B_SCRATCH;
	return AskBuffer;
}

/* Add a string to the mini-buffer. */

void
minib_add(str, movedown)
char	*str;
int	movedown;
{
	register Buffer	*saveb = curbuf;

	SetBuf(get_minibuf());
	LineInsert(1);
	ins_str(str, NO);
	if (movedown)
		CurAskPtr = curline;
	SetBuf(saveb);
}

/* look for any substrings of the form $foo in linebuf, and expand
   them according to their value in the environment (if possible) -
   this munges all over curchar and linebuf without giving it a second
   thought (I must be getting lazy in my old age) */
private void
EVexpand()
{
	register int	c;
	register char	*lp = linebuf,
			*ep;
	char	varname[128],
		*vp,
		*lp_start;
	Mark	*m = MakeMark(curline, curchar, M_FLOATER);

	while ((c = *lp++) != '\0') {
		if (c != '$')
			continue;
		lp_start = lp - 1;	/* the $ */
		vp = varname;
		while ((c = *lp++) != '\0') {
			if (!isword(c))
				break;
			*vp++ = c;
		}
		*vp = '\0';
		/* if we find an env. variable with the right
		   name, we insert it in linebuf, and then delete
		   the variable name that we're replacing - and
		   then we continue in case there are others ... */
		if ((ep = getenv(varname)) != NIL) {
			curchar = lp_start - linebuf;
			ins_str(ep, NO);
			del_char(FORWARD, (int)strlen(varname) + 1, NO);
			lp = linebuf + curchar;
		}
	}
	ToMark(m);
	DelMark(m);
}

int	InRealAsk = 0;

private char *
real_ask(delim, d_proc, def, prompt)
char	*delim,
	*def,
	*prompt;
int	(*d_proc) proto((int));
{
	jmp_buf	savejmp;
	int	c,
		prompt_len;
	Buffer	*saveb = curbuf;
	int	aborted = NO,
		no_typed = NO;
	data_obj	*push_cmd = LastCmd;
	int	o_a_v = arg_value(),
		o_i_an_a = is_an_arg();
#ifdef MAC
		menus_off();
#endif

	if (InRealAsk)
		complain((char *) 0);
	push_env(savejmp);
	InRealAsk += 1;
	SetBuf(get_minibuf());
	if (!inlist(AskBuffer->b_first, CurAskPtr))
		CurAskPtr = curline;
	prompt_len = strlen(prompt);
	ToFirst();	/* Beginning of buffer. */
	linebuf[0] = '\0';
	modify();
	makedirty(curline);

	if (setjmp(mainjmp))
		if (InJoverc) {		/* this is a kludge */
			aborted = YES;
			goto cleanup;
		}

	for (;;) {
		clr_arg_value();
		last_cmd = this_cmd;
		init_strokes();
cont:		s_mess("%s%s", prompt, linebuf);
		Asking = curchar + prompt_len;
		c = getch();
		if ((c == EOF) || strchr(delim, c)) {
			if (DoEVexpand)
				EVexpand();
			if (d_proc == (int(*) proto((int)))0 || (*d_proc)(c) == 0)
				goto cleanup;
		} else if (c == AbortChar) {
			message("[Aborted]");
			aborted = YES;
			goto cleanup;
		} else switch (c) {
		case CTL('N'):
		case CTL('P'):
			if (CurAskPtr != 0) {
				int	n = (c == CTL('P') ? -arg_value() : arg_value());
				CurAskPtr = next_line(CurAskPtr, n);
				if (CurAskPtr == curbuf->b_first && CurAskPtr->l_next != 0)
					CurAskPtr = CurAskPtr->l_next;
				(void) ltobuf(CurAskPtr, linebuf);
				modify();
				makedirty(curline);
				Eol();
				this_cmd = 0;
			}
			break;

		case CTL('R'):
			if (def)
				ins_str(def, NO);
			else
				rbell();
			break;

		default:
			dispatch(c);
			break;
		}
		if (curbuf != AskBuffer)
			SetBuf(AskBuffer);
		if (curline != curbuf->b_first) {
			CurAskPtr = curline;
			curline = curbuf->b_first;	/* with whatever is in linebuf */
		}
		if (this_cmd == ARG_CMD)
			goto cont;
	}
cleanup:
	pop_env(savejmp);

	LastCmd = push_cmd;
	set_arg_value(o_a_v);
	set_is_an_arg(o_i_an_a);
	no_typed = (linebuf[0] == '\0');
	strcpy(Minibuf, linebuf);
	SetBuf(saveb);
	InRealAsk = Asking = Interactive = NO;
	if (!aborted) {
		if (!charp()) {
			Placur(ILI, 0);
			flusho();
		}
		if (no_typed)
			return 0;
	} else
		complain(mesgbuf);
	return Minibuf;
}

#ifdef	STDARGS
	char *
ask(char *def, char *fmt,...)
#else
	/*VARARGS2*/ char *
ask(def, fmt, va_alist)
	char	*def,
		*fmt;
	va_dcl
#endif
{
	char	prompt[128];
	char	*ans;
	va_list	ap;

	va_init(ap, fmt);
	format(prompt, sizeof prompt, fmt, ap);
	va_end(ap);
	ans = real_ask("\r\n", (int (*) proto((int))) 0, def, prompt);
	if (ans == 0) {		/* Typed nothing. */
		if (def == 0)
			complain("[No default]");
		return def;
	}
	return ans;
}

#ifdef	STDARGS
	char *
do_ask(char *delim, int (*d_proc) proto((int)), char *def, char *fmt,...)
#else
	/*VARARGS4*/ char *
do_ask(delim, d_proc, def, fmt, va_alist)
	char	*delim,
		*def,
		*fmt;
	int	(*d_proc) proto((int));
	va_dcl
#endif
{
	char	prompt[128];
	va_list	ap;

	va_init(ap, fmt);
	format(prompt, sizeof prompt, fmt, ap);
	va_end(ap);
	return real_ask(delim, d_proc, def, prompt);
}

#ifdef	STDARGS
	int
yes_or_no_p(char *fmt, ...)
#else
	/*VARARGS1*/ int
yes_or_no_p(fmt, va_alist)
	char	*fmt;
	va_dcl
#endif
{
	char	prompt[128];
	int	c;
	va_list	ap;

	va_init(ap, fmt);
	format(prompt, sizeof prompt, fmt, ap);
	va_end(ap);
	for (;;) {
		message(prompt);
		Asking = strlen(prompt);	/* so redisplay works */
		c = getch();
		Asking = NO;
		if (c == AbortChar)
			complain("[Aborted]");
		switch (CharUpcase(c)) {
		case 'Y':
			return YES;

		case 'N':
			return NO;

		default:
			add_mess("[Type Y or N]");
			SitFor(10);
		}
	}
	/* NOTREACHED */
}

#if defined(F_COMPLETION)
static char	*fc_filebase;
int	DispBadFs = YES;	/* display bad file names? */
# if !defined(MSDOS)
char	BadExtensions[128] = ".o";
# else /* MSDOS */
char	BadExtensions[128] = ".obj .exe .com .bak .arc .lib .zoo";
# endif /* MSDOS */

private int
bad_extension(name)
char	*name;
{
	char	*ip,
		*bads = BadExtensions;
	size_t	namelen = strlen(name),
		ext_len;
	int	stop = NO;

	do {
		if ((ip = strchr(bads, ' ')) == 0) {
			ip = bads + strlen(bads);
			stop = YES;
		}
		if ((ext_len = ip - bads) == 0)
			continue;
		if ((ext_len < namelen) &&
		    (strncmp(&name[namelen - ext_len], bads, ext_len) == 0))
			return YES;
	} while ((bads = ip + 1), !stop);
	return NO;
}

private int
f_match(file)
char	*file;
{
	int	len = strlen(fc_filebase);

	if (DispBadFs == NO)
		if (bad_extension(file))
			return NO;

	return ((len == 0) ||
#if defined(MSDOS)
		(casencmp(file, fc_filebase, strlen(fc_filebase)) == 0)
#else
		(strncmp(file, fc_filebase, strlen(fc_filebase)) == 0)
#endif
		);
}

private int
isdir(name)
char	*name;
{
	struct stat	stbuf;
	char	filebuf[FILESIZE];

	PathParse(name, filebuf);
	return ((stat(filebuf, &stbuf) != -1) &&
		(stbuf.st_mode & S_IFDIR) == S_IFDIR);
}

private void
fill_in(dir_vec, n)
register char	**dir_vec;
int	n;
{
	int	minmatch = 0,
		numfound = 0,
		lastmatch = -1,
		i,
		the_same = TRUE, /* After filling in, are we the same
				    as when we were called? */
		is_ntdir;	/* Is Newly Typed Directory name */

	for (i = 0; i < n; i++) {
		/* if it's no, then we have already filtered them out
		   in f_match() so there's no point in doing it again */
		if (DispBadFs == YES) {
			if (bad_extension(dir_vec[i]))
				continue;
		}
		if (numfound)
			minmatch = min(minmatch,
				       numcomp(dir_vec[lastmatch], dir_vec[i]));
		else
			minmatch = strlen(dir_vec[i]);
		lastmatch = i;
		numfound += 1;
	}
	/* Ugh.  Beware--this is hard to get right in a reasonable
	   manner.  Please excuse this code--it's past my bedtime. */
	if (numfound == 0) {
		rbell();
		return;
	}
	Eol();
	if (minmatch > (int)strlen(fc_filebase)) {
		the_same = FALSE;
		null_ncpy(fc_filebase, dir_vec[lastmatch], (size_t) minmatch);
		Eol();
		makedirty(curline);
	}
	is_ntdir = ((numfound == 1) &&
		    (curchar > 0) &&
		    (linebuf[curchar - 1] != '/') &&
		    (isdir(linebuf)));
	if (the_same && !is_ntdir) {
		add_mess((n == 1) ? " [Unique]" : " [Ambiguous]");
		SitFor(7);
	}
	if (is_ntdir)
		insert_c('/', 1);
}

/* called from do_ask() when one of "\r\n ?" is typed.  Does the right
   thing, depending on which. */

private int
f_complete(c)
int	c;
{
	char	dir[FILESIZE],
		**dir_vec;
	int	nentries,
		i;

	if (c == CR || c == LF)
		return 0;	/* tells ask to return now */
#if !defined(MSDOS)		/* kg */
	if ((fc_filebase = strrchr(linebuf, '/')) != 0) {
#else /* MSDOS */
	fc_filebase = strrchr(linebuf, '/');
	if (fc_filebase == (char *)0)
		fc_filebase = strrchr(linebuf, '\\');
	if (fc_filebase == (char *)0)
		fc_filebase = strrchr(linebuf, ':');
	if (fc_filebase != (char *)0) {
#endif /* MSDOS */
		char	tmp[FILESIZE];

		fc_filebase += 1;
		null_ncpy(tmp, linebuf, (size_t) (fc_filebase - linebuf));
		if (tmp[0] == '\0')
			strcpy(tmp, "/");
		PathParse(tmp, dir);
	} else {
		fc_filebase = linebuf;
		strcpy(dir, ".");
	}
	if ((nentries = scandir(dir, &dir_vec, f_match, alphacomp)) == -1) {
		add_mess(" [Unknown directory: %s]", dir);
		SitFor(7);
		return 1;
	}
	if (nentries == 0) {
		add_mess(" [No match]");
		SitFor(7);
	} else if (c == ' ' || c == '\t')
		fill_in(dir_vec, nentries);
	else {
		/* we're a '?' */
		int	maxlen = 0,
			ncols,
			col,
			lines,
			linespercol;

		TOstart("Completion", FALSE);	/* false means newline only on request */
		Typeout("(! means file will not be chosen unless typed explicitly)");
		Typeout((char *) 0);
		Typeout("Possible completions (in %s):", dir);
		Typeout((char *) 0);

		for (i = 0; i < nentries; i++)
			maxlen = max((int)strlen(dir_vec[i]), maxlen);
		maxlen += 4;	/* pad each column with at least 4 spaces */
		ncols = (CO - 2) / maxlen;
		linespercol = 1 + (nentries / ncols);

		for (lines = 0; lines < linespercol; lines++) {
			for (col = 0; col < ncols; col++) {
				int	isbad,
					which;

				which = (col * linespercol) + lines;
				if (which >= nentries)
					break;
				if (DispBadFs == YES)
					isbad = bad_extension(dir_vec[which]);
				else
					isbad = NO;
				Typeout("%s%-*s", isbad ? "!" : NullStr,
					maxlen - isbad, dir_vec[which]);
			}
			Typeout((char *) 0);
		}
		TOstop();
	}
	freedir(&dir_vec, nentries);
	return 1;
}

#endif

char *
ask_file(prmt, def, buf)
char	*prmt,
	*def,
	*buf;
{
	char	*ans,
		prompt[128],
		*pretty_name = pr_name(def, YES);

	if (prmt)
		strcpy(prompt, prmt);
	else
		swritef(prompt, ProcFmt);
#if defined(F_COMPLETION)
	ans = real_ask("\r\n \t?", f_complete, pretty_name, prompt);
	if (ans == 0)
		complain((char *)0);
#else
	ans = ask(pretty_name, prompt);
#endif
	PathParse(ans, buf);

	return buf;
}
