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

#ifdef	MAC
# include "mac.h"
#else	/* !MAC */
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
# ifdef	F_COMPLETION
#  include <sys/stat.h>
# endif
#endif	/* !MAC */

int	AbortChar = CTL('G');
bool	DoEVexpand = NO;	/* should we expand evironment variables? */

bool	Asking = NO;
int	AskingWidth;

char	Minibuf[LBSIZE];
private Line	*CurAskPtr = NULL;	/* points at some line in mini-buffer */
private Buffer	*AskBuffer = NULL;	/* Askbuffer points to actual structure */

/* The way the mini-buffer works is this:  The first line of the mini-buffer
   is where the user does his stuff.  The rest of the buffer contains
   strings that the user often wants to use, for instance, file names, or
   common search strings, etc.  If he types C-N or C-P while in ask(), we
   bump the point up or down a line and extract the contents (we make sure
   is somewhere in the mini-buffer). */

private Buffer *
get_minibuf()
{
	if (AskBuffer) {		/* make sure ut still exists */
		register Buffer	*b;

		for (b = world; b != NULL; b = b->b_next)
			if (b == AskBuffer)
				return b;
	}
	AskBuffer = do_select((Window *)NULL, "*minibuf*");
	AskBuffer->b_type = B_SCRATCH;
	return AskBuffer;
}

/* Add a string to the mini-buffer. */

void
minib_add(str, movedown)
char	*str;
bool	movedown;
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
			if (!jisword(c))
				break;
			*vp++ = c;
		}
		*vp = '\0';
		/* if we find an env. variable with the right
		   name, we insert it in linebuf, and then delete
		   the variable name that we're replacing - and
		   then we continue in case there are others ... */
		if ((ep = getenv(varname)) != NULL) {
			curchar = lp_start - linebuf;
			ins_str(ep, NO);
			del_char(FORWARD, (int)strlen(varname) + 1, NO);
			lp = linebuf + curchar;
		}
	}
	ToMark(m);
	DelMark(m);
}

bool	InRealAsk = NO;

private char *
real_ask(delim, d_proc, def, prompt)
char	*delim,
	*def,
	*prompt;
bool	(*d_proc) proto((int));
{
	jmp_buf	savejmp;
	int	c,
		prompt_len;
	Buffer	*saveb = curbuf;
	volatile int	aborted = NO;
	int	no_typed = NO;
	data_obj	*push_cmd = LastCmd;
	int	o_a_v = arg_value(),
		o_i_an_a = is_an_arg();
#ifdef	MAC
		menus_off();
#endif

	if (InRealAsk)
		complain((char *) NULL);
	push_env(savejmp);
	InRealAsk = YES;
	SetBuf(get_minibuf());
	if (!inlist(AskBuffer->b_first, CurAskPtr))
		CurAskPtr = curline;
	prompt_len = strlen(prompt);
	ToFirst();	/* Beginning of buffer. */
	linebuf[0] = '\0';
	modify();
	makedirty(curline);

	if (setjmp(mainjmp)) {
		if (InJoverc) {		/* this is a kludge */
			aborted = YES;
			goto cleanup;
		}
	}

	for (;;) {
		clr_arg_value();
		last_cmd = this_cmd;
		init_strokes();
cont:
		s_mess("%s%s", prompt, linebuf);
		Asking = YES;
		AskingWidth = curchar + prompt_len;
		c = getch();

		if ((c == EOF) || (c != '\0' && strchr(delim, c) != NULL)) {
			if (DoEVexpand)
				EVexpand();
			if (d_proc == (bool(*) proto((int)))NULL || !(*d_proc)(c))
				break;
		} else if (c == AbortChar) {
			message("[Aborted]");
			aborted = YES;
			break;
		} else switch (c) {
		case CTL('N'):
		case CTL('P'):
	        case CTL('U'):          /* Allow ^U as a synonym for ^P */
			if (CurAskPtr != NULL) {
				int	n = (c == CTL('P') ? -arg_value() : arg_value());
				CurAskPtr = next_line(CurAskPtr, n);
				if (CurAskPtr == curbuf->b_first && CurAskPtr->l_next != NULL)
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
			flushscreen();
		}
		if (no_typed)
			return NULL;
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
	ans = real_ask("\r\n", (bool (*) proto((int))) NULL, def, prompt);
	if (ans == NULL) {		/* Typed nothing. */
		if (def == NULL)
			complain("[No default]");
		return def;
	}
	return ans;
}

#ifdef	STDARGS
char *
do_ask(char *delim, bool (*d_proc) proto((int)), char *def, const char *fmt,...)
#else
/*VARARGS4*/ char *
do_ask(delim, d_proc, def, fmt, va_alist)
	char	*delim,
		*def;
	const char	*fmt;
	bool	(*d_proc) proto((int));
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
		Asking = YES;	/* so redisplay works */
		AskingWidth = strlen(prompt);
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

#ifdef	F_COMPLETION

private char	*fc_filebase;
bool	DispBadFs = YES;	/* display bad file names? */
# ifndef	MSDOS
char	BadExtensions[128] = ".o";
# else	/* MSDOS */
char	BadExtensions[128] = ".obj .exe .com .bak .arc .lib .zoo";
# endif	/* MSDOS */

private int
bad_extension(name)
char	*name;
{
	char	*ip,
		*bads;
	size_t	namelen = strlen(name),
		ext_len;

#ifdef	UNIX
	if (strcmp(name, ".")==0 || strcmp(name, "..")==0)
		return YES;
#endif
	for (ip=bads=BadExtensions; *ip!='\0'; bads = ip+1) {
		if ((ip = strchr(bads, ' ')) == NULL)
			ip = bads + strlen(bads);
		ext_len = ip - bads;
		if (ext_len != 0 && ext_len < namelen &&
		    (strncmp(&name[namelen - ext_len], bads, ext_len) == 0))
			return YES;
	}
	return NO;
}

private int
f_match(file)
char	*file;
{
	int	len = strlen(fc_filebase);

	if (!DispBadFs && bad_extension(file))
		return NO;

	return ((len == 0) ||
#ifdef	MSDOS
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
		if (DispBadFs && bad_extension(dir_vec[i]))
			continue;
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

private bool
f_complete(c)
int	c;
{
	char	dir[FILESIZE],
		**dir_vec;
	int	nentries,
		i;

	if (c == CR || c == LF)
		return FALSE;	/* tells ask to return now */
	fc_filebase = strrchr(linebuf, '/');
#ifdef	MSDOS
	if (fc_filebase == NULL) {
		fc_filebase = strrchr(linebuf, '\\');
		if (fc_filebase == NULL)
			fc_filebase = strrchr(linebuf, ':');
	}
#endif	/* MSDOS */
	if (fc_filebase != NULL) {
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
	if ((nentries = jscandir(dir, &dir_vec, f_match, alphacomp)) == -1) {
		add_mess(" [Unknown directory: %s]", dir);
		SitFor(7);
		return TRUE;
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
		Typeout((char *)NULL);
		Typeout("Possible completions (in %s):", dir);
		Typeout((char *)NULL);

		for (i = 0; i < nentries; i++)
			maxlen = max((int)strlen(dir_vec[i]), maxlen);
		maxlen += 4;	/* pad each column with at least 4 spaces */
		ncols = (CO - 2) / maxlen;
		linespercol = 1 + (nentries / ncols);

		for (lines = 0; lines < linespercol; lines++) {
			for (col = 0; col < ncols; col++) {
				bool	isbad;
				int	which;

				which = (col * linespercol) + lines;
				if (which >= nentries)
					break;
				if (DispBadFs)
					isbad = bad_extension(dir_vec[which]);
				else
					isbad = NO;
				Typeout("%s%-*s", isbad ? "!" : NullStr,
					maxlen - isbad, dir_vec[which]);
			}
			Typeout((char *) NULL);
		}
		TOstop();
	}
	freedir(&dir_vec, nentries);
	return TRUE;
}

#endif	/* F_COMPLETION */

char *
ask_file(prmt, def, buf)
const char	*prmt;
char	*def,
	*buf;
{
	char	*ans,
		prompt[128],
		*pretty_name = pr_name(def, YES);

	if (prmt) {
		strcpy(prompt, prmt);
	} else {
		if (def != NULL && *def != '\0') {
			swritef(prompt, sizeof(prompt), ": %f (default %s) ",
				pretty_name);
			if ((int)strlen(prompt) * 2 >= CO)
				swritef(prompt, sizeof(prompt), ProcFmt);
		} else {
			swritef(prompt, sizeof(prompt), ProcFmt);
		}
	}
#ifdef	F_COMPLETION
	ans = real_ask("\r\n \t?", f_complete, pretty_name, prompt);
	if (ans == NULL && (ans = pretty_name) == NULL)
		complain("[No default file name]");
#else
	ans = ask(pretty_name, prompt);
#endif
	PathParse(ans, buf);

	return buf;
}
