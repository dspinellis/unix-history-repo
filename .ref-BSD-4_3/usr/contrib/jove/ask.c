/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "termcap.h"
#include "ctype.h"
#include <signal.h>
#include <varargs.h>

#ifdef F_COMPLETION
#	include <sys/stat.h>
#endif

int	Asking = 0;
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
	if (AskBuffer)
		return AskBuffer;
	AskBuffer = do_select((Window *) 0, "Minibuf");
	AskBuffer->b_type = B_SCRATCH;
	return AskBuffer;
}

/* Add a string to the mini-buffer. */

minib_add(str, movedown)
char	*str;
{
	register Buffer	*saveb = curbuf;

	SetBuf(get_minibuf());
	LineInsert();
	ins_str(str, NO);
	if (movedown)
		CurAskPtr = curline;
	SetBuf(saveb);
}

static char *
real_ask(delim, d_proc, def, prompt)
char	*delim,
	*def,
	*prompt;
int	(*d_proc)();
{
	static int	InAsk = 0;
	jmp_buf	savejmp;
	int	c,
		prompt_len;
	Buffer	*saveb = curbuf;
	int	abort = 0,
		no_typed = 0;
	data_obj	*push_cmd = LastCmd;
	int	o_exp = exp,
		o_exp_p = exp_p;

	if (InAsk)
		complain((char *) 0);
	push_env(savejmp);
	InAsk++;
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
			abort++;
			goto cleanup;
		}

	for (;;) {
		exp = 1;
		exp_p = 0;
		last_cmd = this_cmd;
		init_strokes();
cont:		s_mess("%s%s", prompt, linebuf);
		Asking = curchar + prompt_len;
		c = getch();
		if ((c == EOF) || index(delim, c)) {
			if (d_proc == 0 || (*d_proc)(c) == 0)
				goto cleanup;
		} else switch (c) {
		case CTL(G):
			message("[Aborted]");
			abort++;
			goto cleanup;

		case CTL(N):
		case CTL(P):
			if (CurAskPtr != 0) {
				int	n = (c == CTL(P) ? -exp : exp);

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

		case CTL(R):
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
	exp_p = o_exp_p;
	exp = o_exp;
	no_typed = (linebuf[0] == '\0');
	strcpy(Minibuf, linebuf);
	SetBuf(saveb);
	InAsk = Asking = Interactive = 0;
	if (!abort) {
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

/* VARARGS2 */

char *
ask(def, fmt, va_alist)
char	*def,
	*fmt;
va_dcl
{
	char	prompt[128];
	char	*ans;
	va_list	ap;

	va_start(ap);
	format(prompt, sizeof prompt, fmt, ap);
	va_end(ap);
	ans = real_ask("\r\n", (int (*)()) 0, def, prompt);
	if (ans == 0) {		/* Typed nothing. */
		if (def == 0)
			complain("[No default]");
		return def;
	}
	return ans;
}

/* VARARGS2 */

char *
do_ask(delim, d_proc, def, fmt, va_alist)
char	*delim,
	*def,
	*fmt;
int	(*d_proc)();
va_dcl
{
	char	prompt[128];
	va_list	ap;

	va_start(ap);
	format(prompt, sizeof prompt, fmt, ap);
	va_end(ap);
	return real_ask(delim, d_proc, def, prompt);
}

#ifdef F_COMPLETION
static char	*fc_filebase;
char	BadExtensions[128] = ".o";

static
bad_extension(name, bads)
char	*name,
	*bads;
{
	char	*ip;
	int	namelen = strlen(name),
		ext_len,
		stop = 0;

	do {
		if (ip = index(bads, ' '))
			*ip = 0;
		else {
			ip = bads + strlen(bads);
			stop++;
		}
		if ((ext_len = ip - bads) == 0)
			continue;
		if ((ext_len < namelen) &&
		    (strcmp(&name[namelen - ext_len], bads) == 0))
			return YES;
	} while ((bads = ip + 1), !stop);
	return NO;
}

f_match(file)
char	*file;
{
	int	len = strlen(fc_filebase);

	return ((len == 0) ||
		(strncmp(file, fc_filebase, strlen(fc_filebase)) == 0));
}

static
isdir(name)
char	*name;
{
	struct stat	stbuf;
	char	filebuf[FILESIZE];

	PathParse(name, filebuf);
	return ((stat(filebuf, &stbuf) != -1) &&
		(stbuf.st_mode & S_IFDIR) == S_IFDIR);
}

static
fill_in(dir_vec, n)
register char	**dir_vec;
{
	int	minmatch = 0,
    		numfound = 0,
    		lastmatch = -1,
		i,
		the_same = TRUE, /* After filling in, are we the same
				    as when we were called? */
		is_ntdir;	/* Is Newly Typed Directory name */
	char	bads[128];

	for (i = 0; i < n; i++) {
		strcpy(bads, BadExtensions);
		/* bad_extension() is destructive */
		if (bad_extension(dir_vec[i], bads))
			continue;
		if (numfound)
			minmatch = min(minmatch,
				       numcomp(dir_vec[lastmatch], dir_vec[i]));
		else
			minmatch = strlen(dir_vec[i]);
		lastmatch = i;
		numfound++;
	}
	/* Ugh.  Beware--this is hard to get right in a reasonable
	   manner.  Please excuse this code--it's past my bedtime. */
	if (numfound == 0) {
		rbell();
		return;
	}
	Eol();
	if (minmatch > strlen(fc_filebase)) {
		the_same = FALSE;
		null_ncpy(fc_filebase, dir_vec[lastmatch], minmatch);
		Eol();
		makedirty(curline);
	}
	is_ntdir = ((numfound == 1) &&
		    (curchar > 0) &&
		    (linebuf[curchar - 1] != '/') &&
		    (isdir(linebuf)));
	if (the_same && !is_ntdir) {
		add_mess(n == 1 ? " [Unique]" : " [Ambiguous]");
		(void) SitFor(7);
	}
	if (is_ntdir)
		Insert('/');
}

extern int	alphacomp();

/* called from do_ask() when one of "\r\n ?" is typed.  Does the right
   thing, depending on which. */

static
f_complete(c)
{
	char	dir[FILESIZE],
		**dir_vec;
	int	nentries,
		i;

	if (c == CR || c == LF)
		return 0;	/* tells ask to return now */
	if ((fc_filebase = rindex(linebuf, '/')) != 0) {
		char	tmp[FILESIZE];

		null_ncpy(tmp, linebuf, (++fc_filebase - linebuf));
		if (tmp[0] == '\0')
			strcpy(tmp, "/");
		PathParse(tmp, dir);
	} else {		
		fc_filebase = linebuf;
		strcpy(dir, ".");
	}
	if ((nentries = scandir(dir, &dir_vec, f_match, alphacomp)) == -1) {
		add_mess(" [Unknown directory: %s]", dir);
		(void) SitFor(7);
		return 1;
	}
	if (nentries == 0) {
		add_mess(" [No match]");
		(void) SitFor(7);
	} else if (c == ' ')
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
			maxlen = max(strlen(dir_vec[i]), maxlen);
		maxlen += 4;	/* pad each column with at least 4 spaces */
		ncols = (CO - 2) / maxlen;
		linespercol = 1 + (nentries / ncols);

		for (lines = 0; lines < linespercol; lines++) {
			for (col = 0; col < ncols; col++) {
				int	isbad,
					which;
				char	bads[128];

				which = (col * linespercol) + lines;
				if (which >= nentries)
					break;
				strcpy(bads, BadExtensions);
				isbad = bad_extension(dir_vec[which], bads);
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
ask_file(def, buf)
char	*def,
	*buf;
{
	char	*ans,
		prompt[128],
		*pretty_name = pr_name(def);

	if (def != 0 && *def != '\0')
		sprintf(prompt, ": %f (default %s) ", pretty_name);
	else
		sprintf(prompt, ProcFmt);
#ifdef F_COMPLETION
  	ans = real_ask("\r\n ?", f_complete, pretty_name, prompt);
	if (ans == 0 && (ans = pretty_name) == 0)
		complain("[No default file name]");
#else
	ans = ask(pretty_name, prompt);
#endif
	PathParse(ans, buf);

	return buf;
}
