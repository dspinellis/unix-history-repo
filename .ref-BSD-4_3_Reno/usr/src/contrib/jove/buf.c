/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Contains commands that deal with creating, selecting, killing and
   listing buffers, and buffer modes, and find-file, etc. */

#include "jove.h"
#include "ctype.h"
#include "disp.h"
#if defined(IPROCS)
# include "fp.h"
# include "iproc.h"
#endif

#ifdef MAC
# include "mac.h"
#else
# include <sys/stat.h>
#endif

private Buffer
	*buf_alloc proto((void)),
	*mak_buf proto((void));

private char *line_cnt proto((Buffer *, char *));

private void
	defb_wind proto((Buffer *)),
	kill_buf proto((Buffer *)),
	mkbuflist proto((char **));

private char	*Mainbuf = "Main",
	*NoName = "Sans un nom!";

Buffer	*world = 0,		/* First in the list */
	*curbuf = 0,	/* pointer into world for current buffer */
	*lastbuf = 0;	/* Last buffer we were in so we have a default
			   buffer during a select buffer. */

/* Toggle BIT in the current buffer's minor mode flags.  If argument is
   supplied, a positive one always turns on the mode and zero argument
   always turns it off. */

void
TogMinor(bit)
int	bit;
{
	if (is_an_arg()) {
		if (arg_value() == 0)
			curbuf->b_minor &= ~bit;
		else
			curbuf->b_minor |= bit;
	} else
		curbuf->b_minor ^= bit;
	UpdModLine = YES;
}

/* Creates a new buffer, links it at the end of the buffer chain, and
   returns it. */

private Buffer *
buf_alloc()
{
	register Buffer	*b,
			*lastbp;

	lastbp = 0;
	for (b = world; b != 0; lastbp = b, b = b->b_next)
		;

	b = (Buffer *) emalloc(sizeof (Buffer));
	if (lastbp)
		lastbp->b_next = b;
	else
		world = b;
	b->b_first = 0;
	b->b_next = 0;
#if defined(MAC)
	b->Type = BUFFER;	/* kludge, but simplifies menu handlers */
	b->Name = 0;
#endif
	return b;
}

/* Makes a buffer and initializes it.  Obsolete.  Used to take two
   arguments, a buffer name and a file name. */

private Buffer *
mak_buf()
{
	register Buffer	*newb;
	register int	i;

	newb = buf_alloc();
	newb->b_fname = 0;
	newb->b_name = NoName;
	set_ino(newb);
	newb->b_marks = 0;
	newb->b_themark = 0;		/* Index into markring */
	/* No marks yet */
	for (i = 0; i < NMARKS; i++)
		newb->b_markring[i] = 0;
	newb->b_modified = 0;
	newb->b_type = B_FILE;  /* File until proven SCRATCH */
	newb->b_ntbf = 0;
	newb->b_minor = 0;
	newb->b_major = TEXT;
	newb->b_first = 0;
	newb->b_map = 0;
#if defined(IPROCS)
	newb->b_process = 0;
#endif
	initlist(newb);
#if defined(MAC)
	Bufchange = 1;
#endif
	return newb;
}

void
ReNamBuf()
{
	register char	*new = 0,
			*prompt = ProcFmt,
			*second = "%s already exists; new name? ";

	for (;;) {
		new = ask((char *) 0, prompt, new);
		if (!buf_exists(new))
			break;
		prompt = second;
	}
	setbname(curbuf, new);
}

void
FindFile()
{
	register char	*name;
	char	fnamebuf[FILESIZE];

	name = ask_file((char *) 0, curbuf->b_fname, fnamebuf);
	SetABuf(curbuf);
	SetBuf(do_find(curwind, name, 0));
}

private void
mkbuflist(bnamp)
register char	**bnamp;
{
	register Buffer	*b;

	for (b = world; b != 0; b = b->b_next)
		if (b->b_name != 0)
			*bnamp++ = b->b_name;
	*bnamp = 0;
}

char *
ask_buf(def)
Buffer	*def;
{
	char	*bnames[100];
	register char	*bname;
	register int	offset;
	char	prompt[100];

	if (def != 0 && def->b_name != 0)
		swritef(prompt, ": %f (default %s) ", def->b_name);
	else
		swritef(prompt, ProcFmt);
	mkbuflist(bnames);
	offset = complete(bnames, prompt, RET_STATE);
	if (offset == EOF)
		complain((char *) 0);
	if (offset == ORIGINAL || offset == AMBIGUOUS)
		bname = Minibuf;
	else if (offset == NULLSTRING) {
		if (def)
			bname = def->b_name;
		else
			complain((char *) 0);
	} else if (offset < 0)
		complain((char *) 0);
	else
		bname = bnames[offset];

	return bname;
}

void
BufSelect()
{
	register char	*bname;

	bname = ask_buf(lastbuf);
	SetABuf(curbuf);
	SetBuf(do_select(curwind, bname));
}

#if defined(MSDOS)

private void
BufNSelect(n)
{
	char *bnames[100];
	char *bname;
	int i;

	mkbuflist(bnames);
	for (i=0; i<n; i++)
	    if (bnames[i] == 0)
	       complain("[No such buffer]");
	bname = bnames[n-1];
	SetABuf(curbuf);
	SetBuf(do_select(curwind, bname));
}

void Buf1Select() { BufNSelect(1); }
void Buf2Select() { BufNSelect(2); }
void Buf3Select() { BufNSelect(3); }
void Buf4Select() { BufNSelect(4); }
void Buf5Select() { BufNSelect(5); }
void Buf6Select() { BufNSelect(6); }
void Buf7Select() { BufNSelect(7); }
void Buf8Select() { BufNSelect(8); }
void Buf9Select() { BufNSelect(9); }
void Buf10Select() { BufNSelect(10); }

#endif /* MSDOS */

private void
defb_wind(b)
register Buffer *b;
{
	register Window	*w = fwind;
	char	*alt;

	if (lastbuf == b || lastbuf == 0) {
		lastbuf = 0;
		alt = (b->b_next != 0) ? b->b_next->b_name : Mainbuf;
	} else
		alt = lastbuf->b_name;

	do {
		if (w->w_bufp == b) {
			if (one_windp() || alt != Mainbuf)
				(void) do_select(w, alt);
			else {
				Window	*save = w->w_next;

				del_wind(w);
				w = save->w_prev;
			}
		}
		w = w->w_next;
	} while (w != fwind || w->w_bufp == b);
}

private Buffer *
getNMbuf()
{
	register Buffer	*delbuf;
	register char	*bname;

	bname = ask_buf(curbuf);
	if ((delbuf = buf_exists(bname)) == 0)
		complain("[No such buffer]");
	if (delbuf->b_modified)
		confirm("%s modified, are you sure? ", bname);
	return delbuf;
}

void
BufErase()
{
	register Buffer	*delbuf;

	if ((delbuf = getNMbuf()) != NIL) {
		initlist(delbuf);
		delbuf->b_modified = 0;
	}
}

private void
kill_buf(delbuf)
register Buffer	*delbuf;
{
	register Buffer	*b,
			*lastb = 0;

#if defined(IPROCS)
	pbuftiedp(delbuf);	/* check for lingering processes */
#endif
	/* clean up windows associated with this buffer */
	if (delbuf == curbuf)
		curbuf = NULL;
	if (delbuf == lastbuf)
		lastbuf = curbuf;	/* even if NULL */
	defb_wind(delbuf);
	if (curbuf == NULL)
		SetBuf(curwind->w_bufp);

	/* unlink the buffer */
	for (b = world; b != 0; lastb = b, b = b->b_next)
		if (b == delbuf)
			break;
	if (lastb)
		lastb->b_next = delbuf->b_next;
	else
		world = delbuf->b_next;

#if !defined(MAC)
	if (perr_buf == delbuf) {
		ErrFree();
		perr_buf = 0;
	}
#endif

	lfreelist(delbuf->b_first);
	if (delbuf->b_name)
		free(delbuf->b_name);
	if (delbuf->b_fname)
		free(delbuf->b_fname);
	flush_marks(delbuf);
	free((char *) delbuf);

#if defined(MAC)
	Bufchange = 1;
#endif
}

/* offer to kill some buffers */

void
KillSome()
{
	register Buffer	*b,
			*next;
	Buffer	*oldb;
	register char	*y_or_n;

	for (b = world; b != 0; b = next) {
		next = b->b_next;
		if (yes_or_no_p("Kill %s? ", b->b_name) == NO)
			continue;
		if (IsModified(b)) {
			y_or_n = ask("No", "%s modified; should I save it? ", b->b_name);
			if (CharUpcase(*y_or_n) == 'Y') {
				oldb = curbuf;
				SetBuf(b);
				SaveFile();
				SetBuf(oldb);
			}
		}
		kill_buf(b);
	}
}

void
BufKill()
{
	Buffer	*b;

	if ((b = getNMbuf()) == 0)
		return;
	kill_buf(b);
}

private char *
line_cnt(b, buf)
register Buffer	*b;
char	*buf;
{
	register int	nlines = 0;
	register Line	*lp;

	for (lp = b->b_first; lp != 0; lp = lp->l_next, nlines++)
		;
	swritef(buf, "%d", nlines);
	return buf;
}

private const char	*const TypeNames[] = {
	0,
	"Scratch",
	"File",
	"Process",
};

void
BufList()
{
	register char	*fmt = "%-2s %-5s %-11s %-1s %-*s  %-s";
	register Buffer	*b;
	int	bcount = 1,		/* To give each buffer a number */
		buf_width = 11;
	char	nbuf[10];

	for (b = world; b != 0; b = b->b_next)
		buf_width = max(buf_width, (int)strlen(b->b_name));

	TOstart("Buffer list", TRUE);	/* true means auto-newline */

	Typeout("(* means buffer needs saving)");
	Typeout("(+ means file hasn't been read yet)");
	Typeout(NullStr);
	Typeout(fmt, "NO", "Lines", "Type", NullStr, buf_width, "Name", "File");
	Typeout(fmt, "--", "-----", "----", NullStr, buf_width, "----", "----");
	for (b = world; b != 0; b = b->b_next) {
		Typeout(fmt, itoa(bcount++),
				line_cnt(b, nbuf),
				TypeNames[b->b_type],
				IsModified(b) ? "*" :
					 b->b_ntbf ? "+" : NullStr,
				buf_width,
				/* For the * (variable length field) */
				b->b_name,
				filename(b));

		if (TOabort)
			break;
	}
	TOstop();
}

void
bufname(b)
register Buffer	*b;
{
	char	tmp[100],
		*cp;
	int	try = 1;

	if (b->b_fname == 0)
		complain("[No file name]");
	cp = basename(b->b_fname);
	strcpy(tmp, cp);
	while (buf_exists(tmp)) {
		swritef(tmp, "%s.%d", cp, try);
		try += 1;
	}
	setbname(b, tmp);
}

void
initlist(b)
register Buffer	*b;
{
	lfreelist(b->b_first);
	b->b_first = b->b_dot = b->b_last = 0;
	(void) listput(b, b->b_first);

	SavLine(b->b_dot, NullStr);
	b->b_char = 0;
	AllMarkSet(b, b->b_dot, 0);
	if (b == curbuf)
		getDOT();
}

/* Returns pointer to buffer with name NAME, or if NAME is a string of digits
   returns the buffer whose number equals those digits.  Otherwise, returns
   0. */

Buffer *
buf_exists(name)
register char	*name;
{
	register Buffer	*bp;
	int	n;

	if (name == 0)
		return 0;

	for (bp = world; bp != 0; bp = bp->b_next)
		if (strcmp(bp->b_name, name) == 0)
			return bp;

	/* Doesn't match any names.  Try for a buffer number... */

	if (chr_to_int(name, 10, YES, &n) != INT_BAD) {
		for (bp = world; n > 1; bp = bp->b_next) {
			if (bp == 0)
				break;
			n -= 1;
		}
		return bp;
	}

	return 0;
}

/* Returns buffer pointer with a file name NAME, if one exists.  Stat's the
   file and compares inodes, in case NAME is a link, as well as the actual
   characters that make up the file name. */

Buffer *
file_exists(name)
register char	*name;
{
	struct stat	stbuf;
	register struct stat	*s = &stbuf;
	register Buffer	*b = 0;
	char	fnamebuf[FILESIZE];

#if defined(MSDOS)
	strlwr(name);
#endif /* MSDOS */
	if (name) {
		PathParse(name, fnamebuf);
		if (stat(fnamebuf, s) == -1)
			s->st_ino = 0;
		for (b = world; b != 0; b = b->b_next) {
			if (
#if !defined(MSDOS)
			    (b->b_ino != 0 && b->b_ino == s->st_ino &&
			     b->b_dev != 0 && b->b_dev == s->st_dev) ||
#endif /* MSDOS */
			    (b->b_fname != 0 &&
			     strcmp(b->b_fname, fnamebuf) == 0))
				break;
		}
	}
	return b;
}

void
setbname(b, name)
register Buffer	*b;
register char	*name;
{
	UpdModLine = YES;	/* Kludge ... but speeds things up considerably */
	if (name) {
		if (b->b_name == NoName)
			b->b_name = 0;
		b->b_name = ralloc(b->b_name, strlen(name) + 1);
		strcpy(b->b_name, name);
	} else
		b->b_name = 0;
#if defined(MAC)
	Bufchange = 1;
#endif
}

void
setfname(b, name)
register Buffer	*b;
register char	*name;
{
	char	wholename[FILESIZE],
		oldname[FILESIZE],
		*oldptr = oldname;
	Buffer	*save = curbuf;

	SetBuf(b);
	UpdModLine = YES;	/* Kludge ... but speeds things up considerably */
	if (b->b_fname == 0)
		oldptr = 0;
	else
		strcpy(oldname, b->b_fname);
	if (name) {
#if defined(MSDOS)
		strlwr(name);
#endif /* MSDOS */
		PathParse(name, wholename);
		curbuf->b_fname = ralloc(curbuf->b_fname, strlen(wholename) + 1);
		strcpy(curbuf->b_fname, wholename);
	} else
		b->b_fname = 0;
	DoAutoExec(curbuf->b_fname, oldptr);
	curbuf->b_mtime = curbuf->b_dev = curbuf->b_ino = 0;	/* until they're known. */
	SetBuf(save);
#if defined(MAC)
	Bufchange = 1;
#endif
}

void
set_ino(b)
register Buffer	*b;
{
	struct stat	stbuf;

	if (b->b_fname == 0 || stat(pr_name(b->b_fname, NO), &stbuf) == -1) {
		b->b_dev = 0;
		b->b_ino = 0;
		b->b_mtime = 0;
	} else {
		b->b_dev = stbuf.st_dev;
		b->b_ino = stbuf.st_ino;
		b->b_mtime = stbuf.st_mtime;
	}
}

/* Find the file `fname' into buf and put in in window `w' */

Buffer *
do_find(w, fname, force)
register Window	*w;
register char	*fname;
int	force;
{
	register Buffer	*b;

	b = file_exists(fname);
	if (b == 0) {
		b = mak_buf();
		setfname(b, fname);
		bufname(b);
		set_ino(b);
		b->b_ntbf = 1;
	}
	if (force) {
		Buffer	*oldb = curbuf;

		SetBuf(b);	/* this'll read the file */
		SetBuf(oldb);
	}
	if (w)
		tiewind(w, b);
	return b;
}

/* set alternate buffer */

void
SetABuf(b)
Buffer	*b;
{
	if (b != 0)
		lastbuf = b;
}


/* check to see if BP is a valid buffer pointer */
private int
valid_bp(bp)
register Buffer	*bp;
{
	register Buffer	*b;

	for (b = world; b != 0; b = b->b_next)
		if (b == bp)
			break;
	return b != 0;
}

void
SetBuf(newbuf)
register Buffer	*newbuf;
{
	if (newbuf == curbuf || newbuf == 0)
		return;

	if (!valid_bp(newbuf))
		complain("Internal error: (0x%x) is not a valid buffer pointer!", newbuf);
	lsave();
	curbuf = newbuf;
	curline = newbuf->b_dot;
	curchar = newbuf->b_char;
	getDOT();
	/* do the read now ... */
	if (curbuf->b_ntbf)
		read_file(curbuf->b_fname, 0);
#if defined(MAC)
	Modechange = 1;
#endif
}

Buffer *
do_select(w, name)
register Window	*w;
register char	*name;
{
	register Buffer	*new;

	if ((new = buf_exists(name)) == 0) {
		new = mak_buf();
		setfname(new, (char *) 0);
		setbname(new, name);
	}
	if (w)
		tiewind(w, new);
	return new;
}

void
buf_init()
{
	SetBuf(do_select(curwind, Mainbuf));
}
