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
#ifdef	IPROCS
# include "fp.h"
# include "iproc.h"
#endif

#ifdef	MAC
# include "mac.h"
#else
# include <sys/stat.h>
#endif

private void
	setbname proto((Buffer *, char *));

private char	*Mainbuf = "Main",
	*NoName = "Sans un nom!";

Buffer	*world = NULL,		/* First in the list */
	*curbuf = NULL,		/* pointer into world for current buffer */
	*lastbuf = NULL;	/* Last buffer we were in so we have a default
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

private Buffer	*free_bufs = NULL;

private Buffer *
buf_alloc()
{
	register Buffer	*b,
			*lastbp;

	lastbp = NULL;
	for (b = world; b != NULL; b = b->b_next)
		lastbp = b;

	if (free_bufs != NULL) {
		b = free_bufs;
		free_bufs = b->b_next;
	} else {
		b = (Buffer *) emalloc(sizeof (Buffer));
	}
	if (lastbp)
		lastbp->b_next = b;
	else
		world = b;
	b->b_first = NULL;
	b->b_next = NULL;
#ifdef	MAC
	b->Type = BUFFER;	/* kludge, but simplifies menu handlers */
	b->Name = NULL;
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
	newb->b_fname = NULL;
	newb->b_name = NoName;
	set_ino(newb);
	newb->b_marks = NULL;
	newb->b_themark = 0;		/* Index into markring */
	/* No marks yet */
	for (i = 0; i < NMARKS; i++)
		newb->b_markring[i] = NULL;
	newb->b_modified = NO;
	newb->b_type = B_FILE;  /* File until proven SCRATCH */
	newb->b_ntbf = NO;
	newb->b_minor = 0;
	newb->b_major = TEXT;
	newb->b_first = NULL;
	newb->b_map = NULL;
#ifdef	IPROCS
	newb->b_process = NULL;
#endif
	initlist(newb);
#ifdef	MAC
	Bufchange = YES;
#endif
	return newb;
}

void
ReNamBuf()
{
	register char	*new = NULL,
			*prompt = ProcFmt,
			*second = "%s already exists; new name? ";

	for (;;) {
		new = ask((char *)NULL, prompt, new);
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

	name = ask_file((char *)NULL, curbuf->b_fname, fnamebuf);
	SetABuf(curbuf);
	SetBuf(do_find(curwind, name, NO));
}

private void
mkbuflist(bnamp, ebnamp)
register char	**bnamp;
char		**ebnamp;
{
	register Buffer	*b;

	for (b = world; b != NULL; b = b->b_next) {
		if (b->b_name != NULL) {
			*bnamp++ = b->b_name;
			if (bnamp >= ebnamp)
				complain("too many buffers to list");
		}
	}
	*bnamp = NULL;
}

char *
ask_buf(def)
Buffer	*def;
{
	char	*bnames[100];
	register char	*bname;
	register int	offset;
	char	prompt[100];

	if (def != NULL && def->b_name != NULL) {
		swritef(prompt, sizeof(prompt), ": %f (default %s) ",
			def->b_name);
	} else {
		swritef(prompt, sizeof(prompt), ProcFmt);
	}
	mkbuflist(bnames, &bnames[sizeof(bnames) / sizeof(*bnames)]);
	offset = complete(bnames, prompt, RET_STATE);
	if (offset == EOF)
		complain((char *)NULL);
	if (offset == ORIGINAL || offset == AMBIGUOUS) {
		bname = Minibuf;
	} else if (offset == NULLSTRING) {
		if (def == NULL)
			complain((char *)NULL);
		bname = def->b_name;
	} else {
		if (offset < 0)
			complain((char *)NULL);
		bname = bnames[offset];
	}

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

#ifdef	MSDOS

private void
BufNSelect(n)
int	n;
{
	register Buffer	*b;

	for (b = world; b != NULL; b = b->b_next) {
		if (b->b_name != NULL) {
			if (n == 0) {
				SetABuf(curbuf);
				SetBuf(do_select(curwind, b->b_name));
				return;
			}
			n -= 1;
		}
	}
	complain("[No such buffer]");
}

void Buf0Select() { BufNSelect(0); }
void Buf1Select() { BufNSelect(1); }
void Buf2Select() { BufNSelect(2); }
void Buf3Select() { BufNSelect(3); }
void Buf4Select() { BufNSelect(4); }
void Buf5Select() { BufNSelect(5); }
void Buf6Select() { BufNSelect(6); }
void Buf7Select() { BufNSelect(7); }
void Buf8Select() { BufNSelect(8); }
void Buf9Select() { BufNSelect(9); }

#endif	/* MSDOS */

private void
defb_wind(b)
register Buffer *b;
{
	register Window	*w = fwind;
	char	*alt;

	if (lastbuf == b || lastbuf == NULL) {
		lastbuf = NULL;
		alt = (b->b_next != NULL) ? b->b_next->b_name : Mainbuf;
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
	if ((delbuf = buf_exists(bname)) == NULL)
		complain("[No such buffer]");
	if (delbuf->b_modified)
		confirm("%s modified, are you sure? ", bname);
	return delbuf;
}

void
BufErase()
{
	register Buffer	*delbuf;

	if ((delbuf = getNMbuf()) != NULL) {
		initlist(delbuf);
		delbuf->b_modified = NO;
	}
}

/* Free a buffer structure.
 * The actual struct is preserved to reduce the damage
 * from dangling references to it.  They seem to be pervasive.
 * We try to reset enough that a dangling reference will be useless.
 */

private void
kill_buf(delbuf)
register Buffer	*delbuf;
{
	register Buffer	*b,
			*lastb = NULL;

#ifdef	IPROCS
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
	for (b = world; b != NULL; lastb = b, b = b->b_next)
		if (b == delbuf)
			break;
	if (lastb)
		lastb->b_next = delbuf->b_next;
	else
		world = delbuf->b_next;

#ifndef	MAC
	if (perr_buf == delbuf) {
		ErrFree();
		perr_buf = NULL;
	}
#endif

	lfreelist(delbuf->b_first);
	delbuf->b_first = delbuf->b_dot = delbuf->b_last = NULL;
	if (delbuf->b_name) {
		free((UnivPtr) delbuf->b_name);
		delbuf->b_name = NULL;
	}
	if (delbuf->b_fname) {
		free((UnivPtr) delbuf->b_fname);
		delbuf->b_fname = NULL;
	}
	flush_marks(delbuf);
	delbuf->b_marks = NULL;

	delbuf->b_next = free_bufs;
	free_bufs = delbuf;
#ifdef	MAC
	Bufchange = YES;
	delbuf->Name = NULL;
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

	for (b = world; b != NULL; b = next) {
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

	if ((b = getNMbuf()) == NULL)
		return;
	kill_buf(b);
}

private char *
line_cnt(b, buf, size)
register Buffer	*b;
char	*buf;
size_t	size;
{
	register int	nlines = 0;
	register Line	*lp;

	for (lp = b->b_first; lp != NULL; lp = lp->l_next, nlines++)
		;
	swritef(buf, size, "%d", nlines);
	return buf;
}

private const char	*const TypeNames[] = {
	NULL,
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

	for (b = world; b != NULL; b = b->b_next)
		buf_width = max(buf_width, (int)strlen(b->b_name));

	TOstart("Buffer list", TRUE);	/* true means auto-newline */

	Typeout("(* means buffer needs saving)");
	Typeout("(+ means file hasn't been read yet)");
	Typeout(NullStr);
	Typeout(fmt, "NO", "Lines", "Type", NullStr, buf_width, "Name", "File");
	Typeout(fmt, "--", "-----", "----", NullStr, buf_width, "----", "----");
	for (b = world; b != NULL; b = b->b_next) {
		Typeout(fmt, itoa(bcount++),
				line_cnt(b, nbuf, sizeof(nbuf)),
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

private void
bufname(b)
register Buffer	*b;
{
	char	tmp[100],
		*cp;
	int	try = 1;

	if (b->b_fname == NULL)
		complain("[No file name]");
	cp = basename(b->b_fname);
	strcpy(tmp, cp);
	while (buf_exists(tmp)) {
		swritef(tmp, sizeof(tmp), "%s.%d", cp, try);
		try += 1;
	}
	setbname(b, tmp);
}

void
initlist(b)
register Buffer	*b;
{
	lfreelist(b->b_first);
	b->b_first = b->b_dot = b->b_last = NULL;
	(void) listput(b, b->b_first);

	SavLine(b->b_dot, NullStr);
	b->b_char = 0;
	AllMarkSet(b, b->b_dot, 0);
	if (b == curbuf)
		getDOT();
}

/* Returns pointer to buffer with name NAME, or if NAME is a string of digits
   returns the buffer whose number equals those digits.  Otherwise, returns
   NULL. */

Buffer *
buf_exists(name)
register char	*name;
{
	register Buffer	*bp;
	int	n;

	if (name == NULL)
		return NULL;

	for (bp = world; bp != NULL; bp = bp->b_next)
		if (strcmp(bp->b_name, name) == 0)
			return bp;

	/* Doesn't match any names.  Try for a buffer number... */

	if (chr_to_int(name, 10, YES, &n) != INT_BAD) {
		for (bp = world; n > 1; bp = bp->b_next) {
			if (bp == NULL)
				break;
			n -= 1;
		}
		return bp;
	}

	return NULL;
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
	register Buffer	*b = NULL;
	char	fnamebuf[FILESIZE];

#ifdef	MSDOS
	strlwr(name);
#endif	/* MSDOS */
	if (name) {
		PathParse(name, fnamebuf);
		if (stat(fnamebuf, s) == -1)
			s->st_ino = 0;
		for (b = world; b != NULL; b = b->b_next) {
			if (
#ifndef	MSDOS
			    (b->b_ino != 0 && b->b_ino == s->st_ino &&
			     b->b_dev != 0 && b->b_dev == s->st_dev) ||
#endif	/* MSDOS */
			    (b->b_fname != NULL &&
			     strcmp(b->b_fname, fnamebuf) == 0))
				break;
		}
	}
	return b;
}

private void
setbname(b, name)
register Buffer	*b;
register char	*name;
{
	UpdModLine = YES;	/* Kludge ... but speeds things up considerably */
	if (name != NULL) {
		if (b->b_name == NoName)
			b->b_name = NULL;
		b->b_name = freealloc((UnivPtr) b->b_name, strlen(name) + 1);
		strcpy(b->b_name, name);
	} else {
		b->b_name = NULL;
	}
#ifdef	MAC
	Bufchange = YES;
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
	if (b->b_fname == NULL)
		oldptr = NULL;
	else
		strcpy(oldname, b->b_fname);
	if (name) {
#ifdef	MSDOS
		strlwr(name);
#endif	/* MSDOS */
		PathParse(name, wholename);
		curbuf->b_fname = freealloc((UnivPtr) curbuf->b_fname, strlen(wholename) + 1);
		strcpy(curbuf->b_fname, wholename);
	} else
		b->b_fname = NULL;
	DoAutoExec(curbuf->b_fname, oldptr);
	curbuf->b_mtime = curbuf->b_dev = curbuf->b_ino = 0;	/* until they're known. */
	SetBuf(save);
#ifdef	MAC
	Bufchange = YES;
#endif
}

void
set_ino(b)
register Buffer	*b;
{
	struct stat	stbuf;

	if (b->b_fname == NULL || stat(pr_name(b->b_fname, NO), &stbuf) == -1) {
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
bool	force;
{
	register Buffer	*b;

	b = file_exists(fname);
	if (b == NULL) {
		b = mak_buf();
		setfname(b, fname);
		bufname(b);
		set_ino(b);
		b->b_ntbf = YES;
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
	if (b != NULL)
		lastbuf = b;
}


/* check to see if BP is a valid buffer pointer */
private bool
valid_bp(bp)
register Buffer	*bp;
{
	register Buffer	*b;

	for (b = world; b != NULL; b = b->b_next)
		if (b == bp)
			return YES;
	return NO;
}

void
SetBuf(newbuf)
register Buffer	*newbuf;
{
	if (newbuf == curbuf || newbuf == NULL)
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
		read_file(curbuf->b_fname, NO);
#ifdef	MAC
	Modechange = YES;
#endif
}

Buffer *
do_select(w, name)
register Window	*w;
register char	*name;
{
	register Buffer	*new;

	if ((new = buf_exists(name)) == NULL) {
		new = mak_buf();
		setfname(new, (char *)NULL);
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
