/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

/* Contains commands that deal with creating, selecting, killing and
   listing buffers, and buffer modes, and find-file, etc. */

#include "jove.h"

#include <sys/stat.h>

char	*Mainbuf = "Main",
	*NoName = "Sans un nom!";

Buffer	*world = 0,		/* First in the list */
	*curbuf = 0,
	*lastbuf = 0;	/* Last buffer we were in so we have a default
			   buffer during a select buffer. */

/* Toggle BIT in the current buffer's minor mode flags.  If argument is
   supplied, a positive one always turns on the mode and zero argument
   always turns it off. */

TogMinor(bit)
{
	if (exp_p) {
		if (exp == 0)
			curbuf->b_minor &= ~bit;
		else
			curbuf->b_minor |= bit;
	} else
		curbuf->b_minor ^= bit;
	UpdModLine++;
}

/* Creates a new buffer, links it at the end of the buffer chain, and
   returns it. */

static Buffer *
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

	return b;
}

/* Makes a buffer and initializes it.  Obsolete.  Used to take two
   arguments, a buffer name and a file name. */

static Buffer *
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
	initlist(newb);
	return newb;
}

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

FindFile()
{
	register char	*name;
	char	fnamebuf[FILESIZE];

	name = ask_file(curbuf->b_fname, fnamebuf);
	SetABuf(curbuf);
	SetBuf(do_find(curwind, name, 0));
}

static
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
		sprintf(prompt, ": %f (default %s) ", def->b_name);
	else
		sprintf(prompt, ProcFmt);
	mkbuflist(bnames);
	offset = complete(bnames, prompt, RET_STATE);
	if (offset == EOF)
		complain((char *) 0);
	if (offset == ORIGINAL)
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

BufSelect()
{
	register char	*bname;

	bname = ask_buf(lastbuf);
	SetABuf(curbuf);
	SetBuf(do_select(curwind, bname));
}

static
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
			if (one_windp())
				(void) do_select(w, alt);
			else {
				register Window	*save = w->w_next;

				del_wind(w);
				w = save->w_prev;
			}
		}				
		w = w->w_next;
	} while (w != fwind);
}

Buffer *
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

BufErase()
{
	register Buffer	*delbuf;

	if (delbuf = getNMbuf()) {
		initlist(delbuf);
		delbuf->b_modified = 0;
	}
}

static
kill_buf(delbuf)
register Buffer	*delbuf;
{
	register Buffer	*b,
			*lastb = 0;
	extern Buffer	*perr_buf;

#ifdef IPROCS
	pbuftiedp(delbuf);	/* Make sure buffer is not tied to a process */
#endif
	for (b = world; b != 0; lastb = b, b = b->b_next)
		if (b == delbuf)
			break;
	if (lastb)
		lastb->b_next = delbuf->b_next;
	else
		world = delbuf->b_next;

#define okay_free(ptr)	if (ptr) free(ptr)

	lfreelist(delbuf->b_first);
	okay_free(delbuf->b_name);
	okay_free(delbuf->b_fname);
	free((char *) delbuf);

	if (delbuf == lastbuf)
		SetABuf(curbuf);
	if (perr_buf == delbuf) {
		ErrFree();
		perr_buf = 0;
	}
	defb_wind(delbuf);
	if (curbuf == delbuf)
		SetBuf(curwind->w_bufp);
}

/* offer to kill some buffers */

KillSome()
{
	register Buffer	*b,
			*next;
	Buffer	*oldb;
	register char	*y_or_n;

	for (b = world; b != 0; b = next) {
		next = b->b_next;
		y_or_n = ask("No", "Kill %s? ", b->b_name);
		if (Upper(*y_or_n) != 'Y')
			continue;
		if (IsModified(b)) {
			y_or_n = ask("No", "%s modified; should I save it? ", b->b_name);
			if (Upper(*y_or_n) == 'Y') {
				oldb = curbuf;
				SetBuf(b);
				SaveFile();
				SetBuf(oldb);
			}
		}
		kill_buf(b);
	}
}

BufKill()
{
	Buffer	*b;

	if ((b = getNMbuf()) == 0)
		return;
	kill_buf(b);
}

static char *
line_cnt(b, buf)
register Buffer	*b;
char	*buf;
{
	register int	nlines = 0;
	register Line	*lp;

	for (lp = b->b_first; lp != 0; lp = lp->l_next, nlines++)
		;
	sprintf(buf, "%d", nlines);
	return buf;
}

static char	*TypeNames[] = {
	0,
	"Scratch",
	"File",
	"Process",
	"I-process"
};

BufList()
{
	register char	*format = "%-2s %-5s %-11s %-1s %-*s  %-s";
	register Buffer	*b;
	int	bcount = 1,		/* To give each buffer a number */
		buf_width = 11;
	char	nbuf[10];
	extern int	ModCount;

	for (b = world; b != 0; b = b->b_next)
		buf_width = max(buf_width, strlen(b->b_name));

	TOstart("Buffer list", TRUE);	/* true means auto-newline */

	Typeout("(* means buffer needs saving)");
	Typeout("(+ means file hasn't been read yet)");
	Typeout(NullStr);
	Typeout(format, "NO", "Lines", "Type", NullStr, buf_width, "Name", "File");
	Typeout(format, "--", "-----", "----", NullStr, buf_width, "----", "----");
	for (b = world; b != 0; b = b->b_next) {
		Typeout(format, itoa(bcount++),
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
		sprintf(tmp, "%s.%d", cp, try);
		try++;
	}
	setbname(b, tmp);
}

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
	register int	n;

	if (name == 0)
		return 0;

	for (bp = world; bp != 0; bp = bp->b_next)
		if (strcmp(bp->b_name, name) == 0)
			return bp;

	/* Doesn't match any names.  Try for a buffer number... */

	if ((n = chr_to_int(name, 10, 1)) > 0) {
		for (bp = world; n > 1; bp = bp->b_next) {
			if (bp == 0)
				break;
			--n;
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

	if (name) {
		PathParse(name, fnamebuf);
		if (stat(fnamebuf, s) == -1)
			s->st_ino = 0;
		for (b = world; b != 0; b = b->b_next) {
			if ((b->b_ino != 0 && b->b_ino == s->st_ino) ||
			    (strcmp(b->b_fname, fnamebuf) == 0))
				break;
		}
	}
	return b;
}

char *
ralloc(obj, size)
register char	*obj;
{
	register char	*new;

	if (obj)
		new = realloc(obj, (unsigned) size);
	if (new == 0 || !obj)
		new = emalloc(size);
	return new;
}

setbname(b, name)
register Buffer	*b;
register char	*name;
{
	UpdModLine++;	/* Kludge ... but speeds things up considerably */
	if (name) {
		if (b->b_name == NoName)
			b->b_name = 0;
		b->b_name = ralloc(b->b_name, strlen(name) + 1);
		strcpy(b->b_name, name);
	} else
		b->b_name = 0;
}

setfname(b, name)
register Buffer	*b;
register char	*name;
{
	char	wholename[FILESIZE],
		oldname[FILESIZE],
		*oldptr = oldname;
	Buffer	*save = curbuf;

	SetBuf(b);
	UpdModLine++;	/* Kludge ... but speeds things up considerably */
	if (b->b_fname == 0)
		oldptr = 0;
	else
		strcpy(oldname, b->b_fname);
	if (name) {
		PathParse(name, wholename);
		curbuf->b_fname = ralloc(curbuf->b_fname, strlen(wholename) + 1);
		strcpy(curbuf->b_fname, wholename);
	} else
		b->b_fname = 0;
	DoAutoExec(curbuf->b_fname, oldptr);
	curbuf->b_mtime = curbuf->b_ino = 0;	/* until they're known. */
	SetBuf(save);
}

set_ino(b)
register Buffer	*b;
{
	struct stat	stbuf;

	if (b->b_fname == 0 || stat(b->b_fname, &stbuf) == -1) {
		b->b_ino = 0;
		b->b_mtime = 0;
	} else {
		b->b_ino = stbuf.st_ino;
		b->b_mtime = stbuf.st_mtime;
	}
}

/* Find the file `fname' into buf and put in in window `w' */

Buffer *
do_find(w, fname, force)
register Window	*w;
register char	*fname;
{
	register Buffer	*b;

	b = file_exists(fname);
	if (b == 0) {
		b = mak_buf();
		setfname(b, fname);
		bufname(b);
		set_ino(b);
		b->b_ntbf = 1;
		if (force) {
			Buffer	*oldb = curbuf;

			SetBuf(b);	/* this'll read the file */
			SetBuf(oldb);
		}
	}
	if (w)
		tiewind(w, b);
	return b;
}

/* Set alternate buffer */

SetABuf(b)
Buffer	*b;
{
	if (b != 0)
		lastbuf = b;
}

SetBuf(newbuf)
register Buffer	*newbuf;
{
	register Buffer	*oldb = curbuf;

	if (newbuf == curbuf || newbuf == 0)
		return;

	lsave();
	curbuf = newbuf;
	getDOT();
	/* Do the read now ... */
	if (curbuf->b_ntbf)
		read_file(curbuf->b_fname, 0);

#ifdef IPROCS
	if (oldb != 0 && oldb->b_type != curbuf->b_type) {
		if (curbuf->b_type == B_IPROCESS)
			PushPBs();		/* Push process bindings */
		else if (oldb->b_type == B_IPROCESS)
			PopPBs();
	}
	assign_p();		/* Set cur_proc */
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
