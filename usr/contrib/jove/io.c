/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"

#ifdef IPROCS
#	include <signal.h>
#endif

#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>

long	io_chars;		/* number of chars in this open_file */
int	io_lines;		/* number of lines in this open_file */
private int	tellall;	/* display file io info? */

#ifdef VMUNIX
char	iobuff[LBSIZE],
	genbuf[LBSIZE],
	linebuf[LBSIZE];
#else
char	*iobuff,
	*genbuf,
	*linebuf;
#endif

#ifdef BACKUPFILES
int	BkupOnWrite = 0;
#endif

close_file(fp)
File	*fp;
{
	if (fp) {
		f_close(fp);
		if (tellall != QUIET)
			add_mess(" %d lines, %D characters.",
				 io_lines,
				 io_chars);
	}
}

/* Write the region from line1/char1 to line2/char2 to FP.  This
   never CLOSES the file since we don't know if we want to. */

int	EndWNewline = 1;

putreg(fp, line1, char1, line2, char2, makesure)
register File	*fp;
Line	*line1,
	*line2;
{
	register int	c;
	register char	*lp;

	if (makesure)
		(void) fixorder(&line1, &char1, &line2, &char2);
	while (line1 != line2->l_next) {
		lp = lcontents(line1) + char1;
		if (line1 == line2)
			fputnchar(lp, (char2 - char1), fp);
		else while (c = *lp++) {
			putc(c, fp);
			io_chars++;
		}
		if (line1 != line2) {
			io_lines++;
			io_chars++;
			putc('\n', fp);
		}
		line1 = line1->l_next;
		char1 = 0;
	}
	flush(fp);
}

read_file(file, is_insert)
char	*file;
{
	Bufpos	save;
	File	*fp;

	if (!is_insert) {
		curbuf->b_ntbf = 0;
		set_ino(curbuf);
	}
	fp = open_file(file, iobuff, F_READ, !COMPLAIN, !QUIET);
	if (fp == NIL) {
		if (!is_insert && errno == ENOENT)
			s_mess("(new file)");
		else
			s_mess(IOerr("open", file));
		return;
	}
	DOTsave(&save);
	dofread(fp);
	SetDot(&save);
	if (is_insert && io_chars > 0)
		modify();
	getDOT();
	close_file(fp);
}

dofread(fp)
register File	*fp;
{
	char	end[LBSIZE];
	int	xeof = 0;
	Line	*savel = curline;
	int	savec = curchar;

	strcpy(end, linebuf + curchar);
	xeof = f_gets(fp, linebuf + curchar, LBSIZE - curchar);
	SavLine(curline, linebuf);
	if (!xeof) do {
		xeof = f_gets(fp, linebuf, LBSIZE);
		curline = listput(curbuf, curline);
		curline->l_dline = putline(linebuf) | DIRTY;
	} while (!xeof);
	linecopy(linebuf, (curchar = strlen(linebuf)), end);
	SavLine(curline, linebuf);
	IFixMarks(savel, savec, curline, curchar);
}

SaveFile()
{
	if (IsModified(curbuf)) {
		if (curbuf->b_fname == 0)
			WriteFile();
		else {
			filemunge(curbuf->b_fname);
			chk_mtime(curbuf->b_fname, "save");
			file_write(curbuf->b_fname, 0);
			unmodify();
		}
	} else
		message("No changes need to be written.");
}

char	*HomeDir;	/* home directory */
int	HomeLen = -1;	/* length of home directory string */

#ifndef CHDIR

char *
pr_name(fname)
char	*fname;
{
	if (fname == 0)
		return 0;

	if (strncmp(fname, HomeDir, HomeLen) == 0) {
		static char	name_buf[100];

		sprintf(name_buf, "~%s", fname + HomeLen);
		return name_buf;
	}

	return fname;
}

#else

#define NDIRS	5

private char	*DirStack[NDIRS] = {0};
private int	DirSP = 0;	/* Directory stack pointer */
#define PWD	(DirStack[DirSP])

char *
pwd()
{
	return PWD;
}

char *
pr_name(fname)
char	*fname;
{
	int	n;

	if (fname == 0)
		return 0;
	n = numcomp(fname, PWD);

	if ((PWD[n] == 0) &&	/* Matched to end of PWD */
	    (fname[n] == '/'))
		return fname + n + 1;

	if (strcmp(HomeDir, "/") != 0 && strncmp(fname, HomeDir, HomeLen) == 0) {
		static char	name_buf[100];

		sprintf(name_buf, "~%s", fname + HomeLen);
		return name_buf;
	}

	return fname;	/* return entire path name */
}

Chdir()
{
	char	dirbuf[FILESIZE];

	(void) ask_file(PWD, dirbuf);
	if (chdir(dirbuf) == -1) {
		s_mess("cd: cannot change into %s.", dirbuf);
		return;
	}
	UpdModLine++;
	setCWD(dirbuf);
}

#ifndef JOB_CONTROL
char *
getwd()
{
	Buffer	*old = curbuf;
	char	*ret_val;

	SetBuf(do_select((Window *) 0, "pwd-output"));
	curbuf->b_type = B_PROCESS;
	(void) UnixToBuf("pwd-output", NO, 0, YES, "/bin/pwd", "pwd", 0);
	ToFirst();
	ret_val = sprint(linebuf);
	SetBuf(old);
	return ret_val;
}
#endif

setCWD(d)
char	*d;
{
	if (PWD == 0)
		PWD = malloc((unsigned) strlen(d) + 1);
	else {
		extern char	*ralloc();

		PWD = ralloc(PWD, strlen(d) + 1);
	}
	strcpy(PWD, d);
}

getCWD()
{
	char	*cwd = getenv("CWD");
#ifdef JOB_CONTROL
	extern char	*getwd();
	char	pathname[FILESIZE];
#endif

	if (cwd == 0)
#ifdef JOB_CONTROL
		cwd = getwd(pathname);
#else
		cwd = getwd();
#endif

	setCWD(cwd);
}	

prDIRS()
{
	register int	i;

	s_mess(": %f ");
	for (i = DirSP; i >= 0; i--)
		add_mess("%s ", pr_name(DirStack[i]));
}

prCWD()
{
	s_mess(": %f => \"%s\"", PWD);
}

Pushd()
{
	char	*newdir,
		dirbuf[FILESIZE];

	newdir = ask_file(NullStr, dirbuf);	/* Parses directories ... */
	UpdModLine++;
	if (*newdir == 0) {	/* Wants to swap top two entries */
		char	*old_top;

		if (DirSP == 0)
			complain("pushd: no other directory.");
		old_top = PWD;
		DirStack[DirSP] = DirStack[DirSP - 1];
		DirStack[DirSP - 1] = old_top;
		(void) chdir(PWD);
	} else {
		if (chdir(dirbuf) == -1) {
			s_mess("pushd: cannot change into %s.", dirbuf);
			return;
		}

		if (DirSP + 1 >= NDIRS)
			complain("pushd: full stack; max of %d pushes.", NDIRS);
		DirSP++;
		setCWD(dirbuf);
	}
	prDIRS();
}

Popd()
{
	if (DirSP == 0)
		complain("popd: directory stack is empty.");
	UpdModLine++;
	free(PWD);
	PWD = 0;
	DirSP--;
	(void) chdir(PWD);	/* If this doesn't work, we's in deep shit. */
	prDIRS();
}

private char *
dbackup(base, offset, c)
register char	*base,
		*offset,
		c;
{
	while (offset > base && *--offset != c)
		;
	return offset;
}

dfollow(file, into)
char	*file,
	*into;
{
	char	*dp,
		*sp;

	if (*file == '/') {		/* Absolute pathname */
		strcpy(into, "/");
		file++;
	} else
		strcpy(into, PWD);
	dp = into + strlen(into);

	sp = file;
	do {
		if (*file == 0)
			break;
		if (sp = index(file, '/'))
			*sp = 0;
		if (strcmp(file, ".") == 0)
			;	/* So it will get to the end of the loop */
		else if (strcmp(file, "..") == 0) {
			*(dp = dbackup(into, dp, '/')) = 0;
			if (dp == into)
				strcpy(into, "/"), dp = into + 1;
		} else {
			if (into[strlen(into) - 1] != '/')
				(void) strcat(into, "/");
			(void) strcat(into, file);
			dp += strlen(file);	/* stay at the end */
		}
		file = sp + 1;
	} while (sp != 0);
}

#endif CHDIR

get_hdir(user, buf)
register char	*user,
		*buf;
{
	char	fbuf[LBSIZE],
		pattern[100];
	register int	u_len;
	File	*fp;

	u_len = strlen(user);
	fp = open_file("/etc/passwd", fbuf, F_READ, COMPLAIN, QUIET);
	sprintf(pattern, "%s:[^:]*:[^:]*:[^:]*:[^:]*:\\([^:]*\\):", user);
	while (f_gets(fp, genbuf, LBSIZE) != EOF)
		if ((strncmp(genbuf, user, u_len) == 0) &&
		    (LookingAt(pattern, genbuf, 0))) {
			putmatch(1, buf, FILESIZE);
			close_file(fp);
			return;
		}
	f_close(fp);
	complain("[unknown user: %s]", user);
}

PathParse(name, intobuf)
char	*name,
	*intobuf;
{
	char	localbuf[FILESIZE];

	intobuf[0] = localbuf[0] = '\0';
	if (*name == '\0')
		return;
	if (*name == '~') {
		if (name[1] == '/' || name[1] == '\0') {
			strcpy(localbuf, HomeDir);
			name++;
		} else {
			char	*uendp = index(name, '/'),
				unamebuf[30];

			if (uendp == 0)
				uendp = name + strlen(name);
			name = name + 1;
			null_ncpy(unamebuf, name, uendp - name);
			get_hdir(unamebuf, localbuf);
			name = uendp;
		}
	} else if (*name == '\\')
		name++;
	(void) strcat(localbuf, name);
#ifdef CHDIR
	dfollow(localbuf, intobuf);
#else
	strcpy(intobuf, localbuf);
#endif
}

filemunge(newname)
char	*newname;
{
	struct stat	stbuf;

	if (newname == 0)
		return;
	if (stat(newname, &stbuf))
		return;
	if ((stbuf.st_ino != curbuf->b_ino) &&
	    ((stbuf.st_mode & S_IFMT) != S_IFCHR) &&
	    (strcmp(newname, curbuf->b_fname) != 0)) {
		rbell();
		confirm("\"%s\" already exists; overwrite it? ", newname);
	}
}

WrtReg()
{
	DoWriteReg(0);
}

AppReg()
{
	DoWriteReg(1);
}

int	CreatMode = DFLT_MODE;

DoWriteReg(app)
{
	char	fnamebuf[FILESIZE],
		*fname;
	Mark	*mp = CurMark();
	File	*fp;

	/* Won't get here if there isn't a Mark */
	fname = ask_file((char *) 0, fnamebuf);

#ifdef BACKUPFILES
	if (!app) {
		filemunge(fname);

		if (BkupOnWrite)
			file_backup(fname);
	}
#else
	if (!app)
		filemunge(fname);
#endif

	fp = open_file(fname, iobuff, app ? F_APPEND : F_WRITE, COMPLAIN, !QUIET);
	putreg(fp, mp->m_line, mp->m_char, curline, curchar, YES);
	close_file(fp);
}

int	OkayBadChars = 0;

WriteFile()
{
	char	*fname,
		fnamebuf[FILESIZE];

	fname = ask_file(curbuf->b_fname, fnamebuf);
	/* Don't allow bad characters when creating new files. */
	if (!OkayBadChars && strcmp(curbuf->b_fname, fnamebuf) != 0) {
		static char	*badchars = "!$^&*()~`{}\"'\\|<>? ";
		register char	*cp = fnamebuf;
		register int	c;

		while (c = *cp++)
			if (c < ' ' || c == '\177' || index(badchars, c))
				complain("'%p': bad character in filename.", c);
	}

	chk_mtime(fname, "write");
	filemunge(fname);
	if (curbuf->b_type != B_IPROCESS)
		curbuf->b_type = B_FILE;  /* In case it wasn't before. */
	setfname(curbuf, fname);
	file_write(fname, 0);
	unmodify();
}

File *
open_file(fname, buf, how, ifbad, loudness)
register char	*fname;
char	*buf;
register int	how;
{
	register File	*fp;

	io_chars = 0;
	io_lines = 0;
	tellall = loudness;

	fp = f_open(fname, how, buf, LBSIZE);
	if (fp == NIL) {
                message(IOerr((how == F_READ) ? "open" : "create", fname));
		if (ifbad == COMPLAIN)
			complain((char *) 0);
	} else {
		int	readonly = FALSE;

		if (access(fname, W_OK) == -1 && errno != ENOENT)
			readonly = TRUE;
							 
		if (loudness != QUIET)
			f_mess("\"%s\"%s", pr_name(fname),
				   readonly ? " [Read only]" : NullStr);
	}
	return fp;
}

/* Check to see if the file has been modified since it was
   last written.  If so, make sure they know what they're
   doing.

   I hate to use another stat(), but to use confirm we gotta
   do this before we open the file. */

chk_mtime(fname, how)
char	*fname,
	*how;
{
	struct stat	stbuf;
	Buffer	*b;
    	char	*mesg = "Shall I go ahead and %s anyway? ";

	if ((curbuf->b_mtime != 0) &&		/* if we care ... */
	    (b = file_exists(fname)) &&		/* we already have this file */
	    (b == curbuf) &&			/* and it's the current buffer */
	    (stat(fname, &stbuf) != -1) &&	/* and we can stat it */
	    (stbuf.st_mtime != b->b_mtime)) {	/* and there's trouble. */
	    	rbell();
		redisplay();	/* Ring that bell! */
	    	TOstart("Warning", TRUE);
	    	Typeout("\"%s\" now saved on disk is not what you last", pr_name(fname));
		Typeout("visited or saved.  Probably someone else is editing");
		Typeout("your file at the same time.  Type \"y\" if I should");
		Typeout("%s anyway.", how);
	    	f_mess(mesg, how);
	    	TOstop();
	    	confirm(mesg, how);
	}
}

file_write(fname, app)
char	*fname;
{
	File	*fp;

#ifdef BACKUPFILES
	if (!app && BkupOnWrite)
		file_backup(fname);
#endif

	fp = open_file(fname, iobuff, app ? F_APPEND : F_WRITE, COMPLAIN, !QUIET);

	if (EndWNewline) {	/* Make sure file ends with a newLine */
		Bufpos	save;

		DOTsave(&save);
		ToLast();
		if (length(curline))	/* Not a blank Line */
			DoTimes(LineInsert(), 1);	/* Make it blank */
		SetDot(&save);
	}
	putreg(fp, curbuf->b_first, 0, curbuf->b_last, length(curbuf->b_last), NO);
	set_ino(curbuf);
	close_file(fp);
}

ReadFile()
{
	char	*fname,
		fnamebuf[FILESIZE];

	fname = ask_file(curbuf->b_fname, fnamebuf);
	chk_mtime(fname, "read");

	if (IsModified(curbuf)) {
		char	*y_or_n;
		int	c;

		for (;;) {
			rbell();
			y_or_n = ask(NullStr, "Shall I make your changes to \"%s\" permanent? ", curbuf->b_name);
			c = Upper(*y_or_n);
			if (c == 'Y' || c == 'N')
				break;
		}			
		if (c == 'Y')
			SaveFile();
	}

	unmodify();
	initlist(curbuf);
	setfname(curbuf, fname);
	read_file(fname, 0);
}

InsFile()
{
	char	*fname,
		fnamebuf[FILESIZE];

	fname = ask_file(curbuf->b_fname, fnamebuf);
	read_file(fname, 1);
}

#include "temp.h"

int	DOLsave = 0;	/* Do Lsave flag.  If lines aren't being save
			   when you think they should have been, this
			   flag is probably not being set, or is being
			   cleared before lsave() was called. */

int	nleft,		/* Number of good characters left in current block */
	tmpfd;
disk_line	tline;	/* Pointer to end of tmp file */

char	*tfname;

tmpinit()
{
	tfname = mktemp(TMPFILE);
	(void) close(creat(tfname, 0600));
	tmpfd = open(tfname, 2);
	if (tmpfd == -1) {
		printf("%s?\n", tfname);
		finish(0);
	}
	block_init();
	tline = 2;
}

tmpclose()
{
	(void) close(tmpfd);
	tmpfd = -1;
	(void) unlink(tfname);
}

/* Get a line at `tl' in the tmp file into `buf' which should be LBSIZE
   long. */

int	Jr_Len;		/* Length of Just Read Line. */

char *
getline(tl, buf)
disk_line	tl;
char	*buf;
{
	register char	*bp,
			*lp;
	register int	nl;

	lp = buf;
	bp = getblock(tl, READ);
	nl = nleft;
	tl &= ~OFFMSK;

	while (*lp++ = *bp++) {
		if (--nl == 0) {
			/* += INCRMT moves tl to the next block in
			   the tmp file. */
			bp = getblock(tl += INCRMT, READ);
			nl = nleft;
		}
	}
	Jr_Len = (lp - buf) - 1;

	return buf;
}

/* Put `buf' and return the disk address */

int	nretries = 0;

disk_line
putline(buf)
char	*buf;
{
	register char	*bp,
			*lp;
	register int	nl;
	disk_line	tl;

	lp = buf;
	tl = tline;
	bp = getblock(tl, WRITE);
	nl = nleft;
	tl &= ~OFFMSK;
	while (*bp = *lp++) {
		if (*bp++ == '\n') {
			*--bp = 0;
			break;
		}
		if (--nl == 0) {
			tline = (tl += INCRMT);
			bp = getblock(tl, WRITE);
			lp = buf;	/* start over ... */
			nretries++;
			nl = nleft;
		}
	}
	tl = tline;
	tline += (((lp - buf) + BNDRY - 1) >> SHFT) & 077776;

	return tl;
}

typedef struct block {
	short	b_dirty,
		b_bno;
	char	b_buf[BUFSIZ];
	struct block
		*b_LRUnext,
		*b_LRUprev,
		*b_HASHnext;
} Block;

#define HASHSIZE	7	/* Primes work best (so I'm told) */
#define B_HASH(bno)	(bno % HASHSIZE)

private Block	b_cache[NBUF],
		*bht[HASHSIZE] = {0},		/* Block hash table */
		*f_block = 0,
		*l_block = 0;
private int	max_bno = -1,
		NBlocks;

private
block_init()
{
	register Block	*bp,	/* Block pointer */
			**hp;	/* Hash pointer */
	register short	bno;

	for (bp = b_cache, bno = NBUF; --bno >= 0; bp++) {
		NBlocks++;
		bp->b_dirty = 0;
		bp->b_bno = bno;
		if (l_block == 0)
			l_block = bp;
		bp->b_LRUprev = 0;
		bp->b_LRUnext = f_block;
		if (f_block != 0)
			f_block->b_LRUprev = bp;
		f_block = bp;

		bp->b_HASHnext = *(hp = &bht[B_HASH(bno)]);
		*hp = bp;
	}
}

private Block *
lookup(bno)
register short	bno;
{
	register Block	*bp;

	for (bp = bht[B_HASH(bno)]; bp != 0; bp = bp->b_HASHnext)
		if (bp->b_bno == bno)
			break;
	return bp;
}

private
LRUunlink(b)
register Block	*b;
{
	if (b->b_LRUprev == 0)
		f_block = b->b_LRUnext;
	else
		b->b_LRUprev->b_LRUnext = b->b_LRUnext;
	if (b->b_LRUnext == 0)
		l_block = b->b_LRUprev;
	else
		b->b_LRUnext->b_LRUprev = b->b_LRUprev;
}

private Block *
b_unlink(bp)
register Block	*bp;
{
	register Block	*hp,
			*prev = 0;

	LRUunlink(bp);
	/* Now that we have the block, we remove it from its position
	   in the hash table, so we can THEN put it somewhere else with
	   it's new block assignment. */

	for (hp = bht[B_HASH(bp->b_bno)]; hp != 0; prev = hp, hp = hp->b_HASHnext)
		if (hp == bp)
			break;
	if (hp == 0) {
		printf("\rBlock %d missing!", bp->b_bno);
		finish(0);
	}
	if (prev)
		prev->b_HASHnext = hp->b_HASHnext;
	else
		bht[B_HASH(bp->b_bno)] = hp->b_HASHnext;

	if (bp->b_dirty) {	/* Do, now, the delayed write */
		blkio(bp, write);
		bp->b_dirty = 0;
	}

	return bp;
}

/* Get a block which contains at least part of the line with the address
   atl.  Returns a pointer to the block and sets the global variable
   nleft (number of good characters left in the buffer). */

char *
getblock(atl, iof)
disk_line	atl;
{
	register int	bno,
			off;
	register Block	*bp;
	static Block	*lastb = 0;

	bno = (atl >> OFFBTS) & BLKMSK;
	off = (atl << SHFT) & LBTMSK;
	if (bno >= NMBLKS)
		error("Tmp file too large.  Get help!");
	nleft = BUFSIZ - off;
	if (lastb != 0 && lastb->b_bno == bno)
		return lastb->b_buf + off;

	/* The requested block already lives in memory, so we move
	   it to the end of the LRU list (making it Most Recently Used)
	   and then return a pointer to it. */

	if (bp = lookup(bno)) {
		if (bp != l_block) {
			LRUunlink(bp);
			if (l_block == 0)
				f_block = l_block = bp;
			else
				l_block->b_LRUnext = bp;
			bp->b_LRUprev = l_block;
			l_block = bp;
			bp->b_LRUnext = 0;
		}
		if (bp->b_bno > max_bno)
			max_bno = bp->b_bno;
		bp->b_dirty |= iof;
		lastb = bp;
		return bp->b_buf + off;
	}

	/* The block we want doesn't reside in memory so we take the
	   least recently used clean block (if there is one) and use
	   it.  */

	bp = f_block;
	if (bp->b_dirty)	/* The best block is dirty ... */
		SyncTmp();

	bp = b_unlink(bp);
	if (l_block == 0)
		l_block = f_block = bp;
	else
		l_block->b_LRUnext = bp;	/* Place it at the end ... */
	bp->b_LRUprev = l_block;
	l_block = bp;
	bp->b_LRUnext = 0;		/* so it's Most Recently Used */

	bp->b_dirty = iof;
	bp->b_bno = bno;
	bp->b_HASHnext = bht[B_HASH(bno)];
	bht[B_HASH(bno)] = bp;

	/* Get the current contents of the block UNLESS this is a new
	   block that's never been looked at before, i.e., it's past
	   the end of the tmp file. */

	if (bp->b_bno <= max_bno)
		blkio(bp, read);
	else
		max_bno = bno;

	lastb = bp;
	return bp->b_buf + off;
}

char *
lbptr(line)
Line	*line;
{
	return getblock(line->l_dline, READ);
}

private
blkio(b, iofcn)
register Block	*b;
register int	(*iofcn)();
{
	(void) lseek(tmpfd, (long) ((unsigned) b->b_bno) * BUFSIZ, 0);
	if ((*iofcn)(tmpfd, b->b_buf, BUFSIZ) != BUFSIZ)
		error("Tmp file %s error.", (iofcn == read) ? "read" : "write");
}

SyncTmp()
{
	register Block	*b;

	for (b = f_block; b != 0; b = b->b_LRUnext)
		if (b->b_dirty) {
			blkio(b, write);
			b->b_dirty = 0;
		}
}

/* save the current contents of linebuf, if it has changed */

lsave()
{
	if (curbuf == 0 || !DOLsave)	/* Nothing modified recently */
		return;

	if (strcmp(lbptr(curline), linebuf) != 0)
		SavLine(curline, linebuf);	/* Put linebuf on the disk. */
	DOLsave = 0;
}

#ifdef BACKUPFILES
file_backup(fname)
char *fname;
{
	char *s;
	register int	i;
	int	fd1,
		fd2;
	char	tmp1[BUFSIZ],
		tmp2[BUFSIZ];
	
	strcpy(tmp1, fname);
	
	if ((s = rindex(tmp1, '/')) == NULL)
		sprintf(tmp2, "#%s", fname);
	else {
		*s++ = NULL;
		sprintf(tmp2, "%s/#%s", tmp1, s);
	}

	if ((fd1 = open(fname, 0)) < 0)
		return;

	if ((fd2 = creat(tmp2, CreatMode)) < 0) {
		(void) close(fd1);
		return;
	}

	while ((i = read(fd1, tmp1, sizeof(tmp1))) > 0)
		write(fd2, tmp1, i);

#ifdef BSD4_2
	(void) fsync(fd2);
#endif
	(void) close(fd2);
	(void) close(fd1);
}
#endif
