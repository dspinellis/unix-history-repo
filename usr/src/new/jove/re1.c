/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "io.h"
#include "re.h"
#include "ctype.h"

#ifdef MAC
#	include "mac.h"
#else
#	include <sys/stat.h>
#endif

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private Bufpos * doisearch(int, int, int);

private void
	IncSearch(int),
	replace(int, int);
private int
	isearch(int, Bufpos *),
	lookup(char *, char *, char *, char *),
	substitute(int, Line *, int, Line *, int);
#else
private Bufpos * doisearch();

private void
	IncSearch(),
	replace();
private int
	isearch(),
	lookup(),
	substitute();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif

private int
substitute(query, l1, char1, l2, char2)
Line	*l1,
	*l2;
{
	Line	*lp;
	int	numdone = 0,
		offset = curchar,
		stop = NO;
	disk_line	UNDO_da = 0;
	Line		*UNDO_lp = 0;

	lsave();
	REdirection = FORWARD;

	lp = l1;
	for (lp = l1; (lp != l2->l_next) && !stop; lp = lp->l_next) {
		offset = (lp == l1) ? char1 : 0;
		while (!stop && re_lindex(lp, offset, compbuf, alternates, 0)) {
			if (lp == l2 && REeom > char2)	/* nope, leave this alone */
				break;
			DotTo(lp, REeom);
			offset = curchar;
			if (query) {
				message("Replace (Type '?' for help)? ");
reswitch:			redisplay();
				switch (CharUpcase(getchar())) {
				case '.':
					stop = YES;
					/* Fall into ... */

				case ' ':
				case 'Y':
					break;

				case BS:
				case RUBOUT:
				case 'N':
					if (linebuf[offset++] == '\0')
						goto nxtline;
					continue;

				case CTL('W'):
					re_dosub(linebuf, YES);
					numdone += 1;
					offset = curchar = REbom;
					makedirty(curline);
					/* Fall into ... */

				case CTL('R'):
				case 'R':
					RErecur();
					offset = curchar;
					lp = curline;
					continue;

				case CTL('U'):
				case 'U':
					if (UNDO_lp == 0)
						continue;
					lp = UNDO_lp;
					lp->l_dline = UNDO_da | DIRTY;
					offset = 0;
					numdone -= 1;
					continue;

				case 'P':
				case '!':
					query = 0;
					break;

				case CR:
				case LF:
				case 'Q':
					goto done;

				case CTL('L'):
					RedrawDisplay();
					goto reswitch;

				default:
					rbell();
message("Space or Y, Period, Rubout or N, C-R or R, C-W, C-U or U, P or !, Return.");
					goto reswitch;
				}
			}
			re_dosub(linebuf, NO);
			numdone += 1;
			modify();
			offset = curchar = REeom;
			makedirty(curline);
			if (query) {
				message(mesgbuf);	/* no blinking */
				redisplay();		/* show the change */
			}
			UNDO_da = curline->l_dline;
			UNDO_lp = curline;
			if (linebuf[offset] == 0)
nxtline:			break;
		}
	}
done:	return numdone;
}

/* prompt for search and replacement strings and do the substitution */
private void
replace(query, inreg)
{
	Mark	*m;
	char	*rep_ptr;
	Line	*l1 = curline,
		*l2 = curbuf->b_last;
	int	char1 = curchar,
		char2 = length(curbuf->b_last),
		numdone;

	if (inreg) {
		m = CurMark();
		l2 = m->m_line;
		char2 = m->m_char;
		(void) fixorder(&l1, &char1, &l2, &char2);
	}

	/* get search string */
	strcpy(rep_search, ask(rep_search[0] ? rep_search : (char *) 0, ProcFmt));
	REcompile(rep_search, UseRE, compbuf, alternates);
	/* Now the replacement string.  Do_ask() so the user can play with
	   the default (previous) replacement string by typing C-R in ask(),
	   OR, he can just hit Return to replace with nothing. */
	rep_ptr = do_ask("\r\n", (int (*)()) 0, rep_str, ": %f %s with ", rep_search);
	if (rep_ptr == 0)
		rep_ptr = NullStr;
	strcpy(rep_str, rep_ptr);

	if (((numdone = substitute(query, l1, char1, l2, char2)) != 0) &&
	    (inreg == NO)) {
		do_set_mark(l1, char1);
		add_mess(" ");		/* just making things pretty */
	} else
		message("");
	add_mess("(%d substitution%n)", numdone, numdone);
}

void
RegReplace()
{
	replace(0, YES);
}

void
QRepSearch()
{
	replace(1, NO);
}

void
RepSearch()
{
	replace(0, NO);
}

/* Lookup a tag in tag file FILE.  FILE is assumed to be sorted
   alphabetically.  The FASTTAGS code, which is implemented with
   a binary search, depends on this assumption.  If it's not true
   it is possible to comment out the fast tag code (which is clearly
   labeled) and everything else will just work. */

private int
lookup(searchbuf, filebuf, tag, file)
char	*searchbuf,
	*filebuf,
	*tag,
	*file;
{
	register int	taglen = strlen(tag);
	char	line[BUFSIZ],
		pattern[128];
	register File	*fp;
	struct stat	stbuf;
	int	fast = YES,
		success = NO;
	register off_t	lower, upper;

	sprintf(pattern, "^%s[^\t]*\t*\\([^\t]*\\)\t*[?/]\\([^?/]*\\)[?/]", tag);
	fp = open_file(file, iobuff, F_READ, !COMPLAIN, QUIET);
	if (fp == NIL)
		return 0;

	/* ********BEGIN FAST TAG CODE******** */

	if (stat(file, &stbuf) < 0)
		fast = NO;
	else {
		lower = 0;
		upper = stbuf.st_size;
		if (upper - lower < BUFSIZ)
			fast = NO;
	}
	if (fast == YES) for (;;) {
		off_t	mid;
		int	whichway,
			chars_eq;

		if (upper - lower < BUFSIZ) {
			f_seek(fp, lower);
			break;			/* stop this nonsense */
		}
		mid = (lower + upper) / 2;
		f_seek(fp, mid);
		f_toNL(fp);
		if (f_gets(fp, line, sizeof line) == EOF)
			break;
		chars_eq = numcomp(line, tag);
		if (chars_eq == taglen && iswhite(line[chars_eq]))
			goto found;
		whichway = line[chars_eq] - tag[chars_eq];
		if (whichway < 0) {		/* line is BEFORE tag */
			lower = mid;
			continue;
		} else if (whichway > 0) {	/* line is AFTER tag */
			upper = mid;
			continue;
		}
	}
	f_toNL(fp);
	/* END FAST TAG CODE */

	while (f_gets(fp, line, sizeof line) != EOF) {
		int	cmp;

		if (line[0] > *tag)
			break;
		else if ((cmp = strncmp(line, tag, taglen)) > 0)
			break;
		else if (cmp < 0)
			continue;
		/* if we get here, we've found the match */
found:		if (!LookingAt(pattern, line, 0)) {
			complain("I thought I saw it!");
			break;
		} else {
			putmatch(1, filebuf, FILESIZE);
			putmatch(2, searchbuf, 100);
			success = YES;
			break;
		}
	}
	close_file(fp);
		
	if (success == NO)
		s_mess("Can't find tag \"%s\".", tag);
	return success;
}

#ifndef MSDOS
char	TagFile[FILESIZE] = "./tags";
#else /* MSDOS */
char	TagFile[FILESIZE] = "tags";
#endif /* MSDOS */

void
find_tag(tag, localp)
char	*tag;
{
	char	filebuf[FILESIZE],
		sstr[100],
		tfbuf[FILESIZE];
	register Bufpos	*bp;
	register Buffer	*b;
	char	*tagfname;

	if (!localp) {
		char	prompt[128];

		sprintf(prompt, "With tag file (%s default): ", TagFile);
		tagfname = ask_file(prompt, TagFile, tfbuf);
	} else
		tagfname = TagFile;
	if (lookup(sstr, filebuf, tag, tagfname) == 0)
		return;
	set_mark();
	b = do_find(curwind, filebuf, 0);
	if (curbuf != b)
		SetABuf(curbuf);
	SetBuf(b);
	if ((bp = dosearch(sstr, BACKWARD, 0)) == 0 &&
	    ((bp = dosearch(sstr, FORWARD, 0)) == 0))
		message("Well, I found the file, but the tag is missing.");
	else
		SetDot(bp);
}

void
FindTag()
{
	int	localp = !is_an_arg();
	char	tag[128];

	strcpy(tag, ask((char *) 0, ProcFmt));
	find_tag(tag, localp);
}

/* Find Tag at Dot. */

void
FDotTag()
{
	int	c1 = curchar,
		c2 = c1;
	char	tagname[50];

	if (!ismword(linebuf[curchar]))
		complain("Not a tag!");
	while (c1 > 0 && ismword(linebuf[c1 - 1]))
		c1 -= 1;
	while (ismword(linebuf[c2]))
		c2 += 1;

	null_ncpy(tagname, linebuf + c1, c2 - c1);
	find_tag(tagname, !is_an_arg());
}

/* I-search returns a code saying what to do:
   STOP:	We found the match, so unwind the stack and leave
		where it is.
   DELETE:	Rubout the last command.
   BACKUP:	Back up to where the isearch was last NOT failing.

   When a character is typed it is appended to the search string, and
   then, isearch is called recursively.  When C-S or C-R is typed, isearch
   is again called recursively. */

#define STOP	1
#define DELETE	2
#define BACKUP	3
#define TOSTART	4

static char	ISbuf[128],
		*incp = 0;
int	SExitChar = CR;

#define cmp_char(a, b)	((a) == (b) || (CaseIgnore && (CharUpcase(a) == CharUpcase(b))))

static Bufpos *
doisearch(dir, c, failing)
register int	c,
		dir,
		failing;
{
	static Bufpos	buf;
	Bufpos	*bp;
	extern int	okay_wrap;

	if (c == CTL('S') || c == CTL('R'))
		goto dosrch;

	if (failing)
		return 0;
	DOTsave(&buf);
	if (dir == FORWARD) {
		if (cmp_char(linebuf[curchar], c)) {
			buf.p_char = curchar + 1;
			return &buf;
		}
	} else {
		if (look_at(ISbuf))
			return &buf;
	}
dosrch:	okay_wrap = YES;
	if ((bp = dosearch(ISbuf, dir, 0)) == 0)
		rbell();	/* ring the first time there's no match */
	okay_wrap = NO;
	return bp;
}

void
IncFSearch()
{
	IncSearch(FORWARD);
}

void
IncRSearch()
{
	IncSearch(BACKWARD);
}

private void
IncSearch(dir)
{
	Bufpos	save_env;

	DOTsave(&save_env);
	ISbuf[0] = 0;
	incp = ISbuf;
	if (isearch(dir, &save_env) == TOSTART)
		SetDot(&save_env);
	else {
		if (LineDist(curline, save_env.p_line) >= MarkThresh)
			do_set_mark(save_env.p_line, save_env.p_char);
	}
	setsearch(ISbuf);
}

/* Nicely recursive. */

private int
isearch(dir, bp)
Bufpos	*bp;
{
	Bufpos	pushbp;
	int	c,
		ndir,
		failing;
	char	*orig_incp;

	if (bp != 0) {		/* Move to the new position. */
		pushbp.p_line = bp->p_line;
		pushbp.p_char = bp->p_char;
		SetDot(bp);
		failing = 0;
	} else {
		DOTsave(&pushbp);
		failing = 1;
	}
	orig_incp = incp;
	ndir = dir;		/* Same direction as when we got here, unless
				   we change it with C-S or C-R. */
	for (;;) {
		SetDot(&pushbp);
		message(NullStr);
		if (failing)
			add_mess("Failing ");
		if (dir == BACKWARD)
			add_mess("reverse-");
		add_mess("I-search: %s", ISbuf);
		DrawMesg(NO);
		add_mess(NullStr);	/* tell me this is disgusting ... */
		c = getch();
		if (c == SExitChar)
			return STOP;
		if (c == AbortChar) {
			/* If we're failing, we backup until we're no longer
			   failing or we've reached the beginning; else, we
			   just about the search and go back to the start. */
			if (failing)
				return BACKUP;
			return TOSTART;
		}
		switch (c) {
		case RUBOUT:
		case BS:
			return DELETE;

		case CTL('\\'):
			c = CTL('S');

		case CTL('S'):
		case CTL('R'):
			/* If this is the first time through and we have a
			   search string left over from last time, use that
			   one now. */
			if (incp == ISbuf) {
				strcpy(ISbuf, getsearch());
				incp = &ISbuf[strlen(ISbuf)];
			}
			ndir = (c == CTL('S')) ? FORWARD : BACKWARD;
			/* If we're failing and we're not changing our
			   direction, don't recur since there's no way
			   the search can work. */
			if (failing && ndir == dir) {
				rbell();
				continue;
			}
			break;

		case '\\':
			if (incp > &ISbuf[(sizeof ISbuf) - 1]) {
				rbell();
				continue;
			}
			*incp++ = '\\';
			add_mess("\\");
			/* Fall into ... */

		case CTL('Q'):
		case CTL('^'):
			add_mess("");
			c = getch() | 0400;
			/* Fall into ... */

		default:
			if (c & 0400)
				c &= CHARMASK;
			else {
#ifdef IBMPC
				if (c == RUBOUT || c == 0xff || (c < ' ' && c != '\t')) {
#else
				if (c > RUBOUT || (c < ' ' && c != '\t')) {
#endif
					Ungetc(c);
					return STOP;
				}
			}
			if (incp > &ISbuf[(sizeof ISbuf) - 1]) {
				rbell();
				continue;
			}
			*incp++ = c;
			*incp = 0;
			break;
		}
		add_mess("%s", orig_incp);
		add_mess(" ...");	/* so we know what's going on */
		DrawMesg(NO);		/* do it now */
		switch (isearch(ndir, doisearch(ndir, c, failing))) {
		case TOSTART:
			return TOSTART;

		case STOP:
			return STOP;

		case BACKUP:
			/* If we're not failing, we just continue to to the
			   for loop; otherwise we keep returning to the 
			   previous levels until we find one that isn't
			   failing OR we reach the beginning. */
			if (failing)
				return BACKUP;
			/* Fall into ... */

		case DELETE:
			incp = orig_incp;
			*incp = 0;
			continue;
		}
	}
}
