/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "re.h"
#include "ctype.h"
#include "chars.h"
#include "disp.h"

#ifdef	MAC
# include "mac.h"
#else
# include <sys/stat.h>
#endif

private void
	IncSearch proto((int));

private int
	isearch proto((int, Bufpos *));

private int
substitute(re_blk, query, l1, char1, l2, char2)
struct RE_block	*re_blk;
Line	*l1,
	*l2;
bool	query;
int	char1,
	char2;
{
	Line	*lp;
	int	numdone = 0,
		UNDO_nd = 0,
		offset = char1;
	bool	stop = NO;
	daddr	UNDO_da = NULL_DADDR;
	Line	*UNDO_lp = NULL;

	lsave();

	for (lp = l1; lp != l2->l_next; lp = lp->l_next) {
		int	crater = -1;	/* end of last substitution on this line */
		int	LineDone = NO;	/* already replaced last empty string on line? */

		while (!LineDone
		&& re_lindex(lp, offset, FORWARD, re_blk, NO, crater)
		&& (lp != l2 || REeom <= char2))
		{
			DotTo(lp, REeom);
			offset = curchar;
			if (query) {
				int	c;

				message("Replace (Type '?' for help)? ");
reswitch:
				redisplay();
				c = jgetchar();
				if (c == AbortChar)
					return numdone;

				switch (CharUpcase(c)) {
				case '.':
					stop = YES;
					/*FALLTHROUGH*/
				case ' ':
				case 'Y':
					break;

				case BS:
				case RUBOUT:
				case 'N':
					if (REbom == REeom) {
						offset += 1;
						if (linebuf[REeom] == '\0')
							LineDone = YES;
					}
					continue;

				case CTL('W'):
					re_dosub(re_blk, linebuf, YES);
					if (lp == l2)
						char2 += REdelta;
					modify();
					numdone += 1;
					curchar = REbom;
					makedirty(curline);
					UNDO_da = curline->l_dline;
					UNDO_lp = curline;
					/*FALLTHROUGH*/
				case CTL('R'):
				case 'R':
					RErecur();
					UNDO_lp = NULL;	/* can't reliably undo this */
					offset = curchar;
					lp = curline;
					continue;

				case CTL('U'):
				case 'U':
					if (UNDO_lp == NULL) {
						rbell();
						goto reswitch;
					}
					if (UNDO_lp == NULL)
						getline(UNDO_da, linebuf);	/* someone ought to */
					lp = UNDO_lp;
					lp->l_dline = UNDO_da;
					makedirty(lp);
					offset = 0;
					numdone = UNDO_nd;
					UNDO_lp = NULL;
					continue;

				case 'P':
				case '!':
					query = FALSE;
					break;

				case CR:
				case LF:
				case 'Q':
					return numdone;

				case CTL('L'):
					RedrawDisplay();
					goto reswitch;

				default:
					rbell();
message("Space or Y, Period, Rubout or N, C-R or R, C-W, C-U or U, P or !, Return.");
					goto reswitch;
				}
			}
			if (UNDO_lp != curline) {
				UNDO_da = curline->l_dline;
				UNDO_lp = curline;
				UNDO_nd = numdone;
			}
			if (REbom == REeom && linebuf[REeom] == '\0')
				LineDone = YES;
			re_dosub(re_blk, linebuf, NO);
			if (lp == l2)
				char2 += REdelta;
			numdone += 1;
			modify();
			crater = offset = curchar = REeom;
			makedirty(curline);
			if (query) {
				message(mesgbuf);	/* no blinking */
				redisplay();		/* show the change */
			}
			if (stop)
				return numdone;
		}
		offset = 0;
	}
	return numdone;
}

/* prompt for search and replacement strings and do the substitution */
private void
replace(query, inreg)
bool	query,
	inreg;
{
	Mark	*m;
	char	*rep_ptr;
	Line	*l1 = curline,
		*l2 = curbuf->b_last;
	int	char1 = curchar,
		char2 = length(curbuf->b_last),
		numdone;
	struct RE_block	re_blk;

	if (inreg) {
		m = CurMark();
		l2 = m->m_line;
		char2 = m->m_char;
		(void) fixorder(&l1, &char1, &l2, &char2);
	}

	/* get search string */
	strcpy(rep_search, ask(rep_search[0] ? rep_search : (char *)NULL, ProcFmt));
	REcompile(rep_search, UseRE, &re_blk);
	/* Now the replacement string.  Do_ask() so the user can play with
	   the default (previous) replacement string by typing C-R in ask(),
	   OR, he can just hit Return to replace with nothing. */
	rep_ptr = do_ask("\r\n", (bool (*) ptrproto((int))) NULL, rep_str,
		": %f %s with ", rep_search);
	if (rep_ptr == NULL)
		rep_ptr = NullStr;
	strcpy(rep_str, rep_ptr);

	if ((numdone = substitute(&re_blk, query, l1, char1, l2, char2)) != 0
	&& !inreg)
	{
		do_set_mark(l1, char1);
		add_mess(" ");		/* just making things pretty */
	} else {
		message("");
	}
	add_mess("(%d substitution%n)", numdone, numdone);
}

void
RegReplace()
{
	replace(NO, YES);
}

void
QRepSearch()
{
	replace(YES, NO);
}

void
RepSearch()
{
	replace(NO, NO);
}

/* Lookup a tag in tag file FILE.  FILE is assumed to be sorted
   alphabetically.  The FASTTAGS code, which is implemented with
   a binary search, depends on this assumption.  If it's not true
   it is possible to comment out the fast tag code (which is clearly
   labeled), delete the marked test in the sequential loop, and
   everything else will just work. */

private bool
lookup(searchbuf, filebuf, tag, file)
char	*searchbuf,
	*filebuf,
	*tag,
	*file;
{
	register size_t	taglen = strlen(tag);
	char	line[JBUFSIZ],
		pattern[128];
	register File	*fp;
	struct stat	stbuf;
	bool	success = NO;

	fp = open_file(file, iobuff, F_READ, NO, YES);
	if (fp == NULL)
		return NO;
	swritef(pattern, sizeof(pattern), "^%s[^\t]*\t*\\([^\t]*\\)\t*\\([?/]\\)\\(.*\\)\\2$", tag);

	/* ********BEGIN FAST TAG CODE******** */

	if (stat(file, &stbuf) >= 0) {
		/* Invariant: if there is a line matching the tag, it
		 * begins somewhere after position lower, and begins
		 * at or before upper.  There is one possible
		 * exception: if lower is 0, the line with the tag
		 * might be the very first line.
		 *
		 * When this loop is done, we seek to lower, advance
		 * past the next newline (unless lower is 0), and fall
		 * into the sequential search.
		 */
		register off_t	lower = 0;
		register off_t	upper = stbuf.st_size;

		for (;;) {
			off_t	mid;
			int	chars_eq;

			if (upper - lower < JBUFSIZ)
				break;	/* small range: search sequentially */
			mid = (lower + upper) / 2;
			f_seek(fp, mid);	/* mid will not be 0 */
			f_toNL(fp);
			if (f_gets(fp, line, sizeof line))
				break;		/* unexpected: bail out */
			chars_eq = numcomp(line, tag);
			if (chars_eq == taglen && jiswhite(line[chars_eq])) {
				/* we hit the exact line: get out */
				lower = mid;
				break;
			}
			if (line[chars_eq] < tag[chars_eq])
				lower = mid;	/* line is BEFORE tag */
			else
				upper = mid;	/* line is AFTER tag */
		}
		/* sequentially search from lower */
		f_seek(fp, lower);
		if (lower > 0)
			f_toNL(fp);
	}

	/* END FAST TAG CODE */

	while (!f_gets(fp, line, sizeof line)) {
		int	cmp = line[0] - *tag;

		if (cmp == 0) {
			cmp = strncmp(line, tag, taglen);
			if (cmp == 0) {
				/* we've found the match */
				if (!LookingAt(pattern, line, 0)) {
					complain("I thought I saw it!");
				} else {
					putmatch(1, filebuf, (size_t)FILESIZE);
					putmatch(3, searchbuf, (size_t)100);
					success = YES;
				}
				break;
			}
		}
		if (cmp > 0)
			break;	/* failure: gone too far.  PRESUMES ALPHABETIC ORDER */
	}
	close_file(fp);

	if (success == NO)
		s_mess("Can't find tag \"%s\".", tag);
	return success;
}

char	TagFile[FILESIZE] = "tags";

void
find_tag(tag, localp)
char	*tag;
int	localp;
{
	char	filebuf[FILESIZE],
		sstr[100],
		tfbuf[FILESIZE];
	register Bufpos	*bp;
	register Buffer	*b;
	char	*tagfname;

	if (!localp) {
		char	prompt[128];

		swritef(prompt, sizeof(prompt), "With tag file (%s default): ", TagFile);
		tagfname = ask_file(prompt, TagFile, tfbuf);
	} else
		tagfname = TagFile;
	if (!lookup(sstr, filebuf, tag, tagfname))
		return;
	set_mark();
	b = do_find(curwind, filebuf, NO);
	if (curbuf != b)
		SetABuf(curbuf);
	SetBuf(b);
	if ((bp = dosearch(sstr, BACKWARD, NO)) == NULL &&
	    ((bp = dosearch(sstr, FORWARD, NO)) == NULL))
		message("Well, I found the file, but the tag is missing.");
	else
		SetDot(bp);
}

void
FindTag()
{
	int	localp = !is_an_arg();
	char	tag[128];

	strcpy(tag, ask((char *)NULL, ProcFmt));
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

	null_ncpy(tagname, linebuf + c1, (size_t) (c2 - c1));
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
		*incp = NULL;
int	SExitChar = CR;

#define cmp_char(a, b)	((a) == (b) || (CaseIgnore && (CharUpcase(a) == CharUpcase(b))))

private Bufpos *
doisearch(dir, c, failing)
register int	c,
		dir;
bool		failing;
{
	static Bufpos	buf;
	Bufpos	*bp;

	if (c != CTL('S') && c != CTL('R')) {
		if (failing)
			return NULL;
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
	}
	okay_wrap = YES;
	if ((bp = dosearch(ISbuf, dir, NO)) == NULL)
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
int	dir;
{
	Bufpos	save_env;

	DOTsave(&save_env);
	ISbuf[0] = '\0';
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
int	dir;
Bufpos	*bp;
{
	Bufpos	pushbp;
	int	c,
		ndir;
	bool	failing;
	char	*orig_incp;

	if (bp != NULL) {		/* Move to the new position. */
		pushbp.p_line = bp->p_line;
		pushbp.p_char = bp->p_char;
		SetDot(bp);
		failing = NO;
	} else {
		DOTsave(&pushbp);
		failing = YES;
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
			/*FALLTHROUGH*/
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
			/*FALLTHROUGH*/
		case CTL('Q'):
		case CTL('^'):
			add_mess("");
			c = getch() | 0400;
			/*FALLTHROUGH*/
		default:
			if (c & 0400)
				c &= CHARMASK;
			else {
#ifdef	IBMPC
				if (c == RUBOUT || c == 0xff ||
				    (c < ' ' && c != '\t')
#else
				if (c > RUBOUT || (c < ' ' && c != '\t')
#endif
				    || PrefChar(c)) {
					Ungetc(c);
					return STOP;
				}
			}
			if (incp > &ISbuf[(sizeof ISbuf) - 1]) {
				rbell();
				continue;
			}
			*incp++ = c;
			*incp = '\0';
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
			/*FALLTHROUGH*/
		case DELETE:
			incp = orig_incp;
			*incp = '\0';
			continue;
		}
	}
}
