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
#include "re.h"

static
substitute(query, l1, char1, l2, char2)
Line	*l1,
	*l2;
{
	Line	*lp;
	int	numdone = 0,
		offset = curchar,
		stop = 0;
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
				switch (Upper(getchar())) {
				case '.':
					stop++;
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

				case CTL(W):
					re_dosub(linebuf, YES);
					numdone++;
					offset = curchar = REbom;
					makedirty(curline);
					/* Fall into ... */

				case CTL(R):
				case 'R':
					RErecur();
					offset = curchar;
					lp = curline;
					continue;

				case CTL(U):
				case 'U':
					if (UNDO_lp == 0)
						continue;
					lp = UNDO_lp;
					lp->l_dline = UNDO_da | DIRTY;
					offset = 0;
					numdone--;
					continue;

				case 'P':
				case '!':
					query = 0;
					break;

				case CR:
				case LF:
				case 'Q':
					goto done;

				case CTL(L):
					RedrawDisplay();
					goto reswitch;

				default:
					rbell();
message("Space or Y, Period, Rubout or N, C-R or R, C-W, C-U or U, P or !, Return.");
					goto reswitch;
				}
			}
			re_dosub(linebuf, NO);
			numdone++;
			modify();
			offset = curchar = REeom;
			makedirty(curline);
			if (query) {
				message(mesgbuf);	/* No blinking. */
				redisplay();		/* Show the change. */
			}
			UNDO_da = curline->l_dline;
			UNDO_lp = curline;
			if (linebuf[offset] == 0)
nxtline:			break;
		}
	}
	SetMark();
done:	s_mess("%d substitution%n.", numdone, numdone);
}

/* Prompt for search and replacement strings and do the substitution.  The
   point is restored when we're done. */

static
replace(query, inreg)
{
	Mark	*save = MakeMark(curline, curchar, FLOATER),
		*m;
	char	*rep_ptr;
	Line	*l1 = curline,
		*l2 = curbuf->b_last;
	int	char1 = curchar,
		char2 = length(curbuf->b_last);

	if (inreg) {
		m = CurMark();
		l2 = m->m_line;
		char2 = m->m_char;
		(void) fixorder(&l1, &char1, &l2, &char2);
	}

	/* Get search string. */
	strcpy(rep_search, ask(rep_search[0] ? rep_search : (char *) 0, ProcFmt));
	REcompile(rep_search, UseRE, compbuf, alternates);
	/* Now the replacement string.  Do_ask() so the user can play with
	   the default (previous) replacement string by typing C-R in ask(),
	   OR, he can just hit Return to replace with nothing. */
	rep_ptr = do_ask("\r\n", (int (*)()) 0, rep_str, ": %f %s with ", rep_search);
	if (rep_ptr == 0)
		rep_ptr = NullStr;
	strcpy(rep_str, rep_ptr);

	substitute(query, l1, char1, l2, char2);
	ToMark(save);
	DelMark(save);
}

RegReplace()
{
	replace(0, YES);
}

QRepSearch()
{
	replace(1, NO);
}

RepSearch()
{
	replace(0, NO);
}

/* C tags package. */

static
lookup(searchbuf, filebuf, tag, file)
char	*searchbuf,
	*filebuf,
	*tag,
	*file;
{
	register int	taglen = strlen(tag);
	char	line[128],
		pattern[100];
	File	*fp;

	fp = open_file(file, iobuff, F_READ, !COMPLAIN, QUIET);
	if (fp == NIL)
		return 0;
	sprintf(pattern, "^%s[^\t]*\t\\([^\t]*\\)\t[?/]\\(.*\\)[?/]$", tag);
	while (f_gets(fp, line, sizeof line) != EOF) {
		if (line[0] != *tag || strncmp(tag, line, taglen) != 0)
			continue;
		if (!LookingAt(pattern, line, 0)) {
			complain("I thought I saw it!");
			break;
		} else {
			putmatch(2, searchbuf, 100);
			putmatch(1, filebuf, 100);
			close_file(fp);
			return 1;
		}
	}
	f_close(fp);
	s_mess("Can't find tag \"%s\".", tag);
	return 0;
}

char	TagFile[128] = "./tags";

find_tag(tag, localp)
char	*tag;
{
	char	filebuf[FILESIZE],
		sstr[100];
	register Bufpos	*bp;
	register Buffer	*b;
	char	*tagfname;

	if (!localp)
		tagfname = ask(TagFile, "With tag file (%s default): ", TagFile);
	else
		tagfname = TagFile;
	if (lookup(sstr, filebuf, tag, tagfname) == 0)
		return;
	SetMark();
	b = do_find(curwind, filebuf, 0);
	if (curbuf != b)
		SetABuf(curbuf);
	SetBuf(b);
	if ((bp = dosearch(sstr, BACKWARD, 0)) == 0 &&
	    (WrapScan || ((bp = dosearch(sstr, FORWARD, 0)) == 0)))
		message("Well, I found the file, but the tag is missing.");
	else
		SetDot(bp);
}

FindTag()
{
	int	localp = !exp_p;
	char	tag[128];

	strcpy(tag, ask((char *) 0, ProcFmt));
	find_tag(tag, localp);
}

/* Find Tag at Dot. */

FDotTag()
{
	int	c1 = curchar,
		c2 = c1;
	char	tagname[50];

	if (!ismword(linebuf[curchar]))
		complain("Not a tag!");
	while (c1 > 0 && ismword(linebuf[c1 - 1]))
		c1--;
	while (ismword(linebuf[c2]))
		c2++;

	null_ncpy(tagname, linebuf + c1, c2 - c1);
	find_tag(tagname, !exp_p);
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

#define cmp_char(a, b)	((a) == (b) || (CaseIgnore && (Upper(a) == Upper(b))))

static Bufpos *
doisearch(dir, c, failing)
register int	c,
		dir,
		failing;
{
	static Bufpos	buf;
	Bufpos	*bp;

	if (c == CTL(S) || c == CTL(R))
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
dosrch:	if ((bp = dosearch(ISbuf, dir, 0)) == 0)
		rbell();	/* ring the first time there's no match */
	return bp;
}

IncFSearch()
{
	IncSearch(FORWARD);
}

IncRSearch()
{
	IncSearch(BACKWARD);
}

static
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
			DoSetMark(save_env.p_line, save_env.p_char);
	}
	setsearch(ISbuf);
	message(ISbuf);
}

/* Nicely recursive. */

static
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
		switch (c) {
		case RUBOUT:
		case BS:
			return DELETE;

		case CTL(G):
			/* If we're failing, we backup until we're no longer
			   failing or we've reached the beginning; else, we
			   just about the search and go back to the start. */
			if (failing)
				return BACKUP;
			return TOSTART;

		case CTL(\\):
			c = CTL(S);
		case CTL(S):
		case CTL(R):
			/* If this is the first time through and we have a
			   search string left over from last time, use that
			   one now. */
			if (incp == ISbuf) {
				strcpy(ISbuf, getsearch());
				incp = &ISbuf[strlen(ISbuf)];
			}
			ndir = (c == CTL(S)) ? FORWARD : BACKWARD;
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

		case CTL(Q):
		case CTL(^):
			add_mess("");
			c = getch() | 0400;
			/* Fall into ... */

		default:
			if (c & 0400)
				c &= 0177;
			else {
				if (c > RUBOUT || (c < ' ' && c != '\t')) {
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
