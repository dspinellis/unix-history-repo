/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* search package */

#include "jove.h"
#include "re.h"
#include "ctype.h"

private char *insert proto((char *, char *, int));

private void
	REreset proto((void)),
	search proto((int, int, int));
private int
	backref proto((int, char *)),
	do_comp proto((struct RE_block *,int)),
	member proto((char *, int, int)),
	REgetc proto((void)),
	REmatch proto((char *, char *));

char	searchstr[128],		/* global search string */
	rep_search[128],	/* replace search string */
	rep_str[128];		/* contains replacement string */

int	REdirection;		/* current direction we're searching in */

int	CaseIgnore = 0,		/* ignore case? */
	WrapScan = 0,		/* wrap at end of buffer? */
	UseRE = 0;		/* use regular expressions */

#define cind_cmp(a, b)	(CaseEquiv[a] == CaseEquiv[b])

private int	REpeekc;
private char	*REptr;

private int
REgetc()
{
	int	c;

	if ((c = REpeekc) != -1)
		REpeekc = -1;
	else if (*REptr)
		c = *REptr++;
	else
		c = 0;

	return c;
}

#define STAR 	01	/* Match any number of last RE. */
#define AT_BOL	2	/* ^ */
#define AT_EOL	4	/* $ */
#define AT_BOW	6	/* \< */
#define AT_EOW	8	/* \> */
#define OPENP	10	/* \( */
#define CLOSEP	12	/* \) */
#define CURLYB	14	/* \{ */

#define NOSTR	14	/* Codes <= NOSTR can't be *'d. */

#define ANYC	(NOSTR+2)		/* . */
#define NORMC	(ANYC+2)		/* normal character */
#define CINDC	(NORMC+2)		/* case independent character */
#define ONE_OF	(CINDC+2)		/* [xxx] */
#define NONE_OF	(ONE_OF+2)	/* [^xxx] */
#define BACKREF	(NONE_OF+2)	/* \# */
#define EOP	(BACKREF+2)	/* end of pattern */

/* ONE_OF/NONE_OF is represented as a bit vector.
 * These symbols parameterize the representation.
 */

#define	BYTESIZE	8
#define	SETSIZE		(NCHARS / BYTESIZE)
#define	SETBYTE(c)	((c) / BYTESIZE)
#define	SETBIT(c)	(1 << ((c) % BYTESIZE))

#define NPAR	10	/* [0-9] - 0th is the entire matched string, i.e. & */
private char	*comp_ptr,
		**alt_p,
		**alt_endp;

void
REcompile(pattern, re, re_blk)
char	*pattern;
int	re;
struct RE_block	*re_blk;
{
	REptr = pattern;
	REpeekc = -1;
	comp_ptr = re_blk->r_compbuf;
	alt_p = re_blk->r_alternates;
	alt_endp = alt_p + NALTS;
	*alt_p++ = comp_ptr;
	re_blk->r_nparens = 0;
	(void) do_comp(re_blk, re ? OKAY_RE : NORM);
	*alt_p = NULL;

	re_blk->r_anchored = NO;
	re_blk->r_firstc = '\0';
	/* do a little post processing */
	if (re_blk->r_alternates[1] == NULL) {
		char	*p;

		p = re_blk->r_alternates[0];
		for (;;) {
			switch (*p) {
			case OPENP:
			case CLOSEP:
				p += 2;
				continue;

			case AT_BOW:
			case AT_EOW:
				p += 1;
				continue;

			case AT_BOL:
				re_blk->r_anchored = YES;
				/* don't set firstc -- won't work */
				break;

			case NORMC:
			case CINDC:
				re_blk->r_firstc = CaseEquiv[p[2]];
				break;

			default:
				break;
			}
			break;
		}
	}
}

/* compile the pattern into an internal code */

private int
do_comp(re_blk, kind)
struct RE_block	*re_blk;
int	kind;
{
	char	*this_verb,
		*prev_verb,
		*start_p,
		*comp_endp;
	int	parens[NPAR],
		*parenp,
		c,
		ret_code;

	parenp = parens;
	this_verb = NULL;
	ret_code = 1;
	comp_endp = &re_blk->r_compbuf[COMPSIZE - 6];

	/* wrap the whole expression around (implied) parens */
	if (kind == OKAY_RE) {
		*comp_ptr++ = OPENP;
		*comp_ptr++ = re_blk->r_nparens;
		*parenp++ = re_blk->r_nparens++;
	}

	start_p = comp_ptr;

	while ((c = REgetc()) != '\0') {
		if (comp_ptr > comp_endp)
toolong:		complain("Search string too long/complex.");
		prev_verb = this_verb;
		this_verb = comp_ptr;

		if (kind == NORM && strchr(".[*", c) != 0)
			goto defchar;
		switch (c) {
		case '\\':
			switch (c = REgetc()) {
			case 0:
				complain("[Premature end of pattern]");
				/*NOTREACHED*/

			case '{':
			    {
				char	*wcntp;		/* word count */

				*comp_ptr++ = CURLYB;
				wcntp = comp_ptr;
				*comp_ptr++ = 0;
				for (;;) {
					int	comp_val;
					char	*comp_len;

					comp_len = comp_ptr++;
					comp_val = do_comp(re_blk, IN_CB);
					*comp_len = comp_ptr - comp_len;
					(*wcntp) += 1;
					if (comp_val == 0)
						break;
				}
				break;
			    }

			case '}':
				if (kind != IN_CB)
					complain("Unexpected \\}.");
				ret_code = 0;
				goto outahere;

			case '(':
				if (re_blk->r_nparens >= NPAR)
					complain("Too many ('s; max is %d.", NPAR);
				*comp_ptr++ = OPENP;
				*comp_ptr++ = re_blk->r_nparens;
				*parenp++ = re_blk->r_nparens++;
				break;

			case ')':
				if (parenp == parens)
					complain("Too many )'s.");
				*comp_ptr++ = CLOSEP;
				*comp_ptr++ = *--parenp;
				break;

			case '|':
				if (alt_p >= alt_endp)
					complain("Too many alternates; max %d.", NALTS);
				/* close off previous alternate */
				*comp_ptr++ = CLOSEP;
				*comp_ptr++ = *--parenp;
				*comp_ptr++ = EOP;
				*alt_p++ = comp_ptr;

				/* start a new one */
				re_blk->r_nparens = 0;
				*comp_ptr++ = OPENP;
				*comp_ptr++ = re_blk->r_nparens;
				*parenp++ = re_blk->r_nparens++;
				start_p = comp_ptr;
				break;

			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				*comp_ptr++ = BACKREF;
				*comp_ptr++ = c - '0';
				break;

			case '<':
				*comp_ptr++ = AT_BOW;
				break;

			case '>':
				*comp_ptr++ = AT_EOW;
				break;

			default:
				goto defchar;
			}
			break;

		case ',':
			if (kind != IN_CB)
				goto defchar;
			goto outahere;

		case '.':
			*comp_ptr++ = ANYC;
			break;

		case '^':
			if (comp_ptr == start_p) {
				*comp_ptr++ = AT_BOL;
				break;
			}
			goto defchar;

		case '$':
			if ((REpeekc = REgetc()) != 0 && REpeekc != '\\')
				goto defchar;
			*comp_ptr++ = AT_EOL;
			break;

		case '[':
		    {
			int	chrcnt;

			*comp_ptr++ = ONE_OF;
			if (comp_ptr + SETSIZE >= comp_endp)
				goto toolong;
			byte_zero(comp_ptr, (size_t) SETSIZE);
			if ((REpeekc = REgetc()) == '^') {
				*this_verb = NONE_OF;
				/* Get it for real this time. */
				(void) REgetc();
			}
			chrcnt = 0;
			while ((c = REgetc()) != ']' && c != 0) {
				if (c == '\\') {
					c = REgetc();
					if (c == 0)
						break;
				} else if ((REpeekc = REgetc()) == '-') {
					int	i;

					i = c;
					(void) REgetc();     /* reread '-' */
					c = REgetc();
					if (c == 0)
						break;
					while (i < c) {
						comp_ptr[SETBYTE(i)] |= SETBIT(i);
						i += 1;
					}
				}
				comp_ptr[SETBYTE(c)] |= SETBIT(c);
				chrcnt += 1;
			}
			if (c == 0)
				complain("Missing ].");
			if (chrcnt == 0)
				complain("Empty [].");
			comp_ptr += SETSIZE;
			break;
		    }

		case '*':
			if (prev_verb == NULL || *prev_verb <= NOSTR || (*prev_verb&STAR)!=0)
				goto defchar;

			if (*prev_verb == NORMC || *prev_verb == CINDC) {
				char	lastc = comp_ptr[-1];

				/* The * operator applies only to the
				 * previous character.  Since we were
				 * building a string-matching command
				 * (NORMC or CINDC), we must split it
				 * up and work with the last character.
				 *
				 * Note that the STARed versions of these
				 * commands do not operate on strings, and
				 * so do not need or have character counts.
				 */

 				if (prev_verb[1] == 1) {
					/* Only one char in string:
					 * delete old command.
					 */
					this_verb = prev_verb;
 				} else {
					/* Several chars in string:
					 * strip off the last.
					 * New verb is derived from old.
					 */
					prev_verb[1] -= 1;
					this_verb -= 1;
					*this_verb = *prev_verb;
 				}
				comp_ptr = this_verb + 1;
				*comp_ptr++ = lastc;
			} else {
				/* This command is just the previous one,
				 * whose verb we will modify.
				 */
				this_verb = prev_verb;
			}
			*this_verb |= STAR;
			break;
		default:
defchar:
			if ((prev_verb == NULL) ||
			    !(*prev_verb == NORMC || *prev_verb == CINDC)) {
				/* create new string command */
				*comp_ptr++ = (CaseIgnore) ? CINDC : NORMC;
				*comp_ptr++ = 0;
			} else {
				/* merge this into previous string command */
				this_verb = prev_verb;
			}
			this_verb[1] += 1;
			*comp_ptr++ = c;
			break;
		}
	}
outahere:

	/* End of pattern, let's do some error checking. */
	if (kind == OKAY_RE) {
		*comp_ptr++ = CLOSEP;
		*comp_ptr++ = *--parenp;
	}
	if (parenp != parens)
		complain("Unmatched ()'s.");
	if (kind == IN_CB && c == 0)	/* end of pattern with missing \}. */
		complain("Missing \\}.");
	*comp_ptr++ = EOP;

	return ret_code;
}

private char	*pstrtlst[NPAR],	/* index into re_blk->r_lbuf */
		*pendlst[NPAR],
		*REbolp,	/* begining-of-line pointer */
		*locrater,	/* roof of last substitution */
		*loc1,	/* start of matched text */
		*loc2;	/* roof of matched text */

int	REbom,		/* beginning and end columns of match */
	REeom,
	REdelta;	/* increase in line length due to last re_dosub */

private int
backref(n, linep)
int	n;
register char	*linep;
{
	register char	*backsp,
			*backep;

	backsp = pstrtlst[n];
	backep = pendlst[n];
	while (*backsp++ == *linep++)
		if (backsp >= backep)
			return 1;
	return 0;
}

private int
member(comp_ptr, c, af)
register char	*comp_ptr;
register int	c,
		af;
{
	if (c == 0)
		return 0;	/* try to match EOL always fails */
	if (comp_ptr[SETBYTE(c)] & SETBIT(c))
		return af;
	return !af;
}

private int
REmatch(linep, comp_ptr)
register char	*linep,
		*comp_ptr;
{
	char	*first_p;
	register int	n;

	for (;;) switch (*comp_ptr++) {
	case NORMC:
		n = *comp_ptr++;
		while (--n >= 0)
			if (*linep++ != *comp_ptr++)
				return NO;
		continue;

	case CINDC:	/* case independent comparison */
		n = *comp_ptr++;
		while (--n >= 0)
			if (!cind_cmp(*linep++, *comp_ptr++))
				return NO;
		continue;

	case EOP:
		loc2 = linep;
		REeom = (loc2 - REbolp);
		return YES;	/* Success! */

	case AT_BOL:
		if (linep == REbolp && linep != locrater)
			continue;
		return NO;

	case AT_EOL:
		if (*linep == '\0')
			continue;
		return NO;

	case ANYC:
		if (*linep++ != 0)
			continue;
		return NO;

	case AT_BOW:
		if (linep != locrater && ismword(*linep)
		&& (linep == REbolp || !ismword(linep[-1])))
			continue;
		return NO;

	case AT_EOW:
		if (linep != locrater && (*linep == 0 || !ismword(*linep)) &&
		    (linep != REbolp && ismword(linep[-1])))
			continue;
		return NO;

	case ONE_OF:
	case NONE_OF:
		if (member(comp_ptr, *linep++, comp_ptr[-1] == ONE_OF)) {
			comp_ptr += SETSIZE;
			continue;
		}
		return NO;

	case OPENP:
		pstrtlst[*comp_ptr++] = linep;
		continue;

	case CLOSEP:
		pendlst[*comp_ptr++] = linep;
		continue;

	case BACKREF:
		if (pstrtlst[n = *comp_ptr++] == 0) {
			s_mess("\\%d was not specified.", n + 1);
			return NO;
		}
		if (backref(n, linep)) {
			linep += pendlst[n] - pstrtlst[n];
			continue;
		}
		return NO;

	case CURLYB:
	    {
		int	wcnt,
			any;

		wcnt = *comp_ptr++;
		any = 0;

		while (--wcnt >= 0) {
			if (any == 0)
				any = REmatch(linep, comp_ptr + 1);
			comp_ptr += *comp_ptr;
		}
		if (any == 0)
			return NO;
		linep = loc2;
		continue;
	    }

	case ANYC | STAR:
		first_p = linep;
		while (*linep++)
			;
		goto star;

	case NORMC | STAR:
		first_p = linep;
		while (*comp_ptr == *linep++)
			;
		comp_ptr += 1;
		goto star;

	case CINDC | STAR:
		first_p = linep;
		while (cind_cmp(*comp_ptr, *linep++))
			;
		comp_ptr += 1;
		goto star;

	case ONE_OF | STAR:
	case NONE_OF | STAR:
		first_p = linep;
		while (member(comp_ptr, *linep++, comp_ptr[-1] == (ONE_OF | STAR)))
			;
		comp_ptr += SETSIZE;
		/* fall through */
star:
		/* linep points *after* first unmatched char.
		 * first_p points at where starred element started matching.
		 */
		while (--linep > first_p) {
			if ((*comp_ptr != NORMC || *linep == comp_ptr[2]) &&
			    REmatch(linep, comp_ptr))
				return YES;
		}
		continue;

	case BACKREF | STAR:
		first_p = linep;
		n = *comp_ptr++;
		while (backref(n, linep))
			linep += pendlst[n] - pstrtlst[n];
		while (linep > first_p) {
			if (REmatch(linep, comp_ptr))
				return YES;
			linep -= pendlst[n] - pstrtlst[n];
		}
		continue;

	default:
		complain("RE error match (%d).", comp_ptr[-1]);
	}
	/* NOTREACHED */
}

private void
REreset()
{
	register int	i;

	for (i = 0; i < NPAR; i++)
		pstrtlst[i] = pendlst[i] = 0;
}

/* Index LINE at OFFSET.  If lbuf_okay is nonzero it's okay to use linebuf
   if LINE is the current line.  This should save lots of time in things
   like paren matching in LISP mode.  Saves all that copying from linebuf
   to a local buffer.  substitute() is the guy who calls re_lindex with
   lbuf_okay as 0, since the substitution gets placed in linebuf ...
   doesn't work too well when the source and destination strings are the
   same.  I hate all these arguments!

   This code is cumbersome, repetetive for reasons of efficiency.  Fast
   search is a must as far as I am concerned. */

int
re_lindex(line, offset, re_blk, lbuf_okay, crater)
Line	*line;
int	offset;
struct RE_block	*re_blk;
int	lbuf_okay;
int	crater;	/* offset of previous substitute (or -1) */
{
	register char	*p;
	register int	firstc = re_blk->r_firstc;
	register int	anchored = re_blk->r_anchored;
	int		re_dir = REdirection;
	char		**alts = re_blk->r_alternates;

	REreset();
	if (lbuf_okay) {
		REbolp = lbptr(line);
		if (offset == -1)
			offset = strlen(REbolp);	/* arg! */
	} else {
		REbolp = ltobuf(line, re_blk->r_lbuf);
		if (offset == -1) {	/* Reverse search, find end of line. */
			offset = Jr_Len;	/* Just Read Len. */
		}
	}

	if (anchored == YES) {
		if (re_dir == FORWARD) {
			if (offset != 0 || crater != -1)
				return NO;
		} else
			offset = 0;
	}

	p = REbolp + offset;
	locrater = REbolp + crater;

	if (firstc != '\0') {
		char	*first_alt = *alts;

		if (re_dir == FORWARD) {
			while (CaseEquiv[*p] != firstc || !REmatch(p, first_alt))
				if (*p++ == '\0')
					return NO;
		} else {
			while (CaseEquiv[*p] != firstc || !REmatch(p, first_alt))
				if (--p < REbolp)
					return NO;
		}
	} else {
		for (;;) {
			register char	**altp = alts;

			while (*altp != NULL)
				if (REmatch(p, *altp++))
					goto success;
			if (anchored ||
			    (re_dir == FORWARD ? *p++ == '\0' : --p < REbolp))
				return NO;
		}
success:;
	}
	loc1 = p;
	REbom = loc1 - REbolp;

	return YES;
}

int	okay_wrap = 0;	/* Do a wrap search ... not when we're
			   parsing errors ... */

Bufpos *
dosearch(pattern, dir, re)
char	*pattern;
int dir,
    re;
{
	Bufpos	*pos;
	struct RE_block	re_blk;		/* global re-compiled buffer */

	if (bobp() && eobp())	/* Can't match!  There's no buffer. */
		return 0;

	REcompile(pattern, re, &re_blk);

	pos = docompiled(dir, &re_blk);
	return pos;
}

Bufpos *
docompiled(dir, re_blk)
int dir;
register struct RE_block	*re_blk;
{
	static Bufpos	ret;
	register Line	*lp;
	register int	offset;
	int	we_wrapped = NO;

	lsave();
	/* Search now lsave()'s so it doesn't make any assumptions on
	   whether the the contents of curline/curchar are in linebuf.
	   Nowhere does search write all over linebuf.  However, we have to
	   be careful about what calls we make here, because many of them
	   assume (and rightly so) that curline is in linebuf. */

	REdirection = dir;
	lp = curline;
	offset = curchar;
	if (dir == BACKWARD) {
		if (bobp()) {
			if (okay_wrap && WrapScan)
				goto doit;
			return 0;
		}
		/* here we simulate BackChar() */
		if (bolp()) {
			lp = lp->l_prev;
			offset = length(lp);
		} else
			offset -= 1;
	} else if ((dir == FORWARD) &&
		   (lbptr(lp)[offset] == '\0') &&
		   !lastp(lp)) {
		lp = lp->l_next;
		offset = 0;
	}

	do {
		if (re_lindex(lp, offset, re_blk, YES, -1))
			break;
doit:		lp = (dir == FORWARD) ? lp->l_next : lp->l_prev;
		if (lp == 0) {
			if (okay_wrap && WrapScan) {
				lp = (dir == FORWARD) ?
				     curbuf->b_first : curbuf->b_last;
				we_wrapped = YES;
			} else
				 break;
		}
		if (dir == FORWARD)
			offset = 0;
		else
			offset = -1;	/* signals re_lindex ... */
	} while (lp != curline);

	if (lp == curline && we_wrapped)
		lp = 0;
	if (lp == 0)
		return 0;
	ret.p_line = lp;
	ret.p_char = (dir == FORWARD) ? REeom : REbom;
	return &ret;
}

private char *
insert(off, endp, which)
char	*off,
	*endp;
int which;
{
	register char	*pp;
	register int	n;

	n = pendlst[which] - pstrtlst[which];
	pp = pstrtlst[which];
	while (--n >= 0) {
		*off++ = *pp++;
		if (off >= endp)
			len_error(ERROR);
	}
	return off;
}

/* Perform the substitution.  If DELP is nonzero the matched string is
   deleted, i.e., the substitution string is not inserted. */

void
re_dosub(re_blk, tobuf, delp)
struct RE_block	*re_blk;
char	*tobuf;
int delp;
{
	register char	*tp,
			*rp;
	char	*endp;

	tp = tobuf;
	endp = tp + LBSIZE;
	rp = re_blk->r_lbuf;

	while (rp < loc1)
		*tp++ = *rp++;

	if (!delp) {
		register int	c;

		rp = rep_str;
		while ((c = *rp++) != '\0') {
			if (c == '\\') {
				c = *rp++;
				if (c >= '0' && c < re_blk->r_nparens + '0') {
					tp = insert(tp, endp, c - '0');
					continue;
				}
				if (c == '\0') {
					*tp++ = '\\';
					rp--;   /* be sure to hit again */
				}
			}
			*tp++ = c;
			if (tp >= endp)
				len_error(ERROR);
		}
	}
	rp = loc2;
	REdelta = -REeom;
	REeom = tp - tobuf;
	REdelta += REeom;
	if (loc1==rp && *rp!='\0') {
		/* Skip an extra character if the matched text was a null
		 * string, but don't skip over the end of line.  This is to
		 * prevent an infinite number of replacements in the same
		 * position, e.g., replace "^" with "".
		 */
		REeom += 1;
	}
	loc2 = re_blk->r_lbuf + REeom;
	while ((*tp++ = *rp++) != '\0')
		if (tp >= endp)
			len_error(ERROR);
}

void
putmatch(which, buf, size)
int which;
char	*buf;
size_t size;
{
	*(insert(buf, buf + size, which)) = '\0';
}

void
setsearch(str)
char	*str;
{
	strcpy(searchstr, str);
}

char *
getsearch()
{
	return searchstr;
}

void
RErecur()
{
	char	repbuf[sizeof rep_str];
	Mark	*m = MakeMark(curline, REbom, M_FLOATER);

	message("Type C-X C-C to continue with query replace.");

	byte_copy(rep_str, repbuf, sizeof rep_str);
	Recur();
	byte_copy(repbuf, rep_str, sizeof rep_str);
	if (!is_an_arg())
		ToMark(m);
	DelMark(m);
}

void
ForSearch()
{
	search(FORWARD, UseRE, YES);
}

void
RevSearch()
{
	search(BACKWARD, UseRE, YES);
}

void
FSrchND()
{
	search(FORWARD, UseRE, NO);
}

void
RSrchND()
{
	search(BACKWARD, UseRE, NO);
}

private void
search(dir, re, setdefault)
int dir,
    re,
    setdefault;
{
	Bufpos	*newdot;
	char	*s;

	s = ask(searchstr, ProcFmt);
	if (setdefault)
		setsearch(s);
	okay_wrap = YES;
	newdot = dosearch(s, dir, re);
	okay_wrap = NO;
	if (newdot == 0) {
		if (WrapScan)
			complain("No \"%s\" in buffer.", s);
		else
			complain("No \"%s\" found to %s.", s,
				 (dir == FORWARD) ? "bottom" : "top");
	}
	PushPntp(newdot->p_line);
	SetDot(newdot);
}

/* Do we match PATTERN at OFFSET in BUF? */

int
LookingAt(pattern, buf, offset)
char	*pattern,
	*buf;
int offset;
{
	struct RE_block	re_blk;
	char	**alt = re_blk.r_alternates;

	REcompile(pattern, YES, &re_blk);
	REreset();
	locrater = NULL;
	REbolp = buf;

	while (*alt)
		if (REmatch(buf + offset, *alt++))
			return YES;
	return NO;
}

int
look_at(expr)
char	*expr;
{
	struct RE_block	re_blk;

	REcompile(expr, 0, &re_blk);
	REreset();
	locrater = NULL;
	REbolp = linebuf;
	if (REmatch(linebuf + curchar, re_blk.r_alternates[0]))
		return YES;
	return NO;
}
