/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* search package */

#include "jove.h"
#include "ctype.h"
#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private char * insert(char *, char *, int);

private void
	REreset(void),
	search(int, int, int);
private int
	backref(int, char *),
	do_comp(int),
	member(char *, int, int),
	REgetc(void),
	REmatch(char *, char *);
#else
private char * insert();

private void
	REreset(),
	search();
private int
	backref(),
	do_comp(),
	member(),
	REgetc(),
	REmatch();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif

#define NALTS	16	/* number of alternate search strings */

char	searchstr[128],
	compbuf[256],		/* global default compbuf */
	rep_search[128],	/* replace search string */
	rep_str[128],		/* contains replacement string */
	*cur_compb,		/* usually points at compbuf */
	REbuf[LBSIZE],		/* points at line we're scanning */
	*alternates[NALTS];

int	REdirection;

int	CaseIgnore = 0,
	WrapScan = 0,
	UseRE = 0;

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

#define ANYC	NOSTR+2		/* . */
#define NORMC	ANYC+2		/* normal character */
#define CINDC	NORMC+2		/* case independent character */
#define ONE_OF	CINDC+2		/* [xxx] */
#define NONE_OF	ONE_OF+2	/* [^xxx] */
#define BACKREF	NONE_OF+2	/* \# */
#define EOP	BACKREF+2	/* end of pattern */

#define NPAR	10	/* [0-9] - 0th is the entire matched string, i.e. & */
private int	nparens;
private char	*comp_p,
		*start_p,
		**alt_p,
		**alt_endp;

void
REcompile(pattern, re, into_buf, alt_bufp)
char	*pattern,
	*into_buf,
	**alt_bufp;
{
	REptr = pattern;
	REpeekc = -1;
	comp_p = cur_compb = start_p = into_buf;
	alt_p = alt_bufp;
	alt_endp = alt_p + NALTS;
	*alt_p++ = comp_p;
	nparens = 0;
	(void) do_comp(re ? OKAY_RE : NORM);
	*alt_p = 0;
}

/* compile the pattern into an internal code */

private int
do_comp(kind)
{
	char	*last_p,
		*chr_cnt = 0;
	int	parens[NPAR],
		*parenp,
		c,
		ret_code;

	parenp = parens;
	last_p = 0;
	ret_code = 1;

	if (kind == OKAY_RE) {
		*comp_p++ = OPENP;
		*comp_p++ = nparens;
		*parenp++ = nparens++;
		start_p = comp_p;
	}

	while (c = REgetc()) {
		if (comp_p > &cur_compb[(sizeof compbuf) - 6])
toolong:		complain("Search string too long/complex.");
		if (c != '*')
			last_p = comp_p;

		if (kind == NORM && index(".[*", c) != 0)
			goto defchar;
		switch (c) {
		case '\\':
			switch (c = REgetc()) {
			case 0:
				complain("Premature end of pattern.");

			case '{':
			    {
			    	char	*wcntp;		/* word count */

			    	*comp_p++ = CURLYB;
			    	wcntp = comp_p;
			    	*comp_p++ = 0;
			    	for (;;) {
			    		int	comp_val;
			    		char	*comp_len;

			    		comp_len = comp_p++;
			    		comp_val = do_comp(IN_CB);
			    		*comp_len = comp_p - comp_len;
			    		(*wcntp) += 1;
			    		if (comp_val == 0)
			    			break;
			    	}
			    	break;
			    }

			case '}':
				if (kind != IN_CB)
					complain("Unexpected \}.");
				ret_code = 0;
				goto outahere;

			case '(':
				if (nparens >= NPAR)
					complain("Too many ('s; max is %d.", NPAR);
				*comp_p++ = OPENP;
				*comp_p++ = nparens;
				*parenp++ = nparens++;
				break;

			case ')':
				if (parenp == parens)
					complain("Too many )'s.");
				*comp_p++ = CLOSEP;
				*comp_p++ = *--parenp;
				break;

			case '|':
				if (alt_p >= alt_endp)
					complain("Too many alternates; max %d.", NALTS);
				*comp_p++ = CLOSEP;
				*comp_p++ = *--parenp;
				*comp_p++ = EOP;
				*alt_p++ = comp_p;
				nparens = 0;
				*comp_p++ = OPENP;
				*comp_p++ = nparens;
				*parenp++ = nparens++;
				start_p = comp_p;
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
				*comp_p++ = BACKREF;
				*comp_p++ = c - '0';
				break;

			case '<':
				*comp_p++ = AT_BOW;
				break;

			case '>':
				*comp_p++ = AT_EOW;
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
			*comp_p++ = ANYC;
			break;

		case '^':
			if (comp_p == start_p) {
				*comp_p++ = AT_BOL;
				break;
			}
			goto defchar;

		case '$':
			if ((REpeekc = REgetc()) != 0 && REpeekc != '\\')
				goto defchar;
			*comp_p++ = AT_EOL;
			break;

		case '[':
		    {
		    	int	chrcnt;

		    	*comp_p++ = ONE_OF;
			if (comp_p + 16 >= &cur_compb[(sizeof compbuf)])
				goto toolong;
		    	bzero(comp_p, 16);
		    	if ((REpeekc = REgetc()) == '^') {
		    		*last_p = NONE_OF;
		    		/* Get it for real this time. */
		    		(void) REgetc();
		    	}
		    	chrcnt = 1;
		    	while ((c = REgetc()) != ']' && c != 0) {
		    		if (c == '\\')
		    			c = REgetc();
				else if ((REpeekc = REgetc()) == '-') {
					int	c2;

					(void) REgetc();     /* reread '-' */
					c2 = REgetc();
					while (c < c2) {
						comp_p[c/8] |= (1 << (c%8));
						c += 1;
					}
				}
				comp_p[c/8] |= (1 << (c%8));
		    		chrcnt += 1;
		    	}
		    	if (c == 0)
		    		complain("Missing ].");
		    	if (chrcnt == 1)
		    		complain("Empty [].");
		    	comp_p += 16;
		    	break;
		    }

		case '*':
			if (last_p == 0 || *last_p <= NOSTR)
				goto defchar;

			/* The * operator applies only to the previous
			   character.  If we were building a chr_cnt at
			   the time we got the *, we have to remove the
			   last character from the chr_cnt (by decrementing
			   *chr_cnt) and replacing it with a new STAR entry.

			   If we are decrementing the count to 0, we just
			   delete the chr_cnt entry altogether, replacing
			   it with the STAR entry. */

			if (chr_cnt) {
				char	lastc = chr_cnt[*chr_cnt];
 
 			/* The * operator applies only to the previous
 			   character.  If we were building a chr_cnt at
 			   the time we got the *, we have to remove the
 			   last character from the chr_cnt (by decrementing
 			   *chr_cnt) and replacing it with a new STAR entry.
 
 			   If we are decrementing the count to 0, we just
 			   delete the chr_cnt entry altogether, replacing
 			   it with the STAR entry. */
 
 				if (*chr_cnt == 1) {
 					comp_p = chr_cnt;
 					comp_p[-1] |= STAR;
 					*comp_p++ = lastc;
 				} else {
 					comp_p = chr_cnt + *chr_cnt;
 					(*chr_cnt) -= 1;
 					*comp_p++ = chr_cnt[-1] | STAR;
 					*comp_p++ = lastc;
 				}
			} else
				*last_p |= STAR;
			break;
		default:
defchar:		if (chr_cnt)
				(*chr_cnt) += 1;
			else {
				*comp_p++ = (CaseIgnore) ? CINDC : NORMC;
				chr_cnt = comp_p++;
				*chr_cnt = 1;   /* last_p[1] = 1; */
			}
			*comp_p++ = c;
			continue;
		}
		chr_cnt = FALSE;
	}
outahere:
	/* End of pattern, let's do some error checking. */
	if (kind == OKAY_RE) {
		*comp_p++ = CLOSEP;
		*comp_p++ = *--parenp;
	}
	if (parenp != parens)
		complain("Unmatched ()'s.");
	if (kind == IN_CB && c == 0)	/* End of pattern with \}. */
		complain("Missing \}.");
	*comp_p++ = EOP;

	return ret_code;
}

private char	*pstrtlst[NPAR],	/* index into REbuf */
		*pendlst[NPAR],
		*REbolp,
		*locs,
		*loc1,
		*loc2;

int	REbom,
	REeom,		/* beginning and end of match */
	REalt_num;	/* if alternatives, which one matched? */

private int
backref(n, linep)
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
member(comp_p, c, af)
register char	*comp_p;
register int	c,
		af;
{
	if (c == 0)
		return 0;	/* try to match EOL always fails */
	if (comp_p[c/8] & (1 << (c%8)))
		return af;
	return !af;
}

private int
REmatch(linep, comp_p)
register char	*linep,
		*comp_p;
{
	char	*first_p = linep;
	register int	n;

	for (;;) switch (*comp_p++) {
	case NORMC:
		n = *comp_p++;
		while (--n >= 0)
			if (*linep++ != *comp_p++)
				return 0;
		continue;

	case CINDC:	/* case independent comparison */
		n = *comp_p++;
		while (--n >= 0)
			if (!cind_cmp(*linep++, *comp_p++))
				return 0;
		continue;

	case EOP:
		loc2 = linep;
		REeom = (loc2 - REbolp);
		return 1;	/* Success! */

	case AT_BOL:
		if (linep == REbolp)
			continue;
		return 0;

	case AT_EOL:
		if (*linep == 0)
			continue;
		return 0;

	case ANYC:
		if (*linep++ != 0)
			continue;
		return 0;

	case AT_BOW:
		if (ismword(*linep) && (linep == REbolp || !ismword(linep[-1])))
			continue;
		return 0;

	case AT_EOW:
		if ((*linep == 0 || !ismword(*linep)) &&
		    (linep != REbolp && ismword(linep[-1])))
			continue;
		return 0;

	case ONE_OF:
	case NONE_OF:
		if (member(comp_p, *linep++, comp_p[-1] == ONE_OF)) {
			comp_p += 16;
			continue;
		}
		return 0;

	case OPENP:
		pstrtlst[*comp_p++] = linep;
		continue;

	case CLOSEP:
		pendlst[*comp_p++] = linep;
		continue;

	case BACKREF:
		if (pstrtlst[n = *comp_p++] == 0) {
			s_mess("\\%d was not specified.", n + 1);
			return 0;
		}
		if (backref(n, linep)) {
			linep += pendlst[n] - pstrtlst[n];
			continue;
		}
		return 0;

	case CURLYB:
	    {
	    	int	wcnt,
	    		any;

	    	wcnt = *comp_p++;
	    	any = 0;

	    	while (--wcnt >= 0) {
	    		if (any == 0)
	    			any = REmatch(linep, comp_p + 1);
	    		comp_p += *comp_p;
	    	}
	    	if (any == 0)
	    		return 0;
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
		while (*comp_p == *linep++)
			;
		comp_p += 1;
		goto star;

	case CINDC | STAR:
		first_p = linep;
		while (cind_cmp(*comp_p, *linep++))
			;
		comp_p += 1;
		goto star;

	case ONE_OF | STAR:
	case NONE_OF | STAR:
		first_p = linep;
		while (member(comp_p, *linep++, comp_p[-1] == (ONE_OF | STAR)))
			;
		comp_p += 16;
		goto star;

	case BACKREF | STAR:
		first_p = linep;
		n = *comp_p++;
		while (backref(n, linep))
			linep += pendlst[n] - pstrtlst[n];
		while (linep >= first_p) {
			if (REmatch(linep, comp_p))
				return 1;
			linep -= pendlst[n] - pstrtlst[n];
		}
		continue;

star:		do {
			linep -= 1;
			if (linep < locs)
				break;
			if (REmatch(linep, comp_p))
				return 1;
		} while (linep > first_p);
		return 0;

	default:
		complain("RE error match (%d).", comp_p[-1]);
	}
	/* NOTREACHED. */
}

private void
REreset()
{
	register int	i;

	for (i = 0; i < NPAR; i++)
		pstrtlst[i] = pendlst[i] = 0;
}

/* Index LINE at OFFSET, the compiled EXPR, with alternates ALTS.  If
   lbuf_okay is nonzero it's okay to use linebuf if LINE is the current
   line.  This should save lots of time in things like paren matching in
   LISP mode.  Saves all that copying from linebuf to REbuf.  substitute()
   is the guy who calls re_lindex with lbuf_okay as 0, since the substitution
   gets placed in linebuf ... doesn't work too well when the source and
   destination strings are the same.  I hate all these arguments!

   This code is cumbersome, repetetive for reasons of efficiency.  Fast
   search is a must as far as I am concerned. */

int
re_lindex(line, offset, expr, alts, lbuf_okay)
Line	*line;
char	*expr,
	**alts;
{
	int	isquick;
	register int	firstc,
			c;
	register char	*resp;

	REreset();
	if (lbuf_okay) {
		REbolp = lbptr(line);
		if (offset == -1)
			offset = strlen(REbolp);	/* arg! */
	} else {
		REbolp = ltobuf(line, REbuf);
		if (offset == -1) {	/* Reverse search, find end of line. */
			extern int	Jr_Len;

			offset = Jr_Len;	/* Just Read Len. */
		}
	}
	resp = REbolp;
	isquick = ((expr[0] == NORMC || expr[0] == CINDC) &&
		   (alternates[1] == 0));
	if (isquick) {
		firstc = expr[2];
		if (expr[0] == CINDC)
			firstc = CaseEquiv[firstc];
	}
	locs = REbolp + offset;

	if (REdirection == FORWARD) {
	    do {
		char	**altp = alts;

		if (isquick) {
			if (expr[0] == NORMC)
				while ((c = *locs++) != 0 && c != firstc)
					;
			else
				while (((c = *locs++) != 0) &&
					(CaseEquiv[c] != firstc))
					;
			if (*--locs == 0)
				break;
		}
		REalt_num = 1;
		while (*altp) {
			if (REmatch(locs, *altp++)) {
				loc1 = locs;
				REbom = loc1 - REbolp;
				return 1;
			}
			REalt_num += 1;
		}
	    } while (*locs++);
	} else {
	    do {
		char	**altp = alts;

		if (isquick) {
			if (expr[0] == NORMC) {
				while (locs >= REbolp && *locs-- != firstc)
					;
				if (*++locs != firstc)
					break;
			} else {
				while (locs >= REbolp && CaseEquiv[*locs--] != firstc)
					;
				if (CaseEquiv[*++locs] != firstc)
					break;
			}
		}
		REalt_num = 1;
		while (*altp) {
			if (REmatch(locs, *altp++)) {
				loc1 = locs;
				REbom = loc1 - REbolp;
				return 1;
			}
			REalt_num += 1;
		}
	    } while (--locs >= resp);
	}

	return 0;
}

int	okay_wrap = 0;	/* Do a wrap search ... not when we're
			   parsing errors ... */

Bufpos *
dosearch(pattern, dir, re)
char	*pattern;
{
	Bufpos	*pos;

	if (bobp() && eobp())	/* Can't match!  There's no buffer. */
		return 0;

	REcompile(pattern, re, compbuf, alternates);

	pos = docompiled(dir, compbuf, alternates);
	return pos;
}

Bufpos *
docompiled(dir, expr, alts)
char	*expr,
	**alts;
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
			offset = strlen(lbptr(lp));
		} else
			offset -= 1;
	} else if ((dir == FORWARD) &&
		   (lbptr(lp)[offset] == '\0') &&
		   !lastp(lp)) {
		lp = lp->l_next;
		offset = 0;
	}

	do {
		if (re_lindex(lp, offset, expr, alts, YES))
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
re_dosub(tobuf, delp)
char	*tobuf;
{
	register char	*tp,
			*rp,
			*repp;
	int	c;
	char	*endp;

	tp = tobuf;
	endp = tp + LBSIZE;
	rp = REbuf;
	repp = rep_str;

	while (rp < loc1)
		*tp++ = *rp++;

	if (!delp) while (c = *repp++) {
		if (c == '\\') {
			c = *repp++;
			if (c == '\0') {
				*tp++ = '\\';
	  			goto endchk;
			} else if (c >= '1' && c <= nparens + '1') {
				tp = insert(tp, endp, c - '0');
				continue;
			}
		} else if (c == '&') {
			tp = insert(tp, endp, 0);
			continue;
		}
		*tp++ = c;
endchk:		if (tp >= endp)
			len_error(ERROR);
	}
	rp = loc2;
	loc2 = REbuf + max(1, tp - tobuf);
	REeom = loc2 - REbuf;
	/* At least one character past the match, to prevent an infinite
	   number of replacements in the same position, e.g.,
	   replace "^" with "". */
	while (*tp++ = *rp++)
		if (tp >= endp)
			len_error(ERROR);
}

void
putmatch(which, buf, size)
char	*buf;
{
	*(insert(buf, buf + size, which)) = 0;
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
	char	sbuf[sizeof searchstr],
		cbuf[sizeof compbuf],
		repbuf[sizeof rep_str],
		*altbuf[NALTS];
	int	npars;
	Mark	*m = MakeMark(curline, REbom, M_FLOATER);

	message("Type C-X C-C to continue with query replace.");

	npars = nparens;
	byte_copy(compbuf, cbuf, sizeof compbuf);
	byte_copy(searchstr, sbuf, sizeof searchstr);
	byte_copy(rep_str, repbuf, sizeof rep_str);
	byte_copy((char *) alternates, (char *) altbuf, sizeof alternates);
	Recur();
	nparens = npars;
	byte_copy(cbuf, compbuf, sizeof compbuf);
	byte_copy(sbuf, searchstr, sizeof searchstr);
	byte_copy(repbuf, rep_str, sizeof rep_str);
	byte_copy((char *) altbuf, (char *) alternates, sizeof alternates);
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
{
	register char	**alt = alternates;

	REcompile(pattern, 1, compbuf, alternates);
	REreset();
	locs = buf + offset;
	REbolp = buf;

	while (*alt)
		if (REmatch(locs, *alt++))
			return 1;
	return 0;
}

int
look_at(expr)
char	*expr;
{
	REcompile(expr, 0, compbuf, alternates);
	REreset();
	locs = linebuf + curchar;
	REbolp = linebuf;
	if (REmatch(locs, alternates[0]))
		return 1;
	return 0;
}

