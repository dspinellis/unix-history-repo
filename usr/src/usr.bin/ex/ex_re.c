/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_re.h"

/*
 * Global, substitute and regular expressions.
 * Very similar to ed, with some re extensions and
 * confirmed substitute.
 */
global(k)
	bool k;
{
	register char *gp;
	register int c;
	register line *a1;
	char globuf[GBSIZE], *Cwas;
	int lines = lineDOL();
	int oinglobal = inglobal;
	char *oglobp = globp;

	Cwas = Command;
	/*
	 * States of inglobal:
	 *  0: ordinary - not in a global command.
	 *  1: text coming from some buffer, not tty.
	 *  2: like 1, but the source of the buffer is a global command.
	 * Hence you're only in a global command if inglobal==2. This
	 * strange sounding convention is historically derived from
	 * everybody simulating a global command.
	 */
	if (inglobal==2)
		error("Global within global@not allowed");
	markDOT();
	setall();
	nonzero();
	if (skipend())
		error("Global needs re|Missing regular expression for global");
	c = getchar();
	ignore(compile(c, 1));
	savere(scanre);
	gp = globuf;
	while ((c = getchar()) != '\n') {
		switch (c) {

		case EOF:
			c = '\n';
			goto brkwh;

		case '\\':
			c = getchar();
			switch (c) {

			case '\\':
				ungetchar(c);
				break;

			case '\n':
				break;

			default:
				*gp++ = '\\';
				break;
			}
			break;
		}
		*gp++ = c;
		if (gp >= &globuf[GBSIZE - 2])
			error("Global command too long");
	}
brkwh:
	ungetchar(c);
out:
	newline();
	*gp++ = c;
	*gp++ = 0;
	saveall();
	inglobal = 2;
	for (a1 = one; a1 <= dol; a1++) {
		*a1 &= ~01;
		if (a1 >= addr1 && a1 <= addr2 && execute(0, a1) == k)
			*a1 |= 01;
	}
	/* should use gdelete from ed to avoid n**2 here on g/.../d */
	if (inopen)
		inopen = -1;
	for (a1 = one; a1 <= dol; a1++) {
		if (*a1 & 01) {
			*a1 &= ~01;
			dot = a1;
			globp = globuf;
			commands(1, 1);
			a1 = zero;
		}
	}
	globp = oglobp;
	inglobal = oinglobal;
	endline = 1;
	Command = Cwas;
	netchHAD(lines);
	setlastchar(EOF);
	if (inopen) {
		ungetchar(EOF);
		inopen = 1;
	}
}

bool	xflag;
int	scount, slines, stotal;

substitute(c)
	int c;
{
	register line *addr;
	register int n;
	int gsubf;

	gsubf = compsub(c);
	if(FIXUNDO)
		save12(), undkind = UNDCHANGE;
	stotal = 0;
	slines = 0;
	for (addr = addr1; addr <= addr2; addr++) {
		scount = 0;
		if (dosubcon(0, addr) == 0)
			continue;
		if (gsubf) {
#ifdef notdef
			/*
			 * should check but loc2 is already munged.
			 * This needs a fancier check later.
			 */
			if (loc1 == loc2)
				error("substitution loop");
#endif
			while (*loc2)
				if (dosubcon(1, addr) == 0)
					break;
		}
		if (scount) {
			stotal += scount;
			slines++;
			putmark(addr);
			n = append(getsub, addr);
			addr += n;
			addr2 += n;
		}
	}
	if (stotal == 0 && !inglobal && !xflag)
		error("Fail|Substitute pattern match failed");
	snote(stotal, slines);
	return (stotal);
}

compsub(ch)
{
	register int seof, c, uselastre;
	static int gsubf;

	if (!value(EDCOMPATIBLE))
		gsubf = xflag = 0;
	uselastre = 0;
	switch (ch) {

	case 's':
		ignore(skipwh());
		seof = getchar();
		if (endcmd(seof) || any(seof, "gcr")) {
			ungetchar(seof);
			goto redo;
		}
		if (isalpha(seof) || isdigit(seof))
			error("Substitute needs re|Missing regular expression for substitute");
		seof = compile(seof, 1);
		uselastre = 1;
		comprhs(seof);
		gsubf = 0;
		xflag = 0;
		break;

	case '~':
		uselastre = 1;
		/* fall into ... */
	case '&':
	redo:
		if (re.Expbuf[0] == 0)
			error("No previous re|No previous regular expression");
		break;
	}
	for (;;) {
		c = getchar();
		switch (c) {

		case 'g':
			gsubf = !gsubf;
			continue;

		case 'c':
			xflag = !xflag;
			continue;

		case 'r':
			uselastre = 1;
			continue;

		default:
			ungetchar(c);
			setcount();
			newline();
			if (uselastre)
				savere(subre);
			else
				resre(subre);
			return (gsubf);
		}
	}
}

comprhs(seof)
	int seof;
{
	register char *rp, *orp;
	register int c;
	char orhsbuf[LBSIZE / 2];

	rp = rhsbuf;
	CP(orhsbuf, rp);
	for (;;) {
		c = getchar();
		if (c == seof)
			break;
		switch (c) {

		case '\\':
			c = getchar();
			if (c == EOF) {
				ungetchar(c);
				break;
			}
			if (value(MAGIC)) {
				/*
				 * When "magic", \& turns into a plain &,
				 * and all other chars work fine quoted.
				 */
				if (c != '&')
					c |= QUOTE;
				break;
			}
magic:
			if (c == '~') {
				for (orp = orhsbuf; *orp; *rp++ = *orp++)
					if (rp >= &rhsbuf[LBSIZE / 2 + 1])
						goto toobig;
				continue;
			}
			c |= QUOTE;
			break;

		case '\n':
		case EOF:
			if (!(globp && globp[0])) {
				ungetchar(c);
				goto endrhs;
			}

		case '~':
		case '&':
			if (value(MAGIC))
				goto magic;
			break;
		}
		if (rp >= &rhsbuf[LBSIZE / 2 - 1])
toobig:
			error("Replacement pattern too long@- limit 256 characters");
		*rp++ = c;
	}
endrhs:
	*rp++ = 0;
}

getsub()
{
	register char *p;

	if ((p = linebp) == 0)
		return (EOF);
	strcLIN(p);
	linebp = 0;
	return (0);
}

dosubcon(f, a)
	bool f;
	line *a;
{

	if (execute(f, a) == 0)
		return (0);
	if (confirmed(a)) {
		dosub();
		scount++;
	}
	return (1);
}

confirmed(a)
	line *a;
{
	register int c, ch;

	if (xflag == 0)
		return (1);
	pofix();
	pline(lineno(a));
	if (inopen)
		putchar('\n' | QUOTE);
	c = column(loc1 - 1);
	ugo(c - 1 + (inopen ? 1 : 0), ' ');
	ugo(column(loc2 - 1) - c, '^');
	flush();
	ch = c = getkey();
again:
	if (c == '\r')
		c = '\n';
	if (inopen)
		putchar(c), flush();
	if (c != '\n' && c != EOF) {
		c = getkey();
		goto again;
	}
	noteinp();
	return (ch == 'y');
}

getch()
{
	char c;

	if (read(2, &c, 1) != 1)
		return (EOF);
	return (c & TRIM);
}

ugo(cnt, with)
	int with;
	int cnt;
{

	if (cnt > 0)
		do
			putchar(with);
		while (--cnt > 0);
}

int	casecnt;
bool	destuc;

dosub()
{
	register char *lp, *sp, *rp;
	int c;

	lp = linebuf;
	sp = genbuf;
	rp = rhsbuf;
	while (lp < loc1)
		*sp++ = *lp++;
	casecnt = 0;
	while (c = *rp++) {
		if (c & QUOTE)
			switch (c & TRIM) {

			case '&':
				sp = place(sp, loc1, loc2);
				if (sp == 0)
					goto ovflo;
				continue;

			case 'l':
				casecnt = 1;
				destuc = 0;
				continue;

			case 'L':
				casecnt = LBSIZE;
				destuc = 0;
				continue;

			case 'u':
				casecnt = 1;
				destuc = 1;
				continue;

			case 'U':
				casecnt = LBSIZE;
				destuc = 1;
				continue;

			case 'E':
			case 'e':
				casecnt = 0;
				continue;
			}
		if (c < 0 && (c &= TRIM) >= '1' && c < nbra + '1') {
			sp = place(sp, braslist[c - '1'], braelist[c - '1']);
			if (sp == 0)
				goto ovflo;
			continue;
		}
		if (casecnt)
			*sp++ = fixcase(c & TRIM);
		else
			*sp++ = c & TRIM;
		if (sp >= &genbuf[LBSIZE])
ovflo:
			error("Line overflow@in substitute");
	}
	lp = loc2;
	loc2 = sp + (linebuf - genbuf);
	while (*sp++ = *lp++)
		if (sp >= &genbuf[LBSIZE])
			goto ovflo;
	strcLIN(genbuf);
}

fixcase(c)
	register int c;
{

	if (casecnt == 0)
		return (c);
	casecnt--;
	if (destuc) {
		if (islower(c))
			c = toupper(c);
	} else
		if (isupper(c))
			c = tolower(c);
	return (c);
}

char *
place(sp, l1, l2)
	register char *sp, *l1, *l2;
{

	while (l1 < l2) {
		*sp++ = fixcase(*l1++);
		if (sp >= &genbuf[LBSIZE])
			return (0);
	}
	return (sp);
}

snote(total, lines)
	register int total, lines;
{

	if (!notable(total))
		return;
	printf(mesg("%d subs|%d substitutions"), total);
	if (lines != 1 && lines != total)
		printf(" on %d lines", lines);
	noonl();
	flush();
}

compile(eof, oknl)
	int eof;
	int oknl;
{
	register int c;
	register char *ep;
	char *lastep;
	char bracket[NBRA], *bracketp, *rhsp;
	int cclcnt;

	if (isalpha(eof) || isdigit(eof))
		error("Regular expressions cannot be delimited by letters or digits");
	ep = expbuf;
	c = getchar();
	if (eof == '\\')
		switch (c) {

		case '/':
		case '?':
			if (scanre.Expbuf[0] == 0)
error("No previous scan re|No previous scanning regular expression");
			resre(scanre);
			return (c);

		case '&':
			if (subre.Expbuf[0] == 0)
error("No previous substitute re|No previous substitute regular expression");
			resre(subre);
			return (c);

		default:
			error("Badly formed re|Regular expression \\ must be followed by / or ?");
		}
	if (c == eof || c == '\n' || c == EOF) {
		if (*ep == 0)
			error("No previous re|No previous regular expression");
		if (c == '\n' && oknl == 0)
			error("Missing closing delimiter@for regular expression");
		if (c != eof)
			ungetchar(c);
		return (eof);
	}
	bracketp = bracket;
	nbra = 0;
	circfl = 0;
	if (c == '^') {
		c = getchar();
		circfl++;
	}
	ungetchar(c);
	for (;;) {
		if (ep >= &expbuf[ESIZE - 2])
complex:
			cerror("Re too complex|Regular expression too complicated");
		c = getchar();
		if (c == eof || c == EOF) {
			if (bracketp != bracket)
cerror("Unmatched \\(|More \\('s than \\)'s in regular expression");
			*ep++ = CEOF;
			if (c == EOF)
				ungetchar(c);
			return (eof);
		}
		if (value(MAGIC)) {
			if (c != '*' || ep == expbuf)
				lastep = ep;
		} else
			if (c != '\\' || peekchar() != '*' || ep == expbuf)
				lastep = ep;
		switch (c) {

		case '\\':
			c = getchar();
			switch (c) {

			case '(':
				if (nbra >= NBRA)
cerror("Awash in \\('s!|Too many \\('d subexressions in a regular expression");
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;

			case ')':
				if (bracketp <= bracket)
cerror("Extra \\)|More \\)'s than \\('s in regular expression");
				*ep++ = CKET;
				*ep++ = *--bracketp;
				continue;

			case '<':
				*ep++ = CBRC;
				continue;

			case '>':
				*ep++ = CLET;
				continue;
			}
			if (value(MAGIC) == 0)
magic:
			switch (c) {

			case '.':
				*ep++ = CDOT;
				continue;

			case '~':
				rhsp = rhsbuf;
				while (*rhsp) {
					if (*rhsp & QUOTE) {
						c = *rhsp & TRIM;
						if (c == '&')
error("Replacement pattern contains &@- cannot use in re");
						if (c >= '1' && c <= '9')
error("Replacement pattern contains \\d@- cannot use in re");
					}
					if (ep >= &expbuf[ESIZE-2])
						goto complex;
					*ep++ = CCHR;
					*ep++ = *rhsp++ & TRIM;
				}
				continue;

			case '*':
				if (ep == expbuf)
					break;
				if (*lastep == CBRA || *lastep == CKET)
cerror("Illegal *|Can't * a \\( ... \\) in regular expression");
				if (*lastep == CCHR && (lastep[1] & QUOTE))
cerror("Illegal *|Can't * a \\n in regular expression");
				*lastep |= STAR;
				continue;

			case '[':
				*ep++ = CCL;
				*ep++ = 0;
				cclcnt = 1;
				c = getchar();
				if (c == '^') {
					c = getchar();
					ep[-2] = NCCL;
				}
				if (c == ']')
cerror("Bad character class|Empty character class '[]' or '[^]' cannot match");
				while (c != ']') {
					if (c == '\\' && any(peekchar(), "]-^\\"))
						c = getchar() | QUOTE;
					if (c == '\n' || c == EOF)
						cerror("Missing ]");
					*ep++ = c;
					cclcnt++;
					if (ep >= &expbuf[ESIZE])
						goto complex;
					c = getchar();
				}
				lastep[1] = cclcnt;
				continue;
			}
			if (c == EOF) {
				ungetchar(EOF);
				c = '\\';
				goto defchar;
			}
			*ep++ = CCHR;
			if (c == '\n')
cerror("No newlines in re's|Can't escape newlines into regular expressions");
/*
			if (c < '1' || c > NBRA + '1') {
*/
				*ep++ = c;
				continue;
/*
			}
			c -= '1';
			if (c >= nbra)
cerror("Bad \\n|\\n in regular expression with n greater than the number of \\('s");
			*ep++ = c | QUOTE;
			continue;
*/

		case '\n':
			if (oknl) {
				ungetchar(c);
				*ep++ = CEOF;
				return (eof);
			}
cerror("Badly formed re|Missing closing delimiter for regular expression");

		case '$':
			if (peekchar() == eof || peekchar() == EOF || oknl && peekchar() == '\n') {
				*ep++ = CDOL;
				continue;
			}
			goto defchar;

		case '.':
		case '~':
		case '*':
		case '[':
			if (value(MAGIC))
				goto magic;
defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
			continue;
		}
	}
}

cerror(s)
	char *s;
{

	expbuf[0] = 0;
	error(s);
}

same(a, b)
	register int a, b;
{

	return (a == b || value(IGNORECASE) &&
	   ((islower(a) && toupper(a) == b) || (islower(b) && toupper(b) == a)));
}

char	*locs;

execute(gf, addr)
	line *addr;
{
	register char *p1, *p2;
	register int c;

	if (gf) {
		if (circfl)
			return (0);
#ifdef notdef
		if (loc1 == loc2)
			loc2++;
#endif
		locs = p1 = loc2;
	} else {
		if (addr == zero)
			return (0);
		p1 = linebuf;
		getline(*addr);
		locs = 0;
	}
	p2 = expbuf;
	if (circfl) {
		loc1 = p1;
		return (advance(p1, p2));
	}
	/* fast check for first character */
	if (*p2 == CCHR) {
		c = p2[1];
		do {
			if (c != *p1 && (!value(IGNORECASE) ||
			   !((islower(c) && toupper(c) == *p1) ||
			   (islower(*p1) && toupper(*p1) == c))))
				continue;
			if (advance(p1, p2)) {
				loc1 = p1;
				return (1);
			}
		} while (*p1++);
		return (0);
	}
	/* regular algorithm */
	do {
		if (advance(p1, p2)) {
			loc1 = p1;
			return (1);
		}
	} while (*p1++);
	return (0);
}

#define	uletter(c)	(isalpha(c) || c == '_')

advance(lp, ep)
	register char *lp, *ep;
{
	register char *curlp;
	char *sp, *sp1;
	int c;

	for (;;) switch (*ep++) {

	case CCHR:
/* useless
		if (*ep & QUOTE) {
			c = *ep++ & TRIM;
			sp = braslist[c];
			sp1 = braelist[c];
			while (sp < sp1) {
				if (!same(*sp, *lp))
					return (0);
				sp++, lp++;
			}
			continue;
		}
*/
		if (!same(*ep, *lp))
			return (0);
		ep++, lp++;
		continue;

	case CDOT:
		if (*lp++)
			continue;
		return (0);

	case CDOL:
		if (*lp == 0)
			continue;
		return (0);

	case CEOF:
		loc2 = lp;
		return (1);

	case CCL:
		if (cclass(ep, *lp++, 1)) {
			ep += *ep;
			continue;
		}
		return (0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep += *ep;
			continue;
		}
		return (0);

	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CDOT|STAR:
		curlp = lp;
		while (*lp++)
			continue;
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (same(*lp, *ep))
			lp++;
		lp++;
		ep++;
		goto star;

	case CCL|STAR:
	case NCCL|STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1] == (CCL|STAR)))
			continue;
		ep += *ep;
		goto star;
star:
		do {
			lp--;
			if (lp == locs)
				break;
			if (advance(lp, ep))
				return (1);
		} while (lp > curlp);
		return (0);

	case CBRC:
		if (lp == expbuf)
			continue;
		if ((isdigit(*lp) || uletter(*lp)) && !uletter(lp[-1]) && !isdigit(lp[-1]))
			continue;
		return (0);

	case CLET:
		if (!uletter(*lp) && !isdigit(*lp))
			continue;
		return (0);

	default:
		error("Re internal error");
	}
}

cclass(set, c, af)
	register char *set;
	register int c;
	int af;
{
	register int n;

	if (c == 0)
		return (0);
	if (value(IGNORECASE) && isupper(c))
		c = tolower(c);
	n = *set++;
	while (--n)
		if (n > 2 && set[1] == '-') {
			if (c >= (set[0] & TRIM) && c <= (set[2] & TRIM))
				return (af);
			set += 3;
			n -= 2;
		} else
			if ((*set++ & TRIM) == c)
				return (af);
	return (!af);
}
