#include "ex.h"
#include "ex_re.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June/September 1977
 */

compile(eof, oknl)
	int eof;
	char oknl;
{
	register c;
	register char *ep;
	char *lastep;
	char bracket[NBRA], *bracketp, *rhsp;
	int cclcnt;

	if (letter(eof) || digit(eof))
		error("Re delimiter must not be letter or digit|Regular expressions cannot be delimited by letters or digits");
	ep = expbuf;
	c = getchar();
	if (eof == '\\')
		switch (c) {

		case '/':
		case '?':
			if (scanre.sexpbuf[0] == 0)
noscanre:
				error("No previous scan re|No previous scanning regular expression");
			resre(&scanre);
			return (c);

		case '&':
			if (subre.sexpbuf[0] == 0)
nosubre:
				error("No previous substitute re|No previous substitute regular expression");
			resre(&subre);
			return (c);

		default:
			error("Badly formed re|Regular expression \\ must be followed by /, ?, or &");
		}
	if (c == eof || c == '\n' || c == EOF) {
		if (*ep == 0)
			error("No previous re|No previous regular expression");
		if (c == '\n' && oknl == 0)
			error("Missing closing delimiter@for regular expression");
		if (c == '\n')
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
						c = *rhsp & 0177;
						if (c == '&')
							error("Replacement pattern contains &@- cannot use in re");
						if (c >= '1' && c <= '9')
							error("Replacement pattern contains \\d@- cannot use in re");
					}
					if (ep >= &expbuf[ESIZE-2])
						goto complex;
					*ep++ = CCHR;
					*ep++ = *rhsp++ & 0177;
				}
				continue;

			case '*':
				if (ep == expbuf)
					break;
				if (*lastep == CBRA || *lastep == CKET)
					cerror("Illegal *|Can't * a \\( ... \\) in regular expression");
				if (*lastep == CCHR && (lastep[1] & QUOTE))
					cerror("Illegal *|Can't * a \\n in regular expression");
				*lastep =| STAR;
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
			if (c < '1' || c > NBRA + '1') {
				*ep++ = c;
				continue;
			}
			c =- '1';
			if (c >= nbra)
				cerror("Bad \\n|\\n in regular expression with n greater than the number of \\('s");
			*ep++ = c | QUOTE;
			continue;

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

	return (a == b || value(IGNORECASE) && (a ^ b) == ' ' && letter(a) == letter(b));
}

execute(gf, addr)
	int *addr;
{
	register char *p1, *p2;
	register c;

	if (gf) {
		if (circfl)
			return (0);
		locs = p1 = loc2;
	} else {
		if (addr == zero)
			return (0);
		p1 = getline(*addr);
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
			    (c ^ *p1) != ' ' || letter(c) != letter(*p1)))
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

#define	uletter(c)	(letter(c) || c == '_')

advance(lp, ep)
	register char *lp, *ep;
{
	register char *curlp;
	char *nextep, *sp, *sp1, c;

	for (;;) switch (*ep++) {

	case CCHR:
		if (*ep & QUOTE) {
			c = *ep++ & 0177;
			sp = braslist[c];
			sp1 = braelist[c];
			while (sp < sp1) {
				if (!same(*sp, *lp))
					return (0);
				sp++, lp++;
			}
			continue;
		}
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
			ep =+ *ep;
			continue;
		}
		return (0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep =+ *ep;
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
		ep =+ *ep;
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
		if (uletter(*lp) && !uletter(lp[-1]) && !digit(lp[-1]))
			continue;
		return (0);

	case CLET:
		if (!uletter(*lp) && !digit(*lp))
			continue;
		return (0);

	default:
		error("Re internal error@- if possible remember what you did and tell system staff");
	}
}

cclass(set, c, af)
	register char *set;
	register c;
	int af;
{
	register n;

	if (c == 0)
		return (0);
	if (value(IGNORECASE) && ucletter(c))
		c = letter(c);
	n = *set++;
	while (--n)
		if (n > 2 && set[1] == '-') {
			if (c >= (set[0] & 0177) && c <= (set[2] & 0177))
				return (af);
			set =+ 3;
			n =- 2;
		} else
			if ((*set++ & 0177) == c)
				return (af);
	return (!af);
}

copy(to, from, size)
	register char *from, *to;
	register int size;
{

	if (size > 0)
		do
			*to++ = *from++;
		while (--size > 0);
}
