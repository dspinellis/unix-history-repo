/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C shell
 */

/*
 * These lexical routines read input and form lists of words.
 * There is some involved processing here, because of the complications
 * of input buffering, and especially because of history substitution.
 */

char	*word();

/*
 * Peekc is a peek characer for getC, peekread for readc.
 * There is a subtlety here in many places... history routines
 * will read ahead and then insert stuff into the input stream.
 * If they push back a character then they must push it behind
 * the text substituted by the history substitution.  On the other
 * hand in several places we need 2 peek characters.  To make this
 * all work, the history routines read with getC, and make use both
 * of ungetC and unreadc.  The key observation is that the state
 * of getC at the call of a history reference is such that calls
 * to getC from the history routines will always yield calls of
 * readc, unless this peeking is involved.  That is to say that during
 * getexcl the variables lap, exclp, and exclnxt are all zero.
 *
 * Getdol invokes history substitution, hence the extra peek, peekd,
 * which it can ungetD to be before history substitutions.
 */
char	peekc, peekd;
char	peekread;

char	*exclp;			/* (Tail of) current word from ! subst */
struct	wordent *exclnxt;	/* The rest of the ! subst words */
int	exclc;			/* Count of remainig words in ! subst */
char	*alvecp;		/* "Globp" for alias resubstitution */

/*
 * Lex returns to its caller not only a wordlist (as a "var" parameter)
 * but also whether a history substitution occurred.  This is used in
 * the main (process) routine to determine whether to echo, and also
 * when called by the alias routine to determine whether to keep the
 * argument list.
 */
bool	hadhist;

#define	ungetC(c)	peekc = c
#define	ungetD(c)	peekd = c

lex(hp)
	register struct wordent *hp;
{
	register struct wordent *wdp;
	int c;

	lineloc = btell();
	hp->next = hp->prev = hp;
	hp->word = "";
	alvecp = 0, hadhist = 0;
	do
		c = readc(0);
	while (c == ' ' || c == '\t');
	if (c == '^' && intty)
		/* ^lef^rit	from tty is short !:s^lef^rit */
		getexcl(c);
	else
		unreadc(c);
	wdp = hp;
	/*
	 * The following loop is written so that the links needed
	 * by freelex will be ready and rarin to go even if it is
	 * interrupted.
	 */
	do {
		register struct wordent *new = (struct wordent *) calloc(1, sizeof *wdp);

		new->prev = wdp;
		new->next = hp;
		wdp->next = new;
		wdp = new;
		wdp->word = word();
	} while (wdp->word[0] != '\n');
	hp->prev = wdp;
	return (hadhist);
}

prlex(sp0)
	struct wordent *sp0;
{
	register struct wordent *sp = sp0->next;

	for (;;) {
		printf("%s", sp->word);
		sp = sp->next;
		if (sp == sp0)
			break;
		printf(" ");
	}
}

copylex(hp, fp)
	register struct wordent *hp;
	struct wordent *fp;
{
	register struct wordent *wdp;

	wdp = hp;
	fp = fp->next;
	do {
		register struct wordent *new = (struct wordent *) calloc(1, sizeof *wdp);

		new->prev = wdp;
		new->next = hp;
		wdp->next = new;
		wdp = new;
		wdp->word = savestr(fp->word);
		fp = fp->next;
	} while (wdp->word[0] != '\n');
	hp->prev = wdp;
}

freelex(vp)
	register struct wordent *vp;
{
	register struct wordent *fp;

	while (vp->next != vp) {
		fp = vp->next;
		vp->next = fp->next;
		xfree(fp->word);
		xfree(fp);
	}
	vp->prev = vp;
}

char	*WORDMETA =	"# '`\"\t;&<>()|\n";

char *
word()
{
	register char c, c1;
	register char *wp;
	char wbuf[BUFSIZ];
	register bool dolflg;
	register int i;

	wp = wbuf;
	i = BUFSIZ - 4;
loop:
	c = getC(DOALL);
	switch (c) {

	case ' ':
	case '\t':
		goto loop;

	case '`':
	case '\'':
	case '"':
		*wp++ = c, --i, c1 = c;
		dolflg = c == '"' ? DOALL : DOEXCL;
		for (;;) {
			c = getC(dolflg);
			if (c == c1)
				break;
			if (c == '\n') {
				seterrc("Unmatched ", c1);
				ungetC(c);
				goto ret;
			}
			if (c == '\\') {
				c = getC(0);
				if (c == '!')
					c |= QUOTE;
				else {
					if (c == '\n' && c1 != '`')
						c |= QUOTE;
					ungetC(c), c = '\\';
				}
			}
			if (--i <= 0)
				goto toochars;
			*wp++ = c;
		}
		*wp++ = c, --i;
		goto pack;

	case '&':
	case '|':
	case '<':
	case '>':
		*wp++ = c;
		c1 = getC(DOALL);
		if (c1 == c)
			*wp++ = c1;
		else
			ungetC(c1);
		goto ret;

	case '#':
		if (intty)
			break;
		if (wp != wbuf) {
			ungetC(c);
			goto ret;
		}
		c = 0;
		do {
			c1 = c;
			c = getC(0);
		} while (c != '\n');
		if (c1 == '\\')
			goto loop;
		/* fall into ... */

	case ';':
	case '(':
	case ')':
	case '\n':
		*wp++ = c;
		goto ret;

casebksl:
	case '\\':
		c = getC(0);
		if (c == '\n') {
			if (onelflg == 1)
				onelflg = 2;
			goto loop;
		}
		if (c != '!')
			*wp++ = '\\', --i;
		c |= QUOTE;
		break;
	}
	ungetC(c);
pack:
	for (;;) {
		c = getC(DOALL);
		if (c == '\\') {
			c = getC(0);
			if (c == '\n') {
				if (onelflg == 1)
					onelflg = 2;
				goto ret;
			}
			if (c != '!')
				*wp++ = '\\', --i;
			c |= QUOTE;
		}
		if (any(c, WORDMETA + intty)) {
			ungetC(c);
			if (any(c, "\"'`"))
				goto loop;
			goto ret;
		}
		if (--i <= 0)
			goto toochars;
		*wp++ = c;
	}
toochars:
	seterr("Word too long");
	wp = &wbuf[1];
ret:
	*wp = 0;
	return (savestr(wbuf));
}

getC(flag)
	register int flag;
{
	register char c;

top:
	if (c = peekc) {
		peekc = 0;
		return (c);
	}
	if (lap) {
		c = *lap++;
		if (c == 0) {
			lap = 0;
			goto top;
		}
		if (any(c, WORDMETA + intty))
			c |= QUOTE;
		return (c);
	}
	if (c = peekd) {
		peekd = 0;
		return (c);
	}
	if (exclp) {
		if (c = *exclp++)
			return (c);
		if (exclnxt && --exclc >= 0) {
			exclnxt = exclnxt->next;
			setexclp(exclnxt->word);
			return (' ');
		}
		exclp = 0;
		exclnxt = 0;
	}
	if (exclnxt) {
		exclnxt = exclnxt->next;
		if (--exclc < 0)
			exclnxt = 0;
		else
			setexclp(exclnxt->word);
		goto top;
	}
	c = readc(0);
	if (c == '$' && (flag & DODOL)) {
		getdol();
		goto top;
	}
	if (c == '!' && (flag & DOEXCL)) {
		getexcl(0);
		goto top;
	}
	return (c);
}

getdol()
{
	register char *np;
	char name[40];
	register int c;
	int sc;
	bool special = 0;

	np = name, *np++ = '$';
	c = sc = getC(DOEXCL);
	if (any(c, "\t \n")) {
		ungetD(c);
		ungetC('$' | QUOTE);
		return;
	}
	if (c == '{')
		*np++ = c, c = getC(DOEXCL);
	if (c == '#' || c == '?')
		special++, *np++ = c, c = getC(DOEXCL);
	*np++ = c;
	switch (c) {
	
	case '$':
		if (special)
			goto vsyn;
		goto ret;

	case '\n':
		ungetD(c);
		np--;
		goto vsyn;

	case '*':
		if (special)
			goto vsyn;
		goto ret;

	default:
		if (digit(c)) {
/*
 * let $?0 pass for now
			if (special)
				goto vsyn;
*/
			while (digit(c = getC(DOEXCL))) {
				if (np < &name[sizeof name / 2])
					*np++ = c;
			}
		} else if (letter(c))
			while (letter(c = getC(DOEXCL))) {
				if (np < &name[sizeof name / 2])
					*np++ = c;
			}
		else
			goto vsyn;
	}
	if (c == '[') {
		*np++ = c;
		do {
			c = getC(DOEXCL);
			if (c == '\n') {
				ungetD(c);
				np--;
				goto vsyn;
			}
			if (np >= &name[sizeof name - 8])
				goto vsyn;
			*np++ = c;
		} while (c != ']');
		c = getC(DOEXCL);
	}
	if (c == ':') {
		*np++ = c, c = getC(DOEXCL);
		if (c == 'g')
			*np++ = c, c = getC(DOEXCL);
		*np++ = c;
		if (!any(c, "htrqx"))
			goto vsyn;
	} else
		ungetD(c);
	if (sc == '{') {
		c = getC(DOEXCL);
		if (c != '}') {
			ungetC(c);
			goto vsyn;
		}
		*np++ = c;
	}
ret:
	*np = 0;
	addla(name);
	return;

vsyn:
	seterr("Variable syntax");
	goto ret;
}

addla(cp)
	char *cp;
{
	char buf[BUFSIZ];

	if (lap != 0 && strlen(cp) + strlen(lap) >= BUFSIZ - 4) {
		seterr("Expansion buf ovflo");
		return;
	}
	if (lap)
		strcpy(buf, lap);
	strcpy(labuf, cp);
	if (lap)
		strcat(labuf, buf);
	lap = labuf;
}

char	lhsb[32];
char	slhs[32];
char	rhsb[64];
int	quesarg;

getexcl(sc)
	char sc;
{
	register struct wordent *hp, *ip;
	int left, right, dol;
	register int c;

	if (sc == 0) {
		sc = getC(0);
		if (sc != '{') {
			ungetC(sc);
			sc = 0;
		}
	}
	quesarg = -1;
	lastev = eventno;
	hp = gethent(sc);
	if (hp == 0)
		return;
	hadhist = 1;
	dol = 0;
	if (hp == alhistp)
		for (ip = hp->next->next; ip != alhistt; ip = ip->next)
			dol++;
	else
		for (ip = hp->next->next; ip != hp->prev; ip = ip->next)
			dol++;
	left = 0, right = dol;
	if (sc == '^') {
		ungetC('s'), unreadc('^'), c = ':';
		goto subst;
	}
	c = getC(0);
	if (!any(c, ":^$*-%"))
		goto subst;
	left = right = -1;
	if (c == ':') {
		c = getC(0);
		unreadc(c);
		if (letter(c) || c == '&') {
			c = ':';
			left = 0, right = dol;
			goto subst;
		}
	} else
		ungetC(c);
	if (!getsel(&left, &right, dol))
		return;
	c = getC(0);
	if (c == '*')
		ungetC(c), c = '-';
	if (c == '-') {
		if (!getsel(&left, &right, dol))
			return;
		c = getC(0);
	}
subst:
	exclc = right - left + 1;
	while (--left >= 0)
		hp = hp->next;
	if (sc == '^' || c == ':') {
		do {
			hp = getsub(hp);
			c = getC(0);
		} while (c == ':');
	}
	unreadc(c);
	if (sc == '{') {
		c = getC(0);
		if (c != '}')
			seterr("Bad ! form");
	}
	exclnxt = hp;
}

struct wordent *
getsub(en)
	struct wordent *en;
{
	register char *cp;
	int delim;
	register int c;
	int sc;
	bool global = 0;
	char orhsb[sizeof rhsb];

	exclnxt = 0;
	sc = c = getC(0);
	if (c == 'g')
		global++, c = getC(0);
	switch (c) {

	case 'p':
		justpr++;
		goto ret;

	case 'x':
	case 'q':
		global++;
		/* fall into ... */

	case 'h':
	case 'r':
	case 't':
		break;

	case '&':
		if (slhs[0] == 0) {
			seterr("No prev sub");
			goto ret;
		}
		strcpy(lhsb, slhs);
		break;

/*
	case '~':
		if (lhsb[0] == 0)
			goto badlhs;
		break;
*/

	case 's':
		delim = getC(0);
		if (letter(delim) || digit(delim) || any(delim, " \t\n")) {
			unreadc(delim);
bads:
			lhsb[0] = 0;
			seterr("Bad substitute");
			goto ret;
		}
		cp = lhsb;
		for (;;) {
			c = getC(0);
			if (c == '\n') {
				unreadc(c);
				goto bads;
			}
			if (c == delim)
				break;
			if (cp > &lhsb[sizeof lhsb - 2])
				goto bads;
			if (c == '\\') {
				c = getC(0);
				if (c != delim && c != '\\')
					*cp++ = '\\';
			}
			*cp++ = c;
		}
		if (cp != lhsb)
			*cp++ = 0;
		else if (lhsb[0] == 0) {
badlhs:
			seterr("No prev lhs");
			goto ret;
		}
		cp = rhsb;
		strcpy(orhsb, cp);
		for (;;) {
			c = getC(0);
			if (c == '\n') {
				unreadc(c);
				break;
			}
			if (c == delim)
				break;
/*
			if (c == '~') {
				if (&cp[strlen(orhsb)] > &rhsb[sizeof rhsb - 2])
					goto toorhs;
				cp = strend(strcpy(cp, orhsb));
				continue;
			}
*/
			if (cp > &rhsb[sizeof rhsb - 2]) {
toorhs:
				seterr("Rhs too long");
				goto ret;
			}
			if (c == '\\') {
				c = getC(0);
				if (c != delim /* && c != '~' */)
					*cp++ = '\\';
			}
			*cp++ = c;
		}
		*cp++ = 0;
		break;

	default:
		if (c == '\n')
			unreadc(c);
		seterrc("Bad ! modifier: ", c);
		goto ret;
	}
	strcpy(slhs, lhsb);
	if (exclc)
		en = dosub(sc, en, global);
ret:
	return (en);
}

struct wordent *
dosub(sc, en, global)
	int sc;
	struct wordent *en;
	bool global;
{
	struct wordent lex;
	bool didsub = 0;
	struct wordent *hp = &lex;
	register struct wordent *wdp;
	register int i = exclc;

	wdp = hp;
	while (--i >= 0) {
		register struct wordent *new = (struct wordent *) calloc(1, sizeof *wdp);

		new->prev = wdp;
		new->next = hp;
		wdp->next = new;
		wdp = new;
		en = en->next;
		wdp->word = global || didsub == 0 ?
		    subword(en->word, sc, &didsub) : savestr(en->word);
	}
	if (didsub == 0)
		seterr("Modifier failed");
	hp->prev = wdp;
	return (&enthist(-1000, &lex, 0)->Hlex);
}

char *
subword(cp, type, adid)
	char *cp;
	int type;
	bool *adid;
{
	char wbuf[BUFSIZ];
	register char *wp, *mp, *np;
	register int i;

	switch (type) {

	case 'r':
	case 'h':
	case 't':
	case 'q':
	case 'x':
		wp = domod(cp, type);
		if (wp == 0)
			return (savestr(cp));
		*adid = 1;
		return (wp);

	default:
		wp = wbuf;
		i = BUFSIZ - 4;
		for (mp = cp; *mp; mp++)
			if (matchs(mp, lhsb)) {
				for (np = cp; np < mp;)
					*wp++ = *np++, --i;
				for (np = rhsb; *np; np++) switch (*np) {

				case '\\':
					if (np[1] == '&')
						np++;
					/* fall into ... */

				default:
					if (--i < 0)
						goto ovflo;
					*wp++ = *np;
					continue;

				case '&':
					i -= strlen(lhsb);
					if (i < 0)
						goto ovflo;
					*wp = 0;
					strcat(wp, lhsb);
					wp = strend(wp);
					continue;
				}
				mp += strlen(lhsb);
				i -= strlen(mp);
				if (i < 0) {
ovflo:
					seterr("Subst buf ovflo");
					return ("");
				}
				*wp = 0;
				strcat(wp, mp);
				*adid = 1;
				return (savestr(wbuf));
			}
		return (savestr(cp));
	}
}

char *
domod(cp, type)
	char *cp;
	int type;
{
	register char *wp, *xp;
	register int c;

	switch (type) {

	case 'x':
	case 'q':
		wp = savestr(cp);
		for (xp = wp; c = *xp; xp++)
			if ((c != ' ' && c != '\t') || type == 'q')
				*xp |= QUOTE;
		return (wp);

	case 'h':
	case 't':
		if (!any('/', cp))
			return (0);
		wp = strend(cp);
		while (*--wp != '/')
			continue;
		if (type == 'h')
take:
			xp = savestr(cp), xp[wp - cp] = 0;
		else
			xp = savestr(wp + 1);
		return (xp);

	case 'r':
		wp = strend(cp);
		for (wp--; wp >= cp && *wp != '.'; wp--)
			if (*wp == '/')
				return (0);
		if (wp < cp)
			return (0);
		goto take;
	}
	return (0);
}

matchs(str, pat)
	register char *str, *pat;
{

	while (*str && *pat && *str == *pat)
		str++, pat++;
	return (*pat == 0);
}

getsel(al, ar, dol)
	register int *al, *ar;
	int dol;
{
	register int c = getC(0);
	register int i;
	bool first = *al < 0;

	switch (c) {

	case '%':
		if (quesarg == -1)
			goto bad;
		if (*al < 0)
			*al = quesarg;
		*ar = quesarg;
		break;

	case '-':
		if (*al < 0) {
			*al = 0;
			*ar = dol - 1;
			unreadc(c);
		}
		return (1);

	case '^':
		if (*al < 0)
			*al = 1;
		*ar = 1;
		break;

	case '$':
		if (*al < 0)
			*al = dol;
		*ar = dol;
		break;

	case '*':
		if (*al < 0)
			*al = 1;
		*ar = dol;
		if (*ar < *al) {
			*ar = 0;
			*al = 1;
			return (1);
		}
		break;

	default:
		if (digit(c)) {
			i = 0;
			while (digit(c)) {
				i = i * 10 + c - '0';
				c = getC(0);
			}
			if (i < 0)
				i = dol + 1;
			if (*al < 0)
				*al = i;
			*ar = i;
		} else
			if (*al < 0)
				*al = 0, *ar = dol;
			else
				*ar = dol - 1;
		unreadc(c);
		break;
	}
	if (first) {
		c = getC(0);
		unreadc(c);
		if (any(c, "-$*"))
			return (1);
	}
	if (*al > *ar || *ar > dol) {
bad:
		seterr("Bad ! arg selector");
		return (0);
	}
	return (1);
}

struct wordent *
gethent(sc)
	int sc;
{
	register struct Hist *hp;
	register char *np;
	register int c;
	int event;
	bool back = 0;

	c = sc == '^' ? '!' : getC(0);
	switch (c) {

	case ':':
	case '^':
	case '$':
	case '*':
	case '%':
		ungetC(c);
		if (lastev == eventno && alhistp)
			return (alhistp);
		event = lastev;
		break;

	case '!':
		event = eventno;
		break;

	case '-':
		back = 1;
		c = getC(0);
		goto number;

	default:
		if (any(c, "(=")) {
			unreadc(c);
			ungetC('!');
			return (0);
		}
		if (digit(c))
			goto number;
		np = lhsb;
		while (!any(c, ": \t\\\n}")) {
			if (np < &lhsb[sizeof lhsb - 2])
				*np++ = c;
			c = getC(0);
		}
		unreadc(c);
		if (np == lhsb) {
			ungetC('!');
			return (0);
		}
		*np++ = 0;
		hp = findev(lhsb, 0);
		if (hp)
			lastev = hp->Hnum;
		return (&hp->Hlex);

	case '?':
		np = lhsb;
		for (;;) {
			c = getC(0);
			if (c == '\n') {
				unreadc(c);
				break;
			}
			if (c == '?')
				break;
			if (np < &lhsb[sizeof lhsb - 2])
				*np++ = c;
		}
		if (np == lhsb) {
			if (lhsb[0] == 0) {
				seterr("No prev search");
				return (0);
			}
		} else
			*np++ = 0;
		hp = findev(lhsb, 1);
		if (hp)
			lastev = hp->Hnum;
		return (&hp->Hlex);

	number:
		event = 0;
		while (digit(c)) {
			event = event * 10 + c - '0';
			c = getC(0);
		}
		if (back)
			event = eventno + (alhistp == 0) - (event ? event : 0);
		unreadc(c);
		break;
	}
	for (hp = Histlist.Hnext; hp; hp = hp->Hnext)
		if (hp->Hnum == event) {
			hp->Href = eventno;
			lastev = hp->Hnum;
			return (&hp->Hlex);
		}
	np = putn(event);
	noev(np);
	return (0);
}

struct Hist *
findev(cp, anyarg)
	char *cp;
	bool anyarg;
{
	register struct Hist *hp;

	for (hp = Histlist.Hnext; hp; hp = hp->Hnext)
		if (matchev(hp, cp, anyarg))
			return (hp);
	noev(cp);
	return (0);
}

noev(cp)
	char *cp;
{

	seterr2(cp, ": Event not found");
}

matchev(hp, cp, anyarg)
	register struct Hist *hp;
	char *cp;
	bool anyarg;
{
	register char *dp;
	struct wordent *lp = &hp->Hlex;
	int argno = 0;
	
	for (;;) {
		lp = lp->next;
		if (lp->word[0] == '\n')
			return (0);
		for (dp = lp->word; *dp; dp++) {
			if (matchs(dp, cp)) {
				if (anyarg)
					quesarg = argno;
				return (1);
			}
			if (!anyarg)
				return (0);
		}
		argno++;
	}
}

setexclp(cp)
	register char *cp;
{

	if (cp[0] == '\n')
		return;
	exclp = cp;
}

unreadc(c)
	char c;
{

	peekread = c;
}

readc(wanteof)
	bool wanteof;
{
	register int c;

	if (c = peekread) {
		peekread = 0;
		return (c);
	}
top:
	if (alvecp) {
		if (c = *alvecp++)
			return (c);
		if (*alvec) {
			alvecp = *alvec++;
			return (' ');
		}
	}
	if (alvec) {
		if (alvecp = *alvec) {
			alvec++;
			goto top;
		}
		/* Infinite source! */
		return ('\n');
	}
	do {
		if (arginp == (char *) 1 || onelflg == 1) {
			if (wanteof)
				return (-1);
			exitstat();
		}
		if (arginp) {
			if ((c = *arginp++) == 0) {
				arginp = (char *) 1;
				return ('\n');
			}
			return (c);
		}
		c = bgetc();
		if (c < 0) {
#include <sgtty.h>
			struct sgttyb tty;

			if (wanteof)
				return (-1);
			/* was isatty but raw with ignoreeof yields problems */
			if (adrof("ignoreeof") && gtty(SHIN, &tty)==0 && (tty.sg_flags & RAW) == 0) {
				if (loginsh)
					printf("\nUse \"logout\" to logout.\n");
				else
					printf("\nUse \"exit\" to leave csh.\n");
				reset();
			}
			doneinp = 1;
			reset();
		}
		if (c == '\n' && onelflg)
			onelflg--;
	} while (c == 0);
	return (c);
}

bgetc()
{
	register int buf, off, c;

#ifdef TELL
	if (cantell) {
		if (fseekp < fbobp || fseekp > feobp) {
			fbobp = feobp = fseekp;
			lseek(SHIN, fseekp, 0);
		}
		if (fseekp == feobp) {
			fbobp = feobp;
			do
				c = read(SHIN, fbuf[0], BUFSIZ);
			while (c < 0 && errno == EINTR);
			if (c <= 0)
				return (-1);
			feobp += c;
		}
		c = fbuf[0][fseekp - fbobp];
		fseekp++;
		return (c);
	}
#endif
again:
	buf = (int) fseekp / BUFSIZ;
	if (buf >= fblocks) {
		register char **nfbuf = (char **) calloc(fblocks+2, sizeof (char **));

		if (fbuf) {
			blkcpy(nfbuf, fbuf);
			xfree(fbuf);
		}
		fbuf = nfbuf;
		fbuf[fblocks] = calloc(BUFSIZ, sizeof (char));
		fblocks++;
		goto again;
	}
	if (fseekp >= feobp) {
		buf = (int) feobp / BUFSIZ;
		off = (int) feobp % BUFSIZ;
		do
			c = read(SHIN, fbuf[buf] + off, BUFSIZ - off);
		while (c < 0 && errno == EINTR);
		if (c <= 0)
			return (-1);
		feobp += c;
		goto again;
	}
	c = fbuf[buf][(int) fseekp % BUFSIZ];
	fseekp++;
	return (c);
}

bfree()
{
	register int sb, i;

#ifdef TELL
	if (cantell)
		return;
#endif
	if (whyles)
		return;
	sb = (int) (fseekp - 1) / BUFSIZ;
	if (sb > 0) {
		for (i = 0; i < sb; i++)
			xfree(fbuf[i]);
		blkcpy(fbuf, &fbuf[sb]);
		fseekp -= BUFSIZ * sb;
		feobp -= BUFSIZ * sb;
		fblocks -= sb;
	}
}

bseek(l)
	long l;
{
	register struct whyle *wp;

	fseekp = l;
#ifdef TELL
	if (!cantell) {
#endif
		if (!whyles)
			return;
		for (wp = whyles; wp->w_next; wp = wp->w_next)
			continue;
		if (wp->w_start > l)
			l = wp->w_start;
#ifdef TELL
	}
#endif
}

/* any similarity to bell telephone is purely accidental */
long
btell()
{

	return (fseekp);
}

btoeof()
{

	lseek(SHIN, 0l, 2);
	fseekp = feobp;
	wfree();
	bfree();
}

#ifdef TELL
settell()
{

	cantell = 0;
	if (arginp || onelflg || intty)
		return;
	if (lseek(SHIN, 0l, 1) < 0 || errno == ESPIPE)
		return;
	fbuf = (char **) calloc(2, sizeof (char **));
	fblocks = 1;
	fbuf[0] = calloc(BUFSIZ, sizeof (char));
	fseekp = fbobp = feobp = tell(SHIN);
	cantell = 1;
}
#endif
