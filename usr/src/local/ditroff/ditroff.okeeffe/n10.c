#ifndef lint
/*
static char sccsid[]="@(#)n10.c	1.2	(CWI)	86/08/15";
*/
static char sccsid[] = "@(#)n10.c	1.3 (Berkeley) %G%";
#endif

/*
n10.c

Device interfaces
*/

#include "tdef.h"
#include "ext.h"
#include "tw.h"
#include <sgtty.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

struct t t;	/* terminal characteristics */

int	dtab;
int	plotmode;
int	esct;

char	xchname[4 * (NROFFCHARS-128)];	/* hy, em, etc. */
short	xchtab[NROFFCHARS-128];		/* indexes into chname[] */
char	*codestr;
char	*chname = xchname;
short	*chtab = xchtab;


int	Inch;
int	Hor;
int	Vert;
int	nfonts	= 4;	/* R, I, B, S */

/* these characters are used as various signals or values
/* in miscellaneous places.
/* values are set in specnames in t10.c
*/

int	c_hyphen;
int	c_emdash;
int	c_rule;
int	c_minus;
int	c_fi;
int	c_fl;
int	c_ff;
int	c_ffi;
int	c_ffl;
int	c_acute;
int	c_grave;
int	c_under;
int	c_rooten;
int	c_boxrule;
int	c_lefthand;
int	c_dagger;
int	c_isalnum;

ptinit()
{
	register int i, j;
	register char *p, *cp, *q;
	int nread, fd;
	extern char *skipstr(), *getstr(), *getint();
	struct stat stbuf;
	char check[50];

	strcat(termtab, devname);
	if ((fd = open(termtab, 0)) < 0) {
		errprint("cannot open %s", termtab);
		exit(-1);
	}

	fstat(fd, &stbuf);
	codestr = malloc((int) stbuf.st_size);

	nread = read(fd, codestr, (int) stbuf.st_size);
	close(fd);

	p = codestr;
	p = skipstr(p);		/* skip over type, could check */
	p = skipstr(p); p = getint(p, &t.bset);
	p = skipstr(p); p = getint(p, &t.breset);
	p = skipstr(p); p = getint(p, &t.Hor);
	p = skipstr(p); p = getint(p, &t.Vert);
	p = skipstr(p); p = getint(p, &t.Newline);
	p = skipstr(p); p = getint(p, &t.Char);
	p = skipstr(p); p = getint(p, &t.Em);
	p = skipstr(p); p = getint(p, &t.Halfline);
	p = skipstr(p); p = getint(p, &t.Adj);
	p = skipstr(p); p = getstr(p, t.twinit = p);
	p = skipstr(p); p = getstr(p, t.twrest = p);
	p = skipstr(p); p = getstr(p, t.twnl = p);
	p = skipstr(p); p = getstr(p, t.hlr = p);
	p = skipstr(p); p = getstr(p, t.hlf = p);
	p = skipstr(p); p = getstr(p, t.flr = p);
	p = skipstr(p); p = getstr(p, t.bdon = p);
	p = skipstr(p); p = getstr(p, t.bdoff = p);
	p = skipstr(p); p = getstr(p, t.iton = p);
	p = skipstr(p); p = getstr(p, t.itoff = p);
	p = skipstr(p); p = getstr(p, t.ploton = p);
	p = skipstr(p); p = getstr(p, t.plotoff = p);
	p = skipstr(p); p = getstr(p, t.up = p);
	p = skipstr(p); p = getstr(p, t.down = p);
	p = skipstr(p); p = getstr(p, t.right = p);
	p = skipstr(p); p = getstr(p, t.left = p);
	p = skipstr(p); p = getstr(p, t.eject = p);

	getstr(p, check);
	if (strcmp(check, "charset") != 0) {
		errprint("device table apparently curdled");
		exit(1);
	}

	for (i = 0; i < 128; i++)
		t.width[i] = 1;	/* default ascii widths */

	i = 0;
/* this ought to be a pointer array and in place in codestr */
	cp = chname + 1;	/* bug if starts at 0, in setch */
	while (p < codestr + nread) {
		while (*p == ' ' || *p == '\t' || *p == '\n')
			p++;
		chtab[i] = cp - chname;	/* index, not pointer */
		*cp++ = *p++;	/* 2-char names */
		*cp++ = *p++;
		*cp++ = '\0';
		while (*p == ' ' || *p == '\t')
			p++;
		t.width[i+128] = *p++ - '0';
		while (*p == ' ' || *p == '\t')
			p++;
		t.codetab[i] = p;
		p = getstr(p, p);	/* compress string */
		p++;
		i++;
	}

	sps = EM;
	ics = EM * 2;
	dtab = 8 * t.Em;
	for (i = 0; i < 16; i++)
		tabtab[i] = dtab * (i + 1);
	pl = 11 * INCH;
	po = PO;
	spacesz = SS;
	lss = lss1 = VS;
	ll = ll1 = lt = lt1 = LL;
	smnt = nfonts = 5;	/* R I B BI S */
	specnames();	/* install names like "hyphen", etc. */
	if (eqflg)
		t.Adj = t.Hor;
}

char *skipstr(s)	/* skip over leading space plus string */
	char *s;
{
	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;
	while (*s != ' ' && *s != '\t' && *s != '\n')
		if (*s++ == '\\')
			s++;
	return s;
}

char *getstr(s, t)	/* find next string in s, copy to t */
	char *s, *t;
{
	int quote = 0;

	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;
	if (*s == '"') {
		s++;
		quote = 1;
	}
	for (;;) {
		if (quote && *s == '"') {
			s++;
			break;
		}
		if (!quote && (*s == ' ' || *s == '\t' || *s == '\n'))
			break;
		if (*s != '\\')
			*t++ = *s++;
		else {
			s++;	/* skip \\ */
			if (isdigit(s[0]) && isdigit(s[1]) && isdigit(s[2])) {
				*t++ = (s[0]-'0')<<6 | (s[1]-'0')<<3 | s[2]-'0';
				s += 2;
			} else if (isdigit(s[0])) {
				*t++ = *s - '0';
			} else if (*s == 'b') {
				*t++ = '\b';
			} else if (*s == 'n') {
				*t++ = '\n';
			} else if (*s == 'r') {
				*t++ = '\r';
			} else if (*s == 't') {
				*t++ = '\t';
			} else {
				*t++ = *s;
			}
			s++;
		}
	}
	*t = '\0';
	return s;
}

char *getint(s, pn)	/* find integer at s */
	char *s;
	int *pn;
{
	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;
	*pn = 0;
	while (isdigit(*s))
		*pn = 10 * *pn + *s++ - '0';
	return s;
}

specnames()
{
	static struct {
		int	*n;
		char	*v;
	} spnames[] = {
		&c_hyphen, "hy",
		&c_emdash, "em",
		&c_rule, "ru",
		&c_minus, "\\-",
		&c_fi, "fi",
		&c_fl, "fl",
		&c_ff, "ff",
		&c_ffi, "Fi",
		&c_ffl, "Fl",
		&c_acute, "aa",
		&c_grave, "ga",
		&c_under, "ul",
		&c_rooten, "rn",
		&c_boxrule, "br",
		&c_lefthand, "lh",
		&c_isalnum, "__",
		0, 0
	};
	int	i;

	for (i = 0; spnames[i].n; i++)
		*spnames[i].n = findch(spnames[i].v);
	if (c_isalnum == 0)
		c_isalnum = NROFFCHARS;
}


findch(s)	/* find char s in chname */
register char	*s;
{
	register int	i;

	for (i = 0; chtab[i] != 0; i++)
		if (strcmp(s, &chname[chtab[i]]) == 0)
			return(i + 128);
	return(0);
}

twdone()
{
	int waitf;

	oputs(t.twrest);
	flusho();
	if (pipeflg) {
		close(fileno(ptid));
		wait(&waitf);
	}
	if (ttysave != -1) {
		ttys.sg_flags = ttysave;
		stty(1, &ttys);
	}
}


ptout(i)
	tchar i;
{
	*olinep++ = i;
	if (olinep >= &oline[LNSIZE])
		olinep--;
	if (cbits(i) != '\n')
		return;
	olinep--;
	lead += dip->blss + lss - t.Newline;
	dip->blss = 0;
	esct = esc = 0;
	if (olinep > oline) {
		move();
		ptout1();
		oputs(t.twnl);
	} else {
		lead += t.Newline;
		move();
	}
	lead += dip->alss;
	dip->alss = 0;
	olinep = oline;
}


ptout1()
{
	register k;
	register char	*codep;
	extern char	*plot();
	int	w, j, phyw;
	tchar * q, i;
	static int oxfont = FT;	/* start off in roman */

	for (q = oline; q < olinep; q++) {
		i = *q;
		if (ismot(i)) {
			j = absmot(i);
			if (isnmot(i))
				j = -j;
			if (isvmot(i))
				lead += j;
			else 
				esc += j;
			continue;
		}
		if ((k = cbits(i)) <= 040) {
			switch (k) {
			case ' ': /*space*/
				esc += t.Char;
				break;
			case '\033':
			case '\007':
			case '\016':
			case '\017':
				oput(k);
				break;
			}
			continue;
		}
		phyw = w = t.Char * t.width[k];
		if (iszbit(i))
			w = 0;
		if (esc || lead)
			move();
		esct += w;
		xfont = fbits(i);
		if (xfont != oxfont) {
			switch (oxfont) {
			case ULFONT:	oputs(t.itoff); break;
			case BDFONT:	oputs(t.bdoff); break;
			case BIFONT:	oputs(t.itoff); oputs(t.bdoff); break;
			}
			switch (xfont) {
			case ULFONT:
				if (*t.iton & 0377) oputs(t.iton); break;
			case BDFONT:
				if (*t.bdon & 0377) oputs(t.bdon); break;
			case BIFONT:
				if (*t.bdon & 0377) oputs(t.bdon);
				if (*t.iton & 0377) oputs(t.iton);
				break;
			}
			oxfont = xfont;
		}
		if ((xfont == ULFONT || xfont == BIFONT) && !(*t.iton & 0377)) {
			for (j = w / t.Char; j > 0; j--)
				oput('_');
			for (j = w / t.Char; j > 0; j--)
				oput('\b');
		}
		if (!(*t.bdon & 0377) && ((j = bdtab[xfont]) || xfont == BDFONT || xfont == BIFONT))
			j++;
		else
			j = 1;	/* number of overstrikes for bold */
		if (k < 128) {	/* ordinary ascii */
			oput(k);
			while (--j > 0) {
				oput('\b');
				oput(k);
			}
		} else {
			int oj = j;
			codep = t.codetab[k-128];
			while (*codep != 0) {
				if (*codep & 0200) {
					codep = plot(codep);
					oput(' ');
				} else {
					if (*codep == '%')	/* escape */
						codep++;
					oput(*codep);
					if (*codep != '\b')
						for (j = oj; --j > 0; ) {
							oput('\b');
							oput(*codep);
						}
					codep++;
				}
			}
		}
		if (!w)
			for (j = phyw / t.Char; j > 0; j--)
				oput('\b');
	}
}


char	*plot(x)
char	*x;
{
	register int	i;
	register char	*j, *k;

	oputs(t.ploton);
	k = x;
	if ((*k & 0377) == 0200)
		k++;
	for (; *k; k++) {
		if (*k == '%') {	/* quote char within plot mode */
			oput(*++k);
		} else if (*k & 0200) {
			if (*k & 0100) {
				if (*k & 040)
					j = t.up; 
				else 
					j = t.down;
			} else {
				if (*k & 040)
					j = t.left; 
				else 
					j = t.right;
			}
			if ((i = *k & 037) == 0) {	/* 2nd 0200 turns it off */
				++k;
				break;
			}
			while (i--)
				oputs(j);
		} else 
			oput(*k);
	}
	oputs(t.plotoff);
	return(k);
}


move()
{
	register k;
	register char	*i, *j;
	char	*p, *q;
	int	iesct, dt;

	iesct = esct;
	if (esct += esc)
		i = "\0"; 
	else 
		i = "\n\0";
	j = t.hlf;
	p = t.right;
	q = t.down;
	if (lead) {
		if (lead < 0) {
			lead = -lead;
			i = t.flr;
			/*	if(!esct)i = t.flr; else i = "\0";*/
			j = t.hlr;
			q = t.up;
		}
		if (*i & 0377) {
			k = lead / t.Newline;
			lead = lead % t.Newline;
			while (k--)
				oputs(i);
		}
		if (*j & 0377) {
			k = lead / t.Halfline;
			lead = lead % t.Halfline;
			while (k--)
				oputs(j);
		} else { /* no half-line forward, not at line begining */
			k = lead / t.Newline;
			lead = lead % t.Newline;
			if (k > 0) 
				esc = esct;
			i = "\n";
			while (k--) 
				oputs(i);
		}
	}
	if (esc) {
		if (esc < 0) {
			esc = -esc;
			j = "\b";
			p = t.left;
		} else {
			j = " ";
			if (hflg)
				while ((dt = dtab - (iesct % dtab)) <= esc) {
					if (dt % t.Em)
						break;
					oput(TAB);
					esc -= dt;
					iesct += dt;
				}
		}
		k = esc / t.Em;
		esc = esc % t.Em;
		while (k--)
			oputs(j);
	}
	if ((*t.ploton & 0377) && (esc || lead)) {
		oputs(t.ploton);
		esc /= t.Hor;
		lead /= t.Vert;
		while (esc--)
			oputs(p);
		while (lead--)
			oputs(q);
		oputs(t.plotoff);
	}
	esc = lead = 0;
}


ptlead()
{
	move();
}


dostop()
{
	char	junk;

	flusho();
	read(2, &junk, 1);
}


newpage(){;}
pttrailer(){;}
