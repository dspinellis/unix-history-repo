#ifndef lint
static char *sccsid = "@(#)refer6.c	4.1 (Berkeley) 5/6/83";
#endif

#include "refer..c"
#define dsde (macro? "de" : "ds")
#define ifnl (macro? sep : ' ')

putref(n, tvec)
char *tvec[];
{
	char *s, *tx;
	char buf1[BUFSIZ], buf2[50];
	int nauth = 0, i, lastype = 0, cch, macro = 0, la;
	int lauth = 0, ltitle = 0, lother = 0;

	fprintf(fo, ".]-%c", sep);
	for (i = 0; i < n; i++) {
		s = tvec[i];
		if (*s == 0)
			continue;
		if (control(s[0])) {
			if (lastype && macro)
				fprintf(fo, "..%c", sep);
			if (control(s[1])) {
				cch = s[2];
				tx = s+3;
				macro = 1;
			}
			else {
				cch = s[1];
				tx = s+2;
				macro = 0;
			}
		}
		else {
			cch = lastype;
			tx = s;
		}
#if EBUG
		fprintf(stderr, "smallcaps %s cch %c\n",smallcaps, cch);
#endif
		if (mindex(smallcaps, cch))
			tx = caps(tx, buf1);
#if EBUG
		fprintf(stderr, " s %o tx %o %s\n",s,tx,tx);
#endif
		if (!control(s[0])) {	/* append to previous item */
			if (lastype != 0) {
				if (macro)
					fprintf(fo, "%s%c", tx, sep);
				else
					fprintf(fo, ".as [%c \" %s%c",lastype,tx,sep);
				if (lastype == 'T')
					ltitle = (mindex(".;,?", last(tx))!=0);
				if (lastype == 'A')
					lauth = last(tx) == '.';
			}
			continue;
		}
		if (mindex("XYZ[]", cch)) {	/* skip these */
			lastype = 0;
			continue;
		}
		else {
			if (cch == 'A') {
				if (nauth < authrev)
					tx = revauth(tx, buf2);
				if (nauth++ == 0)
					if (macro)
						fprintf(fo,
						".de [%c%c%s%c",cch,sep,tx,sep);
					else
						fprintf(fo,
						".ds [%c%s%c", cch,tx,sep);
				else {
					la = (tvec[i+1][1]!='A');
					fprintf(fo, ".as [A \"");
					if (la == 0 || nauth != 2)
						fprintf(fo, ",");
					if (la)
						fprintf(fo,"%s", 
						mindex(smallcaps, 'A') ? " \\s-2AND\\s+2" : " and");
					fprintf(fo, "%s%c", tx, sep);
				}
				lauth = last(tx) == '.';
			}
			else {
				if (macro)
					fprintf(fo,
						".de [%c%c%s%c",cch,sep,tx,sep);
				else
					fprintf(fo, ".ds [%c%s%c",cch,tx, sep);
			}
		}
		if (cch == 'P')
			fprintf(fo, ".nr [P %d%c", mindex(s, '-')!=0, sep);
		lastype = cch;
		if (cch == 'T')
			ltitle = (mindex(".;,?", last(tx)) != 0);
		if (cch == 'O')
			lother = (mindex(".;,?", last(tx)) != 0);
	}
	if (lastype && macro)
		fprintf(fo, "..%c", sep);
	fprintf(fo, ".nr [T %d%c", ltitle, sep);
	fprintf(fo, ".nr [A %d%c", lauth, sep);
	fprintf(fo, ".nr [O %d%c", lother, sep);
	fprintf(fo, ".][ %s%c", class(n, tvec), '\n');
}

tabs (sv, line)
char *sv[], *line;
{
	char *p;
	int n = 0;

	sv[n++] = line;
	for (p = line; *p; p++) {
		if (*p == '\n') {
			*p = 0;
			sv[n++] = p+1;
		}
	}
	return(n-1);
}

char *
class (nt, tv)
char *tv[];
{
	if (hastype (nt, tv, 'J'))
		return("1 journal-article");
	if (hastype (nt, tv, 'B'))
		return("3 article-in-book");
	if (hastype (nt, tv, 'R'))
		return ("4 tech-report");
	if (hastype (nt, tv, 'G'))
		return ("4 tech-report");
	if (hastype (nt, tv, 'I'))
		return("2 book");
	if (hastype (nt, tv,'M'))
		return ("5 bell-tm");
	return("0 other");
}

hastype (nt, tv, c)
char *tv[];
{
	int i;
	for (i = 0; i < nt; i++)
		if (control(tv[i][0]) && tv[i][1]==c )
			return(1);
	return(0);
}

char *
caps(a, b)
char *a, *b;
{
	char *p;
	int c, alph, this;

	p = b;
	alph = 0;
	while (c = *a++) {
		this = isalpha(c);
		if (this && alph == 1) {
			*b++ = '\\';
			*b++ = 's';
			*b++ = '-';
			*b++ = '2';
		}
		if (!this && alph > 1) {
			*b++ = '\\';
			*b++ = 's';
			*b++ = '+';
			*b++ = '2';
		}
		if (this)
			c &= (~040);
		*b++ = c;
		alph = this ? alph+1 : 0;
	}
	if (alph > 1) {
		*b++ = '\\';
		*b++ = 's';
		*b++ = '+';
		*b++ = '2';
	}
	*b = 0;
	return(p);
}

char *
revauth(s, b)
char *s, *b;
{
	char *init, *name, *jr, *p, *bcop;

	bcop = b;
	init = name = s;
	while (*name)
		name++;
	jr = name;
	while (name > init && *name!= ' ')
		name--;
	if (name[-1] == ',' || name[-1]== '(' ) {
		jr = --name;
		while (name>init && *name != ' ')
			name--;
	}
	p = name;
	while (p < jr)
		*b++ = *p++;
	*b++ = ',';
	while (init < name)
		*b++ = *init++;
	if (*jr)
		jr++;
	while(*jr)
		*b++ = *jr++;
	*b++ = 0;
	return(bcop);
}

last(s)
char *s;
{
	while (*s)
		s++;
	return(*--s);
}
