#ifndef lint
static char *sccsid = "@(#)refer5.c	4.8 (Berkeley) %G%";
#endif

#include "refer..c"
#define SAME 0
#define NFLAB 3000		/* number of bytes to record all labels */
#define NLABC 1000		/* max number of labels */

static char sig[MXSIG];
static char bflab[NFLAB];
static char *labtab[NLABC];
static char *lbp = bflab;
static char labc[NLABC];
static char stbuff[50];
static int  prevsig;

putsig (nf, flds, nref, nstline, endline, toindex)   /* choose signal style */
char *flds[], *nstline, *endline;
{
	char t[100], t1[MXSIG], t2[100], format[10], *sd, *stline;
	int addon, another = 0;
	static FILE *fhide = 0;
	int i;
	char tag;

	if (labels) {
		if (nf == 0)	/* old */
			sprintf(t, "%s%c", labtab[nref], labc[nref]);
		else {
			*t = 0;
			if (keywant)
				sprintf(t, "%s", fpar(nf,flds,t1,keywant,1,0));
			if (science && t[0] == 0) {
				sd = fpar(nf, flds, t2, 'D', 1, 0);
				sprintf(t, "%s, %s", fpar(nf,flds,t1,'A',1,0),
					sd);
			}
			else if (t[0] == 0) {
				sprintf(format,
					nmlen>0 ? "%%.%ds%%s" : "%%s%%s",
					nmlen);
				/* format is %s%s for default labels */
				/* or %.3s%s eg if wanted */
				sd = fpar(nf, flds, t2, 'D', 1, 0);
				if (dtlen > 0) {
					char *sdb;
					for (sdb = sd; *sd; sd++)
						;
					sd = sd - dtlen;
					if (sd < sdb)
						sd = sdb;
				}
				sprintf(t, format, fpar(nf,flds,t1,'A',1,0),
					sd);
			}
			if (keywant) {
				addon = 0;
				for (sd = t; *sd; sd++)
					;
				if (*--sd == '-') {
					addon = 1;
					*sd = 0;
				}
			}
			if ((!keywant || addon) && !science) {
			    addch(t, keylet(t, nref));
			}
			else {
			    tokeytab (t,nref);
			}
		}
	}
	else {
		sprintf(t, "%c%d%c", FLAG, nref, FLAG);
	}
	another = prefix (".[", sd=lookat());
	if (another && (strcmp(".[\n", sd) != SAME))
		fprintf(stderr, "File %s line %d: punctuation ignored from: %s",
			Ifile, Iline, sd);
	if ((strlen(sig) + strlen(t)) > MXSIG)
		err("sig overflow (%d)", MXSIG);
	strcat(sig, t);
#if EBUG
	fprintf(stderr, "sig is now %s leng %d\n",sig,strlen(sig));
#endif
	trimnl(nstline);
	trimnl(endline);
	stline = stbuff;
	if (prevsig == 0) {
		strcpy (stline, nstline);
		prevsig=1;
	}
	if (stline[2] || endline[2]) {
		stline += 2;
		endline += 2;
	}
	else {
		stline  = "\\*([.";
		endline = "\\*(.]";
	}
	if (science) {
		stline = " (";
		endline = ")";
	}
	if (bare == 0) {
		if (!another) {
			sprintf(t1, "%s%s\%s\n", stline, sig, endline);
			if (strlen(t1) > MXSIG)
				err("t1 overflow (%d)", MXSIG);
			append(t1);
			flout();
			sig[0] = 0;
			prevsig = 0;
			if (fo == fhide) {
				int ch;
				fclose(fhide); 
				fhide = fopen(hidenam, "r");
				fo = ftemp;
				while ((ch = getc(fhide)) != EOF)
					putc(ch, fo);
				fclose(fhide);
				unlink(hidenam);
			}
		}
		else {
			if (labels) {
				strcat(sig, ",\\|");
			} else {
				/*
				 * Seperate reference numbers with AFLAG
				 * for later sorting and condensing.
				 */
				t1[0] = AFLAG;
				t1[1] = '\0';
				strcat(sig, t1);
			}
			if (fo == ftemp) {	/* hide if need be */
				sprintf(hidenam, "/tmp/rj%dc", getpid());
#if EBUG
				fprintf(stderr, "hiding in %s\n", hidenam);
#endif
				fhide = fopen(hidenam, "w");
				if (fhide == NULL)
					err("Can't get scratch file %s",
						hidenam);
				fo = fhide;
			}
		}
	}
	if (bare < 2)
		if (nf > 0 && toindex)
			fprintf(fo,".ds [F %s%c",t,sep);
	if (bare > 0)
		flout();
#if EBUG
	fprintf(stderr, "sig is now %s\n",sig);
#endif
}

char *
fpar (nf, flds, out, c, seq, prepend)
char *flds[], *out;
{
	char *p, *s;
	int i, fnd = 0;

	for(i = 0; i < nf; i++)
		if (flds[i][1] == c && ++fnd >= seq) {
			/* for titles use first word otherwise last */
			if (c == 'T' || c == 'J') {
				p = flds[i]+3;
				if (prefix("A ", p))
					p += 2;
				if (prefix("An ", p))
					p += 3;
				if (prefix("The ", p))
					p += 4;
				mycpy2(out, p, 20);
				return(out);
			}
			/* if its not 'L' then use just the last word */
			s = p = flds[i]+2;
			if (c != 'L') {
			    for(; *p; p++);
			    while (p > s && *p != ' ')
				    p--;
			}
			/* special wart for authors */
			if (c == 'A' && (p[-1] == ',' || p[1] =='(')) {
				p--;
				while (p > s && *p != ' ')
					p--;
				mycpy(out, p+1);
			}
			else
				strcpy(out, p+1);
			if (c == 'A' && prepend)
				initadd(out, flds[i]+2, p);
			return(out);
		}
	return(0);
}

putkey(nf, flds, nref, keystr)
char *flds[], *keystr;
{
	char t1[50], *sf;
	int ctype, i, count;

	fprintf(fo, ".\\\"");
	if (nf <= 0)
		fprintf(fo, "%s%c%c", labtab[nref], labc[nref], sep);
	else {
		while (ctype = *keystr++) {
			count = atoi(keystr);
			if (*keystr=='+')
				count=999;
			if (count <= 0)
				count = 1;
			for(i = 1; i <= count; i++) {
				sf = fpar(nf, flds, t1, ctype, i, 1);
				if (sf == 0)
					break;
				sf = artskp(sf);
				fprintf(fo, "%s%c", sf, '-');
			}
		}
		fprintf(fo, "%c%d%c%c", FLAG, nref, FLAG, sep);
	}
}


tokeytab (t, nref)
char *t;
{
	strcpy(labtab[nref]=lbp, t);
	while (*lbp++)
		;
}

keylet(t, nref)
char *t;
{
	int i;
	int x = 'a' - 1;

	for(i = 1; i < nref; i++) {
		if (strcmp(labtab[i], t) == 0)
			x = labc[i];
	}
	tokeytab (t, nref);
	if (lbp-bflab > NFLAB)
		err("bflab overflow (%d)", NFLAB);
	if (nref > NLABC)
		err("nref in labc overflow (%d)", NLABC);
#if EBUG
	fprintf(stderr, "lbp up to %d of %d\n", lbp-bflab, NFLAB);
#endif
	return(labc[nref] = x+1);
}

mycpy(s, t)
char *s, *t;
{
	while (*t && *t != ',' && *t != ' ')
		*s++ = *t++;
	*s = 0;
}

mycpy2(s, t, n)
char *s, *t;
{
	int c;

	while (n-- && (c= *t++) > 0) {
		if (c == ' ')
			c = '-';
		*s++ = c;
	}
	*s = 0;
}

initadd(to, from, stop)
char *to, *from, *stop;
{
	int c, nalph = 1;

	while (*to)
		to++;
	while (from < stop) {
		c = *from++;
		if (!isalpha(c)) {
			if (nalph)
				*to++ = '.';
			nalph = 0;
			continue;
		}
		if (nalph++ == 0)
			*to++ = c;
	}
	*to = 0;
}

static char *articles[] = {
	"the ", "an ", "a ", 0
};

char *
artskp(s)	/* skips over initial "a ", "an ", "the " in s */
char *s;
{

	char **p, *r1, *r2;

	for (p = articles; *p; p++) {
		r2 = s;
		for (r1 = *p; ((*r1 ^ *r2) & ~040 ) == 0; r1++)
			r2++;
		if (*r1 == 0 && *r2 != 0)
			return(r2);
	}
	return(s);
}
