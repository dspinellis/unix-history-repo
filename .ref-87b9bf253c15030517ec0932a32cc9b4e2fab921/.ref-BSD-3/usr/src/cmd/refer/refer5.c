# include "refer..c"
# define SAME 0
# define NFLAB 2000
# define NLABC 100
static char sig[NLABC];

static char bflab[NFLAB];
static char *labtab[NLABC];
static char *lbp = bflab;
static char labc[NLABC];
static char stbuff[50];
static int  prevsig;

putsig (nf, flds, nref, nstline, endline)
	char *flds[], *nstline, *endline;
{
/* choose signal style */
char t[100], t1[100], t2[100], format[10], *sd;
int another = 0;
int addon;
char *stline;
static FILE *fhide = 0;
if (labels)
	{
	if (nf==0) /* old */
		sprintf(t, "%s%c", labtab[nref], labc[nref]);
	else
		{
		*t=0;
		if (keywant)
			sprintf(t, "%s", fpar(nf, flds, t1, keywant, 1, 0));
		if (t[0]==0)
			{
			sprintf(format, nmlen>0 ? "%%.%ds%%s" : "%%s%%s", nmlen);
			/* format is %s%s for default labels or %.3s%s eg if wanted */
			sd = fpar(nf, flds, t2, 'D', 1, 0);
			if (dtlen>0)
				{
				char *sdb;
				for(sdb=sd; *sd; sd++);
				sd = sd-dtlen;
				if (sd<sdb) sd=sdb;
				}
			sprintf(t, format, fpar(nf, flds, t1, 'A', 1, 0), sd);
			}
		if (keywant)
			{
			addon=0;
			for(sd=t; *sd; sd++);
			if (*--sd == '-')
				{
				addon=1;
				*sd=0;
				}
			}
		if (!keywant || addon)
			addch( t, keylet(t, nref));
		}
	}
else 
	{
	if (sort)
		sprintf(t, "%c%d%c", FLAG, nref, FLAG);
	else
		sprintf(t, "%d", nref);
	}
another = prefix (".[", sd=lookat());
if (another && (strcmp(".[\n", sd) != SAME))
	fprintf(stderr, "File %s, line %d- punctuation ignored from: %s", Ifile, Iline, sd);
strcat (sig, t);
# if D1
fprintf(stderr, "sig is now %s leng %d\n",sig,strlen(sig));
# endif
trimnl(nstline);
trimnl(endline);
stline=stbuff;
if (prevsig==0)
	{
	strcpy (stline, nstline);
	prevsig=1;
	}
if (stline[2] || endline[2])
	{
	stline += 2;
	endline += 2;
	}
else
	{
	stline  = "\\*([.";
	endline = "\\*(.]";
	}
if (bare==0)
	{
	if (another==0)
		{
		sprintf(t1, "%s%s\%s\n", stline, sig, endline);
		append(t1);
		flout();
		sig[0]=0;
		prevsig=0;
		if (fo == fhide)
			{
			int ch;
			fclose(fhide); fhide= fopen(hidenam, "r");
			fo= ftemp;
			while ((ch = getc(fhide)) != EOF)
				putc(ch, fo);
			fclose(fhide);
			unlink(hidenam);
			}
		}
	else
		{
		strcat (sig, ",\\|");
		/* hide if need be */
		if (fo == ftemp)
			{
			sprintf(hidenam, "/tmp/rj%dc", getpid());
# if D1
fprintf(stderr, "hiding in %s\n", hidenam);
# endif
			fhide= fopen(hidenam, "w");
			if (fhide==NULL) err("Can't get scratch file %s", hidenam);
			fo = fhide;
			}
		}
	}
if (bare<2)
	if (nf>0) fprintf(fo,".ds [F %s%c",t,sep);
if (bare>0)
	flout();
# if D1
fprintf(stderr, "sig is now %s\n",sig);
# endif
}
char *
fpar (nf, flds, out, c, seq, prepend)
	char *flds[], *out;
{
char *p, *s;
int i, fnd = 0;
for(i=0; i<nf; i++)
	if (flds[i][1]==c && ++fnd >= seq)
		{
		if (c=='T' || c == 'J') /* for titles use first word otherwise last */
			{
			p=flds[i]+3;
			if (prefix("A ", p)) p +=2;
			if (prefix("An ", p)) p +=3;
			if (prefix("The ", p)) p+= 4;
			mycpy2(out, p, 20);
			return(out);
			}
		for(s=p= flds[i]+2; *p; p++);
		while (p>s && *p != ' ') p--;
		/* special wart for authors */
		if (c=='A' && (p[-1] == ',' || p[1] =='('))
			{
			p--;
			while (p>s && *p != ' ') p--;
			mycpy (out, p+1);
			}
		else
			strcpy (out, p+1);
		if (c=='A' && prepend)
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
else
	{
	while (ctype= *keystr++)
		{
		count = atoi(keystr);
		if (*keystr=='+') count=999;
		if (count<=0) count=1;
		for(i=1; i<=count; i++)
			{
			sf= fpar(nf, flds, t1, ctype, i, 1);
			if (sf==0)
				break;
			sf = artskp(sf);
			fprintf(fo, "%s%c", sf, '-');
			}
		}
	fprintf(fo, "%c%d%c%c", FLAG, nref, FLAG, sep);
	}
}
keylet(t, nref)
	char *t;
{
int i;
int x = 'a'-1;
for(i=1; i<nref;i++)
	{
	if (strcmp(labtab[i], t) == 0)
		x = labc[i];
	}
strcpy(labtab[nref]=lbp, t);
while (*lbp++);
if (lbp-bflab >NFLAB)
	err("bflab overflow (%d)", NFLAB);
if (nref >NLABC)
	err ("nref in labc overflow (%d)", NLABC);
# ifdef D1
fprintf(stderr, "lbp up to %d of 2000\n", lbp-bflab);
# endif
return (labc[nref] = x+1);
}
mycpy(s,t)
	char *s, *t;
{
while (*t && *t != ',' && *t != ' ')
	*s++ = *t++;
*s=0;
}
mycpy2 (s, t, n)
	char *s, *t;
{
int c;
while (n-- && (c= *t++)>0)
	{
	if (c==' ')c= '-';
	*s++ = c;
	}
*s=0;
}
initadd(to, from, stop)
	char *to, *from, *stop;
{
	int c, nalph = 1;
while (*to) to++;
while (from<stop)
	{
	c = *from++;
	if (!isalpha(c))
		{
		if (nalph)
			*to++ = '.';
		nalph=0;
		continue;
		}
	if (nalph++ ==0)
		*to++ = c;
	}
*to=0;
}

static char *articles[] = {"the ", "an ", "a ", 0};
char *
artskp(s)
	char *s;
{
/* skips over initial "a ", "an ", or "the " in s */
	char **p, *r1, *r2;
for(p=articles; *p; p++)
	{
	r2 = s;
	for (r1= *p; ((*r1 ^ *r2) & ~040 ) == 0; r1++)
		r2++;
	if (*r1==0 && *r2 != 0)
		return(r2);
	}
return(s);
}
