# include "refer..c"
# define SAME 0
# define NFLAB 3000
# define NLABC 1000
static char sig[400];

static char bflab[NFLAB];
static char *labtab[NLABC];
char *lookat(), *artskp();
static char *lbp = bflab;
static char labc[NLABC];
static char stbuff[50];
static int  prevsig;

putsig (nf, flds, nref, nstline, endline)
	char *flds[], *nstline, *endline;
{
/* choose signal style */
char t[200], t1[200], t2[200], format[10], *sd;
char *fpar();
int another = 0;
int addon, addlet;
char *stline, *pr;
static FILE *fhide = 0;
if (labels)
	{
	if (nf==0) /* old */
		sprintf(t, "%s%c", labtab[nref], labc[nref]);
	else
		{
		*t=0;
		if (keywant)
			{
			pr = fpar(nf, flds, t1, keywant, 1, 0);
			if (pr) strcpy(t, pr);
			}
		if (t[0]==0)
			{
			if (labblkflg)
			sprintf(format, nmlen>0 ? "%%.%ds %%s" : "%%s %%s", nmlen);
			else
			sprintf(format, nmlen>0 ? "%%.%ds%%s" : "%%s%%s", nmlen);
# if D1
			fprintf(stderr, "format is /%s/\n", format);
# endif
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
# if D1
			fprintf(stderr, "tag is /%s/\n",t);
# endif
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
		addlet = keylet(t, nref);
		if (!keywant || addon)
			addch( t, addlet);
		}
	if (sort)
		sprintf(t, "%c%d%c", FLAG, nref, FLAG);
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
# if D1
fprintf(stderr, "bare %d fhide %o fo %o another %d\n",bare, fhide, fo, another);
# endif
if (bare==0)
	{
	if (another==0)
		{
		sprintf(t1, "%s%s\%s\n", stline, sig, endline);
		append(t1);
		flout();
		sig[0]=0;
		prevsig=0;
		if (fo != NULL && fo == fhide)
			{
			int ch;
# if D1
fprintf(stderr, "more hiding\n");
# endif
			fclose(fhide); fhide= fopen(hidenam, "r");
			fo= ftemp;
			while ((ch = getc(fhide)) != EOF)
				putc(ch, fo);
			fclose(fhide);
			unlink(hidenam);
# if D1
fprintf(stderr, "past this stuff\n");
# endif
			}
		}
	else
		{
		strcat (sig, (labels ? ", " : ",\\|"));
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
	if (control(flds[i][0]) &&flds[i][1]==c && ++fnd >= seq)
		{
		if (c== 'L')
			{
			p = flds[i]+3;
			strcpy(out, p);
			return(out);
			}
		if (c!= 'A' && c != 'D') /* if not author, date use first word */
			{
			p=flds[i]+3;
			p = artskp (p);
			mycpy2(out, p, 20);
			return(out);
			}
		if (c=='A' && lfirst(p=flds[i]+3)) /* author in style Jones, A. */
			{
			for(s=out; *p!=','; p++)
				*s++ = *p;
			*s++ = 0;
			if (prepend)
				{
				while (isspace(*p))p++;
				*s++ = *p;
				*s=0;
				}
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
	fprintf(fo, "%d%c", nref, sep);
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
	int c, nalph= 1;
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

static char *articles[] ={"the ", "an ", "a ", 0};
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
expkey (or, nr, fwrite)
	FILE *fwrite;
{
int uniq, less, i;
char *s;
# if D1
fprintf(stderr, "old %d key %s: '%c' new %d\n",or, labtab[or],labc[or],nr);
# endif
/* is this unique? how many are before it ? */
uniq=1; less= 'a';
s = labtab[or];
for(i=0; i <= refnum; i++)
	{
	if (i==or) continue;
	if (strcmp(labtab[i], s)!=SAME)
		continue;
	uniq=0;
	if (newr[i] != 0 && newr[i] < nr)
		less++;
	}
if (uniq)
	fprintf(fwrite, "%s", s);
else
	fprintf(fwrite,"%s%c", s, less);
}
lfirst(s)
	char *s;
{
/* decides if s is name of format Jones, A */
char *index(), *p;
p = index(s, ',');
if (p==NULL) return(0);
while (isspace(*++p)) ;
if (strncmp(p, "Jr", 2)==0)
	return(0);
if (strncmp(p, "II", 2)==0)
	return(0);
if (isupper(*p))
	return(1);
return(0);
}
