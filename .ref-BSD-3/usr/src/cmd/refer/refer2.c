# include "refer..c"
extern FILE *in;
# define NFLD 30
# define TLEN 400
char one[ANSLEN];
int onelen = ANSLEN;
static char dr [100] = "";
doref(firline)
	char *firline;
{
char buff[QLEN], dbuff[3*QLEN], answer[ANSLEN], temp[TLEN];
char line[LLINE];
char *p, **sr, *flds[NFLD], *r;
int nf, nr, alph, query = 0, chp, digs;

/* get query */
buff[0] = dbuff[0] = 0;
while (input(line))
	{
	if (prefix(".]", line))
		break;
	if (control(line[0])) query=1;
	strcat (query  ? dbuff: buff, line);
	if (strlen(buff)>QLEN)
		err("buff too big (%d)", strlen(buff));
	assert (strlen(dbuff) <3*QLEN);
	}
if (strcmp (buff, "$LIST$\n")==0)
	{
# if D1
fprintf(stderr, "dump sorted list\n");
# endif
	assert ( dbuff[0]==0);
	dumpold();
	return;
	}
answer[0] = 0;
# ifdef D1
	fprintf(stderr, "query is %s\n",buff);
# endif
for( p=buff; *p; p++)
	{
	if (isupper(*p)) *p |= 040;
	}
alph = digs =0;
for(p=buff; *p; p++)
	{
	if (isalpha(chp = *p)) alph++;
	else
	if (isdigit(*p)) digs++;
	else
		{
		*p=0;
		if ( (alph+digs<3) || common(p-alph))
			{
			r = p-alph;
			while (r < p)
				*r++ = ' ';
			}
		if ( alph==0 && digs >0)
			{
			r = p-digs;
# if D1
fprintf(stderr, "number, %d long, text is %s\n",digs,r);
# endif
			if (digs != 4 || (atoi(r)/100 != 19))
				{
				while (r<p)
					*r++ = ' ';
				}
			}
		*p=' ';
		alph = digs = 0;
		}
	}
# ifdef D1
	fprintf(stderr, "query translated to %s\n", buff);
# endif
one[0]=0;
if (buff[0]) /* do not search if no query */
for( sr= rdata; sr < search; sr++)
	{
# ifdef D1
	fprintf(stderr, "now searching %s\n", *sr);
# endif
	temp[0]=0;
	corout (buff, temp, "hunt", *sr, TLEN);
	assert (strlen(temp)<TLEN);
	if (strlen(temp)+strlen(answer)>LLINE)
		err("Accumulated answers too large",0);
	strcat (answer, temp);
	if (strlen(answer)>LLINE)
		err("answer too long (%d)", strlen(answer));
	if (newline(answer) > 0) break;
	}
# if D1
fprintf(stderr, "answer:\n%s****\n", answer);
# endif
assert (strlen(one)<ANSLEN);
assert (strlen(answer)<ANSLEN);
if (buff[0])
switch (newline(answer))
	{
	case 0:
		fprintf (stderr, "No such paper %s\n", buff);
		return;
	default:
		fprintf(stderr, "too many hits for '%s'\n", trimnl(buff));
		choices(answer);
		p = buff;
		while (*p != '\n') p++;
		*++p=0;
	case 1:
# ifdef D1
	fprintf(stderr, "found one\n");
# endif
		if (endpush)
			if (nr = chkdup(answer))
				{
				if (bare<2)
					putsig (0, flds, nr, firline, line);
				return;
				}
# if D1
		fprintf(stderr, "one[0] was %o\n",one[0]);
# endif
		if (one[0]==0)
			corout (answer, one, "deliv", dr, QLEN);
# if D1
		fprintf(stderr, "one now %o\n",one[0]);
# endif
		break;
	}
assert (strlen(buff)<QLEN);
assert (strlen(one)<ANSLEN);
nf = tabs(flds, one);
nf += tabs(flds+nf, dbuff);
# if D1
fprintf(stderr, "one %.20s dbuff %.20s nf %d\n",one,dbuff, nf);
# endif
assert (nf < NFLD);
refnum++;
if (sort) putkey (nf, flds, refnum, keystr);
if (bare<2)
	putsig (nf, flds, refnum, firline, line);
else
	flout();
# if D1
fprintf(stderr, "put signal\n");
# endif
putref (nf, flds);
# if D1
fprintf(stderr, "put ref\n");
# endif
}
newline(s)
	char *s;
{
int k = 0, c;
while (c = *s++)
	if (c == '\n')
		k++;
return(k);
}
choices( buff )
	char *buff;
{
char ob[LLINE], *p, *r, *q, *t;
int nl;
for(r=p= buff; *p; p++)
	{
	if (*p == '\n')
		{
		*p++ = 0;
			{
			corout (r, ob, "deliv", dr, LLINE);
			nl = 1;
			for( q=ob; *q; q++)
				{
				if (nl && (q[0] == '.' || q[0] == '%') && q[1] == 'T')
					{
					q += 3;
					for (t=q; *t && *t != '\n'; t++);
					*t = 0;
					fprintf(stderr, "%.70s\n", q);
					q=0; break;
					}
				nl = *q == '\n';
				}
			if (q)
				fprintf(stderr, "??? at %s\n",r);
			}
		r=p;
		}
	}
}

control(c)
	{
	if (c=='.') return(1);
	if (c=='%') return(1);
	return(0);
	}
