# include "what..c"
struct wst { char *tx; int ct; } ;
# define NW 5
# define ZIPF 10
# define HASHF 3
# define WLEN 10
# define SAME 0
# define TSIZE HASHF*ZIPF*NW
int HSIZE;
static struct wst word[TSIZE];
static char tbuf[NW*ZIPF*WLEN], *tp tbuf;
# define NF 10

freqwd ( fn, wd, nin )
	char *fn[], *wd[];
{
	FILE *fi[NF];
	int nw 0, i, any, nf, j, wexch(), wcomp();
	char tw[20];
for(HSIZE=TSIZE; !prime(HSIZE); HSIZE--);
for(nf=0; fn[nf] && nf<NF; nf++)
	fi[nf] = fn[nf][0] ? fopen(fn[nf], "r") : NULL;
do {
	any=0;
	for(i=0; i<nf; i++)
		{
		if (fi[i]==NULL) continue;
		if (gw(fi[i], tw)==0)
			{
			fclose(fi[i]);
			fi[i]==NULL;
			continue;
			}
		any=1;
		if (common(tw)) continue;
		if (strlen(tw)<3) continue;
		j = lookup (tw);
		if (j<0 && nw < ZIPF*NW)
			{
			j = -j;
			strcpy (tp, tw);
			word[j].tx = tp;
			while (*tp++);
			_assert (tp < tbuf+NW*ZIPF*WLEN);
			word[j].ct = 1;
			nw++;
			}
		else if (j>0)
			word[j].ct++;
		}
	} while (any>0);
shell ( TSIZE, wcomp, wexch );
for(nw=0; word[nw].ct >0 && nw<TSIZE; nw++)
	if (nw>=nin*2 && word[nw].ct != word[0].ct)
		break;
for(i=0; i<nw; i++)
	wd[i] = word[i].tx;
return(nw);
}

lookup (wt)
	char *wt;
{
int h;
h = hash(wt);
for( h = h%HSIZE; word[h].tx; h = (h+1)%HSIZE)
	{
	if (h==0) continue;
	if (strcmp(wt, word[h].tx) == SAME)
		return (h);
	}
return ( -h );
}

hash (s)
	char *s;
{
int k 0, c 0, i 0;
while ( c = *s++ )
	k ^= (c << (i++%5) );
return (k>0 ? k : -k);
}

gw (f, t)
	char *t;
	FILE *f;
{
int start 1, oldc ' ', c;
if (f==NULL) return (0);
while ( (c=getc(f)) != EOF)
	{
	if (isupper(c)) c= tolower(c);
	if (start==1)
		if (!alphanum(c, oldc))
			continue;
		else
			start=0;
	if (start==0)
		if (alphanum(c, oldc))
			*t++ = c;
		else
			{
			*t=0;
			return(1);
			}
	oldc=c;
	}
return(0);
}

alphanum( c, oldc )
{
if (isalpha(c) || isdigit(c)) return(1);
if (isalpha(oldc))
	if (c== '\'' || c == '-') return(1);
return(0);
}

wcomp (n1, n2)
{
return (word[n1].ct >= word[n2].ct);
}

wexch (n1, n2)
{
struct wst tt;
tt.tx = word[n1].tx; tt.ct = word[n1].ct;
word[n1].tx = word[n2].tx; word[n1].ct = word[n2].ct;
word[n2].tx = tt.tx; word[n2].ct = tt.ct;
}

prime(n)
{
/* only executed once- slow is ok */
int i;
if (n%2==0) return(0);
for(i=3; i*i<=n; i+= 2)
	if (n%i ==0 ) return(0);
return(1);
}
trimnl(s)
	char *s;
{
	while (*s)s++;
	if (*--s=='\n') *s=0;
}


/* this is the test for what4.c as a standalone prog ...
main (argc, argv)
	char *argv[];
{
char *ff[10], *wd[20], **ffp ff;
int n, i;
while (--argc)
	*ffp++ = *++argv;
*ffp=0;
n=freqwd(ff,wd);
for(i=0; i<n; i++)
 printf("%s\n",wd[i]);
printf("total of %d items\n",n);
}
 /* .... */
