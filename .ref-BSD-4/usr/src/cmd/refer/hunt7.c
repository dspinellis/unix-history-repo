# include "stdio.h"
# define SAME 0
# include "assert.h"
# define FGCT 10
# define FGSIZE 150
int keepold = 1; /* keep old things for fgrep search */
char fgspace[FGSIZE];
char *fgp = fgspace;
char *fgnames[FGCT];
char **fgnamp = fgnames;
findline(in, out, outlen, indexdate)
	long indexdate;
	char *in, *out;
{
static char name[100] = "";
char *p, **ftp;
extern long gdate();
static FILE *fa = NULL;
long lp, llen;
int len, k, nofil;

# if D1
	fprintf(stderr, "findline: %s\n", in);
# endif
	if (mindex(in, '!'))
		return(remote(in, out));
	nofil = in[0]==0;
	for(p=in; *p && *p != ':' && *p != ';'; p++)
		;
	if (*p) *p++=0;
	else p=in;
	k = sscanf(p, "%ld,%ld", &lp, &llen);
# ifdef D1
	fprintf(stderr, "p %s k %d lp %ld llen %ld\n",p,k,lp,llen);
# endif
	if (k<2)
		{
		lp = 0;
		llen=outlen;
		}
# ifdef D1
	fprintf(stderr, "lp %ld llen %ld\n",lp, llen);
# endif
# ifdef D1
fprintf(stderr, "fa now %o, p %o in %o %s\n",fa, p,in,in);
# endif
	if (nofil)
{
# if D1
fprintf(stderr, "set fa to stdin\n");
# endif
		fa = stdin;
}
	else
	if (strcmp (name, in) != 0 || 1)
		{
# if D1
fprintf(stderr, "old: %s new %s not equal\n",name,in);
# endif
		if (fa != NULL)
			fa = freopen(in, "r", fa);
		else
			fa = fopen(in, "r");
# if D1
		if (fa==NULL) fprintf(stderr, "failed to (re)open *%s*\n",in);
# endif
		if (fa == NULL)
			return(0);
			/* err("Can't open %s", in); */
		strcpy(name, in);
		if (gdate(fa) > indexdate && indexdate != 0)
			{
			if (keepold)
				{
				for(ftp=fgnames; ftp<fgnamp; ftp++)
					if (strcmp(*ftp, name)==SAME)
						return(0);
				strcpy (*fgnamp++ = fgp, name);
				assert(fgnamp<fgnames+FGCT);
				while (*fgp && *fgp!=':')
					fgp++;
				*fgp++ = 0;
				assert (fgp<fgspace+FGSIZE);
				return(0);
				}
			fprintf(stderr, "Warning: index predates file '%s'\n", name);
			}
		}
# if D1
	else
		fprintf(stderr, "old %s new %s same fa %o\n", name,in,fa);
# endif
	if (fa != NULL)
		{
		fseek (fa, lp, 0);
		len = (llen >= outlen) ? outlen-1 : llen;
		len = fread (out, 1, len, fa);
		out[len] = 0;
# ifdef D1
	fprintf(stderr, "length as read is %d\n",len);
# endif
		}
	return(len);
}
