# include "stdio.h"
# include "assert.h"
# define TXTLEN 1000
char *outbuf = 0;
extern char *soutput;
extern int soutlen, iflong;
extern long indexdate;
baddrop(master, nf, fc, nitem, qitem, rprog, full)
	union ptr {unsigned *a; long *b; } master;
	FILE *fc;
	char *qitem[], *rprog;
{
/* checks list of drops for real bad drops;
   uses "deliv" to find items. */
int i, g, j, need, got, na, len;
long lp;
char res[100], *ar[50], output[TXTLEN];
extern int colevel, reached;
# if D1
if (iflong)
fprintf(stderr,"in baddrop, nf %d master %ld %ld %ld\n", nf, master.b[0], master.b[1], master.b[2]);
else
fprintf(stderr,"in baddrop, nf %d master %d %d %d\n", nf, master.a[0], master.a[1], master.a[2]);
# endif
for (i=g=0; i<nf; i++)
	{
	lp = iflong ? master.b[i] : master.a[i];
# if D1
	if (iflong)
	fprintf(stderr, "i %d master %lo lp %lo\n", i, master.b[i], lp);
	else
	fprintf(stderr, "i %d master %o lp %lo\n", i, master.a[i], lp);
# endif
	fseek (fc, lp, 0);
	fgets( res, 100, fc);
# if D1
	fprintf(stderr, "tag %s", res);
# endif
	if (!auxil(res,output))
		{
		char *s; int c;
# if D1
		fprintf(stderr, "not auxil try rprog %c\n", rprog? 'y': 'n');
# endif
		for(s=res; c= *s; s++)
			if (c == ';' || c == '\n')
				{*s=0; break;}
		len = rprog ?
			corout (res, output, rprog, 0, TXTLEN) :
			findline (res, output, TXTLEN, indexdate);
		}
# if D1
	_assert (len <TXTLEN);
	fprintf(stderr,"item %d of %d, tag %s len %d output\n%s\n..\n",i,nf,res, len,output);
# endif
	if (len==0)
		continue;
	need = colevel ? reached : nitem;
	na=0;
	ar[na++] = "fgrep";
	ar[na++] = "-r";
	ar[na++] = "-n";
	ar[na++] = need;
	ar[na++] = "-i";
	ar[na++] = output;
	ar[na++] = len;
	for(j=0; j<nitem; j++)
		ar[na++] = qitem[j];
# ifdef D1
	fprintf(stderr, "calling fgrep len %d ar[4] %s %o %d \n",len,ar[4],ar[5],ar[6]);
# endif
	if (fgrep(na, ar)==0)
		{
# ifdef D1
	fprintf(stderr, "fgrep found it\n");
# endif
		if (iflong)
			master.b[g++] = master.b[i];
		else
			master.a[g++] = master.a[i];
		if (full >= g)
			if (soutput==0)
				fputs(output, stdout);
			else
				strcpy (soutput, output);
		}
# ifdef D1
	fprintf(stderr, "after fgrep\n");
# endif
	}
return(g);
}
auxil( res, output)
	char *res, *output;
{
extern FILE *fd;
long lp, c; int len;
if (fd==0)return(0);
while (c = *res++) 
	{
	if (c == ';')
		{
		sscanf(res, "%ld,%d", &lp, &len);
		fseek (fd, lp, 0);
		fgets(output, len, fd);
		return(1);
		}
	}
return(0);
}
