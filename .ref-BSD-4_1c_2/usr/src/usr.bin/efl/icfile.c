#include "defs"

struct { char chars[ 10 ]; };


crii()	/* create names for intermediate files */
{

#ifdef unix
sprintf(icfile->filename, "eflc.%d", getpid());
sprintf(idfile->filename, "efld.%d", getpid());
sprintf(iefile->filename, "efle.%d", getpid());
#endif

#ifdef gcos
sprintf(icfile->filename, "code.efl");
sprintf(idfile->filename, "data.efl");
sprintf(iefile->filename, "equv.efl");
#endif
}



rmiis()
{
rmii(icfile);
rmii(idfile);
rmii(iefile);
}




rmii(p)		/* discard the intermediate file */
struct fileblock *p;
{
#ifdef unix
if(p)
	{
	fclose(p->fileptr);
	unlink(p->filename);
	}
#endif

#ifdef gcos
if(p)
	fclose(p->fileptr, "d");
#endif
}


opiis()
{
opii(icfile);
opii(idfile);
opii(iefile);
}




opii(p)	/* open the intermediate file for writing */
struct fileblock *p;
{

#ifdef unix
if( (p->fileptr = fopen(p->filename, "w")) == NULL)
	fatal("cannot open intermediate file");
#endif

#ifdef gcos
if( (p->fileptr = fopen(p->filename, "wi")) == NULL)
	fatal("cannot open intermediate file");
#endif

}



swii(p)
struct fileblock *p;
{
iifilep = p;
}



putii(w,n)
int *w, n;
{
if( fwrite(w,sizeof(int),n, iifilep->fileptr) != n)
	fatal("write error");
}



getii(w, n)
int *w, n;
{
if( fread(w,sizeof(int), n, iifilep->fileptr) != n)
	fatal("read error");
}




cliis()
{
clii(icfile);
clii(idfile);
clii(iefile);
}




clii(p)	/* close the intermediate file */
struct fileblock *p;
{
#ifdef unix
fclose(p->fileptr);
#endif

#ifdef gcos
fclose(p->fileptr, "rl");
#endif
}



rewii(p)	/* close and rewind the intermediate file for reading */
struct fileblock *p;
{
swii(p);
putic(ICEOF,0);
clii(p);

#ifdef unix
if( (p->fileptr = fopen(p->filename, "r")) == NULL)
	fatal("cannot open intermediate file");
#endif

#ifdef gcos
if( (p->fileptr = fopen(p->filename, "ri")) == NULL)
	fatal("cannot open intermediate file");
#endif
}



putic(c,p)
int c;
int p;
{
int w[2];
prevbg = (c==ICINDENT);
w[0] = c;
w[1] = p;
putii(w,2);
}


getic(p)
int *p;
{
int w[2];

getii(w,2);
*p = w[1];
return( w[0] );
}



putsii(l, p)
int l;
char *p;
{
int word;
register int i, m, n;

n = strlen(p);
putic(l, n);
m = (n/sizeof(int)) ;
while(m-- > 0)
	{
	for(i=0 ; i<sizeof(int); ++i)
		word.chars[i] = *p++;
	putii(&word, 1);
	}
n -= (n/sizeof(int))*sizeof(int);
if(n > 0)
	{
	for(i=0 ; i<n ; ++i)
		word.chars[i] = *p++;
	putii(&word,1);
	}
}




ptr getsii(n)
int n;
{
static int incomm[100];
int m;
register int *q, *qm;
char *p;

m = (n + sizeof(int)-1 ) / sizeof(int);
q = incomm;
qm = q + m;

while(q < qm)
	getii(q++, 1);
p = incomm;
p[n] = '\0';

return(incomm);
}
