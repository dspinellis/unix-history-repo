# include "stdio.h"
int nh 500;
int saw[6000];
char *comname "/usr/lib/eign";

main (argc,argv)
	char *argv[];
{

int i, z;
char *name;

FILE *f;

while (argc>1 && argv[1][0] == '-')
	{
	switch(argv[1][1])
		{
		case 'h':
			nh = atoi(argv[1]+2); break;
		}
	argc--; argv++;
	}
if (argc<=1)
	dofile(stdin, "");
else
for(i=1; i<argc; i++)
	{
	f = fopen(name=argv[i], "r");
	if (f==NULL)
		err("No file %s",name);
	else
		dofile(f, name);
	}
for(z=i=0; i<nh; i++)
	{
	if (saw[i]) z++;
	}
printf("hashes %d used %d\n",nh,z);
}
# include "stdio.h"

dofile(f, name)
	FILE *f;
	char *name;
{

/* read file f & spit out keys & ptrs */
# define MAXLINE 750
char line[MAXLINE], *s;
char key[20], *p;
int k 0;
int c, lim;
int alph 0;
int used 0;
long lp 0;

while (fgets(line, MAXLINE, f))
	{
	k++;
	used=alph=0;
	lim = strlen(line);
	p = key;
	for(s=line; c= *s; s++)
		{
		if (isalpha(c) || isdigit(c))
			{
			if (alph++ < 6)
				*p++ = c;
			}
		else
			{
			*p = 0;
			if (outkey(p=key))
				{
				tkey(key,k);
				used=1;
				}
			alph=0;
			}
		}
	lp += lim;
	}
}

outkey( ky)
	char *ky;
{
	int n;
n = strlen(ky);
if (n<3) return(0);
if (isdigit(ky[0]))
	if (ky[0] != '1' || ky[1] != '9' || n!= 4) return(0);
return(1);
}
# include "stdio.h"
hash (s)
	char *s;
{
int c, n, q;
for(q=n=0; c= *s; s++)
	n += (c*n + c << (n%4));
return(n);
}
err (s, a)
	char *s;
{
fprintf(stderr, "Error: ");
fprintf(stderr, s, a);
putc('\n', stderr);
}
prefix(t, s)
	char *t, *s;
{
int c, d;
while ( (c= *t++) == *s++)
	if (c==0) return(1);
return(c==0 ? 1: 0);
}
mindex(s, c)
	char *s;
{
register char *p;
for( p=s; *p; p++)
	if (*p ==c)
		return(p);
return(0);
}
tkey(s,nw)
	char *s;
{
int x;
x = abs(hash(s)) % nh;
/* if (saw[x]) printf("%d %d\n", x, nw); */
saw[x]= nw;
}
abs(n)
{
return(n>0 ? n : -n);
}
