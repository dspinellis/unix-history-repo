/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)flagger.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

char wds[100][40];
int synwd[100];
int mark[100];
int justw = 0;
extern int comcount;
int blank[100];
int wdp, wdf;
int bl;
int sargc; 
char **sargv;
FILE *inf;

main(argc,argv)
char *argv[];
{
	int i;
	while (--argc && **++argv== '-')
		switch(argv[0][1])
		{
		case 'w': 
			justw=1; 
			break;
		case 'c': 
			comcount=atoi(argv[0]+2); 
			break;
		}
	wdp=wdf=0;
	if (argc>0)
	{
		argc--;
		inf = fopen(argv[0], "r");
		if (inf==NULL) exit(0);
		argv++;
	}
	else
		inf=stdin;
	sargc=argc;  
	sargv= argv;
	while ( gw (wds[wdp = next(wdp)], &bl))
	{
		blank[wdp] = bl;
		mark[wdp]=0;
		synwd[wdp] = common(upcase(wds[wdp]));
		if (common(sstrip(upcase(wds[wdp]))))
			synwd[wdp]=1;
		if (allpunct(wds[wdp]))
			synwd[wdp]=1;
		if (strlen(wds[wdp])<3)
			synwd[wdp]=1;
		if (synwd[wdp]==1)
		{
			for(i=wdp; i!=wdf; i=prev(i))
			{
				if (synwd[i]>0)
					continue;
				mark[i]=1;
				break;
			}
		}
	}
	if (wdp<0) return(0);
	i=wdf -1;
	i = next(i);
	while (i != wdp)
		i= next(i);
}

next(i)
{
	int j;
	j = (i+1) % 100;
	if (j==wdf)
	{
		if (justw==0)
		{
			if (mark[j] ) putchar('*');
			printf("%s",wds[j]);
			if (blank[j]) putchar(' ');
		}
		else
			if (mark[j]) printf("%s\n", wds[j]);
		wdf = (wdf+1)%100;
	}
	return(j);
}

prev(i)
{
	i = (i-1)%100;
	return(i);
}

allpunct(s)
char *s;
{
	int c;
	while (c = *s++)
		if (isalpha(c))
			return(0);
	return(1);
}

gw(s, b)
char *s;
int *b;
{
	int c, type, nt;
	c = getc(inf);
	while (c==EOF)
	{
		fclose(inf);
		inf=NULL;
		if (sargc-->0)
		{
			inf = fopen ( *sargv++, "r");
		}
		if (inf==NULL) return(0);
		c = getc(inf);
	}
	*s++ = c;
	type = isalpha(c) || isdigit(c);
	while ( (c = getc(inf)) != EOF )
	{
		nt = isalpha(c) || isdigit(c);
		if (nt==type)
			*s++= c;
		else
			break;
	}
	*s=0;
	if (c== ' ')
	{
		*b = 1;
		return(1);
	}
	while (c==EOF)
	{
		fclose(inf); 
		inf=NULL;
		if (sargc-- > 0)
		{
			inf= fopen( *sargv++, "r");
		}
		if (inf==NULL) return(0);
		c = getc(inf);
	}
	ungetc(c, inf);
	*b=0;
	return(1);
}

trimnl(s)
char *s;
{
	while (*s) s++;
	if (*--s=='\n') *s=0;
}

upcase(s)
char *s;
{
	static char buf[100];
	strcpy (buf, s);
	for(s=buf; *s; s++)
		if (isupper(*s))
			*s = *s-'A'+'a';
	return(buf);
}

sstrip(s)
char *s;
{
	char *p ; 
	p=s;
	while (*s) s++;
	if (*--s=='s') *s=0;
	return(p);
}
