#ifndef lint
static char *sccsid = "@(#)what2.c	4.2 (Berkeley) 9/11/87";
#endif

#include "stdio.h"
#include "ctype.h"
#define NS 5

struct sf {
	char *text;
	int olap;
} 
sents[NS];
struct sf *sp;
char stext[NS][500];

describe (file, argc, argv, rf)
char *file, *argv[];
FILE *rf;
{
	int ns = 0;
	char linbuf[BUFSIZ], *line, *p;
	int i, wrflg = 0, wrote = 0, ln = 0;
	FILE *fi;
	fi = fopen(file, "r");
	if (fi==NULL) return;
	for(i=1; i<argc; i++)
		lcase(argv[i]);
	while (gsent(linbuf, BUFSIZ, fi))
	{
		wrote=0;
		for(line=linbuf; *line==' '; line++);
		if (line[0]==0) continue;
		for(p=line; *p; p++)
			if (*p=='\t') *p= ' ';
		if (wrflg && line[0]=='.' && isupper(line[1]))
			wrflg=0;
		if (wrflg)
		{
			output(line, ln, rf);
			wrote=1;
		}
		if (prefix(".TL", line))
			wrflg=1;
		if (prefix(".AU", line))
			wrflg = ln = 1;
		if (prefix(".DA", line) || prefix(".ND", line))
			output(line+4, 1, rf);
		if (line[0]=='.')
			continue;
		if (wrote) continue;
		ns=update(ns, line, count(line,argc,argv));
	}
	fclose(fi);
	for(sp=sents; sp<sents+ns; sp++)
		output(sp->text, 0, rf);
}

int state = 0;
int oldc = '\n';

gsent(buf, bsize, fi)
char *buf;
FILE *fi;
{
	char *s;
	int c, leng = 0;
	/* state
		0: looking for '.' 
		1: looking for nl or space aftter '.'
		2: looking for nl after line with dot.
		*/
	s=buf;
	if (state==2)
		*s++='.';
	while ( (c = getc(fi)) > 0 )
	{
		switch(state)
		{
		case 0: /* normal */
			if (c=='.' && oldc == '\n')
			{
				*s=0;
				state=2;
				oldc='\n';
				return(1);
			}
			*s++ = (c=='\n'? ' ': c);
			if (s>=buf+bsize)
			{
				*--s = 0;
				return(1);
			}
			if (c=='.' || c == '?' || c=='!')
				if (leng>1)
					state=1;
			leng = (isalpha(c) ? leng+1 : 0);
			break;
		case 1: /* found ., want nl or space */
			if (c==' ' || c == '\n')
			{
				*s=0;
				state=0;
				oldc=c;
				return(1);
			}
			*s++ = (c=='\n' ? ' ' : c);
			state=0;
			leng = 0;
			break;
		case 2: /* found trof line, want nl */
			if (c == '\n')
			{
				*s=0;
				state=0;
				oldc='\n';
				return(1);
			}
			*s++ = c;
			break;
		}
		oldc=c;
	}
	*s=0;
	return(0);
}

prefix( p, s)
char *p, *s;
{
	int c;
	while ( (c= *p++) == *s++)
		if (c==0)
			return(1);
	return(c==0);
}

output (s, ln, rf)
char *s;
FILE *rf;
{
	char *t;
	int more = 1;
	t=s;
	while (more)
	{
		while (t<s+72 && *t)
			t++;
		if (*t)
		{
			while (*t != ' ' && t>(s+25))
				t--;
			*t=0;
			more=1;
		}
		else
			more=0;
		printf("%s%s\n",ln++ ? "     " : "   ", s);
		if (rf!=NULL)
			fprintf(rf, "%s\n", s);
		s= ++t;
	}
}

count(isent, nw, wds)
char *wds[], *isent;
{
	int saw[50], ct;
	char sb[BUFSIZ], *s = sb;
	int i, c;
	for(i=1; i<nw; i++)
		saw[i]=0;
	while (c = *isent++)
	{
		*s++ = isupper(c) ? tolower(c) : c;
	}
	*s=0;
	s=sb;
	while (*s++)
	{
		if (s[-1]!=' ') continue;
		for(i=1; i<nw; i++)
		{
			if (saw[i])continue;
			if (prefix(wds[i], s))
				saw[i]=1;
		}
	}
	ct=0;
	for(i=1; i<nw; i++)
		if (saw[i])
			ct++;
	return(ct);
}

lcase(s)
char *s;
{
	register int c;
	for(; c= *s; s++)
	{
		if (isupper(c))
			*s= tolower(c);
	}
}

update( ns, line, kov)
char *line;
{
	/* see if sentence array should be updated */
	int lval = 100;
	char *ob;
	struct sf *sp, *least = NULL;
	if (kov<=0) return (ns) ; /* no*/
	if (ns<NS)
	{
		sp=sents+ns;
		strcpy (sp->text = stext[ns], line);
		sp->olap = kov;
		return(ns+1);
	}
	for(sp=sents+ns-1; sp>=sents; sp--)
	{
		if (sp->olap < lval)
		{
			least = sp;
			lval = sp->olap;
		}
	}
	if (kov <= lval) return(ns);
	ob = least->text;
	while (++least < sents+NS)
	{
		(least-1)->text = least->text;
		(least-1)->olap = least->olap;
	}
	sp = sents+NS-1;
	strcpy (sp->text=ob, line);
	sp->olap = kov;
	return(NS);
}
