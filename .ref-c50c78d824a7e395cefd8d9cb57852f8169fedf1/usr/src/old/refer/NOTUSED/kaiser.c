#ifndef lint
static char *sccsid = "@(#)kaiser.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <ctype.h>

char *months[] {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 0};

main()
{
	int state 1000, i, book, volume, corp, report;
	int na;
	char *v[20], **vv, **rr;
	char ubuff[1000], *up;
	char line[100];
	char *p, *s, *r, *q;
	while (gets(line))
	{
		if (line[1]>'9' || line[1]<'0') continue;
		switch(line[0])
		{
		case 'T':
			if (state > 'T')
			{
				book=0;
				report=0;
				printf("\n%%T ");
			}
			printf("%s\n", line+18);
			state='T';
			na = getargs(line+18, v);
			for(i=0;i<na;i++)
				if (strcmp(v[i], "(Book)")==0)
					book=1;
			continue;
		case 'A':
			state = 'A';
			na=getargs(line+18, vv=v);
			if (na<=0) continue;
			while (na>0)
			{
				printf("%%A ");
				corp=0;
				for(p=vv[1]; *p; p++)
					if (islower(*p))
						corp=1;
				if (corp==0)
				{
					for(p=vv[1]; *p; p++)
						printf("%c. ", *p);
					if (na>2 &&strcmp(vv[2], "+"))
					{
						printf("%s", vv[0]);
						if (strcmp(vv[2], "Jr.")==0)
							printf(",");
						printf(" %s\n",vv[2]);
						vv++;
						na--;
					}
					else
						printf("%s\n", vv[0]);
				}
				else
					printf("%s %s\n",vv[0],vv[1]);
				vv+=2;
				na-=2;
				if (strcmp(vv[0], "+")==0)
				{
					vv++;
					na--;
				}
			}
			continue;
		case 'U':
			if (state!='U')
				ubuff[0]=0;
			else
				strcat(ubuff, " ");
			state = 'U';
			strcat(ubuff, line+18);
			if (line[2]=='.')
			{ /* end of item */
				p=ubuff; /*start*/
				volume=0;
				for(s=ubuff; *s; s++)
					if (s[-1]==' ' && prefix("Vol", s))
					{
						for(q=s-1; q>ubuff; q--)
						{
							if (*q==' ' || *q==',') *q=0;
							else break;
						}
						volume=1;
						break;
					}
				if (*s==0)
					for(s=ubuff; *s && (*s!=',' || sprefix("Inc", s+1)); s++)
						;
				else
					s++;
				if (*s==',')*s++=0;
				if (book)
					printf("%%I %s\n",ubuff);
				else if (volume)
					printf("%%J %s\n", ubuff);
				else if (substr(ubuff, "Report")!=0)
				{
					report=1;
					printf("%%R %s\n", ubuff);
				}
				else
					printf("%%J %s\n", ubuff);
				if (volume)
				{
					s += 3; /* Vol */
					if (*s=='.') s++;
					while (*s==' ')s++;
					printf("%%V ");
					while (*s && *s != ' ' && *s!=',' && *s!=';' && *s!= ':')
						putchar(*s++);
					putchar('\n');
					if (*s==':')
					{
						printf("%%N ");
						while (*s==' ')s++;
						while (isdigit(*s))
							putchar(*s++);
						putchar('\n');
					}
					*s++=0;
					while (*s==' ')*s++=0;
					if (s[0]=='N' && s[1]=='o' && (s[2]==' '||s[2]=='.'))
					{
						s+=2;
						while (*s==' '||*s=='.')s++;
						printf("%%N ");
						while (isdigit(*s)||*s=='-')
							putchar(*s++);
						putchar('\n');
					}
					if (*s==',') *s++=0;
				}
				for(rr=months; *rr; rr++)
				{
					q= substr(s, *rr);
					if (q)
					{
						for(r=q; *r; r++);
						r--;
						if (*r=='.')*r=0;
						printf("%%D %s\n",q);
						*(q-1)=0;
						break;
					}
				}
				if (*rr==0)
				{
					for(q=s; *q; q++)
					{
						if (q[0]=='1' && q[1]=='9' && (q[4]==0 || (q[4]=='.' && q[5]==0)))
						{
							if (q[4]=='.') q[4]=0;
							printf("%%D %s\n",q);
							rr=months;
							q[-1]=0;
							if (q==s) q[0]=0;
							break;
						}
					}
				}
				if (*rr==0) /* no date */
					printf("%%D 19xx\n");
				/* if book bite off next field for city, if report for issuer */
				if (book)
				{
					for(q=s; *q && *q != ','; q++)
						;
					if (*q==',')
					{
						r=q;
						r++;
						while (*r==' ')r++;
						if (isupper(r[0]) && isupper(r[1]))
						{
							r+=2;
							*r++=0;
							while (*r==' ')r++;
						}
						else
							*q=0;
						printf("%%C %s\n", s);
						s=r;
					}
				}
				for(q=s; *q; q++)
				{
					if (q[0]==' ' && q[1]=='p' && (q[2]=='p'||q[2]==0))
					{
						for(r=q; r>s; r--)
						{
							if (*r==' ' || *r==',')
								*r=0;
						}
						*q=0;
						q+=2;
						if (q[0]=='p')q++;
						while (*q==' '||*q=='.')q++;
						r=q;
						while (isdigit(*q)||*q=='.'||*q=='-'||isalpha(*q))q++;
						*q++=0;
						while (*q==' ')q++;
						printf("%%P %s\n",r);
						break;
					}
				}
				s=ispp(s);
				while (*s==' ')s++;
				while (*q==' ')q++;
				if (*s||*q)
					printf("%%O %s %s\n", *s?s:"", *q?q:"");
			}
			continue;
		}
	}
}

getargs(s, arps)
char *s, *arps[];
{
	int i;
	i = 0;
	while (1)
	{
		arps[i++]=s;
		while (*s != 0 && *s!=' '&& *s != '\t')s++;
		if (*s==0) break;
		*s++ =0;
		while (*s==' ' || *s=='\t')s++;
		if (*s==0)break;
	}
	return(i);
}

prefix(small, big)
char *small, *big;
{
	int c;
	while ((c= *small++) == *big++)
		if (c==0) return(1);
	return(c==0);
}

substr(big, small)
char *small, *big;
{
	while (*big)
		if (prefix(small, big))
			return(big);
		else
			big++;
	return(0);
}

sprefix(small, big)
char *small, *big;
{
	while (*big==' ') big++;
	return(prefix(small,big));
}

ispp(s)
char *s;
{
	static char buff[50];
	char *p, *t;
	p=s;
	while (*p==' ') p++;
	if (!isdigit(*p)) return(s);
	t=p;
	while (isdigit(*p))p++;
	if (p[0]!='p' || p[1]!='p') return(s);
	*p=0;
	sprintf(buff, "%spp.", t);
	return(buff);
}
