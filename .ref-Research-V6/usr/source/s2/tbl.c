#
# define ever (;;)
# define MAXCHS 2000
main(argc,argv)
	char *argv[];
{
char line[200];
extern int cin;
extern char *cspace;
	cspace = getvec(MAXCHS + 100);
for ever
{
	if (argc>1)
	{
	argc--;
	argv++;
	if (match(*argv, "-ms"))
		*argv = "/usr/lib/tmac.s";
	cin = copen(*argv, 'r');
	if (cin < 0)
		{
		printf(2,"where is input file %s\n",*argv);
		cexit();
		}
	}
while (gets(line))
	if (match(line, ".TS"))
		tableput();
	else
		puts(line);
if (argc <= 1) break;
}
cexit();
}
# define MAXCOL 35
# define MAXLIN 220
# define ONELINE 250
char *tabentry[MAXLIN][MAXCOL];
char extra[MAXCHS];
char *cspace, *cstore;
char *exstore extra;
int sep[MAXCOL], dwide[MAXCOL];
char *instead[MAXLIN];
int newtab[MAXLIN];
char *style[MAXCOL];
tableput()
{
/* read input, write output, make tables on the way */
char st[ONELINE], *format;
int ilin, nlin, icol, ncol, k, ch, ws, cs, ns;
int maxchnge, ct;
printf(".TS\n");
/* get command line */
cstore = cspace;
exstore = extra;
ncol = 0;
for (ilin=0; ilin<MAXLIN; ilin++)
	newtab[ilin]=0;
gets (st);
for (k=0; k<ONELINE && st[k] != '\0'; k++)
	if (!space(st[k]))
		{
		style[ncol] = st+k;
		dwide[ncol] =0;
		for(; letter(st[k]); k++)
			{
			if ((st[k]=='n' || st[k] == 'N') &&
				!dwide[ncol])
				{
				dwide[ncol]=1;
				sep[ncol++] = 0;
				style[ncol] = style[ncol-1];
				dwide [ncol] = 0;
				}
			}
		if (digit(st[k]))
			sep[ncol] = numb(st+k);
		else
			sep[ncol] = 3;
		ncol++;
		while (digit(st[k]))
			st[k++] = '\0';
		if (st[k] == '\0')
			break;
		st[k] = '\0';
		}
/* now get input lines */
for (nlin=0; gets(cstore) && !match(cstore, ".TE"); nlin++)
	{
	if (cstore[0] == '.' && letter(cstore[1]))
		{
		instead[nlin] = cstore;
		while (*cstore) cstore++;
		}
	else instead[nlin] = 0;
	for (icol = 0; icol <ncol; icol++)
		{
		tabentry[nlin][icol] = cstore;
		for(; (ch= *cstore) != '\0' && ch != '\t'; cstore++)
				;
		*cstore++ = '\0';
		if (dwide[icol] )
			if (broken(style[icol],nlin))
				{
				tabentry[nlin][icol+1]=maknew(tabentry[nlin][icol]);
				icol++;
				if (tabentry[nlin][icol] ==0)
					newtab[nlin]=newtab[nlin+1]=1;
				}
			else
				tabentry[nlin][++icol] = 0;
		while (span(style[icol+1],nlin) )
			tabentry[nlin][++icol] = "";
		if (ch == '\0') break;
		}
	while (++icol <ncol)
		tabentry[nlin][icol] = "";
	while (*cstore != '\0')
		 *cstore++;
	if (cstore-cspace > MAXCHS)
		cstore = cspace  = getvec(MAXCHS+100);
	}
/* find longest command string */
for (icol=maxchnge=0; icol<ncol; icol++)
	if (slen(style[icol]) >maxchnge)
		maxchnge = slen(style[icol]);
/* set tab stops */
printf(".nr 49 0\n");
for (icol = 0; icol<ncol; icol++)
	{
	ct = 1 + (icol>0 ? sep[icol-1] : 0);
	printf(".nr %2d 0\n", icol+50);
	for (ilin=0; ilin < nlin; ilin++)
		{
		if (icol>0 && dwide[icol-1]>0 && tabentry[ilin][icol]==0)
			{
			printf(".nr 48 \\n(%2d+\\w'%s'+%dn\n",
				icol+48, tabentry[ilin][icol-1], sep[icol-1]);
			printf(".if \\n(48-\\n(%2d .nr %2d \\n(48\n",
				icol+50,icol+50);
			}
		if ( !span(style[icol],ilin) && /* not part of span */
		   (dwide[icol] == 0 || tabentry[ilin][icol+1]!= 0)
				/* not a double item */
		     && (!span(style[icol+1],ilin) || icol==ncol))
			{
			printf(".nr 47 \\n(%2d+\\w'%s'+%dn\n",
			icol+49,tabentry[ilin][icol], ct);
			printf(".if \\n(47-\\n(%2d .nr %2d \\n(47\n",
				icol+50,icol+50);
			}
		}
	/* clean up spanned headings */
	for(ilin=0; ilin<maxchnge; ilin++)
		{
		if( !(span(style[icol],ilin)) &&
		  (icol ==1 || dwide[icol-1] == 0) &&
		  span(style[icol+(dwide[icol]?2:1)],ilin))
			printf(".nr %d \\n(%2d+\\w'%s'+%dn\n",
			  ilin+30, icol+49, tabentry[ilin][icol], ct);
		else if (span(style[icol],ilin) &&
		  (icol+1==ncol || !span(style[icol+1],ilin)))
			printf(".if \\n(%d-\\n(%d .nr %d \\n(%d\n",
				30+ilin, icol+50, icol+50, ilin+30);
		}
	}
/* run out table, put field characters in */
printf (".fc  @\n");
for (ilin=0; ilin<nlin; ilin++)
	{
	if (instead[ilin])
		{
		printf("%s\n",instead[ilin]);
		continue;
		}
	/* is a new set of tab stops needed */
	if (ilin < maxchnge || newtab[ilin])
		settab(ncol, ilin);
	for (icol=0; icol<ncol; icol++)
		{
		switch ( ylet(style[icol],ilin))
			{
			default:
			case 'L': case 'l':
				format = "%s@"; break;
			case 'R': case 'r':
				format= "@%s"; break;
			case 'n': case 'N':
				if (!dwide[icol] || tabentry[ilin][icol+1] != 0)
					{
					format=dwide[icol]? "@%s" : "%s@";
					break;
					}
			case 'c': case 'C':
				format = "@%s@"; break;
			case 's':  case 'S':
				format= "";
				break;
			}
		if (! (dwide [icol-1]>0 && tabentry[ilin][icol] == 0 ))
		printf(format, tabentry[ilin][icol]);
		if (!span(style[icol+1],ilin))
			for (k=sep[icol]; k>0; k--)
				printf(" ");
		}
	printf("\n");
	}
printf(".fc\n");
printf(".TE\n");
}
match (s1, s2)
	char *s1, *s2;
{
	while (*s1 == *s2)
		if (*s1++ == '\0')
			return(1);
		else
			*s2++;
	return(0);
}
slen(str)
	char *str;
{
	int k;
	for (k=0; *str++ != '\0'; k++);
	return(k);
}
space(ch)
	{
	switch (ch)
		{
		case ' ': case '\t':
			return(1);
	}
	return(0);
	}
letter (ch)
	{
	if (ch >= 'a' && ch <= 'z')
		return(1);
	if (ch >= 'A' && ch <= 'Z')
		return(1);
	return(0);
	}
numb(str)
	char *str;
	{
	/* convert to integer */
	int k;
	for (k=0; *str >= '0' && *str <= '9'; str++)
		k = k*10 + *str - '0';
	return(k);
	}
broken(str, nlin)
	{
	switch(ylet(str,nlin))
		{
		case 'n': case 'N':
			return(1);
		}
	return(0);
	}
ylet (str, k)
	char *str;
{
	k++;
	while (*str &&k--)
		str++;
	return(*--str);
}
span(str, k)
	{
	switch(ylet(str,k))
		{
		case 's': case 'S':
			return(1);
		}
	return(0);
	}
maknew(str)
	char *str;
{
	/* make two numerical fields */
	int point;
	char *p, *q;
	p = str;
	for (point=0; *str; str++)
		if (*str=='.')
			point=1;
	if (!point && *(str-1)== '$')
		return(0);
	for(; str>p; str--)
		if ( (point && *str == '.') ||
		    (!point && digit(*(str-1)) ) )
			break;
	if (!point && p==str) /* not numerical, don't split */
		return(0);
	p= str;
	q = exstore;
	while (*exstore++ = *str++);
	*p = 0;
	return(q);
	}
digit(x)
	{
	return(x>= '0' && x<= '9');
	}
settab(ncol, ilin)
{
	int k, icol;
	printf(".ta ");
	for (icol = 0; icol<ncol; icol++)
	   if ((dwide[icol] == 0 || tabentry[ilin][icol+1] != 0)
		&& !span(style[icol+1],ilin))
		printf("\\n(%du ",icol+50);
	printf("\n");
}
