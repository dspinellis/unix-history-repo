main(argc,argv)
	char *argv[];
{
/* program to convert tables to new format */
char *col[20], *len[20];
char s[200], *p;
char stt[30];
int i,j,k,m, first;
extern int cin, cout;
for(i=0;i<20;i++)
	col[i]=len[i]=0;
first=1;
while (argc>1 || first)
	{
	first=0;
	if (argc>1)
		{
		cin = copen(argv[1], 'r');
		cout = copen(tmpnam(stt), 'w');
		}
	if (cin<0)
		{
		printf("can't open file %s\n",argv[1]);
		cexit(1);
		}
	while (gets(s))
		{
		puts(s);
		if (!prefix(".TS", s))
			continue;
		gets(s);
		k=0;
		for(p=s; *p; p++)
			{
			if (letter(*p))
				{
				col[k++]=p;
				while (letter(*p))
					p++;
				if (digit(*p))
					len[k-1]=p;
				while (digit(*p))
					p++;
				}
			if (*p==0)break;
			}
		for(i=m=0; i<k; i++)
			m=max(length(col[i]),m);
		for(i=0; i<m; i++)
			{
			for(j=0; j<k; j++)
				{
				printf("%c ", *(col[j])++);
				if (!letter(*(col[j]))) col[j]--;
				if (i==0 && len[i])
					{
					p=len[j];
					while (digit(*p))
						putchar(*p++);
					}
				}
			if (i+1==m)
				printf(".");
			printf("\n");
			}
		for(i=0;i<20; i++)
			col[i]=len[i]=0;
		}
	if (argc>1)
		{
		printf(-1, s, "mv %s %s", stt, argv[1]);
		system(s);
		argc--;
		argv++;
		}
	}
}
length(s)
	char *s;
{
int k;
for(k=0; s[k]; k++)
	if (!letter(s[k])) break;
return(k);
}
digit(c)
{
return(c>='0' && c<= '9');
}
letter(c)
{
return((c>='a' && c<= 'z') || (c>= 'A' && c<='Z'));
}
prefix(small, big)
	char *small, *big;
{
int c;
while ((c= *small++) == *big++)
	if (c==0) return(1);
return(c==0);
}
max(a,b)
{
return(a>b? a : b);
}
