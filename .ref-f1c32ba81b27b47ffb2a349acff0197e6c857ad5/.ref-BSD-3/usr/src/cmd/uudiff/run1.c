# include "stdio.h"
# define LAST "\177\177"
main (argc,argv)
	char *argv[];
{
	FILE *rem, *loc, *cfil;
	char namel[100], namer[100];
	char cname[20], zname[20];
	int mr, ml;
	long suml, sumr;
ml=mr=1;
loc = fopen(argv[1], "r");
rem = fopen (argv[2], "r");
cfil = fopen (argv[3], "w");
ml = fetch (loc, namel, &suml, ml);
mr = fetch (rem, namer, &sumr, mr);
while ( ml || mr)
	{
	int x;
	x = strcmp(namel, namer);
	if (x>0) x=1;
	if (x<0) x= -1;
	switch(x)
		{
		case -1: /* name l lower */
			/* check that file is not our zz* tempfile */
			if (notmp(namel, argv[2]))
			printf("Missing on remote system: %s\n",namel);
			ml = fetch (loc, namel, &suml, ml);
			continue;
		case 0: /* match names */
			if (strcmp(namel, LAST)==0)
				exit(0);
			if (suml == sumr)
				printf("Presumed identical: %s\n",namel);
			else
				{
				printf("Differ: %s\n", namel);
				fprintf(cfil, "%s\n", namel);
				}
			ml=fetch(loc, namel, &suml, ml);
			mr=fetch(rem, namer, &sumr, mr);
			continue;
		case 1: /* name 2 lower */
			printf("Extraneous file on remote system: %s\n",namer);
			mr=fetch(rem, namer, &sumr, mr);
			continue;
		default:
			printf("illegal case %d\n", strcmp(namel,namer));
			exit(0);
		}
	}
}
fetch (f, s, lp, m)
	FILE *f;
	char *s;
	long *lp;
{
char b[200];
if (m==0 ||fgets(b, 200, f)==0)
	{
	strcpy (s, LAST);
	return (0);
	}
sscanf(b, "%s %lo", s, lp);
return (1);
}
notmp(s1, s2)
	char *s1, *s2;
{
char bf1[20], bf2[20];
strcpy (bf1, s1);
strcpy (bf2, s2);
bf1[2]= bf2[2] = '0';
return (strcmp(bf1, bf2));
}
