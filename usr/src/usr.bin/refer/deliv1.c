# include "stdio.h"
main(argc,argv)
	char *argv[];
{
/* goes from file:begin,l to actual characters */
char line[750], *p, name[100];
FILE *fa NULL;
long lp;
int len;

if (argc>1 && argv[1] && argv[1][0])
	chdir (argv[1]);
name[0]="";
while (gets(line))
	{
	if (line[0]=='$' && line[1]=='$') 
		{
		chdir(line+2);
		continue;
		}
	for(p=line; *p!= ':'; p++)
		;
	*p++ = 0;
	sscanf(p, "%ld,%d", &lp, &len);
	if (p==line)
		fa = stdin;
	else
	if (strcmp (name, line) != 0)
		{
		if (fa != NULL)
			fclose(fa);
		fa = fopen(line, "r");
		if (fa == NULL)
			err("Can't open %s", line);
		strcpy(name, line);
		}
	if (fa != NULL)
		{
		fseek (fa, lp, 0);
		fread (line, 1, len, fa);
		line[len] = 0;
		fputs(line, stdout);
		}
	}
}
