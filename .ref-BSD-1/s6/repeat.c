/*
 * repeat count command
 *
 * Author: Jeff Schriebman
 */
char	*args[100];
char	string[10000];

main(argc, argv)
int argc;
char *argv[];
{
	int i,knt,status;
	char **argp, *strp, *p;

	if(argc < 3) {
		printf("arg count\n");
		exit();
	}
	argc--;
	argv++;
	knt= a2i(*argv++);
	argc--;
	argp = args;
	strp = string;
	for (i=0; i<9; i++)
		*strp++ = "/usr/bin/"[i];
	for(i=0; i<argc; i++) {
		*argp++ = strp;
		p = *argv++;
		while(*strp++ = *p++);
	}
	*argp = 0;

	for(i=0; i<knt; i++)
	{
		if(fork())wait(&status);
		else
		{
			execv(string+9, args);
			execv(string+4, args);
			execv(string, args);
			printf("%s not found\n", string+9);
			exit();
		}
	}
}
a2i(strng)
char	strng[];
{
	int i,n,sign;
	char	ch;

	i=0;
	n=0; sign=1;
	while((ch=strng[i++])==' ');
	if(ch== '-'){sign= -1; ch=strng[i++];};
	while(ch>='0' & ch<='9')
	{	n=n*10+(ch-'0');
		ch=strng[i++];
	};
	return(sign*n);
}
