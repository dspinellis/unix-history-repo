/* nice */
int	nicarg	4;
char	*args[100];
char	string[10000];

main(argc, argv)
int argc;
char *argv[];
{
	int i;
	register char **argp, *strp, *p;

	if(argc > 1 && argv[1][0] == '-') {
		nicarg = atoi(&argv[1][1]);
		argc--;
		argv++;
	}
	if(argc < 2) {
		printf("usage: nice [ -n ] command\n");
		exit(1);
	}
	argc--;
	argv++;
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
	nice(nicarg);
	execv(string+9, args);
	execv(string+4, args);
	execv(string, args);
	printf("%s not found\n", string+9);
	exit(1);
}
