/* nohup */
char	*args[100];
char	string[10000];

main(argc, argv)
int argc;
char *argv[];
{
	int i;
	char **argp, *strp, *p;

	if(argc < 2) {
		printf("arg count\n");
		exit();
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
	signal(1, 1);
	signal(2, 1);
	signal(3, 1);
	execv(string+9, args);
	execv(string+4, args);
	execv(string, args);
	printf("%s not found\n", string+9);
}
