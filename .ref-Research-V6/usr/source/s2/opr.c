/*
 *	opr -- off line print dispatcher
 *		chooses spooling routine appropriate 
 *		to destination
 *
 *	last entry in table isdefault
 */

char *code[] {
	"-lp",	"/lib/lpr",	/* line printer */
	0
};

main(argc, argv)
char **argv;
{
	int i, j;

	argv[argc] = 0;
	for(i=0; code[i]; i=+2)
	if(argc > 1)
		for(j=0; code[i][j]==argv[1][j]; j++)
			if(code[i][j] == 0)
					execv(code[i+1], &argv[1]);
	execv(code[i-1], argv);
	write(2, "can't start daemon\n", 19);
}
