/*
 *	opr -- off line print dispatcher
 *		chooses spooling routine appropriate 
 *		to destination
 *
 *	last entry in table isdefault
 */

char *code[] = {
	"-sp",	"/usr/lib/npr",	/* spider network printer */
	"-lp",	"/usr/bin/lpr",	/* line printer */
	"-mh",	"/usr/lib/dpr",	/* GCOS via 201 dataphone */
	0
};

main(argc, argv)
char **argv;
{
	int i, j;

	argv[argc] = 0;
	for(i=0; code[i+2]; i+=2)
	if(argc > 1)
		for(j=0; code[i][j]==argv[1][j]; j++)
			if(code[i][j] == 0)
				goto OK;
OK:
	execv(code[i+1]+4, &argv[0]);
	execv(code[i+1], &argv[0]);
	write(2, "can't start daemon\n", 19);
}
