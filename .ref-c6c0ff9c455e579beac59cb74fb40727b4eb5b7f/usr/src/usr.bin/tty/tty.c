static char *sccsid = "@(#)tty.c	4.1 (Berkeley) %G%";
/*
 * Type tty name
 */

char	*ttyname();

main(argc, argv)
char **argv;
{
	register char *p;

	p = ttyname(0);
	if(argc==2 && !strcmp(argv[1], "-s"))
		;
	else
		printf("%s\n", (p? p: "not a tty"));
	exit(p? 0: 1);
}
