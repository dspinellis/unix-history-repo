static char *sccsid = "@(#)call.c	4.1 (Berkeley) %G%";
char *dn;

main(argc, argv)
char *argv[];
{
	register f, n, c;


	if(argc < 2)
		goto arg;
	dn = "/dev/dn0";
	if(*argv[1] == '-') {
		dn = argv[1]+1;
		argc--;
		argv++;
	}
	if(argc < 2)
		goto arg;
	c = 0;
loop:
	f = open(dn, 1);
	if(f < 0)
		goto slp;
	for(n=0; argv[1][n]; n++)
		;
	alarm(120);
	if(write(f, argv[1], n) == n)
		exit(0);

slp:
	if(f >= 0)
		close(f);
	c++;
	if(c > 100)
		exit(1);
	sleep(10);
	goto loop;

arg:
	printf("arg c\n");
	exit(1);
}
