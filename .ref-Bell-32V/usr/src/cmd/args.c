main(argc, argv)
int argc;
char *argv[];
{
	register int i;
	register char *cp;

	argc--;
	for(i=1; i<=argc; i++) {
		printf("%d:	",i);
		for(cp=argv[i]; *cp ; ++cp) {
			if (*cp&0200) putchar('@');
			*cp &= 0177;
			if (*cp=='@') putchar('@');
			putchar(*cp);
		}
		printf("\n");
	}
	exit(0);
}

putchar(c)
{
	write(1,&c,1);
}
