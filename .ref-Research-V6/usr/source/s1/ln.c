#
/*
 * ln target [ new name ]
 */

struct ibuf {
	int	inum;
	int	iflags;
	char	inl;
	char	iuid;
	int	isize;
	int	iaddr[8];
	char	*ictime[2];
	char	*imtime[2];
	int	fill;
};

#define	DIR	040000
#define	FMT	060000

main(argc, argv)
char **argv;
{
	static struct ibuf statb;
	register char *np;

	if (argc<2) {
		write(1, "Usage: ln target [ newname ]\n", 29);
		exit(1);
	}
	if (argc==2) {
		np = argv[1];
		while(*np++);
		while (*--np!='/' && np>argv[1]);
		np++;
		argv[2] = np;
	}
	stat(argv[1], &statb);
	if ((statb.iflags&FMT) == DIR) {
		write(1, "No directory link\n", 18);
		exit(1);
	}
	if (link(argv[1], argv[2])<0) {
		write(1, "Can't link\n", 11);
		exit(1);
	}
	exit(0);
}
