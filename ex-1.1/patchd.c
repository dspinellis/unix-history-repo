/*
 * patchd symbol value [ file ]
 *
 * Like a db ! but works separate i/d.
 */
long	geto(), getsym(), itol();
char	*file;
struct	header {
	int	magic;
	int	txt_size;
	int	data_size;
	int	bss_size;
	int	sym_size;
	int	entry_loc;
	int	unused;
	int	no_reloc;
} header;

struct	nl {
	char	name[8];
	int	typ;
	int	*val;
} *nlp;

int	*initbrk;

main(argc, argv)
	int argc;
	char *argv[];
{
	long iaddr, addr;
	int value, word;
	int i;

	if (argc != 3 && argc != 4) {
		write(2, "Usage: patchd symbol value [ file ]\n", 36);
		exit(1);
	}
	close(0);
	file = argc == 4 ? argv[3] : "a.out";
	if (open(file, 2) < 0) {
		perror(file);
		exit(1);
	}
	value = geto(argv[2]);
	seek(0, 0, 0);
	if (read(0, &header, sizeof header) != sizeof header)
		goto oops;
	if (header.magic < 0407 || header.magic > 0411) {
		perror(file, "Not object file");
		exit(1);
	}
	addr = iaddr = getsym(argv[1]) + 16;
	switch (header.magic) {

	case 0407:	/* Non-shared */
		/* Just skip past header */
		break;

	case 0410:	/* Shared */
		/* Round to 4kW boundary */
		addr -= ((header.txt_size + 8191) &~ 8191) - header.txt_size;
		break;

	case 0411:	/* Separate i/d */
		addr += header.txt_size;
		break;

	}
	if (lseek(0, addr, 0)) {
seekerr:
		Perror(file, "Seek failed");
		exit(1);
	}
	if (read(0, &word, 2) != 2)
		goto oops;
	if (lseek(0, addr, 0))
		goto seekerr;
	if (write(0, &value, 2) != 2)
		goto oops;
	printf("Was %o now %o\n", word, value);
	exit(1);
oops:
	perror(file);
	exit(1);
}

long
geto(cp)
	char *cp;
{
	long i;

	for (i = 0; *cp >= '0' && *cp <= '7'; i = (i << 3) | *cp++ - '0')
		continue;
	if (*cp) {
		write(2, "Bad number.\n", 12);
		exit(1);
	}
	return (i);
}

Perror(cp, dp)
	char *cp, *dp;
{
	extern int errno;
	extern char *sys_errlist[];

	errno = 0;
	sys_errlist[0] = dp;
	perror(cp);
}

long
getsym(cp)
	char *cp;
{
	static char symbol[8];
	int i, j, n;

	seek(0, sizeof header, 0);
	lseek(0, 0, header.txt_size, 1);
	lseek(0, 0, header.data_size, 1);
	for (i = 0, j = 0; i < 8; i++) {
		symbol[i] = cp[j];
		if (cp[j])
			j++;
	}
	initbrk = sbrk(0);
	n = ldiv(0, header.sym_size, 12);
	if (n == 0) {
		Perror(file, "No name list");
		exit (1);
	}
	nlp = sbrk(n * 12);
	if (read(0, nlp, n * 12) != n * 12) {
		Perror(file, "Error reading");
		exit (1);
	}
	for (i = 0; i < n; i++) {
		for (j = 0; j < 8; j++)
			if (nlp->name[j] != symbol[j])
				break;
		if (j == 8)
			break;
		nlp++;
	}
	if (i == n) {
		Perror(cp, "Symbol not found");
		exit(1);
	}
	j = nlp->typ & 037;
	if (j > 4)
		j = 1;
	if (j == 0 && nlp->val)
		j = 5;
	if (!(nlp->typ&040) || j != 3) {
		Perror(cp, "Inappropriate symbol");
		exit(1);
	}
	return (itol(0, nlp->val));
}
