/*
 * nm - print name list
 *
 * Modified by Bill Joy UCB August 23, 1977
 * to take multiple file arguments.
 */
int	fout;

int	cflg;
int	nflg;
int	uflg;
int	rflg	1;
int	gflg;
int	pflg;

struct	nl {
	char	name[8];
	int	typ;
	int	*val;
} *nlp;

struct {
	int	magic;
	int	txt_size;
	int	data_size;
	int	bss_size;
	int	sym_size;
	int	entry_loc;
	int	unused;
	int	no_reloc;
} header;

int	compare();

int	*initbrk;

main(Argc, argv)
	int Argc;
	char *argv[];
{
	register char *name, *argp;
	register int argc;
	int i, j, n;

	initbrk = sbrk(0);
	fout = dup(1);
	Argc--, argv++;
	argc = Argc;
	while (argc > 0) {
		argp = argv[0];
		if (*argp++ != '-')
			break;
		argc--, argv++;
		while (*argp) switch (*argp++) {
			case 'n':
				nflg++;
				continue;
			case 'c':
				cflg++;
				continue;
			case 'g':
				gflg++;
				continue;
			case 'u':
				uflg++;
				continue;
			case 'r':
				rflg = -1;
				continue;
			case 'p':
				pflg++;
				continue;
			default:
				continue;
		}
	}
	do {
		if (argc > 0)
			argc--, name = *argv++;
		else
			name = "a.out";
		close(0);
		if (open(name, 0) < 0) {
			perror(name);
			continue;
		}
		if (read(0, &header, sizeof header) != sizeof header) {
			Perror(name, "Cannot read header");
			continue;
		}
		if (header.magic < 0407 || header.magic > 0411) {
			Perror(name, "Not object file");
			continue;
		}
		lseek(0, 0, header.txt_size, 1);
		lseek(0, 0, header.data_size, 1);		/* data */
		if (!header.no_reloc) {
			lseek(0, 0, header.txt_size, 1);
			lseek(0, 0, header.data_size, 1);
		}
		n = ldiv(0, header.sym_size, 12);
		if (n == 0) {
			Perror(name, "No name list");
			continue;
		}
		brk(initbrk);
		nlp = sbrk(n * 12);
		if (read(0, nlp, n * 12) != n * 12) {
			Perror(name, "Error reading");
			continue;
		}
		if (pflg == 0)
			qsort(nlp, n, 12, compare);
		if (Argc > 1)
			printf("%s:\n", name);
		for (i = 0; i < n; i++) {
			if (gflg && (nlp->typ & 040) == 0)
				goto out;
			if (cflg) {
				if (nlp->name[0] != '_')
					goto out;
				for (j = 0; j < 7; j++)
					nlp->name[j] = nlp->name[j+1];
				nlp->name[7] = '\0';
			}
			j = nlp->typ & 037;
			if (j > 4)
				j = 1;
			if (j == 0 && nlp->val)
				j = 5;
			if (uflg && j != 0)
				goto out;
			if (!uflg) {
				if (j == 0)
					printf("      ");
				else
					printo(nlp->val);
				printf("%c ", (nlp->typ&040? "UATDBC":"uatdbc")[j]);
			}
			printf("%.8s\n", nlp);
out:
			nlp++;
		}
		flush();
	} while (argc > 0);
}

compare(p1, p2)
	register struct nl *p1, *p2;
{
	int a, i;

	a = 0;
	if (nflg) {
		if (p1->val > p2->val) {
			a = 1;
			goto out;
		}
		if (p1->val < p2->val) {
			a = -1;
			goto out;
		}
	}
	for (i = 0; i < 8; i++)
		if (p1->name[i] != p2->name[i]) {
			if (p1->name[i] > p2->name[i])
				a = 1;
			else
				a = -1;
			goto out;
		}
out:
	return (a * rflg);
}

printo(v)
{
	int i;

	printf("%c", v<0?'1':'0');
	for (i = 0; i < 5; i++) {
		printf("%c", ((v>>12)&7)+'0');
		v =<< 3;
	}
}

Perror(name, mesg)
	char *name, *mesg;
{
	extern char *sys_errlist[];
	extern int errno;

	sys_errlist[0] = mesg;
	errno = 0;
	perror(name);
}

long
longof(l)
	long l;
{

	return (l);
}
