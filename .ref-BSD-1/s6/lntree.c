#
#define isdir(a) ((a.flags & 060000) == 040000)
#define isquot(a) ((a.flags & 060000) == 020000 && a.addr[0] == -1)
/*
 * lntree [ - ] [ -q ] source dest
 *
 * Author : Bill Joy (UCB) October 16, 1976
 */
char progname[] "lntree";
char usagestr[] "usage: %s [ - ] [ -q ] source dest\n";

struct dbuff {
	int	ino;
	char	name[14];
};
struct stbuff {
	int	dev;
	int	inumber;
	int	flags;
	char	nlinks;
	char	uid;
	char	gid;
	char	size0;
	int	size1;
	int	addr[8];
	int	actime[2];
	int	modtime[2];
};

char *concat();
int status;
int specfil;
int newmode;
int suser;
int terse;

int noquot;

char **xargv;

main(argc,argv)
char *argv[];
{
	register char *argp;
	int *num;

	suser = getuid() == 0;
	argv++;
	argc--;
	while (argc && *(argp = *argv) == '-')
	{
		if (!*++argp)
			terse++;
		else
		{
			do
			{
				switch(*argp++)
				{
				case 'q':
					noquot++;
					break;
				default:
					goto usage;
				}
			} while(*argp);
		}
		argc--;
		argv++;
	}
	if (argc != 2)
usage:
	{
		printf(usagestr, progname);
		exit(1);
	}
	else
	{
		xargv = argv;
		ok(argv[0], argv[1]);
		lntree(argv[0], argv[1]);
	}
	exit(0);
}

owner(a)
struct stbuff *a;
{
	return(((a->gid & 0377) << 8) | (a->uid & 0377));
}

lntree(spth, dpth)
char *spth, *dpth;
{
	char nspth[120], ndpth[120];
	int curq, maxq;
	char qstr[8];
	struct dbuff sdbuff;
	struct stbuff source, dest;
	int sunit, i;

	if (stat(spth, &source) < 0)
		panic("\"%s\": cannot stat", spth);
	else if (stat(dpth, &dest) < 0)
		panic("\"%s\": cannot stat", dpth);
	else if (!isdir(source))
		panic("\"%s\": not a directory", spth);
	else if (!isdir(dest))
		panic("\"%s\": not a directory", dpth);
	else if ((sunit = open(spth, 0)) < 0)
		panic("\"%s\": cannot open directory", spth);
	else
	{
		while ((i = read(sunit, &sdbuff, 16) == 16))
		{
			concat(nspth, spth, sdbuff.name);
			concat(ndpth, dpth, sdbuff.name);
			if (sdbuff.ino == 0)
				continue;
			else if (stat(nspth, &source) < 0)
				panic("\"%s\": cannot stat", nspth);
			else if (source.flags & 060000)
				if (isdir(source))
				{
					if (equal(sdbuff.name, "."))
						continue;
					else if (equal(sdbuff.name, ".."))
						continue;
					else if (source.inumber == 1)
						panic("source tree across devices detected at \"%s\" on pass 2", nspth);
					sys("mkdir", ndpth, 0);
					newmode = source.flags & 07777;
					if (chmod(ndpth, newmode))
						panic("can't chmod of \"%s\" to %o", ndpth, 0);
					if (suser)
						chown(ndpth, owner(&source));
					lntree(nspth, ndpth);
				}
				else if (isquot(source))
					if (noquot)
						continue;
					else if (suser)
					{
						curq = source.addr[1];
						maxq = source.addr[2];
						itoa(maxq, qstr);
						sys("quot", dpth, "0", qstr, 0);
						chown(ndpth, owner(&source));
					}
					else
						printf("\"%s.q\" no can do\n", spth);
				else
					goto doit;
			else
doit:
				if (link(nspth, ndpth) < 0)
					panic("can't link \"%s\" to \"%s\"", ndpth, nspth);
		}
		if (i == -1)
			panic("error reading \"%s\":", spth);
		else if (i && i != 16)
			panic("bad directory structure: \"%s\"", spth);
	}
	close(sunit);
}

ok(spth, dpth)
char *spth, *dpth;
{
	struct stbuff source, dest;
	int head;

	if (stat(spth, &source) < 0)
		panic("\"%s\": cannot stat", spth);
	else if (stat(dpth, &dest) < 0)
		panic("\"%s\": cannot stat", dpth);
	else if (!isdir(source))
		panic("\"%s\": not a directory", spth);
	else if (!isdir(dest))
		panic("\"%s\": not a directory", dpth);
	else if (source.dev != dest.dev)
		panic("\%s\" and \"%s\" are on different devices", spth, dpth);
	else
	{
		check(spth, dest.inumber);
		if (specfil && !terse)
			cani();
	}
}

cani()
{
	register char c, ch;

	if (!terse && ttyn(0) != 'x')
	{
		printf("\nok ? ");
		ch = c = getchar();
		while (ch != '\n' && ch)
			ch = getchar();
		if (c != 'y')
			exit(1);
	}
}

check(spth, dinode)
char *spth;
{
	char nspth[120];
	struct dbuff sdbuff;
	struct stbuff source;
	int sunit, i;

	if (stat(spth, &source) < 0)
		panic("\"%s\": cannot stat", spth);
	else if (source.inumber == dinode)
		panic("\"%s\" is a subdirectory of \"%s\" at \"%s\"", xargv[0], xargv[1], spth);
	else if (!isdir(source))
		panic("\"%s\": not a directory", spth);
	else if ((sunit = open(spth, 0)) < 0)
		panic("\"%s\": cannot open directory", spth);
	else
	{
		while ((i = read(sunit, &sdbuff, 16) == 16))
		{
			if (sdbuff.ino == 0)
				continue;
			else if (equal(sdbuff.name, "."))
				continue;
			else if (equal(sdbuff.name, ".."))
				continue;
			concat(nspth, spth, sdbuff.name);
			if (stat(nspth, &source) < 0)
				panic("\"%s\": cannot stat", nspth);
			else if (source.flags & 060000)
				if (isdir(source))
				{
					if (source.inumber == 1)
						panic("source tree extends across devices at \"%s\"", nspth);
					else
						check(nspth, dinode);
				}
				else if (isquot(source))
					continue;
				else
				{
					if (!terse)
						printf("special file: \"%s\"\n", nspth);
					specfil++;
				}
		}
		if (i == -1)
			panic("error reading \"%s\":", spth);
		else if (i && i != sizeof sdbuff)
			panic("bad directory structure: \"%s\"");
	}
	close(sunit);
}

char *concat(a, b, c)
char *a, *b, *c;
{
	register char *ra, *rb;
	register cnt;

	cnt = 14;
	rb = c;
	while (*rb++)
		if (!--cnt)
			break;
	cnt = 14 - cnt;
	rb = b;
	while (*rb++)
		cnt++;
	if (cnt >= 100)
		panic("path name too long: \"%s/%s\"", b, c);
	ra = a;
	rb = b;
	while (*ra++ = *rb++)
		continue;
	*(ra-1) = '/';
	rb = c;
	cnt = 14;
	while (*ra++ = *rb++)
		if (!--cnt)
		{
			*ra++ = 0;
			break;
		}
	return(a);
}

panic(a1, a2, a3, a4)
{
	if (a1)
	{
		printf("%s: ", progname);
		printf(a1, a2, a3, a4);
		putchar('\n');
	}
	exit(1);
}

equal(a, b)
char *a, *b;
{
	register char *ra, *rb;

	ra = a;
	rb = b;
	while (*ra == *rb++)
		if (*ra)
			ra++;
		else
			return(!*--rb);
	return(0);
}

char *itoastr;

itoa(aint, str)
char *str;
{
	itoastr = str;
	if (aint)
		itoa2(aint);
	else
		*itoastr++ = '0';
	*itoastr++ = 0;
}

itoa2(aint)
{
	if (aint)
	{
		itoa2(ldiv(0, aint, 10));
		*itoastr++ = '0' + lrem(0, aint, 10);
	}
}

sys(name)
char *name;
{
	int i, k;
	register char *argp, *rp;
	char routine[25];

	if ((i = fork()) < 0)
		panic("try again.");
	else if (i)
	{
		while ((k = wait(&status)) != i && k != -1)
			continue;
		if (k == -1)
		{
			printf("no children to wait for !!!\n");
			status = 0;
		}
		else if (status)
			panic("\"%s\" failed", name);
	}
	else
	{
		argp = "/usr/bin/";
		rp = routine;
		while(*rp++ = *argp++)
			continue;
		rp--;
		argp = name;
		while (*rp++ = *argp++)
			continue;
		execv(routine+4, &name);
		execv(routine, &name);
		panic("can't find \"%s\"", name);
	}
}

putchar(c)
{
	write(2, &c, 1);
}
