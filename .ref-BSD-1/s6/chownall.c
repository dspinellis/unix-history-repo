#
#define isdir(a) ((a.flags & 060000) == 040000)
/*
 * chownall [ - ] [ -nname ] [ -a ] [ -g# ] [ -u# ] directory ...
 *
 * Author: Bill Joy (UCB) Oct 13, 1976
 */
char progname[] "chownall";
char usagestr[] "usage: %s [ - ] [ -nname ] [ -a ] [ -g# ] [ -u# ] directory ...\n";

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
int terse;
int lev;
int all;
int chggrp;
int newuser;
int newgid;
char *newname;
int chgusr;
int newuid;
int srcuid;
int srcgid;

char **xargv;

main(argc,argv)
char *argv[];
{
	register char *argp;
	int *num;

/*
	if (getuid())
		panic("NOT super-user");
*/
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
				case 'a':
					all++;
					break;
				case 'g':
					if (chggrp)
						goto usage;
					chggrp++;
					num = &newgid;
					goto getnum;
				case 'u':
					if (chgusr)
						goto usage;
					chgusr++;
					num = &newuid;
getnum:
					*num = 0;
					if (!(*argp >= '0' && *argp <= '9'))
						panic("bad user/group number");
					do
						*num = *num * 10 + *argp++ - '0';
					while (*argp >= '0' && *argp <= '9');
					if (*num > 255)
						panic("group/user number too large (%d > 255)", *num);
					break;
				case 'n':
					if (chgusr || chggrp)
						panic("Can't specify -n with -g or -a");
					chgusr++;
					chggrp++;
					newuser = findname(argp);
					newuid = newuser & 0377;
					newgid = (newuser >> 8) & 0377;
					newname = argp;
					all++;
					while (*argp) ++argp;
					break;
				default:
					goto usage;
				}
			} while(*argp);
		}
		argc--;
		argv++;
	}
	if (argc == 0)
usage:
	{
		printf(usagestr, progname);
		exit(1);
	}
	else
	{
		if (!chgusr && !chggrp)
			panic("no changes specified");
		else if (all && (!chgusr || !chggrp))
			panic("all option requires both user and group to be specified");
		while(argc)
		{
			xargv = argv;
			if (all && !terse)
			{
				printf("last chance before scribbling uid %d and gid %d",
					newuid, newgid);
				if (newname) printf(" (%s)",newname);
				printf(" all over \"%s\"\n", argv[0]);
				cani();
			}
			chtree(argv[0]);
			argv++;
			argc--;
		}
	}
	exit(0);
}

int	old;
int	new;

owner(xgid, xuid)
{
	old = ((xgid & 0377) << 8) | (xuid & 0377);
	if (all)
		goto spankingnew;
	else if (chgusr)
		if (xuid != srcuid || xgid != srcgid)
			new = old;
		else
spankingnew:
			new = (((chggrp ? newgid : srcgid) & 0377) << 8) | (newuid & 0377);
	else if (chggrp && xgid == srcgid)
		new = ((newgid & 0377) << 8) | (xuid & 0377);
	else
		new = old;
	return(new);
}

chtree(spth)
char *spth;
{
	char nspth[120];
	struct dbuff sdbuff;
	struct stbuff source;
	int sunit, i;

	if (stat(spth, &source) < 0)
		panic("\"%s\": cannot stat", spth);
	else if (!isdir(source))
		panic("\"%s\": not a directory", spth);
	else if ((sunit = open(spth, 0)) < 0)
		panic("\"%s\": cannot open directory", spth);
	else
	{
		if (lev == 0)
		{
			srcuid = source.uid;
			srcgid = source.gid;
			if (!srcuid && !srcgid && !chgusr)
			{
				printf("\"%s\" is owned by the root\n.", spth);
				printf("I don't think I should change all group 0's to group %d's there.\n", newgid);
				printf("Sorry.\n");
				exit(1);
			}
			if (chgusr && !chggrp && source.uid == 0)
				panic("owner of \"%s\" is uid 0 in his group !?!", spth);
			else if (!chgusr && chggrp && source.uid != 0)
				panic("owner of \"%s\" is not uid 0 of his group !?!", spth);
		}
		lev++;
		while ((i = read(sunit, &sdbuff, 16) == 16))
		{
			concat(nspth, spth, sdbuff.name);
			if (sdbuff.ino == 0)
				continue;
			else if (stat(nspth, &source) < 0)
				panic("\"%s\": cannot stat", nspth);
			else if (isdir(source))
				if (equal(sdbuff.name, ".."))
					continue;		/* Whew ! */
				else if (!equal(sdbuff.name, "."))
					chtree(nspth);
			if (owner(source.gid, source.uid) != old)
				if (chown(nspth, new) == -1)
					panic("can't chown \"%s\" to %o !?!", nspth, new);
		}
		if (i == -1)
			panic("error reading \"%s\":", spth);
		else if (i && i != 16)
			panic("panic: bad directory structure: \"%s\"", spth);
	}
	close(sunit);
	if (!--lev)
		if (chgusr && !chggrp || chggrp && !chgusr)
			if (chown(spth, owner(srcgid, srcuid)) == -1)
				panic("can't chown \"%s\" to %o !?!", spth, new);
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
	printf("%s: ", progname);
	printf(a1, a2, a3, a4);
	putchar('\n');
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

cani()
{
	register char c, ch;

	printf("ok ? ");
	ch = c = getchar();
	while (ch != '\n' && ch)
		ch = getchar();
	if (c != 'y')
		exit(1);
}

putchar(c)
{
	write(2, &c, 1);
}

findname(user)
	char *user;
{
	int pbuf[259];
	char *p,c,*pfile;
	int u,v;
	pfile = "/etc/passwd";
	pbuf[0] = open(pfile, 0);
	if(pbuf[0] < 0) {
		write(2, "Cannot open /etc/passwd\n", 25);
		exit(9);
	}
	goto l1;

/*
 * skip to beginning of next line
 */

skip:
	while(c != '\n') {
		if(c < 0)
			goto ill;
		c = getc(pbuf);
	}

/*
 * compare user names
 */

l1:
	c = getc(pbuf);
	if(c < 0) {
		write(2, "User name not found in password file\n", 37);
		exit(9);
	}
	p = user;
	while(c != ':') {
		if(*p++ != c)
			goto skip;
		c = getc(pbuf);
	}
	if(*p)
		goto skip;
/*
 * skip old password
 */
	do {
		c = getc(pbuf);
		if(c < 0)
			goto ill;
	} while(c != ':');


/*
 * validate uid and gid
 */

	u = 0;
	do {
		c = getc(pbuf);
		if(c >= '0' && c <= '9')
			u = u*10 + c-'0';
		if(c < 0)
			goto ill;
	} while(c != ':');
	v = 0;
	do {
		c = getc(pbuf);
		if(c >= '0' && c <= '9')
			v = v*10 + c-'0';
		if(c < 0)
			goto ill;
	} while(c != ':');
	return ((v<<8) | u);
ill:	write(2,"Password file illformed\n",24);
	exit(9);
}
