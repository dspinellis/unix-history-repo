#define	FNSIZE 64
/*
 * Expreserve - preserve a file in /usr/preserve
 * Bill Joy UCB November 13, 1977
 *
 * This routine is very naive - it doesn't remove anything from
 * /usr/preserve... this may mean that we will be unable to preserve
 * stuff there... the danger in doing anything with /usr/preserve
 * is that the clock may be screwed up and we may get confused.
 *
 * We are called in two ways - first from the editor with no argumentss
 * and the standard input open on the temp file. Second with an argument
 * to preserve the entire contents of /tmp (root only).
 */

struct stbuff {
	char	major, minor;
	int	inumber;
	int	flags;
	char	nlinks;
	char	uid, gid;
	char	size0;
	int	size1;
	int	addr[8];
	long	actime, modtime;
} stbuff;

int	mask	0377;

main(argc, argv)
	int argc;
	char *argv[];
{
	int tf;
	struct {
		int inum;
		char name[16];
	} dirent;
	char name[30];

	if (argc == 1) {
		if (copyout(0))
			exit(1);
		exit(0);
	}
	if (getuid() & mask) {
		write(2, "NOT super user\n", 15);
		exit(1);
	}
	if (chdir("/tmp") < 0) {
		perror("/tmp");
		exit(1);
	}
	tf = open(".", 0);
	if (tf < 0) {
		perror("/tmp");
		exit(1);
	}
	dirent.name[14] = 0;
	while(read(tf, &dirent, sizeof dirent - 2) == sizeof dirent - 2) {
		if (dirent.inum == 0)
			continue;
#define eq(a, b) strcmp(a, b) == 0
		if (eq(dirent.name, "."))
			continue;
		if (eq(dirent.name, ".."))
			continue;
		if (eq(dirent.name, ".q"))
			continue;
		if (stat(dirent.name, &stbuff))
			continue;
		if ((stbuff.flags & 060000) != 0)
			continue;
		if (dirent.name[0] != 'E' || dirent.name[1] != 'x')
			continue;
		copyout(dirent.name);
	}
	exit(0);
}

char	pattern[]	"/usr/preserve/Exaa`XXXXX";

copyout(name)
	char *name;
{
	int i;
	static char reenter;
	char buf[512];
	struct header {
		int	Atime[2];
		int	Auid;
		int	Alines;
		int	Afname[FNSIZE];
		int	Ablocks[100];
	} header;
	char *cp;

	if (reenter == 0) {
		mkdigits(pattern);
		reenter++;
	}
	if (name != 0) {
		close(0);
		if (open(name, 2) < 0)
			return (-1);
	}
	seek(0, 0, 0);
	if (read(0, &header, sizeof header) != sizeof header) {
format:
		if (name == 0)
			write(2, "Buffer format error\n", 20);
		return (-1);
	}
	if (header.Alines < 0)
		goto format;
	if (header.Ablocks[0] != 1 || header.Ablocks[1] != 2)
		goto format;
	if (name == 0 && header.Auid != (getuid() & mask))
		goto format;
	if (seek(0, 0, 0))
		goto format;
	if (header.Afname[0] == 0) {
		strcpy(header.Afname, "LOST");
		write(0, &header, sizeof header);
		header.Afname[0] = 0;
		seek(0, 0, 0);
	}
	mknext(pattern);
	close(1);
	if (creat(pattern, 0600) < 0) {
		if (name == 0)
			perror(pattern);
		return (1);
	}
	chown(pattern, header.Auid);
	for (;;) {
		i = read(0, buf, 512);
		if (i < 0) {
			if (name)
				perror("Buffer read error");
			unlink(cp);
			return (-1);
		}
		if (i == 0) {
			if (name) {
				unlink(name);
				notify(header.Auid, header.Afname);
			}
			return (0);
		}
		if (write(1, buf, i) != i) {
			if (name == 0)
				perror(pattern);
			unlink(cp);
			return (-1);
		}
	}
}

strcpy(to, from)
	char *to, *from;
{

	while (*to++ = *from++)
		continue;
}

strcmp(a, b)
	register char *a, *b;
{

	while (*a && *b && *a == *b)
		a++, b++;
	if (*a == 0 && *b == 0)
		return (0);
	return (*a - *b);
}

mkdigits(cp)
	char *cp;
{
	register int i, j;

	for (i = getpid(), j = 5, cp =+ strlen(cp); j > 0; i =/ 10, j--)
		*--cp = i % 10 | '0';
}

mknext(cp)
	char *cp;
{
	char *dcp;
	int spbuf[18];

	dcp = cp + strlen(cp) - 1;
	while (digit(*dcp))
		dcp--;
whoops:
	if (dcp[0] == 'z') {
		dcp[0] = 'a';
		if (dcp[-1] == 'z') {
			dcp[-1] = 'a';
			if (dcp[-2] == 'z')
				write(2, "Can't find a name\n", 17);
			dcp[-2]++;
		} else
			dcp[-1]++;
	} else
		dcp[0]++;
	if (stat(cp, spbuf) == 0)
		goto whoops;
}

digit(c)
	char c;
{

	return (c >= '0' && c <= '9');
}

notify(nuid, nfname)
	int nuid;
	char *nfname;
{
	register char *cp;
	char pwbuf[512];
	int pid, pvec[2];
	extern int fout;

	if (getpw(nuid, pwbuf) < 0)
		return;
	if (pipe(pvec) < 0)
		return;
	for (cp = pwbuf; *cp && *cp != ':'; cp++)
		continue;
	*cp = 0;
	pid = fork();
	if (pid < 0)
		return;
	if (pid == 0) {
		close(0);
		close(pvec[1]);
		dup(pvec[0]);
		close(pvec[0]);
		close(1);
		dup(2);
		execl("/bin/mail", "mail", pwbuf, 0);
		execl("/usr/bin/mail", "mail", pwbuf, 0);
		while (read(0, &pwbuf, 512) > 0)
			continue;
		exit(1);
	}
	close(pvec[0]);
	fout = pvec[1];
	if (nfname[0] == 0)
		printf(
"A copy of an editor buffer of yours was saved when the system went down.\n\
No name was associated with this buffer so it has been named \"LOST\".\n"
		);
	else
		printf(
"A copy of an editor buffer of your file \"%s\"\n\
was saved when the system went down.\n",
		nfname);
	printf(
"This buffer can be retrieved using the \"recover\" command of the editor.\n\
For more information enter the editor (edit or ex) and type \"help recover\".\n"
		);
	flush();
	close(pvec[1]);
	wait();
}
