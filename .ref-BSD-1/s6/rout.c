#
/*
 * Rout - rout out the shit in /tmp
 * Bill Joy UCB October 12, 1977
 */

/* Current definition of shit is anything not accessed for 3 hours */

#define	SHITTIME	60 * 60 * 3

main()
{
	struct {
		int inum;
		char name[16];
	} dirent;
	char name[30];
	long tvec;
	struct {
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

	close(0);
	if (chdir("/tmp") < 0) {
		perror("/tmp");
		exit(1);
	}
	if (open(".", 0) < 0) {
		perror("/tmp");
		exit(1);
	}
	time(&tvec);
	dirent.name[14] = 0;
	while(read(0, &dirent, sizeof dirent - 2) == sizeof dirent - 2) {
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
		if ((stbuff.flags & 060000) == 040000)
			continue;
		if (tvec - stbuff.actime >= SHITTIME) {
			psecs(tvec - stbuff.actime);
			printf(" %s\n", dirent.name);
			unlink(dirent.name);
		}
	}
}
psecs(l)
	long l;
{
	register int i;

	i = l / 3600.0;
	if (i) {
		p2dig(i);
		printf(":");
		i = l % 3600;
		p2dig(i / 60);
		goto minsec;
	}
	i = l;
	p2dig(i / 60);
minsec:
	i =% 60;
	printf(":");
	p2dig(i);
}

p2dig(i)
	int i;
{
	prn(i / 10);
	prn(i % 10);
}
prn(n)
	int n;
{
	register a;

	a = n / 10;
	if (a != 0)
		prn(a);
	n = n % 10 + '0';
	putchar(n);
}

strcmp(cp, dp)
	register char *cp, *dp;
{

	while (cp[0] && dp[0] && *cp == *dp)
		cp++, dp++;
	if (*cp == 0 && *dp == 0)
		return (0);
	return (1);
}
