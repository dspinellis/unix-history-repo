#include "ex.h"
#include "ex_io.h"
/*
 * Ex recovery program - "exrecover dir name"
 * Bill Joy UCB October 1977
 *
 * This program searches through the specified directory and then
 * the directory /usr/preserve looking for an instance of the specified
 * file from a crashed editor or a crashed system.
 * If this file is found, it is unscrambled and written to
 * the standard output.
 * This program normally lives in /usr/lib/exrecover, and is invoked
 * by the ex "recover" command.
 * If this program terminates without a "broken pipe" diagnostic
 * (i.e. the editor doesn't die right away) then the buffer we are
 * writing from is removed when we finish.  This is potentially a mistake
 * as there is not enough handshaking to guarantee that the file has actually
 * been recovered, but should suffice for most cases.
 */
char	nabuf[100];
extern	int iblock, oblock;
int	mask	0377;

main(argc, argv)
	int argc;
	char *argv[];
{
	char *cp;
	extern int tfile, fout;
	int b, i;

	fout = 2;
	fendcore = sbrk(0);
	dot = zero = dol = fendcore;
	one = zero + 1;
	endcore = fendcore - 2;
	iblock = oblock = -1;
	if (argc != 3)
		error(" Wrong number of arguments to exrecover");
	strcpy(file, argv[2]);
	findtmp(argv[1]);
	cp = ctime(header.Atime);
	cp[19] = 0;
	fout = 2;
	printf(" [Dated: %s]", cp);
	header.Alines++;
	if (sbrk(header.Alines * 2) == -1)
		error(" Not enough core for lines");
	for (b = 0; header.Alines > 0; b++, header.Alines =- 256) {
		seek(tfile, blocks[b], 3);
		i = header.Alines < 256 ? header.Alines * 2 : 512;
		if (read(tfile, dot, i) != i)
			ioerror();
		dot =+ i / 2;
	}
	dot--;
	dol = dot;
	scrapbad();
	if (dol > zero) {
		addr1 = one;
		addr2 = dol;
		io = 1;
		putfile();
	}
	unlink(nabuf);
	exit(0);
}

error(str, inf)
	char *str;
{

	printf(str, inf);
	putchar('\n');
	exit(1);
}

/*
 * Routines to keep ex_io.c happy
 */
struct	varbl varbls[1];	/* sham */

strcpy(to, from)
	char *to, *from;
{
	register char *ato;

	ato = to;
	while (*to++ = *from++)
		continue;
	return (ato);
}

strcat(after, what)
	char *after, *what;
{
	register char *both;

	both = after;
	while (*after)
		after++;
	strcpy(after, what);
	return (both);
}

lprintf(a1, a2, a3)
{

	printf(a1, a2, a3);
}

change()
{
	/* more sham */
}

strcmp(cp, dp)
	char *cp, *dp;
{

	while (*cp && *dp && *cp == *dp)
		cp++, dp++;
	return (*cp - *dp);
}

findtmp(dir)
	char *dir;
{

	if (searchdir(dir))
		return;
	if (searchdir("/usr/preserve"))
		return;
	error(" File not found");
}

searchdir(dirname)
	char *dirname;
{
	struct dirent {
		int ino;
		char xxxx[14];
	} dirent0;
	struct {
		int ino;
		char name[16];
	} dirent;
	int dir;

	dir = open(dirname, 0);
	if (dir < 0)
		return (0);
	while (read(dir, &dirent, sizeof dirent0) == sizeof dirent0) {
		if (dirent.ino == 0)
			continue;
		if (dirent.name[0] != 'E')
			continue;
		strcpy(nabuf, dirname);
		strcat(nabuf, "/");
		strcat(nabuf, dirent.name);
		if (yeah(nabuf)) {
			close(dir);
			return (1);
		}
	}
	close(dir);
	return (0);
}

yeah(name)
	char *name;
{
	extern int tfile;

	tfile = open(name, 2);
	if (tfile < 0)
		return (0);
	if (read(tfile, &header, sizeof header) != sizeof header) {
nope:
		close(tfile);
		return (0);
	}
	if (!eq(savedfile, file))
		goto nope;
	if ((getuid() & mask) != header.Auid)
		goto nope;
	seek(tfile, 504, 0);
	write(tfile, "LOST", 5);
	return (1);
}

putnl()
{
	putchar('\n');
}

char	TTYNAM[20];

preserve()
{

}

strend(cp)
	char *cp;
{

	while (*cp)
		cp++;
	return (cp);
}

scrapbad()
{
	register int *ip;
	char *jp;
	struct stb stbuf;
	long size;
	int bno, cnt, bad, was;
	char bk[512], *maxt;
	static char header;
	extern int fout;

	fout = 2;
	fstat(tfile, &stbuf);
	size = stbuf.size0 & 0377;
	size =<< 16;
	size =| stbuf.size1;
	maxt = (size >> 2) | 7;
	bno = (maxt >> 7) & 0777;
/*
	printf("size %ld, maxt %o, bno %d\n", size, maxt, bno);
*/
	while (bno > 0) {
		seek(tfile, bno, 3);
		cnt = read(tfile, bk, 512);
		while (cnt > 0)
			if (bk[--cnt] == 0)
				goto null;
		bno--;
	}
null:
	maxt = ((bno << 7) | (cnt >> 2)) & 0177776;
/*
	printf("bno %d, cnt %d, maxt %o\n", bno, cnt, maxt);
*/
	was = bad = 0;
	for (ip = one; ip <= dol; ip++)
		if (*ip > maxt) {
/*
			printf("%d bad, %o > %o\n", ip - zero, *ip, maxt);
*/
			if (was == 0)
				was = ip - zero;
			*ip = 0176;
		} else if (was) {
			if (bad == 0)
				printf(" [Lost line(s):");
			printf(" %d", was);
			if ((ip - 1) - zero > was)
				printf("-%d", (ip - 1) - zero);
			bad++;
			was = 0;
		}
	if (was != 0) {
		if (bad == 0)
			printf(" [Lost line(s):");
		printf(" %d", was);
		if (dol - zero != was)
			printf("-%d", dol - zero);
		bad++;
	}
	if (bad)
		putchar(']');
}
