#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>

struct types {
	int	mode;
	char	*name;
} types[] = {
	{ S_IFDIR,	"directory" },
	{ S_IFCHR,	"character special" },
	{ S_IFBLK,	"block special" },
	{ S_IFREG,	"regular" },
	{ S_IFLNK,	"symbolic link" },
	{ S_IFSOCK,	"socket" },
	{ 0, 0 }
};

main()
{
	register int i, j = getdtablesize();
	struct stat sb;
	char *filetype();

	for (i = 0; i < j; i++) {
		if (fstat(i, &sb) < 0)
			continue;
		fprintf(stderr, "%d: %s, inode #%d, mode %o, dev (%d,%d)",
		    i, filetype(sb.st_mode & S_IFMT), sb.st_ino,
		    sb.st_mode &~ S_IFMT,
		    major(sb.st_dev), minor(sb.st_dev));
		if ((sb.st_mode & S_IFMT) == S_IFCHR ||
		    (sb.st_mode & S_IFMT) == S_IFBLK)
			fprintf(stderr, ", rdev (%d, %d)",
			    major(sb.st_rdev), minor(sb.st_rdev));
		putc('\n', stderr);
	}
	exit(0);
}

char *
filetype(t)
	register int t;
{
	register struct types *p;

	for (p = types; p->mode; p++)
		if (p->mode == t)
			return (p->name);
	return ("???");
}
