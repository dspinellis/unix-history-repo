/*-
 * Copyright (c) 1982, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)open.c	7.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/reboot.h>

#include <ufs/ufs/dir.h>
#include <stand/saio.h>

int	firstopen;
u_int	opendev;			/* last device opened */
extern u_int bootdev;

struct dirstuff {
	int loc;
	struct iob *io;
	char *name;
};

#ifndef SMALL
static ino_t dlook __P((char *, struct iob *, char *));
static int find __P((char *, struct iob *));
static int getdev __P((char *, int));
static int getunit __P((char *));
static struct direct *readdir __P((struct dirstuff *));
static int openi __P((int, struct iob *));
#endif

open(str, how)
	char *str;
	int how;
{
	static int firstopen = 1;
	register struct iob *file;
	register char *t;
	register int cnt;
	int fdesc, args[8], *argp;

	if (firstopen) {
		for (cnt = 0; cnt < SOPEN_MAX; cnt++)
			iob[cnt].i_flgs = 0;
		firstopen = 0;
	}

	for (fdesc = 0;; fdesc++) {
		if (fdesc == SOPEN_MAX)
			_stop("No more file slots");
		if (iob[fdesc].i_flgs == 0) {
			file = &iob[fdesc];
			file->i_flgs |= F_ALLOC;
			file->i_adapt = file->i_ctlr = file->i_unit =
			    file->i_part = 0;
			break;
		}
	}

	for (cnt = 0; cnt < sizeof(args)/sizeof(args[0]); args[cnt++] = 0);
#ifndef SMALL
	for (t = str; *t && *t != '/' && *t != ':' && *t != '('; ++t)
		if (isupper(*t))
			*t = tolower(*t);
	switch(*t) {
	case '(':	/* type(adapt, ctlr, drive, partition)file */
		if ((file->i_dev = getdev(str, t - str)) == -1)
			goto bad;
		for (argp = args + 4, cnt = 0; *t != ')'; ++cnt) {
			for (++t; isspace(*t); ++t);
			if (*t == ')')
				break;
			if (!isdigit(*t))
				goto badspec;
			*argp++ = atoi(t);
			for (++t; isdigit(*t); ++t);
			if (*t != ',' && *t != ')' || cnt == 4)
				goto badspec;
		}
		for (++t; isspace(*t); ++t);
		argp -= 4;
		file->i_adapt = *argp++;
		file->i_ctlr = *argp++;
		file->i_unit = *argp++;
		file->i_part = *argp;
		break;
	case ':':	/* [A-Za-z]*[0-9]*[A-Za-z]:file */
		for (t = str; *t != ':' && !isdigit(*t); ++t);
		if ((file->i_dev = getdev(str, t - str)) == -1)
			goto bad;
		if ((file->i_unit = getunit(t)) == -1)
			goto bad;
		for (; isdigit(*t); ++t);
		if (*t >= 'a' && *t <= 'h')
			file->i_part = *t++ - 'a';
		if (*t != ':') {
			errno = EOFFSET;
			goto badspec;
		}
		for (++t; isspace(*t); ++t);
		break;
	case '/':
	default:		/* default bootstrap unit and device */
#else
	{
#endif /* SMALL */
		file->i_dev = B_TYPE(bootdev);
		file->i_adapt = B_ADAPTOR(bootdev);
		file->i_ctlr = B_CONTROLLER(bootdev);
		file->i_unit = B_UNIT(bootdev);
		file->i_part = B_PARTITION(bootdev);
		t = str;
	}

	opendev = MAKEBOOTDEV(file->i_dev, file->i_adapt, file->i_ctlr,
	    file->i_unit, file->i_part);

	if (errno = devopen(file))
		goto bad;

	if (*t == '\0') {
		file->i_flgs |= how + 1;
		file->i_cc = 0;
		file->i_offset = 0;
		return (fdesc+3);
	}
#ifndef SMALL
	else if (how != 0) {
		printf("Can't write files yet.. Sorry\n");
		errno = EIO;
		goto bad;
	}
#endif
	file->i_ma = (char *)(&file->i_fs);
	file->i_cc = SBSIZE;
	file->i_bn = SBOFF / DEV_BSIZE + file->i_boff;
	file->i_offset = 0;
	if (devread(file) < 0) {
		errno = file->i_error;
		printf("super block read error\n");
		goto bad;
	}
	if ((cnt = find(t, file)) == 0) {
		errno = ESRCH;
		goto bad;
	}
	if (openi(cnt, file) < 0) {
		errno = file->i_error;
		goto bad;
	}
	file->i_offset = 0;
	file->i_cc = 0;
	file->i_flgs |= F_FILE | (how+1);
	return (fdesc+3);

#ifndef SMALL
badspec:
	printf("malformed device specification\nusage: device(adaptor, controller, drive, partition)file -or- <device><unit><partitionletter>:<file>\n");
#endif
bad:
	file->i_flgs = 0;
	return (-1);
}

#ifndef SMALL
static
getdev(str, len)
	register char *str;
	int len;
{
	register struct devsw *dp;
	register int i;
	char savedch = str[len];

	str[len] = '\0';
	for (dp = devsw, i = 0; i < ndevs; dp++, i++)
		if (dp->dv_name && strcmp(str, dp->dv_name) == 0) {
			str[len] = savedch;
			return (i);
		}
	printf("Unknown device\nKnown devices are:\n");
	for (dp = devsw, i = 0; i < ndevs; dp++, i++)
		if (dp->dv_name)
			printf(" %s", dp->dv_name);
	printf("\n");
	errno = ENXIO;
	return (-1);
}

static
getunit(cp)
	register char *cp;
{
	int unit;

	unit = atoi(cp);
	if ((u_int)unit > 255) {
		printf("minor device number out of range (0-255)\n");
		errno = EUNIT;
		return (-1);
	}
	return (unit);
}
#endif /* SMALL */

static
find(path, file)
	register char *path;
	struct iob *file;
{
	register char *q;
	char *dir, c;
	int n;

	if (path == NULL || *path == '\0') {
		printf("null path\n");
		return (0);
	}

	if (openi((ino_t) ROOTINO, file) < 0) {
		printf("can't read root inode\n");
		return (0);
	}
	dir = path;
	while (*path) {
		while (*path == '/')
			path++;
		q = path;
		while(*q != '/' && *q != '\0')
			q++;
		c = *q;
		*q = '\0';
		if (q == path) path = "." ;	/* "/" means "/." */

		if ((n = dlook(path, file, dir)) != 0) {
			if (c == '\0')
				break;
			if (openi(n, file) < 0)
				return (0);
			*q = c;
			path = q;
			continue;
		} else {
			printf("%s: not found\n", path);
			return (0);
		}
	}
	return (n);
}

static ino_t
dlook(s, io, dir)
	char *s;
	register struct iob *io;
	char *dir;
{
	register struct direct *dp;
	register struct dinode *ip;
	struct dirstuff dirp;
	int len;

	if (s == NULL || *s == '\0')
		return (0);
	ip = &io->i_ino;
	if ((ip->di_mode&IFMT) != IFDIR) {
		printf("%s: not a directory\n", dir);
		return (0);
	}
	if (ip->di_size == 0) {
		printf("%s: zero length directory\n", dir);
		return (0);
	}
	len = strlen(s);
	dirp.loc = 0;
	dirp.io = io;
	dirp.name = dir;
	for (dp = readdir(&dirp); dp != NULL; dp = readdir(&dirp)) {
		if(dp->d_ino == 0)
			continue;
		if (dp->d_namlen == len && !strcmp(s, dp->d_name))
			return (dp->d_ino);
	}
	return (0);
}

static struct direct *
readdir(dirp)
	register struct dirstuff *dirp;
{
	register struct direct *dp;
	register struct iob *io;
	daddr_t lbn, d;
	int off;

	io = dirp->io;
	for(;;) {
		if (dirp->loc >= io->i_ino.di_size)
			return (NULL);
		off = blkoff(&io->i_fs, dirp->loc);
		if (off == 0) {
			lbn = lblkno(&io->i_fs, dirp->loc);
			d = bmap(io, lbn);
			if(d == 0)
				return (NULL);
			io->i_bn = fsbtodb(&io->i_fs, d) + io->i_boff;
			io->i_ma = io->i_buf;
			io->i_cc = dblksize(&io->i_fs, &io->i_ino, lbn);
			if (devread(io) < 0) {
				errno = io->i_error;
				printf("%s: directory read error, bn %ld\n",
					dirp->name, io->i_bn);
				return (NULL);
			}
		}
		dp = (struct direct *)(io->i_buf + off);
		dirp->loc += dp->d_reclen;
		if (dp->d_ino == 0) {
			if (dp->d_reclen == 0) {
				printf("%s: bad directory entry, offset %ld\n",
				       dirp->name, dirp->loc);
				return (NULL);
			}
			continue;
		}
		return (dp);
	}
}

static
openi(n, io)
	int n;
	register struct iob *io;
{
	register struct dinode *dp;
	int cc;

	io->i_offset = 0;
	io->i_bn = fsbtodb(&io->i_fs, itod(&io->i_fs, n)) + io->i_boff;
	io->i_cc = io->i_fs.fs_bsize;
	io->i_ma = io->i_buf;
	cc = devread(io);
	dp = (struct dinode *)io->i_buf;
	io->i_ino = dp[itoo(&io->i_fs, n)];
	return (cc);
}
