/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sys.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "dir.h"
#include "reboot.h"
#include "saio.h"

#define	isdigit(c)	((c) >= '0' && (c) <= '9')
#define	isspace(c)	((c) == ' ' || (c) == '\t')
#define	isupper(c)	((c) >= 'A' && (c) <= 'Z')
#define	tolower(c)	((c) - 'A' + 'a')

ino_t	dlook();

struct dirstuff {
	int loc;
	struct iob *io;
};

struct iob iob[NFILES];

static
openi(n, io)
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
	io->i_ino.i_ic = dp[itoo(&io->i_fs, n)].di_ic;
	return (cc);
}

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

#define	NBUFS	4
static char	b[NBUFS][MAXBSIZE];
static daddr_t	blknos[NBUFS];

static daddr_t
sbmap(io, bn)
	register struct iob *io;
	daddr_t bn;
{
	register struct inode *ip;
	int i, j, sh;
	daddr_t nb, *bap;

	ip = &io->i_ino;
	if (bn < 0) {
		printf("bn negative\n");
		return ((daddr_t)0);
	}

	/*
	 * blocks 0..NDADDR are direct blocks
	 */
	if(bn < NDADDR) {
		nb = ip->i_db[bn];
		return (nb);
	}

	/*
	 * addresses NIADDR have single and double indirect blocks.
	 * the first step is to determine how many levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(&io->i_fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		printf("bn ovf %D\n", bn);
		return ((daddr_t)0);
	}

	/*
	 * fetch the first indirect block address from the inode
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		printf("bn void %D\n",bn);
		return ((daddr_t)0);
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		if (blknos[j] != nb) {
			io->i_bn = fsbtodb(&io->i_fs, nb) + io->i_boff;
			io->i_ma = b[j];
			io->i_cc = io->i_fs.fs_bsize;
			if (devread(io) != io->i_fs.fs_bsize) {
				if (io->i_error)
					errno = io->i_error;
				printf("bn %D: read error\n", io->i_bn);
				return ((daddr_t)0);
			}
			blknos[j] = nb;
		}
		bap = (daddr_t *)b[j];
		sh /= NINDIR(&io->i_fs);
		i = (bn / sh) % NINDIR(&io->i_fs);
		nb = bap[i];
		if(nb == 0) {
			printf("bn void %D\n",bn);
			return ((daddr_t)0);
		}
	}
	return (nb);
}

static ino_t
dlook(s, io, dir)
	char *s;
	register struct iob *io;
	char *dir;
{
	register struct direct *dp;
	register struct inode *ip;
	struct dirstuff dirp;
	int len;

	if (s == NULL || *s == '\0')
		return (0);
	ip = &io->i_ino;
	if ((ip->i_mode&IFMT) != IFDIR) {
		printf("%s: not a directory\n", dir);
		return (0);
	}
	if (ip->i_size == 0) {
		printf("%s: zero length directory\n", dir);
		return (0);
	}
	len = strlen(s);
	dirp.loc = 0;
	dirp.io = io;
	for (dp = readdir(&dirp); dp != NULL; dp = readdir(&dirp)) {
		if(dp->d_ino == 0)
			continue;
		if (dp->d_namlen == len && !strcmp(s, dp->d_name))
			return (dp->d_ino);
	}
	return (0);
}

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register struct dirstuff *dirp;
{
	register struct direct *dp;
	register struct iob *io;
	daddr_t lbn, d;
	int off;

	io = dirp->io;
	for(;;) {
		if (dirp->loc >= io->i_ino.i_size)
			return (NULL);
		off = blkoff(&io->i_fs, dirp->loc);
		if (off == 0) {
			lbn = lblkno(&io->i_fs, dirp->loc);
			d = sbmap(io, lbn);
			if(d == 0)
				return (NULL);
			io->i_bn = fsbtodb(&io->i_fs, d) + io->i_boff;
			io->i_ma = io->i_buf;
			io->i_cc = blksize(&io->i_fs, &io->i_ino, lbn);
			if (devread(io) < 0) {
				errno = io->i_error;
				printf("bn %D: directory read error\n",
					io->i_bn);
				return (NULL);
			}
		}
		dp = (struct direct *)(io->i_buf + off);
		dirp->loc += dp->d_reclen;
		if (dp->d_ino == 0)
			continue;
		return (dp);
	}
}

lseek(fdesc, addr, ptr)
	int fdesc, ptr;
	off_t addr;
{
	register struct iob *io;

#ifndef SMALL
	if (ptr != L_SET) {
		printf("Seek not from beginning of file\n");
		errno = EOFFSET;
		return (-1);
	}
#endif
	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((io = &iob[fdesc])->i_flgs & F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	io->i_offset = addr;
	io->i_bn = addr / DEV_BSIZE;
	io->i_cc = 0;
	return (0);
}

getc(fdesc)
	int fdesc;
{
	register struct iob *io;
	register struct fs *fs;
	register char *p;
	int c, lbn, off, size, diff;


	if (fdesc >= 0 && fdesc <= 2)
		return (getchar());
	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((io = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	p = io->i_ma;
	if (io->i_cc <= 0) {
		if ((io->i_flgs & F_FILE) != 0) {
			diff = io->i_ino.i_size - io->i_offset;
			if (diff <= 0)
				return (-1);
			fs = &io->i_fs;
			lbn = lblkno(fs, io->i_offset);
			io->i_bn = fsbtodb(fs, sbmap(io, lbn)) + io->i_boff;
			off = blkoff(fs, io->i_offset);
			size = blksize(fs, &io->i_ino, lbn);
		} else {
			io->i_bn = io->i_offset / DEV_BSIZE;
			off = 0;
			size = DEV_BSIZE;
		}
		io->i_ma = io->i_buf;
		io->i_cc = size;
		if (devread(io) < 0) {
			errno = io->i_error;
			return (-1);
		}
		if ((io->i_flgs & F_FILE) != 0) {
			if (io->i_offset - off + size >= io->i_ino.i_size)
				io->i_cc = diff + off;
			io->i_cc -= off;
		}
		p = &io->i_buf[off];
	}
	io->i_cc--;
	io->i_offset++;
	c = (unsigned)*p++;
	io->i_ma = p;
	return (c);
}

int	errno;

read(fdesc, buf, count)
	int fdesc, count;
	char *buf;
{
	register i, size;
	register struct iob *file;
	register struct fs *fs;
	int lbn, off;

	errno = 0;
	if (fdesc >= 0 & fdesc <= 2) {
		i = count;
		do {
			*buf = getchar();
		} while (--i && *buf++ != '\n');
		return (count - i);
	}
	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((file = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	if ((file->i_flgs&F_READ) == 0) {
		errno = EBADF;
		return (-1);
	}
#ifndef SMALL
	if ((file->i_flgs & F_FILE) == 0) {
		file->i_cc = count;
		file->i_ma = buf;
		file->i_bn = file->i_boff + (file->i_offset / DEV_BSIZE);
		i = devread(file);
		if (i < 0)
			errno = file->i_error;
		else
			file->i_offset += i;
		return (i);
	}
#endif
	if (file->i_offset+count > file->i_ino.i_size)
		count = file->i_ino.i_size - file->i_offset;
	if ((i = count) <= 0)
		return (0);
	/*
	 * While reading full blocks, do I/O into user buffer.
	 * Anything else uses getc().
	 */
	fs = &file->i_fs;
	while (i) {
		off = blkoff(fs, file->i_offset);
		lbn = lblkno(fs, file->i_offset);
		size = blksize(fs, &file->i_ino, lbn);
		if (off == 0 && size <= i) {
			file->i_bn = fsbtodb(fs, sbmap(file, lbn)) +
			    file->i_boff;
			file->i_cc = size;
			file->i_ma = buf;
			if (devread(file) < 0) {
				errno = file->i_error;
				return (-1);
			}
			file->i_offset += size;
			file->i_cc = 0;
			buf += size;
			i -= size;
		} else {
			size -= off;
			if (size > i)
				size = i;
			i -= size;
			do {
				*buf++ = getc(fdesc+3);
			} while (--size);
		}
	}
	return (count);
}

#ifndef SMALL
write(fdesc, buf, count)
	int fdesc, count;
	char *buf;
{
	register i;
	register struct iob *file;

	errno = 0;
	if (fdesc >= 0 && fdesc <= 2) {
		i = count;
		while (i--)
			putchar(*buf++);
		return (count);
	}
	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((file = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	if ((file->i_flgs&F_WRITE) == 0) {
		errno = EBADF;
		return (-1);
	}
	file->i_cc = count;
	file->i_ma = buf;
	file->i_bn = file->i_boff + (file->i_offset / DEV_BSIZE);
	i = devwrite(file);
	file->i_offset += count;
	if (i < 0)
		errno = file->i_error;
	return (i);
}
#endif

int	openfirst = 1;
u_int	opendev;		/* last device opened */
extern u_int bootdev;

open(str, how)
	char *str;
	int how;
{
	register char *t;
	register int cnt;
	register struct iob *file;
	int fdesc, args[8], *argp;

	if (openfirst) {
		for (cnt = 0; cnt < NFILES; cnt++)
			iob[cnt].i_flgs = 0;
		openfirst = 0;
	}

	for (fdesc = 0;; fdesc++) {
		if (fdesc == NFILES)
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
		if ((file->i_ino.i_dev = getdev(str, t - str)) == -1)
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
		if ((file->i_ino.i_dev = getdev(str, t - str)) == -1)
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
		file->i_ino.i_dev = B_TYPE(bootdev);
		file->i_adapt = B_ADAPTOR(bootdev);
		file->i_ctlr = B_CONTROLLER(bootdev);
		file->i_unit = B_UNIT(bootdev);
		file->i_part = B_PARTITION(bootdev);
		t = str;
	}

	opendev = MAKEBOOTDEV(file->i_ino.i_dev, file->i_adapt, file->i_ctlr,
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
	printf("malformed device specification\nusage: device(adaptor, controller, drive, partition)file\n");
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

close(fdesc)
	int fdesc;
{
	struct iob *file;

	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((file = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	if ((file->i_flgs&F_FILE) == 0)
		devclose(file);
	file->i_flgs = 0;
	return (0);
}

#ifndef SMALL
ioctl(fdesc, cmd, arg)
	int fdesc, cmd;
	char *arg;
{
	register struct iob *file;
	int error = 0;

	fdesc -= 3;
	if (fdesc < 0 || fdesc >= NFILES ||
	    ((file = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	switch (cmd) {

	case SAIOHDR:
		file->i_flgs |= F_HDR;
		break;

	case SAIOCHECK:
		file->i_flgs |= F_CHECK;
		break;

	case SAIOHCHECK:
		file->i_flgs |= F_HCHECK;
		break;

	case SAIONOBAD:
		file->i_flgs |= F_NBSF;
		break;

	case SAIODOBAD:
		file->i_flgs &= ~F_NBSF;
		break;

	default:
		error = devioctl(file, cmd, arg);
		break;
	}
	if (error < 0)
		errno = file->i_error;
	return (error);
}
#endif /* SMALL */

exit()
{
	_stop("Exit called");
}

_stop(s)
	char *s;
{
	int i;

	for (i = 0; i < NFILES; i++)
		if (iob[i].i_flgs != 0)
			close(i);
	printf("%s\n", s);
	_rtt();
}
