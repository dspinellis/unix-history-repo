/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "ctype.h"
#include "termcap.h"
#include "disp.h"

#ifdef MAC
#	include "mac.h"
#else
#	include <sys/stat.h>
#	ifndef MSDOS
#		include <sys/file.h>
#	else /* MSDOS */
#		include <fcntl.h>
#		include <io.h>
#	endif /* MSDOS */
#endif /* MAC */

#include <errno.h>

private File * f_alloc proto((char *, int, int, char *, int));
#ifdef RAINBOW
private int rbwrite proto((int, char *, int));
#endif

#ifndef L_SET
# define L_SET 0
#endif

#define MAXFILES	20	/* good enough for my purposes */

private File	_openfiles[MAXFILES];	/* must be zeroed initially */

private File *
f_alloc(name, flags, fd, buffer, buf_size)
char	*name,
	*buffer;
int	flags,
	fd,
	buf_size;
{
	register File	*fp;
	register int	i;

	for (fp = _openfiles, i = 0; i < MAXFILES; i++, fp++)
		if (fp->f_flags == 0)
			break;
	if (i == MAXFILES)
		complain("[Too many open files!]");
	fp->f_bufsize = buf_size;
	fp->f_cnt = 0;
	fp->f_fd = fd;
	fp->f_flags = flags;
	if (buffer == 0) {
		buffer = emalloc((size_t)buf_size);
		fp->f_flags |= F_MYBUF;
	}
	fp->f_base = fp->f_ptr = buffer;
	fp->f_name = copystr(name);

	return fp;
}

void
gc_openfiles()
{
	register File	*fp;

	for (fp = _openfiles; fp < &_openfiles[MAXFILES]; fp++)
		if (fp->f_flags != 0 && (fp->f_flags & F_LOCKED) == 0)
			f_close(fp);
}

File *
fd_open(name, flags, fd, buffer, bsize)
char	*name,
	*buffer;
int	flags,
	fd,
	bsize;
{
	return f_alloc(name, flags, fd, buffer, bsize);
}

File *
f_open(name, flags, buffer, buf_size)
char	*name,
	*buffer;
int	flags,
	buf_size;
{
	register int	fd;
	int	mode = F_MODE(flags);

	if (mode == F_READ)
		fd = open(name, 0);
	if (mode == F_APPEND) {
		fd = open(name, 1);
		if (fd == -1)
			mode = F_WRITE;
		else
			(void) lseek(fd, 0L, 2);
	}
	if (mode == F_WRITE)
		fd = creat(name, CreatMode);
	if (fd == -1)
		return NIL;
#ifdef MSDOS
	else
		setmode(fd, 0x8000);
#endif /* MSDOS */
	return f_alloc(name, flags, fd, buffer, buf_size);
}

void
f_close(fp)
File	*fp;
{
	flush(fp);
#ifdef BSD4_2
	if (fp->f_flags & (F_WRITE|F_APPEND))
		(void) fsync(fp->f_fd);
#endif
	(void) close(fp->f_fd);
	if (fp->f_flags & F_MYBUF)
		free(fp->f_base);
	free(fp->f_name);
	fp->f_flags = 0;	/* indicates that we're available */
}

int
filbuf(fp)
File	*fp;
{
	if (fp->f_flags & (F_EOF|F_ERR))
		return EOF;
	fp->f_ptr = fp->f_base;
#ifndef MSDOS
	do
#endif /* MSDOS */
		fp->f_cnt = read(fp->f_fd, fp->f_base, (size_t) fp->f_bufsize);
#ifndef MSDOS
	while (fp->f_cnt == -1 && errno == EINTR);
#endif /* MSDOS */
	if (fp->f_cnt == -1) {
		writef("[Read error %d]", errno);
		fp->f_flags |= F_ERR;
	}
	if (fp->f_cnt == 0) {
		fp->f_flags |= F_EOF;
		return EOF;
	}
	io_chars += fp->f_cnt;
	return jgetc(fp);
}

void
putstr(s)
register char	*s;
{
#ifndef IBMPC
	register int	c;

	while ((c = *s++) != '\0')
		jputchar(c);
#else /* IBMPC */
	write_emif(s);
#endif /* IBMPC */
}

void
fputnchar(s, n, fp)
register char	*s;
register int	n;
register File	*fp;
{
	while (--n >= 0)
		jputc(*s++, fp);
}

void
flusho()
{
#ifndef IBMPC
	_flush(EOF, stdout);
#endif /* IBMPC */
}

void
flush(fp)
File	*fp;
{
	_flush(EOF, fp);
}

void
f_seek(fp, offset)
register File	*fp;
off_t	offset;
{
	if (fp->f_flags & F_WRITE)
		flush(fp);
	fp->f_cnt = 0;		/* next read will filbuf(), next write
				   will flush() with no bad effects */
	lseek(fp->f_fd, (long) offset, L_SET);
}

int		/* is void - but for lints sake */
_flush(c, fp)
int	c;
register File	*fp;
{
	register int	n;

	if (fp->f_flags & (F_READ | F_STRING | F_ERR))
		return EOF;
	if (((n = (fp->f_ptr - fp->f_base)) > 0) &&
#ifndef RAINBOW
	    (write(fp->f_fd, fp->f_base, (size_t)n) != n) &&
#else
	    (rbwrite(fp->f_fd, fp->f_base, n) != n) &&
#endif
	    (fp != stdout)) {
		fp->f_flags |= F_ERR;
		error("[I/O error(%d); file = %s, fd = %d]",
			errno, fp->f_name, fp->f_fd);
	}

	fp->f_cnt = fp->f_bufsize;
	fp->f_ptr = fp->f_base;
	if (c != EOF)
		return jputc(c, fp);
	return EOF;
}

int
f_gets(fp, buf, max)
register File	*fp;
char	*buf;
size_t	max;
{
	register char	*cp = buf;
	register int	c;
	char	*endp = buf + max - 1;

	if (fp->f_flags & F_EOF)
		return EOF;
	while (((c = jgetc(fp)) != EOF) && (c != '\n')) {
		if (c == '\0')  /* possibly different from NULL */
			break;		/* sorry we don't read nulls */
#ifdef MSDOS
		if (c == '\r') {
			if ((c = jgetc(fp)) == '\n')
			   break;
			else
			   *cp++ = '\r';
		}
#endif /* MSDOS */
		if (cp >= endp) {
			add_mess(" [Line too long]");
			rbell();
			return EOF;
		}
		*cp++ = c;
	}
	*cp = '\0';
	if (c == EOF) {
		if (cp != buf)
			add_mess(" [Incomplete last line]");
		fp->f_flags |= F_EOF;
		return EOF;
	}
	io_lines += 1;
	return 0;	/* this means okay */
}

/* skip to beginning of next line, i.e., next read returns first
   character of new line */

void
f_toNL(fp)
register File	*fp;
{
	register int	c;

	if (fp->f_flags & F_EOF)
		return;
	while (((c = jgetc(fp)) != EOF) && (c != '\n'))
		;
	if (c == EOF)
		fp->f_flags |= F_EOF;
}

int
f_readn(fp, addr, n)
register File	*fp;
register char	*addr;
register int	n;
{
	int	c,
		nbytes = n;

	while (--n >= 0) {
		c = jgetc(fp);
		if (f_eof(fp))
			break;
		*addr++ = c;
	}
	return (nbytes - (n + 1));
}

int
f_getint(fp)
File	*fp;
{
	int	n = 0,
		c;

	while (isdigit(c = jgetc(fp)))
		n = (n * 10) + c;
	return n;
}

/* Deals with output to the terminal, setting up the amount of characters
   to be buffered depending on the output baud rate.  Why it's in a
   separate file I don't know ... */

private char	one_buf;

int	BufSize = 1;

private File	_stdout = {1, 1, 1, F_WRITE, &one_buf, &one_buf, (char *) NIL};
File	*stdout = &_stdout;

#undef jputchar		/* for files which forget to include fp.h,
			   here's a real jputchar procedure. */
void
jputchar(c)
int	c;
{
	jputc(c, stdout);
}

#ifdef RAINBOW

/*
 * use the Rainbow's video output function
 */

#include <dos.h>

private int
rbwrite(fd, buf, cnt)
char *buf;
{
	union REGS vr;

	if (fd != 1) {
		write(fd, buf, cnt);
	} else {
		while (cnt-- > 0) {
			vr.x.ax = *buf++;
			vr.x.di = 0;
			int86(0x18, &vr, &vr);
		}
	}
}
#endif /* RAINBOW */
