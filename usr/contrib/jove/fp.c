/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "io.h"
#include "termcap.h"
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>

#define MAXFILES	20	/* good enough for my purposes */

static File	_openfiles[MAXFILES] = {0};

static File *
f_alloc(name, flags, fd, buffer, buf_size)
char	*buffer;
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
		buffer = emalloc(buf_size);
		fp->f_flags |= F_MYBUF;
	}
	fp->f_base = fp->f_ptr = buffer;
	fp->f_name = copystr(name);

	return fp;
}

gc_openfiles()
{
	register File	*fp;

	for (fp = _openfiles; fp < &_openfiles[MAXFILES]; fp++)
		if (fp->f_flags != 0 && (fp->f_flags & F_LOCK) == 0)
			f_close(fp);
}

File *
fd_open(name, flags, fd, buffer, bsize)
char	*buffer;
{
	return f_alloc(name, flags, fd, buffer, bsize);
}

File *
f_open(name, flags, buffer, buf_size)
char	*name,
	*buffer;
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
			(void) lseek(fd, (long) 0, 2);
	}
	if (mode == F_WRITE)
		fd = creat(name, CreatMode);
	if (fd == -1)
		return NIL;
	return f_alloc(name, flags, fd, buffer, buf_size);
}

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

filbuf(fp)
File	*fp;
{
	if (fp->f_flags & (F_EOF|F_ERR))
		return EOF;
	fp->f_ptr = fp->f_base;
	fp->f_cnt = read(fp->f_fd, fp->f_base, fp->f_bufsize);
	if (fp->f_cnt == -1) {
		printf("[Read error %d]", errno);
		fp->f_flags |= F_ERR;
	}
	if (fp->f_cnt == 0) {
		fp->f_flags |= F_EOF;
		return EOF;
	}
	io_chars += fp->f_cnt;
	return getc(fp);
}

putstr(s)
register char	*s;
{
	register int	c;

	while (c = *s++)
		putchar(c);
}

fputnchar(s, n, fp)
register char	*s;
register int	n;
register File	*fp;
{
	while (--n >= 0)
		putc(*s++, fp);
}

putnchar(s, n)
register char	*s;
register int	n;
{
	fputnchar(s, n, stdout);
}

flusho()
{
	_flush(EOF, stdout);
}

flush(fp)
File	*fp;
{
	_flush(EOF, fp);
}

_flush(c, fp)
register File	*fp;
{
	register int	n;

	if ((fp->f_flags & F_READ) ||
	    ((fp->f_flags & F_STRING)))
		return;
	if (((n = (fp->f_ptr - fp->f_base)) > 0) &&
	    (write(fp->f_fd, fp->f_base, n) != n) &&
	    (fp != stdout))
		error("[I/O error(%d); file = %s, fd = %d]",
			errno, fp->f_name, fp->f_fd);

	if (fp == stdout)
		OkayAbort = YES;
	fp->f_cnt = fp->f_bufsize;
	fp->f_ptr = fp->f_base;
	if (c != EOF)
		putc(c, fp);
}

f_gets(fp, buf, max)
register File	*fp;
char	*buf;
{
	register char	*cp = buf;
	register int	c;
	char	*endp = buf + max - 1;

	if (fp->f_flags & F_EOF)
		return EOF;
	while (((c = getc(fp)) != EOF) && (c != '\n')) {
		if (c == NULL)
			continue;	/* sorry we don't read nulls */
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
	io_lines++;
	return NIL;	/* this means okay */
}

/* Deals with output to the terminal, setting up the amount of characters
   to be buffered depending on the output baud rate.  Why it's in a 
   separate file I don't know ... */

static char	one_buf;

int	BufSize = 1;

static File	_stdout = {1, 1, 1, F_WRITE, &one_buf, &one_buf};
File	*stdout = &_stdout;

/* put a string with padding */

tputc(c)
{
	putchar(c);
}

#undef putchar		/* for files which forget to include io.h,
			   here's a real putchar procedure. */
putchar(c)
{
	putc(c, stdout);
}

putpad(str, lines)
char	*str;
{
	if (str)
		tputs(str, lines, tputc);
}

/* Determine the number of characters to buffer at each baud rate.  The
   lower the number, the quicker the response when new input arrives.  Of
   course the lower the number, the more prone the program is to stop in
   output.  Decide what matters most to you. This sets BufSize to the right
   number or chars, and initiaizes `stdout'.  */

settout(ttbuf)
char	*ttbuf;
{
	static int speeds[] = {
		1,	/* 0	*/
		1,	/* 50	*/
		1,	/* 75	*/
		1,	/* 110	*/
		1,	/* 134	*/
		1,	/* 150	*/
		1,	/* 200	*/
		2,	/* 300	*/
		4,	/* 600	*/
		8,	/* 1200 */
		16,	/* 1800	*/
		32,	/* 2400	*/
		128,	/* 4800	*/
		256,	/* 9600	*/
		512,	/* EXTA	*/
		512	/* EXT	*/
	};
	BufSize = min(512, (speeds[ospeed] * max(LI / 24, 1)));
	stdout = fd_open("/dev/tty", F_WRITE|F_LOCK, 1, ttbuf, BufSize);
}

