/* Copyright (c) 1979 Regents of the University of California */
#

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * File I/O.
 */

/*
 * Set up the input pointers while copying the mail file into
 * /tmp.
 */

setptr(ibuf)
	FILE *ibuf;
{
	register int count, s, l;
	off_t offset;
	char linebuf[LINESIZE];
	int maybe, mestmp;
	struct message this;
	extern char tempSet[];

	if ((mestmp = opentemp(tempSet)) < 0)
		exit(1);
	msgCount = 0;
	offset = 0;
	s = 0;
	l = 0;
	maybe = 1;
	for (;;) {
		if ((count = readline(ibuf, linebuf)) == 0) {
			this.m_offset = offsetof(offset);
			this.m_block = blockof(offset);
			this.m_size = s;
			this.m_lines = l;
			if (append(&this, mestmp)) {
				perror(tempSet);
				exit(1);
			}
			fclose(ibuf);
			makemessage(mestmp);
			close(mestmp);
			return;
		}
		if (putline(otf, linebuf) < 0) {
			perror("/tmp");
			exit(1);
		}
		if (maybe && ishead(linebuf)) {
			msgCount++;
			this.m_flag = MUSED;
			this.m_block = blockof(offset);
			this.m_offset = offsetof(offset);
			this.m_size = s;
			this.m_lines = l;
			s = 0;
			l = 0;
			if (append(&this, mestmp)) {
				perror(tempSet);
				exit(1);
			}
		}
		offset += count;
		s += count;
		l++;
		maybe = 0;
		if (linebuf[0] == 0)
			maybe = 1;
	}
}

/*
 * Drop the passed line onto the passed output buffer.
 * If a write error occurs, return -1, else the count of
 * characters written, including the newline.
 */

putline(obuf, linebuf)
	FILE *obuf;
	char *linebuf;
{
	register int c;

	c = strlen(linebuf);
	fputs(linebuf, obuf);
	putc('\n', obuf);
	if (ferror(obuf))
		return(-1);
	return(c+1);
}

/*
 * Read up a line from the specified input into the line
 * buffer.  Return the number of characters read.  Do not
 * include the newline at the end.
 */

readline(ibuf, linebuf)
	FILE *ibuf;
	char *linebuf;
{
	register char *cp;
	register c;

again:
	do {
		clearerr(ibuf);
		for (cp=linebuf, c=getc(ibuf); c!='\n' && c!= EOF; c=getc(ibuf))
			if (cp - linebuf < LINESIZE-1)
					*cp++ = c;
	} while (ferror(ibuf) && ibuf == stdin);
	*cp = 0;
	if (c == EOF && cp == linebuf)
		return(0);
	return(cp - linebuf + 1);
}

/*
 * Return a file buffer all ready to read up the
 * passed message pointer.
 */

FILE *
setinput(mp)
	register struct message *mp;
{
	off_t off;

	fflush(otf);
	off = mp->m_block;
	off <<= 9;
	off += mp->m_offset;
	if (fseek(itf, off, 0) < 0) {
		perror("fseek");
		panic("temporary file seek");
	}
	return(itf);
}

/*
 * Take the data out of the passed ghost file and toss it into
 * a dynamically allocated message structure.
 */

makemessage(f)
{
	register struct message *m;
	register char *mp;
	register count;

	mp = calloc((unsigned) (msgCount + 1), sizeof *m);
	if (mp == NOSTR) {
		printf("Insufficient memory for %d messages\n", msgCount);
		exit(1);
	}
	message = (struct message *) mp;
	dot = message;
	lseek(f, 0L, 0);
	while (count = read(f, mp, 512))
		mp += count;
	for (m = &message[0]; m < &message[msgCount]; m++) {
		m->m_size = (m+1)->m_size;
		m->m_lines = (m+1)->m_lines;
	}
	message[msgCount].m_size = 0;
	message[msgCount].m_lines = 0;
}

/*
 * Append the passed message descriptor onto the temp file.
 * If the write fails, return 1, else 0
 */

append(mp, f)
	struct message *mp;
{
	if (write(f, (char *) mp, sizeof *mp) != sizeof *mp)
		return(1);
	return(0);
}

/*
 * Terminate an editing session by attempting to write out the user's
 * file from the temporary.
 */

edstop()
{
	register int gotcha;
	register struct message *mp;
	FILE *obuf;

	for (mp = &message[0], gotcha = 0; mp < &message[msgCount]; mp++)
		if (mp->m_flag & (MODIFY|MDELETED)) {
			gotcha++;
			break;
		}
	if (!gotcha)
		return;
	printf("\"%s\" ", editfile);
	flush();
	if ((obuf = fopen(editfile, "w")) == NULL) {
		perror(editfile);
		reset();
	}
	for (mp = &message[0]; mp < &message[msgCount]; mp++) {
		if ((mp->m_flag & (MDELETED|MSAVED)) != 0)
			continue;
		if (send(mp, obuf) < 0) {
			perror(editfile);
			reset();
		}
	}
	fflush(obuf);
	if (ferror(obuf)) {
		perror(editfile);
		reset();
	}
	printf("complete\n");
	flush();
}

/*
 * Empty the output buffer.
 */

clrbuf(buf)
	register FILE *buf;
{

	buf = stdout;
	buf->_ptr = buf->_base;
	buf->_cnt = BUFSIZ;
}

/*
 * Open a temp file by creating, closing, unlinking, and
 * reopening.  Return the open file descriptor.
 */

opentemp(file)
	char file[];
{
	register int f;

	if ((f = creat(file, 0600)) < 0) {
		perror(file);
		return(-1);
	}
	close(f);
	if ((f = open(file, 2)) < 0) {
		perror(file);
		unlink(file);
		return(-1);
	}
	unlink(file);
	return(f);
}

/*
 * Flush the standard output.
 */

flush()
{
	fflush(stdout);
	fflush(stderr);
}

/*
 * Determine the size of the file possessed by
 * the passed buffer.
 */

off_t
fsize(iob)
	FILE *iob;
{
	register int f;
	struct stat sbuf;

	f = fileno(iob);
	if (fstat(f, &sbuf) < 0)
		return(0);
	return(sbuf.st_size);
}
