#

#include "rcv.h"
#include <sys/stat.h>
#include <errno.h>

/*
 * Mail -- a mail program
 *
 * File I/O.
 */

static char *SccsId = "@(#)fio.c	2.4 %G%";

/*
 * Set up the input pointers while copying the mail file into
 * /tmp.
 */

setptr(ibuf)
	FILE *ibuf;
{
	register int c;
	register char *cp, *cp2;
	register int count, s, l;
	off_t offset;
	char linebuf[LINESIZE];
	char wbuf[LINESIZE];
	int maybe, mestmp, flag, inhead;
	struct message this;
	extern char tempSet[];

	if ((mestmp = opentemp(tempSet)) < 0)
		exit(1);
	msgCount = 0;
	offset = 0;
	s = 0;
	l = 0;
	maybe = 1;
	flag = MUSED|MNEW;
	for (;;) {
		cp = linebuf;
		c = getc(ibuf);
		while (c != EOF && c != '\n') {
			if (cp - linebuf >= LINESIZE - 1) {
				ungetc(c, ibuf);
				*cp = 0;
				break;
			}
			*cp++ = c;
			c = getc(ibuf);
		}
		*cp = 0;
		if (cp == linebuf && c == EOF) {
			this.m_flag = flag;
			flag = MUSED|MNEW;
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
		count = cp - linebuf + 1;
		for (cp = linebuf; *cp;)
			putc(*cp++, otf);
		putc('\n', otf);
		if (ferror(otf)) {
			perror("/tmp");
			exit(1);
		}
		if (maybe && linebuf[0] == 'F' && ishead(linebuf)) {
			msgCount++;
			this.m_flag = flag;
			flag = MUSED|MNEW;
			inhead = 1;
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
		if (linebuf[0] == 0)
			inhead = 0;
		if (inhead && index(linebuf, ':')) {
			cp = linebuf;
			cp2 = wbuf;
			while (isalpha(*cp))
				*cp2++ = *cp++;
			*cp2 = 0;
			if (icequal(wbuf, "status")) {
				cp = index(linebuf, ':');
				if (index(cp, 'R'))
					flag |= MREAD;
				if (index(cp, 'O'))
					flag &= ~MNEW;
				inhead = 0;
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
 * Quickly read a line from the specified input into the line
 * buffer; return characters read.
 */

freadline(ibuf, linebuf)
	register FILE *ibuf;
	register char *linebuf;
{
	register int c;
	register char *cp;

	c = getc(ibuf);
	cp = linebuf;
	while (c != '\n' && c != EOF) {
		if (c == 0) {
			c = getc(ibuf);
			continue;
		}
		if (cp - linebuf >= BUFSIZ-1) {
			*cp = 0;
			return(cp - linebuf + 1);
		}
		*cp++ = c;
		c = getc(ibuf);
	}
	if (c == EOF && cp == linebuf)
		return(0);
	*cp = 0;
	return(cp - linebuf + 1);
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
	register int c;

	do {
		clearerr(ibuf);
		c = getc(ibuf);
		for (cp = linebuf; c != '\n' && c != EOF; c = getc(ibuf)) {
			if (c == 0)
				continue;
			if (cp - linebuf < LINESIZE-2)
				*cp++ = c;
		}
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
	if (message != (struct message *) 0)
		cfree((char *) message);
	message = (struct message *) mp;
	dot = message;
	lseek(f, 0L, 0);
	while (count = read(f, mp, BUFSIZ))
		mp += count;
	for (m = &message[0]; m < &message[msgCount]; m++) {
		m->m_size = (m+1)->m_size;
		m->m_lines = (m+1)->m_lines;
		m->m_flag = (m+1)->m_flag;
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
 * Delete a file, but only if the file is a plain file.
 */

remove(name)
	char name[];
{
	struct stat statb;
	extern int errno;

	if (stat(name, &statb) < 0)
		return(-1);
	if ((statb.st_mode & S_IFMT) != S_IFREG) {
		errno = EISDIR;
		return(-1);
	}
	return(unlink(name));
}

/*
 * Terminate an editing session by attempting to write out the user's
 * file from the temporary.  Save any new stuff appended to the file.
 */
edstop()
{
	register int gotcha, c;
	register struct message *mp;
	FILE *obuf, *ibuf, *readstat;
	struct stat statb;
	char tempname[30], *id;
	int (*sigs[3])();

	if (readonly)
		return;
	holdsigs();
	if (Tflag != NOSTR) {
		if ((readstat = fopen(Tflag, "w")) == NULL)
			Tflag = NOSTR;
	}
	for (mp = &message[0], gotcha = 0; mp < &message[msgCount]; mp++) {
		if (mp->m_flag & MNEW) {
			mp->m_flag &= ~MNEW;
			mp->m_flag |= MSTATUS;
		}
		if (mp->m_flag & (MODIFY|MDELETED|MSTATUS))
			gotcha++;
		if (Tflag != NOSTR && (mp->m_flag & (MREAD|MDELETED)) != 0) {
			if ((id = hfield("article-id", mp)) != NOSTR)
				fprintf(readstat, "%s\n", id);
		}
	}
	if (Tflag != NOSTR)
		fclose(readstat);
	if (!gotcha || Tflag != NOSTR)
		goto done;
	ibuf = NULL;
	if (stat(editfile, &statb) >= 0 && statb.st_size > mailsize) {
		strcpy(tempname, "/tmp/mboxXXXXXX");
		mktemp(tempname);
		if ((obuf = fopen(tempname, "w")) == NULL) {
			perror(tempname);
			relsesigs();
			reset(0);
		}
		if ((ibuf = fopen(editfile, "r")) == NULL) {
			perror(editfile);
			fclose(obuf);
			remove(tempname);
			relsesigs();
			reset(0);
		}
		fseek(ibuf, mailsize, 0);
		while ((c = getc(ibuf)) != EOF)
			putc(c, obuf);
		fclose(ibuf);
		fclose(obuf);
		if ((ibuf = fopen(tempname, "r")) == NULL) {
			perror(tempname);
			remove(tempname);
			relsesigs();
			reset(0);
		}
		remove(tempname);
	}
	printf("\"%s\" ", editfile);
	flush();
	if ((obuf = fopen(editfile, "w")) == NULL) {
		perror(editfile);
		relsesigs();
		reset(0);
	}
	c = 0;
	for (mp = &message[0]; mp < &message[msgCount]; mp++) {
		if ((mp->m_flag & MDELETED) != 0)
			continue;
		c++;
		if (send(mp, obuf) < 0) {
			perror(editfile);
			relsesigs();
			reset(0);
		}
	}
	gotcha = (c == 0 && ibuf == NULL);
	if (ibuf != NULL) {
		while ((c = getc(ibuf)) != EOF)
			putc(c, obuf);
		fclose(ibuf);
	}
	fflush(obuf);
	if (ferror(obuf)) {
		perror(editfile);
		relsesigs();
		reset(0);
	}
	fclose(obuf);
	if (gotcha) {
		remove(editfile);
		printf("removed\n");
	}
	else
		printf("complete\n");
	flush();

done:
	relsesigs();
}

/*
 * Hold signals SIGHUP - SIGQUIT.
 */
holdsigs()
{
	register int i;

	for (i = SIGHUP; i <= SIGQUIT; i++)
		sighold(i);
}

/*
 * Release signals SIGHUP - SIGQUIT
 */
relsesigs()
{
	register int i;

	for (i = SIGHUP; i <= SIGQUIT; i++)
		sigrelse(i);
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
		remove(file);
		return(-1);
	}
	remove(file);
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

/*
 * Take a file name, possibly with shell meta characters
 * in it and expand it by using "sh -c echo filename"
 * Return the file name as a dynamic string.
 */

char *
expand(name)
	char name[];
{
	char xname[BUFSIZ];
	char cmdbuf[BUFSIZ];
	register int pid, l, rc;
	register char *cp, *Shell;
	int s, pivec[2], (*sigint)();
	struct stat sbuf;

	if (!anyof(name, "~{[*?$`'\"\\"))
		return(name);
	if (pipe(pivec) < 0) {
		perror("pipe");
		return(name);
	}
	sprintf(cmdbuf, "echo %s", name);
	if ((pid = vfork()) == 0) {
		Shell = value("SHELL");
		if (Shell == NOSTR)
			Shell = SHELL;
		close(pivec[0]);
		close(1);
		dup(pivec[1]);
		close(pivec[1]);
		close(2);
		execl(Shell, Shell, "-c", cmdbuf, 0);
		_exit(1);
	}
	if (pid == -1) {
		perror("fork");
		close(pivec[0]);
		close(pivec[1]);
		return(NOSTR);
	}
	close(pivec[1]);
	l = read(pivec[0], xname, BUFSIZ);
	close(pivec[0]);
	while (wait(&s) != pid);
		;
	s &= 0377;
	if (s != 0 && s != SIGPIPE) {
		fprintf(stderr, "\"Echo\" failed\n");
		goto err;
	}
	if (l < 0) {
		perror("read");
		goto err;
	}
	if (l == 0) {
		fprintf(stderr, "\"%s\": No match\n", name);
		goto err;
	}
	if (l == BUFSIZ) {
		fprintf(stderr, "Buffer overflow expanding \"%s\"\n", name);
		goto err;
	}
	xname[l] = 0;
	for (cp = &xname[l-1]; *cp == '\n' && cp > xname; cp--)
		;
	*++cp = '\0';
	if (any(' ', xname) && stat(xname, &sbuf) < 0) {
		fprintf(stderr, "\"%s\": Ambiguous\n", name);
		goto err;
	}
	return(savestr(xname));

err:
	return(NOSTR);
}

/*
 * A nicer version of Fdopen, which allows us to fclose
 * without losing the open file.
 */

FILE *
Fdopen(fildes, mode)
	char *mode;
{
	register int f;
	FILE *fdopen();

	f = dup(fildes);
	if (f < 0) {
		perror("dup");
		return(NULL);
	}
	return(fdopen(f, mode));
}
