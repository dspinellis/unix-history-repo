/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)fio.c	5.6 (Berkeley) %G%";
#endif /* notdef */

#include "rcv.h"
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <errno.h>

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
	register FILE *ibuf;
{
	register c;
	register char *cp, *cp2;
	register count;
	char linebuf[LINESIZE];
	int maybe, inhead;
	FILE *mestmp;
	off_t offset;
	struct message this;
	extern char tempSet[];

	if ((c = opentemp(tempSet)) < 0)
		exit(1);
	if ((mestmp = fdopen(c, "r+")) == NULL)
		panic("Can't open temporary");
	msgCount = 0;
	maybe = 1;
	inhead = 0;
	offset = 0;
	this.m_flag = MUSED|MNEW;
	this.m_size = 0;
	this.m_lines = 0;
	this.m_block = 0;
	this.m_offset = 0;
	for (;;) {
		if (fgets(linebuf, LINESIZE, ibuf) == NULL) {
			if (append(&this, mestmp)) {
				perror(tempSet);
				exit(1);
			}
			fclose(ibuf);
			makemessage(mestmp);
			return;
		}
		count = strlen(linebuf);
		fwrite(linebuf, sizeof *linebuf, count, otf);
		if (ferror(otf)) {
			perror("/tmp");
			exit(1);
		}
		linebuf[count - 1] = 0;
		if (maybe && linebuf[0] == 'F' && ishead(linebuf)) {
			msgCount++;
			if (append(&this, mestmp)) {
				perror(tempSet);
				exit(1);
			}
			this.m_flag = MUSED|MNEW;
			this.m_size = 0;
			this.m_lines = 0;
			this.m_block = blockof(offset);
			this.m_offset = offsetof(offset);
			inhead = 1;
		} else if (linebuf[0] == 0) {
			inhead = 0;
		} else if (inhead) {
			for (cp = linebuf, cp2 = "status";; cp++) {
				if ((c = *cp2++) == 0) {
					while (isspace(*cp++))
						;
					if (cp[-1] != ':')
						break;
					while (c = *cp++)
						if (c == 'R')
							this.m_flag |= MREAD;
						else if (c == 'O')
							this.m_flag &= ~MNEW;
					inhead = 0;
					break;
				}
				if (*cp != c && *cp != toupper(c))
					break;
			}
		}
		offset += count;
		this.m_size += count;
		this.m_lines++;
		maybe = linebuf[0] == 0;
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
	fwrite(linebuf, sizeof *linebuf, c, obuf);
	putc('\n', obuf);
	if (ferror(obuf))
		return (-1);
	return (c + 1);
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
	register int n;

	clearerr(ibuf);
	if (fgets(linebuf, LINESIZE, ibuf) == NULL)
		return -1;
	n = strlen(linebuf);
	if (n > 0 && linebuf[n - 1] == '\n')
		linebuf[--n] = '\0';
	return n;
}

/*
 * Return a file buffer all ready to read up the
 * passed message pointer.
 */
FILE *
setinput(mp)
	register struct message *mp;
{

	fflush(otf);
	if (fseek(itf, positionof(mp->m_block, mp->m_offset), 0) < 0) {
		perror("fseek");
		panic("temporary file seek");
	}
	return (itf);
}

/*
 * Take the data out of the passed ghost file and toss it into
 * a dynamically allocated message structure.
 */
makemessage(f)
	FILE *f;
{
	register size = (msgCount + 1) * sizeof (struct message);
	off_t lseek();

	if (message != 0)
		free((char *) message);
	if ((message = (struct message *) malloc((unsigned) size)) == 0)
		panic("Insufficient memory for %d messages", msgCount);
	dot = message;
	size -= sizeof (struct message);
	fflush(f);
	lseek(fileno(f), (long) sizeof *message, 0);
	if (read(fileno(f), (char *) message, size) != size)
		panic("Message temporary file corrupted");
	message[msgCount].m_size = 0;
	message[msgCount].m_lines = 0;
	fclose(f);
}

/*
 * Append the passed message descriptor onto the temp file.
 * If the write fails, return 1, else 0
 */
append(mp, f)
	struct message *mp;
	FILE *f;
{
	return fwrite((char *) mp, sizeof *mp, 1, f) != 1;
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
	return unlink(name);
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
	char *mktemp();

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
	fflush(stdout);
	if ((obuf = fopen(editfile, "r+")) == NULL) {
		perror(editfile);
		relsesigs();
		reset(0);
	}
	trunc(obuf);
	c = 0;
	for (mp = &message[0]; mp < &message[msgCount]; mp++) {
		if ((mp->m_flag & MDELETED) != 0)
			continue;
		c++;
		if (send(mp, obuf, 0) < 0) {
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
	fflush(stdout);

done:
	relsesigs();
}

static int sigdepth = 0;		/* depth of holdsigs() */
static int omask = 0;
/*
 * Hold signals SIGHUP, SIGINT, and SIGQUIT.
 */
holdsigs()
{

	if (sigdepth++ == 0)
		omask = sigblock(sigmask(SIGHUP)|sigmask(SIGINT)|sigmask(SIGQUIT));
}

/*
 * Release signals SIGHUP, SIGINT, and SIGQUIT.
 */
relsesigs()
{

	if (--sigdepth == 0)
		sigsetmask(omask);
}

/*
 * Open a temp file by creating and unlinking.
 * Return the open file descriptor.
 */
opentemp(file)
	char file[];
{
	int f;

	if ((f = open(file, O_CREAT|O_EXCL|O_RDWR, 0600)) < 0)
		perror(file);
	remove(file);
	return (f);
}

/*
 * Determine the size of the file possessed by
 * the passed buffer.
 */
off_t
fsize(iob)
	FILE *iob;
{
	struct stat sbuf;

	if (fstat(fileno(iob), &sbuf) < 0)
		return 0;
	return sbuf.st_size;
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
	register int pid, l;
	register char *cp, *Shell;
	int pivec[2];
	struct stat sbuf;
	union wait s;

	if (name[0] == '+' && getfold(cmdbuf) >= 0) {
		sprintf(xname, "%s/%s", cmdbuf, name + 1);
		return(expand(savestr(xname)));
	}
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
	if (s.w_status != 0 && s.w_termsig != SIGPIPE) {
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
 * Determine the current folder directory name.
 */
getfold(name)
	char *name;
{
	char *folder;

	if ((folder = value("folder")) == NOSTR)
		return (-1);
	if (*folder == '/')
		strcpy(name, folder);
	else
		sprintf(name, "%s/%s", homedir, folder);
	return (0);
}

/*
 * A nicer version of Fdopen, which allows us to fclose
 * without losing the open file.
 */
FILE *
Fdopen(fildes, mode)
	char *mode;
{
	int f;

	if ((f = dup(fildes)) < 0) {
		perror("dup");
		return (NULL);
	}
	return fdopen(f, mode);
}
