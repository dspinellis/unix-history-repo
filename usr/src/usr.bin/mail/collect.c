/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)collect.c	5.10 (Berkeley) %G%";
#endif /* not lint */

/*
 * Mail -- a mail program
 *
 * Collect input from standard input, handling
 * ~ escapes.
 */

#include "rcv.h"
#include <sys/stat.h>
#include <sys/wait.h>

/*
 * Read a message from standard output and return a read file to it
 * or NULL on error.
 */

/*
 * The following hokiness with global variables is so that on
 * receipt of an interrupt signal, the partial message can be salted
 * away on dead.letter.
 */

static	int	(*saveint)();		/* Previous SIGINT value */
static	int	(*savehup)();		/* Previous SIGHUP value */
static	int	(*savecont)();		/* Previous SIGCONT value */
static	FILE	*collf;			/* File for saving away */
static	int	hadintr;		/* Have seen one SIGINT so far */

static	jmp_buf	coljmp;			/* To get back to work */

FILE *
collect(hp, printheaders)
	struct header *hp;
{
	FILE *fp, *fbuf;
	int lc, cc, escape, eof;
	int collrub(), intack(), collcont();
	register int c, t;
	char linebuf[LINESIZE], *cp;
	extern char tempMail[];
	int notify();
	char getsub;
	int omask;

	noreset++;
	fp = NULL;
	collf = NULL;

	/*
	 * Start catching signals from here, but we're still die on interrupts
	 * until we're in the main loop.
	 */
	omask = sigblock(sigmask(SIGINT) | sigmask(SIGHUP));
	if ((saveint = signal(SIGINT, SIG_IGN)) != SIG_IGN)
		signal(SIGINT, value("ignore") != NOSTR ? intack : collrub);
	if ((savehup = signal(SIGHUP, SIG_IGN)) != SIG_IGN)
		signal(SIGHUP, collrub);
	savecont = signal(SIGCONT, SIG_DFL);
	if (setjmp(coljmp)) {
		remove(tempMail);
		goto err;
	}
	sigsetmask(omask & ~(sigmask(SIGINT) | sigmask(SIGHUP)));

	if ((fp = fopen(tempMail, "w+")) == NULL) {
		perror(tempMail);
		goto err;
	}
	collf = fp;
	remove(tempMail);

	/*
	 * If we are going to prompt for a subject,
	 * refrain from printing a newline after
	 * the headers (since some people mind).
	 */
	t = GTO|GSUBJECT|GCC|GNL;
	getsub = 0;
	if (hp->h_subject == NOSTR && value("interactive") != NOSTR &&
	    value("ask"))
		t &= ~GNL, getsub++;
	if (printheaders) {
		puthead(hp, stdout, t);
		fflush(stdout);
	}
	if ((cp = value("escape")) != NOSTR)
		escape = *cp;
	else
		escape = ESCAPE;
	eof = 0;
	hadintr = 0;

	/*
	 * We can put the setjmp here because register variable
	 * needs to be saved in the loop.
	 */
	if (!setjmp(coljmp)) {
		signal(SIGCONT, collcont);
		if (getsub)
			grabh(hp, GSUBJECT);
	} else {
		/*
		 * Come here for printing the after-signal message.
		 * Duplicate messages won't be printed because
		 * the write is aborted if we get a SIGTTOU.
		 */
cont:
		if (hadintr) {
			fflush(stdout);
			fprintf(stderr,
			"\n(Interrupt -- one more to kill letter)\n");
		} else {
			printf("(continue)\n");
			fflush(stdout);
		}
	}
	for (;;) {
		if (readline(stdin, linebuf) < 0) {
			if (value("interactive") != NOSTR &&
			    value("ignoreeof") != NOSTR) {
				if (++eof > 35)
					break;
				printf("Use \".\" to terminate letter\n");
				continue;
			}
			break;
		}
		eof = 0;
		hadintr = 0;
		if (linebuf[0] == '.' && linebuf[1] == '\0' &&
		    value("interactive") != NOSTR &&
		    (value("dot") != NOSTR || value("ignoreeof") != NOSTR))
			break;
		if (linebuf[0] != escape || value("interactive") == NOSTR) {
			if (putline(fp, linebuf) < 0)
				goto err;
			continue;
		}
		c = linebuf[1];
		switch (c) {
		default:
			/*
			 * On double escape, just send the single one.
			 * Otherwise, it's an error.
			 */
			if (c == escape) {
				if (putline(fp, &linebuf[1]) < 0)
					goto err;
				else
					break;
			}
			printf("Unknown tilde escape.\n");
			break;
		case 'C':
			/*
			 * Dump core.
			 */
			core();
			break;
		case '!':
			/*
			 * Shell escape, send the balance of the
			 * line to sh -c.
			 */
			shell(&linebuf[2]);
			break;
		case ':':
			/*
			 * Escape to command mode, but be nice!
			 */
			execute(&linebuf[2], 1);
			goto cont;
		case '.':
			/*
			 * Simulate end of file on input.
			 */
			goto out;
		case 'q':
			/*
			 * Force a quit of sending mail.
			 * Act like an interrupt happened.
			 */
			hadintr++;
			collrub(SIGINT);
			exit(1);
		case 'h':
			/*
			 * Grab a bunch of headers.
			 */
			grabh(hp, GTO|GSUBJECT|GCC|GBCC);
			goto cont;
		case 't':
			/*
			 * Add to the To list.
			 */
			hp->h_to = cat(hp->h_to, extract(&linebuf[2], GTO));
			break;
		case 's':
			/*
			 * Set the Subject list.
			 */
			cp = &linebuf[2];
			while (isspace(*cp))
				cp++;
			hp->h_subject = savestr(cp);
			break;
		case 'c':
			/*
			 * Add to the CC list.
			 */
			hp->h_cc = cat(hp->h_cc, extract(&linebuf[2], GCC));
			break;
		case 'b':
			/*
			 * Add stuff to blind carbon copies list.
			 */
			hp->h_bcc = cat(hp->h_bcc, extract(&linebuf[2], GBCC));
			break;
		case 'd':
			strcpy(linebuf + 2, deadletter);
			/* fall into . . . */
		case 'r':
			/*
			 * Invoke a file:
			 * Search for the file name,
			 * then open it and copy the contents to fp.
			 */
			cp = &linebuf[2];
			while (isspace(*cp))
				cp++;
			if (*cp == '\0') {
				printf("Interpolate what file?\n");
				break;
			}
			cp = expand(cp);
			if (cp == NOSTR)
				break;
			if (isdir(cp)) {
				printf("%s: Directory\n", cp);
				break;
			}
			if ((fbuf = fopen(cp, "r")) == NULL) {
				perror(cp);
				break;
			}
			printf("\"%s\" ", cp);
			fflush(stdout);
			lc = 0;
			cc = 0;
			while (readline(fbuf, linebuf) >= 0) {
				lc++;
				if ((t = putline(fp, linebuf)) < 0) {
					fclose(fbuf);
					goto err;
				}
				cc += t;
			}
			fclose(fbuf);
			printf("%d/%d\n", lc, cc);
			break;
		case 'w':
			/*
			 * Write the message on a file.
			 */
			cp = &linebuf[2];
			while (any(*cp, " \t"))
				cp++;
			if (*cp == '\0') {
				fprintf(stderr, "Write what file!?\n");
				break;
			}
			if ((cp = expand(cp)) == NOSTR)
				break;
			rewind(fp);
			exwrite(cp, fp, 1);
			break;
		case 'm':
		case 'f':
			/*
			 * Interpolate the named messages, if we
			 * are in receiving mail mode.  Does the
			 * standard list processing garbage.
			 * If ~f is given, we don't shift over.
			 */
			if (!rcvmode) {
				printf("No messages to send from!?!\n");
				break;
			}
			cp = &linebuf[2];
			while (any(*cp, " \t"))
				cp++;
			if (forward(cp, fp, c) < 0)
				goto err;
			goto cont;
		case '?':
			if ((fbuf = fopen(THELPFILE, "r")) == NULL) {
				perror(THELPFILE);
				break;
			}
			while ((t = getc(fbuf)) != EOF)
				putchar(t);
			fclose(fbuf);
			break;
		case 'p':
			/*
			 * Print out the current state of the
			 * message without altering anything.
			 */
			rewind(fp);
			printf("-------\nMessage contains:\n");
			puthead(hp, stdout, GTO|GSUBJECT|GCC|GBCC|GNL);
			while ((t = getc(fp)) != EOF)
				putchar(t);
			goto cont;
		case '|':
			/*
			 * Pipe message through command.
			 * Collect output as new message.
			 */
			rewind(fp);
			fp = mespipe(fp, &linebuf[2]);
			goto cont;
		case 'v':
		case 'e':
			/*
			 * Edit the current message.
			 * 'e' means to use EDITOR
			 * 'v' means to use VISUAL
			 */
			rewind(fp);
			if ((fp = mesedit(fp, c)) == NULL)
				goto err;
			goto cont;
		}
	}
	goto out;
err:
	if (fp != NULL) {
		fclose(fp);
		fp = NULL;
	}
out:
	if (fp != NULL)
		rewind(fp);
	signal(SIGINT, saveint);
	signal(SIGHUP, savehup);
	signal(SIGCONT, savecont);
	sigsetmask(omask);
	noreset = 0;
	return(fp);
}

/*
 * Write a file, ex-like if f set.
 */

exwrite(name, fp, f)
	char name[];
	FILE *fp;
{
	register FILE *of;
	register int c;
	long cc;
	int lc;
	struct stat junk;

	if (f) {
		printf("\"%s\" ", name);
		fflush(stdout);
	}
	if (stat(name, &junk) >= 0 && (junk.st_mode & S_IFMT) == S_IFREG) {
		if (!f)
			fprintf(stderr, "%s: ", name);
		fprintf(stderr, "File exists\n");
		return(-1);
	}
	if ((of = fopen(name, "w")) == NULL) {
		perror(NOSTR);
		return(-1);
	}
	lc = 0;
	cc = 0;
	while ((c = getc(fp)) != EOF) {
		cc++;
		if (c == '\n')
			lc++;
		putc(c, of);
		if (ferror(of)) {
			perror(name);
			fclose(of);
			return(-1);
		}
	}
	fclose(of);
	printf("%d/%ld\n", lc, cc);
	fflush(stdout);
	return(0);
}

/*
 * Edit the message being collected on fp.
 * Write the message out onto some poorly-named temp file
 * and point an editor at it.
 *
 * On return, make the edit file the new temp file.
 */

FILE *
mesedit(fp, c)
	FILE *fp;
{
	int pid;
	union wait s;
	FILE *fbuf;
	register int t;
	int (*sigint)(), (*sigcont)();
	struct stat sbuf;
	extern char tempEdit[];
	register char *edit;

	sigint = signal(SIGINT, SIG_IGN);
	sigcont = signal(SIGCONT, SIG_DFL);
	if (stat(tempEdit, &sbuf) >= 0) {
		printf("%s: file exists\n", tempEdit);
		goto out;
	}
	close(creat(tempEdit, 0600));
	if ((fbuf = fopen(tempEdit, "w")) == NULL) {
		perror(tempEdit);
		goto out;
	}
	while ((t = getc(fp)) != EOF)
		putc(t, fbuf);
	fflush(fbuf);
	if (ferror(fbuf)) {
		perror(tempEdit);
		remove(tempEdit);
		goto fix;
	}
	fclose(fbuf);
	if ((edit = value(c == 'e' ? "EDITOR" : "VISUAL")) == NOSTR)
		edit = c == 'e' ? EDITOR : VISUAL;
	pid = vfork();
	if (pid == 0) {
		if (sigint != SIG_IGN)
			signal(SIGINT, SIG_DFL);
		execl(edit, edit, tempEdit, 0);
		perror(edit);
		_exit(1);
	}
	if (pid == -1) {
		perror("fork");
		remove(tempEdit);
		goto out;
	}
	while (wait(&s) != pid)
		;
	if (s.w_status != 0) {
		printf("Fatal error in \"%s\"\n", edit);
		remove(tempEdit);
		goto out;
	}

	/*
	 * Now switch to new file.
	 */

	if ((fbuf = fopen(tempEdit, "a+")) == NULL) {
		perror(tempEdit);
		remove(tempEdit);
		goto out;
	}
	remove(tempEdit);
	collf = fbuf;
	fclose(fp);
	fp = fbuf;
	goto out;
fix:
	perror(tempEdit);
out:
	signal(SIGCONT, sigcont);
	signal(SIGINT, sigint);
	return(fp);
}

/*
 * Pipe the message through the command.
 * Old message is on stdin of command;
 * New message collected from stdout.
 * Sh -c must return 0 to accept the new message.
 */

FILE *
mespipe(fp, cmd)
	FILE *fp;
	char cmd[];
{
	extern char tempEdit[];
	register FILE *nf;
	int pid;
	union wait s;
	int (*saveint)();
	char *Shell;

	if ((nf = fopen(tempEdit, "w+")) == NULL) {
		perror(tempEdit);
		return(fp);
	}
	remove(tempEdit);
	saveint = signal(SIGINT, SIG_IGN);
	if ((Shell = value("SHELL")) == NULL)
		Shell = "/bin/sh";
	if ((pid = vfork()) == -1) {
		perror("fork");
		goto err;
	}
	if (pid == 0) {
		int fd;
		/*
		 * stdin = current message.
		 * stdout = new message.
		 */

		close(0);
		dup(fileno(fp));
		close(1);
		dup(fileno(nf));
		for (fd = getdtablesize(); --fd > 2;)
			close(fd);
		execl(Shell, Shell, "-c", cmd, 0);
		perror(Shell);
		_exit(1);
	}
	while (wait(&s) != pid)
		;
	if (s.w_status != 0 || pid == -1) {
		fprintf(stderr, "\"%s\" failed!?\n", cmd);
		goto err;
	}
	if (fsize(nf) == 0) {
		fprintf(stderr, "No bytes from \"%s\" !?\n", cmd);
		goto err;
	}

	/*
	 * Take new files.
	 */

	fseek(nf, 0L, 2);
	collf = nf;
	fclose(fp);
	signal(SIGINT, saveint);
	return(nf);

err:
	fclose(nf);
	signal(SIGINT, saveint);
	return(fp);
}

/*
 * Interpolate the named messages into the current
 * message, preceding each line with a tab.
 * Return a count of the number of characters now in
 * the message, or -1 if an error is encountered writing
 * the message temporary.  The flag argument is 'm' if we
 * should shift over and 'f' if not.
 */
forward(ms, fp, f)
	char ms[];
	FILE *fp;
{
	register int *msgvec, *ip;
	extern char tempMail[];

	msgvec = (int *) salloc((msgCount+1) * sizeof *msgvec);
	if (msgvec == (int *) NOSTR)
		return(0);
	if (getmsglist(ms, msgvec, 0) < 0)
		return(0);
	if (*msgvec == NULL) {
		*msgvec = first(0, MMNORM);
		if (*msgvec == NULL) {
			printf("No appropriate messages\n");
			return(0);
		}
		msgvec[1] = NULL;
	}
	printf("Interpolating:");
	for (ip = msgvec; *ip != NULL; ip++) {
		touch(*ip);
		printf(" %d", *ip);
		if (f == 'm') {
			if (transmit(&message[*ip-1], fp) < 0L) {
				perror(tempMail);
				return(-1);
			}
		} else
			if (send(&message[*ip-1], fp, 0) < 0) {
				perror(tempMail);
				return(-1);
			}
	}
	printf("\n");
	return(0);
}

/*
 * Send message described by the passed pointer to the
 * passed output buffer.  Insert a tab in front of each
 * line.  Return a count of the characters sent, or -1
 * on error.
 */
long
transmit(mailp, fp)
	struct message *mailp;
	FILE *fp;
{
	register struct message *mp;
	register int ch;
	long c, n;
	int bol;
	FILE *ibuf;
	char *tabst;

	mp = mailp;
	ibuf = setinput(mp);
	c = mp->m_size;
	n = c;
	bol = 1;
	if ((tabst = value("tabstr")) == NOSTR)
		tabst = "\t";
	while (c-- > 0L) {
		ch = getc(ibuf);
		if (ch == '\n')
			bol = 1;
		else if (bol) {
			bol = 0;
			fputs(tabst, fp);
			n++;
		}
		putc(ch, fp);
		if (ferror(fp)) {
			perror("/tmp");
			return(-1L);
		}
	}
	return(n);
}

/*
 * Print (continue) when continued after ^Z.
 */
/*ARGSUSED*/
collcont(s)
{

	hadintr = 0;
	longjmp(coljmp, 1);
}

/*
 * On interrupt, go here to save the partial
 * message on ~/dead.letter.
 * Then restore signals and execute the normal
 * signal routine.  We only come here if signals
 * were previously set anyway.
 */
collrub(s)
{
	register FILE *dbuf;
	register int c;

	if (s == SIGINT && hadintr == 0) {
		hadintr = 1;
		longjmp(coljmp, 1);
	}
	rewind(collf);
	if (s == SIGINT && value("nosave") != NOSTR || fsize(collf) == 0)
		goto done;
	if ((dbuf = fopen(deadletter, "w")) == NULL)
		goto done;
	chmod(deadletter, 0600);
	while ((c = getc(collf)) != EOF)
		putc(c, dbuf);
	fclose(dbuf);
done:
	fclose(collf);
	signal(SIGINT, saveint);
	signal(SIGHUP, savehup);
	signal(SIGCONT, savecont);
	if (rcvmode) {
		if (s == SIGHUP)
			hangup(SIGHUP);
		else
			stop(s);
	} else
		exit(1);
}

/*
 * Acknowledge an interrupt signal from the tty by typing an @
 */
/*ARGSUSED*/
intack(s)
{

	puts("@");
	fflush(stdout);
	clearerr(stdin);
}
