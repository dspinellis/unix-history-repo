/* Copyright (c) 1979 Regents of the University of California */
#

/*
 * Mail -- a mail program
 *
 * Collect input from standard input, handling
 * ~ escapes.
 */

#include "rcv.h"
#include <sys/stat.h>

/*
 * Read a message from standard output and return a read file to it
 * or NULL on error.
 */


/*
 * The following hokiness with global variables is so that on
 * receipt of an interrupt signal, the partial message can be salted
 * away on dead.letter.  The output file must be available to flush,
 * and the input to read.  Several open files could be saved all through
 * Mail if stdio allowed simultaneous read/write access.
 */

static	int	(*savesig)();		/* Previous SIGINT value */
static	FILE	*newi;			/* File for saving away */
static	FILE	*newo;			/* Output side of same */
static	int	hf;			/* Ignore interrups */

FILE *
collect(hp)
	struct header *hp;
{
	FILE *ibuf, *fbuf, *obuf;
	int lc, cc, escape, collrub(), intack();
	register int c, t;
	char linebuf[LINESIZE], *cp;
	extern char tempMail[];

	if (value("ignore") != NOSTR)
		hf = 1;
	else
		hf = 0;
	if ((savesig = signal(SIGINT, SIG_IGN)) != SIG_IGN)
		signal(SIGINT, hf ? intack : collrub);
	newi = NULL;
	newo = NULL;
	if ((obuf = fopen(tempMail, "w")) == NULL) {
		perror(tempMail);
		goto err;
	}
	newo = obuf;
	if ((ibuf = fopen(tempMail, "r")) == NULL) {
		perror(tempMail);
		newo = NULL;
		fclose(obuf);
		goto err;
	}
	newi = ibuf;
	unlink(tempMail);
	if (hp->h_seq != 0) {
		puthead(hp, stdout);
		fflush(stdout);
	}
	if (intty && hp->h_subj == NOSTR && value("ask"))
		grabh(hp, GSUBJ);
	escape = ESCAPE;
	if ((cp = value("escape")) != NOSTR)
		escape = *cp;
	while (readline(stdin, linebuf) > 0) {
		if (linebuf[0] != escape) {
			if ((t = putline(obuf, linebuf)) < 0)
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
				if (putline(obuf, &linebuf[1]) < 0)
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

		case '.':
			/*
			 * Simulate end of file on input.
			 */
			goto eof;

		case 'q':
		case 'Q':
			/*
			 * Force a quit of sending mail.
			 * Act like an interrupt happened.
			 */

			collrub(SIGINT);
			exit(1);

		case 'h':
			/*
			 * Grab a bunch of headers.
			 */
			if (!intty || !outtty) {
				printf("~h: no can do!?\n");
				break;
			}
			grabh(hp, GTO|GSUBJ|GCC);
			printf("(continue)\n");
			break;

		case 't':
			/*
			 * Add to the To list.
			 */

			hp->h_to = addto(hp->h_to, &linebuf[2]);
			hp->h_seq++;
			break;

		case 's':
			/*
			 * Set the Subj list.
			 */

			cp = &linebuf[2];
			while (any(*cp, " \t"))
				cp++;
			hp->h_subj = savestr(cp);
			hp->h_seq++;
			break;

		case 'c':
			/*
			 * Add to the CC list.
			 */

			hp->h_cc = addto(hp->h_cc, &linebuf[2]);
			hp->h_seq++;
			break;

		case 'd':
			copy(deadletter, &linebuf[2]);
			/* fall into . . . */

		case 'r':
			/*
			 * Invoke a file:
			 * Search for the file name,
			 * then open it and copy the contents to obuf.
			 */

			cp = &linebuf[2];
			while (any(*cp, " \t"))
				cp++;
			if (*cp == '\0') {
				printf("Interpolate what file?\n");
				break;
			}
			if (isdir(cp)) {
				printf("%s: directory\n");
				break;
			}
			if ((fbuf = fopen(cp, "r")) == NULL) {
				perror(cp);
				break;
			}
			printf("\"%s\" ", cp);
			flush();
			lc = 0;
			cc = 0;
			while (readline(fbuf, linebuf) > 0) {
				lc++;
				if ((t = putline(obuf, linebuf)) < 0) {
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
			fflush(obuf);
			rewind(ibuf);
			exwrite(cp, ibuf, 1);
			break;

		case 'm':
			/*
			 * Interpolate the named messages, if we
			 * are in receiving mail mode.  Does the
			 * standard list processing garbage.
			 */

			if (!rcvmode) {
				printf("No messages to send from!?!\n");
				break;
			}
			cp = &linebuf[2];
			while (any(*cp, " \t"))
				cp++;
			if (forward(cp, obuf) < 0)
				goto err;
			printf("(continue)\n");
			break;

		case '?':
			if ((fbuf = fopen(THELPFILE, "r")) == NULL) {
				printf("No help just now.\n");
				break;
			}
			t = getc(fbuf);
			while (t != -1) {
				putchar(t);
				t = getc(fbuf);
			}
			fclose(fbuf);
			break;

		case 'p':
			/*
			 * Print out the current state of the
			 * message without altering anything.
			 */

			fflush(obuf);
			rewind(ibuf);
			printf("-------\nMessage contains:\n");
			puthead(hp, stdout);
			t = getc(ibuf);
			while (t != EOF) {
				putchar(t);
				t = getc(ibuf);
			}
			printf("(continue)\n");
			break;

		case '^':
		case '|':
			/*
			 * Pipe message through command.
			 * Collect output as new message.
			 */

			obuf = mespipe(ibuf, obuf, &linebuf[2]);
			newo = obuf;
			ibuf = newi;
			newi = ibuf;
			printf("(continue)\n");
			break;

		case 'v':
		case 'e':
			/*
			 * Edit the current message.
			 * 'e' means to use EDITOR
			 * 'v' means to use VISUAL
			 */

			if ((obuf = mesedit(ibuf, obuf, c)) == NULL)
				goto err;
			newo = obuf;
			ibuf = newi;
			printf("(continue)\n");
			break;
			break;
		}
		flush();
	}
eof:
	fclose(obuf);
	rewind(ibuf);
	signal(SIGINT, savesig);
	return(ibuf);

err:
	fclose(ibuf);
	fclose(obuf);
	signal(SIGINT, savesig);
	return(NULL);
}

/*
 * Write a file, ex-like if f set.
 */

exwrite(name, ibuf, f)
	char name[];
	FILE *ibuf;
{
	register FILE *of;
	register int c;
	long cc;
	int lc;
	struct stat junk;

	if (stat(name, &junk) >= 0) {
		fprintf(stderr, "%s: File exists\n", name);
		return(-1);
	}
	if ((of = fopen(name, "w")) == NULL) {
		perror(name);
		return(-1);
	}
	lc = 0;
	cc = 0;
	if (f) {
		printf("\"%s\" ", name);
		fflush(stdout);
	}
	while ((c = getc(ibuf)) != EOF) {
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
 * Edit the message being collected on ibuf and obuf.
 * Write the message out onto some poorly-named temp file
 * and point an editor at it.
 *
 * On return, make the edit file the new temp file.
 */

FILE *
mesedit(ibuf, obuf, c)
	FILE *ibuf, *obuf;
{
	int pid, s;
	FILE *fbuf;
	register int t;
	int (*sig)();
	struct stat sbuf;
	extern char tempMail[], tempEdit[];
	register char *edit;

	sig = signal(SIGINT, SIG_IGN);
	if (stat(tempEdit, &sbuf) >= 0) {
		printf("%s: file exists\n", tempEdit);
		goto out;
	}
	close(creat(tempEdit, 0600));
	if ((fbuf = fopen(tempEdit, "w")) == NULL) {
		perror(tempEdit);
		goto out;
	}
	fflush(obuf);
	rewind(ibuf);
	t = getc(ibuf);
	while (t != EOF) {
		putc(t, fbuf);
		t = getc(ibuf);
	}
	fflush(fbuf);
	if (ferror(fbuf)) {
		perror(tempEdit);
		unlink(tempEdit);
		goto fix;
	}
	fclose(fbuf);
	pid = fork();
	if (pid == 0) {
		if (sig != SIG_IGN)
			signal(SIGINT, SIG_DFL);
		if ((edit = value(c == 'e' ? "EDITOR" : "VISUAL")) == NOSTR)
			edit = c == 'e' ? EDITOR : VISUAL;
		execl(edit, edit, tempEdit, 0);
		perror(edit);
		exit(1);
	}
	if (pid == -1) {
		perror("fork");
		unlink(tempEdit);
		goto out;
	}
	while (wait(&s) != pid)
		;
	if (s != 0) {
		printf("Fatal error in \"%s\"\n", EDITOR);
		unlink(tempEdit);
		goto out;
	}

	/*
	 * Now switch to new file.
	 */

	if ((fbuf = fopen(tempEdit, "a")) == NULL) {
		perror(tempEdit);
		unlink(tempEdit);
		goto out;
	}
	if ((ibuf = fopen(tempEdit, "r")) == NULL) {
		perror(tempEdit);
		fclose(fbuf);
		unlink(tempEdit);
		goto out;
	}
	unlink(tempEdit);
	fclose(obuf);
	fclose(newi);
	obuf = fbuf;
	goto out;
fix:
	perror(tempEdit);
out:
	signal(SIGINT, sig);
	newi = ibuf;
	return(obuf);
}

/*
 * Pipe the message through the command.
 * Old message is on stdin of command;
 * New message collected from stdout.
 * Sh -c must return 0 to accept the new message.
 */

FILE *
mespipe(ibuf, obuf, cmd)
	FILE *ibuf, *obuf;
	char cmd[];
{
	register FILE *ni, *no;
	int pid, s;
	int (*savesig)();
	char *Shell;

	newi = ibuf;
	if ((no = fopen(tempEdit, "w")) == NULL) {
		perror(tempEdit);
		return(obuf);
	}
	if ((ni = fopen(tempEdit, "r")) == NULL) {
		perror(tempEdit);
		fclose(no);
		unlink(tempEdit);
		return(obuf);
	}
	unlink(tempEdit);
	savesig = signal(SIGINT, SIG_IGN);
	fflush(obuf);
	rewind(ibuf);
	if ((Shell = value("SHELL")) == NULL)
		Shell = "/bin/sh";
	if ((pid = fork()) == -1) {
		perror("fork");
		goto err;
	}
	if (pid == 0) {
		/*
		 * stdin = current message.
		 * stdout = new message.
		 */

		close(0);
		dup(fileno(ibuf));
		close(1);
		dup(fileno(no));
		for (s = 4; s < 15; s++)
			close(s);
		execl(Shell, Shell, "-c", cmd, 0);
		perror(Shell);
		exit(1);
	}
	while (wait(&s) != pid)
		;
	if (s != 0) {
		fprintf(stderr, "\"%s\" failed!?\n", cmd);
		goto err;
	}
	if (fsize(ni) == 0) {
		fprintf(stderr, "No bytes from \"%s\" !?\n", cmd);
		goto err;
	}

	/*
	 * Take new files.
	 */

	newi = ni;
	fclose(ibuf);
	fclose(obuf);
	signal(SIGINT, savesig);
	return(no);

err:
	fclose(no);
	fclose(ni);
	signal(SIGINT, savesig);
	return(obuf);
}

/*
 * Interpolate the named messages into the current
 * message, preceding each line with a tab.
 * Return a count of the number of characters now in
 * the message, or -1 if an error is encountered writing
 * the message temporary.
 */

forward(ms, obuf)
	char ms[];
	FILE *obuf;
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
		if (transmit(&message[*ip-1], obuf) < 0) {
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

transmit(mailp, obuf)
	struct message *mailp;
	FILE *obuf;
{
	register struct message *mp;
	register int c, ch;
	int n, bol;
	FILE *ibuf;

	mp = mailp;
	ibuf = setinput(mp);
	c = msize(mp);
	n = c;
	bol = 1;
	while (c-- > 0) {
		if (bol) {
			bol = 0;
			putc('\t', obuf);
			n++;
			if (ferror(obuf)) {
				perror("/tmp");
				return(-1);
			}
		}
		ch = getc(ibuf);
		if (ch == '\n')
			bol++;
		putc(ch, obuf);
		if (ferror(obuf)) {
			perror("/tmp");
			return(-1);
		}
	}
	return(n);
}

/*
 * On interrupt, go here to save the partial
 * message on #/dead.letter.
 * Then restore signals and execute the normal
 * signal routine.  We only come here if signals
 * were previously set anyway.
 */

collrub(s)
{
	register FILE *dbuf;
	register int c;

	signal(SIGINT, SIG_IGN);
	fclose(newo);
	rewind(newi);
	if (value("save") == NOSTR)
		goto done;
	if ((dbuf = fopen(deadletter, "w")) == NULL)
		goto done;
	chmod(deadletter, 0600);
	while ((c = getc(newi)) != EOF)
		putc(c, dbuf);
	fclose(dbuf);

done:
	fclose(newi);
	signal(SIGINT, savesig);
	if (rcvmode)
		stop();
	else
		exit(1);
}

/*
 * Acknowledge an interrupt signal from the tty by typing an @
 */

intack(s)
{
	
	signal(SIGINT, SIG_IGN);
	putchar('@');
	fflush(stdout);
	clearerr(stdin);
	signal(SIGINT, intack);
}

/*
 * Add a string to the end of a header entry field.
 */

char *
addto(hf, news)
	char hf[], news[];
{
	register char *cp, *cp2, *linebuf;

	if (hf == NOSTR)
		hf = "";
	linebuf = salloc(strlen(hf) + strlen(news) + 1);
	for (cp = hf; any(*cp, " \t"); cp++)
		;
	for (cp2 = linebuf; *cp;)
		*cp2++ = *cp++;
	*cp2++ = ' ';
	for (cp = news; any(*cp, " \t"); cp++)
		;
	while (*cp != '\0')
		*cp2++ = *cp++;
	*cp2 = '\0';
	return(linebuf);
}
