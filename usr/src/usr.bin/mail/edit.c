/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)edit.c	5.3 (Berkeley) %G%";
#endif not lint

#include "rcv.h"
#include <stdio.h>
#include <sys/stat.h>
#include <sys/wait.h>

/*
 * Mail -- a mail program
 *
 * Perform message editing functions.
 */

/*
 * Edit a message list.
 */

editor(msgvec)
	int *msgvec;
{
	char *edname;

	if ((edname = value("EDITOR")) == NOSTR)
		edname = EDITOR;
	return(edit1(msgvec, edname));
}

/*
 * Invoke the visual editor on a message list.
 */

visual(msgvec)
	int *msgvec;
{
	char *edname;

	if ((edname = value("VISUAL")) == NOSTR)
		edname = VISUAL;
	return(edit1(msgvec, edname));
}

/*
 * Edit a message by writing the message into a funnily-named file
 * (which should not exist) and forking an editor on it.
 * We get the editor from the stuff above.
 */

edit1(msgvec, ed)
	int *msgvec;
	char *ed;
{
	register int c;
	int *ip, pid, mesg;
	int (*sigint)(), (*sigquit)();
	FILE *ibuf, *obuf;
	char edname[15], nbuf[10];
	register struct message *mp;
	extern char tempEdit[];
	off_t fsize(), size;
	struct stat statb;
	long modtime;
	union wait status;

	/*
	 * Set signals; locate editor.
	 */

	sigint = signal(SIGINT, SIG_IGN);
	sigquit = signal(SIGQUIT, SIG_IGN);

	/*
	 * Deal with each message to be edited . . .
	 */

	for (ip = msgvec; *ip && ip-msgvec < msgCount; ip++) {
		mesg = *ip;
		mp = &message[mesg-1];
		mp->m_flag |= MODIFY;
		touch(mesg);
		dot = mp;

		/*
		 * Make up a name for the edit file of the
		 * form "Message%d" and make sure it doesn't
		 * already exist.
		 */
		sprintf(edname, "Message%d", mesg);
		if (!access(edname, 2)) {
			printf("%s: file exists\n", edname);
			goto out;
		}

		/*
		 * Copy the message into the edit file.
		 */
		close(creat(edname, 0600));
		if ((obuf = fopen(edname, "w")) == NULL) {
			perror(edname);
			goto out;
		}
		if (send(mp, obuf, 0) < 0) {
			perror(edname);
			fclose(obuf);
			remove(edname);
			goto out;
		}
		fflush(obuf);
		if (ferror(obuf)) {
			remove(edname);
			fclose(obuf);
			goto out;
		}
		fclose(obuf);

		/*
		 * If we are in read only mode, make the
		 * temporary message file readonly as well.
		 */

		if (readonly)
			chmod(edname, 0400);

		/*
		 * Fork/execl the editor on the edit file.
		 */

		if (stat(edname, &statb) < 0)
			modtime = 0;
		modtime = statb.st_mtime;
		pid = vfork();
		if (pid == -1) {
			perror("fork");
			remove(edname);
			goto out;
		}
		if (pid == 0) {
			if (sigint != SIG_IGN)
				signal(SIGINT, SIG_DFL);
			if (sigquit != SIG_IGN)
				signal(SIGQUIT, SIG_DFL);
			execl(ed, ed, edname, 0);
			perror(ed);
			_exit(1);
		}
		while (wait(&status) != pid)
			;

		/*
		 * If in read only mode, just remove the editor
		 * temporary and return.
		 */

		if (readonly) {
			remove(edname);
			continue;
		}

		/*
		 * Now copy the message to the end of the
		 * temp file.
		 */

		if (stat(edname, &statb) < 0) {
			perror(edname);
			goto out;
		}
		if (modtime == statb.st_mtime) {
			remove(edname);
			goto out;
		}
		if ((ibuf = fopen(edname, "r")) == NULL) {
			perror(edname);
			remove(edname);
			goto out;
		}
		remove(edname);
		fseek(otf, (long) 0, 2);
		size = ftell(otf);
		mp->m_block = blockof(size);
		mp->m_offset = offsetof(size);
		mp->m_size = fsize(ibuf);
		mp->m_lines = 0;
		while ((c = getc(ibuf)) != EOF) {
			if (c == '\n')
				mp->m_lines++;
			putc(c, otf);
			if (ferror(otf))
				break;
		}
		if (ferror(otf))
			perror("/tmp");
		fclose(ibuf);
	}

	/*
	 * Restore signals and return.
	 */

out:
	signal(SIGINT, sigint);
	signal(SIGQUIT, sigquit);
	return 0;
}
