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
static char sccsid[] = "@(#)edit.c	5.5 (Berkeley) 2/18/88";
#endif /* notdef */

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
	char edname[15];
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
		(void) sprintf(edname, "Message%d", mesg);
		if (!access(edname, 2)) {
			printf("%s: file exists\n", edname);
			goto out;
		}

		/*
		 * Copy the message into the edit file.
		 */
		(void) close(creat(edname, 0600));
		if ((obuf = fopen(edname, "w")) == NULL) {
			perror(edname);
			goto out;
		}
		if (send(mp, obuf, 0) < 0) {
			perror(edname);
			(void) fclose(obuf);
			(void) remove(edname);
			goto out;
		}
		(void) fflush(obuf);
		if (ferror(obuf)) {
			(void) remove(edname);
			(void) fclose(obuf);
			goto out;
		}
		(void) fclose(obuf);

		/*
		 * If we are in read only mode, make the
		 * temporary message file readonly as well.
		 */

		if (readonly)
			(void) chmod(edname, 0400);

		/*
		 * Fork/execl the editor on the edit file.
		 */

		if (stat(edname, &statb) < 0)
			modtime = 0;
		modtime = statb.st_mtime;
		pid = vfork();
		if (pid == -1) {
			perror("fork");
			(void) remove(edname);
			goto out;
		}
		if (pid == 0) {
			if (sigint != SIG_IGN)
				(void) signal(SIGINT, SIG_DFL);
			if (sigquit != SIG_IGN)
				(void) signal(SIGQUIT, SIG_DFL);
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
			(void) remove(edname);
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
			(void) remove(edname);
			goto out;
		}
		if ((ibuf = fopen(edname, "r")) == NULL) {
			perror(edname);
			(void) remove(edname);
			goto out;
		}
		(void) remove(edname);
		(void) fseek(otf, (long) 0, 2);
		size = ftell(otf);
		mp->m_block = blockof(size);
		mp->m_offset = offsetof(size);
		mp->m_size = fsize(ibuf);
		mp->m_lines = 0;
		while ((c = getc(ibuf)) != EOF) {
			if (c == '\n')
				mp->m_lines++;
			(void) putc(c, otf);
			if (ferror(otf))
				break;
		}
		if (ferror(otf))
			perror("/tmp");
		(void) fclose(ibuf);
	}

	/*
	 * Restore signals and return.
	 */

out:
	(void) signal(SIGINT, sigint);
	(void) signal(SIGQUIT, sigquit);
	return 0;
}
