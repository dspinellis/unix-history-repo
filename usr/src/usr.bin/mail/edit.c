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
static char sccsid[] = "@(#)edit.c	5.6 (Berkeley) %G%";
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
	int i, pid;
	int (*sigint)(), (*sigquit)();
	FILE *fp;
	extern char tempEdit[];
	char *edname = tempEdit;
	register struct message *mp;
	off_t fsize(), size;
	struct stat statb;
	time_t modtime;
	union wait status;

	/*
	 * Set signals; locate editor.
	 */
	sigint = signal(SIGINT, SIG_IGN);
	sigquit = signal(SIGQUIT, SIG_IGN);
	/*
	 * Deal with each message to be edited . . .
	 */
	for (i = 0; msgvec[i] && i < msgCount; i++) {
		mp = &message[msgvec[i] - 1];
		mp->m_flag |= MODIFY;
		touch(msgvec[i]);
		dot = mp;
		/*
		 * Copy the message into the edit file.
		 * If we are in read only mode, make the
		 * temporary message file readonly as well.
		 */
		if ((c = creat(edname, readonly ? 0400 : 0600)) < 0) {
			perror(edname);
			goto out;
		}
		if ((fp = fdopen(c, "w")) == NULL) {
			perror(edname);
			(void) unlink(edname);
			goto out;
		}
		if (send(mp, fp, 0) < 0) {
			perror(edname);
			(void) fclose(fp);
			(void) unlink(edname);
			goto out;
		}
		(void) fflush(fp);
		if (ferror(fp)) {
			(void) unlink(edname);
			(void) fclose(fp);
			goto out;
		}
		if (fstat(fileno(fp), &statb) < 0)
			modtime = 0;
		else
			modtime = statb.st_mtime;
		(void) fclose(fp);
		/*
		 * Fork/execl the editor on the edit file.
		 */
		pid = vfork();
		if (pid == -1) {
			perror("fork");
			(void) unlink(edname);
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
			(void) unlink(edname);
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
			(void) unlink(edname);
			goto out;
		}
		if ((fp = fopen(edname, "r")) == NULL) {
			perror(edname);
			(void) unlink(edname);
			goto out;
		}
		(void) unlink(edname);
		(void) fseek(otf, (long) 0, 2);
		size = ftell(otf);
		mp->m_block = blockof(size);
		mp->m_offset = offsetof(size);
		mp->m_size = fsize(fp);
		mp->m_lines = 0;
		while ((c = getc(fp)) != EOF) {
			if (c == '\n')
				mp->m_lines++;
			(void) putc(c, otf);
			if (ferror(otf))
				break;
		}
		if (ferror(otf))
			perror("/tmp");
		(void) fclose(fp);
	}
	/*
	 * Restore signals and return.
	 */
out:
	(void) signal(SIGINT, sigint);
	(void) signal(SIGQUIT, sigquit);
	return 0;
}
