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
static char sccsid[] = "@(#)quit.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include "rcv.h"
#include <sys/stat.h>
#include <sys/file.h>

/*
 * Rcv -- receive mail rationally.
 *
 * Termination processing.
 */

/*
 * The "quit" command.
 */
quitcmd()
{
	/*
	 * If we are sourcing, then return 1 so execute() can handle it.
	 * Otherwise, return -1 to abort command loop.
	 */
	if (sourcing)
		return 1;
	return -1;
}

/*
 * Save all of the undetermined messages at the top of "mbox"
 * Save all untouched messages back in the system mailbox.
 * Remove the system mailbox, if none saved there.
 */

quit()
{
	int mcount, p, modify, autohold, anystat, holdbit, nohold;
	FILE *ibuf, *obuf, *fbuf, *rbuf, *readstat, *abuf;
	register struct message *mp;
	register int c;
	extern char tempQuit[], tempResid[];
	struct stat minfo;
	char *mbox;

	/*
	 * If we are read only, we can't do anything,
	 * so just return quickly.
	 */
	if (readonly)
		return;
	/*
	 * If editing (not reading system mail box), then do the work
	 * in edstop()
	 */
	if (edit) {
		edstop();
		return;
	}

	/*
	 * See if there any messages to save in mbox.  If no, we
	 * can save copying mbox to /tmp and back.
	 *
	 * Check also to see if any files need to be preserved.
	 * Delete all untouched messages to keep them out of mbox.
	 * If all the messages are to be preserved, just exit with
	 * a message.
	 */

	fbuf = fopen(mailname, "r");
	if (fbuf == NULL)
		goto newmail;
	flock(fileno(fbuf), LOCK_EX);
	rbuf = NULL;
	if (fstat(fileno(fbuf), &minfo) >= 0 && minfo.st_size > mailsize) {
		printf("New mail has arrived.\n");
		rbuf = fopen(tempResid, "w");
		if (rbuf == NULL || fbuf == NULL)
			goto newmail;
#ifdef APPEND
		fseek(fbuf, mailsize, 0);
		while ((c = getc(fbuf)) != EOF)
			(void) putc(c, rbuf);
#else
		p = minfo.st_size - mailsize;
		while (p-- > 0) {
			c = getc(fbuf);
			if (c == EOF)
				goto newmail;
			(void) putc(c, rbuf);
		}
#endif
		fclose(rbuf);
		if ((rbuf = fopen(tempResid, "r")) == NULL)
			goto newmail;
		remove(tempResid);
	}

	/*
	 * Adjust the message flags in each message.
	 */

	anystat = 0;
	autohold = value("hold") != NOSTR;
	holdbit = autohold ? MPRESERVE : MBOX;
	nohold = MBOX|MSAVED|MDELETED|MPRESERVE;
	if (value("keepsave") != NOSTR)
		nohold &= ~MSAVED;
	for (mp = &message[0]; mp < &message[msgCount]; mp++) {
		if (mp->m_flag & MNEW) {
			mp->m_flag &= ~MNEW;
			mp->m_flag |= MSTATUS;
		}
		if (mp->m_flag & MSTATUS)
			anystat++;
		if ((mp->m_flag & MTOUCH) == 0)
			mp->m_flag |= MPRESERVE;
		if ((mp->m_flag & nohold) == 0)
			mp->m_flag |= holdbit;
	}
	modify = 0;
	if (Tflag != NOSTR) {
		if ((readstat = fopen(Tflag, "w")) == NULL)
			Tflag = NOSTR;
	}
	for (c = 0, p = 0, mp = &message[0]; mp < &message[msgCount]; mp++) {
		if (mp->m_flag & MBOX)
			c++;
		if (mp->m_flag & MPRESERVE)
			p++;
		if (mp->m_flag & MODIFY)
			modify++;
		if (Tflag != NOSTR && (mp->m_flag & (MREAD|MDELETED)) != 0) {
			char *id;

			if ((id = hfield("article-id", mp)) != NOSTR)
				fprintf(readstat, "%s\n", id);
		}
	}
	if (Tflag != NOSTR)
		fclose(readstat);
	if (p == msgCount && !modify && !anystat) {
		printf("Held %d message%s in %s\n",
			p, p == 1 ? "" : "s", mailname);
		fclose(fbuf);
		return;
	}
	if (c == 0) {
		if (p != 0) {
			writeback(rbuf);
			fclose(fbuf);
			return;
		}
		goto cream;
	}

	/*
	 * Create another temporary file and copy user's mbox file
	 * darin.  If there is no mbox, copy nothing.
	 * If he has specified "append" don't copy his mailbox,
	 * just copy saveable entries at the end.
	 */

	mbox = expand("&");
	mcount = c;
	if (value("append") == NOSTR) {
		if ((obuf = fopen(tempQuit, "w")) == NULL) {
			perror(tempQuit);
			fclose(fbuf);
			return;
		}
		if ((ibuf = fopen(tempQuit, "r")) == NULL) {
			perror(tempQuit);
			remove(tempQuit);
			fclose(obuf);
			fclose(fbuf);
			return;
		}
		remove(tempQuit);
		if ((abuf = fopen(mbox, "r")) != NULL) {
			while ((c = getc(abuf)) != EOF)
				(void) putc(c, obuf);
			fclose(abuf);
		}
		if (ferror(obuf)) {
			perror(tempQuit);
			fclose(ibuf);
			fclose(obuf);
			fclose(fbuf);
			return;
		}
		fclose(obuf);
		close(creat(mbox, 0600));
		if ((obuf = fopen(mbox, "r+")) == NULL) {
			perror(mbox);
			fclose(ibuf);
			fclose(fbuf);
			return;
		}
	}
	if (value("append") != NOSTR) {
		if ((obuf = fopen(mbox, "a")) == NULL) {
			perror(mbox);
			fclose(fbuf);
			return;
		}
		fchmod(fileno(obuf), 0600);
	}
	for (mp = &message[0]; mp < &message[msgCount]; mp++)
		if (mp->m_flag & MBOX)
			if (send(mp, obuf, saveignore, NOSTR) < 0) {
				perror(mbox);
				fclose(ibuf);
				fclose(obuf);
				fclose(fbuf);
				return;
			}

	/*
	 * Copy the user's old mbox contents back
	 * to the end of the stuff we just saved.
	 * If we are appending, this is unnecessary.
	 */

	if (value("append") == NOSTR) {
		rewind(ibuf);
		c = getc(ibuf);
		while (c != EOF) {
			(void) putc(c, obuf);
			if (ferror(obuf))
				break;
			c = getc(ibuf);
		}
		fclose(ibuf);
		fflush(obuf);
	}
	trunc(obuf);
	if (ferror(obuf)) {
		perror(mbox);
		fclose(obuf);
		fclose(fbuf);
		return;
	}
	fclose(obuf);
	if (mcount == 1)
		printf("Saved 1 message in mbox\n");
	else
		printf("Saved %d messages in mbox\n", mcount);

	/*
	 * Now we are ready to copy back preserved files to
	 * the system mailbox, if any were requested.
	 */

	if (p != 0) {
		writeback(rbuf);
		fclose(fbuf);
		return;
	}

	/*
	 * Finally, remove his /usr/mail file.
	 * If new mail has arrived, copy it back.
	 */

cream:
	if (rbuf != NULL) {
		abuf = fopen(mailname, "r+");
		if (abuf == NULL)
			goto newmail;
		while ((c = getc(rbuf)) != EOF)
			(void) putc(c, abuf);
		fclose(rbuf);
		trunc(abuf);
		fclose(abuf);
		alter(mailname);
		fclose(fbuf);
		return;
	}
	demail();
	fclose(fbuf);
	return;

newmail:
	printf("Thou hast new mail.\n");
	if (fbuf != NULL)
		fclose(fbuf);
}

/*
 * Preserve all the appropriate messages back in the system
 * mailbox, and print a nice message indicated how many were
 * saved.  On any error, just return -1.  Else return 0.
 * Incorporate the any new mail that we found.
 */
writeback(res)
	register FILE *res;
{
	register struct message *mp;
	register int p, c;
	FILE *obuf;

	p = 0;
	if ((obuf = fopen(mailname, "r+")) == NULL) {
		perror(mailname);
		return(-1);
	}
#ifndef APPEND
	if (res != NULL)
		while ((c = getc(res)) != EOF)
			(void) putc(c, obuf);
#endif
	for (mp = &message[0]; mp < &message[msgCount]; mp++)
		if ((mp->m_flag&MPRESERVE)||(mp->m_flag&MTOUCH)==0) {
			p++;
			if (send(mp, obuf, (struct ignoretab *)0, NOSTR) < 0) {
				perror(mailname);
				fclose(obuf);
				return(-1);
			}
		}
#ifdef APPEND
	if (res != NULL)
		while ((c = getc(res)) != EOF)
			(void) putc(c, obuf);
#endif
	fflush(obuf);
	trunc(obuf);
	if (ferror(obuf)) {
		perror(mailname);
		fclose(obuf);
		return(-1);
	}
	if (res != NULL)
		fclose(res);
	fclose(obuf);
	alter(mailname);
	if (p == 1)
		printf("Held 1 message in %s\n", mailname);
	else
		printf("Held %d messages in %s\n", p, mailname);
	return(0);
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
	char tempname[30];
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
			char *id;

			if ((id = hfield("article-id", mp)) != NOSTR)
				fprintf(readstat, "%s\n", id);
		}
	}
	if (Tflag != NOSTR)
		fclose(readstat);
	if (!gotcha || Tflag != NOSTR)
		goto done;
	ibuf = NULL;
	if (stat(mailname, &statb) >= 0 && statb.st_size > mailsize) {
		strcpy(tempname, "/tmp/mboxXXXXXX");
		mktemp(tempname);
		if ((obuf = fopen(tempname, "w")) == NULL) {
			perror(tempname);
			relsesigs();
			reset(0);
		}
		if ((ibuf = fopen(mailname, "r")) == NULL) {
			perror(mailname);
			fclose(obuf);
			remove(tempname);
			relsesigs();
			reset(0);
		}
		fseek(ibuf, mailsize, 0);
		while ((c = getc(ibuf)) != EOF)
			(void) putc(c, obuf);
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
	printf("\"%s\" ", mailname);
	fflush(stdout);
	if ((obuf = fopen(mailname, "r+")) == NULL) {
		perror(mailname);
		relsesigs();
		reset(0);
	}
	trunc(obuf);
	c = 0;
	for (mp = &message[0]; mp < &message[msgCount]; mp++) {
		if ((mp->m_flag & MDELETED) != 0)
			continue;
		c++;
		if (send(mp, obuf, (struct ignoretab *) NULL, NOSTR) < 0) {
			perror(mailname);
			relsesigs();
			reset(0);
		}
	}
	gotcha = (c == 0 && ibuf == NULL);
	if (ibuf != NULL) {
		while ((c = getc(ibuf)) != EOF)
			(void) putc(c, obuf);
		fclose(ibuf);
	}
	fflush(obuf);
	if (ferror(obuf)) {
		perror(mailname);
		relsesigs();
		reset(0);
	}
	fclose(obuf);
	if (gotcha) {
		remove(mailname);
		printf("removed\n");
	} else
		printf("complete\n");
	fflush(stdout);

done:
	relsesigs();
}
