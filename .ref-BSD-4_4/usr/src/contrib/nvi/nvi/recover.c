/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)recover.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "pathnames.h"
#include "recover.h"

/*
 * Recovery code.
 *
 * The basic scheme is there's a btree file, whose name we specify.  The first
 * time a file is modified, and then at RCV_PERIOD intervals after that, the
 * btree file is synced to disk.  Each time a keystroke is requested for a file
 * the terminal routines check to see if the file needs to be synced.  This, of
 * course means that the data structures had better be consistent each time the
 * key routines are called.
 *
 * We don't use timers other than to flag that the file should be synced.  All
 * of the SCR and HDR data structures would have to be locked, the database
 * routines would have to lock out the timers for each update, etc.  It's just
 * not worth it.  The only way we can lose in the current scheme is if the file
 * is saved, the user types furiously for RCV_PERIOD - 1 seconds, and types
 * nothing more.  Not too likely.
 */

/*
 * rcv_tmp --
 *	Build a file name that will be used as the recovery file.
 */
int
rcv_tmp(sp, ep)
	SCR *sp;
	EXF *ep;
{
	sigset_t set, oset;
	int fd;
	char *p, path[MAXPATHLEN];

	if ((p = strrchr(ep->name, '/')) == NULL)
		p = ep->name;
	else
		++p;
	(void)snprintf(path, sizeof(path),
	    "%s/vi.%s.XXXXXX", _PATH_PRESERVE, p);

	(void)sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);
	fd = mkstemp(path);
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);

	if (fd == -1) {
		msgq(sp, M_ERR,
		    "Error: %s: %s", _PATH_PRESERVE, strerror(errno));
		return (1);
	}
	if ((ep->rcv_path = strdup(path)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	return (0);
}

/*
 * rcv_init --
 *	Force the file to be snapshotted for recovery.
 */
int
rcv_init(sp, ep)
	SCR *sp;
	EXF *ep;
{
	struct itimerval value;
	recno_t lno;

	F_CLR(ep, F_FIRSTMODIFY | F_RCV_ON);

	/* Force read of entire file. */
	if (file_lline(sp, ep, &lno))
		goto err;

	(void)sp->s_busy_cursor(sp,
	    lno > 500 ? "Copying file for recovery..." : NULL);

	/* Sync it to backing store. */
	if (ep->db->sync(ep->db, R_RECNOSYNC)) {
		msgq(sp, M_ERR, "Preservation failed: %s: %s",
		    ep->rcv_path, strerror(errno));
		goto err;
	}

	if (!F_ISSET(sp->gp, G_RECOVER_SET)) {
		/* Start the recovery timer. */
		(void)signal(SIGALRM, SIG_IGN);
		value.it_interval.tv_sec = value.it_interval.tv_usec = 0;
		value.it_value.tv_sec = RCV_PERIOD;
		value.it_value.tv_usec = 0;
		if (setitimer(ITIMER_REAL, &value, NULL)) {
			msgq(sp, M_ERR,
			    "Error: setitimer: %s", strerror(errno));
			goto err;
		}
		(void)signal(SIGALRM, rcv_alrm);

		/* Initialize the about-to-die handler. */
		(void)signal(SIGHUP, rcv_hup);
	}

	F_SET(ep, F_RCV_ON);
	return (0);

err:	msgq(sp, M_ERR, "Recovery after system crash not possible.");
	return (1);
}

/*
 * rcv_end --
 *	Turn off the timer, handlers.
 */
void
rcv_end()
{
	struct itimerval value;

	(void)signal(SIGALRM, SIG_IGN);
	(void)signal(SIGHUP, SIG_IGN);

	value.it_interval.tv_sec = value.it_interval.tv_usec = 0;
	value.it_value.tv_sec = value.it_value.tv_usec = 0;
	(void)setitimer(ITIMER_REAL, &value, NULL);
}

/*
 * rcv_sync --
 *	Sync the backing file.
 */
int
rcv_sync(sp, ep)
	SCR *sp;
	EXF *ep;
{
	if (ep->db->sync(ep->db, R_RECNOSYNC)) {
		msgq(sp, M_ERR, "Preservation failed: %s: %s",
		    ep->rcv_path, strerror(errno));
		F_CLR(ep, F_RCV_ON);
		return (1);
	}
	return (0);
}

/*
 * rcv_alrm --
 *	Recovery timer interrupt handler.
 *
 *	The only race should be with linking and unlinking the SCR
 *	chain, and using the underlying EXF * from the SCR structure.
 */
void
rcv_alrm(signo)
	int signo;
{
	SCR *sp;

	/* Walk the list of screens, noting that the file should be synced. */
	for (sp = __global_list->scrhdr.next;
	    sp != (SCR *)&__global_list->scrhdr; sp = sp->next)
		if (sp->ep != NULL && F_ISSET(sp->ep, F_RCV_ON))
			F_SET(sp->ep, F_RCV_ALRM);
}

/*
 * rcv_hup --
 *	Recovery death interrupt handler.
 *
 *	The only race should be with linking and unlinking the SCR
 *	chain, and using the underlying EXF * from the SCR structure.
 *
 *	DON'T USE MSG ROUTINES, THEY'RE NOT PROTECTED AGAINST US!
 */
void
rcv_hup(signo)
	int signo;
{
	SCR *sp;

	/* Walk the list of screens, sync'ing the files. */
	for (sp = __global_list->scrhdr.next;
	    sp != (SCR *)&__global_list->scrhdr; sp = sp->next)
		if (sp->ep != NULL)
			if (F_ISSET(sp->ep, F_RCV_ON))
				(void)sp->ep->db->sync(sp->ep->db, R_RECNOSYNC);
			else
				(void)unlink(sp->ep->rcv_path);

	/* Die with the proper exit status. */
	(void)signal(SIGHUP, SIG_DFL);
	(void)kill(0, SIGHUP);

	/* NOTREACHED */
	exit (1);
}

/*
 * rcv_list --
 *	List the files that can be recovered by this user.
 */
int
rcv_list()
{
	struct dirent *dp;
	struct stat sb;
	DIR *dirp;
	uid_t myid;
	int found;
	char *p;

	if (chdir(_PATH_PRESERVE) || (dirp = opendir(".")) == NULL) {
		(void)fprintf(stderr,
		    "vi: %s: %s\n", _PATH_PRESERVE, strerror(errno));
		return (1);
	}

	myid = getuid();

	found = 0;
	while ((dp = readdir(dirp)) != NULL) {
		if (dp->d_namlen <= 3 || strncmp(dp->d_name, "vi.", 3))
			continue;
		if (stat(dp->d_name, &sb)) {
			(void)fprintf(stderr,
			    "vi: %s: %s\n", dp->d_name, strerror(errno));
			continue;
		}
		if ((p = strrchr(dp->d_name + 3, '.')) == NULL)
			continue;
		if (myid != 0 && myid != sb.st_uid)
			continue;
		*p = '\0';

		found = 1;
		(void)printf("%s: %s", dp->d_name + 3, ctime(&sb.st_mtime));
	}
	if (found == 0)
		(void)printf("vi: no files to recover.\n");
	(void)closedir(dirp);
	return (0);
}

/*
 * rcv_read --
 *	Start the recovered file as the file to edit.
 */
EXF *
rcv_read(sp, name)
	SCR *sp;
	char *name;
{
	struct dirent *dp;
	struct stat sb;
	DIR *dirp;
	uid_t myid;
	int found;
	char *p, path[MAXPATHLEN];

	if ((dirp = opendir(_PATH_PRESERVE)) == NULL) {
		msgq(sp, M_ERR, "%s: %s", _PATH_PRESERVE, strerror(errno));
		return (NULL);
	}

	myid = getuid();

	found = 0;
	while ((dp = readdir(dirp)) != NULL) {
		if (dp->d_namlen <= 3 || strncmp(dp->d_name, "vi.", 3))
			continue;
		if ((p = strrchr(dp->d_name + 3, '.')) == NULL)
			continue;
		(void)snprintf(path, sizeof(path),
		    "%s/%s", _PATH_PRESERVE, dp->d_name);
		if (stat(path, &sb)) {
			msgq(sp, M_ERR,
			    "%s: %s", dp->d_name, strerror(errno));
			continue;
		}
		if (myid != 0 && myid != sb.st_uid)
			continue;
		*p = '\0';
		if (strcmp(dp->d_name + 3, name))
			continue;

		*p = '.';
		found = 1;
		break;
	}
	(void)closedir(dirp);
	if (!found) {
		msgq(sp, M_INFO,
		    "No files named %s, owned by you, to edit.", name);
		return (NULL);
	}
	return (file_start(sp, NULL, path));
}
