/*-
 * Copyright (c) 1992, 1993
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
static char sccsid[] = "@(#)exf.c	8.2 (Berkeley) 6/17/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"
#include "pathnames.h"
#include "recover.h"

static int file_def __P((SCR *, EXF *));

/*
 * file_get --
 *	Return the appropriate structure if we've seen this file before,
 *	otherwise insert a new file into the list of files before or after
 *	the specified file.
 */
EXF *
file_get(sp, ep, name, append)
	SCR *sp;
	EXF *ep;
	char *name;
	int append;
{
	EXF *tep;

	/*
	 * Check for the file.  Ignore files without names, but check
	 * F_IGNORE files, in case of re-editing.  If the file is in
	 * play, just return.
	 */
	if (name != NULL)
		for (tep = sp->gp->exfhdr.next;
		    tep != (EXF *)&sp->gp->exfhdr; tep = tep->next)
			if (!strcmp(tep->name, name)) {
				if (tep->refcnt != 0)
					return (tep);
				break;
			}

	if (name == NULL || tep == (EXF *)&sp->gp->exfhdr) {
		/*
		 * If not found, build an entry for it.
		 * Allocate and initialize the file structure.
		 */
		if ((tep = malloc(sizeof(EXF))) == NULL)
			goto e1;
		if (file_def(sp, tep)) {
			FREE(tep, sizeof(EXF));
			goto e1;
		}

		/* Insert into the chain of files. */
		if (append) {
			HDR_APPEND(tep, ep, next, prev, EXF);
		} else {
			HDR_INSERT(tep, ep, next, prev, EXF);
		}

		/* Ignore all files, by default. */
		F_SET(tep, F_IGNORE);
	}

	if (name == NULL)
		tep->name = NULL;
	else {
		if ((tep->name = strdup(name)) == NULL)
			goto e2;
		tep->nlen = strlen(tep->name);
	}
	return (tep);

e2:	HDR_DELETE(tep, next, prev, EXF);
	free(tep);
e1:	msgq(sp, M_ERR, "Error: %s", strerror(errno));
	return (NULL);
}

/*
 * file_set --
 *	Append an argc/argv set of files to the file list.
 */
int
file_set(sp, argc, argv)
	SCR *sp;
	int argc;
	char *argv[];
{
	EXF *ep;

	for (; *argv; ++argv) {
		if ((ep =
		    file_get(sp, (EXF *)&sp->gp->exfhdr, *argv, 0)) == NULL)
			return (1);
		F_CLR(ep, F_IGNORE);
	}
	return (0);
}

/*
 * file_first --
 *	Return the first file.
 */
EXF *
file_first(sp, all)
	SCR *sp;
	int all;
{
	EXF *tep;

	for (tep = sp->gp->exfhdr.next;
	    tep != (EXF *)&sp->gp->exfhdr; tep = tep->next)
		if (all || !F_ISSET(tep, F_IGNORE))
			return (tep);
	return (NULL);
}

/*
 * file_next --
 *	Return the next file, if any.
 */
EXF *
file_next(sp, ep, all)
	SCR *sp;
	EXF *ep;
	int all;
{
	while ((ep = ep->next) != (EXF *)&sp->gp->exfhdr)
		if (all || !F_ISSET(ep, F_IGNORE))
			return (ep);
	return (NULL);
}

/*
 * file_prev --
 *	Return the previous file, if any.
 */
EXF *
file_prev(sp, ep, all)
	SCR *sp;
	EXF *ep;
	int all;
{
	while ((ep = ep->prev) != (EXF *)&sp->gp->exfhdr)
		if (all || !F_ISSET(ep, F_IGNORE))
			return (ep);
	return (NULL);
}

/*
 * file_start --
 *	Start editing a file.
 */
EXF *
file_start(sp, ep, rcv_fname)
	SCR *sp;
	EXF *ep;
	char *rcv_fname;
{
	RECNOINFO oinfo;
	struct stat sb;
	size_t psize;
	int fd, sverrno;
	char *oname, tname[sizeof(_PATH_TMPNAME) + 1];

	/* If not a specific file, create one. */
	if (ep == NULL &&
	    (ep = file_get(sp, (EXF *)&sp->gp->exfhdr, NULL, 1)) == NULL)
		return (NULL);

	/* If already in play, up the count and return. */
	if (ep->refcnt > 0) {
		++ep->refcnt;
		return (ep);
	}

	/*
	 * If no name or backing file, create a backing temporary file, saving
	 * the temp file name so can later unlink it.  Point the name at the
	 * temporary name (we display it to the user until they rename it).
	 */
	if (ep->name == NULL || stat(ep->name, &sb)) {
		(void)strcpy(tname, _PATH_TMPNAME);
		if ((fd = mkstemp(tname)) == -1) {
			msgq(sp, M_ERR,
			    "Temporary file: %s", strerror(errno));
			return (NULL);
		}
		(void)close(fd);
		if ((ep->tname = strdup(tname)) == NULL) {
			(void)unlink(tname);
			return (NULL);
		}

		if (ep->name == NULL) {
			F_SET(ep, F_NONAME);
			ep->name = ep->tname;
			ep->nlen = strlen(ep->name);
		}
		oname = ep->tname;
		psize = 4 * 1024;
	} else {
		oname = ep->name;

		/* Try to keep it at 10 pages or less per file. */
		if (sb.st_size < 40 * 1024)
			psize = 4 * 1024;
		else if (sb.st_size < 320 * 1024)
			psize = 32 * 1024;
		else
			psize = 64 * 1024;
	}
	
	/* Set up recovery. */
	memset(&oinfo, 0, sizeof(RECNOINFO));
	oinfo.bval = '\n';			/* Always set. */
	oinfo.psize = psize;
	oinfo.flags = F_ISSET(sp->gp, G_SNAPSHOT) ? R_SNAPSHOT : 0;
	if (rcv_fname == NULL) {
		if (rcv_tmp(sp, ep)) {
			oinfo.bfname = NULL;
			msgq(sp, M_ERR,
		    "Modifications not recoverable if the system crashes.");
		} else {
			F_SET(ep, F_RCV_ON);
			oinfo.bfname = ep->rcv_path;
		}
	} else if ((ep->rcv_path = strdup(rcv_fname)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (NULL);
	} else {
		oinfo.bfname = ep->rcv_path;
		F_SET(ep, F_MODIFIED);
	}

	/*
	 * Open a db structure.
	 *
	 * XXX
	 * We need to distinguish the case of a lock not being available
	 * from the file or file system simply doesn't support locking.
	 * We assume that EAGAIN is the former.  There really isn't a
	 * portable way to do this.
	 */
	ep->db = dbopen(oname,
	    O_EXLOCK | O_NONBLOCK| O_RDONLY, DEFFILEMODE, DB_RECNO, &oinfo);
	if (ep->db == NULL) {
		sverrno = errno;
		ep->db = dbopen(oname,
		    O_NONBLOCK | O_RDONLY, DEFFILEMODE, DB_RECNO, &oinfo);
		if (ep->db == NULL) {
			msgq(sp, M_ERR, "%s: %s", oname, strerror(errno));
			return (NULL);
		}
		if (sverrno == EAGAIN) {
			msgq(sp, M_INFO,
			    "%s already locked, session is read-only", oname);
			F_SET(ep, F_RDONLY);
		} else
			msgq(sp, M_VINFO, "%s cannot be locked", oname);
	}

	/*
	 * The -R flag, or doing a "set readonly" during a session causes all
	 * files edited during the session (using an edit command, or even
	 * using tags) to be marked read-only.  Note that changing the file
	 * name (see ex/ex_file.c) however, clears this flag.
	 */
	if (O_ISSET(sp, O_READONLY))
		F_SET(ep, F_RDONLY);

	/* Flush the line caches. */
	ep->c_lno = ep->c_nlines = OOBLNO;

	/* Start logging. */
	log_init(sp, ep);

	++ep->refcnt;
	return (ep);
}

/*
 * file_stop --
 *	Stop editing a file.
 */
int
file_stop(sp, ep, force)
	SCR *sp;
	EXF *ep;
	int force;
{
	if (--ep->refcnt != 0)
		return (0);

	/* Close the db structure. */
	if ((ep->db->close)(ep->db) && !force) {
		msgq(sp, M_ERR, "%s: close: %s", ep->name, strerror(errno));
		return (1);
	}

	/* Delete the recovery file. */
	if (!F_ISSET(ep, F_RCV_NORM))
		(void)unlink(ep->rcv_path);
	if (ep->rcv_path != NULL)
		FREE(ep->rcv_path, strlen(ep->rcv_path));

	/*
	 * Committed to the close.
	 *
	 * Stop logging.
	 */
	(void)log_end(sp, ep);

	/* Unlink any temporary file. */
	if (ep->tname != NULL) {
		if (unlink(ep->tname))
			msgq(sp, M_ERR,
			    "%s: remove: %s", ep->tname, strerror(errno));
		free(ep->tname);
		ep->tname = NULL;
	}

	/* Clean up the flags. */
	F_CLR(ep, F_CLOSECLR);
	return (0);
}

/*
 * file_write --
 *	Write the file to disk.  Historic vi had fairly convoluted
 *	semantics for whether or not writes would happen.  That's
 *	why all the flags.
 */
int
file_write(sp, ep, fm, tm, fname, flags)
	SCR *sp;
	EXF *ep;
	MARK *fm, *tm;
	char *fname;
	int flags;
{
	struct stat sb;
	FILE *fp;
	MARK from, to;
	int fd, oflags;

	/*
	 * Don't permit writing to temporary files.  The problem is that
	 * if it's a temp file, and the user does ":wq", we write and quit,
	 * unlinking the temporary file.  Not what the user had in mind
	 * at all.  This test cannot be forced.
	 */
	if (fname == NULL && F_ISSET(ep, F_NONAME)) {
		msgq(sp, M_ERR, "No filename to which to write.");
		return (1);
	}

	/* Can't write read-only files, unless forced. */
	if (fname == NULL && !LF_ISSET(FS_FORCE) && F_ISSET(ep, F_RDONLY)) {
		if (LF_ISSET(FS_POSSIBLE))
			msgq(sp, M_ERR,
			    "Read-only file, not written; use ! to override.");
		else
			msgq(sp, M_ERR,
			    "Read-only file, not written.");
		return (1);
	}

	/*
	 * If the name was changed, or we're writing to a new file, don't
	 * overwrite anything unless forced, the "writeany" option is set,
	 * or appending.
	 */
	if (!LF_ISSET(FS_FORCE | FS_APPEND) && !O_ISSET(sp, O_WRITEANY) &&
	    (fname != NULL && !stat(fname, &sb) ||
	    F_ISSET(ep, F_NAMECHANGED) && !stat(ep->name, &sb))) {
		if (fname == NULL)
			fname = ep->name;
		if (LF_ISSET(FS_POSSIBLE))
			msgq(sp, M_ERR,
			    "%s exists, not written; use ! to override.",
			    fname);
		else
			msgq(sp, M_ERR, "%s exists, not written.", fname);
		return (1);
	}

	if (fname == NULL)
		fname = ep->name;

	/* Don't do partial writes, unless forced. */
	if (!LF_ISSET(FS_ALL | FS_FORCE) && !stat(fname, &sb)) {
		if (LF_ISSET(FS_POSSIBLE))
			msgq(sp, M_ERR, "Use ! to write a partial file.");
		else
			msgq(sp, M_ERR, "Partial file, not written.");
		return (1);
	}

	/*
	 * Once we've decided that we can actually write the file,
	 * it doesn't matter that the file name was changed -- if
	 * it was, we created the file.
	 */
	F_CLR(ep, F_NAMECHANGED);

	/* Open the file, either appending or truncating. */
	oflags = O_CREAT | O_WRONLY;
	if (LF_ISSET(FS_APPEND))
		oflags |= O_APPEND;
	else
		oflags |= O_TRUNC;
	if ((fd = open(fname, oflags, DEFFILEMODE)) < 0) {
		msgq(sp, M_ERR, "%s: %s", fname, strerror(errno));
		return (1);
	}

	/* Use stdio for buffering. */
	if ((fp = fdopen(fd, "w")) == NULL) {
		(void)close(fd);
		msgq(sp, M_ERR, "%s: %s", fname, strerror(errno));
		return (1);
	}

	/* Build fake addresses, if necessary. */
	if (fm == NULL) {
		from.lno = 1;
		from.cno = 0;
		fm = &from;
		if (file_lline(sp, ep, &to.lno))
			return (1);
		to.cno = 0;
		tm = &to;
	}

	/* Write the file. */
	if (ex_writefp(sp, ep, fname, fp, fm, tm, 1))
		return (1);

	/* If wrote the entire file, clear the modified bit. */
	if (LF_ISSET(FS_ALL))
		F_CLR(ep, F_MODIFIED);

	return (0);
}

/*
 * file_def --
 *	Fill in a default EXF structure.
 */
static int
file_def(sp, ep)
	SCR *sp;
	EXF *ep;
{
	memset(ep, 0, sizeof(EXF));

	ep->c_lno = OOBLNO;
	F_SET(ep, F_FIRSTMODIFY | F_NOSETPOS);

	return (mark_init(sp, ep));
}
