/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fts.c	5.19 (Berkeley) 5/9/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include "fts.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static FTSENT *fts_alloc(), *fts_build(), *fts_sort();
static void fts_load(), fts_lfree();
static u_short fts_stat();
static char *fts_path();

#define	ISSET(opt)	(sp->fts_options & opt)
#define	SET(opt)	(sp->fts_options |= opt)

#define	CHDIR(sp, path)	(!ISSET(FTS_NOCHDIR) && chdir(path))
#define	FCHDIR(sp, fd)	(!ISSET(FTS_NOCHDIR) && fchdir(fd))

/* fts_build flags */
#define	BCHILD		1		/* from fts_children */
#define	BREAD		2		/* from fts_read */

FTS *
fts_open(argv, options, compar)
	char * const *argv;
	register int options;
	int (*compar)();
{
	register FTS *sp;
	register FTSENT *p, *root;
	register int nitems, maxlen;
	FTSENT *parent, *tmp;
	int len;

	/* Allocate/initialize the stream */
	if (!(sp = (FTS *)malloc((u_int)sizeof(FTS))))
		return(NULL);
	bzero(sp, sizeof(FTS));
	sp->fts_compar = compar;
	sp->fts_options = options;

	/* Logical walks turn on NOCHDIR; symbolic links are too hard. */
	if (ISSET(FTS_LOGICAL))
		SET(FTS_NOCHDIR);

	/* Allocate/initialize root's parent. */
	if (!(parent = fts_alloc(sp, "", 0)))
		goto mem1;
	parent->fts_level = FTS_ROOTPARENTLEVEL;

	/* Allocate/initialize root(s). */
	maxlen = -1;
	for (root = NULL, nitems = 0; *argv; ++argv, ++nitems) {
		if (!(len = strlen(*argv))) {
			errno = ENOENT;
			goto mem2;
		}
		if (maxlen < len)
			maxlen = len;
		p = fts_alloc(sp, *argv, len);
		p->fts_level = FTS_ROOTLEVEL;
		p->fts_parent = parent;
		/*
		 * If comparison routine supplied, traverse in sorted
		 * order; otherwise traverse in the order specified.
		 */
		if (compar) {
			p->fts_link = root;
			root = p;
			p->fts_accpath = p->fts_name;
			if (!(options & FTS_NOSTAT))
				p->fts_info = fts_stat(sp, p, 0);
		} else {
			p->fts_link = NULL;
			if (!root)
				tmp = root = p;
			else {
				tmp->fts_link = p;
				tmp = p;
			}
		}
	}
	if (compar && nitems > 1)
		root = fts_sort(sp, root, nitems);

	/*
	 * Allocate a dummy pointer and make fts_read think that we've just
	 * finished the node before the root(s); set p->fts_info to FTS_NS
	 * so that everything about the "current" node is ignored.
	 */
	if (!(sp->fts_cur = fts_alloc(sp, "", 0)))
		goto mem2;
	sp->fts_cur->fts_link = root;
	sp->fts_cur->fts_info = FTS_NS;

	/* Start out with at least 1K+ of path space. */
	if (!fts_path(sp, MAX(maxlen, MAXPATHLEN)))
		goto mem3;

	/*
	 * If using chdir(2), grab a file descriptor pointing to dot to insure
	 * that we can get back here; this could be avoided for some paths,
	 * but almost certainly not worth the effort.  Slashes, symbolic links,
	 * and ".." are all fairly nasty problems.  Note, if we can't get the
	 * descriptor we run anyway, just more slowly.
	 */
	if (!ISSET(FTS_NOCHDIR) && (sp->fts_rfd = open(".", O_RDONLY, 0)) < 0)
		SET(FTS_NOCHDIR);

	return(sp);

mem3:	free(sp->fts_cur);
mem2:	fts_lfree(root);
	free(parent);
mem1:	free(sp);
	return(NULL);
}

static void
fts_load(sp, p)
	FTS *sp;
	register FTSENT *p;
{
	register int len;
	register char *cp;

	/*
	 * Load the stream structure for the next traversal.  Since we don't
	 * actually enter the directory until after the preorder visit, set
	 * the fts_accpath field specially so the chdir gets done to the right
	 * place and the user can access the first node.
	 */
	len = p->fts_pathlen = p->fts_namelen;
	bcopy(p->fts_name, sp->fts_path, len + 1);
	if ((cp = rindex(p->fts_name, '/')) && (cp != p->fts_name || cp[1])) {
		len = strlen(++cp);
		bcopy(cp, p->fts_name, len + 1);
		p->fts_namelen = len;
	}
	p->fts_accpath = p->fts_path = sp->fts_path;

	p->fts_info = fts_stat(sp, p, 0);
	sp->rdev = p->fts_statb.st_dev;
}

fts_close(sp)
	FTS *sp;
{
	register FTSENT *freep, *p;
	int saved_errno;

	if (sp->fts_cur) {
		/*
		 * This still works if we haven't read anything -- the dummy
		 * structure points to the root list, so we step through to
		 * the end of the root list which has a valid parent pointer.
		 */
		for (p = sp->fts_cur; p->fts_level > FTS_ROOTPARENTLEVEL;) {
			freep = p;
			p = p->fts_link ? p->fts_link : p->fts_parent;
			free(freep);
		}
		free(p);
	}

	/* Free up child linked list, sort array, path buffer. */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);
	if (sp->fts_array)
		free(sp->fts_array);
	free(sp->fts_path);

	/* Return to original directory, save errno if necessary. */
	if (!ISSET(FTS_NOCHDIR)) {
		saved_errno = fchdir(sp->fts_rfd) ? errno : 0;
		(void)close(sp->fts_rfd);
	}

	/* Free up the stream pointer. */
	free(sp);

	/* Set errno and return. */
	if (!ISSET(FTS_NOCHDIR) && saved_errno) {
		errno = saved_errno;
		return(-1);
	}
	return(0);
}

/*
 * Special case a root of "/" so that slashes aren't appended causing
 * paths to be written as "//foo".
 */
#define	NAPPEND(p) \
	(p->fts_level == FTS_ROOTLEVEL && p->fts_pathlen == 1 && \
	    p->fts_path[0] == '/' ? 0 : p->fts_pathlen)

FTSENT *
fts_read(sp)
	register FTS *sp;
{
	register FTSENT *p, *tmp;
	register int instr;
	register char *t;

	/* If finished or unrecoverable error, return NULL. */
	if (!sp->fts_cur || ISSET(FTS_STOP))
		return(NULL);

	/* Set current node pointer. */
	p = sp->fts_cur;

	/* Save and zero out user instructions. */
	instr = p->fts_instr;
	p->fts_instr = FTS_NOINSTR;

	/* If used fts_link pointer for cycle detection, restore it. */
	if (sp->fts_savelink) {
		p->fts_link = sp->fts_savelink;
		sp->fts_savelink = NULL;
	}

	/* Any type of file may be re-visited; re-stat and re-turn. */
	if (instr == FTS_AGAIN) {
		p->fts_info = fts_stat(sp, p, 0);
		return(p);
	}

	/*
	 * Following a symlink -- SLNONE test allows application to see
	 * SLNONE and recover.
	 */
	if (instr == FTS_FOLLOW &&
	    (p->fts_info == FTS_SL || p->fts_info == FTS_SLNONE)) {
		p->fts_info = fts_stat(sp, p, 1);
		return(p);
	}

	/* Directory in pre-order. */
	if (p->fts_info == FTS_D) {
		/* If skipped or crossed mount point, do post-order visit. */
		if (instr == FTS_SKIP ||
		    ISSET(FTS_XDEV) && p->fts_statb.st_dev != sp->rdev) {
			if (sp->fts_child) {
				fts_lfree(sp->fts_child);
				sp->fts_child = NULL;
			}
			p->fts_info = FTS_DP;
			return(p);
		} 

		/*
		 * Cd to the subdirectory, reading it if haven't already.  If
		 * the read fails for any reason, or the directory is empty,
		 * the fts_info field of the current node is set by fts_build.
		 * If have already read and now fail to chdir, whack the list
		 * to make the names come out right, and set the parent state
		 * so the application will eventually get an error condition.
		 * If haven't read and fail to chdir, check to see if we're
		 * at the root node -- if so, we have to get back or the root
		 * node may be inaccessible.
		 */
		if (sp->fts_child) {
			if (CHDIR(sp, p->fts_accpath)) {
				p->fts_parent->fts_cderr = errno;
				for (p = sp->fts_child; p; p = p->fts_link)
					p->fts_accpath =
					    p->fts_parent->fts_accpath;
			}
		} else if (!(sp->fts_child = fts_build(sp, BREAD))) {
			if ISSET(FTS_STOP)
				return(NULL);
			if (p->fts_level == FTS_ROOTLEVEL &&
			    FCHDIR(sp, sp->fts_rfd)) {
				SET(FTS_STOP);
				return(NULL);
			}
			return(p);
		}
		p = sp->fts_child;
		sp->fts_child = NULL;
		goto name;
	}

	/* Move to next node on this level. */
next:	tmp = p;
	if (p = p->fts_link) {
		free(tmp);

		/* If reached the top, load the paths for the next root. */
		if (p->fts_level == FTS_ROOTLEVEL) {
			fts_load(sp, p);
			return(sp->fts_cur = p);
		}

		/* User may have called fts_set on the node. */
		if (p->fts_instr == FTS_SKIP)
			goto next;
		if (p->fts_instr == FTS_FOLLOW) {
			p->fts_info = fts_stat(sp, p, 1);
			p->fts_instr = FTS_NOINSTR;
		}

name:		t = sp->fts_path + NAPPEND(p->fts_parent);
		*t++ = '/';
		bcopy(p->fts_name, t, p->fts_namelen + 1);
		return(sp->fts_cur = p);
	}

	/* Move up to the parent node. */
	p = tmp->fts_parent;
	free(tmp);

	if (p->fts_level == FTS_ROOTPARENTLEVEL) {
		/*
		 * Done; free everything up and set errno to 0 so the user
		 * can distinguish between error and EOF.
		 */
		free(p);
		errno = 0;
		return(sp->fts_cur = NULL);
	}

	sp->fts_path[p->fts_pathlen] = '\0';

	/*
	 * Cd back up to the parent directory.  If at a root node, have to cd
	 * back to the original place, otherwise may not be able to access the
	 * original node on post-order.
	 */
	if (p->fts_level == FTS_ROOTLEVEL) {
		if (FCHDIR(sp, sp->fts_rfd)) {
			SET(FTS_STOP);
			return(NULL);
		}
	}
	else if (CHDIR(sp, "..")) {
		SET(FTS_STOP);
		return(NULL);
	}

	/* 
	 * If had a chdir error when trying to get into the directory, set the
	 * info field to reflect this, and restore errno.  The error indicator
	 * has to be reset to 0 so that if the user does an FTS_AGAIN, it all
	 * works.
	 */
	if (p->fts_cderr) {
		errno = p->fts_cderr;
		p->fts_cderr = 0;
		p->fts_info = FTS_ERR;
	} else
		p->fts_info = FTS_DP;
	return(sp->fts_cur = p);
}

/*
 * Fts_set takes the stream as an argument although it's not used in this
 * implementation; it would be necessary if anyone wanted to add global
 * semantics to fts using fts_set.  An error return is allowed for similar
 * reasons.
 */
/* ARGSUSED */
fts_set(sp, p, instr)
	FTS *sp;
	FTSENT *p;
	int instr;
{
	p->fts_instr = instr;
	return(0);
}

FTSENT *
fts_children(sp)
	register FTS *sp;
{
	register FTSENT *p;
	int fd;

	/* Set current node pointer. */
	p = sp->fts_cur;

	/*
	 * Set errno to 0 so that user can tell the difference between an
	 * error and a directory without entries.  If not a directory being
	 * visited in *pre-order*, or we've already had fatal errors, return
	 * immediately.
	 */
	errno = 0;
	if (ISSET(FTS_STOP) || p->fts_info != FTS_D && p->fts_info != FTS_DNR)
		return(NULL);

	/* Free up any previous child list. */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);

	/*
	 * If using chdir on a relative path and called BEFORE fts_read does
	 * its chdir to the root of a traversal, we can lose -- we need to
	 * chdir into the subdirectory, and we don't know where the current
	 * directory is, so we can't get back so that the upcoming chdir by
	 * fts_read will work.
	 */
	if (p->fts_level != FTS_ROOTLEVEL || p->fts_accpath[0] == '/' ||
	    ISSET(FTS_NOCHDIR))
		return(sp->fts_child = fts_build(sp, BCHILD));

	if ((fd = open(".", O_RDONLY, 0)) < 0)
		return(NULL);
	sp->fts_child = fts_build(sp, BCHILD);
	if (fchdir(fd))
		return(NULL);
	(void)close(fd);
	return(sp->fts_child);
}

/*
 * This is the tricky part -- do not casually change *anything* in here.  The
 * idea is to build the linked list of entries that are used by fts_children
 * and fts_read.  There are lots of special cases.
 *
 * The real slowdown in walking the tree is the stat calls.  If FTS_NOSTAT is
 * set and it's a physical walk (so that symbolic links can't be directories),
 * we assume that the number of subdirectories in a node is equal to the number
 * of links to the parent.  This allows stat calls to be skipped in any leaf
 * directories and for any nodes after the directories in the parent node have
 * been found.  This empirically cuts the stat calls by about 2/3.
 */
#define	ISDOT(a)	(a[0] == '.' && (!a[1] || a[1] == '.' && !a[2]))

static FTSENT *
fts_build(sp, type)
	register FTS *sp;
	int type;
{
	register struct dirent *dp;
	register FTSENT *p, *head;
	register int nitems;
	FTSENT *cur;
	DIR *dirp;
	int cderr, descend, len, level, maxlen, nlinks, saved_errno;
	char *cp;

	/* Set current node pointer. */
	cur = sp->fts_cur;

	/*
	 * Open the directory for reading.  If this fails, we're done.
	 * If being called from fts_read, set the fts_info field.
	 */
	if (!(dirp = opendir(cur->fts_accpath))) {
		if (type == BREAD)
			cur->fts_info = FTS_DNR;
		return(NULL);
	}

	/*
	 * Nlinks is the number of possible entries of type directory in the
	 * directory if we're cheating on stat calls, 0 if we're not doing
	 * any stat calls at all, -1 if we're doing stats on everything.
	 */
	nlinks =
	    ISSET(FTS_NOSTAT) && ISSET(FTS_PHYSICAL) ?
	    cur->fts_statb.st_nlink - (ISSET(FTS_SEEDOT) ? 0 : 2) : -1;

	/*
	 * If we're going to need to stat anything or we want to descend
	 * and stay in the directory, chdir.  If this fails we keep going.
	 * We won't be able to stat anything, but we can still return the
	 * names themselves.  Note, that since fts_read won't be able to
	 * chdir into the directory, it will have to return different path
	 * names than before, i.e. "a/b" instead of "b".  Since the node
	 * has already been visited in pre-order, have to wait until the
	 * post-order visit to return the error.  This is all fairly nasty.
	 * If a program needed sorted entries or stat information, they had
	 * better be checking FTS_NS on the returned nodes.
	 */
	if (nlinks || type == BREAD)
		if (FCHDIR(sp, dirfd(dirp))) {
			if (type == BREAD)
				cur->fts_cderr = errno;
			descend = nlinks = 0;
			cderr = 1;
		} else {
			descend = 1;
			cderr = 0;
		}
	else
		descend = 0;

	/*
	 * Figure out the max file name length that can be stored in the
	 * current path -- the inner loop allocates more path as necessary.
	 * We really wouldn't have to do the maxlen calculations here, we
	 * could do them in fts_read before returning the path, but it's a
	 * lot easier here since the length is part of the dirent structure.
	 *
	 * If not changing directories set a pointer so that we can just
	 * append each new name into the path.
	 */
	maxlen = sp->fts_pathlen - cur->fts_pathlen - 1;
	len = NAPPEND(cur);
	if (ISSET(FTS_NOCHDIR)) {
		cp = sp->fts_path + len;
		*cp++ = '/';
	}

	level = cur->fts_level + 1;

	/* Read the directory, attaching each entry to the `link' pointer. */
	for (head = NULL, nitems = 0; dp = readdir(dirp);) {
		if (!ISSET(FTS_SEEDOT) && ISDOT(dp->d_name))
			continue;

		if (!(p = fts_alloc(sp, dp->d_name, (int)dp->d_namlen)))
			goto mem1;
		if (dp->d_namlen > maxlen) {
			if (!fts_path(sp, (int)dp->d_namlen)) {
				/*
				 * No more memory for path or structures.  Save
				 * errno, free up the current structure and the
				 * structures already allocated.
				 */
mem1:				saved_errno = errno;
				if (p)
					free(p);
				fts_lfree(head);
				(void)closedir(dirp);
				errno = saved_errno;
				cur->fts_info = FTS_ERR;
				SET(FTS_STOP);
				return(NULL);
			}
			maxlen = sp->fts_pathlen - sp->fts_cur->fts_pathlen - 1;
		}

		p->fts_pathlen = len + dp->d_namlen + 1;
		p->fts_parent = sp->fts_cur;
		p->fts_level = level;

		if (nlinks) {
			/* Build a file name for fts_stat to stat. */
			if (ISSET(FTS_NOCHDIR)) {
				p->fts_accpath = p->fts_path;
				bcopy(p->fts_name, cp, p->fts_namelen + 1);
			} else
				p->fts_accpath = p->fts_name;
			p->fts_info = fts_stat(sp, p, 0);
			if (nlinks > 0 && p->fts_info == FTS_D)
				--nlinks;
		} else if (cderr) {
			p->fts_info = ISSET(FTS_NOSTAT) ? FTS_NSOK : FTS_NS;
			p->fts_accpath = cur->fts_accpath;
		} else {
			p->fts_accpath =
			    ISSET(FTS_NOCHDIR) ? p->fts_path : p->fts_name;
			p->fts_info = FTS_NSOK;
		}

		p->fts_link = head;
		head = p;
		++nitems;
	}
	(void)closedir(dirp);

	/*
	 * If not changing directories, reset the path back to original
	 * state.
	 */
	if (ISSET(FTS_NOCHDIR)) {
		if (cp - 1 > sp->fts_path)
			--cp;
		*cp = '\0';
	}

	/*
	 * If descended after called from fts_children or called from
	 * fts_read and didn't find anything, get back.  If can't get
	 * back, we're done.
	 */
	if (descend && (!nitems || type == BCHILD) && CHDIR(sp, "..")) {
		cur->fts_info = FTS_ERR;
		SET(FTS_STOP);
		return(NULL);
	}

	/* If we didn't find anything, just do the post-order visit */
	if (!nitems) {
		if (type == BREAD)
			cur->fts_info = FTS_DP;
		return(NULL);
	}

	/* Sort the entries. */
	if (sp->fts_compar && nitems > 1)
		head = fts_sort(sp, head, nitems);
	return(head);
}

static u_short
fts_stat(sp, p, follow)
	FTS *sp;
	register FTSENT *p;
	int follow;
{
	int saved_errno;

	/*
	 * If doing a logical walk, or application requested FTS_FOLLOW, do
	 * a stat(2).  If that fails, check for a non-existent symlink.  If
	 * fail, return the errno from the stat call.
	 */
	if (ISSET(FTS_LOGICAL) || follow) {
		if (stat(p->fts_accpath, &p->fts_statb)) {
			saved_errno = errno;
			if (!lstat(p->fts_accpath, &p->fts_statb)) {
				errno = 0;
				return(FTS_SLNONE);
			} 
			errno = saved_errno;
			bzero(&p->fts_statb, sizeof(struct stat));
			return(FTS_NS);
		}
	} else if (lstat(p->fts_accpath, &p->fts_statb)) {
		bzero(&p->fts_statb, sizeof(struct stat));
		return(FTS_NS);
	}

	/*
	 * Cycle detection is done as soon as we find a directory.  Detection
	 * is by brute force; if the tree gets deep enough or the number of
	 * symbolic links to directories high enough something faster might
	 * be worthwhile.
	 */
	if (S_ISDIR(p->fts_statb.st_mode)) {
		register FTSENT *t;
		register dev_t dev;
		register ino_t ino;

		dev = p->fts_statb.st_dev;
		ino = p->fts_statb.st_ino;
		for (t = p->fts_parent; t->fts_level > FTS_ROOTLEVEL;
		    t = t->fts_parent)
			if (ino == t->fts_statb.st_ino &&
			    dev == t->fts_statb.st_dev) {
				sp->fts_savelink = p->fts_link;
				p->fts_link = t;
				return(FTS_DC);
			}
		return(FTS_D);
	}
	if (S_ISLNK(p->fts_statb.st_mode))
		return(FTS_SL);
	if (S_ISREG(p->fts_statb.st_mode))
		return(FTS_F);
	return(FTS_DEFAULT);
}

#define	R(type, nelem, ptr) \
	(type *)realloc((void *)ptr, (u_int)((nelem) * sizeof(type)))

static FTSENT *
fts_sort(sp, head, nitems)
	FTS *sp;
	FTSENT *head;
	register int nitems;
{
	register FTSENT **ap, *p;

	/*
	 * Construct an array of pointers to the structures and call qsort(3).
	 * Reassemble the array in the order returned by qsort.  If unable to
	 * sort for memory reasons, return the directory entries in their
	 * current order.  Allocate enough space for the current needs plus
	 * 40 so we don't realloc one entry at a time.
	 */
	if (nitems > sp->fts_nitems) {
		sp->fts_nitems = nitems + 40;
		if (!(sp->fts_array =
		    R(FTSENT *, sp->fts_nitems, sp->fts_array))) {
			sp->fts_nitems = 0;
			return(head);
		}
	}
	for (ap = sp->fts_array, p = head; p; p = p->fts_link)
		*ap++ = p;
	qsort((void *)sp->fts_array, nitems, sizeof(FTSENT *), sp->fts_compar);
	for (head = *(ap = sp->fts_array); --nitems; ++ap)
		ap[0]->fts_link = ap[1];
	ap[0]->fts_link = NULL;
	return(head);
}

static FTSENT *
fts_alloc(sp, name, len)
	FTS *sp;
	char *name;
	register int len;
{
	register FTSENT *p;

	/*
	 * Variable sized structures; the name is the last element so
	 * we allocate enough extra space after the structure to store
	 * it.
	 */
	if (!(p = (FTSENT *)malloc((size_t)(sizeof(FTSENT) + len))))
		return(NULL);
	bcopy(name, p->fts_name, len + 1);
	p->fts_namelen = len;
	p->fts_path = sp->fts_path;
	p->fts_instr = FTS_NOINSTR;
	p->fts_cderr = 0;
	p->fts_number = 0;
	p->fts_pointer = NULL;
	return(p);
}

static void
fts_lfree(head)
	register FTSENT *head;
{
	register FTSENT *p;

	/* Free a linked list of structures. */
	while (p = head) {
		head = head->fts_link;
		free(p);
	}
}

/*
 * Allow essentially unlimited paths; certain programs (find, rm, ls) need to
 * work on any tree.  Most systems will allow creation of paths much longer
 * than MAXPATHLEN, even though the kernel won't resolve them.  Add an extra
 * 128 bytes to the requested size so that we don't realloc the path 2 bytes
 * at a time.
 */
static char *
fts_path(sp, size)
	FTS *sp;
	int size;
{
	sp->fts_pathlen += size + 128;
	return(sp->fts_path = R(char, sp->fts_pathlen, sp->fts_path));
}
