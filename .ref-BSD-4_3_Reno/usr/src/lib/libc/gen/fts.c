/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fts.c	5.10 (Berkeley) 6/9/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <string.h>
#include <stdlib.h>

FTSENT *fts_alloc(), *fts_build(), *fts_cycle(), *fts_sort(), *fts_root();
short fts_stat();

/*
 * Special case a root of "/" so that slashes aren't appended causing
 * paths to be written as "//foo".
 */
#define	NAPPEND(p) \
	(p->fts_level == ROOTLEVEL && p->fts_pathlen == 1 && \
	    p->fts_path[0] == '/' ? 0 : p->fts_pathlen)

#define	CHDIR(sp, path)	(!(sp->fts_options & FTS_NOCHDIR) && chdir(path))
#define	FCHDIR(sp, fd)	(!(sp->fts_options & FTS_NOCHDIR) && fchdir(fd))

#define	ROOTLEVEL	0
#define	ROOTPARENTLEVEL	-1

/* fts_build flags */
#define	BCHILD		1		/* from ftschildren */
#define	BREAD		2		/* from ftsread */

static FTS *stream;			/* current stream pointer */

FTS *
ftsopen(argv, options, compar)
	char *argv[];
	register int options;
	int (*compar)();
{
	register FTS *sp;
	register FTSENT *p, *root;
	register int nitems, maxlen;
	FTSENT *parent, *tmp;
	char *fts_path();

	/* allocate/initialize the stream */
	if (!(stream = sp = (FTS *)malloc((u_int)sizeof(FTS))))
		return(NULL);
	bzero(sp, sizeof(FTS));
	sp->fts_compar = compar;

	/*
	 * logical walks turn on NOCHDIR; symbolic links are just too
	 * hard to deal with.
	 */
	sp->fts_options = options;
	if (options & FTS_LOGICAL)
		sp->fts_options |= FTS_NOCHDIR;

	/* allocate/initialize root's parent */
	if (!(parent = fts_alloc("", 0)))
		goto mem1;
	parent->fts_level = ROOTPARENTLEVEL;

	/* allocate/initialize root(s) */
	maxlen = -1;
	for (root = NULL, nitems = 0; *argv; ++argv, ++nitems) {
		if (!(p = fts_root(*argv)))
			goto mem2;
		if (maxlen < p->fts_namelen)
			maxlen = p->fts_namelen;
		/*
		 * if comparison routine supplied, traverse in sorted
		 * order; otherwise traverse in the order specified.
		 */
		if (compar) {
			p->fts_link = root;
			root = p;
			p->fts_accpath = p->fts_name;
			if (!(options & FTS_NOSTAT))
				p->fts_info = fts_stat(p, 0);
		} else {
			p->fts_link = NULL;
			if (!root)
				tmp = root = p;
			else {
				tmp->fts_link = p;
				tmp = p;
			}
		}
		p->fts_level = ROOTLEVEL;
		p->fts_parent = parent;
	}
	if (compar && nitems > 1)
		root = fts_sort(root, nitems);

	/*
	 * allocate a dummy pointer and make ftsread think that we've just
	 * finished the node before the root(s); set p->fts_info to FTS_NS
	 * so that everything about the "current" node is ignored.
	 */
	if (!(sp->fts_cur = fts_alloc("", 0)))
		goto mem2;
	sp->fts_cur->fts_link = root;
	sp->fts_cur->fts_info = FTS_NS;

	/* start out with at least 1K+ of path space */
	if (!fts_path(MAX(maxlen, MAXPATHLEN)))
		goto mem3;

	/*
	 * if using chdir(2), grab a file descriptor pointing to dot to insure
	 * that we can get back here; this could be avoided for some paths,
	 * but almost certainly not worth the effort.  Slashes, symbolic links,
	 * and ".." are all fairly nasty problems.  Note, if we can't get the
	 * descriptor we run anyway, just more slowly.
	 */
	if (!(options & FTS_NOCHDIR) &&
	    (sp->fts_sd = open(".", O_RDONLY, 0)) < 0)
		sp->fts_options |= FTS_NOCHDIR;

	return(sp);

mem3:	(void)free((void *)sp->fts_cur);
mem2:	fts_lfree(root);
	(void)free((void *)parent);
mem1:	(void)free((void *)sp);
	return(NULL);
}

static
fts_load(p)
	register FTSENT *p;
{
	register int len;
	register char *cp;

	/*
	 * load the stream structure for the next traversal; set the
	 * accpath field specially so the chdir gets done to the right
	 * place and the user can access the first node.
	 */
	len = p->fts_pathlen = p->fts_namelen;
	bcopy(p->fts_name, stream->fts_path, len + 1);
	if ((cp = rindex(p->fts_name, '/')) && (cp != p->fts_name || cp[1])) {
		len = strlen(++cp);
		bcopy(cp, p->fts_name, len + 1);
		p->fts_namelen = len;
	}
	p->fts_accpath = p->fts_path = stream->fts_path;
}

ftsclose(sp)
	FTS *sp;
{
	register FTSENT *freep, *p;
	int saved_errno;

	if (sp->fts_cur) {
		/*
		 * this still works if we haven't read anything -- the dummy
		 * structure points to the root list, so we step through to
		 * the end of the root list which has a valid parent pointer.
		 */
		for (p = sp->fts_cur; p->fts_level > ROOTPARENTLEVEL;) {
			freep = p;
			p = p->fts_link ? p->fts_link : p->fts_parent;
			(void)free((void *)freep);
		}
		(void)free((void *)p);
	}

	/* free up child linked list, sort array, path buffer */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);
	if (sp->fts_array)
		(void)free((void *)sp->fts_array);
	(void)free((char *)sp->fts_path);

	/*
	 * return to original directory, save errno if necessary;
	 * free up the directory buffer
	 */
	if (!(sp->fts_options & FTS_NOCHDIR)) {
		saved_errno = fchdir(sp->fts_sd) ? errno : 0;
		(void)close(sp->fts_sd);
	}

	/* free up the stream pointer */
	(void)free((void *)sp);

	/* set errno and return */
	if (!(sp->fts_options & FTS_NOCHDIR) && saved_errno) {
		errno = saved_errno;
		return(-1);
	}
	return(0);
}

FTSENT *
ftsread(sp)
	register FTS *sp;
{
	register FTSENT *p, *tmp;
	register int instr;
	register char *cp;
	static int cd;

	/* if finished or unrecoverable error, return NULL */
	if (!sp->fts_cur || sp->fts_options & FTS__STOP)
		return(NULL);

	/* set global stream pointer, and current node pointer */
	stream = sp;
	p = sp->fts_cur;

	/* save and zero out user instructions */
	instr = p->fts_instr;
	p->fts_instr = 0;

	/* if used link pointer for cycle detection, restore it */
	if (sp->fts_savelink) {
		p->fts_link = sp->fts_savelink;
		sp->fts_savelink = NULL;
	}

	/* any type of file may be re-visited; re-stat and return */
	if (instr == FTS_AGAIN) {
		p->fts_info = fts_stat(p, 0);
		return(p);
	}

	/* following a symbolic link */
	if (p->fts_info == FTS_SL && instr == FTS_FOLLOW) {
		p->fts_info = fts_stat(p, 1);
		return(p);
	}

	/* directory in pre-order */
	if (p->fts_info == FTS_D) {
		/* if skipped or crossed mount point, do post-order visit */
		if (instr == FTS_SKIP || sp->fts_options & FTS_XDEV &&
		    p->fts_statb.st_dev != sp->sdev) {
			if (sp->fts_child) {
				fts_lfree(sp->fts_child);
				sp->fts_child = NULL;
			}
			p->fts_info = FTS_DP;
			return(p);
		} 

		/* read the directory if necessary, and return first entry */
		if (sp->fts_child) 
			if (CHDIR(sp, p->fts_accpath)) {
				fts_lfree(sp->fts_child);
				p->fts_info = FTS_DNX;
			} else {
				p = sp->fts_child;
				cp = sp->fts_path + NAPPEND(p->fts_parent);
				*cp++ = '/';
				bcopy(p->fts_name, cp, p->fts_namelen + 1);
			}
		else {
			if (!(sp->fts_child = fts_build(sp, BREAD)))
				return(p);
			p = sp->fts_child;
		}
		sp->fts_child = NULL;
		return(sp->fts_cur = p);
	}

	/* move to next node on this level */
next:	tmp = p;
	if (p = p->fts_link) {
		/*
		 * if root level node, set up paths and return.  If not the
		 * first time, and it's not an absolute pathname, get back
		 * to starting directory.
		 */
		if (p->fts_level == ROOTLEVEL) {
			fts_load(p);
			(void)free((void *)tmp);
			if (cd &&
			    p->fts_path[0] != '/' && FCHDIR(sp, sp->fts_sd)) {
				/* should never happen... */
				p->fts_path = "starting directory";
				p->fts_info = FTS_ERR;
				sp->fts_options |= FTS__STOP;
				return(sp->fts_cur = p);
			} 
			cd = 1;
			p->fts_info = fts_stat(p, 0);
			sp->sdev = p->fts_statb.st_dev;
		} else {
			(void)free((void *)tmp);

			/* user may have called ftsset on node */
			if (p->fts_instr == FTS_SKIP)
				goto next;
			if (p->fts_instr == FTS_FOLLOW) {
				p->fts_info = fts_stat(p, 1);
				p->fts_instr = 0;
			}

			/* fill in the paths */
			cp = sp->fts_path + NAPPEND(p->fts_parent);
			*cp++ = '/';
			bcopy(p->fts_name, cp, p->fts_namelen + 1);

			/* check for directory cycles */
			if (p->fts_info == FTS_D && (tmp = fts_cycle(p))) {
				sp->fts_savelink = p->fts_link;
				p->fts_link = tmp;
				p->fts_info = FTS_DC;
			}
		}
		return(sp->fts_cur = p);
	}

	/* move to parent */
	p = tmp->fts_parent;
	(void)free((void *)tmp);

	if (p->fts_level == ROOTPARENTLEVEL) {
		/*
		 * done; free everything up and set errno to 0 so the user
		 * can distinguish between error and EOF.
		 */
		(void)free((void *)p);
		errno = 0;
		return(sp->fts_cur = NULL);
	}

	sp->fts_path[p->fts_pathlen] = '\0';
	if (CHDIR(sp, "..")) {
		sp->fts_options |= FTS__STOP;
		p->fts_info = FTS_ERR;
	} else
		p->fts_info = FTS_DP;
	return(sp->fts_cur = p);
}

/*
 * ftsset takes the stream as an argument although it's not used in this
 * implementation; it would be necessary if anyone wanted to add global
 * semantics to fts using ftsset.  A possible error return is allowed for
 * similar reasons.
 */
/* ARGSUSED */
ftsset(sp, p, instr)
	FTS *sp;
	FTSENT *p;
	int instr;
{
	p->fts_instr = instr;
	return(0);
}

FTSENT *
ftschildren(sp)
	register FTS *sp;
{
	register FTSENT *p;
	int fd;

	/*
	 * set errno to 0 so that user can tell the difference between an
	 * error and a directory without entries.
	 */
	errno = 0;
	p = sp->fts_cur;
	if (p->fts_info != FTS_D || sp->fts_options & FTS__STOP)
		return(NULL);
	if (sp->fts_child)
		fts_lfree(sp->fts_child);

	/*
	 * if using chdir on a relative path and called BEFORE ftsread on the
	 * root of a traversal, we can lose because we need to chdir into the
	 * subdirectory, and we don't know where the current directory is to
	 * get back so that the upcoming chdir by ftsread will work.
	 */
	if (p->fts_level != ROOTLEVEL || p->fts_accpath[0] == '/' ||
	    sp->fts_options & FTS_NOCHDIR)
		return(sp->fts_child = fts_build(sp, BCHILD));

	if ((fd = open(".", O_RDONLY, 0)) < 0)
		return(NULL);
	sp->fts_child = fts_build(sp, BCHILD);
	if (fchdir(fd))
		return(NULL);
	(void)close(fd);
	return(sp->fts_child);
}

#define	ISDOT(a)	(a[0] == '.' && (!a[1] || a[1] == '.' && !a[2]))

FTSENT *
fts_build(sp, type)
	register FTS *sp;
	int type;
{
	register struct dirent *dp;
	register FTSENT *p, *head;
	register int nitems;
	DIR *dirp;
	int descend, len, level, maxlen, nlinks, saved_errno;
	char *cp;

	p = sp->fts_cur;
	if (!(dirp = opendir(p->fts_accpath))) {
		if (type == BREAD) {
			errno = 0;
			p->fts_info = FTS_DNR;
		}
		return(NULL);
	}

	/*
	 * the real slowdown in walking the tree is the stat calls.  If
	 * FTS_NOSTAT is set and it's a physical walk (so that symbolic
	 * links can't be directories), fts assumes that the number of
	 * subdirectories in a node is equal to the number of links to
	 * the parent.  This allows stat calls to be skipped in any leaf
	 * directories and for any nodes after the directories in the
	 * parent node have been found.  This empirically cuts the stat
	 * calls by about 2/3.
	 */
	nlinks =
	    sp->fts_options & FTS_NOSTAT && sp->fts_options & FTS_PHYSICAL ?
	    p->fts_statb.st_nlink - (sp->fts_options & FTS_SEEDOT ? 0 : 2) : -1;

	/* if told to descend or found links and not told not to descend. */
	if (nlinks || type == BREAD) {
		if (FCHDIR(sp, dirfd(dirp))) {
			if (type == BREAD) {
				errno = 0;
				p->fts_info = FTS_DNX;
			}
			(void)closedir(dirp);
			return(NULL);
		}
		descend = 1;
	} else
		descend = 0;

	/* get max file name length that can be stored in current path */
	maxlen = sp->fts_pathlen - p->fts_pathlen - 1;

	cp = sp->fts_path + (len = NAPPEND(p));
	*cp++ = '/';

	level = p->fts_level + 1;

	/* read the directory, attching each new entry to the `link' pointer */
	for (head = NULL, nitems = 0; dp = readdir(dirp);) {
		if (ISDOT(dp->d_name) && !(sp->fts_options & FTS_SEEDOT))
			continue;

		if (!(p = fts_alloc(dp->d_name, dp->d_namlen))) {
			saved_errno = errno;
			goto mem1;
		}
		if (dp->d_namlen > maxlen) {
			if (!fts_path((int)dp->d_namlen)) {
				/* quit: this stream no longer has a path */
				sp->fts_options |= FTS__STOP;
				saved_errno = errno;
				(void)free((void *)p);
mem1:				fts_lfree(head);
				if (type == BREAD)
					p->fts_info = FTS_ERR;
				if (descend && CHDIR(sp, "..")) {
					saved_errno = errno;
					sp->fts_options |= FTS__STOP;
				}
				errno = saved_errno;
				(void)closedir(dirp);
				return(NULL);
			}
			maxlen = sp->fts_pathlen - sp->fts_cur->fts_pathlen - 1;
		}

		p->fts_pathlen = len + dp->d_namlen + 1;
		p->fts_accpath =
		    sp->fts_options & FTS_NOCHDIR ? p->fts_path : p->fts_name;

		p->fts_parent = sp->fts_cur;
		p->fts_level = level;

		if (nlinks) {
			/* make sure fts_stat has a filename to stat */
			if (sp->fts_options & FTS_NOCHDIR)
				bcopy(p->fts_name, cp, p->fts_namelen + 1);
			p->fts_info = fts_stat(p, 0);
			if (nlinks > 0 && (p->fts_info == FTS_D ||
			    p->fts_info == FTS_DNR || p->fts_info == FTS_DNX))
				--nlinks;
		} else
			p->fts_info = FTS_NS;

		p->fts_link = head;
		head = p;
		++nitems;
	}
	(void)closedir(dirp);

	/* reset the path */
	if (cp - 1 > sp->fts_path)
		--cp;
	*cp = '\0';

	/*
	 * if descended: if were called from ftsread and didn't find anything,
	 * or were called from ftschildren, get back.
	 */
	if (descend && (!nitems || type == BCHILD) && CHDIR(sp, "..")) {
		sp->fts_options |= FTS__STOP;
		p->fts_info = FTS_ERR;
		return(NULL);
	}

	if (!nitems) {
		if (type == BREAD)
			p->fts_info = FTS_DP;
		return(NULL);
	}

	if (sp->fts_compar && nitems > 1)
		head = fts_sort(head, nitems);

	if (type == BREAD) {
		*cp = '/';
		bcopy(head->fts_name, cp + 1, head->fts_namelen + 1);
	}
	return(head);
}

static short
fts_stat(p, symflag)
	register FTSENT *p;
	int symflag;
{
	/*
	 * detection of symbolic links w/o targets.  If FTS_FOLLOW is set,
	 * the symlink structure is overwritten with the stat structure of
	 * the target.
	 */
	if (stream->fts_options & FTS_LOGICAL || symflag) {
		if (stat(p->fts_accpath, &p->fts_statb))
			return(symflag || !lstat(p->fts_accpath,
			    &p->fts_statb) ? FTS_SLNONE : FTS_ERR);
	} else if (lstat(p->fts_accpath, &p->fts_statb))
		return(FTS_ERR);

	switch(p->fts_statb.st_mode&S_IFMT) {
	case S_IFDIR:
		return(FTS_D);
	case S_IFLNK:
		return(FTS_SL);
	case S_IFREG:
		return(FTS_F);
	}
	return(FTS_DEFAULT);
}

static FTSENT *
fts_cycle(p)
	register FTSENT *p;
{
	register dev_t dev;
	register ino_t ino;

	/*
	 * cycle detection is brute force; if the tree gets deep enough or
	 * the number of symbolic links to directories is really high
	 * something faster might be worthwhile.
	 */
	dev = p->fts_statb.st_dev;
	ino = p->fts_statb.st_ino;
	for (p = p->fts_parent; p->fts_level > ROOTLEVEL; p = p->fts_parent)
		if (ino == p->fts_statb.st_ino && dev == p->fts_statb.st_dev)
			return(p);
	return(NULL);
}

#define	R(type, nelem, ptr) \
	(type *)realloc((void *)ptr, (u_int)((nelem) * sizeof(type)))

static FTSENT *
fts_sort(head, nitems)
	FTSENT *head;
	register int nitems;
{
	register FTSENT **ap, *p;

	/*
	 * construct an array of pointers to the structures and call qsort(3).
	 * Reassemble the array in the order returned by qsort.  If unable to
	 * sort for memory reasons, return the directory entries in their
	 * current order.  Allocate enough space for the current needs plus
	 * 40 so we don't realloc one entry at a time.
	 */
	if (nitems > stream->fts_nitems) {
		stream->fts_nitems = nitems + 40;
		if (!(stream->fts_array =
		    R(FTSENT *, stream->fts_nitems, stream->fts_array))) {
			stream->fts_nitems = 0;
			return(head);
		}
	}
	for (ap = stream->fts_array, p = head; p; p = p->fts_link)
		*ap++ = p;
	qsort((void *)stream->fts_array, nitems, sizeof(FTSENT *),
	    stream->fts_compar);
	for (head = *(ap = stream->fts_array); --nitems; ++ap)
		ap[0]->fts_link = ap[1];
	ap[0]->fts_link = NULL;
	return(head);
}

static FTSENT *
fts_alloc(name, len)
	char *name;
	register int len;
{
	register FTSENT *p;

	/*
	 * variable sized structures; the name is the last element so
	 * allocate enough extra space after the structure to hold it.
	 */
	if (!(p = (FTSENT *)malloc((size_t)(sizeof(FTSENT) + len))))
		return(NULL);
	bcopy(name, p->fts_name, len + 1);
	p->fts_namelen = len;
	p->fts_path = stream->fts_path;
	p->fts_instr = 0;
	p->fts_local.number = 0;
	p->fts_local.pointer = NULL;
	return(p);
}

static
fts_lfree(head)
	register FTSENT *head;
{
	register FTSENT *p;

	while (p = head) {
		head = head->fts_link;
		(void)free((void *)p);
	}
}

/*
 * allow essentially unlimited paths; certain programs (find, remove, ls)
 * need to work on any tree.  Most systems will allow creation of paths
 * much longer than MAXPATHLEN, even though the kernel won't resolve them.
 * Add an extra 128 bytes to the requested size so that we don't realloc
 * the path 2 bytes at a time.
 */
static char *
fts_path(size)
	int size;
{
	stream->fts_pathlen += size + 128;
	return(stream->fts_path =
	    R(char, stream->fts_pathlen, stream->fts_path));
}

static FTSENT *
fts_root(name)
	register char *name;
{
	register char *cp;

	/*
	 * rip trailing slashes; it's somewhat unclear in POSIX 1003.1 what
	 * /a/b/ really is, they don't talk about what a null path component
	 * resolves to.  This hopefully does what the user intended.  Don't
	 * allow null pathnames.
	 */
	for (cp = name; *cp; ++cp);
	if (cp == name) {
		errno = ENOENT;
		return(NULL);
	}
	while (--cp > name && *cp == '/');
	*++cp = '\0';
	return(fts_alloc(name, cp - name));
}
