/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fts.c	5.12 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <string.h>
#include <stdlib.h>

FTSENT *fts_alloc(), *fts_build(), *fts_cycle(), *fts_root(), *fts_sort();
void fts_lfree(), fts_load();
u_short fts_stat();

/*
 * Special case a root of "/" so that slashes aren't appended causing
 * paths to be written as "//foo".
 */
#define	NAPPEND(p) \
	(p->fts_level == ROOTLEVEL && p->fts_pathlen == 1 && \
	    p->fts_path[0] == '/' ? 0 : p->fts_pathlen)

#define	ISSET(opt)	(sp->fts_options & opt)
#define	SET(opt)	(sp->fts_options |= opt)

#define	CHDIR(sp, path)	(!ISSET(FTS_NOCHDIR) && chdir(path))
#define	FCHDIR(sp, fd)	(!ISSET(FTS_NOCHDIR) && fchdir(fd))

/* fts_build flags */
#define	BCHILD		1		/* from fts_children */
#define	BREAD		2		/* from fts_read */

/* fts_level values */
#define	ROOTLEVEL	0
#define	ROOTPARENTLEVEL	-1

FTS *
fts_open(argv, options, compar)
	char *argv[];
	register int options;
	int (*compar)();
{
	register FTS *sp;
	register FTSENT *p, *root;
	register int nitems, maxlen;
	FTSENT *parent, *tmp;
	char *fts_path();

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
	parent->fts_level = ROOTPARENTLEVEL;

	/* Allocate/initialize root(s). */
	maxlen = -1;
	for (root = NULL, nitems = 0; *argv; ++argv, ++nitems) {
		if (!(p = fts_root(sp, *argv)))
			goto mem2;
		if (maxlen < p->fts_namelen)
			maxlen = p->fts_namelen;
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
		p->fts_level = ROOTLEVEL;
		p->fts_parent = parent;
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
	if (!ISSET(FTS_NOCHDIR) && (sp->fts_sd = open(".", O_RDONLY, 0)) < 0)
		SET(FTS_NOCHDIR);

	return(sp);

mem3:	free((void *)sp->fts_cur);
mem2:	fts_lfree(root);
	free((void *)parent);
mem1:	free((void *)sp);
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
	 * Load the stream structure for the next traversal; set the
	 * fts_accpath field specially so the chdir gets done to the
	 * right place and the user can access the first node.
	 */
	len = p->fts_pathlen = p->fts_namelen;
	bcopy(p->fts_name, sp->fts_path, len + 1);
	if ((cp = rindex(p->fts_name, '/')) && (cp != p->fts_name || cp[1])) {
		len = strlen(++cp);
		bcopy(cp, p->fts_name, len + 1);
		p->fts_namelen = len;
	}
	p->fts_accpath = p->fts_path = sp->fts_path;
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
		for (p = sp->fts_cur; p->fts_level > ROOTPARENTLEVEL;) {
			freep = p;
			p = p->fts_link ? p->fts_link : p->fts_parent;
			free((void *)freep);
		}
		free((void *)p);
	}

	/* Free up child linked list, sort array, path buffer. */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);
	if (sp->fts_array)
		free((void *)sp->fts_array);
	free((char *)sp->fts_path);

	/*
	 * Return to original directory, save errno if necessary; free up
	 * the directory buffer.
	 */
	if (!ISSET(FTS_NOCHDIR)) {
		saved_errno = fchdir(sp->fts_sd) ? errno : 0;
		(void)close(sp->fts_sd);
	}

	/* Free up the stream pointer. */
	free((void *)sp);

	/* Set errno and return. */
	if (!ISSET(FTS_NOCHDIR) && saved_errno) {
		errno = saved_errno;
		return(-1);
	}
	return(0);
}

FTSENT *
fts_read(sp)
	register FTS *sp;
{
	register FTSENT *p, *tmp;
	register int instr;
	register char *cp;
	static int cd;

	/* If finished or unrecoverable error, return NULL. */
	if (!sp->fts_cur || ISSET(FTS__STOP))
		return(NULL);

	/* Set current node pointer. */
	p = sp->fts_cur;

	/* Save and zero out user instructions. */
	instr = p->fts_instr;
	p->fts_instr = FTS__NOINSTR;

	/* If used fts_link pointer for cycle detection, restore it. */
	if (sp->fts_savelink) {
		p->fts_link = sp->fts_savelink;
		sp->fts_savelink = NULL;
	}

	/* Any type of file may be re-visited; re-stat and return. */
	if (instr == FTS_AGAIN) {
		p->fts_info = fts_stat(sp, p, 0);
		return(p);
	}

	/* Following a symbolic link. */
	if (p->fts_info == FTS_SL && instr == FTS_FOLLOW) {
		p->fts_info = fts_stat(sp, p, 1);
		return(p);
	}

	/* Directory in pre-order. */
	if (p->fts_info == FTS_D) {
		/* If skipped or crossed mount point, do post-order visit. */
		if (instr == FTS_SKIP || ISSET(FTS_XDEV) &&
		    p->fts_statb.st_dev != sp->sdev) {
			if (sp->fts_child) {
				fts_lfree(sp->fts_child);
				sp->fts_child = NULL;
			}
			p->fts_info = FTS_DP;
			return(p);
		} 

		/* Read the directory if necessary, and return first entry. */
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

	/* Move to next node on this level. */
next:	tmp = p;
	if (p = p->fts_link) {
		/*
		 * If root level node, set up paths and return.  If not the
		 * first time, and it's not an absolute pathname, get back
		 * to starting directory.  If that fails, we're dead.
		 */
		if (p->fts_level == ROOTLEVEL) {
			fts_load(sp, p);
			free((void *)tmp);
			if (cd &&
			    p->fts_path[0] != '/' && FCHDIR(sp, sp->fts_sd)) {
				/* Can't get back to start; we're dead. */
				p->fts_path = "starting directory";
				p->fts_info = FTS_ERR;
				SET(FTS__STOP);
				return(sp->fts_cur = p);
			} 
			cd = 1;
			p->fts_info = fts_stat(sp, p, 0);
			sp->sdev = p->fts_statb.st_dev;
		} else {
			free((void *)tmp);

			/* User may have called fts_set on node. */
			if (p->fts_instr == FTS_SKIP)
				goto next;
			if (p->fts_instr == FTS_FOLLOW) {
				p->fts_info = fts_stat(sp, p, 1);
				p->fts_instr = FTS__NOINSTR;
			}

			/* Fill in the paths. */
			cp = sp->fts_path + NAPPEND(p->fts_parent);
			*cp++ = '/';
			bcopy(p->fts_name, cp, p->fts_namelen + 1);

			/* Check for directory cycles. */
			if (p->fts_info == FTS_D && (tmp = fts_cycle(p))) {
				sp->fts_savelink = p->fts_link;
				p->fts_link = tmp;
				p->fts_info = FTS_DC;
			}
		}
		return(sp->fts_cur = p);
	}

	/* Move to parent. */
	p = tmp->fts_parent;
	free((void *)tmp);

	if (p->fts_level == ROOTPARENTLEVEL) {
		/*
		 * Done; free everything up and set errno to 0 so the user
		 * can distinguish between error and EOF.
		 */
		free((void *)p);
		errno = 0;
		return(sp->fts_cur = NULL);
	}

	sp->fts_path[p->fts_pathlen] = '\0';
	if (CHDIR(sp, "..")) {
		SET(FTS__STOP);
		p->fts_info = FTS_ERR;
	} else
		p->fts_info = FTS_DP;
	return(sp->fts_cur = p);
}

/*
 * fts_set takes the stream as an argument although it's not used in this
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
	 * error and a directory without entries.
	 */
	errno = 0;
	if (ISSET(FTS__STOP) ||
	    p->fts_info != FTS_D && p->fts_info != FTS_DNX &&
	    p->fts_info != FTS_DNR)
		return(NULL);

	/* Free up any previous child list. */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);

	/*
	 * If using chdir on a relative path and called BEFORE fts_read does
	 * its chdir to the root of a traversal, we can lose because we need
	 * to chdir into the subdirectory, and we don't know where the current
	 * directory is to get back so that the upcoming chdir by fts_read
	 * will work.
	 */
	if (p->fts_level != ROOTLEVEL || p->fts_accpath[0] == '/' ||
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

	/* Set current node pointer. */
	p = sp->fts_cur;

	if (!(dirp = opendir(p->fts_accpath))) {
		if (type == BREAD) {
			p->fts_info = FTS_DNR;
			errno = 0;
		}
		return(NULL);
	}

	/*
	 * The real slowdown in walking the tree is the stat calls.  If
	 * FTS_NOSTAT is set and it's a physical walk (so that symbolic
	 * links can't be directories), fts assumes that the number of
	 * subdirectories in a node is equal to the number of links to
	 * the parent.  This allows stat calls to be skipped in any leaf
	 * directories and for any nodes after the directories in the
	 * parent node have been found.  This empirically cuts the stat
	 * calls by about 2/3.
	 */
	nlinks =
	    ISSET(FTS_NOSTAT) && ISSET(FTS_PHYSICAL) ?
	    p->fts_statb.st_nlink - (ISSET(FTS_SEEDOT) ? 0 : 2) : -1;

	/* If told to descend or found links and not told not to descend. */
	descend = 0;
	if (nlinks || type == BREAD)
		if (!FCHDIR(sp, dirfd(dirp))) 
			descend = 1;
		/*
		 * Return all the information possible; an fts_read doing a
		 * relative walk of the tree will have to descend, so it can't
		 * succeed.  Fts_children or absolute walks of the tree can
		 * succeed, but no stat information will be available.  Reset
		 * errno as necessary.
		 */
		else {
			errno = 0;
			if (type == BREAD) {
				(void)closedir(dirp);
				p->fts_info = FTS_DNX;
				return(NULL);
			}
			nlinks = 0;
		}

	/* Get max file name length that can be stored in current path. */
	maxlen = sp->fts_pathlen - p->fts_pathlen - 1;

	cp = sp->fts_path + (len = NAPPEND(p));
	*cp++ = '/';

	level = p->fts_level + 1;

	/* Read the directory, attching each new entry to the `link' pointer. */
	for (head = NULL, nitems = 0; dp = readdir(dirp);) {
		if (ISDOT(dp->d_name) && !ISSET(FTS_SEEDOT))
			continue;

		if (!(p = fts_alloc(sp, dp->d_name, (int)dp->d_namlen))) {
			saved_errno = errno;
			goto mem1;
		}
		if (dp->d_namlen > maxlen) {
			if (!fts_path(sp, (int)dp->d_namlen)) {
				/* Quit: this stream no longer has a path. */
				SET(FTS__STOP);
				saved_errno = errno;
				free((void *)p);
mem1:				fts_lfree(head);
				if (type == BREAD)
					p->fts_info = FTS_ERR;
				if (descend && CHDIR(sp, "..")) {
					/*
					 * chdir error is more interesting
					 * than memory error, since it stops
					 * everything.
					 */
					saved_errno = errno;
					SET(FTS__STOP);
				}
				errno = saved_errno;
				(void)closedir(dirp);
				return(NULL);
			}
			maxlen = sp->fts_pathlen - sp->fts_cur->fts_pathlen - 1;
		}

		p->fts_pathlen = len + dp->d_namlen + 1;
		p->fts_accpath = ISSET(FTS_NOCHDIR) ? p->fts_path : p->fts_name;

		p->fts_parent = sp->fts_cur;
		p->fts_level = level;

		if (nlinks) {
			/* Make sure fts_stat has a filename to stat. */
			if (ISSET(FTS_NOCHDIR))
				bcopy(p->fts_name, cp, p->fts_namelen + 1);
			p->fts_info = fts_stat(sp, p, 0);
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

	/* Reset the path. */
	if (cp - 1 > sp->fts_path)
		--cp;
	*cp = '\0';

	/*
	 * If descended: if were called from fts_read and didn't find anything,
	 * or were called from fts_children, get back.
	 */
	if (descend && (!nitems || type == BCHILD) && CHDIR(sp, "..")) {
		SET(FTS__STOP);
		p->fts_info = FTS_ERR;
		return(NULL);
	}

	if (!nitems) {
		if (type == BREAD)
			p->fts_info = FTS_DP;
		return(NULL);
	}

	if (sp->fts_compar && nitems > 1)
		head = fts_sort(sp, head, nitems);

	if (type == BREAD) {
		*cp = '/';
		bcopy(head->fts_name, cp + 1, head->fts_namelen + 1);
	}
	return(head);
}

static u_short
fts_stat(sp, p, follow)
	FTS *sp;
	register FTSENT *p;
	int follow;
{
	/*
	 * If doing a logical walk, or application requested FTS_FOLLOW, do
	 * a stat(2).  If that fails, either fail or do an lstat(2) for a
	 * non-existent symlink.  (The check has to be done, or we wouldn't
	 * detect a symlink being deleted.)
	 *
	 * Don't leave errno set for FTS_NS cases.		XXX
	 */
	if (ISSET(FTS_LOGICAL) || follow) {
		if (stat(p->fts_accpath, &p->fts_statb)) {
			errno = 0;
			if (follow && !lstat(p->fts_accpath, &p->fts_statb))
				return(FTS_SLNONE);
			else {
				errno = 0;
				return(FTS_NS);
			}
		}
	} else if (lstat(p->fts_accpath, &p->fts_statb)) {
		errno = 0;
		return(FTS_NS);
	}

	if (S_ISDIR(p->fts_statb.st_mode))
		return(FTS_D);
	if (S_ISLNK(p->fts_statb.st_mode))
		return(FTS_SL);
	if (S_ISREG(p->fts_statb.st_mode))
		return(FTS_F);
	return(FTS_DEFAULT);
}

static FTSENT *
fts_cycle(p)
	register FTSENT *p;
{
	register dev_t dev;
	register ino_t ino;

	/*
	 * Cycle detection is brute force; if the tree gets deep enough or
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
	 * allocate enough extra space after the structure to hold it.
	 */
	if (!(p = (FTSENT *)malloc((size_t)(sizeof(FTSENT) + len))))
		return(NULL);
	bcopy(name, p->fts_name, len + 1);
	p->fts_namelen = len;
	p->fts_path = sp->fts_path;
	p->fts_instr = FTS__NOINSTR;
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
		free((void *)p);
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
	return(sp->fts_path = R(char, sp->fts_pathlen, sp->fts_path)); }

static FTSENT *
fts_root(sp, name)
	FTS *sp;
	register char *name;
{
	register char *cp;

	/*
	 * Rip trailing slashes; it's somewhat unclear in POSIX 1003.1 what
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
	return(fts_alloc(sp, name, cp - name));
}
