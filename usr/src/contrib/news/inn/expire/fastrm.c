/*
 * delete a list of filenames from stdin
 *
 * exit(0) if all is OK (files that can't be unlinked because they
 *	didn't exist is "OK")
 *
 * exit(1) in other cases - problems with stdin, no permission, ...
 * written by <kre@munnari.oz.au>
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "configdata.h"
#include "mydir.h"
#include "clibrary.h"
#include "macros.h"
#include "libinn.h"

#define MAX_LINE_SIZE	1024
#define SHORT_NAME	16
#define MAX_DIR_LEN	2048

/* typedef unsigned char chr; */
typedef char	chr;

typedef struct _dnode {
    struct _dnode	*next;
    int			count;
    chr			*dir;
    chr			*longname;
    chr			shortname[SHORT_NAME];
} dnode;

#define	NODENAME(d)	((d)->longname ? (d)->longname : (d)->shortname)


STATIC char	DotDot[] = "../../../../";
STATIC chr	base_dir[MAX_DIR_LEN];
STATIC chr	cur_dir[MAX_DIR_LEN];
STATIC chr	prefix_dir[MAX_DIR_LEN];
STATIC int	prefix_len;
STATIC char	*MyName;
STATIC int	fatals;				/* error counter */
STATIC BOOL	AmRoot;
STATIC BOOL	Debugging;
STATIC int	dotdot;
STATIC int	sortdirs;
STATIC int	cdval = 3;


STATIC void
err_exit(s)
    char	*s;
{
    (void)fprintf(stderr, "%s: %s\n", MyName, s);
    exit(1);
}


STATIC int
myexit()
{
    err_exit("Could not allocate memory");
    /* NOTREACHED */
}


/*
**  Get the next line from stdin into 'l' which has 'len' bytes available.
*/
STATIC BOOL
get_line(l, len)
    register chr	*l;
    register int	len;
{
    static int		count;
    register chr	*p;

    for ( ; ; ) {
	/* Get the line. */
	if (fgets(l, len, stdin) == NULL)
	    return FALSE;
	count++;

	/* See if we got the \n terminator. */
	p = (chr *)strchr(l, '\n');
	if (p != NULL) {
	    /* Yes, ok, that's a good line, trash the \n & return. */
	    *p = '\0';
	    return TRUE;
	}

	/* No, this line is longer than our buffer. */
	(void)fprintf(stderr, "%s: Line %d (%.40s...) too long\n",
		MyName, count, l);
	fatals++;

	/* Trash the rest of the long line. */
	while (fgets(l, len, stdin) != NULL && strchr(l, '\n') == NULL)
	    continue;

	/* Go back and get the nest (just ignore the long one). */
    }
}


/*
**  Remember a file name; this is pretty trivial (only fancy bit is not
**  malloc'ing mem for short names).
*/
STATIC dnode *
build_node(prev, dir, name)
    dnode		*prev;
    chr			*dir;
    chr			*name;
{
    register dnode	*n;
    register int	i;

    n = NEW(dnode, 1);
    if (prev)
	    prev->next = n;
    n->next = NULL;
    n->dir = dir;

    i = strlen(name);
    if (i >= SHORT_NAME) {
	n->longname = COPY(name);
    }
    else {
	n->longname = NULL;
	(void)strcpy(n->shortname, name);
    }
    return n;
}


/*
**  Read lines from stdin (including the first that may have been there
**  from our last time in) until we reach EOF, or until we get a line that
**  names a file not in the same directory as the previous lot remember
**  the file names in the directory we're examining, and count them
*/
STATIC dnode *
build_dir(ip)
    int		*ip;
{
    static chr	line[MAX_LINE_SIZE];
    register dnode	*start;
    register dnode	*n;
    register int	dlen;
    register chr	*p;
    register chr	*dir;

    *ip = 0;
    if (line[0] == '\0' && !get_line(line, (int)sizeof line))
	return NULL;

    /* Build node. */
    p = (chr *)strrchr(line, '/');
    if (p != NULL) {
	*p++ = '\0';
	dlen = strlen(line);
	dir = COPY(line);
    }
    else {
	dir = NULL;
	dlen = -1;
	p = line;
    }
    n = start = build_node((dnode *)NULL, dir, p);
    *ip = 1;

    while (get_line(line, (int)sizeof line)) {
	if ((dlen < 0 && strchr(line, '/'))
	 || (dlen >= 0 && (line[dlen] != '/'
			      || strchr(line + dlen + 1, '/') != NULL
			      || strncmp(dir, line, dlen))))
	    return start;

	n = build_node(n, dir, line + dlen + 1);
	(*ip)++;
    }
    line[0] = '\0';
    return start;
}


/*
**  Sorting predicate for qsort to put nodes in alphabetical order.
*/
STATIC int
comp(a, b)
	POINTER a;
	POINTER b;
{
	dnode *l1, *l2;

	l1 = *CAST(dnode**, a);
	l2 = *CAST(dnode**, b);
	return strcmp(NODENAME(l1), NODENAME(l2));
}


/*
**  Find a node in the list.
*/
STATIC dnode *
inlist(list, num, name)
    register dnode	**list;
    int			num;
    char		*name;
{
    register dnode	**top;
    register dnode	**cur;
    register int	i;

    for (top = list + num - 1; top >= list; ) {
	cur = list + (top - list) / 2;

	i = strcmp(name, NODENAME(*cur));
	if (i == 0)
	    return *cur;

	if (i < 0)
	    top = cur - 1;
	else
	    list = cur + 1;
    }
    return NULL;
}


/*
**  Free a list of nodes.
*/
STATIC void
freelist(list)
    register dnode *list;
{
    register dnode *l;

    while ((l = list) != NULL) {
	list = l->next;
	if (l->longname)
	    DISPOSE(l->longname);
	DISPOSE(l);
    }
}


STATIC void
unlink_node(n)
    dnode		*n;
{
    register chr	*p;
    struct stat		sb;
    int			oerrno;

    p = NODENAME(n);
    if (prefix_len != 0) {
	(void)strcpy(prefix_dir + prefix_len, p);
	p = prefix_dir;
    }

    if (AmRoot) {
	if (stat(p, &sb) < 0) {
	    if (errno != ENOENT) {
		oerrno = errno;
		(void)fprintf(stderr, "%s: stat ", MyName);
		if (*p != '/')
		    (void)fprintf(stderr, "in %s: ", cur_dir);
		(void)fflush(stderr);
		errno = oerrno;
		perror(p);
		fatals++;
	    }
	    return;
	}
	if (S_ISDIR(sb.st_mode)) {
	    (void)fprintf(stderr, "%s: Directory ", MyName);
	    if (*p != '/')
		(void)fprintf(stderr, "in %s: ", cur_dir);
	    (void)fprintf(stderr, "\"%s\"\n", p);
	    fatals++;
	    return;
	}
    }

    if (Debugging) {
	if (*p == '/')
	    (void)printf("%s\n", p);
	else
	    (void)printf("%s / %s\n", cur_dir, p);
	return;
    }

    if (unlink(p) < 0) {
	if (errno != ENOENT) {
	    oerrno = errno;
	    (void)fprintf(stderr, "%s: unlink ", MyName);
	    if (*p != '/')
		(void)fprintf(stderr, "in %s: ", cur_dir);
	    (void)fflush(stderr);
	    errno = oerrno;
	    perror(p);
	    fatals++;
	}
    }
}


STATIC void
copynsegs(from, to, n)
    register chr	*from;
    register chr	*to;
    register int	n;
{
    register chr	c;

    while ((*to++ = c = *from++) != '\0')
	if (c == '/' && --n <= 0)
	    break;

    if (c == '/')
	*--to = '\0';
}


STATIC int
slashcount(p)
    register chr	*p;
{
    register int	i;

    for (i = 0; *p; )
	if (*p++ == '/')
	    i++;
    return i;
}


/*
**  Set our environment (process working cirectory, and global vars) to
**  reflect a change to directory 'd' (relative to base_dir if 'd' is not
**  an absolute path).   We're likely to want to do different things
**  depending on the amount of work to do in 'd' - that's given by 'num'.
**  Return FALSE if the directory can be determined not to exist.
*/
STATIC BOOL
setup_dir(d, num)
    register chr	*d;
    int			num;
{
    register chr	*p;
    register chr	*q;
    register chr	*abs;
    register int	bsegs;
    register int	oerrno;
    chr			string[MAX_DIR_LEN];

    bsegs = slashcount(base_dir);
    if (d == NULL)
	abs = base_dir;
    else if (*d == '/')
	abs = d;
    else if (*d == '\0')
	abs = (chr *)"/";
    else {
	while (d[0] == '.' && d[1] == '/')
	    for (d += 2; *d == '/'; )
		    d++;
	while (bsegs > 0 && d[0] == '.' && d[1] == '.' && d[2] == '/')
	    for (bsegs--, d += 3; *d == '/'; )
		d++;
	if (bsegs <= 0)
	    err_exit("Can't handle that many ..'s in path");
	abs = string;
	copynsegs(base_dir, abs, bsegs + 1);
	(void)strcat(abs, "/");
	(void)strcat(abs, d);
    }

    /* Now "abs" is the full path name of the directory we want to
     * be at, and "cur_dir" is where we presently are. */
    for (p = abs, q = cur_dir; *p == *q; ) {
	/* If we've reached the end, this is easy, we want to be in
	 * the same place as we were (which is probably really some
	 * kind of error, it shouldn't happen). */
	if (*p == '\0')
	    return TRUE;
	p++;
	q++;
    }

    prefix_len = 0;
    if (*p == 0 && *q == '/') {
	/* Want to go back up the tree, it might be faster to chdir(abs)
	 * or chdir(../../..).  But, since the this case happens rarely if
	 * the user cares about speed (sorted input will usually mean that
	 * we don't simply want to go back up the tree) it's not worth the
	 * bother. */
	if (cdval == 0 || num < cdval) {
	    /* Except if we have just a couple of files in this directory
	     * to deal with, in which case we'll just use their absolute
	     * path names. */
	    (void)strcpy(prefix_dir, abs);
	    prefix_len = p - abs;
	    return TRUE;
	}

	if (chdir(abs) < 0) {
	    oerrno = errno;
	    (void)fprintf(stderr, "%s: chdir to ", MyName);
	    (void)fflush(stderr);
	    errno = oerrno;
	    perror(abs);
	    /* If we fail here, something is badly broken, since we're
	     * supposedly further down the tree. */
	    err_exit("Chdir failed");
	}
	*q = '\0';
	return TRUE;
    }

    if (*q == '\0' && *p == '/') {
	/* Want to change into a sub-dir of where we were; easy. */
	p++;
	if (cdval == 0 || num < cdval) {
	    (void)strcpy(prefix_dir, p);
	    prefix_len = strlen(p);
	    return TRUE;
	}

	if (chdir(p) < 0) {
	    oerrno = errno;
	    (void)fprintf(stderr, "%s: chdir from %s to ", MyName,
		    cur_dir);
	    (void)fflush(stderr);
	    errno = oerrno;
	    perror(p);
	    if (oerrno == ENOENT)
		return FALSE;
	    err_exit("Chdir failed");
	}
	(void)strcpy(cur_dir, abs);
	return TRUE;
    }

    /* If its possible, (promised we have a pure tree), see if its
     * worth going up the tree with ".." then down again, or if
     * its better to simply start again at the start. */
    if (dotdot) {
	bsegs = slashcount(q);
	/* 1 default "dotdot" here can be 0, 1, 2, or 3, 1 seems
	 * frationally faster than 2, bigger values would require
	 * extending the "../../../" string, but are very unlikely
	 * to be helpful; '0' is the same as not using -u. */
	if (bsegs <= dotdot) {
	    /* Looks like its probably worth using "..". */
	    while (p > abs && *--p != '/')
		continue;
	    p++;
	    (void)strcpy(prefix_dir, DotDot + 9 - bsegs * 3);
	    (void)strcpy(prefix_dir + (bsegs + 1) * 3, p);

	    if (cdval == 0 || num < cdval) {
		prefix_len = strlen(prefix_dir);
		return TRUE;
	    }

	    if (chdir(prefix_dir) < 0) {
		oerrno = errno;
		(void)fprintf(stderr, "%s: chdir from %s to ",
			MyName, cur_dir);
		(void)fflush(stderr);
		errno = oerrno;
		perror(prefix_dir);
		if (oerrno == ENOENT)
		    return FALSE;
		err_exit("Chdir failed");
	    }

	    /* Now patch up curdir to reflect where we are. */
	    while (q > cur_dir && *--q != '/')
		continue;
	    (void)strcpy(q + 1, p);
	    return TRUE;
	}
    }

    /* Simply use the absolute path. */
    if (cdval == 0 || num < cdval) {
	(void)strcpy(prefix_dir, abs);
	prefix_len = strlen(abs);
	return TRUE;
    }
    if (chdir(abs) < 0) {
	oerrno = errno;
	(void)fprintf(stderr, "%s: chdir to ", MyName);
	(void)fflush(stderr);
	errno = oerrno;
	perror(abs);
	if (oerrno == ENOENT)
	    return FALSE;
	err_exit("Chdir failed");
    }

    (void)strcpy(cur_dir, abs);
    return TRUE;
}


STATIC void
unlink_dir(list, num)
    register dnode	*list;
    register int	num;
{
    static dnode	**dptrs;
    static int		ndp;
    register dnode	*l;
    register dnode	**pl;
    register DIR	*dfd;
    register DIRENTRY	*d;
    register BOOL	sorted;
    struct stat		sb;

    if (!setup_dir(list->dir, num)) {
	/* The directory doesn't exist, no point attempting to
	 * delete anything, just forget it all. */
	if (list->dir)
	    DISPOSE(list->dir);
	freelist(list);
	return;
    }

    if (list->dir)
	DISPOSE(list->dir);
    if (sortdirs == 0 || num < sortdirs) {
	if (prefix_len != 0) {
	    prefix_dir[prefix_len++] = '/';
	    prefix_dir[prefix_len] = '\0';
	}
	/*  Easier to just unlink the files than worry about the
	 *  order we unlink them in. */
	while ((l = list) != NULL) {
	    unlink_node(l);
	    list = l->next;
	    if (l->longname)
		DISPOSE(l->longname);
	    DISPOSE(l);
	}
	return;
    }

    if (ndp == 0) {
	ndp = num;
	dptrs = NEW(dnode*, ndp);
    }
    else if (num > ndp) {
	ndp = num + 16;
	RENEW(dptrs, dnode*, ndp);
    }

    if ((pl = dptrs) == NULL)
	err_exit("Out of mem in unlink_dir");

    for (sorted = TRUE, *pl = list, l = list->next; l; l = l->next) {
	if (sorted && strcmp(NODENAME(*pl), NODENAME(l)) > 0)
	    sorted = FALSE;
	*++pl = l;
    }

    if (!sorted)
	qsort((char *)dptrs, num, sizeof (dnode *), comp);

    if (prefix_len == 0) {
	if ((dfd = opendir(".")) == NULL) {
	    (void)fprintf(stderr, "Can't open \".\" in directory \"%s\"\n",
		    cur_dir);
	    fatals++;
	    freelist(list);
	    return;
	}
    }
    else {
	if ((dfd = opendir(prefix_dir)) == NULL) {
	    if (prefix_dir[0] == '/')
		(void)fprintf(stderr, "Can't open directory \"%s\"\n",
			prefix_dir);
	    else
		(void)fprintf(stderr, "Can't open directory \"%s\" in \"%s\"\n",
			prefix_dir, cur_dir);
	    if (stat(prefix_dir, &sb) >= 0 || errno != ENOENT)
		    fatals++;
	    freelist(list);
	    return;
	}
    }

    if (prefix_len != 0) {
	prefix_dir[prefix_len++] = '/';
	prefix_dir[prefix_len] = '\0';
    }

    while ((d = readdir(dfd)) != NULL)
	if ((l = inlist(dptrs, num, d->d_name)) != NULL)
	    unlink_node(l);
    (void)closedir(dfd);
    freelist(list);
}


STATIC BOOL
bad_path(p)
    register char	*p;
{
    while (*p) {
	if (p[0] == '.' && (p[1] == '/' || p[1] == '.' && p[2] == '/'))
	    return TRUE;
	while (*p && *p != '/')
	    p++;
	if (p[0] == '/' && p[1] == '/')
	    return TRUE;
	if (*p == '/')
	    p++;
    }
    return FALSE;
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    register dnode	*list;
    register char	*p;
    register int	oerrno;
    int			count;
    BOOL		empty_error;

    MyName = av[0];
    if ((p = strrchr(MyName, '/')) != NULL)
	MyName = p + 1;
    ONALLLOCFAIL(myexit);
    AmRoot = geteuid() == 0;
    empty_error = FALSE;

    while (ac > 2) {
	if (*(p = av[1]) != '-')
	    break;

	while (*++p) {
	    switch (*p) {
	    default:
		(void)fprintf(stderr, "Usage: %s [ -u -s ] base_dir\n", MyName);
		exit(1);
	    case 'd':
		Debugging = TRUE;
		continue;
	    case 'u':
		dotdot = 1;
		if (!isdigit(p[1]))
		    continue;
		dotdot = atoi(p + 1);
		if (dotdot >= strlen(DotDot)/(SIZE_T)3)
		    dotdot = strlen(DotDot)/(SIZE_T)3 - 1;
		break;
	    case 's':
		sortdirs = 5;
		if (!isdigit(p[1]))
		    continue;
		sortdirs = atoi(p + 1);
		break;
	    case 'c':
		cdval = 1;
		if (!isdigit(p[1]))
		    continue;
		cdval = atoi(p + 1);
		break;
	    case 'e':
		empty_error = TRUE;
		continue;
	    case 'a':
	    case 'r':
		continue;
	    }
	    break;
	}
	ac--;
	av++;
    }

    if (ac != 2) {
	(void)fprintf(stderr, "Usage: %s base_dir\n", MyName);
	exit(1);
    }

    p = av[1];
    if (*p != '/' || bad_path(p) || strlen(p) >= (SIZE_T)MAX_DIR_LEN) {
	(void)fprintf(stderr, "%s: Bad base path: %s\n", MyName, p);
	exit(1);
    }

    (void)strcpy(base_dir, p);
    (void)strcpy(cur_dir, p);

    if (chdir(cur_dir) < 0) {
	oerrno = errno;
	(void)fprintf(stderr, "%s: chdir to base path ", MyName);
	(void)fflush(stderr);
	errno = oerrno;
	perror(cur_dir);
	exit(1);
    }

    while ((list = build_dir(&count)) != NULL) {
	empty_error = FALSE;
	unlink_dir(list, count);
    }
    if (fatals || empty_error)
	exit(1);

    exit(0);
    /* NOTREACHED */
}
