/* dropsbr.c - write to a mailbox */
#ifndef	lint
static char Id[] = "@(#)$Id: dropsbr.c,v 1.3 1993/08/25 17:43:26 jromine Exp $";
#endif

#include <stdio.h>
#ifndef	MMDFONLY
#include "../h/mh.h"
#include "../h/dropsbr.h"
#include "../zotnet/mts.h"
#else	MMDFONLY
#include "dropsbr.h"
#include "strings.h"
#include "mmdfonly.h"
#endif	MMDFONLY
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>


#define	MMDF	1
#define	UUCP	2

/*  */

static	int	mbx_style = MMDF;


extern int  errno;

off_t   lseek ();

/*  */

int     mbx_mmdf () {
    int     style = mbx_style;

    mbx_style = MMDF;
    return style;
}


int     mbx_uucp () {
    int     style = mbx_style;

    mbx_style = UUCP;
    return style;
}

/*  */

int     mbx_open (file, uid, gid, mode)
char   *file;
int     uid,
        gid,
	mode;
{
    int     clear,
            fd;

    if ((fd = mbx_Xopen (file, uid, gid, mode, &clear)) == NOTOK)
	return fd;

    if (!clear)
	switch (mbx_style) {
	    case MMDF: 
	    default: 
		if (mbx_chk (fd) == NOTOK) {
		    (void) close (fd);
		    return NOTOK;
		}
		break;

	    case UUCP: 
		if (lseek (fd, (off_t)0, 2) == (off_t) NOTOK) {
		    (void) close (fd);
		    return NOTOK;
		}
		break;
	}

    return fd;
}

/*  */

int	mbx_Xopen (file, uid, gid, mode, clear)
char   *file;
int	uid,
	gid,
	mode,
       *clear;
{
    register int j;
    int	    count,
            fd;
    struct stat st;

    for (*clear = 0, count = 4, j = 0; count > 0; count--)
	if ((fd = lkopen (file, 6)) == NOTOK)
	    switch (errno) {
		case ENOENT: 
		    if (mbx_create (file, uid, gid, mode) == NOTOK)
			return NOTOK;
		    (*clear)++;
		    break;

#ifdef	BSD42
		case EWOULDBLOCK:
#endif	BSD42
		case ETXTBSY: 
		    j = errno;
		    sleep (5);
		    break;

		default: 
		    return NOTOK;
	    }
	else {
	    *clear = fstat (fd, &st) != NOTOK && st.st_size == 0L;
	    break;
	}

    errno = j;
    return fd;
}

/*  */

static int  mbx_create (file, uid, gid, mode)
char   *file;
int     uid,
        gid,
	mode;
{
    int     fd;

    if ((fd = creat (file, 0600)) == NOTOK)
	return NOTOK;

    (void) close (fd);
    (void) chown (file, uid, gid);
    (void) chmod (file, mode);

    return OK;
}


static int  mbx_chk (fd)
int     fd;
{
    int     count;
    char    ldelim[BUFSIZ];

    count = strlen (mmdlm2);

    if (lseek (fd, (off_t) (-count), 2) == (off_t) NOTOK
	    || read (fd, ldelim, count) != count)
	return NOTOK;
    ldelim[count] = NULL;

    if (strcmp (ldelim, mmdlm2)
	    && write (fd, "\n", 1) != 1
	    && write (fd, mmdlm2, count) != count)
	return NOTOK;

    return OK;
}

/*  */

int	mbx_read (fp, pos, drops, noisy)
register FILE  *fp;
register long	pos;
struct drop **drops;
int	noisy;
{
    register int    len,
                    size;
    long    ld1,
            ld2;
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct drop   *cp,
                           *dp,
                           *ep,
                           *pp;

    pp = (struct drop  *) calloc ((unsigned) (len = MAXFOLDER), sizeof *dp);
    if (pp == NULL) {
	if (noisy)
	    admonish (NULLCP, "unable to allocate drop storage");
	return NOTOK;
    }

    ld1 = (long) strlen (mmdlm1);
    ld2 = (long) strlen (mmdlm2);

    (void) fseek (fp, pos, 0);
    for (ep = (dp = pp) + len - 1; fgets (buffer, sizeof buffer, fp);) {
	size = 0;
	if (strcmp (buffer, mmdlm1) == 0)
	    pos += ld1, dp -> d_start = pos;
	else {
	    dp -> d_start = pos, pos += (long) strlen (buffer);
	    for (bp = buffer; *bp; bp++, size++)
		if (*bp == '\n')
		    size++;
	}

	while (fgets (buffer, sizeof buffer, fp) != NULL)
	    if (strcmp (buffer, mmdlm2) == 0)
		break;
	    else {
		pos += (long) strlen (buffer);
		for (bp = buffer; *bp; bp++, size++)
		    if (*bp == '\n')
			size++;
	    }

	if (dp -> d_start != pos) {
	    dp -> d_id = 0;
	    dp -> d_size = size;
	    dp -> d_stop = pos;
	    dp++;
	}
	pos += ld2;

	if (dp >= ep) {
	    register int    curlen = dp - pp;

	    cp = (struct drop  *) realloc ((char *) pp,
		                    (unsigned) (len += MAXFOLDER) * sizeof *pp);
	    if (cp == NULL) {
		if (noisy)
		    admonish (NULLCP, "unable to allocate drop storage");
		free ((char *) pp);
		return 0;
	    }
	    dp = cp + curlen, ep = (pp = cp) + len - 1;
	}
    }

    if (dp == pp)
	free ((char *) pp);
    else
	*drops = pp;
    return (dp - pp);
}

/*  */

int	mbx_write (mailbox, md, fp, id, last, pos, stop, mapping, noisy)
char   *mailbox;
register FILE *fp;
int     md,
	id,
	mapping,
	noisy;
long	last;
register long	pos,
		stop;
{
    register int    i,
                    j,
                    size;
    register long   start,
                    off;
    register char  *cp;
    char    buffer[BUFSIZ];

    off = (long) lseek (md, (off_t)0, 1);
    j = strlen (mmdlm1);
    if (write (md, mmdlm1, j) != j)
	return NOTOK;
    start = (long) lseek (md, (off_t)0, 1);
    size = 0;

    (void) fseek (fp, pos, 0);
    while (fgets (buffer, sizeof buffer, fp) != NULL && pos < stop) {
	i = strlen (buffer);
	for (j = 0; (j = stringdex (mmdlm1, buffer)) >= 0; buffer[j]++)
	    continue;
	for (j = 0; (j = stringdex (mmdlm2, buffer)) >= 0; buffer[j]++)
	    continue;
	if (write (md, buffer, i) != i)
	    return NOTOK;
	pos += (long) i;
	if (mapping)
	    for (cp = buffer; i-- > 0; size++)
		if (*cp++ == '\n')
		    size++;
    }

    stop = (long) lseek (md, (off_t)0, 1);
    j = strlen (mmdlm2);
    if (write (md, mmdlm2, j) != j)
	return NOTOK;
    if (mapping)
	(void) map_write (mailbox, md, id, last, start, stop, off, size, noisy);

    return OK;
}

/*  */

int     mbx_copy (mailbox, md, fd, mapping, text, noisy)
char   *mailbox;
int     md,
        fd,
	mapping,
	noisy;
char   *text;
{
    register int    i,
                    j,
                    size;
    register long   start,
                    stop,
                    pos;
    register char  *cp;
    char    buffer[BUFSIZ];
    register FILE  *fp;

    pos = (long) lseek (md, (off_t)0, 1);
    size = 0;

    switch (mbx_style) {
	case MMDF: 
	default: 
	    j = strlen (mmdlm1);
	    if (write (md, mmdlm1, j) != j)
		return NOTOK;
	    start = (long) lseek (md, (off_t)0, 1);

	    if (text) {
		i = strlen (text);
		if (write (md, text, i) != i)
		    return NOTOK;
		for (cp = text; *cp++; size++)
		    if (*cp == '\n')
			size++;
	    }
		    
	    while ((i = read (fd, buffer, sizeof buffer)) > 0) {
		for (j = 0;
			(j = stringdex (mmdlm1, buffer)) >= 0;
			buffer[j]++)
		    continue;
		for (j = 0;
			(j = stringdex (mmdlm2, buffer)) >= 0;
			buffer[j]++)
		    continue;
		if (write (md, buffer, i) != i)
		    return NOTOK;
		if (mapping)
		    for (cp = buffer; i-- > 0; size++)
			if (*cp++ == '\n')
			    size++;
	    }

	    stop = (long) lseek (md, (off_t)0, 1);
	    j = strlen (mmdlm2);
	    if (write (md, mmdlm2, j) != j)
		return NOTOK;
	    if (mapping)
		(void) map_write (mailbox, md, 0, 0L, start, stop, pos, size,
			    noisy);

	    return (i != NOTOK ? OK : NOTOK);

	case UUCP: 		/* I hate this... */
	    if ((j = dup (fd)) == NOTOK)
		return NOTOK;
	    if ((fp = fdopen (j, "r")) == NULL) {
		(void) close (j);
		return NOTOK;
	    }
	    start = (long) lseek (md, (off_t)0, 1);

	    if (text) {
		i = strlen (text);
		if (write (md, text, i) != i)
		    return NOTOK;
		for (cp = text; *cp++; size++)
		    if (*cp == '\n')
			size++;
	    }
		    
	    for (j = 0; fgets (buffer, sizeof buffer, fp) != NULL; j++) {
		if (j != 0 && strncmp (buffer, "From ", 5) == 0) {
		    (void) write (fd, ">", 1);
		    size++;
		}
		i = strlen (buffer);
		if (write (md, buffer, i) != i) {
		    (void) fclose (fp);
		    return NOTOK;
		}
		if (mapping)
		    for (cp = buffer; i-- > 0; size++)
			if (*cp++ == '\n')
			    size++;
	    }

	    (void) fclose (fp);
	    (void) lseek (fd, (off_t)0, 2);
	    stop = (long) lseek (md, (off_t)0, 1);
	    if (mapping)
		(void) map_write (mailbox, md, 0, 0L, start, stop, pos, size,
			    noisy);

	    return OK;
    }
}

/*  */

int	mbx_size (md, start, stop)
int	md;
long	start,
	stop;
{
    register int    i,
                    fd;
    register long   pos;
    register FILE  *fp;

    if ((fd = dup (md)) == NOTOK || (fp = fdopen (fd, "r")) == NULL) {
	if (fd != NOTOK)
	    (void) close (fd);
	return NOTOK;
    }

    (void) fseek (fp, start, 0);
    for (i = 0, pos = stop - start; pos-- > 0; i++)
	if (fgetc (fp) == '\n')
	    i++;

    (void) fclose (fp);

    return i;
}

/*  */

int     mbx_close (mailbox, md)
char   *mailbox;
int     md;
{
    (void) lkclose (md, mailbox);

    return OK;
}

/*  */

/* This function is performed implicitly by getbbent.c:

		bb -> bb_map = map_name (bb -> bb_file);
*/

char    *map_name (file)
register char	*file;
{
    register char  *cp,
                   *dp;
    static char buffer[BUFSIZ];

    if ((dp = index (cp = r1bindex (file, '/'), '.')) == NULL)
	dp = cp + strlen (cp);
    if (cp == file)
	(void) sprintf (buffer, ".%.*s%s", dp - cp, cp, ".map");
    else
	(void) sprintf (buffer, "%.*s.%.*s%s", cp - file, file, dp - cp,
		cp, ".map");

    return buffer;
}

/*  */

int	map_read (file, pos, drops, noisy)
char   *file;
long	pos;
struct drop **drops;
int	noisy;
{
    register int    i,
                    md,
                    msgp;
    register char  *cp;
    struct drop d;
    register struct drop   *mp,
                           *dp;

    if ((md = open (cp = map_name (file), 0)) == NOTOK
	    || map_chk (cp, md, mp = &d, pos, noisy)) {
	if (md != NOTOK)
	    (void) close (md);
	return 0;
    }

    msgp = mp -> d_id;
    dp = (struct drop  *) calloc ((unsigned) (msgp + 1), sizeof *dp);
    if (dp == NULL) {
	(void) close (md);
	return 0;
    }

    bcopy ((char *) mp, (char *) dp, sizeof *dp);

    (void) lseek (md, (off_t) sizeof *mp, 0);
    if ((i = read (md, (char *) (dp + 1), msgp * sizeof *dp)) < sizeof *dp) {
	i = 0;
	free ((char *) dp);
    }
    else
	*drops = dp;

    (void) close (md);

    return (i / sizeof *dp);
}

/*  */

int     map_write (mailbox, md, id, last, start, stop, pos, size, noisy)
register char   *mailbox;
int     md,
        id,
	size,
	noisy;
long    last,
	start,
        stop,
        pos;
{
    register int    i;
    int     clear,
            fd,
            td;
    char   *file;
    register struct drop   *dp;
    struct drop    d1,
		   d2,
                  *rp;
    register FILE *fp;

    if ((fd = map_open (file = map_name (mailbox), &clear, md)) == NOTOK)
	return NOTOK;

    if (!clear && map_chk (file, fd, &d1, pos, noisy)) {
	(void) unlink (file);
	(void) mbx_close (file, fd);
	if ((fd = map_open (file, &clear, md)) == NOTOK)
	    return NOTOK;
	clear++;
    }

    if (clear) {
	if ((td = dup (md)) == NOTOK || (fp = fdopen (td, "r")) == NULL) {
	    if (noisy)
		admonish (file, "unable to %s", td != NOTOK ? "fdopen" : "dup");
	    if (td != NOTOK)
		(void) close (td);
	    (void) mbx_close (file, fd);
	    return NOTOK;
	}

	switch (i = mbx_read (fp, 0L, &rp, noisy)) {
	    case NOTOK:
		(void) fclose (fp);
		(void) mbx_close (file, fd);
		return NOTOK;

	    case OK:
		break;

	    default:
		d1.d_id = 0;
		for (dp = rp; i-- >0; dp++) {
		    if (dp -> d_start == start)
			dp -> d_id = id;
		    (void) lseek (fd, (off_t) (++d1.d_id * sizeof *dp), 0);
		    if (write (fd, (char *) dp, sizeof *dp) != sizeof *dp) {
			if (noisy)
			    admonish (file, "write error");
			(void) mbx_close (file, fd);
			(void) fclose (fp);
			return NOTOK;
		    }
		}
		free ((char *) rp);
		break;
	}
    }
    else {
	if (last == 0)
	    last = d1.d_start;
	dp = &d2;
	dp -> d_id = id;
	dp -> d_size = size ? size : mbx_size (fd, start, stop);
	dp -> d_start = start;
	dp -> d_stop = stop;
	(void) lseek (fd, (off_t) (++d1.d_id * sizeof *dp), 0);
	if (write (fd, (char *) dp, sizeof *dp) != sizeof *dp) {
	    if (noisy)
		admonish (file, "write error");
	    (void) mbx_close (file, fd);
	    return NOTOK;
	}
    }

    dp = &d1;
    dp -> d_size = DRVRSN;
    dp -> d_start = last;
    dp -> d_stop = (long) lseek (md, (off_t)0, 1);

    (void) lseek (fd, (off_t)0, 0);
    if (write (fd, (char *) dp, sizeof *dp) != sizeof *dp) {
	if (noisy)
	    admonish (file, "write error");
	(void) mbx_close (file, fd);
	return NOTOK;
    }

    (void) mbx_close (file, fd);

    return OK;
}

/*  */

static int  map_open (file, clear, md)
char   *file;
int    *clear,
	md;
{
    int	    mode;
    struct  stat st;

    mode = fstat (md, &st) != NOTOK ? (int) (st.st_mode & 0777) : m_gmprot ();
    return mbx_Xopen (file, st.st_uid, st.st_gid, mode, clear);
}

/*  */

int  map_chk (file, fd, dp, pos, noisy)
char   *file;
int     fd,
	noisy;
register struct drop *dp;
long	pos;
{
    long    count;
    struct drop d;
    register struct drop    *dl;

    if (read (fd, (char *) dp, sizeof *dp) != sizeof *dp) {
#ifdef	notdef
	admonish (NULLCP, "%s: missing or partial index", file);
#endif	notdef
	return NOTOK;
    }
    
    if (dp -> d_size != DRVRSN) {
	if (noisy)
	    admonish (NULLCP, "%s: version mismatch", file);
	return NOTOK;
    }

    if (dp -> d_stop != pos) {
	if (noisy && pos != 0L)
	    admonish (NULLCP,
		    "%s: pointer mismatch or incomplete index (%ld!=%ld)", 
		    file, dp -> d_stop, pos);
	return NOTOK;
    }

    if ((long) ((dp -> d_id + 1) * sizeof *dp) != (long) lseek (fd, (off_t)0, 2)) {
	if (noisy)
	    admonish (NULLCP, "%s: corrupt index(1)", file);
	return NOTOK;
    }

    dl = &d;
    count = (long) strlen (mmdlm2);
    (void) lseek (fd, (off_t) (dp -> d_id * sizeof *dp), 0);
    if (read (fd, (char *) dl, sizeof *dl) != sizeof *dl
	    || (dl -> d_stop != dp -> d_stop
		&& dl -> d_stop + count != dp -> d_stop)) {
	if (noisy)
	    admonish (NULLCP, "%s: corrupt index(2)", file);
	return NOTOK;
    }

    return OK;
}
