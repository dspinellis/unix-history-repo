/* m_gmsg.c - read a folder */

#include "../h/mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef	BSD42
#ifndef SYS5
#include <ndir.h>
#else	SYS5
#include <dir.h>
#endif  SYS5
#else	BSD42
#include <sys/dir.h>
#endif	BSD42


#define	NINFO	(MAXFOLDER / 5)	/* PLEASE be non-trivial... */
struct info {
    int     msgno;
    short   stats;
    char    pad[sizeof (int) - sizeof (short)];
};

static int  len;
static struct info *head;

/*  */

struct msgs *m_gmsg (name)
register char   *name;
{
#ifdef	COMPAT
    register int    cur,
		    fd;
#endif	COMPAT
    register int    i,
		    j;
    register struct info *rover,
			 *tail;
#ifdef	COMPAT
    register char  *cp;
    char    buffer[BUFSIZ];
#endif	COMPAT
    register struct msgs   *mp;
    register struct direct *dp;
    register    DIR * dd;
    struct stat st;

    if ((dd = opendir (name = m_mailpath (name))) == NULL) {
	free (name);
	return NULL;
    }
    (void) fstat (dd -> dd_fd, &st);

    mp = (struct msgs  *) malloc (MSIZE (mp, 0, 0));
    if (mp == NULL)
	adios (NULLCP, "unable to allocate folder storage");
    mp -> lowmsg = mp -> hghmsg = mp -> nummsg = 0;
    mp -> curmsg = 0;
    mp -> lowsel = mp -> hghsel = mp -> numsel = 0;
    mp -> foldpath = name;
    mp -> msgflags = NULL;
    if (st.st_uid != getuid () || access (name, 02) == NOTOK)
	mp -> msgflags |= READONLY;
#ifdef	COMPAT
    cur = 0;
#endif	COMPAT
    j = strlen (SBACKUP);
    if (head == NULL)
	if ((head = (struct info *)
		malloc ((unsigned) ((len = NINFO) * sizeof *head))) == NULL)
	    adios (NULLCP, "unable to allocate info storage");
    tail = (rover = head) + len;

    while (dp = readdir (dd))
	if (i = m_atoi (dp -> d_name)) {
	    if (rover >= tail) {
		register int curlen = tail - head;

		if ((tail = (struct info *) realloc ((char *) head,
			     (unsigned) ((len += NINFO) * sizeof *head)))
			== NULL)
		    adios (NULLCP, "unable to allocate info storage");
		else
		    rover = tail + curlen, head = tail, tail += len;
	    }
	    if (i > mp -> hghmsg)
		mp -> hghmsg = i;
	    mp -> nummsg++;
	    if (mp -> lowmsg == 0 || i < mp -> lowmsg)
		mp -> lowmsg = i;
	    rover -> msgno = i;
	    rover -> stats = EXISTS;
#ifdef	notdef
	    rover -> stats &= ~DELETED;
#endif	notdef
	    rover++;
	}
	else
	    switch (dp -> d_name[0]) {
		case '.': 
		    continue;

		case ',': 
#ifdef	notdef
		    if ((i = m_atoi (dp -> d_name + 1)) {
			register struct info *l;

			for (l = head; l < rover; l++)
			    if (l -> msgno == i) {
				if (!(l -> stats & EXISTS))
				    l -> stats |= DELETED;
				break;
			    }
		    }
#endif	notdef
		    continue;

#ifdef	MHE
		case '+': 
		    continue;
#endif	MHE

#ifdef	UCI
		case '_': 
		case '#': 
		    continue;
#endif	UCI

		default: 
#ifdef	COMPAT
		    if (strcmp (dp -> d_name, current) == 0) {
			cur++;
			continue;
		    }
#endif	COMPAT
		    if (strcmp (dp -> d_name, LINK) == 0
			    || strncmp (dp -> d_name, SBACKUP, j) == 0)
			continue;
		    mp -> msgflags |= OTHERS;
		    continue;
	    }

    closedir (dd);

/*  */

#ifdef	COMPAT
    (void) sprintf (buffer, "%s-%s", current, name);
    if (cp = m_find (buffer)) {
	i = m_atoi (cp);
	(void) m_delete(buffer);
	if (i > 0)
	    mp -> curmsg = i;
    }
    if (mp -> curmsg == 0 && cur && (fd = open (current, 0)) != NOTOK) {
	    if ((i = read (fd, buffer, sizeof buffer)) > 0) {
		if (cp = index (buffer, '\n'))
		    *cp = NULL;
		if ((i = m_atoi (buffer)) > 0)
		    mp -> curmsg = i;
	    }
	    (void) close (fd);
	}
    if (cur && !(mp -> msgflags & READONLY)){	/* sneaky... */
	(void) sprintf (buffer, "%s/%s", name, current);
	(void) unlink (buffer);
    }
#endif	COMPAT

#ifndef	MTR
    mp -> lowoff = 1;
#else	MTR
    mp -> lowoff = mp -> lowmsg;
#endif	MTR
    mp -> hghoff = mp -> hghmsg + 1;/* for "new" in m_convert */

    mp = (struct msgs  *)
		realloc ((char *) mp, MSIZE (mp, mp -> lowoff, mp -> hghoff));
    if (mp == NULL)
	adios (NULLCP, "unable to allocate folder storage");
#ifndef	MTR
    for (i = mp -> lowmsg; i <= mp -> hghmsg; i++)
	mp -> msgstats[i] = 0;
#else	MTR
    mp -> msgstats = (short *)
		calloc ((unsigned) 1, MSIZEX (mp, mp -> lowmsg, mp -> hghmsg));
    if (mp -> msgstats == NULL)
	adios (NULLCP, "unable to allocate messages storage");
    mp -> msgstats = (mp -> msgbase = mp -> msgstats) - mp -> lowoff;
    if (mp -> msgstats < 0)
	adios (NULLCP, "m_gmsg() botch -- you lose big");
#endif	MTR
    for (tail = head; tail < rover; tail++)
	mp -> msgstats[tail -> msgno] = tail -> stats;
    m_getatr (mp);

    return mp;
}

/*  */

static m_getatr (mp)
register struct msgs *mp;
{
    int     alen,
            bits,
            i,
            j,
            plen,
            state;
    register char  *cp;
    char    name[NAMESZ],
            field[BUFSIZ];
    register struct node   *np;
    register    FILE * fp;

    bits = FFATTRSLOT;

    mp -> msgattrs[i = 0] = getcpy (current);
    mp -> msgattrs[++i] = NULL;
    mp -> attrstats = 0;	/* initially, all public */

    m_getdefs ();
    if (mh_seq == NULL || *mh_seq == NULL)
	goto private_only;

    (void) sprintf (field, "%s/%s", mp -> foldpath, mh_seq);
    if (fp = fopen (field, "r")) {
	for (state = FLD;;) {
	    switch (state = m_getfld (state, name, field, sizeof field, fp)) {
		case FLD: 
		case FLDEOF: 
		    (void) m_setatr (mp, getcpy (name), trimcpy (field));
		    if (state == FLDEOF)
			break;
		    continue;

		case BODY: 
		case BODYEOF: 
		    adios (NULLCP,
			    "no blank lines are permitted in %s/%s",
			    mp -> foldpath, mh_seq);/* fall */

		case FILEEOF:
		    break;

		default: 
		    adios (NULLCP, "%s/%s is poorly formatted",
			    mp -> foldpath, mh_seq);
	    }
	    break;
	}
	(void) fclose (fp);
    }

private_only: ;
    alen = strlen ("atr-");
    plen = strlen (mp -> foldpath) + 1;

    for (np = m_defs; np; np = np -> n_next)
	if (ssequal ("atr-", np -> n_name)
		&& (j = strlen (np -> n_name) - plen) > alen
		&& *(np -> n_name + j) == '-'
		&& strcmp (mp -> foldpath, np -> n_name + j + 1) == 0) {
	    cp = getcpy (np -> n_name + alen);
	    *(cp + j - alen) = NULL;
	    if ((i = m_setatr (mp, cp, getcpy (np -> n_field))) != NOTOK)
		mp -> attrstats |= 1 << (bits + i);/* private */
	}
}

/*  */

static int  m_setatr (mp, name, field)
register struct msgs *mp;
register char   *name,
		*field;
{
    int     bits,
            hack;
    register int    i,
		    j,
		    k;
    register char  *cp,
                  **ap;

    bits = FFATTRSLOT;
    hack = strcmp (current, name) == 0;/* hack... */

    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], name) == 0) {
	    for (j = mp -> lowmsg; j <= mp -> hghmsg; j++)
		mp -> msgstats[j] &= ~(1 << (bits + i));
	    break;
	}
    if (i >= NATTRS) {
	free (name);
	free (field);
	return NOTOK;
    }

    if (mp -> msgattrs[i] == NULL) {
	mp -> msgattrs[i] = name;
	mp -> msgattrs[i + 1] = NULL;
    }
    else
	free (name);

    for (ap = brkstring (field, " ", "\n");
	    *ap;
	    ap++) {
	if (cp = index (*ap, '-'))
	    *cp++ = NULL;
	if ((j = m_atoi (*ap)) > 0) {
#ifdef	notdef
	    if (hack && j >= mp -> lowmsg && j <= mp -> hghmsg
		    && (mp -> msgstats[j] & EXISTS))
		mp -> curmsg = j;
#else	not notdef
	    if (hack)
		mp -> curmsg = j;
#endif	not notdef
	    for (k = cp ? m_atoi (cp) : j; j <= k; j++)
		if (j >= mp -> lowmsg && j <= mp -> hghmsg
			&& (mp -> msgstats[j] & EXISTS))
		    mp -> msgstats[j] |= 1 << (bits + i);
	}
    }
    free (field);

    return i;
}
