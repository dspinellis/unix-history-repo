/* m_draft.c - construct the draft name */
#ifndef	lint
static char ident[] = "@(#)$Id: m_draft.c,v 1.2 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>


extern int errno;


char   *m_draft (folder, msg, use, isdf)
register char  *folder,
               *msg;
register int    use,
               *isdf;
{
    register char  *cp;
    register struct msgs   *mp;
    struct stat st;
    static char buffer[BUFSIZ];

    if (*isdf == NOTOK || folder == NULL || *folder == 0) {
	if (*isdf == NOTOK || (cp = m_find ("Draft-Folder")) == NULL) {
	    *isdf = 0;
	    return m_maildir (msg && *msg ? msg : draft);
	}
	else
	    folder = path (*cp == '+' || *cp == '@' ? cp + 1 : cp,
		    *cp != '@' ? TFOLDER : TSUBCWF);
    }
    *isdf = 1;
    
    (void) chdir (m_maildir (""));
    (void) strcpy (buffer, m_maildir (folder));
    if (stat (buffer, &st) == NOTOK) {
	if (errno != ENOENT)
	    adios (buffer, "error on folder");
	cp = concat ("Create folder \"", buffer, "\"? ", NULLCP);
	if (!getanswer (cp))
	    done (0);
	free (cp);
	if (!makedir (buffer))
	    adios (NULLCP, "unable to create folder %s", buffer);
    }

    if (chdir (buffer) == NOTOK)
	adios (buffer, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);

    if ((mp = m_remsg (mp, 0, MAXFOLDER)) == NULL)
	adios (NULLCP, "unable to allocate folder storage");
    mp -> msgflags |= MHPATH;

    if (!m_convert (mp, msg && *msg ? msg : use ? "cur" : "new"))
	done (1);
    m_setseq (mp);
    if (mp -> numsel > 1)
	adios (NULLCP, "only one message draft at a time!");

    (void) sprintf (buffer, "%s/%s", mp -> foldpath, m_name (mp -> lowsel));
    cp = buffer;

    m_setcur (mp, mp -> lowsel);
    m_sync (mp);
    m_fmsg (mp);

    return cp;
}
