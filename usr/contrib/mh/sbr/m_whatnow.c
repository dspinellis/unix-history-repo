/* m_whatnow.c - exec whatnowproc */

#include "../h/mh.h"
#include <stdio.h>

m_whatnow (ed, nedit, use, file, altmsg, dist, mp, text, inplace, cwd)
struct msgs *mp;
char  *ed,
       *file,
       *altmsg,
       *text,
       *cwd;
int	nedit,
	use,
	dist,
	inplace;
{
    int     found,
            k,
            msgnum,
            vecp;
    register char  *bp;
    char    buffer[BUFSIZ],
           *vec[MAXARGS];

    vecp = 0;
    vec[vecp++] = r1bindex (whatnowproc, '/');
    vec[vecp] = NULL;

    (void) putenv ("mhdraft", file);
    if (mp)
	(void) putenv ("mhfolder", mp -> foldpath);
    else
	(void) unputenv ("mhfolder");
    if (altmsg)
	if (mp == NULL || *altmsg == '/' || cwd == NULL)
	    (void) putenv ("mhaltmsg", altmsg);
	else {
	    (void) sprintf (buffer, "%s/%s", mp -> foldpath, altmsg);
	    (void) putenv ("mhaltmsg", buffer);
	}
    else
	(void) unputenv ("mhaltmsg");
    if (bp = getenv ("mhaltmsg"))/* XXX */
	(void) putenv ("editalt", bp);
    (void) sprintf (buffer, "%d", dist);
    (void) putenv ("mhdist", buffer);
    if (nedit)
	(void) unputenv ("mheditor");
    else
	(void) putenv ("mheditor", ed ? ed : (ed = m_find ("editor")) ? ed : sysed);
    (void) sprintf (buffer, "%d", use);
    (void) putenv ("mhuse", buffer);

    (void) unputenv ("mhmessages");
    (void) unputenv ("mhannotate");
    (void) unputenv ("mhinplace");
    if (text && mp && !(mp -> msgflags & READONLY)) {
	found = 0;
	bp = buffer;
	for (msgnum = mp -> lowmsg; msgnum <= mp -> hghmsg; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		(void) sprintf (bp, "%s%s", found ? " " : "", m_name (msgnum));
		bp += strlen (bp);
		for (k = msgnum + 1;
			k <= mp -> hghmsg && mp -> msgstats[k] & SELECTED;
			k++)
		    continue;
		if (--k > msgnum) {
		    (void) sprintf (bp, "-%s", m_name (k));
		    bp += strlen (bp);
		}
		msgnum = k + 1;
		found++;
	    }
	if (found) {
	    (void) putenv ("mhmessages", buffer);
	    (void) putenv ("mhannotate", text);
	    (void) sprintf (buffer, "%d", inplace);
	    (void) putenv ("mhinplace", buffer);
	}
    }

    m_update ();
    (void) fflush (stdout);

    if (cwd)
	(void) chdir (cwd);

    if (strcmp (vec[0], "whatnow") == 0) {
	WhatNow (vecp, vec);
	done (0);
    }

    execvp (whatnowproc, vec);
    fprintf (stderr, "unable to exec ");
    perror (whatnowproc);

    return 0;
}
