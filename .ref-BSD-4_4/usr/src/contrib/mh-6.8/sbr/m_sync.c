/* m_sync.c - synchronize message sequences */
#ifndef	lint
static char ident[] = "@(#)$Id: m_sync.c,v 1.8 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>
#ifndef	sigmask
#define	sigmask(s)	(1 << ((s) - 1))
#endif	/* not sigmask */


/* decision logic
    1.  public and folder readonly: make it private
    2a. public: add it to the sequences file
    2b. private: add it to the profile
 */


void m_sync (mp)
register struct msgs *mp;
{
    int     bits;
    register int    i;
    register char  *cp;
    char    flags,
	    attr[BUFSIZ],
	    seq[BUFSIZ * 2];
    register FILE  *fp;
#ifndef	BSD42
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
#else	/* BSD42 */
    int	    smask;
#endif	/* BSD42 */

    if (!(mp -> msgflags & SEQMOD))
	return;
    mp -> msgflags &= ~SEQMOD;

    m_getdefs ();
    (void) sprintf (seq, "%s/%s", mp -> foldpath, mh_seq);
    bits = FFATTRSLOT;
    fp = NULL;

    flags = mp -> msgflags;
    if (mh_seq == NULL || *mh_seq == 0)
	mp -> msgflags |= READONLY;

    for (i = 0; mp -> msgattrs[i]; i++) {
	(void) sprintf (attr, "atr-%s-%s", mp -> msgattrs[i], mp -> foldpath);
	if (mp -> msgflags & READONLY
		|| mp -> attrstats & (1 << (bits + i))) {
    priv: ;
	    if (cp = m_seq (mp, mp -> msgattrs[i]))
		m_replace (attr, cp);
	    else
		(void) m_delete (attr);
	}
	else {
	    (void) m_delete (attr);
	    if ((cp = m_seq (mp, mp -> msgattrs[i])) == NULL)
		continue;
	    if (fp == NULL) {
		if ((fp = fopen (seq, "w")) == NULL
			&& (unlink (seq) == NOTOK ||
			    (fp = fopen (seq, "w")) == NULL)) {
		    admonish (attr, "unable to write");
		    goto priv;
		}
#ifndef	BSD42
		hstat = signal (SIGHUP, SIG_IGN);
		istat = signal (SIGINT, SIG_IGN);
		qstat = signal (SIGQUIT, SIG_IGN);
		tstat = signal (SIGTERM, SIG_IGN);
#else	/* BSD42 */
		smask = sigblock (sigmask (SIGHUP) | sigmask (SIGINT)
				    | sigmask (SIGQUIT) | sigmask (SIGTERM));
#endif	/* BSD42 */
	    }
	    fprintf (fp, "%s: %s\n", mp -> msgattrs[i], cp);
	}
    }

    if (fp) {
	(void) fclose (fp);
#ifndef	BSD42
	(void) signal (SIGHUP, hstat);
	(void) signal (SIGINT, istat);
	(void) signal (SIGQUIT, qstat);
	(void) signal (SIGTERM, tstat);
#else	/* BSD42 */
	(void) sigsetmask (smask);
#endif	/* BSD42 */
    }
    else
	if (!(mp -> msgflags & READONLY))
	    (void) unlink (seq);

    mp -> msgflags = flags;
}
