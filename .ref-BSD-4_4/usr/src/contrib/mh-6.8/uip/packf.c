/* packf.c - pack a folder (used to be called "pack") */
#ifndef lint
static char ident[] = "@(#)$Id: packf.c,v 1.4 1992/12/15 00:20:22 jromine Exp $";
#endif	/*  lint */

#include "../h/mh.h"
#include "../h/dropsbr.h"
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define FILESW	0
    "file name", 0,

#define	HELPSW	1
    "help", 4,

    NULL, 0
};

/*  */

extern int errno;


static int  md = NOTOK;

char   *file = NULL;

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     msgp = 0,
            fd,
            msgnum;
    char   *cp,
           *maildir,
           *msgnam,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct msgs *mp;
    struct stat st;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [+folder] [msgs] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case FILESW: 
		    if (file)
			adios (NULLCP, "only one file at a time!");
		    if (!(file = *argp++) || *file == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    msgs[msgp++] = cp;
    }

/*  */

    if (!file)
	file = "./msgbox";
    file = path (file, TFILE);
    if (stat (file, &st) == NOTOK) {
	if (errno != ENOENT)
	    adios (file, "error on file");
	cp = concat ("Create file \"", file, "\"? ", NULLCP);
	if (!getanswer (cp))
	    done (1);
	free (cp);
    }

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!msgp)
	msgs[msgp++] = "all";
    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to ");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp -> hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    m_setseq (mp);

    if ((md = mbx_open (file, getuid (), getgid (), m_gmprot ())) == NOTOK)
	adios (file, "unable to open");

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if ((fd = open (msgnam = m_name (msgnum), 0)) == NOTOK) {
		admonish (msgnam, "unable to read message");
		break;
	    }

	    if (mbx_copy (file, md, fd, 1, NULLCP, 1) == NOTOK)
		adios (file, "error writing to file");

	    (void) close (fd);
	}
    (void) mbx_close (file, md);

    m_replace (pfolder, folder);
    if (mp -> hghsel != mp -> curmsg)
	m_setcur (mp, mp -> lowsel);
    m_sync (mp);
    m_update ();

    done (0);
}

/*  */

void done (status)
int status;
{
    (void) mbx_close (file, md);

    exit (status);
}
