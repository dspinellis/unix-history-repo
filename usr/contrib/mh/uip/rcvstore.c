/* rcvstore.c - incorporate new mail asynchronously
		originally from Julian Onions */

#include "../h/mh.h"
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */

static struct swit switches[] = {
#define CRETSW	0
    "create",	0,
#define NCRETSW	1
    "nocreate", 0,

#define PUBSW	2
    "public",	0,
#define NPUBSW	3
    "nopublic",  0,

#define SEQSW	4
    "sequence name", 0,

#define ZEROSW  5
    "zero",	0,
#define NZEROSW 6
    "nozero",	0,

#define HELPSW  7
    "help", 4,

    NULL, NULL
};

/*  */

extern int  errno;

/*  */

/* ARGSUSED */

main (argc, argv)
int	argc;
char   *argv[];
{
    int     publicsw = -1,
            zerosw = 0,
            msgnum,
            create = 1,
            fd,
            seqp = 0;
    char   *cp,
           *maildir,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *seqs[NATTRS];
    struct msgs *mp;
    struct stat st;

    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
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
		    (void) sprintf (buf, "%s [+folder] [switches]", invo_name);
		    help (buf, switches);
		    done (1);

		case SEQSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument name to %s",
				argp[-2]);
		    if (seqp < NATTRS)
			seqs[seqp++] = cp;
		    else
			adios (NULLCP, "only %d sequences allowed!", NATTRS);
		    continue;
		case PUBSW: 
		    publicsw = 1;
		    continue;
		case NPUBSW: 
		    publicsw = 0;
		    continue;
		case ZEROSW: 
		    zerosw++;
		    continue;
		case NZEROSW: 
		    zerosw = 0;
		    continue;

		case CRETSW: 
		    create++;
		    continue;
		case NCRETSW: 
		    create = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    adios (NULLCP, "usage: %s [+folder] [switches]", invo_name);
    }

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!folder)
	folder = defalt;
    maildir = m_maildir (folder);

    if (stat (maildir, &st) == NOTOK) {
	if (errno != ENOENT)
	    adios (maildir, "error on folder");
	if (!create)
	    adios (NULLCP, "folder %s doesn't exist", maildir);
	if (!makedir (maildir))
	    adios (NULLCP, "unable to create folder %s", maildir);
    }

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);

    (void) signal (SIGHUP, SIG_IGN);
    (void) signal (SIGINT, SIG_IGN);
    (void) signal (SIGQUIT, SIG_IGN);
    (void) signal (SIGTERM, SIG_IGN);

/*  */

    if ((fd = creat (cp = m_scratch ("", invo_name), m_gmprot ())) == NOTOK)
	adios (cp, "unable to create");

    cpydata (fileno (stdin), fd, "standard input", cp);

    if (fstat (fd, &st) == NOTOK) {
	(void) unlink (cp);
	adios (cp, "unable to fstat");
    }
    (void) close (fd);
    if (st.st_size == 0) {
	(void) unlink (cp);
	advise (NULLCP, "empty file");
	done (0);
    }

    msgnum = mp -> hghmsg;
    do {
	msgnum++, mp -> hghmsg++;
	if (msgnum > mp -> hghoff)
	    if ((mp = m_remsg (mp, 0, mp -> hghoff + MAXFOLDER)) == NULL)
		adios (NULLCP, "unable to allocate folder storage");

	mp -> msgstats[msgnum] |= EXISTS | UNSEEN;
	errno = 0;
    } while (link (cp, m_name (msgnum)) == NOTOK && errno == EEXIST);

    (void) unlink (cp);
    if (errno != 0)
	adios (NULLCP, "can't file message %d", msgnum);

    if (mp -> lowmsg == 0)
	mp -> lowmsg = msgnum;
    mp -> msgflags |= SEQMOD;

    seqs[seqp] = NULL;
    for (seqp = 0; seqs[seqp]; seqp++) {
	if (zerosw && !m_seqnew (mp, seqs[seqp], publicsw))
	    done (1);
	if (!m_seqadd (mp, seqs[seqp], msgnum, publicsw))
	    done (1);
    }

    m_setvis (mp, 0);
    m_sync (mp);
    m_update ();

    done (0);
}
