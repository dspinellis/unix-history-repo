/* comp.c - compose a message */
#ifndef	lint
static char ident[] = "@(#)$Id: comp.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	DFOLDSW	0
    "draftfolder +folder", 0,
#define	DMSGSW	1
    "draftmessage msg", 0,
#define	NDFLDSW	2
    "nodraftfolder", 0,

#define	EDITRSW	3
    "editor editor", 0,
#define	NEDITSW	4
    "noedit", 0,

#define	FILESW	5
    "file file", 0,
#define	FORMSW	6
    "form formfile", 0,

#define	USESW	7
    "use", 0,
#define	NUSESW	8
    "nouse", 0,

#define	WHATSW	9
    "whatnowproc program", 0,
#define	NWHATSW	10
    "nowhatnowproc", 0,

#define	HELPSW	11
    "help", 4,


    NULL, 0
};

/*  */

static struct swit aqrunl[] = {
#define	NOSW	0
    "quit", 0,
#define	YESW	1
    "replace", 0,
#define	USELSW	2
    "use", 0,
#define	LISTDSW	3
    "list", 0,
#define	REFILSW	4
    "refile +folder", 0,
#define NEWSW	5
    "new", 0,

    NULL, 0
};


static struct swit aqrul[] = {
    "quit", 0,
    "replace", 0,
    "use", 0,
    "list", 0,
    "refile", 0,

    NULL, 0
};

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     use = NOUSE,
            nedit = 0,
	    nwhat = 0,
            i,
            in,
	    isdf = 0,
            out;
    char   *cp,
           *cwd,
           *maildir,
           *dfolder = NULL,
           *ed = NULL,
           *file = NULL,
           *form = NULL,
           *folder = NULL,
           *msg = NULL,
            buf[BUFSIZ],
            drft[BUFSIZ],
          **ap,
          **argp,
           *arguments[MAXARGS];
    struct msgs *mp = NULL;
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
		    (void) sprintf (buf, "%s [+folder] [msg] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case EDITRSW: 
		    if (!(ed = *argp++) || *ed == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    nedit = 0;
		    continue;
		case NEDITSW: 
		    nedit++;
		    continue;

		case WHATSW: 
		    if (!(whatnowproc = *argp++) || *whatnowproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    nwhat = 0;
		    continue;
		case NWHATSW: 
		    nwhat++;
		    continue;

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case USESW: 
		    use++;
		    continue;
		case NUSESW: 
		    use = NOUSE;
		    continue;

		case FILESW: 	/* compatibility */
		    if (file)
			adios (NULLCP, "only one file at a time!");
		    if (!(file = *argp++) || *file == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    isdf = NOTOK;
		    continue;

		case DFOLDSW: 
		    if (dfolder)
			adios (NULLCP, "only one draft folder at a time!");
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    dfolder = path (*cp == '+' || *cp == '@' ? cp + 1 : cp,
			    *cp != '@' ? TFOLDER : TSUBCWF);
		    continue;
		case DMSGSW: 
		    if (file)
			adios (NULLCP, "only one draft message at a time!");
		    if (!(file = *argp++) || *file == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NDFLDSW: 
		    dfolder = NULL;
		    isdf = NOTOK;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    if (msg)
		adios (NULLCP, "only one message at a time!");
	    else
		msg = cp;
    }

/*  */

    cwd = getcpy (pwd ());

    if (!m_find ("path"))
	free (path ("./", TFOLDER));

    if ((dfolder || m_find ("Draft-Folder")) && !folder && msg && !file)
	file = msg, msg = NULL;
    if (form && (folder || msg))
	    adios (NULLCP, "can't mix forms and folders/msgs");

    if (folder || msg) {
	if (!msg)
	    msg = "cur";
	if (!folder)
	    folder = m_getfolder ();
	maildir = m_maildir (folder);

	if (chdir (maildir) == NOTOK)
	    adios (maildir, "unable to change directory to");
	if (!(mp = m_gmsg (folder)))
	    adios (NULLCP, "unable to read folder %s", folder);
	if (mp -> hghmsg == 0)
	    adios (NULLCP, "no messages in %s", folder);

	if (!m_convert (mp, msg))
	    done (1);
	m_setseq (mp);

	if (mp -> numsel > 1)
	    adios (NULLCP, "only one message at a time!");

	if ((in = open (form = getcpy (m_name (mp -> lowsel)), 0)) == NOTOK)
	    adios (form, "unable to open message");
    }
    else
	if (form) {
	    if ((in = open (libpath (form), 0)) == NOTOK)
		adios (form, "unable to open form file");
	}
	else {
	    if ((in = open (libpath (components), 0)) == NOTOK)
		adios (components, "unable to open default components file");
	    form = components;
	}

/*  */

try_it_again: ;
    (void) strcpy (drft, m_draft (dfolder, file, use, &isdf));
    if ((out = open (drft, 0)) != NOTOK) {
	i = fdcompare (in, out);
	(void) close (out);
	if (use || i)
	    goto edit_it;

	if (stat (drft, &st) == NOTOK)
	    adios (drft, "unable to stat");
	printf ("Draft \"%s\" exists (%ld bytes).", drft, st.st_size);
	for (i = LISTDSW; i != YESW;) {
	    if (!(argp = getans ("\nDisposition? ", isdf ? aqrunl : aqrul)))
		done (1);
	    switch (i = smatch (*argp, isdf ? aqrunl : aqrul)) {
		case NOSW: 
		    done (0);
		case NEWSW: 
		    file = NULL;
		    use = NOUSE;
		    goto try_it_again;
		case YESW: 
		    break;
		case USELSW:
		    use++;
		    goto edit_it;
		case LISTDSW: 
		    (void) showfile (++argp, drft);
		    break;
		case REFILSW: 
		    if (refile (++argp, drft) == 0)
			i = YESW;
		    break;
		default: 
		    advise (NULLCP, "say what?");
		    break;
	    }
	}
    }
    else
	if (use)
	    adios (drft, "unable to open");

    if ((out = creat (drft, m_gmprot ())) == NOTOK)
	adios (drft, "unable to create");
    cpydata (in, out, form, drft);
    (void) close (in);
    (void) close (out);

edit_it: ;
    m_update ();

    if (nwhat)
	done (0);
    (void) what_now (ed, nedit, use, drft, NULLCP, 0, NULLMP, NULLCP, 0, cwd);
    done (1);
}
