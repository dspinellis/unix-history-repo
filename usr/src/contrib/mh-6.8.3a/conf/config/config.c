/* config.c - master MH configuration file */
#ifndef	lint
static char ident[] = "@(#)$Id: config.c,v 1.11 1993/09/01 22:29:10 jromine Exp $";
#endif	/* lint */

/* @(MHWARNING) */

/* LINTLIBRARY */

#include "../h/mh.h"
#ifdef	MHRC
#include <pwd.h>
#endif /* MHRC */
#include <stdio.h>


#if	defined(__STDC__)
#define	binpath(file) "@(MHBINPATH)/"#file
#define	etcpath(file) "@(MHETCPATH)/"#file
#else
#define binpath(file) "@(MHBINPATH)/file"
#define etcpath(file) "@(MHETCPATH)/file"
#endif

static char Config[] = "@(#)Config: @(MHCONFIGFILE)";

@(MHCONFIG)


#ifndef	__STDC__
#ifdef	MHRC
#ifdef	SYS5
struct passwd *getpwnam ();
#endif /* SYS5 */
#endif /* MHRC */
#endif

/*  */

static    char lpath[BUFSIZ];

char   *libpath (file)
char   *file;
{
    char   *cp;
#ifdef	MHRC
    char   *pp;
    struct passwd  *pw;
#endif /* MHRC */

#ifdef	MHRC
    m_getdefs ();
#endif /* MHRC */

    switch (*file) {
	case '/': 
	    return file;

#ifdef	MHRC
	case '~': 
	    if (cp = index (pp = file + 1, '/'))
		*cp++ = '\0';
	    if (*pp == '\0')
		pp = mypath;
	    else
		if (pw = getpwnam (pp))
		    pp = pw -> pw_dir;
		else {
		    if (cp)
			*--cp = '/';
		    goto try_it;
		}

	    (void) sprintf (lpath, "%s/%s", pp, cp ? cp : "");
	    if (cp)
		*--cp = '/';

	    if (access (lpath, 04) != NOTOK)
		return lpath;	/* else fall */
    try_it: ;
#endif /* MHRC */

	default: 
	    if (access ((cp = m_mailpath (file)), 04) != NOTOK)
		return cp;
    }

    (void) sprintf (lpath, etcpath (%s), file);
    return (access (lpath, 04) != NOTOK ? lpath : file);
}

/*  */

/* 
 * Standard yes/no switches structure
 */

struct swit anoyes[] = {
    "no", 0,
    "yes", 0,
    NULL, 0
};

/*  */

/* 
 * MH constants
 */

char   *components = "components";
char   *current = "cur";
char   *defalt = "inbox";
char   *digestcomps = "digestcomps";
char   *distcomps = "distcomps";
char   *draft = "draft";
char   *forwcomps = "forwcomps";
char   *inbox = "inbox";
char   *mh_defaults = etcpath (mh.profile);
char   *mh_profile = ".mh_profile";
char   *mhlformat = "mhl.format";
char   *mhlforward = "mhl.forward";
char   *nsequence = "Sequence-Negation";
char   *pfolder = "Current-Folder";
char   *psequence = "Previous-Sequence";
char   *rcvdistcomps = "rcvdistcomps";
char   *replcomps = "replcomps";
char   *usequence = "Unseen-Sequence";
char   *mhlibdir = "@(MHETCPATH)";	/* NB: this will change */


/* 
 * MH not-so constants
 */

char   *context = "context";
#ifndef	NOMHSEQ
char   *mh_seq = ".mh_sequences";
#else /* NOMHSEQ */
char   *mh_seq = NULL;
#endif /* NOMHSEQ */


/* 
 * MH globals
 */

char    ctxflags;		/* status of user's context */

char   *invo_name;		/* pgm invocation name */
char   *mypath;			/* user's $HOME */
char   *defpath;		/* pathname of user's profile */
char   *ctxpath;		/* pathname of user's context */

struct node *m_defs;		/* profile/context structure */

/*  */

/* 
 * MH processes
 */


/*
 * mhl runs this program as a visual-end.
 */

char   *faceproc = NULL;


/*
 * This program is usually called directly by users, but it is
 * also invoked by the post program to process an "fcc".
 */

char   *fileproc = binpath (refile);


/* 
 * This program is called to incorporate messages into a folder.
 */

char   *incproc = binpath (inc);


/*
 * When a user runs an MH program for the first time, this program
 * is called to create his MH profile, and mail directory.
 */

char   *installproc = etcpath (install-mh);


/*
 * This is the program invoked by a "list" response to "What now?"
 *  whereas, showproc is the program invoked by show, next, prev.
 */

#ifndef	MORE
char   *lproc = "/usr/ucb/more";
#else /* MORE */
char   *lproc = MORE;
#endif /* MORE */


/*
 * This is the path for the Bell equivalent mail program.
 */

char   *mailproc = binpath (mhmail);


/*
 * mhl runs this program as a front-end.
 */

#ifndef	MORE
char   *moreproc = "/usr/ucb/more";
#else /* MORE */
char   *moreproc = MORE;
#endif /* MORE */


/* 
 * This program is mhl - the nifty message lister
 */

char	*mhlproc = etcpath (mhl);


/* 
 * This is the super handy BBoard reading program, which is really just the MH
 * shell program
 */

char	*mshproc = binpath (msh);


/* 
 * This program is called to pack a folder.  
 */

char   *packproc = binpath (packf);


/*
 * This is the delivery program called through send to
 * actually deliver mail to users.  This is the interface to
 * the MTS.
 */

#if BERK && SENDMTS && !SMTP
char   *postproc = etcpath (spost);
#else
char   *postproc = etcpath (post);
#endif /* BERK */


/* 
 * This program is called to remove a folder.  
 */

char   *rmfproc = binpath (rmf);


/* 
 * This program is called to remove a message by rmm or refile -nolink.
 * It's usually empty, which means to rename the file to a backup name.
 */

char   *rmmproc = NULL;


/*
 * This program is usually called by the user's whatnowproc, but it
 * may also be called directly to send a message previously composed.
 */

char   *sendproc = binpath (send);


/*
 * This program is called to list messages by the show program.
 * By setting showproc to mhl, the user can run mhl instead.
 */

#ifndef	MORE
char   *showproc = "/usr/ucb/more";
#else /* MORE */
char   *showproc = MORE;
#endif /* MORE */


/* 
 * This program is called under stand-alone MH to deliver a message to
 * a local user.  Under other MTS's it can be used to emulate a
 * MMDF-II .maildelivery mechanism.
 */

char   *slocalproc = etcpath (slocal);


/* 
 * This program is called by vmh as the back-end to the window management
 * protocol
 */

char	*vmhproc = binpath (msh);


/* 
 * This program is called after comp, et. al., have built a draft
 */

char	*whatnowproc = binpath (whatnow);


/* 
 * This program is called to list/validate the addresses in a message.
 */

char	*whomproc = binpath (whom);

/*  */

/*
 * This is the editor invoked by the various message composition
 * programs.  It SHOULD be a 2-D scope editor, such as Rand's ned
 * or Berkeley's ex, but any editor will work.  We use prompter as
 * the default, since with -prepend it works just fine with forw.
 */

char   *sysed = "@(MHEDITOR)";


/* 
 * This is the MH alias file.
 */

char   *AliasFile = etcpath (MailAliases);

/*  */

/* 
 * File protections
 */


/*
 * Folders (directories) are created with this protection (mode)
 */

#ifndef	FOLDPROT
#define	FOLDPROT	"0711"
#endif /* not FOLDPROT */

char   *foldprot = FOLDPROT;


/*
 * Every NEW message will be created with this protection.  When a
 * message is filed it retains its protection, so this only applies
 * to messages coming in through inc.
 */

#ifndef	MSGPROT
#define	MSGPROT		"0644"
#endif /* not MSGPROT */

char   *msgprot = MSGPROT;
