/*  rn -- new readnews program
 *
 *  From: lwall@sdcrdcf.UUCP (Larry Wall)
 *  Organization: System Development Corporation, Santa Monica
 *
 *  begun:   01/14/83
 *	1.0: 04/08/83
 *      2.0: 09/01/83
 */

static char rnid[] = "@(#)$Header: rn.c,v 4.3.1.4 85/09/10 11:05:13 lwall Exp $";

/* $Log:	rn.c,v $
 * Revision 4.3.1.4  85/09/10  11:05:13  lwall
 * Improved %m in in_char().
 * 
 * Revision 4.3.1.3  85/05/16  16:47:10  lwall
 * Catchup confirmation didn't grok -t.
 * 
 * Revision 4.3.1.2  85/05/13  09:34:53  lwall
 * Fixed default after do_newsgroup() returns from Q command.
 * 
 * Revision 4.3.1.1  85/05/10  11:38:08  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:47:56  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "INTERN.h"
#include "common.h"
#include "rn.h"
#include "EXTERN.h"
#include "rcstuff.h"
#include "term.h"
#include "final.h"
#include "ngdata.h"
#include "util.h"
#include "only.h"
#include "ngsrch.h"
#include "help.h"
#include "last.h"
#include "init.h"
#include "intrp.h"
#include "rcln.h"
#include "sw.h"
#include "addng.h"
#include "ng.h"
#include "INTERN.h"

void
rn_init()
{
    ;
}

void
main(argc,argv)
int argc;
char *argv[];
{
    bool foundany = initialize(argc,argv);
    register char *s;
    bool oh_for_the_good_old_days = FALSE;
    
    if (maxngtodo)
	starthere = 0;
    else if (!foundany) {		/* nothing to do? */
#ifdef VERBOSE
	if (verbose)
	    fputs("\
No unread news in subscribed-to newsgroups.  To subscribe to a new\n\
newsgroup use the g<newsgroup> command.\n\
",stdout) FLUSH;
#endif
	starthere = nextrcline;
    }

    /* loop through all unread news */

    {
	char promptbuf[80];
	bool special = FALSE;		/* temporarily allow newsgroup */
					/*   with no unread news? */
	bool retry;			/* cycle back to top of list? */
	NG_NUM recent_ng = 0;
	
	current_ng = 0;
	do {
	    retry = FALSE;
	    if (findlast) {
		findlast = FALSE;
		starthere = 0;
		if (*lastngname) {
		    if ((ng = find_ng(lastngname)) == nextrcline)
			ng = 0;
		    else {
			set_ngname(lastngname);
		    	set_toread(ng);
			if (toread[ng] <= TR_NONE)
			    ng = 0;
		    }
		}
	    }
	    else {
		ng = starthere;
		starthere = 0;
	    }
	    while (ng <= nextrcline) {	/* for each newsgroup */
		mode = 'n';
		if (ng >= nextrcline) {	/* after the last newsgroup? */
		    ng = nextrcline;	/* force it to 1 after */
#ifdef ONLY
		    if (maxngtodo) {
			if (retry)
#ifdef VERBOSE
			    IF(verbose)
				printf("\nRestriction %s%s still in effect.\n",
				    ngtodo[0],
				    maxngtodo > 1 ? ", etc." : nullstr) FLUSH;
			    ELSE
#endif
#ifdef TERSE
				fputs("\n(\"Only\" mode.)\n",stdout) FLUSH;
#endif
			else {
#ifdef VERBOSE
			    IF(verbose)
				fputs("\nNo articles under restriction.",
				  stdout) FLUSH;
			    ELSE
#endif
#ifdef TERSE
				fputs("\nNo \"only\" articles.",stdout) FLUSH;
#endif
			    end_only();	/* release the restriction */
			    retry = TRUE;
			}
		    }
#endif
		    dfltcmd = (retry ? "npq" : "qnp");
#ifdef VERBOSE
		    IF(verbose)
			sprintf(promptbuf,
			    "\n******** End of newsgroups--what next? [%s] ",
			    dfltcmd);
		    ELSE
#endif
#ifdef TERSE
			sprintf(promptbuf,
			    "\n**** End--next? [%s] ", dfltcmd);
#endif
		}
		else {
		    bool shoe_fits;	/* newsgroup matches restriction? */

		    if (toread[ng] >= TR_NONE) {	/* recalc toread? */
			set_ngname(rcline[ng]);
			if (shoe_fits = (special || inlist(ngname)))
			    set_toread(ng);
			if (paranoid) {
			    recent_ng = current_ng;
			    current_ng = ng;
			    cleanup_rc();
					/* this may move newsgroups around */
			    ng = current_ng;
			    set_ngname(rcline[ng]);
			}
		    }
		    if (toread[ng] < (maxngtodo||special ? TR_NONE : TR_ONE) || !shoe_fits) {
					/* unwanted newsgroup? */
			ng++;		/* then skip it */
			continue;
		    }
		    dfltcmd = "ynq";
#ifdef VERBOSE
		    IF(verbose)
			sprintf(promptbuf,
			    "\n******** %3ld unread article%c in %s--read now? [%s] ",
			    (long)toread[ng], (toread[ng]==TR_ONE ? ' ' : 's'),
			    ngname, dfltcmd);	/* format prompt string */
		    ELSE
#endif
#ifdef TERSE
			sprintf(promptbuf,
			    "\n**** %3ld in %s--read? [%s] ",
			    (long)toread[ng],
			    ngname,dfltcmd);	/* format prompt string */
#endif
		}
		special = FALSE;	/* go back to normal mode */
		if (ng != current_ng) {
		    recent_ng = current_ng;
					/* remember previous newsgroup */
		    current_ng = ng;	/* remember current newsgroup */
		}
    reask_newsgroup:
		unflush_output();	/* disable any ^O in effect */
		fputs(promptbuf,stdout) FLUSH;/* print prompt */
		fflush(stdout);
    reinp_newsgroup:
		eat_typeahead();
		getcmd(buf);
		if (errno || *buf == '\f') {
		    putchar('\n') FLUSH; /* if return from stop signal */
		    goto reask_newsgroup;	/* give them a prompt again */
		}
		setdef(buf,dfltcmd);
#ifdef VERIFY
		printcmd();
#endif
		switch (*buf) {
		case 'p':		/* find previous unread newsgroup */
		    do {
			if (ng <= 0)
			    break;
			ng--;
			if (toread[ng] == TR_NONE)
			    set_toread(ng);
		    } while (toread[ng] <= TR_NONE);
		    break;
		case 'P':		/* goto previous newsgroup */
		    do {
			if (ng <= 0)
			    break;
			ng--;
		    } while (toread[ng] < TR_NONE);
		    special = TRUE;	/* don't skip it if toread==0 */
		    break;
		case '-':
		    ng = recent_ng;	/* recall previous newsgroup */
		    special = TRUE;	/* don't skip it if toread==0 */
		    break;
		case 'q': case 'Q': case 'x':	/* quit? */
		    oh_for_the_good_old_days = (*buf == 'x');
		    putchar('\n') FLUSH;
		    ng = nextrcline+1;	/* satisfy */
		    retry = FALSE;	/*   loop conditions */
		    break;
		case '^':
		    putchar('\n') FLUSH;
		    ng = 0;
		    break;
		case 'n': case '+':	/* find next unread newsgroup */
		    if (ng == nextrcline) {
			putchar('\n') FLUSH;
			retry = TRUE;
		    }
		    else if (toread[ng] > TR_NONE)
			retry = TRUE;
		    ng++;
		    break;
		case 'N':		/* goto next newsgroup */
		    ng++;
		    special = TRUE;	/* and don't skip it if toread==0 */
		    break;
		case '1':		/* goto 1st newsgroup */
		    ng = 0;
		    special = TRUE;	/* and don't skip it if toread==0 */
		    break;
		case '$':
		    ng = nextrcline;	/* goto last newsgroup */
		    retry = TRUE;
		    break;
		case 'L':
		    list_newsgroups();
		    goto reask_newsgroup;
		case '/': case '?':	/* scan for newsgroup pattern */
#ifdef NGSEARCH
		    switch (ng_search(buf,TRUE)) {
		    case NGS_ABORT:
			goto reinp_newsgroup;
		    case NGS_INTR:
#ifdef VERBOSE
			IF(verbose)
			    fputs("\n(Interrupted)\n",stdout) FLUSH;
			ELSE
#endif
#ifdef TERSE
			    fputs("\n(Intr)\n",stdout) FLUSH;
#endif
			ng = current_ng;
			goto reask_newsgroup;
		    case NGS_FOUND:
			special = TRUE;	/* don't skip it if toread==0 */
			break;
		    case NGS_NOTFOUND:
#ifdef VERBOSE
			IF(verbose)
			    fputs("\n\nNot found--use g to add newsgroups\n",
				stdout) FLUSH;
			ELSE
#endif
#ifdef TERSE
			    fputs("\n\nNot found\n",stdout) FLUSH;
#endif
			goto reask_newsgroup;
		    }
#else
		    notincl("/");
#endif
		    break;
		case 'm':
#ifndef RELOCATE
		    notincl("m");
		    break;
#endif		    
		case 'g':	/* goto named newsgroup */
		    if (!finish_command(FALSE))
					/* if they didn't finish command */
			goto reinp_newsgroup;	/* go try something else */
		    for (s = buf+1; *s == ' '; s++);
					/* skip leading spaces */
		    if (!*s)
			strcpy(s,ngname);
#ifdef RELOCATE
		    if (!get_ng(s,*buf=='m'))	/* try to find newsgroup */
#else
		    if (!get_ng(s,FALSE))	/* try to find newsgroup */
#endif
			ng = current_ng;/* if not found, go nowhere */
		    special = TRUE;	/* don't skip it if toread==0 */
		    break;
#ifdef DEBUGGING
		case 'D':
		    printf("\nTries: %d Hits: %d\n",
			softtries,softtries-softmisses) FLUSH;
		    goto reask_newsgroup;
#endif
		case '!':		/* shell escape */
		    if (escapade())	 /* do command */
			goto reinp_newsgroup;
					/* if rubbed out, re input */
		    goto reask_newsgroup;
		case Ctl('k'):		/* edit global KILL file */
		    edit_kfile();
		    goto reask_newsgroup;
		case 'c':		/* catch up */
#ifdef CATCHUP
reask_catchup:
#ifdef VERBOSE
		IF(verbose)
		    in_char("\nDo you really want to mark everything as read? [yn] ", 'C');
		ELSE
#endif
#ifdef TERSE
		    in_char("\nReally? [ynh] ", 'C');
#endif
		    putchar('\n') FLUSH;
		    setdef(buf,"y");
		    if (*buf == 'h') {
#ifdef VERBOSE
		    printf("Type y or SP to mark all articles as read.\n");
		    printf("Type n to leave articles marked as they are.\n");
#else
		    printf("y or SP to mark all read.\n");
		    printf("n to forget it.\n");
#endif
			goto reask_catchup;
		    }
		    else if (*buf!=' ' && *buf!='y' && *buf!='n' && *buf!='q') {
			printf(hforhelp);
			settle_down();
			goto reask_catchup;
		    } else if ( (*buf == ' ' || *buf == 'y') && ng<nextrcline )
			catch_up(ng);
		    else
			retry = TRUE;
		    ng++;
#else
		    notincl("c");
#endif
		    break;
		case 'u':		/* unsubscribe */
		    if (ng < nextrcline && toread[ng] >= TR_NONE) {
					/* unsubscribable? */
			printf(unsubto,rcline[ng]) FLUSH;
			rcchar[ng] = NEGCHAR;
					/* unsubscribe to (from?) it */
			toread[ng] = TR_UNSUB;
					/* and make line invisible */
			ng++;		/* do an automatic 'n' */
		    }
		    break;
		case 'h': {		/* help */
		    int cmd;

		    if ((cmd = help_ng()) > 0)
			pushchar(cmd);
		    goto reask_newsgroup;
		}
		case 'a':
#ifndef FINDNEWNG
		    notincl("a");
		    goto reask_newsgroup;
#else
		    /* FALL THROUGH */
#endif
		case 'o':
#ifdef ONLY
		{
#ifdef FINDNEWNG
		    bool doscan = (*buf == 'a');
#endif

		    if (!finish_command(TRUE)) /* get rest of command */
			goto reinp_newsgroup;	/* if rubbed out, try something else */
		    end_only();
		    if (buf[1]) {
			bool minusd = instr(buf+1,"-d") != Nullch;

			sw_list(buf+1);
			if (minusd)
			    cwd_check();
			putchar('\n') FLUSH;
#ifdef FINDNEWNG
			if (doscan && maxngtodo)
			    scanactive();
#endif
		    }
		    ng = 0;		/* simulate ^ */
		    retry = FALSE;
		    break;
		}
#else
		    notincl("o");
		    goto reask_newsgroup;
#endif
		case '&':
		    if (switcheroo()) /* get rest of command */
			goto reinp_newsgroup;	/* if rubbed out, try something else */
		    goto reask_newsgroup;
		case 'l': {		/* list other newsgroups */
		    if (!finish_command(TRUE)) /* get rest of command */
			goto reinp_newsgroup;	/* if rubbed out, try something else */
		    for (s = buf+1; *s == ' '; s++);
		    			/* skip leading spaces */
		    sprintf(cmd_buf,"%s '%s'",filexp(NEWSGROUPS),s);
		    resetty();
		    if (doshell(sh,cmd_buf))
#ifdef VERBOSE
			IF(verbose)
			    fputs("    (Error from newsgroups program)\n",
				stdout) FLUSH;
			ELSE
#endif
#ifdef TERSE
			    fputs("(Error)\n",stdout) FLUSH;
#endif
		    noecho();
		    crmode();
		    goto reask_newsgroup;
		}
		case '.': case '=':
		case 'y': case 'Y': /* do normal thing */
		    if (ng >= nextrcline) {
			fputs("\nNot on a newsgroup.",stdout) FLUSH;
			goto reask_newsgroup;
		    }
		    if (*buf == '=')
			s = savestr("=");
		    else if (*buf == '.') {	/* start command? */
			if (!finish_command(FALSE)) /* get rest of command */
			    goto reinp_newsgroup;
			s = savestr(buf+1);
					/* do_newsgroup will free it */
		    }
		    else
			s = Nullch;
		    if (toread[ng])
			retry = TRUE;
		    switch (do_newsgroup(s)) {
		    case NG_ERROR:
		    case NG_NORM:
			ng++;
			break;
		    case NG_ASK:
			dfltcmd = "ynq";
			goto reask_newsgroup;
		    case NG_MINUS:
			ng = recent_ng;	/* recall previous newsgroup */
			special = TRUE;	/* don't skip it if toread==0 */
			break;
		    }
		    break;
#ifdef STRICTCR
		case '\n':
		    fputs(badcr,stdout) FLUSH;
		    goto reask_newsgroup;
#endif
		case 'v':
		    printf("\n%s\n",rnid) FLUSH;
		    goto reask_newsgroup;
		default:
		    printf("\n%s",hforhelp) FLUSH;
		    settle_down();
		    goto reask_newsgroup;
		}
	    }
	} while (retry);
    }

    /* now write .newsrc back out */

    write_rc();

    if (oh_for_the_good_old_days)
	get_old_rc();

    finalize(0);			/* and exit */
}

/* set current newsgroup */

void
set_ngname(what)
char *what;
{
    int len = strlen(what)+1;

    growstr(&ngname,&ngnlen,len);
    strcpy(ngname,what);
    growstr(&ngdir,&ngdlen,len);
    strcpy(ngdir,getngdir(ngname));
}

static char *myngdir;
static int ngdirlen = 0;

char *
getngdir(ngnam)
char *ngnam;
{
    register char *s;

    growstr(&myngdir,&ngdirlen,strlen(ngnam)+1);
    strcpy(myngdir,ngnam);
    for (s = myngdir; *s; s++)
	if (*s == '.')
	    *s = '/';
    return myngdir;
}

