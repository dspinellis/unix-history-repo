/* $Header: kfile.c,v 4.3.1.3 85/05/29 09:11:52 lwall Exp $
 *
 * $Log:	kfile.c,v $
 * Revision 4.3.1.3  85/05/29  09:11:52  lwall
 * Suppressed some killing messages on -t.
 * 
 * Revision 4.3.1.2  85/05/10  14:21:29  lwall
 * Prevented THRU from setting art < absfirst.
 * 
 * Revision 4.3.1.1  85/05/10  11:34:33  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:41:53  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "term.h"
#include "util.h"
#include "artsrch.h"
#include "ng.h"
#include "bits.h"
#include "intrp.h"
#include "ngstuff.h"
#include "rcstuff.h"
#include "rn.h"
#include "INTERN.h"
#include "kfile.h"

static bool exitcmds = FALSE;

void
kfile_init()
{
    ;
}

#ifndef KILLFILES
int
edit_kfile()
{
    notincl("^K");
    return -1;
}

#else KILLFILES

char killglobal[] = KILLGLOBAL;
char killlocal[] = KILLLOCAL;

void
mention(str)
char *str;
{
#ifdef VERBOSE
    IF(verbose) {
#ifdef NOFIREWORKS
	no_sofire();
#endif
	standout();
	fputs(str,stdout);
	un_standout();
	putchar('\n');
    }
    ELSE
#endif
#ifdef TERSE
	putchar('.');
#endif
    fflush(stdout);
}

int
do_kfile(kfp,entering)
FILE *kfp;
int entering;
{
    art = lastart+1;
    fseek(kfp,0L,0);			/* rewind file */
    while (fgets(buf,LBUFLEN,kfp) != Nullch) {
	buf[strlen(buf)-1] = '\0';
	if (strnEQ(buf,"THRU",4)) {
	    ART_NUM tmpart;

	    tmpart = atol(buf+4)+1;
	    if (tmpart < absfirst)
		tmpart = absfirst;
	    check_first(tmpart);
	    firstart = tmpart;
	    continue;
	}
	if (*buf == 'X') {		/* exit command? */
	    if (entering) {
		exitcmds = TRUE;
		continue;
	    }
	    strcpy(buf,buf+1);
	}
	else {
	    if (!entering)
		continue;
	}
	if (*buf == '&') {
	    mention(buf);
	    switcheroo();
	}
	else if (*buf == '/' && firstart <= lastart) {
	    mention(buf);
	    switch (art_search(buf, (sizeof buf), FALSE)) {
	    case SRCH_ABORT:
		continue;
	    case SRCH_INTR:
#ifdef VERBOSE
		IF(verbose)
		    printf("\n(Interrupted at article %ld)\n",(long)art)
		      FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\n(Intr at %ld)\n",(long)art) FLUSH;
#endif
		return -1;
	    case SRCH_DONE:
		break;
	    case SRCH_SUBJDONE:
		fputs("\tsubject not found (???)\n",stdout) FLUSH;
		break;
	    case SRCH_NOTFOUND:
		fputs("\tnot found\n",stdout) FLUSH;
		break;
	    case SRCH_FOUND:
		fputs("\tfound\n",stdout) FLUSH;
	    }
	}
    }
    return 0;
}

void
kill_unwanted(starting,message,entering)
ART_NUM starting;
char *message;
int entering;
{
    bool intr = FALSE;			/* did we get an interrupt? */
    ART_NUM oldfirst;
    bool anytokill = (toread[ng] > 0);

    if (localkfp || globkfp) {
	if (!entering && !exitcmds)
	    return;
	exitcmds = FALSE;
	oldfirst = firstart;
	firstart = starting;
	clear();
#ifdef VERBOSE
	IF(verbose)
	    if (message)
		fputs(message,stdout) FLUSH;
#endif
	if (localkfp)
	    intr = do_kfile(localkfp,entering);
	if (globkfp && !intr)
	    intr = do_kfile(globkfp,entering);
	if (entering && localkfp && !intr)
	    setthru(lastart);
	putchar('\n') FLUSH;
#ifdef VERBOSE
	IF(verbose)
	    if (entering)
		get_anything();
#endif
	if (anytokill)			/* if there was anything to kill */
	    forcelast = FALSE;		/* allow for having killed it all */
	firstart = oldfirst;
    }
}

void
setthru(thru)
ART_NUM thru;
{
    FILE *newkfp;

    fseek(localkfp,0L,0);		/* rewind current file */
    strcpy(buf,filexp(getval("KILLLOCAL",killlocal)));
    UNLINK(buf);			/* to prevent file reuse */
    if (newkfp = fopen(buf,"w")) {
	fprintf(newkfp,"THRU %ld\n",(long)thru);
	while (fgets(buf,LBUFLEN,localkfp) != Nullch) {
	    if (strnEQ(buf,"THRU",4))
		continue;
	    fputs(buf,newkfp);
	}
	fclose(newkfp);
	open_kfile(KF_LOCAL);		/* and reopen local file */
    }
    else
	printf(cantcreate,buf) FLUSH;
}

/* edit KILL file for newsgroup */

int
edit_kfile()
{
    int r = -1;

    if (in_ng)
	strcpy(buf,filexp(getval("KILLLOCAL",killlocal)));
    else
	strcpy(buf,filexp(getval("KILLGLOBAL",killglobal)));
    if ((r = makedir(buf,MD_FILE)) >= 0) {
	sprintf(cmd_buf,"%s %s",
	    filexp(getval("VISUAL",getval("EDITOR",defeditor))),buf);
	printf("\nEditing %s KILL file:\n%s\n",
	    (in_ng?"local":"global"),cmd_buf) FLUSH;
	resetty();			/* make sure tty is friendly */
	r = doshell(sh,cmd_buf);/* invoke the shell */
	noecho();			/* and make terminal */
	crmode();			/*   unfriendly again */
	open_kfile(in_ng);
    }
    else
	printf("Can't make %s\n",buf) FLUSH;
    return r;
}

void
open_kfile(local)
int local;
{
    char *kname = filexp(local ?
	getval("KILLLOCAL",killlocal) :
	getval("KILLGLOBAL",killglobal)
	);
    
    stat(kname,&filestat);
    if (!filestat.st_size)		/* nothing in the file? */
	UNLINK(kname);			/* delete the file */
    if (local) {
	if (localkfp)
	    fclose(localkfp);
	localkfp = fopen(kname,"r");
    }
    else {
	if (globkfp)
	    fclose(globkfp);
	globkfp = fopen(kname,"r");
    }
}

void
kf_append(cmd)
char *cmd;
{
    strcpy(cmd_buf,filexp(getval("KILLLOCAL",killlocal)));
    if (makedir(cmd_buf,MD_FILE) >= 0) {
#ifdef VERBOSE
	IF(verbose)
	    printf("\nDepositing command in %s...",cmd_buf);
	ELSE
#endif
#ifdef TERSE
	    printf("\n--> %s...",cmd_buf);
#endif
	fflush(stdout);
	sleep(2);
	if ((tmpfp = fopen(cmd_buf,"a")) != Nullfp) {
	    fseek(tmpfp,0L,2);		/* get to EOF for sure */
	    fprintf(tmpfp,"%s\n",cmd);
	    fclose(tmpfp);
	    fputs("done\n",stdout) FLUSH;
	}
	else
	    printf(cantopen,cmd_buf) FLUSH;
    }
}
#endif KILLFILES
