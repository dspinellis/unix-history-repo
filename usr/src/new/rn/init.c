/* $Header: init.c,v 4.3.1.4 86/09/05 14:24:02 lwall Exp $
 *
 * $Log:	init.c,v $
 * Revision 4.3.1.4  86/09/05  14:24:02  lwall
 * Removed net.announce dependency.
 * 
 * Revision 4.3.1.3  85/07/23  18:08:36  lwall
 * Fixed up NOLINEBUF option to work.
 * 
 * Revision 4.3.1.2  85/05/21  14:22:46  lwall
 * Sped up "rn -c" by avoiding unnecessary initialization.
 * 
 * Revision 4.3.1.1  85/05/10  11:33:39  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  16:16:13  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "final.h"
#include "term.h"
#include "last.h"
#include "rn.h"
#include "rcstuff.h"
#include "ngdata.h"
#include "only.h"
#include "intrp.h"
#include "addng.h"
#include "sw.h"
#include "art.h"
#include "artsrch.h"
#include "artio.h"
#include "backpage.h"
#include "bits.h"
#include "cheat.h"
#include "head.h"
#include "help.h"
#include "kfile.h"
#include "ngsrch.h"
#include "ngstuff.h"
#include "rcln.h"
#include "respond.h"
#include "ng.h"
#include "INTERN.h"
#include "init.h"

bool
initialize(argc,argv)
int argc;
char *argv[];
{
    char *tcbuf;
    register bool foundany = FALSE;
    long time();
#ifdef NOLINEBUF
    static char std_out_buf[BUFSIZ];	/* must be static or malloced */

    setbuf(stdout, std_out_buf);
#endif

    tcbuf = safemalloc(1024);		/* make temp buffer for termcap and */
					/* other initialization stuff */
    
    /* init terminal */
    
    term_init();			/* must precede sw_init() so that */
					/* ospeed is set for baud-rate */
					/* switches.  Actually terminal */
					/* mode setting is in term_set() */

    /* we have to know rnlib to look up global switches in %X/INIT */

    lib = savestr(filexp(LIB));
    rnlib = savestr(filexp(RNLIB));

    /* decode switches */

    sw_init(argc,argv,&tcbuf);          /* must not do % interps! */
					/* (but may mung environment) */

    /* init signals, status flags */

    final_init();
    
    /* start up file expansion and the % interpreter */

    intrp_init(tcbuf);
    
    /* now make sure we have a current working directory */

    if (!checkflag)
	cwd_check();
    
    /* now that we know where to save things, cd to news directory */

    if (chdir(spool)) {
	printf(nocd,spool) FLUSH;
	finalize(1);
    }

    /* if we aren't just checking, turn off echo */

    if (!checkflag)
	term_set(tcbuf);

    /* get info on last rn run, if any */

    if (!checkflag)
	last_init(tcbuf);

    free(tcbuf);			/* recover 1024 bytes */

    /* make sure we are the sole possessors of .newsrc */

    if (!checkflag)
	lock_check();

    /* check for news news */

    if (!checkflag)
	newsnews_check();

    /* open active file, etc. */

    ngdata_init();

    /* now read in the .newsrc file */

    foundany = rcstuff_init();

    /* it looks like we will actually read something, so init everything */

    addng_init();
    art_init();
    artio_init();
    artsrch_init();
    backpage_init();
    bits_init();
    cheat_init();
/*  final_init();	already done */
    head_init();
    help_init();
/*  intrp_init();      already done */
    kfile_init();
/*  last_init();	already done */
    ng_init();
/*  ngdata_init();	already done */
    ngsrch_init();
    ngstuff_init();
    only_init();
    rcln_init();
/*  rcstuff_init();	already done */
    respond_init();
    rn_init();
    search_init();
/*  sw_init();      already done */
/*  term_init();	already done */
    util_init();

#ifdef FINDNEWNG
    fstat(actfp->_file,&filestat);	/* did active file grow? */
    if (filestat.st_size != lastactsiz) {
	long actsiz = filestat.st_size;	/* remember new size */
	NG_NUM oldnext = nextrcline;	/* remember # lines in newsrc */
#ifdef FASTNEW
	bool munged = writesoft || !lastactsiz;
					/* bad soft ptrs -> edited active */
#else
	bool munged = TRUE;		/* just assume .newsrc munged */
#endif

#ifdef VERBOSE
	IF(verbose)
	    fputs("\nChecking active list for new newsgroups...\n",stdout)
	      FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("\nNew newsgroups:\n",stdout) FLUSH;
#endif
#ifdef FASTNEW
	if (!munged) {			/* maybe just do tail of file? */
	    fseek(actfp,lastactsiz-1,0);
	    fgets(buf,LBUFLEN,actfp);
	    munged = (*buf != '\n');
	    if (!munged)
		munged = newlist(munged,FALSE);
	}
#endif
	if (munged) {			/* must we scan entire file? */
	    fseek(actfp,0L,0);		/* rewind active file */
	    newlist(munged,FALSE);      /* sure hope they use hashing... */
	}
	lastactsiz = actsiz;		/* remember for .rnlast */
	if (nextrcline != oldnext) {	/* did we add any new groups? */
	    foundany = TRUE;		/* let main() know */
	    starthere = 0;              /* and start ng scan from the top */
	}
    }
#endif
    time(&lasttime);			/* remember when we inited-- */
					/* ends up back in .rnlast */
    writelast();                       /* in fact, put it there now */
    
#ifdef FINDNEWNG
# ifdef ONLY
    if (maxngtodo)			/* patterns on command line? */
	foundany |= scanactive();
# endif
#endif

    return foundany;
}

/* make sure there is no rn out there already */

void
lock_check()
{
    lockname = savestr(filexp(LOCKNAME));
    if (!checkflag) {
	tmpfp = fopen(lockname,"r");
	if (tmpfp != Nullfp) {
	    int processnum;
    
	    fgets(buf,LBUFLEN,tmpfp);
	    fclose(tmpfp);
	    processnum = atoi(buf);
#ifdef VERBOSE
	    IF(verbose)
		printf("You seem to have left an rn running, process %d.\n",
		    processnum) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("Rn left running, #%d.\n", processnum) FLUSH;
#endif
	    if (kill(processnum, SIGEMT)) {
				    /* does process not exist? */
				    /* (rn ignores SIGEMT) */
		sleep(2);
#ifdef VERBOSE
		IF(verbose)
		    fputs("\n\
That process does not seem to exist anymore.  The count of read articles\n\
may be incorrect in the last newsgroup accessed by that other (defunct)\n\
process.\n\n",stdout) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    fputs("\nProcess crashed.\n",stdout) FLUSH;
#endif
		if (*lastngname) {
#ifdef VERBOSE
		    IF(verbose)
			printf("(The last newsgroup accessed was %s.)\n\n",
			lastngname) FLUSH;
		    ELSE
#endif
#ifdef TERSE
			printf("(In %s.)\n\n",lastngname) FLUSH;
#endif
		}
		get_anything();
		putchar('\n') FLUSH;
	    }
	    else {
#ifdef VERBOSE
		IF(verbose)
		    fputs("\n\
You may not have two copies of rn running simultaneously.  Goodbye.\n\
",stdout) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    fputs("\nCan't start another.\n",stdout) FLUSH;
#endif
		finalize(0);
	    }
	}
	tmpfp = fopen(lockname,"w");
	if (tmpfp == Nullfp) {
	    printf(cantcreate,lockname) FLUSH;
	    sig_catcher(0);
	}
	fprintf(tmpfp,"%d\n",getpid());
	fclose(tmpfp);
    }
}

void
newsnews_check()
{
    char *newsnewsname = filexp(NEWSNEWSNAME);

    if ((tmpfp = fopen(newsnewsname,"r")) != Nullfp) {
	fstat(tmpfp->_file,&filestat);
	if (filestat.st_mtime > lasttime) {
	    while (fgets(buf,sizeof(buf),tmpfp) != Nullch)
		fputs(buf,stdout) FLUSH;
	    get_anything();
	    putchar('\n') FLUSH;
	}
	fclose(tmpfp);
    }
}
