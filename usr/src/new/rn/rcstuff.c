/* $Header: rcstuff.c,v 4.3.1.5 86/07/24 14:09:10 lwall Exp $
 *
 * $Log:	rcstuff.c,v $
 * Revision 4.3.1.5  86/07/24  14:09:10  lwall
 * Removed check for spool directory existence in get_ng.
 * 
 * Revision 4.3.1.4  85/09/10  11:04:44  lwall
 * Improved %m in in_char().
 * 
 * Revision 4.3.1.3  85/05/29  09:13:25  lwall
 * %d that should be %ld.
 * 
 * Revision 4.3.1.2  85/05/17  11:40:08  lwall
 * Sped up "rn -c" by not mallocing unnecessarily.
 * 
 * Revision 4.3.1.1  85/05/10  11:37:18  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:45:56  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "ngdata.h"
#include "term.h"
#include "final.h"
#include "rn.h"
#include "intrp.h"
#include "only.h"
#include "rcln.h"
#include "INTERN.h"
#include "rcstuff.h"

char *rcname INIT(Nullch);		/* path name of .newsrc file */
char *rctname INIT(Nullch);		/* path name of temp .newsrc file */
char *rcbname INIT(Nullch);		/* path name of backup .newsrc file */
char *softname INIT(Nullch);		/* path name of .rnsoft file */
FILE *rcfp INIT(Nullfp);			/* .newsrc file pointer */

#ifdef HASHNG
    short hashtbl[HASHSIZ];
#endif

bool
rcstuff_init()
{
    register NG_NUM newng;
    register char *s;
    register int i;
    register bool foundany = FALSE;
    char *some_buf;
    long length;

#ifdef HASHNG
    for (i=0; i<HASHSIZ; i++)
	hashtbl[i] = -1;
#endif

    /* make filenames */

    rcname = savestr(filexp(RCNAME));
    rctname = savestr(filexp(RCTNAME));
    rcbname = savestr(filexp(RCBNAME));
    softname = savestr(filexp(SOFTNAME));
    
    /* make sure the .newsrc file exists */

    newsrc_check();

    /* open .rnsoft file containing soft ptrs to active file */

    tmpfp = fopen(softname,"r");
    if (tmpfp == Nullfp)
	writesoft = TRUE;

    /* read in the .newsrc file */

    for (nextrcline = 0;
	(some_buf = get_a_line(buf,LBUFLEN,rcfp)) != Nullch;
	nextrcline++) {
					/* for each line in .newsrc */
	char tmpbuf[10];

	newng = nextrcline;		/* get it into a register */
	length = len_last_line_got;	/* side effect of get_a_line */
	if (length <= 1) {		/* only a newline??? */
	    nextrcline--;		/* compensate for loop increment */
	    continue;
	}
	if (newng >= MAXRCLINE) {	/* check for overflow */
	    fputs("Too many lines in .newsrc\n",stdout) FLUSH;
	    finalize(1);
	}
	if (tmpfp != Nullfp && fgets(tmpbuf,10,tmpfp) != Nullch)
	    softptr[newng] = atoi(tmpbuf);
	else
	    softptr[newng] = 0;
	some_buf[--length] = '\0';	/* wipe out newline */
	if (checkflag)			/* no extra mallocs for -c */
	    rcline[newng] = some_buf;
	else if (some_buf == buf) {
	    rcline[newng] = savestr(some_buf);
					/* make a semipermanent copy */
	}
	else {
	    /*NOSTRICT*/
#ifndef lint
	    some_buf = saferealloc(some_buf,(MEM_SIZE)(length+1));
#endif lint
	    rcline[newng] = some_buf;
	}
#ifdef NOTDEF
	if (strnEQ(some_buf,"to.",3)) {	/* is this a non-newsgroup? */
	    nextrcline--;		/* destroy this line */
	    continue;
	}
#endif
	if (*some_buf == ' ' ||
	  *some_buf == '\t' ||
	  strnEQ(some_buf,"options",7)) {		/* non-useful line? */
	    toread[newng] = TR_JUNK;
	    rcchar[newng] = ' ';
	    rcnums[newng] = 0;
	    continue;
	}
	for (s = rcline[newng]; *s && *s != ':' && *s != NEGCHAR; s++) ;
	if (!*s && !checkflag) {
#ifndef lint
	    rcline[newng] = saferealloc(rcline[newng],(MEM_SIZE)length+2);
#endif lint
	    s = rcline[newng] + length;
	    *s = ':';
	    *(s+1) = '\0';
	}
	rcchar[newng] = *s;		/* salt away the : or ! */
	rcnums[newng] = (char)(s - rcline[newng]); 
	rcnums[newng]++;		/* remember where it was */
	*s = '\0';			/* null terminate newsgroup name */
#ifdef HASHNG
	if (!checkflag)
	    sethash(newng);
#endif
	if (rcchar[newng] == NEGCHAR) {
	    toread[newng] = TR_UNSUB;
	    continue;
	}

	/* now find out how much there is to read */

	if (!inlist(buf) || (suppress_cn && foundany && !paranoid))
	    toread[newng] = TR_NONE;	/* no need to calculate now */
	else
	    set_toread(newng);
#ifdef VERBOSE
	if (!checkflag && softmisses == 1) {
	    softmisses++;		/* lie a little */
	    fputs("(Revising soft pointers--be patient.)\n",stdout) FLUSH;
	}
#endif
	if (toread[newng] > TR_NONE) {	/* anything unread? */
	    if (!foundany) {
		starthere = newng;
		foundany = TRUE;	/* remember that fact*/
	    }
	    if (suppress_cn) {		/* if no listing desired */
		if (checkflag) {	/* if that is all they wanted */
		    finalize(1);	/* then bomb out */
		}
	    }
	    else {
#ifdef VERBOSE
		IF(verbose)
		    printf("Unread news in %-20s %5ld article%s\n",
			rcline[newng],(long)toread[newng],
			toread[newng]==TR_ONE ? nullstr : "s") FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("%s: %ld article%s\n",
			rcline[newng],(long)toread[newng],
			toread[newng]==TR_ONE ? nullstr : "s") FLUSH;
#endif
		if (int_count) {
		    countdown = 1;
		    int_count = 0;
		}
		if (countdown) {
		    if (! --countdown) {
			fputs("etc.\n",stdout) FLUSH;
			if (checkflag)
			    finalize(1);
			suppress_cn = TRUE;
		    }
		}
	    }
	}
    }
    fclose(rcfp);			/* close .newsrc */
    if (tmpfp != Nullfp)
	fclose(tmpfp);			/* close .rnsoft */
    if (checkflag) {			/* were we just checking? */
	finalize(foundany);		/* tell them what we found */
    }
    if (paranoid)
	cleanup_rc();

#ifdef DEBUGGING
    if (debug & DEB_HASH) {
	page_init();
	for (i=0; i<HASHSIZ; i++) {
	    sprintf(buf,"%d	%d",i,hashtbl[i]);
	    print_lines(buf,NOMARKING);
	}
    }
#endif

    return foundany;
}

/* try to find or add an explicitly specified newsgroup */
/* returns TRUE if found or added, FALSE if not. */
/* assumes that we are chdir'ed to SPOOL */

bool
get_ng(what,do_reloc)
char *what;
bool do_reloc;
{
    char *ntoforget;
    char promptbuf[128];

#ifdef VERBOSE
    IF(verbose)
	ntoforget = "Type n to forget about this newsgroup.\n";
    ELSE
#endif
#ifdef TERSE
	ntoforget = "n to forget it.\n";
#endif
    if (index(what,'/')) {
	dingaling();
	printf("\nBad newsgroup name.\n") FLUSH;
	return FALSE;
    }
    set_ngname(what);
    ng = find_ng(ngname);
    if (ng == nextrcline) {		/* not in .newsrc? */
	if ((softptr[ng] = findact(buf,ngname,strlen(ngname),0L)) < 0 ) {
	    dingaling();
#ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s does not exist!\n",ngname) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\nNo %s!\n",ngname) FLUSH;
#endif
	    sleep(2);
	    return FALSE;
	}
#ifdef VERBOSE
	IF(verbose)
	    sprintf(promptbuf,"\nNewsgroup %s not in .newsrc--add? [yn] ",ngname);
	ELSE
#endif
#ifdef TERSE
	    sprintf(promptbuf,"\nAdd %s? [yn] ",ngname);
#endif
reask_add:
	in_char(promptbuf,'A');
	putchar('\n') FLUSH;
	setdef(buf,"y");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		printf("Type y or SP to add %s to your .newsrc.\n", ngname)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("y or SP to add\n",stdout) FLUSH;
#endif
	    fputs(ntoforget,stdout) FLUSH;
	    goto reask_add;
	}
	else if (*buf == 'n' || *buf == 'q') {
	    return FALSE;
	}
	else if (*buf == 'y') {
	    ng = add_newsgroup(ngname);
	    do_reloc = FALSE;
	}
	else {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_add;
	}
    }
    else if (rcchar[ng] == NEGCHAR) {	/* unsubscribed? */
#ifdef VERBOSE
	IF(verbose)
	    sprintf(promptbuf,
"\nNewsgroup %s is currently unsubscribed to--resubscribe? [yn] ",ngname)
  FLUSH;
	ELSE
#endif
#ifdef TERSE
	    sprintf(promptbuf,"\n%s unsubscribed--resubscribe? [yn] ",ngname)
	      FLUSH;
#endif
reask_unsub:
	in_char(promptbuf,'R');
	putchar('\n') FLUSH;
	setdef(buf,"y");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		printf("Type y or SP to resubscribe to %s.\n", ngname) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("y or SP to resubscribe.\n",stdout) FLUSH;
#endif
	    fputs(ntoforget,stdout) FLUSH;
	    goto reask_unsub;
	}
	else if (*buf == 'n' || *buf == 'q') {
	    return FALSE;
	}
	else if (*buf == 'y') {
	    rcchar[ng] = ':';
	}
	else {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_unsub;
	}
    }

    /* now calculate how many unread articles in newsgroup */

    set_toread(ng);
#ifdef RELOCATE
    if (do_reloc)
	ng = relocate_newsgroup(ng,-1);
#endif
    return toread[ng] >= TR_NONE;
}

/* add a newsgroup to the .newsrc file (eventually) */

NG_NUM
add_newsgroup(ngn)
char *ngn;
{
    register NG_NUM newng = nextrcline++;
					/* increment max rcline index */
    
    rcnums[newng] = strlen(ngn) + 1;
    rcline[newng] = safemalloc((MEM_SIZE)(rcnums[newng] + 1));
    strcpy(rcline[newng],ngn);		/* and copy over the name */
    *(rcline[newng] + rcnums[newng]) = '\0';
    rcchar[newng] = ':';		/* call it subscribed */
    toread[newng] = TR_NONE;	/* just for prettiness */
#ifdef HASHNG
    sethash(newng);			/* so we can find it again */
#endif
#ifdef RELOCATE
    return relocate_newsgroup(newng,-1);
#else
    return newng;
#endif
}

#ifdef RELOCATE
NG_NUM
relocate_newsgroup(ngx,newng)
NG_NUM ngx;
NG_NUM newng;
{
    char *dflt = (ngx!=current_ng ? "$^.L" : "$^L");
    char *tmprcline;
    ART_UNREAD tmptoread;
    char tmprcchar;
    char tmprcnums;
    ACT_POS tmpsoftptr;
    register NG_NUM i;
#ifdef DEBUGGING
    ART_NUM tmpngmax;
#endif
#ifdef CACHEFIRST
    ART_NUM tmpabs1st;
#endif
    
    starthere = 0;                      /* Disable this optimization */
    writesoft = TRUE;			/* Update soft pointer file */
    if (ngx < nextrcline-1) {
#ifdef HASHNG
	for (i=0; i<HASHSIZ; i++) {
	    if (hashtbl[i] > ngx)
		--hashtbl[i];
	    else if (hashtbl[i] == ngx)
		hashtbl[i] = nextrcline-1;
	}
#endif
	tmprcline = rcline[ngx];
	tmptoread = toread[ngx];
	tmprcchar = rcchar[ngx];
	tmprcnums = rcnums[ngx];
	tmpsoftptr = softptr[ngx];
#ifdef DEBUGGING
	tmpngmax = ngmax[ngx];
#endif
#ifdef CACHEFIRST
	tmpabs1st = abs1st[ngx];
#endif
	for (i=ngx+1; i<nextrcline; i++) {
	    rcline[i-1] = rcline[i];
	    toread[i-1] = toread[i];
	    rcchar[i-1] = rcchar[i];
	    rcnums[i-1] = rcnums[i];
	    softptr[i-1] = softptr[i];
#ifdef DEBUGGING
	    ngmax[i-1] = ngmax[i];
#endif
#ifdef CACHEFIRST
	    abs1st[i-1] = abs1st[i];
#endif
	}
	rcline[nextrcline-1] = tmprcline;
	toread[nextrcline-1] = tmptoread;
	rcchar[nextrcline-1] = tmprcchar;
	rcnums[nextrcline-1] = tmprcnums;
	softptr[nextrcline-1] = tmpsoftptr;
#ifdef DEBUGGING
	ngmax[nextrcline-1] = tmpngmax;
#endif
#ifdef CACHEFIRST
	abs1st[nextrcline-1] = tmpabs1st;
#endif
    }
    if (current_ng > ngx)
	current_ng--;
    if (newng < 0) {
      reask_reloc:
	unflush_output();		/* disable any ^O in effect */
#ifdef VERBOSE
	IF(verbose)
	    printf("\nPut newsgroup where? [%s] ", dflt);
	ELSE
#endif
#ifdef TERSE
	    printf("\nPut where? [%s] ", dflt);
#endif
	fflush(stdout);
      reinp_reloc:
	eat_typeahead();
	getcmd(buf);
	if (errno || *buf == '\f') {
			    /* if return from stop signal */
	    goto reask_reloc;	/* give them a prompt again */
	}
	setdef(buf,dflt);
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose) {
		printf("\n\n\
Type ^ to put the newsgroup first (position 0).\n\
Type $ to put the newsgroup last (position %d).\n", nextrcline-1);
		printf("\
Type . to put it before the current newsgroup (position %d).\n", current_ng);
		printf("\
Type -newsgroup name to put it before that newsgroup.\n\
Type +newsgroup name to put it after that newsgroup.\n\
Type a number between 0 and %d to put it at that position.\n", nextrcline-1);
		printf("\
Type L for a listing of newsgroups and their positions.\n") FLUSH;
	    }
	    ELSE
#endif
#ifdef TERSE
	    {
		printf("\n\n\
^ to put newsgroup first (pos 0).\n\
$ to put last (pos %d).\n", nextrcline-1);
		printf("\
. to put before current newsgroup (pos %d).\n", current_ng);
		printf("\
-newsgroup to put before newsgroup.\n\
+newsgroup to put after.\n\
number in 0-%d to put at that pos.\n", nextrcline-1);
		printf("\
L for list of .newsrc.\n") FLUSH;
	    }
#endif
	    goto reask_reloc;
	}
	else if (*buf == 'L') {
	    putchar('\n') FLUSH;
	    list_newsgroups();
	    goto reask_reloc;
	}
	else if (isdigit(*buf)) {
	    if (!finish_command(TRUE))	/* get rest of command */
		goto reinp_reloc;
	    newng = atoi(buf);
	    if (newng < 0)
		newng = 0;
	    if (newng >= nextrcline)
		return nextrcline-1;
	}
	else if (*buf == '^') {
	    putchar('\n') FLUSH;
	    newng = 0;
	}
	else if (*buf == '$') {
	    putchar('\n') FLUSH;
	    return nextrcline-1;
	}
	else if (*buf == '.') {
	    putchar('\n') FLUSH;
	    newng = current_ng;
	}
	else if (*buf == '-' || *buf == '+') {
	    if (!finish_command(TRUE))	/* get rest of command */
		goto reinp_reloc;
	    newng = find_ng(buf+1);
	    if (newng == nextrcline) {
		fputs("Not found.",stdout) FLUSH;
		goto reask_reloc;
	    }
	    if (*buf == '+')
		newng++;
	}
	else {
	    printf("\n%s",hforhelp) FLUSH;
	    settle_down();
	    goto reask_reloc;
	}
    }
    if (newng < nextrcline-1) {
#ifdef HASHNG
	for (i=0; i<HASHSIZ; i++) {
	    if (hashtbl[i] == nextrcline-1)
		hashtbl[i] = newng;
	    else if (hashtbl[i] >= newng)
		++hashtbl[i];
	}
#endif
	tmprcline = rcline[nextrcline-1];
	tmptoread = toread[nextrcline-1];
	tmprcchar = rcchar[nextrcline-1];
	tmprcnums = rcnums[nextrcline-1];
	tmpsoftptr = softptr[nextrcline-1];
#ifdef DEBUGGING
	tmpngmax = ngmax[nextrcline-1];
#endif
#ifdef CACHEFIRST
	tmpabs1st = abs1st[nextrcline-1];
#endif
	for (i=nextrcline-2; i>=newng; i--) {
	    rcline[i+1] = rcline[i];
	    toread[i+1] = toread[i];
	    rcchar[i+1] = rcchar[i];
	    rcnums[i+1] = rcnums[i];
	    softptr[i+1] = softptr[i];
#ifdef DEBUGGING
	    ngmax[i+1] = ngmax[i];
#endif
#ifdef CACHEFIRST
	    abs1st[i+1] = abs1st[i];
#endif
	}
	rcline[newng] = tmprcline;
	toread[newng] = tmptoread;
	rcchar[newng] = tmprcchar;
	rcnums[newng] = tmprcnums;
	softptr[newng] = tmpsoftptr;
#ifdef DEBUGGING
	ngmax[newng] = tmpngmax;
#endif
#ifdef CACHEFIRST
	abs1st[newng] = tmpabs1st;
#endif
    }
    if (current_ng >= newng)
	current_ng++;
    return newng;
}
#endif

/* List out the newsrc with annotations */

void
list_newsgroups()
{
    register NG_NUM i;
    char tmpbuf[2048];
    static char *status[] = {"(READ)","(UNSUB)","(BOGUS)","(JUNK)"};
    int cmd;

    page_init();
    print_lines("\
  #  Status  Newsgroup\n\
",STANDOUT);
    for (i=0; i<nextrcline && !int_count; i++) {
	if (toread[i] >= 0)
	    set_toread(i);
	*(rcline[i] + rcnums[i] - 1) = rcchar[i];
	if (toread[i] > 0)
	    sprintf(tmpbuf,"%3d %6ld   ",i,(long)toread[i]);
	else
	    sprintf(tmpbuf,"%3d %7s  ",i,status[-toread[i]]);
	safecpy(tmpbuf+13,rcline[i],2034);
	*(rcline[i] + rcnums[i] - 1) = '\0';
	if (cmd = print_lines(tmpbuf,NOMARKING)) {
	    if (cmd > 0)
		pushchar(cmd);
	    break;
	}
    }
    int_count = 0;
}

/* find a newsgroup in .newsrc */

NG_NUM
find_ng(ngnam)
char *ngnam;
{
    register NG_NUM ngnum;
#ifdef HASHNG
    register int hashix = hash(ngnam);
    register int incr = 1;

    while ((ngnum = hashtbl[hashix]) >= 0) {
	if (strEQ(rcline[ngnum], ngnam) && toread[ngnum] >= TR_UNSUB)
	    return ngnum;
	hashix = (hashix + incr) % HASHSIZ;
	incr += 2;			/* offsets from original are in n*2 */
    }
    return nextrcline;			/* = notfound */

#else /* just do linear search */

    for (ngnum = 0; ngnum < nextrcline; ngnum++) {
	if (strEQ(rcline[ngnum],ngnam))
	    break;
    }
    return ngnum;
#endif
}

void
cleanup_rc()
{
    register NG_NUM ngx;
    register NG_NUM bogosity = 0;

#ifdef VERBOSE
    IF(verbose)
	fputs("Checking out your .newsrc--hang on a second...\n",stdout)
	  FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("Checking .newsrc--hang on...\n",stdout) FLUSH;
#endif
    for (ngx = 0; ngx < nextrcline; ngx++) {
	if (toread[ngx] >= TR_UNSUB) {
	    set_toread(ngx);		/* this may reset newsgroup */
					/* or declare it bogus */
	}
	if (toread[ngx] == TR_BOGUS)
	    bogosity++;
    }
    for (ngx = nextrcline-1; ngx >= 0 && toread[ngx] == TR_BOGUS; ngx--)
	bogosity--;			/* discount already moved ones */
    if (nextrcline > 5 && bogosity > nextrcline / 2) {
	fputs(
"It looks like the active file is messed up.  Contact your news administrator,\n\
",stdout);
	fputs(
"leave the \"bogus\" groups alone, and they may come back to normal.  Maybe.\n\
",stdout) FLUSH;
    }
#ifdef RELOCATE
    else if (bogosity) {
#ifdef VERBOSE
	IF(verbose)
	    fputs("Moving bogus newsgroups to the end of your .newsrc.\n",
		stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("Moving boguses to the end.\n",stdout) FLUSH;
#endif
	for (; ngx >= 0; ngx--) {
	    if (toread[ngx] == TR_BOGUS)
		relocate_newsgroup(ngx,nextrcline-1);
	}
#ifdef DELBOGUS
reask_bogus:
	in_char("Delete bogus newsgroups? [ny] ", 'D');
	putchar('\n') FLUSH;
	setdef(buf,"n");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\
Type y to delete bogus newsgroups.\n\
Type n or SP to leave them at the end in case they return.\n\
",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("y to delete, n to keep\n",stdout) FLUSH;
#endif
	    goto reask_bogus;
	}
	else if (*buf == 'n' || *buf == 'q')
	    ;
	else if (*buf == 'y') {
	    while (toread[nextrcline-1] == TR_BOGUS && nextrcline > 0)
		--nextrcline;		/* real tough, huh? */
	}
	else {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_bogus;
	}
#endif
    }
#else
#ifdef VERBOSE
    IF(verbose)
	fputs("You should edit bogus newsgroups out of your .newsrc.\n",
	    stdout) FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("Edit boguses from .newsrc.\n",stdout) FLUSH;
#endif
#endif
    paranoid = FALSE;
}

#ifdef HASHNG
/* make an entry in the hash table for the current newsgroup */

void
sethash(thisng)
NG_NUM thisng;
{
    register int hashix = hash(rcline[thisng]);
    register int incr = 1;
#ifdef DEBUGGING
    static int hashhits = 0, hashtries = 0;
#endif

#ifdef DEBUGGING
    hashtries++;
#endif
    while (hashtbl[hashix] >= 0) {
#ifdef DEBUGGING
	hashhits++;
	if (debug & DEB_HASH) {
	    printf("  Hash hits: %d / %d\n",hashhits, hashtries) FLUSH;
	}
	hashtries++;
#endif
	hashix = (hashix + incr) % HASHSIZ;
	incr += 2;			/* offsets from original are in n*2 */
    }
    hashtbl[hashix] = thisng;
}

short prime[] = {1,2,-3,-5,7,11,-13,-17,19,23,-29,-31,37,41,-43,-47,53,57,-59,
    -61,67,71,-73,-79,83,89,-97,-101,1,1,1,1,1,1,1,1,1,1,1,1};

int
hash(ngnam)
register char *ngnam;
{
    register int i = 0;
    register int ch;
    register int sum = 0;
#ifdef DEBUGGING
    char *ngn = ngnam;
#endif

    while (ch = *ngnam++) {
	sum += (ch + i) * prime[i];   /* gives ~ 10% hits at 25% full */
	i++;
    }
#ifdef DEBUGGING
    if (debug & DEB_HASH)
	printf("hash(%s) => %d => %d\n",ngn, sum, (sum<0?-sum:sum)%HASHSIZ)
	  FLUSH;
#endif
    if (sum < 0)
	sum = -sum;
    return sum % HASHSIZ;
}

#endif

void
newsrc_check()
{
    rcfp = fopen(rcname,"r");		/* open it */
    if (rcfp == Nullfp) {			/* not there? */
#ifdef VERBOSE
	IF(verbose)
	    fputs("\
Trying to set up a .newsrc file--running newsetup...\n\n\
",stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("Setting up .newsrc...\n",stdout) FLUSH;
#endif
	if (doshell(sh,filexp(NEWSETUP)) ||
	    (rcfp = fopen(rcname,"r")) == Nullfp) {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\
Can't create a .newsrc--you must do it yourself.\n\
",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("(Fatal)\n",stdout) FLUSH;
#endif
	    finalize(1);
	}
    }
    else {
	UNLINK(rcbname);		/* unlink backup file name */
	link(rcname,rcbname);		/* and backup current name */
    }
}

/* write out the (presumably) revised .newsrc */

void
write_rc()
{
    register NG_NUM tmpng;
    register char *delim;

    rcfp = fopen(rctname, "w");		/* open .newsrc */
    if (rcfp == Nullfp) {
	printf("Can't recreate .newsrc\n") FLUSH;
	finalize(1);
    }

    /* write out each line*/

    for (tmpng = 0; tmpng < nextrcline; tmpng++) {
	if (rcnums[tmpng]) {
	    delim = rcline[tmpng] + rcnums[tmpng] - 1;
	    *delim = rcchar[tmpng];
	}
	else
	    delim = Nullch;
#ifdef DEBUGGING
	if (debug & DEB_NEWSRC_LINE)
	    printf("%s\n",rcline[tmpng]) FLUSH;
#endif
	fprintf(rcfp,"%s\n",rcline[tmpng]);
	if (delim)
	    *delim = '\0';		/* might still need this line */
    }

    fclose(rcfp);			/* close .newsrc */
    UNLINK(rcname);
    link(rctname,rcname);
    UNLINK(rctname);

    if (writesoft) {
	tmpfp = fopen(filexp(softname), "w");	/* open .rnsoft */
	if (tmpfp == Nullfp) {
	    printf(cantcreate,filexp(softname)) FLUSH;
	    return;
	}
	for (tmpng = 0; tmpng < nextrcline; tmpng++) {
	    fprintf(tmpfp,"%ld\n",(long)softptr[tmpng]);
	}
	fclose(tmpfp);
    }
}

void
get_old_rc()
{
    UNLINK(rctname);
    link(rcname,rctname);
    UNLINK(rcname);
    link(rcbname,rcname);
    UNLINK(rcbname);
}
