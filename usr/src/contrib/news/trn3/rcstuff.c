/* $Id: rcstuff.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "cache.h"
#include "bits.h"
#include "ngdata.h"
#include "term.h"
#include "final.h"
#include "trn.h"
#include "intrp.h"
#include "only.h"
#include "rcln.h"
#include "nntp.h"
#include "autosub.h"
#include "hash.h"
#include "INTERN.h"
#include "rcstuff.h"

char *rcname INIT(Nullch);		/* path name of .newsrc file */
char *rctname INIT(Nullch);		/* path name of temp .newsrc file */
char *rcbname INIT(Nullch);		/* path name of backup .newsrc file */
char *softname INIT(Nullch);		/* path name of .rnsoft file */
FILE *rcfp INIT(Nullfp);		/* .newsrc file pointer */

static void grow_rc_arrays _((int));
static void parse_rcline _((NG_NUM));

#ifdef HASHNG
static HASHTABLE *rc_hash;
static int rcline_cmp _((char*,int,HASHDATUM));
static void del_rc_line _((HASHDATUM*,int));
static void ins_rc_line _((HASHDATUM*,int));
#endif

bool
rcstuff_init()
{
    register NG_NUM newng;
    register int i;
    register bool foundany = FALSE;
    char *some_buf;
    long length;
#ifdef USE_NNTP
    char *cp;
#endif

    /* make filenames */

#ifdef USE_NNTP
    if (cp = getenv("NEWSRC"))
	rcname = savestr(filexp(cp));
    else
	rcname = savestr(filexp(RCNAME));
#else
    rcname = savestr(filexp(RCNAME));
#endif

    rctname = savestr(filexp(RCTNAME));
    rcbname = savestr(filexp(RCBNAME));
    softname = savestr(filexp(SOFTNAME));
    
    /* make sure the .newsrc file exists */

    newsrc_check();

    /* open .rnsoft file containing soft ptrs to active file */

    tmpfp = fopen(softname,"r");
    if (tmpfp == Nullfp)
	writesoft = TRUE;

    /* allocate memory for rc file globals */
    grow_rc_arrays(1500);

    /* read in the .newsrc file */

    for (nextrcline = 0;
	(some_buf = get_a_line(buf,LBUFLEN,rcfp)) != Nullch;
	nextrcline++)			/* for each line in .newsrc */
    {
	char tmpbuf[10];

	newng = nextrcline;		/* get it into a register */
	length = len_last_line_got;	/* side effect of get_a_line */
	if (length <= 1) {		/* only a newline??? */
	    nextrcline--;		/* compensate for loop increment */
	    continue;
	}
	if (newng >= maxrcline)		/* check for overflow */
	    grow_rc_arrays(maxrcline + 500);
	if (tmpfp != Nullfp && fgets(tmpbuf,10,tmpfp) != Nullch)
	    softptr[newng] = atol(tmpbuf);
	else
	    softptr[newng] = 0;
	some_buf[--length] = '\0';	/* wipe out newline */
	if (checkflag)			/* no extra mallocs for -c */
	    rcline[newng] = some_buf;
	else if (some_buf == buf)
	    rcline[newng] = savestr(some_buf);  /* make semipermanent copy */
	else {
	    /*NOSTRICT*/
#ifndef lint
	    some_buf = saferealloc(some_buf,(MEM_SIZE)(length+1));
#endif
	    rcline[newng] = some_buf;
	}
	if (*some_buf == ' ' || *some_buf == '\t'
	 || strnEQ(some_buf,"options",7)) {	/* non-useful line? */
	    toread[newng] = TR_JUNK;
	    rcchar[newng] = ' ';
	    rcnums[newng] = 0;
	    continue;
	}
	parse_rcline(newng);
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
	    fputs("(Revising soft pointers -- be patient.)\n",stdout) FLUSH;
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
		    printf("Unread news in %-40s %5ld article%s\n",
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
		    if (!--countdown) {
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
    if (checkflag)			/* were we just checking? */
	finalize(foundany);		/* tell them what we found */
    if (paranoid)
	cleanup_rc();

#ifdef HASHNG
    rc_hash = hashcreate((int)nextrcline, rcline_cmp);
    for (i = 0; i < nextrcline; i++)
	if (toread[i] >= TR_UNSUB)
	    sethash(i);
#endif

    return foundany;
}

static void
parse_rcline(ngnum)
NG_NUM ngnum;
{
    char *s;

    for (s = rcline[ngnum]; *s && *s != ':' && *s != NEGCHAR; s++) ;
    if (!*s && !checkflag) {
#ifndef lint
	rcline[ngnum] = saferealloc(rcline[ngnum],
				(MEM_SIZE)(s - rcline[ngnum]) + 3);
#endif /* lint */
	strcpy(s, ": ");
    }
    if (*s == ':' && s[1] && s[2] == '0') {
	rcchar[ngnum] = '0';
	s[2] = '1';
    } else
	rcchar[ngnum] = *s;	/* salt away the : or ! */
    rcnums[ngnum] = (char)(s - rcline[ngnum]) + 1;
				/* remember where the numbers are */
    *s = '\0';			/* null terminate newsgroup name */
}

void
abandon_ng(ngnum)
NG_NUM ngnum;
{
    char *some_buf = Nullch;

    /* open .oldnewsrc and try to find the prior value for the group. */
    if ((rcfp = fopen(rcbname, "r")) != Nullfp) {
	int length = rcnums[ngnum] - 1;

	while ((some_buf = get_a_line(buf,LBUFLEN,rcfp)) != Nullch) {
	    if (len_last_line_got <= 0)
		continue;
	    some_buf[len_last_line_got-1] = '\0';	/* wipe out newline */
	    if ((some_buf[length] == ':' || some_buf[length] == NEGCHAR)
	     && strnEQ(rcline[ngnum], some_buf, length)) {
		break;
	    }
	    if (some_buf != buf)
		free(some_buf);
	}
	fclose(rcfp);
    } else if (errno != ENOENT) {
	printf("Unable to open %s.\n", rcbname) FLUSH;
	return;
    }
    if (some_buf == Nullch) {
	some_buf = rcline[ngnum] + rcnums[ngnum];
	if (*some_buf == ' ')
	    some_buf++;
	*some_buf = '\0';
	abs1st[ngnum] = 0;	/* force group to be re-calculated */
    }
    else {
	free(rcline[ngnum]);
	if (some_buf == buf) {
	    rcline[ngnum] = savestr(some_buf);
	}
	else {
	    /*NOSTRICT*/
#ifndef lint
	    some_buf = saferealloc(some_buf, (MEM_SIZE)(len_last_line_got));
#endif /* lint */
	    rcline[ngnum] = some_buf;
	}
    }
    parse_rcline(ngnum);
    if (rcchar[ngnum] == NEGCHAR)
	rcchar[ngnum] = ':';
    set_toread(ngnum);
}

/* try to find or add an explicitly specified newsgroup */
/* returns TRUE if found or added, FALSE if not. */
/* assumes that we are chdir'ed to NEWSSPOOL */

bool
get_ng(what,flags)
char *what;
int flags;
{
    char *ntoforget;
    char promptbuf[128];
    int autosub;

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
      check_fuzzy_match:
	if (fuzzyGet && (flags & GNG_FUZZY)) {
	    if (find_close_match())
		what = ngname;
	    else
		return FALSE;
	} else
	    return FALSE;
    }
    set_ngname(what);
    ng = find_ng(ngname);
    if (ng == nextrcline) {		/* not in .newsrc? */
	if (ng >= maxrcline)		/* check for overflow */
	    grow_rc_arrays(maxrcline + 25);
#ifdef USE_NNTP
	softptr[ng] = 0;
	if (!nntp_group(ngname))
#else /* !USE_NNTP */
	if ((softptr[ng] = findact(buf,ngname,strlen(ngname),0L)) < 0)
#endif /* !USE_NNTP */
	{
	    dingaling();
#ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s does not exist!\n",ngname) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\nNo %s!\n",ngname) FLUSH;
#endif
	    if (novice_delays)
		sleep(2);
	    goto check_fuzzy_match;
	}
	autosub = auto_subscribe(ngname);
	if (!autosub) autosub = addnewbydefault;
	if (autosub) {
	    if (append_unsub) {
		printf("(Adding %s to end of your .newsrc %ssubscribed)\n",
		       ngname, (autosub == ADDNEW_SUB) ? "" : "un") FLUSH;
		ng = add_newsgroup(ngname, autosub);
	    } else {
		if (autosub == ADDNEW_SUB) {
		    printf("(Subscribing to %s)\n", ngname) FLUSH;
		    ng = add_newsgroup(ngname, autosub);
		} else {
		    printf("(Ignoring %s)\n", ngname) FLUSH;
		    return FALSE;
		}
	    }
	    flags &= ~GNG_RELOC;
	} else {
#ifdef VERBOSE
	IF(verbose)
	    sprintf(promptbuf,"\nNewsgroup %s not in .newsrc -- subscribe? [ynYN] ",ngname);
	ELSE
#endif
#ifdef TERSE
	    sprintf(promptbuf,"\nSubscribe %s? [ynYN] ",ngname);
#endif
reask_add:
	in_char(promptbuf,'A');
	setdef(buf,"y");
#ifdef VERIFY
	printcmd();
#endif
	putchar('\n') FLUSH;
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		printf("Type y or SP to subscribe to %s.\n\
Type Y to subscribe to this and all remaining new groups.\n\
Type N to leave all remaining new groups unsubscribed.\n", ngname)
		  FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("y or SP to subscribe, Y to subscribe all new groups, N to unsubscribe all\n",stdout) FLUSH;
#endif
	    fputs(ntoforget,stdout) FLUSH;
	    goto reask_add;
	}
	else if (*buf == 'n' || *buf == 'q') {
	    if (append_unsub) {
		ng = add_newsgroup(ngname, NEGCHAR);
	    }
	    return FALSE;
	}
	else if (*buf == 'y') {
	    ng = add_newsgroup(ngname, ':');
	    flags |= GNG_RELOC;
	}
	else if (*buf == 'Y') {
	    addnewbydefault = ADDNEW_SUB;
	    if (append_unsub)
		printf("(Adding %s to end of your .newsrc subscribed)\n",
		       ngname) FLUSH;
	    else
		printf("(Subscribing to %s)\n", ngname) FLUSH;
	    ng = add_newsgroup(ngname, ':');
	    flags &= ~GNG_RELOC;
	}
	else if (*buf == 'N') {
	    addnewbydefault = ADDNEW_UNSUB;
	    if (append_unsub) {
		printf("(Adding %s to end of your .newsrc unsubscribed)\n",
		       ngname) FLUSH;
		ng = add_newsgroup(ngname, NEGCHAR);
		flags &= ~GNG_RELOC;
	    } else {
		printf("(Ignoring %s)\n", ngname) FLUSH;
		return FALSE;
	    }
	}
	else {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_add;
	}
      }
    }
    else if (mode == 'i')		/* adding new groups during init? */
	return FALSE;
    else if (rcchar[ng] == NEGCHAR) {	/* unsubscribed? */
#ifdef VERBOSE
	IF(verbose)
	    sprintf(promptbuf,
"\nNewsgroup %s is unsubscribed -- resubscribe? [yn] ",ngname)
  FLUSH;
	ELSE
#endif
#ifdef TERSE
	    sprintf(promptbuf,"\nResubscribe %s? [yn] ",ngname)
	      FLUSH;
#endif
reask_unsub:
	in_char(promptbuf,'R');
	setdef(buf,"y");
#ifdef VERIFY
	printcmd();
#endif
	putchar('\n') FLUSH;
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
	    register char *cp = rcline[ng] + rcnums[ng];
	    rcchar[ng] = (*cp && cp[1] == '0' ? '0' : ':');
	    flags &= ~GNG_RELOC;
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
    if (flags & GNG_RELOC) {
	ng = relocate_newsgroup(ng,-1);
	if (ng < 0)
	    return FALSE;
    }
#endif
    return toread[ng] >= TR_NONE;
}

/* add a newsgroup to the .newsrc file (eventually) */

NG_NUM
add_newsgroup(ngn, c)
char *ngn;
char_int c;
{
    register NG_NUM newng = nextrcline++;
					/* increment max rcline index */

    if (newng >= maxrcline)		/* check for overflow */
	grow_rc_arrays(maxrcline + 25);

    rcnums[newng] = strlen(ngn) + 1;
    rcline[newng] = safemalloc((MEM_SIZE)(rcnums[newng] + 2));
    strcpy(rcline[newng],ngn);		/* and copy over the name */
    strcpy(rcline[newng]+rcnums[newng], " ");
    rcchar[newng] = c;			/* subscribe or unsubscribe */
    toread[newng] = TR_NONE;	/* just for prettiness */
#ifdef HASHNG
    sethash(newng);			/* so we can find it again */
#endif
    return newng;
}

#ifdef RELOCATE
NG_NUM
relocate_newsgroup(ngx,newng)
NG_NUM ngx;
NG_NUM newng;
{
    char *dflt = (ngx!=current_ng ? "$^.Lq" : "$^Lq");
    char *tmprcline;
    ART_UNREAD tmptoread;
    char tmprcchar;
    char tmprcnums;
    ACT_POS tmpsoftptr;
    register NG_NUM i;
    ART_NUM tmpngmax;
    ART_NUM tmpabs1st;
    
    starthere = 0;                      /* Disable this optimization */
    writesoft = TRUE;			/* Update soft pointer file */
    if (ngx < nextrcline-1) {
#ifdef HASHNG
	if (rc_hash)
	    hashwalk(rc_hash, del_rc_line, ngx);
#endif
	tmprcline = rcline[ngx];
	tmptoread = toread[ngx];
	tmprcchar = rcchar[ngx];
	tmprcnums = rcnums[ngx];
	tmpsoftptr = softptr[ngx];
	tmpngmax = ngmax[ngx];
	tmpabs1st = abs1st[ngx];
	for (i=ngx+1; i<nextrcline; i++) {
	    rcline[i-1] = rcline[i];
	    toread[i-1] = toread[i];
	    rcchar[i-1] = rcchar[i];
	    rcnums[i-1] = rcnums[i];
	    softptr[i-1] = softptr[i];
	    ngmax[i-1] = ngmax[i];
	    abs1st[i-1] = abs1st[i];
	}
	rcline[nextrcline-1] = tmprcline;
	toread[nextrcline-1] = tmptoread;
	rcchar[nextrcline-1] = tmprcchar;
	rcnums[nextrcline-1] = tmprcnums;
	softptr[nextrcline-1] = tmpsoftptr;
	ngmax[nextrcline-1] = tmpngmax;
	abs1st[nextrcline-1] = tmpabs1st;
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
Type L for a listing of newsgroups and their positions.\n\
Type q to abort the current action.\n") FLUSH;
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
L for list of .newsrc.\n\
q to abort\n") FLUSH;
	    }
#endif
	    goto reask_reloc;
	}
	else if (*buf == 'q')
	    return -1;
	else if (*buf == 'L') {
	    putchar('\n') FLUSH;
	    list_newsgroups();
	    goto reask_reloc;
	}
	else if (isdigit(*buf)) {
	    if (!finish_command(TRUE))	/* get rest of command */
		goto reinp_reloc;
	    newng = atol(buf);
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
	    newng = nextrcline-1;
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
	if (rc_hash)
	    hashwalk(rc_hash, ins_rc_line, newng);
#endif
	tmprcline = rcline[nextrcline-1];
	tmptoread = toread[nextrcline-1];
	tmprcchar = rcchar[nextrcline-1];
	tmprcnums = rcnums[nextrcline-1];
	tmpsoftptr = softptr[nextrcline-1];
	tmpngmax = ngmax[nextrcline-1];
	tmpabs1st = abs1st[nextrcline-1];
	for (i=nextrcline-2; i>=newng; i--) {
	    rcline[i+1] = rcline[i];
	    toread[i+1] = toread[i];
	    rcchar[i+1] = rcchar[i];
	    rcnums[i+1] = rcnums[i];
	    softptr[i+1] = softptr[i];
	    ngmax[i+1] = ngmax[i];
	    abs1st[i+1] = abs1st[i];
	}
	rcline[newng] = tmprcline;
	toread[newng] = tmptoread;
	rcchar[newng] = tmprcchar;
	rcnums[newng] = tmprcnums;
	softptr[newng] = tmpsoftptr;
	ngmax[newng] = tmpngmax;
	abs1st[newng] = tmpabs1st;
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
	*(rcline[i] + rcnums[i] - 1) = RCCHAR(rcchar[i]);
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
#ifdef HASHNG
    HASHDATUM data;

    assert(rc_hash != 0);
    data = hashfetch(rc_hash, ngnam, strlen(ngnam));
    if (!data.dat_ptr)
	return nextrcline;		/* = notfound */
    return data.dat_len;

#else /* just do linear search */
    register NG_NUM ngnum;

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
	fputs("Checking out your .newsrc -- hang on a second...\n",stdout)
	  FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("Checking .newsrc -- hang on...\n",stdout) FLUSH;
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
	setdef(buf,"n");
#ifdef VERIFY
	printcmd();
#endif
	putchar('\n') FLUSH;
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
    HASHDATUM data;

    data.dat_ptr = nullstr;
    data.dat_len = thisng;
    hashstore(rc_hash, rcline[thisng], rcnums[thisng]-1, data);
}

static int
rcline_cmp(key, keylen, data)
char *key;
int keylen;
HASHDATUM data;
{
    /* We already know that the lengths are equal, just compare the strings */
    return bcmp(key, rcline[data.dat_len], keylen);
}

static void
del_rc_line(data, ngnum)
HASHDATUM *data;
int ngnum;
{
    if (data->dat_len == ngnum)
	data->dat_len = nextrcline-1;
    else if (data->dat_len > ngnum)
	data->dat_len--;
}

static void
ins_rc_line(data, ngnum)
HASHDATUM *data;
int ngnum;
{
    if (data->dat_len == nextrcline-1)
	data->dat_len = ngnum;
    else if (data->dat_len >= ngnum)
	data->dat_len++;
}
#endif

void
newsrc_check()
{
    rcfp = fopen(rcname,"r");		/* open it */
    if (rcfp == Nullfp) {			/* not there? */
#ifdef VERBOSE
	IF(verbose)
	    fputs("\nTrying to set up a .newsrc file -- running newsetup...\n\n\
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
		fputs("\nCan't create a .newsrc -- you must do it yourself.\n\
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

/* checkpoint the .newsrc */

void
checkpoint_rc()
{
#ifdef DEBUG
    if (debug & DEB_CHECKPOINTING) {
	fputs("(ckpt)",stdout);
	fflush(stdout);
    }
#endif
    if (doing_ng)
	bits_to_rc();			/* do not restore M articles */
    if (rc_changed)
	write_rc();
#ifdef DEBUG
    if (debug & DEB_CHECKPOINTING) {
	fputs("(done)",stdout);
	fflush(stdout);
    }
#endif
}

/* write out the (presumably) revised .newsrc */

void
write_rc()
{
    register NG_NUM tmpng;
    register char *delim;

    rcfp = fopen(rctname, "w");		/* open .newnewsrc */
    if (rcfp == Nullfp) {
	printf(cantrecreate,".newsrc") FLUSH;
	finalize(1);
    }
    if (stat(rcname,&filestat)>=0) {	/* preserve permissions */
	chmod(rctname,filestat.st_mode&0666);
	chown(rctname,filestat.st_uid,filestat.st_gid);	/* if possible */
    }

    /* write out each line*/

    for (tmpng = 0; tmpng < nextrcline; tmpng++) {
	if (rcnums[tmpng]) {
	    delim = rcline[tmpng] + rcnums[tmpng] - 1;
	    *delim = RCCHAR(rcchar[tmpng]);
	    if (rcchar[tmpng] == '0' && delim[2] == '1')
		delim[2] = '0';
	}
	else
	    delim = Nullch;
#ifdef DEBUG
	if (debug & DEB_NEWSRC_LINE)
	    printf("%s\n",rcline[tmpng]) FLUSH;
#endif
	if (fprintf(rcfp,"%s\n",rcline[tmpng]) < 0) {
	write_error:
	    printf(cantrecreate,".newsrc") FLUSH;
	    fclose(rcfp);		/* close .newnewsrc */
	    UNLINK(rctname);
	    finalize(1);
	}
	if (delim) {
	    *delim = '\0';		/* might still need this line */
	    if (rcchar[tmpng] == '0' && delim[2] == '0')
		delim[2] = '1';
	}
    }
    fflush(rcfp);
    if (ferror(rcfp))
	goto write_error;

    fclose(rcfp);			/* close .newnewsrc */
    UNLINK(rcname);
#ifdef HAS_RENAME
    rename(rctname,rcname);
#else
    link(rctname,rcname);
    UNLINK(rctname);
#endif

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
#ifdef HAS_RENAME
    rename(rcname,rctname);
    rename(rcbname,rcname);
#else
    link(rcname,rctname);
    UNLINK(rcname);
    link(rcbname,rcname);
    UNLINK(rcbname);
#endif
}

static void
grow_rc_arrays(newsize)
int newsize;
{
    abs1st = (ART_NUM*)saferealloc((char*)abs1st,
		(MEM_SIZE)newsize * sizeof (ART_NUM));
    ngmax = (ART_NUM*)saferealloc((char*)ngmax,
		(MEM_SIZE)newsize * sizeof (ART_NUM));
    rcline = (char**)saferealloc((char*)rcline,
		(MEM_SIZE)newsize * sizeof (char*));
    toread = (ART_UNREAD*)saferealloc((char*)toread,
		(MEM_SIZE)newsize * sizeof(ART_UNREAD));
    rcchar = (char *) saferealloc(rcchar,
		(MEM_SIZE)newsize * sizeof (char));
    rcnums = (char*)saferealloc(rcnums,
		(MEM_SIZE)newsize * sizeof (char));
    softptr = (ACT_POS*)saferealloc((char*)softptr,
		(MEM_SIZE)newsize * sizeof (ACT_POS));

    bzero((char*)(abs1st+maxrcline), (newsize-maxrcline) * sizeof (ART_NUM));
    bzero((char*)(ngmax+maxrcline), (newsize-maxrcline) * sizeof (ART_NUM));
    maxrcline = newsize;

    return;
}
