/* $Header: artsrch.c,v 4.3 85/05/01 11:35:47 lwall Exp $
 *
 * $Log:	artsrch.c,v $
 * Revision 4.3  85/05/01  11:35:47  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "search.h"
#include "term.h"
#include "util.h"
#include "intrp.h"
#include "bits.h"
#include "kfile.h"
#include "head.h"
#include "final.h"
#include "cheat.h"
#include "ng.h"
#include "artio.h"
#include "INTERN.h"
#include "artsrch.h"

void
artsrch_init()
{
#ifdef ARTSEARCH
#ifdef ZEROGLOB
    init_compex(&sub_compex);
    init_compex(&art_compex);
#endif
#endif
}

/* search for an article containing some pattern */

#ifdef ARTSEARCH
int
art_search(patbuf,patbufsiz,get_cmd)
char *patbuf;				/* if patbuf != buf, get_cmd must */
int patbufsiz;
int get_cmd;				/*   be set to FALSE!!! */
{
    char *pattern;			/* unparsed pattern */
    register char cmdchr = *patbuf;	/* what kind of search? */
    register char *s;
    bool backward = cmdchr == '?' || cmdchr == Ctl('p');
					/* direction of search */
    COMPEX *compex;			/* which compiled expression */
    char *cmdlst = Nullch;		/* list of commands to do */
    int normal_return = SRCH_NOTFOUND;	/* assume no commands */
    bool saltaway = FALSE;		/* store in KILL file? */
    char howmuch;			/* search just the subjects */
    bool doread;			/* search read articles? */
    bool foldcase = TRUE;		/* fold upper and lower case? */

    int_count = 0;
    if (cmdchr == '/' || cmdchr == '?') {	/* normal search? */
	if (get_cmd && buf == patbuf)
	    if (!finish_command(FALSE))	/* get rest of command */
		return SRCH_ABORT;
	compex = &art_compex;
	if (patbuf[1]) {
	    howmuch = 0;
	    doread = FALSE;
	}
	else {
	    howmuch = art_howmuch;
	    doread = art_doread;
	}
	s = cpytill(buf,patbuf+1,cmdchr);/* ok to cpy buf+1 to buf */
	pattern = buf;
	if (*pattern) {
	    if (*lastpat)
		free(lastpat);
	    lastpat = savestr(pattern);
	}
	if (*s) {			/* modifiers or commands? */
	    for (s++; *s && index("Kharc",*s); s++) {
		if (*s == 'h')		/* scan header */
		    howmuch = 1;
		else if (*s == 'a')	/* scan article */
		    howmuch = 2;
		else if (*s == 'r')	/* scan read articles */
		    doread = TRUE;
		else if (*s == 'K')	/* put into KILL file */
		    saltaway = TRUE;
		else if (*s == 'c')	/* make search case sensitive */
		    foldcase = FALSE;
	    }
	}
	while (isspace(*s) || *s == ':')
	    s++;
	if (*s) {
	    if (*s == 'm' || *s == 'M')
		doread = TRUE;
	    if (*s == 'k')		/* grandfather clause */
		*s = 'j';
	    cmdlst = savestr(s);
	    normal_return = SRCH_DONE;
	}
	art_howmuch = howmuch;
	art_doread = doread;
	if (srchahead)
	    srchahead = -1;
    }
    else {
	register char *h;

	howmuch = 0;			/* just search subjects */
	doread = (cmdchr == Ctl('p'));
	if (cmdchr == Ctl('n'))
	    normal_return = SRCH_SUBJDONE;
	compex = &sub_compex;
	pattern = patbuf+1;
	strcpy(pattern,": *");
	h = pattern + strlen(pattern);
	interp(h,patbufsiz - (h-patbuf),"%s");	/* fetch current subject */
	if (cmdchr == 'K') {
	    saltaway = TRUE;
	    cmdchr = 'k';
	}
	if (cmdchr == 'k') {
	    normal_return = SRCH_DONE;
	    cmdlst = savestr("j");
	    mark_as_read(art);		/* this article has this subject */
	    if (!*h) {
#ifdef VERBOSE
		IF(verbose)
		    fputs("\nCannot delete null subject.\n",stdout) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    fputs("\nNull subject.\n",stdout) FLUSH;
#endif
		return SRCH_ABORT;
	    }
#ifdef VERBOSE
	    else if (verbose)
		printf("\nMarking subject \"%s\" as read.\n",h) FLUSH;
#endif
	}
	else if (!srchahead)
	    srchahead = -1;
	h[24] = '\0';		/* compensate for notesfiles */
	while (*h) {
	    if (index("/\\[.^*$'\"",*h) != Nullch)
		*h++ = '.';
	    else
		h++;
	}
#ifdef DEBUGGING
	if (debug) {
	    printf("\npattern = %s\n",pattern) FLUSH;
	}
#endif
    }
    if ((s = compile(compex,pattern,TRUE,foldcase)) != Nullch) {
					/* compile regular expression */
	printf("\n%s\n",s) FLUSH;
	return SRCH_ABORT;
    }
#ifdef KILLFILES
    if (saltaway) {
	char saltbuf[LBUFLEN];

	s = saltbuf;
	sprintf(s,"/%s/",pattern);
	s += strlen(s);
	if (doread)
	    *s++ = 'r';
	if (howmuch==1)
	    *s++ = 'h';
	else if (howmuch==2)
	    *s++ = 'a';
	*s++ = ':';
	if (!cmdlst)
	    cmdlst = savestr("j");
	safecpy(s,cmdlst,LBUFLEN-(s-saltbuf));
	kf_append(saltbuf);
    }
#endif
    if (cmdlst && index(cmdlst,'='))
	normal_return = SRCH_ERROR;	/* listing subjects is an error? */
    if (get_cmd) {
	fputs("\nSearching...\n",stdout) FLUSH;
					/* give them something to read */
    }
    if (backward) {
	if (cmdlst && art < lastart)
	    art++;			/* include current article */
	if (doread)
	    check_first(absfirst);
    }
    else {
	if (art > lastart)
	    art = (doread ? absfirst : firstart) - 1;
	else if (cmdlst && art > absfirst)
	    art--;			/* include current article */
	check_first(art);
    }
    if (srchahead > 0) {
	if (!backward)
	    art = srchahead - 1;
	srchahead = -1;
    }
    assert(!cmdlst || *cmdlst);
    for (;;) {
	if (int_count) {
	    int_count = 0;
	    if (cmdlst)
		free(cmdlst);
	    return SRCH_INTR;
	}
	if (backward ?
		(--art < absfirst || (!doread && art < firstart)) :
		(++art > lastart)
	  ) {			/* out of articles? */
	    if (cmdlst)
		free(cmdlst);
	    return normal_return;
	}
	/*NOSTRICT*/
	if (doread || !was_read(art)) {
	    if (wanted(compex,art,howmuch)) {
				    /* does the shoe fit? */
		if (cmdlst) {
		    if (perform(cmdlst,TRUE)) {
			if (cmdlst)
			    free(cmdlst);
			return SRCH_INTR;
		    }
		}
		else {
		    if (cmdlst)
			free(cmdlst);
		    return SRCH_FOUND;
		}
	    }
	    else if (!cmdlst && ! (art%50)) {
		printf("...%ld",(long)art);
		fflush(stdout);
	    }
	}
    }
}

/* determine if article fits pattern */
/* returns TRUE if it exists and fits pattern, FALSE otherwise */

bool
wanted(compex, artnum, scope)
COMPEX *compex;
ART_NUM artnum;
char scope;
{
    if (!scope) {
	char subj_buf[266];
	
	strcpy(subj_buf, "Subject: ");
	strncpy(subj_buf+9,fetchsubj(artnum,FALSE,FALSE),256);
#ifdef DEBUGGING
	if (debug & DEB_SEARCH_AHEAD)
	    printf("%s\n",subj_buf) FLUSH;
#endif
	return execute(compex,subj_buf) != Nullch;
    }
#ifdef CACHESUBJ
    else
	fetchsubj(artnum,FALSE,FALSE);/* might as well get subject handy */
#endif
    
    if (artopen(artnum) == Nullfp)	/* ensure that article is open */
	return FALSE;			/* if not, return NO MATCH */
    scope--;
    while (fgets(buf,LBUFLEN,artfp) != Nullch) {
					/* for each line of article */
	if (!scope && index(buf,':') == Nullch && *buf != ' ' && *buf != '\t')
					/* if headers only and out of header */
	    return FALSE;		/* say no go */
	if (execute(compex,buf) != Nullch) {
					/* does pattern matcher match? */
	    return TRUE;		/* say Eureka */
	}
    }
    return FALSE;			/* out of article, so no match */
}
#endif

