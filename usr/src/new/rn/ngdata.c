/* $Header: ngdata.c,v 4.3 85/05/01 11:44:38 lwall Exp $
 *
 * $Log:	ngdata.c,v $
 * Revision 4.3  85/05/01  11:44:38  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "ndir.h"
#include "rcstuff.h"
#include "rn.h"
#include "intrp.h"
#include "final.h"
#include "rcln.h"
#include "INTERN.h"
#include "ngdata.h"

void
ngdata_init()
{
/* The following is only for systems that do not zero globals properly */
#ifdef ZEROGLOB
# ifdef CACHEFIRST
    for (i=0; i<MAXRCLINE; i++)
	abs1st[i] = 0;
# endif
#endif	/* ZEROGLOB */

    /* open the active file */

    actfp = fopen(filexp(ACTIVE),"r");
    if (actfp == Nullfp) {
	printf(cantopen,filexp(ACTIVE)) FLUSH;
	finalize(1);
    }
}

/* find the maximum article number of a newsgroup */

ART_NUM
getngsize(num)
register NG_NUM num;
{
    register int len;
    register char *nam;
    char tmpbuf[80];
    ART_POS oldsoft;

    nam = rcline[num];
    len = rcnums[num] - 1;
    softtries++;
#ifdef DEBUGGING
    if (debug & DEB_SOFT_POINTERS)
	printf("Softptr = %ld\n",(long)softptr[num]) FLUSH;
#endif
    oldsoft = softptr[num];
    if ((softptr[num] = findact(tmpbuf, nam, len, (long)oldsoft)) >= 0) {
	if (softptr[num] != oldsoft) {
	    softmisses++;
	    writesoft = TRUE;
	}
    }
    else {
	softptr[num] = 0;
	if (rcchar[num] == ':')		/* unsubscribe quietly */
	    rcchar[num] = NEGCHAR;
	return TR_BOGUS;		/* well, not so quietly, actually */
    }
	
#ifdef DEBUGGING
    if (debug & DEB_SOFT_POINTERS) {
	printf("Should be %ld\n",(long)softptr[num]) FLUSH;
    }
#endif
#ifdef MININACT
    {
	register char *s;
	ART_NUM tmp;

	for (s=tmpbuf+len+1; isdigit(*s); s++) ;
	if (tmp = atol(s))
#ifdef CACHEFIRST
	    abs1st[num] = tmp;
#else
	    abs1st = tmp;
#endif
    }
#endif
    return atol(tmpbuf+len+1);
}

ACT_POS
findact(outbuf,nam,len,suggestion)
char *outbuf;
char *nam;
int len;
long suggestion;
{
    ACT_POS retval;

    fseek(actfp,100000L,1);	/* hopefully this forces a reread */
    if (suggestion == 0L || fseek(actfp,suggestion,0) < 0 ||
      fgets(outbuf,80,actfp) == Nullch ||
      outbuf[len] != ' ' ||
      strnNE(outbuf,nam,len)) {
#ifdef DEBUGGING
	if (debug & DEB_SOFT_POINTERS)
	    printf("Missed, looking for %s in %sLen = %d\n",nam,outbuf,len)
	      FLUSH;
#endif
	fseek(actfp,0L,0);
#ifndef lint
	retval = (ACT_POS)ftell(actfp);
#else
	retval = Null(ACT_POS);
#endif lint
	while (fgets(outbuf,80,actfp) != Nullch) {
	    if (outbuf[len] == ' ' && strnEQ(outbuf,nam,len))
		return retval;
#ifndef lint
	    retval = (ACT_POS) ftell(actfp);
#endif lint
	}
	return (ACT_POS) -1;		/* well, not so quietly, actually */
    }
    else
#ifndef lint
	return (ACT_POS) suggestion;
#else
	return retval;
#endif lint
    /*NOTREACHED*/
}

/* determine the absolutely first existing article number */

ART_NUM
getabsfirst(ngnum,ngsize)
register NG_NUM ngnum;
ART_NUM ngsize;
{
    register ART_NUM a1st;
#ifndef MININACT
    char dirname[MAXFILENAME];
#endif

#ifdef CACHEFIRST
    if (a1st = abs1st[ngnum])
	return a1st;
#endif
#ifdef MININACT
    getngsize(ngnum);
# ifdef CACHEFIRST
    return abs1st[ngnum];
# else
    return abs1st;
# endif
#else not MININACT
    sprintf(dirname,"%s/%s",spool,getngdir(rcline[ngnum]));
    a1st = getngmin(dirname,0L);
    if (!a1st)				/* nothing there at all? */
	a1st = ngsize+1;		/* aim them at end of newsgroup */
# ifdef CACHEFIRST
    abs1st[ngnum] = a1st;
# endif
    return a1st;
#endif MININACT
}

/* scan a directory for minimum article number greater than floor */

ART_NUM
getngmin(dirname,floor)
char *dirname;
ART_NUM floor;
{
    register DIR *dirp;
    register struct direct *dp;
    register ART_NUM min = 1000000;
    register ART_NUM maybe;
    register char *p;
    char tmpbuf[128];
    
    dirp = opendir(dirname);
    if (!dirp)
	return 0;
    while ((dp = readdir(dirp)) != Null(struct direct *)) {
	if ((maybe = atol(dp->d_name)) < min && maybe > floor) {
	    for (p = dp->d_name; *p; p++)
		if (!isdigit(*p))
		    goto nope;
	    if (*dirname == '.' && !dirname[1])
		stat(dp->d_name, &filestat);
	    else {
		sprintf(tmpbuf,"%s/%s",dirname,dp->d_name);
		stat(tmpbuf, &filestat);
	    }
	    if (! (filestat.st_mode & S_IFDIR))
		min = maybe;
	}
      nope:
	;
    }
    closedir(dirp);
    return min==1000000 ? 0 : min;
}

