/* $Id: artio.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "cache.h"
#include "rthread.h"
#include "head.h"
#include "nntp.h"
#include "art.h"
#include "bits.h"
#include "final.h"
#include "INTERN.h"
#include "artio.h"

void
artio_init()
{
    ;
}

/* open an article, unless it's already open */

FILE *
artopen(artnum)
ART_NUM artnum;
{
#ifndef USE_NNTP
    char artname[MAXFILENAME];		/* filename of current article */
#endif
    ARTICLE *ap = find_article(artnum);

    if (!ap || !artnum || (ap->flags & (AF_MISSING|AF_FAKE)) == AF_MISSING) {
	errno = ENOENT;
	return Nullfp;
    }
    if (openart == artnum) {		/* this article is already open? */
	fseek(artfp,0L,0);		/* just get to the beginning */
	return artfp;			/* and say we succeeded */
    }
    if (artfp != Nullfp) {		/* it was somebody else? */
	fclose(artfp);			/* put them out of their misery */
	openart = 0;			/* and remember them no more */
    }
retry_open:
#ifdef USE_NNTP
    artfp = nntp_body(artnum);
#else
    sprintf(artname,"%ld",(long)artnum);
    artfp = fopen(artname,"r");
#endif
    if (!artfp) {
	if (errno == ETIMEDOUT)
	    goto retry_open;
	uncache_article(ap,FALSE);
    } else {
#ifdef LINKART
	char tmpbuf[256];
	char *s;

	if (!fstat(fileno(artfp),&filestat)
	 && filestat.st_size < sizeof tmpbuf) {
	    fgets(tmpbuf,sizeof tmpbuf,artfp);
	    if (*tmpbuf == '/') {	/* is a "link" to another article */
		fclose(artfp);
		if (s=index(tmpbuf,'\n'))
		    *s = '\0';
		if (!(artfp = fopen(tmpbuf,"r"))) {
		    uncache_article(ap,FALSE);
		} else {
		    if (*linkartname)
			free(linkartname);
		    linkartname = savestr(tmpbuf);
		}
	    } else
		fseek(artfp,0L,0);	/* get back to the beginning */
	}
#endif
	openart = artnum;		/* remember what we did here */
    }
    return artfp;			/* and return either fp or NULL */
}
