/* $Header: artio.c,v 4.3 85/05/01 11:35:39 lwall Exp $
 *
 * $Log:	artio.c,v $
 * Revision 4.3  85/05/01  11:35:39  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
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
    char artname[8];			/* filename of current article */

    if (artnum < 1)
	return Nullfp;
    if (openart == artnum) {		/* this article is already open? */
	fseek(artfp,0L,0);		/* just get to the beginning */
	return artfp;			/* and say we succeeded */
    }
    if (artfp != Nullfp) {		/* it was somebody else? */
	fclose(artfp);			/* put them out of their misery */
	openart = 0;			/* and remember them no more */
    }
    sprintf(artname,"%ld",(long)artnum);
					/* produce the name of the article */
    if (artfp = fopen(artname,"r"))	/* if we can open it */
	openart = artnum;		/* remember what we did here */
#ifdef LINKART
    {
	char tmpbuf[256];
	char *s;

	if (fstat(artfp->_file,&filestat))
	    return artfp;
	if (filestat.st_size < (sizeof tmpbuf)) {
	    fgets(tmpbuf,(sizeof tmpbuf),artfp);
	    if (*tmpbuf == '/') {	/* is a "link" to another article */
		fclose(artfp);
		if (s=index(tmpbuf,'\n'))
		    *s = '\0';
		if (!(artfp = fopen(tmpbuf,"r")))
		    openart = 0;
		else {
		    if (*linkartname)
			free(linkartname);
		    linkartname = savestr(tmpbuf);
		}
	    }
	    else
		fseek(artfp,0L,0);		/* get back to the beginning */
	}
    }
#endif
    return artfp;			/* and return either fp or NULL */
}

