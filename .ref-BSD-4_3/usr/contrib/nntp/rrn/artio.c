/* $Header: artio.c,v 4.3 85/05/01 11:35:39 lwall Exp $
 *
 * $Log:	artio.c,v $
 * Revision 4.3  85/05/01  11:35:39  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "server.h"
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
#ifdef SERVER
    static long our_pid;
    char ser_line[256];
#endif
    char artname[32];			/* filename of current article */

#ifdef SERVER
    if (our_pid == 0)
	our_pid = getpid();
#endif

    if (artnum < 1)
	return Nullfp;
    if (openart == artnum) {		/* this article is already open? */
	fseek(artfp,0L,0);		/* just get to the beginning */
	return artfp;			/* and say we succeeded */
    }
    if (artfp != Nullfp) {		/* it was somebody else? */
	fclose(artfp);			/* put them out of their misery */
#ifdef SERVER
	sprintf(artname, "/tmp/rrn%ld.%ld", (long) openart, our_pid);
	UNLINK(artname);
#endif
	openart = 0;			/* and remember them no more */
    }

#ifdef SERVER
    sprintf(artname,"/tmp/rrn%ld.%ld", (long) artnum, our_pid);
    artfp = fopen(artname, "w+");	/* create the temporary article */
    if (artfp == Nullfp) {
	UNLINK(artname);
	return Nullfp;
    }
    sprintf(ser_line, "ARTICLE %d", (long)artnum);
    put_server(ser_line);		/* ask the server for the article */
    if (get_server(ser_line, sizeof(ser_line)) < 0) {
	fprintf(stderr, "rrn: Unexpected close of server socket.\n");
	finalize(1);
    }
    if (*ser_line != CHAR_OK) {		/* and get it's reaction */
	UNLINK(artname);
        return Nullfp;
    }

    for (;;) {
        if (get_server(ser_line, sizeof(ser_line)) < 0) {
	    fprintf(stderr, "rrn: Unexpected close of server socket.\n");
	    finalize(1);
	}
	if (ser_line[0] == '.' && ser_line[1] == '\0')
		break;
	fputs((ser_line[0] == '.' ? ser_line + 1 : ser_line), artfp);
	putc('\n', artfp);
    }

    fseek(artfp, 0L, 0);		/* Then get back to the start */
    openart = artnum;
#else	/* not SERVER */
    sprintf(artname,"%ld",(long)artnum);
					/* produce the name of the article */
    if (artfp = fopen(artname,"r"))	/* if we can open it */
	openart = artnum;		/* remember what we did here */
#endif /* SERVER */
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

