/* scansbr.c - routines to help scan along... */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>


#define MAXSCANL 256		/* longest possible scan line */
#define SBUFSIZ 128		/* buffer size for content part of header
				 * fields.  We want this to be large
				 * enough so that we don't do a lot of
				 * extra FLDPLUS calls on m_getfld but
				 * small enough so that we don't snarf
				 * the entire message body when we're
				 * only going to display 30 characters
				 * of it.
				 */

/*  */

static struct format *fmt;

static struct comp *datecomp;		/* pntr to "date" comp */
static struct comp *bodycomp;		/* pntr to "body" pseudo-comp 
					 * (if referenced) */
static int	ncomps = 0;		/* # of interesting components */
static char	**compbuffers = 0; 	/* buffers for component text */
static struct comp **used_buf = 0;	/* stack for comp that use buffers */

char    	*scanl = 0;		/* text of most recent scanline */

static int  dat[4];			/* aux. data for format routine */

#ifdef	RPATHS
char   *unixline ();			/* info from UNIX From: line */
#endif	RPATHS

#define FPUTS(buf) {\
		if (fputs(buf,scnout) == EOF)\
		    adios (scnmsg, "write error on");\
		}

/*  */

/* ARGSUSED */

int     scan (inb, innum, outnum, nfs, width, curflg, header, size, noisy)
char	*nfs;
int     innum,
        outnum,
	width,
        curflg,
        header,
	noisy;
long	size;
register FILE   *inb;
{
    int     compnum,
            state;
    register int  i;
    register struct comp *cptr;
    register char *tmpbuf;
    register char **nxtbuf;
    register struct comp **savecomp;
    char    *scnmsg;
    FILE    *scnout;
    char    name[NAMESZ];
    static  int slwidth;
#ifdef RPATHS
    char    *cp;
#endif RPATHS

    /* first-time only initialization */
    if (scanl == NULLCP) {
	if (width == 0) {
	    if ((width = sc_width ()) < WIDTH/2)
		width = WIDTH/2;
	    else if (width > MAXSCANL)
		width = MAXSCANL;
	}
	dat[3] = slwidth = width;
	scanl = (char *)malloc( (unsigned) (slwidth + 2) );
	if (outnum)
	    (void) umask( ~ m_gmprot() );

	ncomps = fmt_compile (nfs, &fmt) + 1;
	FINDCOMP(bodycomp, "body");
	FINDCOMP(datecomp, "date");
	nxtbuf = compbuffers = (char **)calloc((unsigned) ncomps,sizeof(char *));
	used_buf = (struct comp **)calloc((unsigned) (ncomps+1),sizeof(struct comp *));
	used_buf += ncomps+1; *--used_buf = 0;
	for (i = ncomps; i--; )
	    *nxtbuf++ = malloc( SBUFSIZ );
    }
    /* each-message initialization */
    nxtbuf = compbuffers;
    savecomp = used_buf;
    tmpbuf = *nxtbuf++;
    dat[0] = innum? innum : outnum;
    dat[1] = curflg;

    /*
     * get the first field.  If the msg is non-empty and we're doing
     * an "inc", open the output file.
     */
    if ((state = m_getfld (FLD, name, tmpbuf, SBUFSIZ, inb)) == FILEEOF)
	return SCNEOF;

    if (outnum) {
	scnmsg = m_name (outnum);
	if (*scnmsg == '?')	/* msg num out of range */
	    return SCNNUM;
	if ((scnout = fopen (scnmsg, "w")) == NULL)
	    adios (scnmsg, "unable to write");
#ifdef	RPATHS
	if ((cp = unixline ()) && *cp) {
	    FPUTS ("Return-Path: ");
	    FPUTS (cp);
	}
#endif	RPATHS
    }

    /* scan - main loop */
    for (compnum = 1; ; state = m_getfld (state, name, tmpbuf, SBUFSIZ, inb)) {
	switch (state) {
	    case FLD: 
	    case FLDPLUS: 
		compnum++;
		if (outnum) {
		    FPUTS (name);
		    (void) putc (':', scnout);
		    FPUTS (tmpbuf);
		}
		/*
		 * if we're interested in this component, save a pointer
		 * to the component text, then start using our next free
		 * buffer as the component temp buffer (buffer switching
		 * saves an extra copy of the component text).
		 */
		if (cptr = wantcomp[CHASH(name)])
		    do {
			if (uleq(name, cptr->c_name)) {
			    if (! cptr->c_text) {
				cptr->c_text = tmpbuf;
				*--savecomp = cptr;
				tmpbuf = *nxtbuf++;
			    }
			    break;
			}
		    } while (cptr = cptr->c_next);

		while (state == FLDPLUS) {
		    state = m_getfld (state, name, tmpbuf, SBUFSIZ, inb);
		    if (outnum)
			FPUTS (tmpbuf);
		}
		break;

	    case BODY: 
		compnum = -1;
		if (! outnum) {
		    state = FILEEOF; /* stop now if scan cmd */
		    goto finished;
		}
		(void) putc ('\n', scnout);
		FPUTS (tmpbuf);
		/*
		 * performance hack: some people like to run "inc" on
		 * things like net.sources or large digests.  We do a
		 * copy directly into the output buffer rather than
		 * going through an intermediate buffer.
		 *
		 * We need the amount of data m_getfld found & don't
		 * want to do a strlen on the long buffer so there's
		 * a hack in m_getfld to save the amount of data it
		 * returned in the global "msg_count".
		 */
	body: 	;
		while (state == BODY) {
		    if (scnout->_cnt <= 0) {
			if (fflush(scnout) == EOF)
			    adios (scnmsg, "write error on");
		    }
		    state = m_getfld( state, name, scnout->_ptr,
				      -(scnout->_cnt), inb );
		    scnout->_cnt -= msg_count;
		    scnout->_ptr += msg_count;
		}
		goto finished;

	    case LENERR: 
	    case FMTERR: 
		fprintf (stderr, 
			innum ? "??Format error (message %d) in "
			      : "??Format error in ",
			outnum ? outnum : innum);
		fprintf (stderr, "component %d\n", compnum);

		if (outnum) {
		    FPUTS ("\n\nBAD MSG:\n");
		    FPUTS (name);
		    (void) putc ('\n', scnout);
		    state = BODY;
		    goto body;
		}
		/* fall through */

	    case FILEEOF:
		goto finished;

	    default: 
		adios (NULLCP, "getfld() returned %d\n", state);
	}
    }
    /*
     * format and output the scan line.
     */
finished:
    if (noisy) {
	if (bodycomp)
	    bodycomp->c_text = tmpbuf;

	if (size)
	    dat[2] = size;
	else if (outnum)
	    dat[2] = ftell(scnout);

	if ( (datecomp && ! datecomp->c_text) || (!size && !outnum)) {
	    struct stat st;
	    (void) fstat (fileno(inb), &st);
	    if (!size && !outnum)
		dat[2] = st.st_size;
	    if (datecomp) {
		if (! datecomp->c_text) {
		    *datecomp->c_tws = *dlocaltime ((long *) &st.st_mtime);
		    datecomp->c_flags = -1;
		} else {
		    datecomp->c_flags = 0;
		}
	    }
	}
	(void) fmtscan (fmt, scanl, slwidth, dat);
	(void) fputs (scanl, stdout);

	if (bodycomp)
	    bodycomp->c_text = NULLCP;
    }

    /* return dynamically allocated buffers to pool */
    while ( cptr = *savecomp++ ) {
	*--nxtbuf = cptr->c_text;
	cptr->c_text = NULLCP;
    }
    *--nxtbuf = tmpbuf;

    if (outnum)
	if (fclose (scnout) == EOF)
	    adios (scnmsg, "write error on");

    return (state == FILEEOF? SCNMSG : SCNERR);
}

/*  */

/* Cheat:  we are loaded with adrparse, which wants a routine called
   OfficialName().  We call adrparse:getm() with the correct arguments
   to prevent OfficialName() from being called.  Hence, the following
   is to keep the loader happy.
 */

char   *OfficialName (name)
register char  *name;
{
    return name;
}
