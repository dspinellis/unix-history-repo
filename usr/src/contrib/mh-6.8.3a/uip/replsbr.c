/* replsbr.c - routines to help repl along... */
#ifndef	lint
static char ident[] = "@(#)$Id: replsbr.c,v 1.17 1993/09/04 19:31:32 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/formatsbr.h"
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>		/* off_t */
#include <sys/file.h>		/* L_SET */


extern short    ccto,		/* from repl.c */
                cccc,
                ccme,
                format,
                outputlinelen,
		querysw;
extern char *fcc,
	    *filter,
            *form;

static int   dftype=0;

static char *badaddrs = NULL;
static char *dfhost=NULL;

static struct mailname  mq={NULL};


#define SBUFSIZ 256		
				/* buffer size for content part of header
				 * fields.  We want this to be large
				 * enough so that we don't do a lot of
				 * extra FLDPLUS calls on m_getfld but
				 * small enough so that we don't snarf
				 * the entire message body when we're
				 * not going to use any of it.
				 */

static struct format *fmt;

static int	ncomps = 0;		/* # of interesting components */
static char	**compbuffers = 0; 	/* buffers for component text */
static struct comp **used_buf = 0;	/* stack for comp that use buffers */

static int dat[5];			/* aux. data for format routine */

static char *addrcomps[] = {
    "from",
    "sender",
    "reply-to",
    "to",
    "cc",
    "bcc",
    "resent-from",
    "resent-sender",
    "resent-reply-to",
    "resent-to",
    "resent-cc",
    "resent-bcc",
    NULL
};

static	insert(), replfilter();
/*  */

/* ARGSUSED */

replout (inb, msg, drft)
    register FILE *inb;
    char    *msg;
    char    *drft;
{
    register int  state;
    register int  i;
    register struct comp *cptr;
    register char *tmpbuf;
    register char **nxtbuf;
    register struct comp **savecomp;
    FILE    *out;
    char    name[NAMESZ];
    char    *scanl;
    int	    char_read = 0;
    char    *cp;
    int      format_len;
    register char **ap;

    (void) umask( ~ m_gmprot() );
    if ((out = fopen (drft, "w")) == NULL)
	adios (drft, "unable to create");

    cp = new_fs (form ? form : replcomps, NULLCP, NULLCP);
    format_len = strlen (cp);
    ncomps = fmt_compile (cp, &fmt) + 1;
    if ((nxtbuf = compbuffers = (char **)
	    calloc((unsigned)ncomps,sizeof(char *))) 
	    == (char **)NULL)
	adios (NULLCP, "unable to allocate component buffers");
    if ((savecomp = used_buf = (struct comp **)
	    calloc((unsigned)(ncomps+1),sizeof(struct comp *)))
	    == (struct comp **)NULL)
	adios (NULLCP, "unable to allocate component buffer stack");
    savecomp += ncomps + 1;
    *--savecomp = (struct comp *)0;	/* point at zero'd end minus 1 */
    for (i = ncomps; i--; )
	if ((*nxtbuf++ = malloc( SBUFSIZ )) == NULL)
	    adios (NULLCP, "unable to allocate component buffer");

    nxtbuf = compbuffers;		/* point at start */
    tmpbuf = *nxtbuf++;

    for (ap = addrcomps; *ap; ap++) {
	FINDCOMP (cptr, *ap);
	if (cptr)
	    cptr -> c_type |= CT_ADDR;
    }

    /* ignore any components killed by command line switches */
    if (!ccto) {
	FINDCOMP (cptr, "to");
	if (cptr)
	    cptr->c_name = "";
    }
    if (!cccc) {
	FINDCOMP (cptr, "cc");
	if (cptr)
	    cptr->c_name = "";
    }
    /* set up the "fcc" pseudo-component */
    if (fcc) {
	FINDCOMP (cptr, "fcc");
	if (cptr)
	    cptr->c_text = getcpy (fcc);
    }
    if (cp = getenv("USER")) {
	FINDCOMP (cptr, "user");
	if (cptr)
	    cptr->c_text = getcpy(cp);
    }
    if (!ccme)
	(void) ismymbox ((struct mailname *)0); /* XXX */

    /* pick any interesting stuff out of msg "inb" */
    for (state = FLD;;) {
	state = m_getfld (state, name, tmpbuf, SBUFSIZ, inb);
	switch (state) {
	    case FLD: 
	    case FLDPLUS: 
		/*
		 * if we're interested in this component, save a pointer
		 * to the component text, then start using our next free
		 * buffer as the component temp buffer (buffer switching
		 * saves an extra copy of the component text).
		 */
		if (cptr = wantcomp[CHASH(name)])
		    do {
			if (uleq(name, cptr->c_name)) {
			    char_read += msg_count;
			    if (! cptr->c_text) {
				cptr->c_text = tmpbuf;
				*--savecomp = cptr;
				tmpbuf = *nxtbuf++;
			    } else {
				i = strlen (cp = cptr->c_text) - 1;
				if (cp[i] == '\n')
				    if (cptr->c_type & CT_ADDR) {
					cp[i] = '\0';
					cp = add (",\n\t", cp);
				    } else {
					cp = add ("\t", cp);
				    }
				cptr->c_text = add (tmpbuf, cp);
			    }
			    while (state == FLDPLUS) {
				state = m_getfld (state, name, tmpbuf,
						  SBUFSIZ, inb);
				cptr->c_text = add (tmpbuf, cptr->c_text);
				char_read += msg_count;
			    }
			    break;
			}
		    } while (cptr = cptr->c_next);

		while (state == FLDPLUS)
		    state = m_getfld (state, name, tmpbuf, SBUFSIZ, inb);
		break;

	    case LENERR: 
	    case FMTERR: 
	    case BODY: 
	    case FILEEOF:
		goto finished;

	    default: 
		adios (NULLCP, "m_getfld() returned %d", state);
	}
    }
    /*
     * format and output the header lines.
     */
finished:
    /* if there's a "subject" component, strip any "re:"s off it */
    FINDCOMP (cptr, "subject")
    if (cptr && (cp = cptr->c_text)) {
	register char *sp = cp;

	for (;;) {
	    while (isspace(*cp))
		cp++;
	    if(uprf(cp, "re:"))
		cp += 3;
	    else
		break;
	    sp = cp;
	}
	if (sp != cptr->c_text) {
	    cp = cptr->c_text;
	    cptr->c_text = getcpy (sp);
	    free (cp);
	}
    }
    i = format_len + char_read + 256;
    scanl = malloc ((unsigned)i + 2);
    dat[0] = dat[1] = dat[2] = dat[4] = 0;
    dat[3] = outputlinelen;
    (void) fmtscan (fmt, scanl, i, dat);
    fputs (scanl, out);
    if (badaddrs) {
	fputs ("\nrepl: bad addresses:\n", out);
	fputs ( badaddrs, out);
    }
    if (filter)
	replfilter (inb, out);

    if (ferror (out))
	adios (drft, "error writing");
    (void) fclose (out);

    /* return dynamically allocated buffers */
    free (scanl);
    for (nxtbuf = compbuffers, i = ncomps;
	    cptr = *savecomp++; nxtbuf++, i--)
	free (cptr->c_text);	/* if not nxtbuf, nxtbuf already freed */
    while ( i-- > 0)
        free (*nxtbuf++);	/* free unused nxtbufs */
    free ((char *) compbuffers);
    free ((char *) used_buf);
}

/*  */

static char *buf;		/* our current working buffer */
static char *bufend;		/* end of working buffer */
static char *last_dst;		/* buf ptr at end of last call */
static unsigned int bufsiz=0;	/* current size of buf */

#define BUFINCR 512		/* how much to expand buf when if fills */

#define CPY(s) { cp = (s); while (*dst++ = *cp++) ; --dst; }

/* check if there's enough room in buf for str.  add more mem if needed */
#define CHECKMEM(str) \
	    if ((len = strlen (str)) >= bufend - dst) {\
		int i = dst - buf;\
		int n = last_dst - buf;\
		bufsiz += ((dst + len - bufend) / BUFINCR + 1) * BUFINCR;\
		buf = realloc (buf, bufsiz);\
		dst = buf + i;\
		last_dst = buf + n;\
		if (! buf)\
		    adios (NULLCP, "formataddr: couldn't get buffer space");\
		bufend = buf + bufsiz;\
	    }


/* fmtscan will call this routine if the user includes the function
 * "(formataddr {component})" in a format string.  "orig" is the
 * original contents of the string register.  "str" is the address
 * string to be formatted and concatenated onto orig.  This routine
 * returns a pointer to the concatenated address string.
 *
 * We try to not do a lot of malloc/copy/free's (which is why we
 * don't call "getcpy") but still place no upper limit on the
 * length of the result string.
 */
char *formataddr (orig, str)
    char *orig;
    char *str;
{
    register int  len;
    char    baddr[BUFSIZ],
	    error[BUFSIZ];
    register int  isgroup;
    register char  *dst;
    register char  *cp;
    register char  *sp;
    register struct mailname *mp = NULL;

    /* if we don't have a buffer yet, get one */
    if (bufsiz == 0) {
	buf = malloc (BUFINCR);
	if (! buf)
	    adios (NULLCP, "formataddr: couldn't allocate buffer space");
	last_dst = buf;		/* XXX */
	bufsiz = BUFINCR - 6;  /* leave some slop */
	bufend = buf + bufsiz;
    }
    /*
     * If "orig" points to our buffer we can just pick up where we
     * left off.  Otherwise we have to copy orig into our buffer.
     */
    if (orig == buf)
	dst = last_dst;
    else if (!orig || !*orig) {
	dst = buf;
	*dst = '\0';
    } else {
	dst = last_dst;		/* XXX */
	CHECKMEM (orig);
	CPY (orig);
    }

    /* concatenate all the new addresses onto 'buf' */
    for (isgroup = 0; cp = getname (str); ) {
	if ((mp = getm (cp, dfhost, dftype, AD_NAME, error)) == NULL) {
	    (void) sprintf (baddr, "\t%s -- %s\n", cp, error);
	    badaddrs = add (baddr, badaddrs);
	    continue;
	}
	if (isgroup && (mp->m_gname || !mp->m_ingrp)) {
	    *dst++ = ';';
	    isgroup = 0;
	}
	if (insert (mp)) {
	    /* if we get here we're going to add an address */
	    if (dst != buf) {
		*dst++ = ',';
		*dst++ = ' ';
	    }
	    if (mp->m_gname) {
		CHECKMEM (mp->m_gname);
		CPY (mp->m_gname);
		isgroup++;
	    }
	    sp = adrformat (mp);
	    CHECKMEM (sp);
	    CPY (sp);
	}
    }

    if (isgroup)
	*dst++ = ';';

    *dst = '\0';
    last_dst = dst;
    return (buf);
}
/*  */

static	insert (np)
register struct mailname *np;
{
    char    buffer[BUFSIZ];
    register struct mailname *mp;

    if (np -> m_mbox == NULL)
	return 0;

    for (mp = &mq; mp -> m_next; mp = mp -> m_next) {
#ifdef BERK
	if (uleq (np -> m_mbox, mp -> m_next -> m_mbox))
	    return 0;
#else	/* not BERK */
	if (uleq (np -> m_host, mp -> m_next -> m_host)
		&& uleq (np -> m_mbox, mp -> m_next -> m_mbox))
	    return 0;
#endif	/* BERK */
    }
    if (!ccme && ismymbox (np))
	return 0;

    if (querysw) {
	(void) sprintf (buffer, "Reply to %s? ", adrformat (np));
	if (!gans (buffer, anoyes))
	return 0;
    }
    mp -> m_next = np;
#ifdef	ISI
    if (ismymbox (np))
	ccme = 0;
#endif	/* ISI */
    return 1;
}

/*  */

static	replfilter (in, out)
register FILE *in,
	      *out;
{
    int	    pid;
    char   *mhl;

    if (filter == NULL)
	return;

    if (access (filter, 04) == NOTOK)
	adios (filter, "unable to read");

    mhl = r1bindex (mhlproc, '/');

    rewind (in);
    (void) lseek (fileno(in), (off_t)0, L_SET);
    (void) fflush (out);

    switch (pid = vfork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");

	case OK: 
	    (void) dup2 (fileno (in), fileno (stdin));
	    (void) dup2 (fileno (out), fileno (stdout));
	    closefds (3);

	    execlp (mhlproc, mhl, "-form", filter, "-noclear", NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (mhlproc);
	    _exit (-1);

	default: 
	    if (pidXwait (pid, mhl))
		done (1);
	    (void) fseek (out, 0L, 2);
	    break;
    }
}
