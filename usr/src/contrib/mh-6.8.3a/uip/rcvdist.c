/* rcvdist.c - a rcvmail program to distribute messages */
#ifndef	lint
static char ident[] = "@(#)$Id: rcvdist.c,v 1.10 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/formatsbr.h"
#include "../h/rcvmail.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	FORMSW	0
    "form formfile",  4,

#define	HELPSW	1
    "help", 4,

    NULL, 0
};

/*  */

static char backup[BUFSIZ] = "";
static char drft[BUFSIZ] = "";
static char tmpfil[BUFSIZ] = "";

static rcvdistout();
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     i,
            child_id,
            vecp = 1;
    char   *addrs = NULL,
           *cp,
           *form = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *vec[MAXARGS];
    register    FILE * fp;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    vec[vecp++] = --cp;
		    continue;
		case HELPSW: 
		    (void) sprintf (buf,
			    "%s [switches] [switches for postproc] address ...",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	    }
	addrs = addrs ? add (cp, add (", ", addrs)) : getcpy (cp);
    }

/*  */

    if (addrs == NULL)
	adios (NULLCP, "usage: %s [switches] [switches for postproc] address ...",
	    invo_name);

    (void) umask (~m_gmprot ());
    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((fp = fopen (tmpfil, "w+")) == NULL)
	adios (tmpfil, "unable to create");
    (void) cpydata (fileno (stdin), fileno (fp), "message", tmpfil);
    (void) fseek (fp, 0L, 0);
    (void) strcpy (drft, m_tmpfil (invo_name));
    rcvdistout (fp, form, addrs);
    (void) fclose (fp);

    if (distout (drft, tmpfil, backup) == NOTOK)
	done (1);

    vec[0] = r1bindex (postproc, '/');
    vec[vecp++] = "-dist";
    vec[vecp++] = drft;
    vec[vecp] = NULL;

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 
	    admonish (NULLCP, "unable to fork");/* fall */
	case OK: 
	    execvp (postproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (postproc);
	    _exit (1);

	default: 
	    done (pidXwait (child_id, postproc));
    }
/* NOTREACHED */
}

/*  */

/* very similar to routine in replsbr.c */

#define	SBUFSIZ	256

static int outputlinelen = OUTPUTLINELEN;

static struct format *fmt;

static int ncomps = 0;
static char **compbuffers = 0;
static struct comp **used_buf = 0;

static int dat[5];

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


static	rcvdistout (inb, form, addrs)
register FILE *inb;
char   *form,
       *addrs;
{
    register int    char_read = 0,
                    format_len,
                    i,
                    state;
    register char  *tmpbuf,
                  **nxtbuf,
		  **ap;
    char   *cp,
           *scanl,
            name[NAMESZ];
    register struct comp   *cptr,
                          **savecomp;
    FILE   *out;

    if ((out = fopen (drft, "w")) == NULL)
	adios (drft, "unable to create");
    
    cp = new_fs (form ? form : rcvdistcomps, NULLCP, NULLCP);
    format_len = strlen (cp);
    ncomps = fmt_compile (cp, &fmt) + 1;
    if ((nxtbuf = compbuffers = (char **)
	    calloc ((unsigned) ncomps, sizeof (char *)))
	    == (char **)NULL)
	adios (NULLCP, "unable to allocate component buffers");
    if ((savecomp = used_buf = (struct comp **)
	    calloc ((unsigned) (ncomps + 1), sizeof (struct comp *)))
	    == (struct comp **) NULL)
	adios (NULLCP, "unable to allocate component buffer stack");
    savecomp += ncomps + 1;
    *--savecomp = 0;

    for (i = ncomps; i--;)
	if ((*nxtbuf++ = malloc (SBUFSIZ)) == NULL)
	    adios (NULLCP, "unable to allocate component buffer");
    nxtbuf = compbuffers;
    tmpbuf = *nxtbuf++;

    for (ap = addrcomps; *ap; ap++) {
	FINDCOMP (cptr, *ap);
	if (cptr)
	    cptr -> c_type |= CT_ADDR;
    }

    FINDCOMP (cptr, "addresses");
    if (cptr)
	cptr -> c_text = addrs;

    for (state = FLD;;) {
	switch (state = m_getfld (state, name, tmpbuf, SBUFSIZ, inb)) {
	    case FLD: 
	    case FLDPLUS: 
		if (cptr = wantcomp[CHASH (name)])
		    do {
			if (uleq (name, cptr -> c_name)) {
			    char_read += msg_count;
			    if (!cptr -> c_text) {
				cptr -> c_text = tmpbuf;
				*--savecomp = cptr;
				tmpbuf = *nxtbuf++;
			    }
			    else {
				i = strlen (cp = cptr -> c_text) - 1;
				if (cp[i] == '\n')
				    if (cptr -> c_type & CT_ADDR) {
					cp[i] = 0;
					cp = add (",\n\t", cp);
				    }
				    else
					cp = add ("\t", cp);
				cptr -> c_text = add (tmpbuf, cp);
			    }
			    while (state == FLDPLUS) {
				state = m_getfld (state, name, tmpbuf,
						  SBUFSIZ, inb);
				cptr -> c_text = add (tmpbuf, cptr -> c_text);
				char_read += msg_count;
			    }
			    break;
			}
		    } while (cptr = cptr -> c_next);

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
finished: ;

    i = format_len + char_read + 256;
    scanl = malloc ((unsigned) i + 2);
    dat[0] = dat[1] = dat[2] = dat[4] = 0;
    dat[3] = outputlinelen;
    (void) fmtscan (fmt, scanl, i, dat);
    fputs (scanl, out);

    if (ferror (out))
	adios (drft, "error writing");
    (void) fclose (out);

    free (scanl);
    for (nxtbuf = compbuffers, i = ncomps; cptr = *savecomp++; nxtbuf++, i--)
	free (cptr -> c_text);
    while (i-- > 0)
        free (*nxtbuf++);
    free ((char *) compbuffers);
    free ((char *) used_buf);
}

/*  */

void done (status)
register int     status;
{
    if (backup[0])
	(void) unlink (backup);
    if (drft[0])
	(void) unlink (drft);
    if (tmpfil[0])
	(void) unlink (tmpfil);

    exit (status ? RCV_MBX : RCV_MOK);
}
