#include "parms.h"
#include "structs.h"
#include <ctype.h>
#include <signal.h>

#ifdef	RCSIDENT
static char *rcsident = "$Header: expand.c,v 1.7 85/01/18 15:40:08 notes Rel $";
#endif

/*
 *	Routines to expand notesgroups specifications
 *
 *	Rick L Spickelmier, UC Berkeley
 *	Lou Salkind, NYU
 */

#define		MAXGROUP	400			/* lots of them */


static int  seqtyp = NOSEQ;				/* sequencer mode */
static int  last_group = 0;				/* active in list */
static int  this_group = 0;				/* next one to show */
static struct nflist_f  groups[MAXGROUP];		/* and the groups */

/*
 * set the sequencer type (external interface)
 */

setseq (i)
{
    seqtyp = i;
}

/*
 * add a notesfile to the active list
 */

addgrp (string)
char   *string;
{
    int     i;

    /* see if it already exists */
    for (i = 0; i < last_group; i++)
    {
	if (strcmp (groups[i].nf_name, string) == 0)
	{						/* already in table */
	    groups[i].nf_active = TRUE;			/* deleted earlier? */
	    groups[i].nf_seqmode = seqtyp;
	    return;
	}
    }
    if (last_group >= MAXGROUP)
    {
	printf ("addgrp: array overflow, ignoring %s\n", string);
	return;
    }
    groups[last_group].nf_active = TRUE;
    groups[last_group].nf_seqmode = seqtyp;
    groups[last_group++].nf_name = strsave (string);
}

/*
 * delete the notesfile from the active list
 */

delgroup (string)
char   *string;
{
    register int    i;					/* might as well be fast */

    for (i = 0; i < last_group; i++)
    {
	if (strcmp (groups[i].nf_name, string) == 0)
	{
	    groups[i].nf_active = FALSE;
	    return;
	}
    }
}

/*
 * given a command line argument, expand it into
 * the appropriate sequence command or notesfile
 * specification
 */

expand (argp)
char   *argp;
{
    char   *endp;

    while (1)						/* do entire string */
    {
	while (isspace (*argp))				/* skip trash chars */
	    argp++;
	if (*argp == '\0')				/* fell off end */
	    return;
	endp = argp;
	while (*endp)
	{
	    if (isspace (*endp) || *endp == ',')
	    {
		*endp++ = '\0';
		break;
	    }
	    endp++;					/* now points at next (or NULL) */
	}

	switch (argp[0])				/* on first character */
	{
/*
 *	Parse options that make sense at this point.
 */
	    case '-': 					/* options */
		{
		    switch (argp[1])
		    {
			case 's': 			/* -S-equencer */
			    seqtyp = NORMSEQ;
			    break;
			case 'x': 			/* e-X-tended sequencer */
			    seqtyp = EXTSEQ;
			    break;
			case 'i': 			/* indexing sequencer */
			    seqtyp = INDXSEQ;
			    break;
			case 'n': 			/* -N-o sequencer */
			    seqtyp = NOSEQ;
			    break;

			default: 
			    break;			/* ignore it */
		    }
		    break;
		}

/*
 *	specified a file  the hard way (via colon).  This usually
 *	happens in the NFSEQ definition
 */

	    case ':': 					/* include a file */
		readrc (&argp[1]);			/* do it */
		break;

/*
 *	Eliminate notesfles.  If the arg is "! pattern", we remove the
 *	notesfiles that "pattern" matches from the list of notesfiles
 *	to read.
 *
 */
	    case '!': 					/* eliminate notesfiles */
		{
		    if (patcheck (&argp[1]))		/* wildcard */
			dopat (&argp[1], delgroup);
		    else
			delgroup (&argp[1]);
		    break;
		}
/*
 *	Anything else is just a "pattern" and specifies some notesfiles
 *	to be added to the list.
 */
	    default: 					/* add notesfiles */
		{
		    if (patcheck (argp))		/* wildcard check */
			dopat (argp, addgrp);
		    else
			addgrp (argp);
		    break;
		}
	}
	argp = endp;
    }
							/* NOT REACHED */
}

/*
 * read a file which contains the command line arguments
 */

readrc (s)
char   *s;
{
    FILE * f;
    char    buf[BUFSIZ];

    if ((f = fopen (s, "r")) == NULL)
    {
	fprintf (stderr, "%s: unable to read file ", Invokedas);
	perror (s);					/* and the error */
	return (-1);
    }
    while (fgets (buf, sizeof buf - 1, f))
	expand (buf);
    fclose (f);
    return (0);
}

/*
 *	return the next active notesfile in the list
 */

struct nflist_f *nextgroup ()
{
    while           (this_group < last_group)
    {
	if              (groups[this_group].nf_active)
	                    return (&groups[this_group++]);/* give it */
	else
	    this_group++;				/* try another */
    }
    return ((struct nflist_f *) NULL);			/* no more */
}
