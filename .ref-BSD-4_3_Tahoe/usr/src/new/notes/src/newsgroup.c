#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: newsgroup.c,v 1.7 85/01/18 15:21:09 notes Rel $";
#endif	RCSIDENT

/*
 *	newsgroup(lookfor, mapped,direct)
 *
 *	This routine looks in the file specified by NEWSALIAS
 *	for a correspondence between the notesfile name supplied 
 *	and a news(1) newsgroup.
 *	The resultant match is placed where the second parameter points.
 *	In the event of no match, the same name is passed back.
 *
 *	direct gives us the direction of mapping: 
 *	NFNEWS says that lookfor is a notesfile and we find a newsgroup
 *	NEWSNF says that lookfor is a newsgroup and we find a nf
 *	in both cases lookfor is the input and mapped is the output
 *
 *	Original Coding:	Ray Essick	April 7, 1982
 */

newsgroup (lookfor, mapped, direct)
char   *lookfor;
char   *mapped;
{

    FILE * groups;
    char    linebuf[CMDLEN];				/* lines in file */
    char   *p,
           *q,
           *r;
    int     c;

    strcpy (mapped, lookfor);				/* ready to fail */

    sprintf (linebuf, "%s/%s/%s", Mstdir, UTILITY, NEWSALIAS);
    if ((groups = fopen (linebuf, "r")) == NULL)
	return (-1);					/* no file, too bad */

    while (1)
    {
	p = linebuf;					/* start line */
	while ((c = getc (groups)) != EOF && c != '\n')
	    *p++ = c;
	if (c == EOF)
	{
	    fclose (groups);
	    return 0;					/* no match */
	}
	*p = '\0';					/* terminate string */
	if (linebuf[0] == '#' || linebuf[0] == '\0')
	    continue;					/* comment or empty */

	q = linebuf;					/* find colon */
	while (*q != ':' && *q)
	    q++;					/* try next */
	if (*q != ':')					/* formatted ok? */
	{
	    fprintf (stderr, "Bad line in newsgroup file: %s\n", linebuf);
	    continue;					/* skip the line */
	}
	*q++ = '\0';					/* break into parts */

/*
 *	Grab the `response' group if there is one.
 */
	r = q;						/* start here */
	while (*r != ':' && *r)
	    r++;					/* not this one */
	if (*r == ':')					/* we have a response group */
	{
	    *r++ = '\0';				/* null terminate base group */
	    if (!*r)					/* see if empty field */
		r = q;					/* give it base group */
	}
	else
	{
	    r = q;					/* base group */
	}

/*
 *	Now decide which to match and which to fill
 */

	switch (direct)					/* which direction */
	{
	    case NFBASENEWS: 				/* notesfile base note */
		if (strcmp (linebuf, lookfor) == 0)	/* match ? */
		{
		    strcpy (mapped, q);			/* copy it to caller */
		    fclose (groups);
		    return 1;				/* success */
		}
		break;					/* out of switch */

	    case NEWSNF: 				/* newsgroup to notesfile */
		if (strcmp (q, lookfor) == 0)
		{
		    strcpy (mapped, linebuf);		/* move find */
		    fclose (groups);
		    return 1;				/* success */
		}
		break;					/* from switch */

	    case NFRESPNEWS: 				/* nf response to news */
		if (strcmp (linebuf, lookfor) == 0)	/* match ? */
		{
		    strcpy (mapped, r);			/* copy to caller */
		    fclose (groups);
		    return 1;				/* success */
		}
		break;					/* out of switch */

	    default: 
		fclose (groups);
		return (-1);				/* what the heck */

	}
    }
/*	yes, we know that this statement is unreachable! */
    return (-1);					/* to satisfy lint */
}
