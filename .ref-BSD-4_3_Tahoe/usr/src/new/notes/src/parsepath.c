#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: parsepath.c,v 1.7.0.2 85/03/18 20:57:00 notes Rel $";
#endif	RCSIDENT

/*
 *	Parse the supplied path and from lines to determine the
 *	address of the sender.  Look at the "from" line first
 *	to see if it contains a pure domain-style address. If so,
 *	use that for origsys and authname. Otherwise, fall into
 *	intuiting it from the "newspath" argument.
 *
 *	newsinput calls this with header.path and header.from as
 *	args.  nfmail calls it with from and (null) as args.
 *
 *
 * fromsys:	contains system that sent us the article.
 *		this helps us avoid sending unnecessary copies
 *		of the article over the net.
 *
 * origsys:	contains the system name of the author.  Note
 *		that for mailing lists gatewayed to news, this
 *		is not the same as the host in the article-ID.
 *
 *	Thanks to Lou Salkind for this code.
 */

char    fromsys[SYSSZ + 1];				/* gave it to us */
char    origsys[SYSSZ + 1];				/* started here */
char    authname[NAMESZ + 1];				/* author */

parsepath (newspath, fromline)
char   *newspath;
char   *fromline;
{
    register int    i,
                    j,
                    c;
    register char  *p,
                   *q;
    char    line[CMDLEN];
    int     gotname = 0;				/* not yet */

    if (fromline != (char *) NULL && fromline[0] != '\0')
    {							/* pure domain exists */
	strcpy (line, fromline);			/* hackable copy */
	if ((p = index (line, '<')) != (char *) NULL)	/* get it */
	{
	    if ((q = index (p, '>')) == (char *) NULL)	/* malformed */
		goto usepath;
	    *q = '\0';					/* truncate */
	    p++;					/* skip the < */
	}
	else
	{
	    p = line;					/* from start */
	    while (*p == ' ' || *p == '\t')
		p++;					/* skip leading trash */
	    q = p;
	    while (*q != '\0' && *q != ' ' && *q != '\n' && *q != '\t')
		q++;					/* on to the next */
	    *q = '\0';					/* zap trailing crap */
	}
	if ((p = index (line, '@')) == (char *) NULL)
	    goto usepath;
	*p++ = '\0';					/* split them */
	strncpy (authname, line, NAMESZ);
	authname[NAMESZ - 1] = '\0';			/* truncate */
	strncpy (origsys, p, HOMESYSSZ);
	origsys[HOMESYSSZ - 1] = '\0';			/* truncate */
#ifdef	notdef
	/* 
	 * (no longer.....)
	 * strip the domain part of the address
	 */
	if ((p = index (origsys, '.')) != (char *) NULL)/* find it */
	    *p = '\0';					/* and zap it */
#endif	notdef
	/* 
	 * check to see we got something
	 */
	if (strlen (authname) > 0)			/* system could be null */
	    gotname++;					/* mark as plucked */
    }
    /* 
     * Determine user and the real originating system.
     * Tuned for B news 2.10
     */
usepath: 						/* fall through from above */
    strcpy (line, newspath);
    for (i = 0; line[i]; i++)				/* find string end */
	if (line[i] == ' ' || line[i] == '\t' || line[i] == '\n')
	{
	    line[i] = '\0';
	    break;
	}
    i--;

    /* 
     * Try and parse an author if we don't already have one
     */
    if (!gotname)
    {
	for (j = i; j > 0; j--)				/* find delimiter */
	    if (line[j] == '@' || line[j] == '!' || line[j] == ':')
		break;
	if (j <= 0)
	{						/* user */
	    for (i = 0; i < (NAMESZ - 1); i++)
	    {
		if ((c = line[i]) == '\0')
		    break;
		authname[i] = c;
	    }
	    authname[i] = '\0';
	}
	else
	    if (line[j] == '@')
	    {						/* user@host */
		int     atcount = j;

		line[atcount] = '\0';			/* drop uucp route */
		for (i = atcount - 1; i > 0; i--)
		{
		    if (line[i] == '!' || line[i] == ':')
		    {
			i++;
			break;
		    }
		}
		j = i;					/* copy user */
		for (i = 0; i < (NAMESZ - 1); i++)
		{
		    if ((c = line[j + i]) == '\0')
			break;
		    authname[i] = c;
		}
		authname[i] = '\0';
		j = atcount + 1;			/* copy origsys */
		for (i = 0; i < (SYSSZ - 1); i++)
		{
		    if ((c = line[j + i]) == '\0')
			break;
#ifdef	notdef
		    /* 
		     * (no longer needed....)
		     * TEMPORARY KLUDGE...
		     * throw out domain part until host
		     * name length increased
		     */
		    if (c == '.')
			break;
#endif	notdef
		    origsys[i] = c;
		}
		origsys[i] = '\0';
	    }
	    else
	    {						/* host!user */
		int     delim = j;
		char    sepchar = line[delim];		/* save delimiter */

		line[delim] = '\0';			/* drop uucp route */
		for (i = delim - 1; i > 0; i--)
		{
		    if (line[i] == '!' || line[i] == ':')
		    {
			i++;
			break;
		    }
		}
		j = i;					/* copy host */
		for (i = 0; i < (NAMESZ - 1); i++)
		{
		    if ((c = line[j + i]) == '\0')
			break;
		    origsys[i] = c;
		}
		origsys[i] = '\0';
#ifdef	FULLDOMAIN
		/* 
		 * if the delimiter was a !, we can place the site in
		 * the "UUCP" domain.
		 *
		 * Exception:  if the site name contains "." [as it will once
		 * the USENIX/UUCP project gets going], we don't want to
		 * add a domain since it is probably already there.
		 */
		if (sepchar == '!')			/* some UUCP site */
		{
		    if (index (origsys, '.') == (char *) NULL)
		    {
			strcat (origsys, ".UUCP");
		    }
		    else
		    {
			/* 
			 * already contains full domain spec
			 */
		    }
		}
		else
		{
		    /* 
		     * a BERKnet site. Don't have a handy domain for them.
		     */
		}
#endif	FULLDOMAIN
		j = delim + 1;				/* copy user */
		for (i = 0; i < (SYSSZ - 1); i++)
		{
		    if ((c = line[j + i]) == '\0')
			break;
		    authname[i] = c;
		}
		authname[i] = '\0';
	    }
    }							/* of if !gotname */

    /* 
     * get fromsys, which is the first host in path
     */

    if (p = index (line, '!'))
    {
	*p = '\0';
	strncpy (fromsys, line, SYSSZ - 1);
    }
    else
	strcpy (fromsys, origsys);
}
