#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: dropt.c,v 1.7.0.1 85/09/09 18:31:21 notes Rel $";
#endif	RCSIDENT

#define BUFSZ	128

/*
 *	this file processes the director options.
 *
 *	call: contains the io pointer to the file.
 *
 *	allows continued access only if the user if a director.
 *
 *	the functions of the director options includes:
 *	1) granting/denial of director priviledges
 *	2) granting/denial of regular access priviledges
 *	3) changing the director message
 *	4) writing a policy note
 *
 *	Returns: -1 normally
 *		 -4 if the user hit cntrl d ( to total exit)
 *
 *	original author/outliner : Ray Essick may 29, 1981
 *
 */

static int  anonrow,					/* allow anon */
            openrow,					/* is open */
            archrow,					/* is archive */
            keeprow,					/* expire action */
            dirmsgrow,					/* expire w/dirmsg */
            netrow,					/* networked */
            expirerow,					/* expire age */
            longrow,					/* longest ok text */
            worksetrow;					/* working set */
static int  lastrow;

direct (io) struct io_f *io;
{
    int     i;						/* scratch */
    int     c;
    long    expires;
    long    workset;
    long    textlength;					/* scratch */
    char    title[DMLEN + 1];				/* hold new director message */
    char    ntitle[NNLEN + 1];				/* hold note file title */
    struct note_f   note;
    struct auth_f   auth;				/* author of policy note */
    struct daddr_f  where;
    char   *r,
            buff[BUFSZ + 1];
    int     buffptr;
    int     start,
            end,
            nnotes,
            nresps;
    int     redraw;					/* paint screen */

    if (allow (io, DRCTOK) == 0)
    {
	at (0, PROMPTMSGX);
	printf ("Sorry, you are not a director");
	fflush (stdout);
	sleep (2);
	return (-1);
    }

    redraw = 1;
    while (1)
    {
	if (redraw)
	    dirplot (io);
getkey: 						/* suck in a key */
	at (-1, 1);
	printf ("Option:  \010");
	c = gchar ();					/* get command */
	printf ("\010 ");				/* overwrite the character */
	redraw = 0;					/* draw if want it */
	switch (c)
	{
	    case '?': 
	    case 'h': 
		help (DIRHLP);
		redraw++;
		break;

	    case 'r': 					/* replot the screen */
	    case '\f': 					/* control-L also */
		redraw++;
		break;

#ifdef	K_KEY
	    case 'k': 					/* same as q */
#endif	K_KEY
	    case 'q': 					/* leave */
		return (-1);

	    case '\004': 
		return QUITFAST;			/* Universal exit */

	    case '!': 					/* give him a shell */
		gshell ();
		redraw++;
		break;

	    case 'p': 					/* run access lists */
		if (accessedit (io) == QUITFAST)
		    return QUITFAST;			/* doit */
		redraw++;
		break;					/* skipt out of the loop */

	    case 'a': 					/* toggle anonymous option */
		locknf (io, DSCRLOCK);			/* lock the thing for a minute */
		getdscr (io, &io -> descr);		/* get up to date descriptor */
		if (io -> descr.d_stat & ANONOK)
		    io -> descr.d_stat &= NOT ANONOK;
		else
		    io -> descr.d_stat |= ANONOK;
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);
		at (anonrow, 18);
		printf (io -> descr.d_stat & ANONOK ? "ON " : "OFF");
		redraw = 0;
		break;

	    case 'A': 					/* Archive option */
		locknf (io, DSCRLOCK);			/* lock the thing for a minute */
		getdscr (io, &io -> descr);		/* get up to date descriptor */
		if (io -> descr.d_stat & ISARCH)
		    io -> descr.d_stat &= NOT ISARCH;
		else
		    io -> descr.d_stat |= ISARCH;
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);
		at (archrow, 18);
		printf (io -> descr.d_stat & ISARCH ? "YES" : "NO ");
		redraw = 0;
		break;

	    case 'l': 					/* message length */
		while (1)
		{
		    at (lastrow, 10);
		    printf ("New Maximum Message Size: ");
		    ceol ();				/* clear to eol */
		    if (gline (buff, BUFSZ) == 1)	/* empty line */
		    {
			at (lastrow + 1, 10);
			printf ("Maximum Message Size Unchanged");
			goto getkey;
		    }
		    if (sscanf (buff, "%ld", &textlength) == 1)
		    {
			if (textlength <= HARDMAX)	/* too big? */
			    break;
			else
			{
			    at (lastrow + 1, 10);
			    printf ("Maximum Allowed is %d", HARDMAX);
			    continue;
			}
		    }
		    at (lastrow + 1, 10);
		    printf ("Enter an integer or <return>");
		}
		locknf (io, DSCRLOCK);			/* CRITICAL SECTION */
		getdscr (io, &io -> descr);		/* update descriptor */
		io -> descr.d_longnote = textlength;	/* new value */
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);		/* all done ... */
		at (longrow, 27);
		printf ("%ld bytes   ", io -> descr.d_longnote);
		redraw = 0;
		break;

	    case 'c': 					/* compress the notefile */
		redraw = 0;
		if (io -> descr.d_stat & OPEN)
		{
		    at (lastrow, 10);
		    printf ("Notefile must be closed to compress");
		    break;
		}
		else
		{
		    at (lastrow, 10);
		    if (askyn ("Really Compress? (y/n) ") != 'y')
		    {
			at (lastrow + 1, 10);
			printf ("Compress not done");
			break;
		    }
		    at (lastrow + 1, 1);
		    printf ("Compressing ");
		    if (compress (io, LOCKIT, 1, &nnotes, &nresps) >= 0)
		    {
			dirplot (io);			/* show it */
			at (-3, 1);
			printf ("Compress left %d notes and %d responses",
				nnotes, nresps);
		    }
		    else
		    {
			dirplot (io);			/* show page */
			at (-3, 1);
			printf ("Compress not done");
		    }
		    break;
		}
		break;

	    case 'e': 					/* change expiration time */
		while (1)
		{
		    at (lastrow, 10);
		    printf ("New Expiration time: ");
		    ceol ();				/* clear to eol */
		    if (gline (buff, BUFSZ) == 1)	/* empty line */
		    {
			at (lastrow + 1, 10);
			printf ("Expiration Threshold Unchanged");
			goto getkey;
		    }
		    if (!strcmp (buff, "Never") || !strcmp (buff, "never") ||
			    !strcmp (buff, "NEVER"))
		    {
			expires = NEVER;
			break;
		    }
		    if (!strcmp (buff, "Default") || !strcmp (buff, "default") ||
			    !strcmp (buff, "DEFAULT"))
		    {
			expires = 0;
			break;
		    }
		    if (sscanf (buff, "%ld", &expires) == 1)
		    {
			break;
		    }

		    at (lastrow + 1, 10);
		    printf ("Want `default', `never', or a number");
		    at (lastrow + 2, 10);
		    printf ("<return> to leave unchanged");
		}

		locknf (io, DSCRLOCK);			/* critical section */
		getdscr (io, &io -> descr);
		io -> descr.d_archtime = expires;	/* update */
		putdscr (io, &io -> descr);		/* replace */
		unlocknf (io, DSCRLOCK);		/* leave critical */
		at (expirerow, 27);
		switch ((int) (io -> descr.d_archtime))	/* update screen */
		{
		    case NEVER: 
			printf ("Never       ");
			break;
		    case 0: 
			printf ("Default     ");
			break;
		    default: 
			printf ("%ld days     ", io -> descr.d_archtime);
			break;
		}
		redraw = 0;
		break;

	    case 'W': 					/* working Set size */
		while (1)
		{
		    at (lastrow, 10);
		    printf ("New Working Set Size: ");
		    ceol ();				/* clear to eol */
		    if (gline (buff, BUFSZ) == 1)	/* empty line */
		    {
			at (lastrow + 1, 10);
			printf ("Working Set Size Unchanged");
			goto getkey;
		    }
		    if (!strcmp (buff, "Default") || !strcmp (buff, "default") ||
			    !strcmp (buff, "DEFAULT"))
		    {
			workset = 0;
			break;
		    }
		    if (sscanf (buff, "%ld", &workset) == 1)
		    {
			break;
		    }

		    at (lastrow + 1, 10);
		    printf ("Want `default' or a number");
		    at (lastrow + 2, 10);
		    printf ("<return> to leave unchanged");
		}

		locknf (io, DSCRLOCK);			/* critical section */
		getdscr (io, &io -> descr);
		io -> descr.d_workset = workset;	/* update */
		putdscr (io, &io -> descr);		/* replace */
		unlocknf (io, DSCRLOCK);		/* leave critical */
		at (worksetrow, 27);
		switch ((int) io -> descr.d_workset)
		{
		    case 0: 
			printf ("Default      ");
			break;
		    default: 
			printf ("%ld Notes    ", io -> descr.d_workset);
		}
		redraw = 0;
		break;

	    case 'E': 					/* keep/delete/default */
		locknf (io, DSCRLOCK);			/* critical section */
		getdscr (io, &io -> descr);
		switch ((int) io -> descr.d_archkeep)	/* change it */
		{
		    case KEEPNO: 
			io -> descr.d_archkeep = KEEPYES;
			break;
		    case KEEPYES: 
			io -> descr.d_archkeep = KEEPDFLT;
			break;
		    case KEEPDFLT: 
		    default: 
			io -> descr.d_archkeep = KEEPNO;
			break;
		}
		putdscr (io, &io -> descr);		/* replace */
		unlocknf (io, DSCRLOCK);		/* leave critical */
		at (keeprow, 27);
		switch ((int) io -> descr.d_archkeep)	/* update display */
		{
		    case KEEPYES: 
			printf ("ARCHIVE");
			break;
		    case KEEPNO: 
			printf ("DELETE ");
			break;
		    case KEEPDFLT: 
			printf ("Default");
			break;
		    default: 
			printf ("UNKNOWN");
			break;
		}
		redraw = 0;
		break;

	    case 'D': 					/* Archive dirmsg */
		locknf (io, DSCRLOCK);			/* critical section */
		getdscr (io, &io -> descr);
		switch ((int) io -> descr.d_dmesgstat)	/* change it */
		{
		    case DIRNOCARE: 
			io -> descr.d_dmesgstat = DIRON;
			break;
		    case DIRON: 
			io -> descr.d_dmesgstat = DIROFF;
			break;
		    case DIROFF: 
			io -> descr.d_dmesgstat = DIRDFLT;
			break;
		    case DIRDFLT: 
		    default: 
			io -> descr.d_dmesgstat = DIRNOCARE;
			break;
		}
		putdscr (io, &io -> descr);		/* replace */
		unlocknf (io, DSCRLOCK);		/* leave critical */
		at (dirmsgrow, 27);
		switch ((int) io -> descr.d_dmesgstat)
		{
		    case DIRNOCARE: 
			printf ("NOCARE   ");
			break;
		    case DIRON: 
			printf ("ON       ");
			break;
		    case DIROFF: 
			printf ("OFF      ");
			break;
		    case DIRDFLT: 
			printf ("Default  ");
			break;
		    default: 
			printf ("UNKNOWN  ");
			break;
		}
		redraw = 0;
		break;

	    case 'o': 					/* toggle open status */
		locknf (io, DSCRLOCK);
		getdscr (io, &io -> descr);
		if (io -> descr.d_stat & OPEN)
		    io -> descr.d_stat &= NOT OPEN;
		else
		    io -> descr.d_stat |= OPEN;
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);
		at (openrow, 18);
		printf (io -> descr.d_stat & OPEN ? "OPEN  " : "CLOSED");
		redraw = 0;
		break;

	    case 'n': 					/* toggle network status */
		locknf (io, DSCRLOCK);
		getdscr (io, &io -> descr);
		if (io -> descr.d_stat & NETWRKD)
		{
		    io -> descr.d_stat &= NOT NETWRKD;
		}
		else
		{
		    io -> descr.d_stat |= NETWRKD;
		}
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);
		at (netrow, 18);
		printf (io -> descr.d_stat & NETWRKD ? "YES" : "NO ");
		redraw = 0;
		break;


	    case 'm': 					/* collect a new director message */
		redraw++;
		at (lastrow, 10);
		printf ("Enter new director message");
		at (lastrow + 2, 10);
		for (i = 0; i < DMLEN; i++)
		    printf ("-");
		at (lastrow + 1, 10);
		i = gline (title, DMLEN - 1);		/* grab message */
		if (i <= 1)
		    break;				/* no new message */
		locknf (io, DSCRLOCK);			/* mutual exclusion */
		getdscr (io, &io -> descr);		/* get up-to-date */
		strncpy (io -> descr.d_drmes, title, DMLEN);/* replace */
		putdscr (io, &io -> descr);
		unlocknf (io, DSCRLOCK);		/* uncritical now */
		break;

	    case 't': 					/* write title for note file */
		redraw++;
		at (lastrow, 10);
		printf ("Enter new title for notefile");
		at (lastrow + 2, 10);
		for (i = 0; i < NNLEN; i++)
		    printf ("-");
		at (lastrow + 1, 10);
		i = gline (ntitle, NNLEN - 1);		/* suck the title */
		if (i <= 1)
		    break;				/* no new message */
		locknf (io, DSCRLOCK);			/* MUTEX */
		getdscr (io, &io -> descr);		/* current descr */
		strncpy (io -> descr.d_title, ntitle, NNLEN);/* update */
		putdscr (io, &io -> descr);		/* and replace */
		unlocknf (io, DSCRLOCK);		/* uncritical now */
		break;

	    case 'w': 					/* let him write a new policy note */
		if (io -> descr.d_plcy)
		{
		    at (0, PROMPTMSGX);
		    if (askyn ("Rewrite policy? (y/n) :") == 'n')
		    {
			redraw++;
			break;
		    }
		}
		at (0, PROMPTMSGX);
		printf ("\nEdit New Policy Text:\n");
		if (gettext (io, &where, (FILE *) NULL, EDIT) == 0)
		{
		    redraw++;
		    break;
		}
		r = title;
		strcpy (title, "POLICY NOTE");
		gettime (&note.n_date);			/* date of writing */
		getname (&auth, 0);			/* get author */
		putnote (io, &where, title, 0, &note, &auth, 1, 1, 1, System, 1);
		dspnote (io, &note, 0);			/* show it to him */
		redraw++;
		break;

	    case 'z': 					/* zap a lot of notes/responses */
	    case 'u': 					/* undelete a bunch */
		{
		    char   *action;			/* del/undel */

		    redraw++;				/* want to repaint */
		    action = c == 'z' ? "delete" : "un-delete";/* for prompts */
		    at (lastrow, 1);
		    printf ("Enter list of notes to %s: ", action);
		    gline (buff, BUFSZ);		/* grab line */
		    at (lastrow + 1, 1);
		    printf ("Going to %s: %s", action, buff);
		    at (lastrow + 2, 1);
		    if (askyn ("Do you really want to do that? ") != 'y')
			break;				/* chicken out */
		    buffptr = 0;
		    at (lastrow + 3, 1);
		    while (listget (buff, &buffptr, &start, &end))
		    {
			if (start > end)
			{
			    printf ("IGNORING %d-%d", start, end);
			    continue;
			}
			if (start == end)
			    printf ("%d ", start);
			else
			    printf ("%d-%d ", start, end);
			mdelete (io, start, end, c == 'z');/* zap those */
		    }
		    goto getkey;			/* leave this stuff on screen */
		}

	    default: 
		printf ("\07");
		redraw = 0;
		goto getkey;				/* hit a bad key */

	}
    }
}
/*
 *	dirplot - Plot the notesfile status
 */

dirplot (io)
struct io_f *io;
{
    int     atrow;
    int     atcol;

    erase ();
    center (io -> descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
    center (io -> descr.d_drmes, DMLEN, 2, 40 - DMLEN / 2);
    atrow = 4;						/* start filling in */
    atcol = 1;
    at (anonrow = atrow++, atcol);
    printf ("(a) Anonymous:   ");			/* at (3,18); */
    printf (io -> descr.d_stat & ANONOK ? "ON" : "OFF");
    at (openrow = atrow++, atcol);
    printf ("(o) Notesfile:   ");			/* at(4,18); */
    printf (io -> descr.d_stat & OPEN ? "OPEN  " : "CLOSED");
    at (netrow = atrow++, atcol);
    printf ("(n) Networked:   ");			/* at(5,18); */
    printf (io -> descr.d_stat & NETWRKD ? "YES" : "NO ");
    at (archrow = atrow++, atcol);
    printf ("(A) Is Archive:  ");			/* at(6,18); */
    printf (io -> descr.d_stat & ISARCH ? "YES" : "NO");
    at (expirerow = atrow++, atcol);
    printf ("(e) Expiration Threshold: ");		/* at (6,27); */
    switch ((int) (io -> descr.d_archtime))
    {
	case NEVER: 
	    printf ("Never");
	    break;
	case 0: 
	    printf ("Default");
	    break;
	default: 
	    printf ("%ld days", io -> descr.d_archtime);
	    break;
    }
    at (keeprow = atrow++, atcol);
    printf ("(E) Expiration Action:    ");		/* at(?,27); */
    switch ((int) io -> descr.d_archkeep)
    {
	case KEEPYES: 
	    printf ("ARCHIVE");
	    break;
	case KEEPNO: 
	    printf ("DELETE ");
	    break;
	case KEEPDFLT: 
	    printf ("Default");
	    break;
	default: 
	    printf ("UNKNOWN");
	    break;
    }
    at (dirmsgrow = atrow++, atcol);
    printf ("(D) Expire with Dirmsg:   ");		/* at (?,27) */
    switch ((int) io -> descr.d_dmesgstat)
    {
	case DIRNOCARE: 
	    printf ("NOCARE   ");
	    break;
	case DIRON: 
	    printf ("ON       ");
	    break;
	case DIROFF: 
	    printf ("OFF      ");
	    break;
	case DIRDFLT: 
	    printf ("Default  ");
	    break;
	default: 
	    printf ("UNKNOWN  ");
	    break;
    }
    at (worksetrow = atrow++, atcol);
    printf ("(W) Working Set Size:     ");		/* at (5,27) */
    switch ((int) io -> descr.d_workset)
    {
	case 0: 
	    printf ("Default");
	    break;
	default: 
	    printf ("%ld Notes", io -> descr.d_workset);
    }
    at (longrow = atrow++, atcol);
    printf ("(l) Maximum text/article: ");		/* at (6,27) */
    printf ("%ld bytes", io -> descr.d_longnote);

    lastrow = atrow;					/* for queries */

/*
 *	Second Column
 */

    atrow = 4;
    atcol = 40;
    at (atrow++, atcol);
    printf ("Policy Note Exists: %s", io -> descr.d_plcy ? "YES" : "NO");
    at (atrow++, atcol);
    printf ("Next note in slot: %d", io -> descr.d_nnote + 1);
    at (atrow++, atcol);
    printf ("Deleted Notes (holes): %ld  ", io -> descr.d_delnote);
    at (atrow++, atcol);
    printf ("Deleted Responses (holes): %ld  ", io -> descr.d_delresp);
/*
 *	Should we show more statistics here?
 *	Things like orphans, adoptions, etc.
 */

    if (atrow > lastrow)
	lastrow = atrow;
    lastrow++;
}
