#include	"parms.h"
#include	"structs.h"
#include	<sys/types.h>
#include	<sys/stat.h>

#ifdef	RCSIDENT
static char rcsid[] = "$Header: readem.c,v 1.7.0.3 85/10/06 01:42:00 notes Rel $";
#endif	RCSIDENT


/*
*	this particular collection of junk handles the basic idea
*	of what to do when you are showing a note.
*	It displays the note, and then manages to collect enough info
*	from the terminal to either progress to the next note or
*	show some of the responses.
*
*	original author : rob kolstad
*	modified	: ray essick may 22, 1981
*	modified (again): Ray Essick December 1981
*	modified (more):  Ray Essick, February 1982
*
*/
readem (io, readnum, firstdis, resp)
struct io_f *io;
int    *firstdis;
{
    struct note_f   note;
    struct resp_f   rsprec;
    struct io_f io2;
    FILE * txtfile;
    int     rrecnum,
            roffset;
    char    tonf[WDLEN + 1];				/* for forwarding */
    char    ntitle[TITLEN + 20];			/* scratch space */
    char    nfsave[WDLEN + 1];				/* path name for 's' and 'S' */
    int     c;						/* input char */
    char   *p,
           *q;						/* scratch pointers */
    int     replot;					/* whether to change what's on the screen */
    int     toresp;					/* init entry as resp */
    int     forward;					/* scroll forward/backward on deleted note */
    int     toauth,					/* send to author */
            znum,					/* forward as resp to this note */
            znote,
            zresp,					/* scratch for asearch */
            i,
            j,
            wtext;					/* send mail with text */
    char    cmdline[CMDLEN];				/* leggo brand build-a-command */
    int     retcode;


    replot = 1;						/* first pass always writes to the screen */
    retcode = -1;					/* init so grabs character */
    forward = 1;					/* default to scroll forward */
    toresp = (resp != 0);				/* for entry */
    while (1)
    {
	x (readnum < 0, "readem: given bad readnum");
	if (readnum > io -> descr.d_nnote)
	    readnum = io -> descr.d_nnote;
	if (readnum == 0 && io -> descr.d_plcy == 0)	/* empty notesfile */
	    return 0;					/* so back to the index */
	getnrec (io, readnum, &note);
	if (note.n_stat & DELETED)
	    if (forward)
		goto nextnt;				/* forward scroll */
	    else
		goto prevnote;				/* backward scroll */
	if (toresp)
	{
	    toresp = 0;
	    goto showit;
	}
	if (replot)
	    retcode = dspnote (io, &note, readnum);	/* show the note if we need new one */
	replot = 1;					/* reset later if don't want replot */
	forward = 1;
	if (retcode < 0)
	{
    input:  at (0, 1);
#ifdef	PROMPT
	    printf (PROMPT);				/* let him know we're ready */
#endif	PROMPT
	    c = gchar ();
	    printf ("\10 \10");				/* Kurt wants this to go away */
	}
	else
	{
	    c = retcode;
	    retcode = (-1);				/* make sure don't loop! */
	}
	switch (c)
	{
	    case '?': 					/* if he doesn't know what to type */
	    case 'h': 
		help (RDMHLP);				/* print the pseudo-man page */
		goto showit;

	    case 'D': 					/* delete this note/response */
		if (resp)				/* check to see if his note */
		{
		    if ((rsprec.r_auth[roffset].aid & UIDMASK) != globuid)
		    {
			at (0, PROMPTMSGX);
			printf ("Not your response");
			replot = 0;
			continue;
		    }
		}
		else
		{
		    if ((note.n_auth.aid & UIDMASK) != globuid)
		    {
			at (0, PROMPTMSGX);
			printf ("Not your note");
			replot = 0;
			continue;
		    }
		    if (readnum == 0)
		    {
			at (0, PROMPTMSGX);
			printf ("Use 'Z' to delete policy");
			replot = 0;
			continue;
		    }
		}

		at (0, 1);
		if (askyn ("Delete? (y/n):   \b\b") == 'n')
		    goto showit;
		printf ("\r                \r");

		locknf (io, DSCRLOCK);			/* CRITICAL section */
		getnrec (io, readnum, &note);		/* this should catch most */
		getdscr (io, &io -> descr);		/* and an up to date descriptor */
		if (resp)				/* go about deleting it */
		{
		    if (resp == note.n_nresp && inorder (&io -> descr.d_lstxmit, &rsprec.r_when[roffset]))
		    {
			delresp (io, readnum, rrecnum, roffset, 0);
			note.n_nresp--;			/* adjust note response count */
			unlocknf (io, DSCRLOCK);	/* must free up the lock */
			break;				/* show next response */
		    }
		    else
		    {
			at (0, PROMPTMSGX);
			printf ("Can't delete: networked, or not last response");
			replot = 0;
			unlocknf (io, DSCRLOCK);	/* release lock here too */
			continue;
		    }
		}
		else					/* its a note */
		{
		    if (note.n_nresp || inorder (&note.n_date, &io -> descr.d_lstxmit))
		    {
			at (0, PROMPTMSGX);
			printf ("Can't delete; note has responses or is networked");
			replot = 0;
			unlocknf (io, DSCRLOCK);	/* release the lock */
			continue;
		    }
		    delnote (io, readnum++, 0);
		    resp = 0;
		    unlocknf (io, DSCRLOCK);		/* release the lock */
		    continue;
		}

	    case 'E': 					/* edit an article */
		if (resp)				/* check to see if his note */
		{
		    if ((rsprec.r_auth[roffset].aid & UIDMASK) != globuid)
		    {
			at (0, PROMPTMSGX);
			printf ("Not your response");
			replot = 0;
			continue;
		    }
		}
		else
		{
		    if ((note.n_auth.aid & UIDMASK) != globuid)
		    {
			at (0, PROMPTMSGX);
			printf ("Not your note");
			replot = 0;
			continue;
		    }
		    if (readnum == 0)
		    {
			at (0, PROMPTMSGX);
			printf ("Sorry, E doesn't work for policy notes yet");
			replot = 0;
			continue;
		    }
		}

		locknf (io, DSCRLOCK);			/* CRITICAL section */
		getnrec (io, readnum, &note);		/* this should catch most */
		getdscr (io, &io -> descr);		/* and an up to date descriptor */
		if (resp)				/* go about deleting it */
		{
		    if (resp == note.n_nresp && inorder (&io -> descr.d_lstxmit, &rsprec.r_when[roffset]))
		    {
			delresp (io, readnum, rrecnum, roffset, 0);
			note.n_nresp--;			/* adjust note response count */
			unlocknf (io, DSCRLOCK);	/* must free up the lock */
			sprintf (nfsave, "/tmp/nfe%d", getpid ());
							/* build scr file */
			x ((txtfile = fopen (nfsave, "w")) == NULL, "readem: scrfile");
			x (chmod (nfsave, 0666) < 0, "readem: chmod");
			pageout (io, &rsprec.r_addr[roffset], txtfile);
							/* dump it */
			fclose (txtfile);		/* also flushes it */
			x ((txtfile = fopen (nfsave, "r")) == NULL, "readem: edit reopen");
			resp = addresp (io, txtfile, readnum, EDIT);
			getnrec (io, readnum, &note);	/* up to date */
							/* add it back in ! */
			x (unlink (nfsave) < 0, "readem: edit unlink");
			break;				/* show next response */
		    }
		    else
		    {
			at (0, PROMPTMSGX);
			printf ("Can't edit: networked, or not last response");
			replot = 0;
			unlocknf (io, DSCRLOCK);	/* release lock here too */
			continue;
		    }
		}
		else					/* its a note */
		{
		    if (note.n_nresp || inorder (&note.n_date, &io -> descr.d_lstxmit))
		    {
			at (0, PROMPTMSGX);
			printf ("Can't edit; note has responses or is networked");
			replot = 0;
			unlocknf (io, DSCRLOCK);	/* release the lock */
			continue;
		    }
		    delnote (io, readnum++, 0);
		    resp = 0;
		    unlocknf (io, DSCRLOCK);		/* release the lock */
		    sprintf (nfsave, "/tmp/nfe%d", getpid ());
							/* build scr file */
		    x ((txtfile = fopen (nfsave, "w")) == NULL, "readem: scrfile");
		    x (chmod (nfsave, 0666) < 0, "readem: chmod");
		    pageout (io, &note.n_addr, txtfile);
							/* dump it */
		    fclose (txtfile);			/* also flushes it */
		    x ((txtfile = fopen (nfsave, "r")) == NULL, "readem: edit reopen");
		    znum = addnote (io, txtfile, "Edit note text:",
			    "Note title: ", &note.ntitle, EDIT);
		    x (unlink (nfsave) < 0, "readem: edit unlink");
		    if (znum > 0)
			readnum = znum;			/* this is the one */
		    continue;
		}

	    case 'Z': 					/* zap notes/responses - directors only */
							/* kills any note/response */
		getdscr (io, &io -> descr);		/* up to date descriptor */
		if (allow (io, DRCTOK) == 0)
		{
		    at (0, PROMPTMSGX);
		    printf ("Not a director");
		    replot = 0;
		    continue;
		}

		at (0, 1);
		if (askyn ("Delete? (y/n):   \b\b") == 'n')
		    goto showit;			/* replotter */
		printf ("\r                  \r");
/*
 *		should log the deletion here, so the "meta-director" can
 *		watch for fascist directors preying on the peasants.
 */
		if (readnum == 0)			/* deleting policy */
		{
		    locknf (io, DSCRLOCK);		/* lock us up */
		    getdscr (io, &io -> descr);		/* grab up-to-date */
		    io -> descr.d_plcy = 0;		/* its gone now */
		    putdscr (io, &io -> descr);		/* replace descriptor */
		    unlocknf (io, DSCRLOCK);
		    return 0;				/* back to the index */
		}
		if (resp)				/* delete a response */
		{
		    delresp (io, readnum, rrecnum, roffset, 1);
							/* kill it */
		    note.n_nresp--;			/* and response count */
		    break;				/* display next response */
		}
		else
		    delnote (io, readnum++, 1);
		continue;

	    case 'r': 					/* replot the current note/response */
	    case '\f': 					/* everyone else uses ^L, might as well */
	showit: 					/* come here to refill screen */
		if (replot == 0)
		    continue;				/* screen appears fine */
		if (resp)
		    break;				/* show him the response */
		else
		{
		    replot = 1;				/* make sure it gets done */
		    continue;
		}

	nextnt: 
	    case '\r': 					/* wants the next note */
	    case '\n': 
		if (readnum == 0)
		    return 0;				/* policy leaves */
		if (++readnum > io -> descr.d_nnote)
		{
		    *firstdis = io -> descr.d_nnote;
		    return 0;
		}
		resp = 0;				/* reset response index */
		continue;

	    case 'm': 					/* mail a note/response via Unix mail */
		toauth = 0;
		wtext = 0;				/* to others and no text */
		goto sendmail;
	    case 'M': 					/* same as 'm' but with text */
		toauth = 0;
		wtext = 1;				/* to others with text */
		goto sendmail;
	    case 'P': 
		toauth = 1;
		wtext = 1;				/* to author with text */
		goto sendmail;
	    case 'p': 
		toauth = 1;
		wtext = 0;				/* to author, no text */
		goto sendmail;

	sendmail: 					/* jump to here once set mail parms */
		if (resp)
		{
		    strcpy (ntitle, "Re: ");		/* prefix */
		    strcat (ntitle, note.ntitle);	/* append title */
		    mailit (io, &rsprec.r_addr[roffset], &rsprec.r_auth[roffset],
			    &rsprec.r_when[roffset], ntitle, toauth, wtext);
		    break;
		}
		else
		{
		    strncpy (ntitle, note.ntitle, TITLEN);
		    mailit (io, &note.n_addr, &note.n_auth,
			    &note.n_date, ntitle, toauth, wtext);
		}
		goto showit;				/* replot current page */

	    case '!': 					/* wants to fork a shell */
		gshell ();
		goto showit;

	    case 'q': 					/* quit this, maybe whole system */
#ifdef	K_KEY
	    case 'k': 
#endif	K_KEY
		return QUITSEQ;

	    case '\04': 
		return QUITFAST;			/* leave totally */

	    case 'z': 					/* total exit w/update */
		return QUITUPD;

	    case 'Q': 					/* exit system without updating sequencer */
#ifdef	K_KEY
	    case 'K': 
#endif	K_KEY
		return QUITNOSEQ;

	    case 'i': 					/* go back to note index */
		*firstdis = readnum;
		return 0;

	    case '\b': 
	    case '-': 					/* display previous response */
		if (resp <= 0)
		    goto prevnote;			/* '-' at base note */
		if (--resp)
		    break;				/* show the previous response */
		continue;				/* show him the base note */

	prevnote: 					/* display previous note */
		if (readnum == 0)
		    return 0;				/* policy leaves */
		forward = 0;				/* set to scroll backwards on deleted note */
		if (--readnum < 1)
		{
		    readnum = 1;			/* zero is policy, so stop at 1 */
		    forward = 1;			/* bounce off bottom end */
		    continue;				/* go hunt for the right note */
		}
		resp = 0;
		continue;

	    case 'x': 
	    case 'X': 
		if (readnum == 0)
		    return 0;				/* policy leaves */
		retcode = tsearch (io, readnum - 1, c == 'x');
							/* look it up */
		if (retcode <= 0)
		    replot = 0;
		else
		{
		    readnum = retcode;
		    resp = 0;
		}
		goto showit;

	    case 'a': 
	    case 'A': 					/* author search from current spot */
		if (readnum == 0)
		    return 0;				/* not from policy ! */
		znote = readnum;
		zresp = resp;
		if (zresp == 0)
		    znote--;
		else
		    zresp++;				/* select 'next' */
		retcode = asearch (io, &znote, &zresp, (c == 'a'));
							/* look */
		if (retcode < 0)
		{
		    replot = 0;
		    goto showit;			/* didn't want anything */
		}
		if (retcode == 0)
		{
		    replot = 0;
		}
		else
		{
		    readnum = znote;
		    resp = zresp;			/* set returned values */
		    getnrec (io, readnum, &note);	/* grab right descriptor */
		}
		goto showit;				/* and display them */

	    case 'd': 					/* toggle a notes director status */
		if (allow (io, DRCTOK) == 0)
		{					/* tell him what's up */
		    at (0, PROMPTMSGX);
		    printf (" Anonymous: %s   Networked: %s",
			    (io -> descr.d_stat & ANONOK) ? "YES" : "NO",
			    (io -> descr.d_stat & NETWRKD) ? "YES" : "NO");
		    replot = 0;				/* leave on screen */
		    goto showit;
		}
		if (resp == 0)				/* toggle a note */
		{
		    locknf (io, DSCRLOCK);
		    getnrec (io, readnum, &note);
		    if (note.n_stat & DIRMES)
			note.n_stat &= NOT DIRMES;
		    else
			note.n_stat |= DIRMES;
		    putnrec (io, readnum, &note);	/* replace */
		    unlocknf (io, DSCRLOCK);
		    goto showit;
		}
		else					/* toggle a response */
		{
		    locknf (io, DSCRLOCK);		/* this locks the resp index too */
		    getrrec (io, rrecnum, &rsprec);	/* grab that block */
		    if (rsprec.r_stat[roffset] & DIRMES)
			rsprec.r_stat[roffset] &= NOT DIRMES;
		    else
			rsprec.r_stat[roffset] |= DIRMES;
		    putrrec (io, rrecnum, &rsprec);	/* replace */
		    unlocknf (io, DSCRLOCK);
		    goto showit;			/* and redisplay */
		}

	    case 'e': 					/* allow him to edit his title */
		if (readnum == 0)
		    continue;				/* don't touch */
		if (resp)
		    goto badkey;			/* bell and reinput */
		else
		{
		    if (allow (io, DRCTOK) == 0 &&
			    (globuid != (note.n_auth.aid & UIDMASK) ||
							/* check uid */
				strcmp (System, note.n_id.sys) != 0))
							/* other sys */
		    {
			at (0, PROMPTMSGX);
			printf ("Not your note");
			replot = 0;
			continue;
		    }
		    at (0, 1);
		    printf ("New Title: ");
		    if ((i = gline (ntitle, TITLEN - 1)) == 1)
							/* glom onto a title */
			continue;			/* empty title, leave alone */
		    strclean (ntitle);			/* zip controls */
		    locknf (io, DSCRLOCK);
		    getnrec (io, readnum, &note);	/* well, update it */
		    strncpy (note.ntitle, ntitle, TITLEN);
		    note.ntitle[TITLEN - 1] = '\0';	/* null for sure */
		    putnrec (io, readnum, &note);	/* and replace */
		    unlocknf (io, DSCRLOCK);
		    goto showit;			/* replot the message */
		}

	    case 't': 					/* talk to the author of a note */
		if (resp)
		    talkto (&rsprec.r_auth[roffset]);
		else
		    talkto (&note.n_auth);
		goto showit;				/* and replot the current message */

	    case 'W': 					/* write a response with the text */
	    case 'w': 					/* let him write a response */
		getdscr (io, &io -> descr);		/* get up to date */
		if (allow (io, RESPOK) == 0)
		{
		    at (0, PROMPTMSGX);
		    printf ("Sorry, you are not allowed to write");
		    replot = 0;
		    continue;				/* back to key processing */
		}
		if (readnum == 0)
		{
		    at (0, PROMPTMSGX);
		    printf ("No responses allowed to policy note");
		    replot = 0;
		    continue;				/* no responses to policy note */
		}

		if (c == 'w')
		    txtfile = NULL;			/* no preface text */
		else
		{
		    sprintf (cmdline, "/tmp/nfx%d", getpid ());
		    x ((txtfile = fopen (cmdline, "w")) == NULL, "readem: bad scrfile");
		    x (chmod (cmdline, 0666) < 0, "readem: chmod failed");
		    if (resp)
		    {
			preptxt (io, txtfile, &rsprec.r_auth[roffset],
				&rsprec.r_when[roffset], &rsprec.r_addr[roffset], NULL);
		    }
		    else
		    {
			preptxt (io, txtfile, &note.n_auth, &note.n_date, &note.n_addr, note.ntitle);
		    }
		    fclose (txtfile);
		    x ((txtfile = fopen (cmdline, "r")) == NULL, "readem: reopen");
		}
		zresp = addresp (io, txtfile, readnum, EDIT);/* put it in */
		if (zresp > 0)
		    getnrec (io, readnum, &note);	/* update descriptor */

		if (txtfile != NULL)
		{
		    fclose (txtfile);			/* toss out scratch */
		    x (unlink (cmdline) < 0, "readem: couldnt unlink scratch");
		}
		if (zresp)
		    resp = zresp;			/* show the new */
		goto showit;

	    case 'B': 					/* bitch, bitch, bitch */
		if (init (&io2, GRIPES) < 0)		/* check gripe file */
		{
		    at (0, PROMPTMSGX);
		    printf ("No gripe file");
		    replot = 0;
		}
		else
		{
		    addnote (&io2, NULL, "Edit Gripe text:", "Gripe Header: ", NULL, EDIT);
							/* let him put the note in */
		    finish (&io2);			/* close up the gripe file */
		}
		goto showit;

	    case 'C': 					/* copy to other notesfile with editing */
	    case 'c': 					/* copy to other notefile without editing */
		if (c == 'C')
		    wtext = 1;
		else
		    wtext = 0;				/* determine which */
		while (1)
		{
		    printf ("\nCopy to: ");
		    if (gline (tonf, NNLEN) == 1)
			goto showit;			/* gave up */
		    if (init (&io2, tonf) >= 0)
			break;
		    printf ("Can't find notesfile %s\n", tonf);
		}
		sprintf (cmdline, "/tmp/nfx%d", getpid ());
		x ((txtfile = fopen (cmdline, "w")) == NULL, "readem:creat scratch failed");
		x (chmod (cmdline, 0666) < 0, "readem: chmod failed");
		if (resp)
		{
		    preptxt (io, txtfile, &rsprec.r_auth[roffset],
			    &rsprec.r_when[roffset], &rsprec.r_addr[roffset], NULL);
		}
		else
		{
		    preptxt (io, txtfile, &note.n_auth, &note.n_date, &note.n_addr, note.ntitle);
		}
		fclose (txtfile);			/* close it */
		x ((txtfile = fopen (cmdline, "r")) == NULL, "readem: couldnt reopen");
		c = 'n';				/* default to note */
		if (allow (&io2, WRITOK) && allow (&io2, READOK) && allow (&io2, RESPOK))
		    c = askyn ("Copy as Response (y/n)? ");
		if (c == 'n' && allow (&io2, WRITOK))
		    if (!resp && wtext == 0)		/* use old title */
			addnote (&io2, txtfile, NULL, NULL, note.ntitle, wtext);
		    else
			addnote (&io2, txtfile, "Edit copied text:", "Copy Title: ", NULL, wtext);
		else
		    if (c == 'y')
		    {
			if (znum = limindx (&io2))
			    addresp (&io2, txtfile, znum, wtext);
		    }
		    else
		    {
			printf ("You haven't permission");
			c = 'b';			/* leave message */
		    }
		if (strcmp (io -> nf, io2.nf) == 0)	/* if was this notefile */
		    getdscr (io, &io -> descr);		/* get new descriptor */
		finish (&io2);				/* close up that notefile */
		if (txtfile != NULL)
		{
		    fclose (txtfile);			/* throw it away */
		    x (unlink (cmdline) < 0, "readem: couldnt unlink scratch");
		}
		if (c == 'b')
		{
		    replot = 0;
		    continue;				/* leave on screen */
		}
		else
		    goto showit;			/* redo the screen */

	    case 'f': 					/* Forward (copy) string to other notefile w/o edit */
	    case 'F': 					/* Forward (copy) string to other notefile w/edit */
		if (resp)
		{
		    at (0, PROMPTMSGX);
		    printf ("f/F only allowed from base note");
		    replot = 0;
		    continue;
		}
		if (c == 'F')
		    wtext = 1;
		else
		    wtext = 0;				/* determine which */
		while (1)
		{
		    printf ("\nForward to: ");
		    if (gline (tonf, NNLEN) == 1)
			goto showit;			/* gave up */
		    if (init (&io2, tonf) >= 0)
			break;
		    printf ("Can't find notesfile %s\n", tonf);
		}
		sprintf (cmdline, "/tmp/nfx%d", getpid ());
		x ((txtfile = fopen (cmdline, "w")) == NULL, "readem:creat scratch failed");
		x (chmod (cmdline, 0666) < 0, "readem: chmod failed");
		preptxt (io, txtfile, &note.n_auth, &note.n_date, &note.n_addr, note.ntitle);
		fclose (txtfile);			/* close it */
		x ((txtfile = fopen (cmdline, "r")) == NULL, "readem: couldnt reopen");
		c = 'n';
		if (allow (&io2, WRITOK))
		{
		    if (wtext == 0)
			znum = addnote (&io2, txtfile, NULL, NULL, note.ntitle, NOEDIT);
		    else
			znum = addnote (&io2, txtfile, "Edit copy text:", "Copy Title:", NULL, EDIT);
		    fclose (txtfile);
		    x ((txtfile = fopen (cmdline, "w")) == NULL, "readem:creat scratch failed");
		    for (i = 1; i <= note.n_nresp; i++)
		    {
			if (wtext)			/* if editing */
			    printf ("Forwarding response %d of %d",
				    i, note.n_nresp);	/* \n by addresp() below */
			if (lrsp (io, readnum, i, &rsprec, &roffset, &rrecnum) == -1)
			    continue;			/* hit end of chain */
			preptxt (io, txtfile, &rsprec.r_auth[roffset],
				&rsprec.r_when[roffset], &rsprec.r_addr[roffset], NULL);
			fclose (txtfile);
			x ((txtfile = fopen (cmdline, "r")) == NULL, "readem:creat scratch failed");
			addresp (&io2, txtfile, znum, wtext);
			fclose (txtfile);
			x ((txtfile = fopen (cmdline, "w")) == NULL, "readem:creat scratch failed");
		    }
		}
		else
		{
		    printf ("You haven't permission");
		    c = 'b';				/* leave message */
		}
		if (strcmp (io -> nf, io2.nf) == 0)	/* if was this notefile */
		    getdscr (io, &io -> descr);		/* get new descriptor */
		finish (&io2);				/* close up that notefile */
		if (txtfile != NULL)
		{
		    fclose (txtfile);			/* throw it away */
		    x (unlink (cmdline) < 0, "readem: couldnt unlink scratch");
		}
		if (c == 'b')
		{
		    replot = 0;
		    continue;				/* leave on screen */
		}
		else
		    goto showit;			/* redo the screen */


	    case 'N': 					/* go to an archive */
		sprintf (tonf, "%s/%s", ARCHDIR, io -> nf);/* build dest */
		goto donest;				/* share common code */


	    case 'n': 					/* nest notesfiles - a stack */
		at (-1, 10);
		printf ("  New notesfile: ");
		printf ("               \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
		if (gline (tonf, NNLEN) == 1)
		    goto showit;			/* forget it, replot */
	donest: 					/* used by N */
		closenf (io);				/* save fids */
		if ((i = control (tonf, NOSEQ)) == -1)	/* do the other */
		    sleep (1);				/* some error there */
		if (opennf (io, 0) < 0)
		{
		    at (0, PROMPTMSGX);
		    printf ("Couldn't reopen notesfile %s", io -> fullname);
		    fflush (stdout);
		    sleep (2);
		    return QUITNOSEQ;			/* don't update */
		}
		if (i == QUITFAST)			/* he in a hurry? */
		    return QUITFAST;			/* oblige him */
		goto showit;				/* redisplay page */

	    case 's': 					/* place text at end of 'nfsave' */
	    case 'S': 					/* place the whole string */
		at (-1, 1);
		printf ("File name:             \b\b\b\b\b\b\b\b\b\b\b\b");
		znum = gline (nfsave, WDLEN);
		at (-1, 1);
		printf ("%*s", znum + 11, " ");		/* overwrite */
		if (znum == 1)				/* no file */
		{
		    at (0, PROMPTMSGX);			/* tell him didn't do */
		    printf ("No Text Saved");
		    replot = 0;
		    continue;
		}
		p = q = nfsave;				/* kill leading spaces */
		while (*p == ' ')
		    p++;				/* skip them */
		for (; *p; p++, q++)
		    *q = *p;				/* move down */
		*q = '\0';				/* terminate */
		for (--q;; q--)				/* strip trailing */
		{
		    if (*q != ' ')
			break;
		    *q = '\0';				/* strip trailing */
		}
		if (nfsave[0] == '|')			/* pipe */
		{
		    p = "Pipe";
		}
		else
		{
		    struct stat sb;			/* hold stat result */

		    if (stat (nfsave, &sb) == 0)	/* find it? */
			p = "Appended";
		    else
			p = "New File";			/* prolly new */
		}
		if (c == 's')				/* save single page */
		{
		    if (resp)
		    {
			znum = savtxt (io, nfsave, &rsprec.r_auth[roffset],
				&rsprec.r_when[roffset], &rsprec.r_addr[roffset], (char *) NULL);
		    }
		    else
		    {
			znum = savtxt (io, nfsave, &note.n_auth, &note.n_date, &note.n_addr, note.ntitle);
		    }
		}
		else					/* save whole string */
		{
		    znum = savtxt (io, nfsave, &note.n_auth, &note.n_date, &note.n_addr, note.ntitle);

		    for (i = 1; i <= note.n_nresp; i++)
		    {
			if (lrsp (io, readnum, i, &rsprec, &roffset, &rrecnum) == -1)
			    continue;			/* hit end of chain */
			znum += savtxt (io, nfsave, &rsprec.r_auth[roffset],
				&rsprec.r_when[roffset], &rsprec.r_addr[roffset], (char *) NULL);
		    }
		}
		at (0, PROMPTMSGX);
		printf ("Saved %d lines in \"%s\" [%s]", znum, nfsave, p);
		replot = 0;				/* dont erase it */
		continue;				/* don't replot */

	    case 'j': 					/* goto next note/resp */
	    case 'l': 
		if (readnum == 0)
		    return 0;				/* policy returns */
		if (resp == note.n_nresp)
		    goto findnext;			/* at end of the responses for this note */
		if ((resp = nxtresp (io, readnum, resp, &io -> stime)) > 0)
		    break;				/* go show it */
		else
		    goto findnext;			/* try next note ! */

	findnext: 
	    case 'J': 					/* next unread note */
	    case 'L': 					/* like J */
		if (readnum == 0)
		    return 0;				/* policy note returns */
		resp = 0;
		if ((readnum = nxtnote (io, readnum, &io -> stime)) > 0)
		    continue;
		else
		{
		    if (c == 'L' || c == 'l')		/* leave */
			return QUITSEQ;			/* and update... */
		    *firstdis = io -> descr.d_nnote;	/* last index page */
		    return 0;				/* and show it */
		}

	    case '+': 
	    case ';': 
	    case ' ': 
		if (readnum == 0)
		    return 0;				/* such is the fate of policy notes */
		resp++;
		if (resp > note.n_nresp)
		    goto nextnt;
		break;

	    case '*': 					/* skip to last note */
		resp = note.n_nresp;
		break;					/* and show it */

	    case '=': 					/* go back to the base note */
		resp = 0;				/* reset index into responses */
		continue;

	    case '1': 					/* skip n responses */
	    case '2': 
	    case '3': 
	    case '4': 
	    case '5': 
	    case '6': 
	    case '7': 
	    case '8': 
	    case '9': 
		if (note.n_nresp < 1)
		    goto nextnt;
		resp += c - '0';			/* let him skip all over responses */
		if (resp > note.n_nresp)
		    resp = note.n_nresp;		/* dont go past end */
		break;


	    default: 					/* something we haven't covered */
	badkey: 					/* so can jump down here */
		printf ("\07");
		replot = 0;				/* leave whatever is up on the screen */
		continue;
	}
	if (resp > note.n_nresp)
	    resp = note.n_nresp;			/* set to the end */
	if (resp == 0)
	    continue;					/* wound up at base note */
	if (lrsp (io, readnum, resp, &rsprec, &roffset, &rrecnum) == -1)
	{
	    getnrec (io, readnum, &note);		/* get a new descriptor */
	    goto showit;				/* dropped something */
	}
	retcode = dspresp (io, &note, &rsprec, roffset, resp, readnum);
							/* show the darn thing */
	replot = 0;					/* leave the response on the screen */
    }
}
