static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "newsgate.h"
/*
 *	newsnote - take a note and dump it in a format that news will
 *	understand. Submit the article to the news program as 
 *	defined in newsgate.h
 *
 *	newsresp - similar to newsnote, but it dumps a response instead.
 *
 *	The routines build some title lines and other headers for 
 *	submission to the news insertion program. The rest of the
 *	article is fed to the news program through a pipe.
 *	This turned out to be mucho mucho easier than building the
 *	properly formatted intersystem files.
 *
 *	Original Coding:	Ray Essick	April 1982
 *	Modified to produce cleaner looking output:
 *				Ray Essick	May 1, 1982
 *				(with good help from Brian Redman (harpo!ber)
 */

extern char *mnames[];					/* month names */
FILE * popen ();

newsnote (io, note, notenum, ngroup)
struct io_f *io;
struct note_f  *note;
char   *ngroup;					/* news group submitted to */
{
    FILE * rnews;					/* rnews pipe */
    char    cmdline[CMDLEN];				/* command line  */
    char    line[TITLEN + 50];				/* scratch line */
    int     i;
    char   *p;
    char    buf[TITLEN + 1];			/* hold title for a moment */
    char    auth[SYSSZ + NAMESZ + 3];		/* hold formatted author */
    struct txthead_f    txthead;

    for (i = 0, p = buf; i < len (note->ntitle, TITLEN); i++, p++) {
	*p = note->ntitle[i];				/* make a title */
    }
    *p = '\0';						/* null terminate it */
    sprintf(line, "%c%s %s%c", '"', buf, NFSUFFIX, '"');
#ifndef BNEWS				/* B news gets correct author! */
    sprintf(cmdline, TONEWS, line, ngroup);		/* build cmd line */
#else
    if (strcmp(note->n_id.sys, SYSTEM) != 0) {	/* build an author name */
	sprintf(auth, "%s\!%s", note->n_id.sys, note->n_auth.aname);
    } else {
	sprintf(auth, "%s", note->n_auth.aname);
    }
    sprintf(cmdline, TONEWS, line, ngroup, auth);
#endif BNEWS

    if ((rnews = popen(cmdline, "w")) == NULL) {
	return(-1);				/* didn't work so well */
    }

    gethrec(io, &note->n_addr, &txthead);
    fprintf(rnews, "#N:%s:%ld:%03o:%d\n", note->n_id.sys, note->n_id.uniqid,
	    note->n_stat, txthead.textlen);


    fprintf(rnews, "%s!%s", note->n_id.sys, note->n_auth.aname);

    fprintf(rnews, "    %3s %2d %02d:%02d:00 %4d\n",
	    mnames[note->n_date.w_month],
	    note->n_date.w_day,
	    note->n_date.w_hours,
	    note->n_date.w_mins,
	    note->n_date.w_year);

    fprintf(rnews, "\n");			/* blank line to text */

    pageout(io, &note->n_addr, rnews);		/* dump text */
						/* dump text */


    fprintf(rnews, "\n");			/* ensure newline at end */
    pclose(rnews);				/* close it */
    sleep(10);					/* wait a while */
    return(0);
}

newsresp (io, note, notenum, rsprec, roffset, respnum, ngroup)
struct io_f *io;
struct note_f  *note;
struct resp_f  *rsprec;
char   *ngroup;
{
    char    cmdline[CMDLEN];		/* leggo brand build-a-command */
    char    line[TITLEN + 50];		/* scratch */
    FILE * rnews;
    int     i;
    char   *p;
    char    buf[TITLEN + 10];		/* hold reformatted title */
    char    auth[SYSSZ + NAMESZ + 3];	/* hold formatted author */
    struct txthead_f    txthead;

    p = buf;
    p += strmove ("Re: ", p);		/* put in response to in */
    for (i = 0; i < len(note->ntitle, TITLEN); i++, p++) {
	*p = note->ntitle[i];		/* move title over */
    }
    *p = '\0';				/* and null terminate */
    sprintf(line, "%c%s %s%c", '"', buf, NFSUFFIX, '"');
#ifndef BNEWS				/* Bnews has some advantages */
    sprintf(cmdline, TONEWS, line, ngroup);	/* build cmd line */
#else
    if (strcmp(rsprec->r_id[roffset].sys, SYSTEM) != 0) {
						/* build an author name */
	sprintf(auth, "%s\!%s", rsprec->r_id[roffset].sys,
		rsprec->r_auth[roffset].aname);
    } else {
	sprintf(auth, "%s", rsprec->r_auth[roffset].aname);
    }
    sprintf (cmdline, TONEWS, line, ngroup, auth);
						/* build cmd line */
#endif BNEWS

    if ((rnews = popen(cmdline, "w")) == NULL) {
	return(-1);				/* didn't work so well */
    }

    gethrec(io, &rsprec->r_addr[roffset], &txthead);
    fprintf(rnews, "#R:%s:%ld:%s:%ld:%03o:%d\n",
	    note->n_id.sys, note->n_id.uniqid,
	    rsprec->r_id[roffset].sys,
	    rsprec->r_id[roffset].uniqid,
	    rsprec->r_stat[roffset], txthead.textlen);


    fprintf(rnews, "%s!%s", rsprec->r_id[roffset].sys,
	    rsprec->r_auth[roffset].aname);

    fprintf(rnews, "    %3s %2d %02d:%02d:00 %4d\n",
	    mnames[rsprec->r_when[roffset].w_month],
	    rsprec->r_when[roffset].w_day,
	    rsprec->r_when[roffset].w_hours,
	    rsprec->r_when[roffset].w_mins,
	    rsprec->r_when[roffset].w_year);

    fprintf(rnews, "\n");			/* blank line to text */

    pageout(io, &rsprec->r_addr[roffset], rnews);    /* dump text */


    fprintf(rnews, "\n");		/* make sure ends on a newline */
    pclose(rnews);			/* close it */
    sleep(10);				/* wait a while */
    return(0);
}
