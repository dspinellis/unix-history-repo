static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
/*
 *	load a file of generic notes.
 *	This routine will read the file supplied (as an fid)   
 *	and place it into the notefile.
 *	locking is supressed if the lockit flag is false
 *	uids are mapped to zero if the system that the note came from
 *	does not match the local system.
 *
 *	Original coding:	Ray Essick	December 1981
 */

long numin ();			/* declare func in ldmisc.c */

loadem (io, infile, lockit, whofrom, extensive)
struct io_f *io;
FILE * infile;
int lockit;
char   *whofrom;		/* where these notes are being loaded from */
int extensive;
{
    struct note_f   note;
    struct note_f   note2;	/* scratch for chknote */
    struct auth_f   auth;
    struct id_f noteid,
                respid;
    struct daddr_f  where;
    char    line[CMDLEN];
    char    title[TITLEN];
    char    zfrom[SYSSZ];	/* hold system this resp came from */
    struct when_f   ztime,
                    ztime2;
    int     i,
            posit,
            count,
            status;
    char    c,
           *p,
           *q;


    while (sukline (infile, line) != -1) { /* get line which identifies stuff */
	c = line[0];
	switch (c) {
	    case 'N': 
		p = line;
		while (*p++ != ':');	/* skip to (and over) first colon */
		parseid (p, &noteid);

		sukline (infile, line);		/* note title */
		for (i = 0; (i < TITLEN) && line[i] && (line[i] != '\n'); i++)
		    title[i] = line[i];
		for (; i < TITLEN; i++) {
		    title[i] = ' ';			/* space fill title */
		}

		sukline (infile, line);			/* note's author */
		p = line;
		q = auth.aname;
		while ((*q++ = *p++) != ':');
		*--q = '\0';		/* null terminate and overwrite colon */
		auth.aid = numin (p) & UIDMASK;
							/* get user id */
		if (strcmp (SYSTEM, noteid.sys) != 0) {
		    auth.aid = ANONUID;	/* non-local map to effective anon */
		}

		sukline (infile, line);			/* time of writing */
		timein (line, &note.n_date);

		if (extensive) {
		    sukline (infile, line);
		    timein (line, &note.n_rcvd);
		    sukline (infile, line);
		    timein (line, &note.n_lmod);
		    sukline (infile, line);
		    strmove (line, note.n_from);
		} else {
		    strmove (whofrom, note.n_from);
					/* set up who came from */
		}

		sukline (infile, line);			/* status */
		sscanf (line, "%o:%d", &status, &count);


		if (extensive == 0) {
		    getperms (io, 1, noteid.sys);
						/* find out if permitted */
		    if (allow (io, WRITOK) == 0) {
			io->nnotdrop++;	/* count it as dropped */
			break;			/* & skip to next */
		    }
		}
		if (lockit)
		    lock(io, 'n');
		posit = chknote (io, &noteid, &note2);
							/* see if here */
		if (posit == 0) {	/* only if not already in the system */ 
		    for (i = 0; i < SYSSZ; i++) {
			note.n_id.sys[i] = noteid.sys[i];
		    }
		    note.n_id.uniqid = noteid.uniqid;
							 /* copy unique id in */
		    puttrec (io, infile, &where, count); /* suck text */
		    putnote (io, &where, title, status, &note, &auth, NOPOLICY,
			    NOLOCKIT, NOADDID, note.n_from, (extensive == NODETAIL));
		    io->nnotrcvd++;		/* count as a recieved */
		} else {
		    if ((note2.n_stat & ORPHND) && NOT (status & ORPHND)) {
		     			/* extant is orphan, new isnt */
			puttrec (io, infile, &note2.n_addr, count);
					/* suck text */
			gettime (&note2.n_rcvd);
			gettime (&note2.n_lmod);
					/* time stamp it */
			copyauth (&auth, &note2.n_auth);
					/* put correct author */
			note2.n_stat = status;
					/* correct status */
			for (i = 0; i < TITLEN; i++) {
			    note2.ntitle[i] = title[i];
			}
			copydate (&note.n_date, &note2.n_date);
			strmove (note.n_from, note2.n_from);
			putnrec (io, posit, &note2);
						/* and replace */
		    } else {
			for (i = 0; i < count; i++) {
			    getc (infile);		/* skip text */
			}
			printf ("%s: duplicate note recieved id=%s:%ld\n",
				SYSTEM, noteid.sys, noteid.uniqid);
			io->nnotdrop++;		/* count a dropped */
		    }
		}

		if (lockit)
		    unlock(io, 'n');
		break;

	    case 'R': 
		p = line;
		while(*p++ != ':');	/* skip to (and over) first colon */
		parseid(p, &noteid);
		while(*p++ != ':');
		while(*p++ != ':');	/* skip over note identifier */
		parseid(p, &respid);	/* get response identifier */

		sukline(infile, line);	/* response's author */
		p = line;
		q = auth.aname;
		while ((*q++ = *p++) != ':');
		*--q = '\0';		/* null terminate and overwrite colon */
		auth.aid = numin(p) & UIDMASK;
					/* get user id */
		if (strcmp(SYSTEM, note.n_id.sys) != 0) {
		    auth.aid = ANONUID;	/* non-local map to effective anon */
		}

		sukline(infile, line);			/* time of writing */
		timein(line, &ztime);

		if (extensive) {
		    sukline(infile, line);
		    timein(line, &ztime2);
		    sukline(infile, line);
		    strmove(line, zfrom);
		} else {
		    strmove(whofrom, zfrom);	 /* set correct source */
		}

		sukline(infile, line);			/* status */
		sscanf(line, "%o:%d", &status, &count);

		if (lockit)
		    lock(io, 'n');
		posit = chknote(io, &noteid, &note);
						/* see if note is here */
		if (posit == 0) {
		    strmove(noteid.sys, note.n_id.sys);
		    note.n_id.uniqid = noteid.uniqid;
					/* build us a fake note */
		    note.n_nresp = 0;
		    note.n_auth.aid = ANONUID;
		    strcpy(note.n_auth.aname, "Unknown");
		    copydate(&ztime, &note.n_date);
		    status = ORPHND;	/* this note is base of an orpahn */
		    for (i = 0, p = "Orphaned Response"; (i < TITLEN) && *p; p++, i++)
			note.ntitle[i] = *p;
		    for (; i < TITLEN; i++) {
			note.ntitle[i] = ' ';	 /* pad */
		    }
		    where.addr = 0;			/* no text */

		    posit = putnote(io, &where, note.ntitle, status, &note,
			    &note.n_auth, NOPOLICY, NOLOCKIT, NOADDID, whofrom, ADDTIME);
					/* enter it */
		    io->norphans++;	/* bump count of recieved orphans */
		}
		if (chkresp(io, &respid, &note, posit) == 0) {
		    if (extensive == 0) {
			getperms(io, 1, respid.sys);
					/* grab systems permissions */
		    }
		    if (allow(io, RESPOK) || extensive) {
			puttrec(io, infile, &where, count);
							/* read text */
			putresp(io, &where, status, posit, &ztime, &auth,
				&note, 0, &respid, 0, zfrom, (extensive == NODETAIL), &ztime2);
			io->nrsprcvd++;		/* he is a rcvd ! */
		    } else {
			io->nrspdrop++;  /* count non-permitted as dropped */
			for (i = 0; i < count; i++) {
			    getc(infile);		/* skip text */
			}
		    }
		} else {
		    io->nrspdrop++;			/* on the floor */
		    for (i = 0; i < count; i++) {
			getc(infile);			/* skip text */
		    }
		    printf("%s: duplicate response id=%s:%ld to note id=%s:%ld\n",
			    SYSTEM, respid.sys, respid.uniqid, noteid.sys, noteid.uniqid);
		}
		if (lockit)
		    unlock(io, 'n');
		break;

	    default: 
		x (-1, "loadem: bad generic file");
		break;
	}
    }
}
