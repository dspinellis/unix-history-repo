#include "parms.h"
#include "structs.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	RCSIDENT
static char rcsid[] = "$Header: times.c,v 1.7 85/01/18 15:39:48 notes Rel $";
#endif	RCSIDENT

/*
 *	getlast(atime, nf, seqon, name)
 *	struct when_f *atime; char *nf, *name
 *		retrieve last time that user was in the notefile.
 *	fixlast(atime, nf, seqon, name)
 *	struct when_f *atime; char *nf, *name
 *		update users last access time.
 *
 *	Restructure of Original effort (Nov 1981 by R. Essick)
 *	Coding:	Ray Essick	January 1982
 */

long    lseek ();					/* make sure gets right type */
char   *malloc ();

getlast (atime, nf, seqon, name)
struct when_f  *atime;
char   *nf,
       *name;
{
    FILE * seqfile;
    FILE * fopen ();
    struct seq_f    entry;
    register int    retcode,
                    i;
    char    fn[WDLEN];
    int     fid,
            size;
    struct stat statval;
    char    holdit[NNLEN + 1];

    static int  nentries = 0;				/* number in file */
    static int  havechecked = 0;			/* for seq file */
    static char lastname[20];				/* last name loaded */
    static struct seq_f *entries = (struct seq_f *) NULL;/* the times */

    *atime = Basetime;					/* pick "old time" */

    if (seqon < NOREADSEQ)				/* don't read */
	return 0;					/* sequencer off */

    if (strcmp (name, lastname) != 0)			/* new user */
    {							/* may never happen */
	havechecked = 0;
	strcpy (lastname, name);
	if (entries)					/* do we have some? */
	{
	    free (entries);				/* return space */
	    entries = (struct seq_f *) NULL;		/* and mark so */
	}
    }

    sprintf (fn, "%s/%s/%s", Mstdir, SEQUENCER, name);
    if (!havechecked)					/* try making incore */
    {
	havechecked++;					/* don't repeat */
	if (stat (fn, &statval) < 0)			/* no file */
	    size = 0;
	else
	    size = statval.st_size;
	if (size != 0)					/* we have a file */
	{
	    nentries = 0;				/* ready to fail */
	    if ((entries = (struct seq_f *) malloc (size)) == NULL)
		goto hardway;
	    if ((fid = open (fn, 0)) < 0)
	    {
		free (entries);				/* return our space */
		goto hardway;				/* couldn't open */
	    }
	    if (read (fid, entries, size) != size)	/* get it all */
	    {
		free (entries);				/* return space */
		close (fid);				/* and the file */
		goto hardway;				/* didn't read all */
	    }
	    nentries = size / (sizeof entry);
	    close (fid);
	}
    }

hardway: 						/* look it up */

/*
 *	Make sure the sequencer entry is short enough 
 *	This turns out to be a constant which can be resolved at 
 *	compile time!
 */
    i = (FILENAMELEN > NNLEN) ? NNLEN : FILENAMELEN;	/* shorter one */
    if (strlen (nf) >= i)				/* 14 before 4.2 */
    {
	strncpy (holdit, nf, i);			/* copy them */
	holdit[i] = '\0';				/* and terminate */
	nf = holdit;					/* move pointer */
    }

    if (nentries == 0)					/* not pre-loaded */
    {
	if ((seqfile = fopen (fn, "r")) == NULL)
	    return (-1);				/* no file, return default */
	do
	    retcode = fread (&entry, sizeof entry, 1, seqfile);
	while (retcode && strcmp (entry.nfname, nf) != 0);

	fclose (seqfile);				/* close the file */

	if (strcmp (entry.nfname, nf) == 0)
	{
	    copydate (&entry.lastin, atime);		/* give him time */
	    return 0;
	}
	else
	    return (-1);
    }
    else						/* search table */
    {
	for (i = 0; i < nentries; i++)
	    if (strcmp (entries[i].nfname, nf) == 0)
	    {
		copydate (&entries[i].lastin, atime);
		return 0;
	    }
    }
    return 0;
}

fixlast (atime, nf, seqon, name)
struct when_f  *atime;
char   *nf,
       *name;
{
    struct seq_f    entry;
    register int    fid;				/* file descriptor */
    register int    atend,
                    i;
    char    fn[WDLEN];
    char    holdit[NNLEN + 1];				/* hold a nf name */

    if (seqon > NOWRITESEQ)				/* if updating */
    {
	sprintf (fn, "%s/%s/%s", Mstdir, SEQUENCER, name);

	if ((fid = open (fn, 2)) < 0)			/* open up */
	{
	    x ((fid = creat (fn, 0666)) < 0, "fixlast: create");
	    x (close (fid) < 0, "fixlast: close I");
	    x ((fid = open (fn, 2)) < 0, "fixlast: open write");
	}

/*
 *	Make sure the sequencer entry is short enough 
 */
	i = (FILENAMELEN > NNLEN) ? NNLEN : FILENAMELEN;/* shorter one */
	if (strlen (nf) >= i)				/* 14 before 4.2 */
	{
	    strncpy (holdit, nf, i);			/* copy them */
	    holdit[i] = '\0';				/* and terminate */
	    nf = holdit;				/* move pointer */
	}

	while ((atend = read (fid, &entry, sizeof entry)) == sizeof entry)
	    if (strcmp (entry.nfname, nf) == 0)
		break;					/* found him */

	x (atend < 0, "fixlast: read error");
	if (atend == sizeof entry)			/* not at the end */
	    x (lseek (fid, -((long) sizeof entry), 1) < 0, "fixlast: reseek");
	else
	{
	    strmove (nf, entry.nfname);			/* build the entry */
	    x (lseek (fid, 0L, 2) < 0, "fixlast: EOF seek");
							/* make sure at end */
	}
	copydate (atime, &entry.lastin);
	x (write (fid, &entry, sizeof entry) != sizeof entry, "fixlast: write");
	x (close (fid) < 0, "fixlast: close II");
    }
}
