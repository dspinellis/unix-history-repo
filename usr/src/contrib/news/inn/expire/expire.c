/*  $Revision: 1.17 $
**
**  Expire news articles.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <errno.h>
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "inndcomm.h"
#include "dbz.h"
#include "qio.h"
#include "macros.h"


/*
**  Stuff that more or less duplicates stuff in innd.
*/
#define NGH_HASH(Name, p, j)    \
	for (p = Name, j = 0; *p; ) j = (j << 5) + j + *p++
#define NGH_SIZE	128
#define NGH_BUCKET(j)	&NGHtable[j & (NGH_SIZE - 1)]

typedef struct _BUFFER {
    int		Size;
    int		Used;
    int		Left;
    char	*Data;
} BUFFER;

typedef struct _NEWSGROUP {
    char		*Name;
    char		*Rest;
    long		Last;
	/* These fields are new. */
    time_t		Keep;
    time_t		Default;
    time_t		Purge;
} NEWSGROUP;

typedef struct _NGHASH {
    int		Size;
    int		Used;
    NEWSGROUP	**Groups;
} NGHASH;


/*
**  Expire-specific stuff.
*/
#define MAGIC_TIME	49710.

typedef struct _BADGROUP {
    struct _BADGROUP	*Next;
    char		*Name;
    BOOL		HasDirectory;
} BADGROUP;

STATIC BADGROUP		*EXPbadgroups;
STATIC BOOL		EXPlinks;
STATIC BOOL		EXPquiet;
STATIC BOOL		EXPsizing;
STATIC BOOL		EXPtracing;
STATIC BOOL		EXPusepost;
STATIC char		EXPnewslib[] = _PATH_NEWSLIB;
STATIC char		ACTIVE[] = _PATH_ACTIVE;
STATIC char		SPOOL[] = _PATH_SPOOL;
STATIC int		nGroups;
STATIC FILE		*EXPunlinkfile;
STATIC long		EXPsaved;
STATIC NEWSGROUP	*Groups;
STATIC NEWSGROUP	EXPdefault;
STATIC NGHASH		NGHtable[NGH_SIZE];
STATIC STRING		EXPreason;
STATIC time_t		EXPremember;
STATIC time_t		Now;

/* Statistics; for -v flag. */
STATIC char		*EXPgraph;
STATIC int		EXPverbose;
STATIC long		EXPprocessed;
STATIC long		EXPunlinked;
STATIC long		EXPhistdrop;
STATIC long		EXPhistremember;
STATIC long		EXPallgone;
STATIC long		EXPstillhere;

STATIC int		EXPsplit();

extern double		atof();



/*
**  Hash a newsgroup and see if we get it.
*/
STATIC NEWSGROUP *
NGfind(Name)
    char		*Name;
{
    register char	*p;
    register int	i;
    unsigned int	j;
    register NEWSGROUP	**ngp;
    char		c;
    NGHASH		*htp;

    /* SUPPRESS 6 *//* Over/underflow from plus expression */
    NGH_HASH(Name, p, j);
    htp = NGH_BUCKET(j);
    for (c = *Name, ngp = htp->Groups, i = htp->Used; --i >= 0; ngp++)
	if (c == ngp[0]->Name[0] && EQ(Name, ngp[0]->Name))
	    return ngp[0];
    return NULL;
}


/*
**  Sorting predicate to put newsgroups in rough order of their activity.
*/
STATIC int
NGcompare(p1, p2)
    POINTER	p1;
    POINTER	p2;
{
    NEWSGROUP	**ng1;
    NEWSGROUP	**ng2;

    ng1 = CAST(NEWSGROUP**, p1);
    ng2 = CAST(NEWSGROUP**, p2);
    return ng1[0]->Last - ng2[0]->Last;
}


/*
**  Build the newsgroup structures from the active file.
*/
STATIC void
BuildGroups(active)
    char		*active;
{
    register NGHASH	*htp;
    register NEWSGROUP	*ngp;
    register char	*p;
    register char	*q;
    register int	i;
    register unsigned	j;
    register int	lines;
    int			NGHbuckets;
    char		*fields[5];

    /* Count the number of groups. */
    for (p = active, i = 0; (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;
    nGroups = i;
    Groups = NEW(NEWSGROUP, i);

    /* Set up the default hash buckets. */
    NGHbuckets = i / NGH_SIZE;
    if (NGHbuckets == 0)
	NGHbuckets = 1;
    for (i = NGH_SIZE, htp = NGHtable; --i >= 0; htp++) {
	htp->Size = NGHbuckets;
	htp->Groups = NEW(NEWSGROUP*, htp->Size);
	htp->Used = 0;
    }

    /* Fill in the array. */
    lines = 0;
    for (p = active, ngp = Groups, i = nGroups; --i >= 0; ngp++, p = q + 1) {
	lines++;
	if ((q = strchr(p, '\n')) == NULL) {
	    (void)fprintf(stderr, "%s: line %d missing newline\n",
		    ACTIVE, lines);
	    exit(1);
	}
	*q = '\0';
	if (EXPsplit(p, ' ', fields, SIZEOF(fields)) != 4) {
	    (void)fprintf(stderr, "%s: line %d wrong number of fields\n",
		    ACTIVE, lines);
	    exit(1);
	}
	ngp->Name = fields[0];
	ngp->Last = atol(fields[1]);
	ngp->Rest = fields[3];

	/* Find the right bucket for the group, make sure there is room. */
	/* SUPPRESS 6 *//* Over/underflow from plus expression */
	NGH_HASH(ngp->Name, p, j);
	htp = NGH_BUCKET(j);
	if (htp->Used >= htp->Size) {
	    htp->Size += NGHbuckets;
	    RENEW(htp->Groups, NEWSGROUP*, htp->Size);
	}
	htp->Groups[htp->Used++] = ngp;
    }

    /* Sort each hash bucket. */
    for (i = NGH_SIZE, htp = NGHtable; --i >= 0; htp++)
    if (htp->Used > 1)
	qsort((POINTER)htp->Groups, (SIZE_T)htp->Used, sizeof htp->Groups[0],
		NGcompare);
}



/*
**  Open a file or give up.
*/
STATIC FILE *
EXPfopen(Remove, Name, Mode)
    BOOL	Remove;
    STRING	Name;
    char	*Mode;
{
    FILE	*F;

    if (Remove && unlink(Name) < 0 && errno != ENOENT)
	(void)fprintf(stderr, "Warning, can't remove %s, %s\n",
		Name, strerror(errno));
    if ((F = fopen(Name, Mode)) == NULL) {
	(void)fprintf(stderr, "Can't open %s in %s mode, %s\n",
		Name, Mode, strerror(errno));
	exit(1);
    }
    return F;
}


/*
**  Split a line at a specified field separator into a vector and return
**  the number of fields found, or -1 on error.
*/
STATIC int
EXPsplit(p, sep, argv, count)
    register char	*p;
    register char	sep;
    register char	**argv;
    register int	count;
{
    register int	i;

    for (i = 1, *argv++ = p; *p; )
	if (*p++ == sep) {
	    if (++i == count)
		/* Overflow. */
		return -1;
	    p[-1] = '\0';
	    for (*argv++ = p; *p == sep; p++)
		continue;
	}
    return i;
}


/*
**  Parse a number field converting it into a "when did this start?".
**  This makes the "keep it" tests fast, but inverts the logic of
**  just about everything you expect.  Print a message and return FALSE
**  on error.
*/
STATIC BOOL
EXPgetnum(line, word, v, name)
    int			line;
    char		*word;
    time_t		*v;
    char		*name;
{
    register char	*p;
    register BOOL	SawDot;
    double		d;

    if (caseEQ(word, "never")) {
	*v = (time_t)0;
	return TRUE;
    }

    /* Check the number.  We don't have strtod yet. */
    for (p = word; ISWHITE(*p); p++)
	continue;
    if (*p == '+' || *p == '-')
	p++;
    for (SawDot = FALSE; *p; p++)
	if (*p == '.') {
	    if (SawDot)
		break;
	    SawDot = TRUE;
	}
	else if (!CTYPE(isdigit, *p))
	    break;
    if (*p) {
	(void)fprintf(stderr, "Line %d, bad `%c' character in %s field\n",
		line, *p, name);
	return FALSE;
    }
    d = atof(word);
    if (d > MAGIC_TIME)
	*v = (time_t)0;
    else
	*v = Now - (time_t)(d * 86400.);
    return TRUE;
}


/*
**  Set the expiration fields for all groups that match this pattern.
*/
STATIC void
EXPmatch(p, v, mod)
    register char	*p;
    register NEWSGROUP	*v;
    register char	mod;
{
    register NEWSGROUP	*ngp;
    register int	i;
    register BOOL	negate;

    negate = *p == '!';
    if (negate)
	p++;
    for (ngp = Groups, i = nGroups; --i >= 0; ngp++)
	if (negate ? !wildmat(ngp->Name, p) : wildmat(ngp->Name, p))
	    if (mod == 'a'
	     || (mod == 'm' && ngp->Rest[0] == NF_FLAG_MODERATED)
	     || (mod == 'u' && ngp->Rest[0] != NF_FLAG_MODERATED)) {
		ngp->Keep      = v->Keep;
		ngp->Default   = v->Default;
		ngp->Purge     = v->Purge;
		if (EXPverbose > 4) {
		    (void)printf("%s", ngp->Name);
		    (void)printf(" %13.13s", ctime(&v->Keep) + 3);
		    (void)printf(" %13.13s", ctime(&v->Default) + 3);
		    (void)printf(" %13.13s", ctime(&v->Purge) + 3);
		    (void)printf(" (%s)\n", p);
		}
	    }
}


/*
**  Parse the expiration control file.  Return TRUE if okay.
*/
STATIC BOOL
EXPreadfile(F)
    register FILE	*F;
{
    register char	*p;
    register int	i;
    register int	j;
    register int	k;
    register char	mod;
    NEWSGROUP		v;
    BOOL		SawDefault;
    char		buff[BUFSIZ];
    char		*fields[6];
    char		**patterns;

    /* Scan all lines. */
    EXPremember = -1;
    SawDefault = FALSE;
    patterns = NEW(char*, nGroups);
    for (i = 1; fgets(buff, sizeof buff, F) != NULL; i++) {
	if ((p = strchr(buff, '\n')) == NULL) {
	    (void)fprintf(stderr, "Line %d too long\n", i);
	    return FALSE;
	}
	*p = '\0';
	if (buff[0] == '\0' || buff[0] == '#')
	    continue;
	if ((j = EXPsplit(buff, ':', fields, SIZEOF(fields))) == -1) {
	    (void)fprintf(stderr, "Line %d too many fields\n", i);
	    return FALSE;
	}

	/* Expired-article remember line? */
	if (EQ(fields[0], "/remember/")) {
	    if (j != 2) {
		(void)fprintf(stderr, "Line %d bad format\n", i);
		return FALSE;
	    }
	    if (EXPremember != -1) {
		(void)fprintf(stderr, "Line %d duplicate /remember/\n", i);
		return FALSE;
	    }
	    if (!EXPgetnum(i, fields[1], &EXPremember, "remember"))
		return FALSE;
	    continue;
	}

	/* Regular expiration line -- right number of fields? */
	if (j != 5) {
	    (void)fprintf(stderr, "Line %d bad format\n", i);
	    return FALSE;
	}

	/* Parse the fields. */
	if (strchr(fields[1], 'M') != NULL)
	    mod = 'm';
	else if (strchr(fields[1], 'U') != NULL)
	    mod = 'u';
	else if (strchr(fields[1], 'A') != NULL)
	    mod = 'a';
	else {
	    (void)fprintf(stderr, "Line %d bad modflag\n", i);
	    return FALSE;
	}
	if (!EXPgetnum(i, fields[2], &v.Keep,    "keep")
	 || !EXPgetnum(i, fields[3], &v.Default, "default")
	 || !EXPgetnum(i, fields[4], &v.Purge,   "purge"))
	    return FALSE;
	/* These were turned into offsets, so the test is the opposie
	 * of what you think it should be.  If Purge isn't forever,
	 * make sure it's greater then the other two fields. */
	if (v.Purge) {
	    /* Some value not forever; make sure other values are in range. */
	    if (v.Keep && v.Keep < v.Purge) {
		(void)fprintf(stderr, "Line %d keep>purge\n", i);
		return FALSE;
	    }
	    if (v.Default && v.Default < v.Purge) {
		(void)fprintf(stderr, "Line %d default>purge\n", i);
		return FALSE;
	    }
	}

	/* Is this the default line? */
	if (fields[0][0] == '*' && fields[0][1] == '\0' && mod == 'a') {
	    if (SawDefault) {
		(void)fprintf(stderr, "Line %d duplicate default\n", i);
		break;
	    }
	    EXPdefault.Keep    = v.Keep;
	    EXPdefault.Default = v.Default;
	    EXPdefault.Purge   = v.Purge;
	    SawDefault = TRUE;
	}

	/* Assign to all groups that match the pattern and flags. */
	if ((j = EXPsplit(fields[0], ',', patterns, nGroups)) == -1) {
	    (void)fprintf(stderr, "Line %d too many patterns\n", i);
	    return FALSE;
	}
	for (k = 0; k < j; k++)
	    EXPmatch(patterns[k], &v, mod);
    }
    DISPOSE(patterns);

    return TRUE;
}


/*
**  Handle a newsgroup that isn't in the active file.
*/
STATIC NEWSGROUP *
EXPnotfound(Entry)
    char		*Entry;
{
    static NEWSGROUP	Removeit;
    register BADGROUP	*bg;
    register char	*p;
    struct stat		Sb;
    char		buff[SPOOLNAMEBUFF];

    /* See if we already know about this group. */
    for (bg = EXPbadgroups; bg; bg = bg->Next)
	if (EQ(Entry, bg->Name))
	    break;
    if (bg == NULL) {
	bg = NEW(BADGROUP, 1);
	bg->Name = COPY(Entry);
	(void)strcpy(buff, bg->Name);
	for (p = buff; *p; p++)
	    if (*p == '.')
		*p = '/';
	bg->HasDirectory = stat(buff, &Sb) >= 0 && S_ISDIR(Sb.st_mode);
	bg->Next = EXPbadgroups;
	EXPbadgroups = bg;
	if (!EXPquiet) {
	    (void)fflush(stdout);
	    (void)fprintf(stderr, "Group not matched (removed?) %s -- %s\n",
		Entry,
		bg->HasDirectory ? "Using default expiration"
				 : "Purging all articles");
	}
    }

    /* Directory still there; use default expiration. */
    if (bg->HasDirectory)
	return &EXPdefault;

    /* No directory -- remove it all now. */
    if (Removeit.Keep == 0) {
	Removeit.Keep = Now;
	Removeit.Default = Now;
	Removeit.Purge = Now;
    }
    return &Removeit;
}


/*
**  Should we keep the specified article?
*/
STATIC BOOL
EXPkeepit(Entry, when, Expires)
    char		*Entry;
    time_t		when;
    time_t		Expires;
{
    register char	*p;
    register NEWSGROUP	*ngp;

    if ((p = strchr(Entry, '/')) == NULL) {
	(void)fflush(stdout);
	(void)fprintf(stderr, "Bad entry, \"%s\"\n", Entry);
	return TRUE;
    }
    *p = '\0';
    if ((ngp = NGfind(Entry)) == NULL)
	ngp = EXPnotfound(Entry);
    *p = '/';

    if (EXPverbose > 2) {
	if (EXPverbose > 3)
	    (void)printf("%s age = %0.2f\n",
		    Entry, (Now - when) / 86400.);
	if (Expires == 0) {
	    if (when < ngp->Default)
		(void)printf("%s too old (no exp)\n", Entry);
	}
	else {
	    if (when < ngp->Purge)
		(void)printf("%s later than purge\n", Entry);
	    if (when > ngp->Keep)
		(void)printf("%s earlier than min\n", Entry);
	    if (Now > Expires)
		(void)printf("%s later than header\n", Entry);
	}

    }

    /* If no expiration, make sure it wasn't posted before the default. */
    if (Expires == 0)
	return when >= ngp->Default;

    /* Make sure it's not posted before the purge cut-off and
     * that it's not due to expire. */
    return when >= ngp->Purge && (Expires >= Now || when >= ngp->Keep);
}


/*
**  An article can be removed.  Either print a note, or actually remove it.
**  Also fill in the article size.
*/
STATIC void
EXPremove(p, size)
    char		*p;
    long		*size;
{
    register char	*q;
    struct stat		Sb;

    /* Turn into a filename and get the size if we need it. */
    for (q = p; *q; q++)
	if (*q == '.')
	    *q = '/';
    if (EXPsizing && *size < 0 && stat(p, &Sb) >= 0)
	*size = (int)(((long)Sb.st_size >> 10) + (((long)Sb.st_size >> 9) & 1));

    if (EXPverbose > 1)
	(void)printf("\tunlink %s\n", p);
    EXPunlinked++;

    if (EXPtracing) {
	(void)printf("%s\n", p);
	return;
    }
    if (EXPunlinkfile) {
	(void)fprintf(EXPunlinkfile, "%s\n", p);
	if (!ferror(EXPunlinkfile))
	    return;
	(void)fprintf(stderr, "Can't write to -z file, %s\n",
		strerror(errno));
	(void)fprintf(stderr, "(Will ignore it for rest of run.)\n");
	(void)fclose(EXPunlinkfile);
	EXPunlinkfile = NULL;
    }
    if (unlink(p) < 0 && errno != ENOENT)
	(void)fprintf(stderr, "Can't unlink %s, %s\n", p, strerror(errno));
}


/*
**  Do the work of expiring one line.
*/
STATIC BOOL
EXPdoline(out, line, length, arts)
    FILE		*out;
    char		*line;
    int			length;
    char		**arts;
{
    static char		IGNORING[] = "Ignoring bad line, \"%.20s...\"\n";
    static long		Offset;
    static BUFFER	New;
    register char	*p;
    register char	*q;
    register char	*first;
    register int	i;
    int			count;
    char		*fields[4];
    time_t		Arrived;
    time_t		Expires;
    time_t		Posted;
    time_t		when;
    long		where;
    long		size;
    datum		key;
    datum		value;
    char 		date[20];

    /* Split up the major fields. */
    i = EXPsplit(line, HIS_FIELDSEP, fields, SIZEOF(fields));
    if (i != 2 && i != 3) {
	(void)fprintf(stderr, IGNORING, line);
	return TRUE;
    }

    /* Split up the time field, robustly. */
    if ((p = strchr(fields[1], HIS_SUBFIELDSEP)) == NULL) {
	/* One sub-field:  when the article arrived. */
	Arrived = atol(fields[1]);
	Expires = 0;
	Posted = Arrived;
    }
    else {
	*p = '\0';
	Arrived = atol(fields[1]);
	*p++ = HIS_SUBFIELDSEP;
	if ((q = strchr(p, HIS_SUBFIELDSEP)) == NULL) {
	    /* Two sub-fields:  arrival and expiration. */
	    Expires = EQ(p, HIS_NOEXP) ? 0 : atol(p);
	    Posted = Arrived;
	}
	else {
	    /* All three sub-fields:  arrival, expiration, posted. */
	    *q = '\0';
	    Expires = EQ(p, HIS_NOEXP) ? 0 : atol(p);
	    *q++ = HIS_SUBFIELDSEP;
	    Posted = atol(q);
	}
    }

    if (i == 2) {
	/* History line for already-expired article. */
	if (Arrived < EXPremember) {
	    if (EXPverbose > 3)
		(void)printf("forget: %s\n", line);
	    EXPhistdrop++;
	    return TRUE;
	}

	/* Not time to forget about this one yet. */
	if (out) {
	    where = Offset;
	    (void)fprintf(out, "%s%c%s\n", fields[0], HIS_FIELDSEP, fields[1]);
	    Offset += strlen(fields[0]) + 1 + strlen(fields[1]) + 1;
	    if (EXPverbose > 3)
		(void)printf("remember: %s\n", line);
	    EXPhistremember++;
	}
    }
    else {
	/* Active article -- split up the file entries. */
	count = EXPsplit(fields[2], ' ', arts, nGroups);
	if (count == -1) {
	    (void)fprintf(stderr, IGNORING, line);
	    return TRUE;
	}
	EXPprocessed++;
	when = EXPusepost ? Posted : Arrived;

	/* Get space to hold the remaining file name entries. */
	if (New.Data == NULL) {
	    New.Size = length;
	    New.Data = NEW(char, New.Size);
	}
	else if (New.Size < length) {
	    New.Size = length;
	    RENEW(New.Data, char, New.Size);
	}

	/* The "first" variable tells us if we haven't saved the first
	 * article yet.  This only matters if we're doing link-saving. */
	first = EXPlinks && count > 1 ? arts[0] : (char *)NULL;

	/* Loop over all file entries, see if we should keep each one. */
	for (size = -1, q = New.Data, i = 0; i < count; i++) {
	    p = arts[i];
	    if (*p == '\0')
		/* Shouldn't happen. */
		continue;
	    if (EXPkeepit(p, when, Expires)) {
		if (EXPverbose > 1)
		    (void)printf("keep %s\n", p);
		if (first != NULL) {
		    /* Keeping one and haven't kept the first; so save it. */
		    if (i > 0)
			q += strlen(strcpy(q, first));
		    first = NULL;
		}
		if (q > New.Data)
		    *q++ = ' ';
		q += strlen(strcpy(q, p));
		continue;
	    }

	    /* Don't delete the file if preserving symbolic links to it. */
	    if (EXPlinks && i == 0 && count > 1)
		continue;
	    EXPremove(arts[i], &size);
	}

	/* If saving links and didn't have to save the leader, delete it. */
	if (EXPlinks && first != NULL)
	    EXPremove(first, &size);

	if (q == New.Data) {
	    if (EXPsizing && size > 0)
		EXPsaved += size;
	    if (EXPremember > 0 && out != NULL) {
		where = Offset;
		(void)sprintf(date, "%ld", (long)Arrived);
		(void)fprintf(out, "%s%c%s%c%s\n",
			fields[0], HIS_FIELDSEP,
			date, HIS_SUBFIELDSEP, HIS_NOEXP);
		Offset += strlen(fields[0]) + 1
			+ strlen(date) + 1 + STRLEN(HIS_NOEXP) + 1;
	        if (EXPverbose > 3)
		    (void)printf("remember history: %s%c%s%c%s\n",
			    fields[0], HIS_FIELDSEP,
			    date, HIS_SUBFIELDSEP, HIS_NOEXP);
		EXPallgone++;
	    }
	}
	else if (out) {
	    where = Offset;
	    (void)fprintf(out, "%s%c%s%c%s\n",
		    fields[0], HIS_FIELDSEP, fields[1], HIS_FIELDSEP,
		    New.Data);
	    Offset += strlen(fields[0]) + 1 + strlen(fields[1]) + 1
		    + strlen(New.Data) + 1;
	    if (EXPverbose > 3)
		(void)printf("remember article: %s%c%s%c%s\n",
			fields[0], HIS_FIELDSEP, fields[1], HIS_FIELDSEP,
			New.Data);
	    EXPstillhere++;
	}
    }

    if (out == NULL)
	return TRUE;

    if (ferror(out)) {
	(void)fprintf(stderr, "Can't write new history, %s\n",
		strerror(errno));
	return FALSE;
    }

    /* Set up the DBZ data.  We don't have to sanitize the Message-ID
     * since it had to have been clean to get in there. */
    key.dptr = fields[0];
    key.dsize = strlen(key.dptr) + 1;
    value.dptr = (char *)&where;
    value.dsize = sizeof where;
    if (EXPverbose > 4)
	(void)printf("\tdbz %s@%ld\n", key.dptr, where);
    if (dbzstore(key, value) < 0) {
	(void)fprintf(stderr, "Can't store key, %s\n", strerror(errno));
	return FALSE;
    }
    return TRUE;
}



/*
**  Clean up link with the server and exit.
*/
STATIC NORETURN
CleanupAndExit(Server, Paused, x)
    BOOL	Server;
    BOOL	Paused;
    int		x;
{
    FILE	*F;

    if (Server)
	(void)ICCreserve("");
    if (Paused && ICCgo(EXPreason) != 0) {
	(void)fprintf(stderr, "Can't unpause server, %s\n",
		strerror(errno));
	x = 1;
    }
    if (Server && ICCclose() < 0) {
	(void)fprintf(stderr, "Can't close communication link, %s\n",
		strerror(errno));
	x = 1;
    }
    if (EXPunlinkfile && fclose(EXPunlinkfile) == EOF) {
	(void)fprintf(stderr, "Can't close -z file, %s\n", strerror(errno));
	x = 1;
    }

    /* Report stats. */
    if (EXPverbose) {
	(void)printf("Article lines processed %8ld\n", EXPprocessed);
	(void)printf("Articles retained       %8ld\n", EXPstillhere);
	(void)printf("Entries expired         %8ld\n", EXPallgone);
	(void)printf("Files unlinked          %8ld\n", EXPunlinked);
	(void)printf("Old entries dropped     %8ld\n", EXPhistdrop);
	(void)printf("Old entries retained    %8ld\n", EXPhistremember);
    }

    /* Append statistics to a summary file */
    if (EXPgraph) {
	F = EXPfopen(FALSE, EXPgraph, "a");
	(void)fprintf(F, "%ld %ld %ld %ld %ld %ld %ld\n",
		(long)Now, EXPprocessed, EXPstillhere, EXPallgone,
		EXPunlinked, EXPhistdrop, EXPhistremember);
	(void)fclose(F);
    }

    exit(x);
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: expire [flags] [expire.ctl]\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		CANTCD[] = "Can't cd to %s, %s\n";
    register int	i;
    register int	line;
    register char	*p;
    register QIOSTATE	*qp;
    FILE		*F;
    char		*active;
    char		**arts;
    STRING		History;
    STRING		HistoryText;
    STRING		HistoryPath;
    STRING		HistoryDB;
    char		*Historydir;
    char		*Historypag;
    char		*NHistory;
    char		*NHistorydir;
    char		*NHistorypag;
    char		buff[SMBUF];
    register FILE	*out;
    BOOL		Server;
    BOOL		Paused;
    BOOL		Bad;
    BOOL		IgnoreOld;
    BOOL		Writing;
    BOOL		UnlinkFile;
    time_t		TimeWarp;

    /* Set defaults. */
    Server = TRUE;
    IgnoreOld = FALSE;
    History = "history";
    HistoryText = _PATH_HISTORY;
    HistoryPath = NULL;
    Writing = TRUE;
    TimeWarp = 0;
    UnlinkFile = FALSE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "f:h:d:g:ilnpqr:stv:w:xz:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'd':
	    HistoryPath = optarg;
	    break;
	case 'f':
	    History = optarg;
	    break;
	case 'g':
	    EXPgraph = optarg;
	    break;
	case 'h':
	    HistoryText = optarg;
	    break;
	case 'i':
	    IgnoreOld = TRUE;
	    break;
	case 'l':
	    EXPlinks = TRUE;
	    break;
	case 'n':
	    Server = FALSE;
	    break;
	case 'p':
	    EXPusepost = TRUE;
	    break;
	case 'q':
	    EXPquiet = TRUE;
	    break;
	case 'r':
	    EXPreason = optarg;
	    break;
	case 's':
	    EXPsizing = TRUE;
	    break;
	case 't':
	    EXPtracing = TRUE;
	    break;
	case 'v':
	    EXPverbose = atoi(optarg);
	    break;
	case 'w':
	    TimeWarp = (time_t)(atof(optarg) * 86400.);
	    break;
	case 'x':
	    Writing = FALSE;
	    break;
	case 'z':
	    EXPunlinkfile = EXPfopen(TRUE, optarg, "a");
	    UnlinkFile = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac != 0 && ac != 1)
	Usage();

    /* Get active file, parse it. */
    if ((active = ReadInFile(ACTIVE, (struct stat *)NULL)) == NULL) {
	(void)fprintf(stderr, "Can't read %s, %s\n",
		ACTIVE, strerror(errno));
	exit(1);
    }
    BuildGroups(active);
    (void)time(&Now);
    Now += TimeWarp;

    /* Parse the control file. */
    if (av[0])
	F = EQ(av[0], "-") ? stdin : EXPfopen(FALSE, av[0], "r");
    else
	F = EXPfopen(FALSE, _PATH_EXPIRECTL, "r");
    if (!EXPreadfile(F)) {
	(void)fclose(F);
	(void)fprintf(stderr, "Format error in control file\n");
	exit(1);
    }
    (void)fclose(F);

    /* Set up the link, reserve the lock. */
    if (EXPreason == NULL) {
	(void)sprintf(buff, "Expiring process %ld", (long)getpid());
	EXPreason = COPY(buff);
    }
    if (Server) {
	/* If we fail, leave evidence behind. */
	if (ICCopen() < 0) {
	    (void)fprintf(stderr, "Can't open channel to server, %s\n",
		    strerror(errno));
	    CleanupAndExit(FALSE, FALSE, 1);
	}
	if (ICCreserve(EXPreason) != 0) {
	    (void)fprintf(stderr, "Can't reserve server\n");
	    CleanupAndExit(FALSE, FALSE, 1);
	}
    }

    /* Make the history filenames. */
    HistoryDB = COPY(HistoryText);
    (void)sprintf(buff, "%s.dir", HistoryDB);
    Historydir = COPY(buff);
    (void)sprintf(buff, "%s.pag", HistoryDB);
    Historypag = COPY(buff);
    if (HistoryPath)
	(void)sprintf(buff, "%s/%s.n", HistoryPath, History);
    else
	(void)sprintf(buff, "%s.n", History);
    NHistory = COPY(buff);
    (void)sprintf(buff, "%s.dir", NHistory);
    NHistorydir = COPY(buff);
    (void)sprintf(buff, "%s.pag", NHistory);
    NHistorypag = COPY(buff);

    if (!Writing)
	out = NULL;
    else {
	/* Open new history files, relative to news lib. */
	if (chdir(EXPnewslib) < 0) {
	    (void)fprintf(stderr, CANTCD, EXPnewslib, strerror(errno));
	    exit(1);
	}
	out = EXPfopen(TRUE, NHistory, "w");
	(void)fclose(EXPfopen(TRUE, NHistorydir, "w"));
	(void)fclose(EXPfopen(TRUE, NHistorypag, "w"));
	if (EXPverbose > 3)
	    (void)printf("created: %s %s %s\n",
		    NHistory, NHistorydir, NHistorypag);
	(void)dbzincore(1);
	if (IgnoreOld) {
	    if (dbzfresh(NHistory, dbzsize(0L), '\t', 'C', 0L) < 0) {
		(void)fprintf(stderr, "Can't create database, %s\n",
			strerror(errno));
		exit(1);
	    }
	}
	else if (dbzagain(NHistory, HistoryDB) < 0) {
	    (void)fprintf(stderr, "Can't dbzagain, %s\n", strerror(errno));
	    exit(1);
	}
    }

    if (chdir(SPOOL) < 0) {
	(void)fprintf(stderr, CANTCD, SPOOL, strerror(errno));
	exit(1);
    }

    /* Main processing loop. */
    arts = NEW(char*, nGroups);
    if ((qp = QIOopen(HistoryText, QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't open history file, %s\n",
		strerror(errno));
	CleanupAndExit(Server, FALSE, 1);
    }
    for (Bad = FALSE, line = 1, Paused = FALSE; ; line++) {
	if ((p = QIOread(qp)) != NULL) {
	    if (!EXPdoline(out, p, QIOlength(qp), arts)) {
		Bad = TRUE;
		if (errno == ENOSPC) {
		    (void)unlink(NHistory);
		    (void)unlink(NHistorydir);
		    (void)unlink(NHistorypag);
		}
		break;
	    }
	    continue;
	}

	/* Read or line-format error? */
	if (QIOerror(qp)) {
	    (void)fprintf(stderr, "Can't read line %d, %s\n",
		    line, strerror(errno));
	    QIOclose(qp);
	    CleanupAndExit(Server, Paused, 1);
	}
	if (QIOtoolong(qp)) {
	    (void)fprintf(stderr, "Line %d too long\n", line);
	    QIOclose(qp);
	    CleanupAndExit(Server, Paused, 1);
	}

	/* We hit EOF. */
	if (Paused || !Server)
	    /* Already paused or we don't want to pause -- we're done. */
	    break;
	if (ICCpause(EXPreason) != 0) {
	    (void)fprintf(stderr, "Can't pause server, %s\n",
		    strerror(errno));
	    QIOclose(qp);
	    CleanupAndExit(Server, Paused, 1);
	}
	Paused = TRUE;
    }
    QIOclose(qp);
    DISPOSE(arts);

    if (Writing) {
	/* Close the output files. */
	if (ferror(out) || fflush(out) == EOF || fclose(out) == EOF) {
	    (void)fprintf(stderr, "Can't close %s, %s\n",
		NHistory, strerror(errno));
	    Bad = TRUE;
	}
	if (dbmclose() < 0) {
	    (void)fprintf(stderr, "Can't close history, %s\n",
		    strerror(errno));
	    Bad = TRUE;
	}

	if (UnlinkFile && EXPunlinkfile == NULL)
	    /* Got -z but file was closed; oops. */
	    Bad = TRUE;

	/* If we're done okay, and we're not tracing, slip in the new files. */
	if (EXPverbose) {
	    if (Bad)
		(void)printf("Expire errors: history files not updated.\n");
	    if (EXPtracing)
		(void)printf("Expire tracing: history files not updated.\n");
	}
	if (!Bad && !EXPtracing) {
	    if (chdir(EXPnewslib) < 0) {
		(void)fprintf(stderr, CANTCD, EXPnewslib, strerror(errno));
		CleanupAndExit(Server, Paused, 1);
	    }
	    /* If user used the -d flag, mark we're done and exit. */
	    if (HistoryPath != NULL) {
		(void)sprintf(buff, "%s.done", NHistory);
		(void)fclose(EXPfopen(FALSE, buff, "w"));
		CleanupAndExit(Server, FALSE, 0);
	    }

	    if (rename(NHistory, HistoryText) < 0
	     || rename(NHistorydir, Historydir) < 0
	     || rename(NHistorypag, Historypag) < 0) {
		(void)fprintf(stderr, "Can't replace history files, %s\n",
			strerror(errno));
		/* Yes -- leave the server paused. */
		CleanupAndExit(Server, FALSE, 1);
	    }
	}
    }

    if (EXPsizing)
	(void)printf("%s approximately %ldk\n",
	    EXPtracing ? "Would remove" : "Removed", EXPsaved);
    CleanupAndExit(Server, Paused, Bad ? 1 : 0);
    /* NOTREACHED */
}
