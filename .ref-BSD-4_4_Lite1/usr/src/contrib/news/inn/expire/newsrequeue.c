/*  $Revision: 1.8 $
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
#include "dbz.h"
#include "qio.h"
#include "macros.h"


/*
**  Hashing functions.  See innd for comments.
*/
#define NGH_HASH(Name, p, j)    \
	for (p = Name, j = 0; *p; ) j = (j << 5) + j + *p++
#define NGH_SIZE	128
#define NGH_BUCKET(j)	&NGHtable[j & (NGH_SIZE - 1)]

typedef struct _NGHASH {
    int			Size;
    int			Used;
    struct _NEWSGROUP	**Groups;
} NGHASH;


/*
**  A site has a filename, and flag saying if we already sent it here.
*/
typedef struct _SITE {
    char		*Name;
    char		**Exclusions;
    char		*Patterns;
    char		**Distributions;
    char		*Flags;
    BOOL		Sent;
} SITE;

/*
**  A newsgroup has a name, and a set of sites that get the group.
*/
typedef struct _NEWSGROUP {
    char		*Name;
    char		Flag;
    int			nSites;
    SITE		**Sites;
} NEWSGROUP;


/*
**  Bit array, indexed by character (8bit chars only).  If ARTpathbits['x']
**  is non-zero, then 'x' is a valid character for a host name.
*/
STATIC char		ARTpathbits[256];
#define ARThostchar(c)	(ARTpathbits[(c)] != '\0')


/*
**  Global variables.
*/
STATIC SITE		*Sites;
STATIC int		nSites;
STATIC NEWSGROUP	*Groups;
STATIC int		nGroups;
STATIC long		Epoch;
STATIC NGHASH		NGHtable[NGH_SIZE];



/*
**  Get the next filename from the history file.
*/
STATIC BOOL
GetName(F, buff)
    register FILE	*F;
    register char	*buff;
{
    static char		SPOOL[] = _PATH_SPOOL;
    register int	c;
    register char	*p;

    /* Skip whitespace before filename. */
    while ((c = getc(F)) == ' ')
	continue;
    if (c == EOF || c == '\n')
	return FALSE;

    (void)strcpy(buff, SPOOL);
    p = &buff[STRLEN(SPOOL)];
    *p++ = '/';
    *p++ = (char)c;
    while ((c = getc(F)) != EOF && c != ' ' && c != '\n')
	*p++ = (char)(c == '.' ? '/' : c);
    *p = '\0';
    return TRUE;
}


/*
**  Find an existing file for the specified Message-ID, and return the
**  open file pointer or NULL.
*/
STATIC FILE *
FindFile(F, id, name)
    FILE		*F;
    char		*id;
    char		*name;
{
    static char		BADLINE[] = "Bad text line for \"%s\", %s\n";
    register char	*p;
    register char	*q;
    register int	i;
    register int	c;
    register FILE	*art;
    datum		key;
    datum		val;
    OFFSET_T		offset;
    char		date[SMBUF];

    key.dsize = strlen(id) + 1;
    key.dptr = id;

    /* Do the lookup. */
    val = dbzfetch(key);
    if (val.dptr == NULL || val.dsize != sizeof offset) {
	(void)fprintf(stderr, "Can't find \"%s\"\n", key.dptr);
	return NULL;
    }

    /* Get the seek offset, and seek. */
    for (p = val.dptr, q = (char *)&offset, i = sizeof offset; --i >= 0; )
	*q++ = *p++;
    if (fseek(F, offset, SEEK_SET) == -1) {
	(void)fprintf(stderr, "Can't seek to %ld, %s\n",
		offset, strerror(errno));
	return NULL;
    }

    if (Epoch) {
	/* Skip forward to the date. */
	while ((c = getc(F)) != EOF && c != '\n')
	    if (c == HIS_FIELDSEP)
		break;
	if (c != HIS_FIELDSEP) {
	    (void)fprintf(stderr, BADLINE, key.dptr, strerror(errno));
	    return NULL;
	}
	for (p = date; (c == getc(F)) != EOF && CTYPE(isdigit, c); )
	    *p++ = (char)c;
	if (c == EOF) {
	    (void)fprintf(stderr, BADLINE, key.dptr, strerror(errno));
	    return NULL;
	}
	*p = '\0';
	if (atol(date) < Epoch)
	    return NULL;
    }
    else {
	/* Move to the filename fields. */
	for (i = 2; (c = getc(F)) != EOF && c != '\n'; )
	    if (c == HIS_FIELDSEP && --i == 0)
		break;
	if (c != HIS_FIELDSEP) {
	    (void)fprintf(stderr, BADLINE, key.dptr, strerror(errno));
	    return NULL;
	}
    }

    /* Loop over all possible files. */
    while (GetName(F, name))
	if ((art = fopen(name, "r")) != NULL)
	    return art;
    return NULL;
}



/*
**  Read the active file and fill in the Groups array.  Note that
**  NEWSGROUP.Sites is filled in later.
*/
STATIC void
ParseActive(name)
    STRING		name;
{
    char		*active;
    register char	*p;
    register char	*q;
    register int	i;
    register unsigned	j;
    register NGHASH	*htp;
    register NEWSGROUP	*ngp;
    int			NGHbuckets;

    /* Read the file, count the number of groups. */
    if ((active = ReadInFile(name, (struct stat *)NULL)) == NULL) {
	(void)fprintf(stderr, "Can't read \"%s\", %s\n",
		name, strerror(errno));
	exit(1);
    }
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

    /* Fill in the newsgroups array. */
    for (p = active, ngp = Groups, i = nGroups; --i >= 0; ngp++, p = q + 1) {
	if ((q = strchr(p, '\n')) == NULL) {
	    (void)fprintf(stderr, "Missing newline near \"%.10s...\"\n", p);
	    exit(1);
	}
	*q = '\0';
	ngp->Name = p;
	ngp->nSites = 0;

	/* Get the first character after the third space. */
	for (j = 0; *p; p++)
	    if (*p == ' ' && ++j == 3)
		break;
	if (*p == '\0') {
	    (void)fprintf(stderr, "Bad format near \"%.10s...\"\n", ngp->Name);
	    exit(1);
	}
	ngp->Flag = p[1];

	/* Find the right bucket for the group, make sure there is room. */
	/* SUPPRESS 6 *//* Over/underflow from plus expression */
	p = strchr(ngp->Name, ' ');
	*p = '\0';
	NGH_HASH(ngp->Name, p, j);
	htp = NGH_BUCKET(j);
	if (htp->Used >= htp->Size) {
	    htp->Size += NGHbuckets;
	    RENEW(htp->Groups, NEWSGROUP*, htp->Size);
	}
	htp->Groups[htp->Used++] = ngp;
    }

    /* Note that we don't bother to sort the buckets. */
}


/*
**  Split text into comma-separated fields.
*/
STATIC char **
CommaSplit(text)
    char		*text;
{
    register int	i;
    register char	*p;
    register char	**argv;
    char		**save;

    /* How much space do we need? */
    for (i = 2, p = text; *p; p++)
	if (*p == ',')
	    i++;

    for (argv = save = NEW(char*, i), *argv++ = p = text; *p; )
	if (*p == ',') {
	    *p++ = '\0';
	    *argv++ = p;
	}
	else
	    p++;
    *argv = NULL;
    return save;
}


/*
**  Read the newsfeeds file and fill in the Sites array.  Finish off the
**  Groups array.
*/
STATIC void
ParseNewsfeeds(name)
    STRING		name;
{
    register char	*p;
    register char	*to;
    register int	i;
    register NEWSGROUP	*ngp;
    register SITE	*sp;
    char		**strings;
    char		*save;
    char		*newsfeeds;

    /* Read in the file, get a gross count of the number of sites. */
    if ((newsfeeds = ReadInFile(name, (struct stat *)NULL)) == NULL) {
	(void)fprintf(stderr, "Can't read \"%s\", %s\n",
		name, strerror(errno));
	exit(1);
    }
    for (p = newsfeeds, i = 0; (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;

    /* Scan the file, parse all multi-line entries. */
    for (strings = NEW(char*, i + 1), i = 0, to = p = newsfeeds; *p; ) {
	for (save = to; *p; ) {
	    if (*p == '\n') {
		p++;
		*to = '\0';
		break;
	    }
	    if (*p == '\\' && p[1] == '\n')
		while (*++p && CTYPE(isspace, *p))
		    continue;
	    else
		*to++ = *p++;
	}
	*to++ = '\0';
	if (*save == COMMENT_CHAR || *save == '\0')
	    continue;
	strings[i++] = COPY(save);
    }
    DISPOSE(newsfeeds);
    if (i == 0) {
	(void)fprintf(stderr, "No sites.\n");
	exit(1);
    }

    /* Get space for the sites. */
    nSites = i;
    Sites = NEW(SITE, nSites);
    for (i = nGroups, ngp = Groups; --i >= 0; ngp++)
	ngp->Sites = NEW(SITE*, nSites);

    /* Do initial processing of the site entries. */
    for (i = 0, sp = Sites; i < nSites; i++, sp++) {
	/* Nip off the first and second fields. */
	sp->Name = strings[i];
	if ((p = strchr(sp->Name, NF_FIELD_SEP)) == NULL) {
	    (void)fprintf(stderr, "No separator for site \"%.10s...\"\n",
		    sp->Name);
	    exit(1);
	}
	*p++ = '\0';
	sp->Patterns = p;

	/* Nip off the third field. */
	if ((p = strchr(sp->Patterns, NF_FIELD_SEP)) == NULL) {
	    (void)fprintf(stderr, "No flags for site \"%s\"\n", sp->Name);
	    exit(1);
	}
	*p++ = '\0';
	sp->Flags = p;

	/* Nip off the last field, build the filename. */
	if ((p = strchr(sp->Flags, NF_FIELD_SEP)) == NULL) {
	    (void)fprintf(stderr, "No last field for site \"%s\"\n", sp->Name);
	    exit(1);
	}
	*p++ = '\0';

	/* Handle the subfields. */
	if ((p = strchr(sp->Name, NF_SUBFIELD_SEP)) != NULL) {
	    *p++ = '\0';
	    sp->Exclusions = CommaSplit(p);
	}
	else
	    sp->Exclusions = NULL;
	if ((p = strchr(sp->Patterns, NF_SUBFIELD_SEP)) != NULL) {
	    *p++ = '\0';
	    sp->Distributions = CommaSplit(p);
	}
	else
	    sp->Distributions = NULL;
    }
}


/*
**  Build the subscription list for a site.
*/
STATIC void
BuildSubList(sp, subbed)
    register SITE	*sp;
    char		*subbed;
{
    static char		SEPS[] = ",";
    register char	subvalue;
    register char	*pat;
    register char	*p;
    register NEWSGROUP	*ngp;
    register int	i;
    BOOL		JustModerated;
    BOOL		JustUnmoderated;

    if (EQ(sp->Name, "ME"))
	return;

    /* Fill in the subbed array with the mask of groups. */
    (void)memset((POINTER)subbed, SUB_DEFAULT, (SIZE_T)nGroups);
    if ((pat = strtok(sp->Patterns, SEPS)) != NULL)
	do {
	    subvalue = *pat != SUB_NEGATE;
	    if (!subvalue)
		pat++;
	    for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
		if (wildmat(ngp->Name, pat))
		    *p = subvalue;
	} while ((pat = strtok((char *)NULL, SEPS)) != NULL);

    /* Parse the flags.. */
    JustModerated = FALSE;
    JustUnmoderated = FALSE;
    if ((p = strtok(sp->Flags, SEPS)) != NULL)
	do {
	    switch (*p) {
	    case 'W':
		if (EQ(p, "Wnm"))
		    break;
		/* FALLTHROUGH */
	    default:
		(void)fprintf(stderr, "Ignoring \"%s\" flag for \"%s\"\n",
			p, sp->Name);
		break;
	    case 'N':
		while (*++p)
		    switch (*p) {
		    default:
			(void)fprintf(stderr, "Unknown N%c flag for \"%s\"\n",
				*p, sp->Name);
			break;
		    case 'm': JustModerated = TRUE;	break;
		    case 'u': JustUnmoderated = TRUE;	break;
		    }
		break;
	    case 'T':
		break;
	    }
	} while ((p = strtok((char *)NULL, SEPS)) != NULL);

    /* Modify the subscription list based on the flags. */
    if (JustModerated)
	for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
	    if (ngp->Flag != NF_FLAG_MODERATED)
		*p = FALSE;
    if (JustUnmoderated)
	for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
	    if (ngp->Flag == NF_FLAG_MODERATED)
		*p = FALSE;

    /* Tell the groups that this site gets that they should feed this site. */
    for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++)
	if (*p++)
	    ngp->Sites[ngp->nSites++] = sp;
}



STATIC NEWSGROUP *
NGfind(Name)
    char			*Name;
{
    register char		*p;
    register int		i;
    register unsigned int	j;
    register NEWSGROUP		**ngp;
    char			c;
    NGHASH			*htp;

    /* SUPPRESS 6 *//* Over/underflow from plus expression */
    NGH_HASH(Name, p, j);
    htp = NGH_BUCKET(j);
    for (c = *Name, ngp = htp->Groups, i = htp->Used; --i >= 0; ngp++)
	if (c == ngp[0]->Name[0] && EQ(Name, ngp[0]->Name))
	    return ngp[0];
    return NULL;
}


/*
**  Split up the Path line.
*/
STATIC char **
ParsePath(p)
    register char	*p;
{
    static char		*save;
    static int		oldlength;
    static char		**hosts;
    register int	i;
    register char	**hp;
    char		*nl;

    if (save)
	DISPOSE(save);
    if ((nl = strchr(p, '\n')) != NULL)
	*nl = '\0';
    save = p = COPY(p);
    if (nl)
	*nl = '\n';

    /* Get an array of character pointers. */
    i = strlen(p);
    if (hosts == NULL) {
	hosts = NEW(char*, i + 1);
	oldlength = i;
    }
    else if (oldlength < i) {
	RENEW(hosts, char*, i + 1);
	oldlength = i;
    }

    /* Loop over text. */
    for (hp = hosts; *p; *p++ = '\0') {
	/* Skip leading separators. */
	for (; *p && !ARThostchar(*p); p++)
	    continue;
	if (*p == '\0')
	    break;

	/* Mark the start of the host, move to the end of it. */
	for (*hp++ = p; *p && ARThostchar(*p); p++)
	    continue;
	if (*p == '\0')
	    break;
    }
    *hp = NULL;
    return hosts;
}


/*
**  Has this site or its excludes already been seen?
*/
STATIC BOOL
Seen(sp, Path)
    SITE		*sp;
    char		**Path;
{
    register char	*p;
    register char	**pp;

    while ((p = *Path++) != NULL) {
	if (EQ(p, sp->Name))
	    return TRUE;
	if ((pp = sp->Exclusions) != NULL)
	    for ( ; *pp; pp++)
		if (EQ(p, *pp))
		    return TRUE;
    }
    return FALSE;
}


/*
**  Check a single word against a distribution list.
*/
STATIC BOOL
WantThisOne(list, p)
    register char	**list;
    register char	*p;
{
    register char	*q;
    register char	c;
    register BOOL	sawbang;

    for (sawbang = FALSE, c = *p; (q = *list) != NULL; list++)
	if (*q == '!') {
	    sawbang = TRUE;
	    if (c == *++q && EQ(p, q))
		return FALSE;
	}
	else if (c == *q && EQ(p, q))
	    return TRUE;

    /* If we saw any !foo's and didn't match, then assume they are all
     * negated distributions and return TRUE, else return false. */
    return sawbang;
}


/*
**  Does the site want this article with this distribution?
*/
STATIC BOOL
Wanted(site, article)
    register char	**site;
    register char	**article;
{
    for ( ; *article; article++)
	if (WantThisOne(site, *article))
	    return TRUE;
    return FALSE;
}


/*
**  Split up the Distribution line.
*/
STATIC char **
ParseDistribs(p)
    register char	*p;
{
    static char		SEPS[] = ", \t";
    static char		*save;
    static int		Size;
    static int		Used;
    static char		**List;
    char		*nl;

    /* Throw out old storage, make sure we have a list. */
    if (save)
	DISPOSE(save);
    if ((nl = strchr(p, '\n')) != NULL)
	*nl = '\0';
    save = p = COPY(p);
    if (nl)
	*nl = '\n';
    if (List == NULL) {
	Size = 10;
	List = NEW(char*, Size);
    }
    Used = 0;

    if ((p = strtok(p, SEPS)) == NULL)
	return NULL;
    do {
	if (Used == Size - 2) {
	    Size += 10;
	    RENEW(List, char*, Size);
	}
	List[Used++] = p;
    } while ((p = strtok((char *)NULL, SEPS)) != NULL);

    List[Used] = NULL;
    return List;
}


/*
**  Process a single file.
*/
STATIC void
QueueArticle(name, id, art)
    char		*name;
    char		*id;
    register FILE	*art;
{
    static char		SEPS[] = ",";
    static char		DISTRIBUTION[] = "Distribution";
    static char		PATH[] = "Path";
    static char		NG[] = "Newsgroups";
    static char		*Buffer;
    static int		Size;
    register SITE	*sp;
    register int	i;
    register char	*p;
    register BOOL	Dirty;
    register char	*nl;
    register NEWSGROUP	*ngp;
    struct stat		Sb;
    char		**Path;
    char		**Distribs;

    /* Read in the file. */
    if (fstat((int)fileno(art), &Sb) < 0) {
	(void)fprintf(stderr, "Can't fstat \"%s\", %s\n",
		name, strerror(errno));
	return;
    }
    if (Buffer == NULL) {
	Size = Sb.st_size;
	Buffer = NEW(char, Size + 1);
    }
    else if (Size < Sb.st_size) {
	Size = Sb.st_size;
	RENEW(Buffer, char, Size + 1);
    }
    if (fread((POINTER)Buffer, (SIZE_T)1, (SIZE_T)Sb.st_size,
	    art) != Sb.st_size) {
	(void)fprintf(stderr, "Can't read \"%s\", %s\n",
		name, strerror(errno));
	return;
    }
    Buffer[Sb.st_size] = '\0';

    /* Clear all sites. */
    for (Dirty = FALSE, i = nSites, sp = Sites; --i >= 0; sp++)
	sp->Sent = FALSE;

    /* Parse the Path and Distribution headers. */
    if ((p = HeaderFind(Buffer, PATH, STRLEN(PATH))) == NULL) {
	(void)fprintf(stderr, "No \"Path\" header in \"%s\"\n", name);
	return;
    }
    Path = ParsePath(p);
    if ((p = HeaderFind(Buffer, DISTRIBUTION, STRLEN(DISTRIBUTION))) == NULL)
	Distribs = NULL;
    else
	Distribs = ParseDistribs(p);

    /* Look at the newsgroups, see who gets the article. */
    if ((p = HeaderFind(Buffer, NG, STRLEN(NG))) == NULL) {
	(void)fprintf(stderr, "No \"Newsgroups\" header in \"%s\"\n", name);
	return;
    }
    if ((nl = strchr(p, '\n')) != NULL)
	*nl = '\0';
    if ((p = strtok(p, SEPS)) != NULL)
	do {
	    if ((ngp = NGfind(p)) != NULL) {
		for (i = 0; i < ngp->nSites; i++) {
		    sp = ngp->Sites[i];
		    if (Path && Seen(sp, Path))
			continue;
		    if (Distribs
		     && sp->Distributions
		     && !Wanted(sp->Distributions, Distribs))
			continue;
		    sp->Sent = TRUE;
		    Dirty = TRUE;
		}
	    }
	} while ((p = strtok((char *)NULL, SEPS)) != NULL);

    /* Write the output. */
    if (Dirty) {
	(void)printf("%s %s", name, id);
	for (i = nSites, sp = Sites; --i >= 0; sp++)
	    if (sp->Sent)
		(void)printf(" %s", sp->Name);
	(void)printf("\n");
	if (fflush(stdout) == EOF || ferror(stdout))
	    (void)fprintf(stderr, "Error writing \"%s\", %s\n",
		    id, strerror(errno));
    }
}



/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage error.\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register int	i;
    register QIOSTATE	*qp;
    register char	*p;
    register char	*q;
#if	defined(DO_NNTPLINK_LOG)
    register char	*r;
#endif	/* defined(DO_NNTPLINK_LOG) */
    register char	*line;
    register FILE	*art;
    register FILE	*F;
    STRING		Active;
    STRING		History;
    STRING		Newsfeeds;
    char		*subbed;
    time_t		t;
    BOOL		Logfile;
    char		name[SPOOLNAMEBUFF];
    char		save;

    /* Set defaults. */
    Active = _PATH_ACTIVE;
    History = _PATH_HISTORY;
    Newsfeeds = _PATH_NEWSFEEDS;
    Logfile = FALSE;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "a:d:h:ln:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'a':
	    Active = optarg;
	    break;
	case 'd':
	    (void)time(&t);
	    Epoch = (long)t - atol(optarg) * 86400L;
	    break;
	case 'h':
	    History = optarg;
	    break;
	case 'l':
	    Logfile = TRUE;
	    break;
	case 'n':
	    Newsfeeds = optarg;
	    break;
	}
    ac -= optind;
    av += optind;
    if (Epoch && Logfile)
	Usage();

    /* Parse positional parameters; at most one, the input file. */
    switch (ac) {
    default:
	Usage();
	/* NOTREACHED */
    case 0:
	break;
    case 1:
	if (freopen(av[0], "r", stdin) == NULL) {
	    (void)fprintf(stderr, "Can't open \"%s\" for input, %s\n",
		    av[0], strerror(errno));
	    exit(1);
	}
	break;
    }
    qp = QIOfdopen((int)fileno(stdin), QIO_BUFFER);

    /* Open the history file. */
    if (dbminit(History) < 0) {
	(void)fprintf(stderr, "Can't set up \"%s\" database, %s\n",
		History, strerror(errno));
	exit(1);
    }
    if ((F = fopen(History, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\" for reading, %s\n",
		History, strerror(errno));
	exit(1);
    }

    if (!Logfile) {
	/* Read the control files and build the subscription list. */
	ParseActive(Active);
	ParseNewsfeeds(Newsfeeds);
	subbed = NEW(char, nGroups);
	for (i = 0; i < nSites; i++)
	    BuildSubList(&Sites[i], subbed);
	DISPOSE(subbed);

	/* Set up the character class tables. */
	(void)memset((POINTER)ARTpathbits, 0, sizeof ARTpathbits);
	p = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-";
	while ((i = *p++) != 0)
	    ARTpathbits[i] = TRUE;
    }


    /* Now we're ready to start reading input. */
    for (i = 1; ; i++) {
	if ((line = QIOread(qp)) == NULL) {
	    /* Read or line-format error? */
	    if (QIOerror(qp)) {
		(void)fprintf(stderr, "Can't read line %d, %s\n",
			i, strerror(errno));
		exit(1);
	    }
	    if (QIOtoolong(qp)) {
		(void)fprintf(stderr, "Line %d too long\n", i);
		exit(1);
	    }

	    /* We hit EOF. */
	    break;
	}

	if (Logfile) {
	    /* Check the log character (correct for zero-origin subscripts. */
	    switch (line[STRLEN("Jan 23 12:52:12.631 +") - 1]) {
	    default:
		(void)fprintf(stderr, "Ignoring \"%s\"\n", line);
		continue;
	    case ART_CANC:
	    case ART_REJECT:
		continue;
	    case ART_ACCEPT:
	    case ART_JUNK:
		break;
	    }
	}

	/* Snip off the Message-ID. */
	if ((p = strchr(line, '<')) == NULL
	 || (q = strchr(p, '>')) == NULL) {
	    (void)fprintf(stderr, "No Message-ID in \"%s\"\n", line);
	    continue;
	}
	save = *++q;
	*q = '\0';

	/* Open the article. */
	if ((art = FindFile(F, p, name)) == NULL)
	    continue;

	if (Logfile) {
#if	defined(DO_NNTPLINK_LOG)
	    /* Skip the (filename) if it's there. */
	    if (save != '\0' && (r = strchr(q + 1, ')')) != NULL)
		(void)printf("%s %s%s\n", name, p, r + 1);
	    else {
		*q = save;
		(void)printf("%s %s\n", name, p);
	    }
#else
	    *q = save;
	    (void)printf("%s %s\n", name, p);
#endif	/* defined(DO_NNTPLINK_LOG) */

	    if (fflush(stdout) == EOF || ferror(stdout))
		(void)fprintf(stderr, "Can't write %s, %s\n",
			p, strerror(errno));
	}
	else
	    QueueArticle(name, p, art);
	(void)fclose(art);
    }

    /* That's all she wrote. */
    QIOclose(qp);
    (void)fclose(F);
    (void)dbmclose();
    exit(0);
    /* NOTREACHED */
}
