/*  $Revision: 1.19 $
**
**  Routine for the in-core data structures for the active and newsfeeds
**  files.
*/
#include "innd.h"
#include "mydir.h"


/*
**  Hash function taken from Chris Torek's hash package posted to
**  comp.lang.c on 18-Oct-90 in <27038@mimsy.umd.edu>.  Thanks, Chris.
*/
#define NGH_HASH(Name, p, j)	\
	for (p = Name, j = 0; *p; ) j = (j << 5) + j + *p++


/*
**  Size of hash table.   Change NGH_BUCKET if not a power of two.
*/
#define NGH_SIZE	512
#define NGH_BUCKET(j)	&NGHtable[j & (NGH_SIZE - 1)]


/*
**  Newsgroup hash entry, which is really a hash bucket -- pointers
**  to all the groups with this hash code.
*/
typedef struct _NGHASH {
    int			Size;
    int			Used;
    NEWSGROUP		**Groups;
} NGHASH;


STATIC BUFFER	NGdirs;
STATIC BUFFER	NGnames;
STATIC NGHASH	NGHtable[NGH_SIZE];
STATIC int	NGHbuckets;
STATIC int	NGHcount;



/*
**  Sorting predicate for qsort call in NGparsefile.  Put newsgroups in
**  rough order of their activity.  Will be better if we write a "counts"
**  file sometime.
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
**  Convert a newsgroup name into a directory name.
*/
STATIC void
NGdirname(p)
    register char	*p;
{
    for ( ; *p; p++)
	if (*p == '.')
	    *p = '/';
}


/*
**  Parse a single line from the active file, filling in ngp.  Be careful
**  not to write NUL's into the in-core copy, since we're either mmap(2)'d,
**  or we want to just blat it out to disk later.
*/
STATIC BOOL
NGparseentry(ngp, p, end)
    register NEWSGROUP		*ngp;
    register char		*p;
    register char		*end;
{
    register char		*q;
    register unsigned int	j;
    register NGHASH		*htp;
    register NEWSGROUP		**ngpp;
    register int		i;

    if ((q = strchr(p, ' ')) == NULL)
	return FALSE;
    i = q - p;

    ngp->NameLength = i;
    ngp->Name = &NGnames.Data[NGnames.Used];
    (void)strncpy(ngp->Name, p, (SIZE_T)i);
    ngp->Name[i] = '\0';
    NGnames.Used += i + 1;

    ngp->Dir = &NGdirs.Data[NGdirs.Used];
    (void)strncpy(ngp->Dir, p, (SIZE_T)i);
    ngp->Dir[i] = '\0';
    NGdirs.Used += i + 1;
    NGdirname(ngp->Dir);

    ngp->LastString = ++q;
    if ((q = strchr(q, ' ')) == NULL || q > end)
	return FALSE;
    ngp->Lastwidth = q - ngp->LastString;
    if ((q = strchr(q, ' ')) == NULL || q > end)
	return FALSE;
    if ((q = strchr(q + 1, ' ')) == NULL || q > end)
	return FALSE;
    ngp->Rest = ++q;
    /* We count on atoi() to stop at the space after the digits! */
    ngp->Last = atol(ngp->LastString);
    ngp->nSites = 0;
    ngp->Sites = NEW(int, NGHcount);
    ngp->Alias = NULL;

    /* Find the right bucket for the group, make sure there is room. */
    /* SUPPRESS 6 *//* Over/underflow from plus expression */
    NGH_HASH(ngp->Name, p, j);
    htp = NGH_BUCKET(j);
    for (p = ngp->Name, ngpp = htp->Groups, i = htp->Used; --i >= 0; ngpp++)
	if (*p == ngpp[0]->Name[0] && EQ(p, ngpp[0]->Name)) {
	    syslog(L_ERROR, "%s duplicate_group %s", LogName, p);
	    return FALSE;
	}
    if (htp->Used >= htp->Size) {
	htp->Size += NGHbuckets;
	RENEW(htp->Groups, NEWSGROUP*, htp->Size);
    }
    htp->Groups[htp->Used++] = ngp;

    return TRUE;
}


/*
**  Parse the active file, building the initial Groups global.
*/
void
NGparsefile()
{
    register char	*p;
    register char	*q;
    register int	i;
    register BOOL	SawMe;
    register NEWSGROUP	*ngp;
    register NGHASH	*htp;
    char		**strings;
    char		*active;
    char		*end;

    /* If re-reading, remove anything we might have had. */
    if (Groups) {
	for (i = nGroups, ngp = Groups; --i >= 0; ngp++)
	    DISPOSE(ngp->Sites);
	DISPOSE(Groups);
	DISPOSE(GroupPointers);
	DISPOSE(NGdirs.Data);
	DISPOSE(NGnames.Data);
    }


    /* Get active file and space for group entries. */
    active = ICDreadactive(&end);
    for (p = active, i = 0; p < end && (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;
    nGroups = i;
    Groups = NEW(NEWSGROUP, nGroups);
    GroupPointers = NEW(NEWSGROUP*, nGroups);

    /* Get space to hold copies of the names and the directory names.
     * This might take more space than individually allocating each
     * element, but it is definitely easier on the system. */
    i = end - active;
    NGdirs.Size = i;
    NGdirs.Data = NEW(char, NGdirs.Size + 1);
    NGdirs.Used = 0;
    NGnames.Size = i;
    NGnames.Data = NEW(char, NGnames.Size + 1);
    NGnames.Used = 0;

    /* Set up the default hash buckets. */
    NGHbuckets = nGroups / NGH_SIZE;
    if (NGHbuckets == 0)
	NGHbuckets = 1;
    if (NGHtable[0].Groups)
	for (i = NGH_SIZE, htp = NGHtable; --i >= 0; htp++)
	    htp->Used = 0;
    else
	for (i = NGH_SIZE, htp = NGHtable; --i >= 0; htp++) {
	    htp->Size = NGHbuckets;
	    htp->Groups = NEW(NEWSGROUP*, htp->Size);
	    htp->Used = 0;
	}

    /* Count the number of sites. */
    SawMe = FALSE;
    for (strings = SITEreadfile(TRUE), i = 0; (p = strings[i]) != NULL; i++)
	if (*p == 'M' && *++p == 'E' && *++p == ':')
	    SawMe = TRUE;
    if (i == 0 || (i == 1 && SawMe)) {
	syslog(L_ERROR, "%s bad_newsfeeds no feeding sites", LogName);
	NGHcount = 1;
    }
    else
	NGHcount = i;

    /* Loop over all lines in the active file, filling in the fields of
     * the Groups array. */
    for (p = active, ngp = Groups, i = nGroups; --i >= 0; ngp++, p = q + 1) {
	ngp->Start = p - active;
	if ((q = strchr(p, '\n')) == NULL || !NGparseentry(ngp, p, q)) {
	    syslog(L_FATAL, "%s bad_active %s...", LogName, MaxLength(p, p));
	    exit(1);
	}
    }

    /* Sort each bucket. */
    for (i = NGH_SIZE, htp = NGHtable; --i >= 0; htp++)
	if (htp->Used > 1)
	    qsort((POINTER)htp->Groups, (SIZE_T)htp->Used,
		sizeof htp->Groups[0], NGcompare);

    /* Chase down any alias flags. */
    for (ngp = Groups, i = nGroups; --i >= 0; ngp++)
	if (ngp->Rest[0] == NF_FLAG_ALIAS) {
	    ngp->Alias = ngp;
	    if ((p = strchr(ngp->Alias->Rest, '\n')) != NULL)
		*p = '\0';
	    ngp->Alias = NGfind(&ngp->Alias->Rest[1]);
	    if (p)
		*p = '\n';
	    if (ngp->Alias != NULL && ngp->Alias->Rest[0] == NF_FLAG_ALIAS)
		syslog(L_NOTICE, "%s alias_error %s too many levels",
		    LogName, ngp->Name);
	}
}


/*
**  Hash a newsgroup and see if we get it.
*/
NEWSGROUP *
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
**  Split a newsgroups header line into the groups we get.  Return a
**  point to static memory and clobber the argument along the way.
*/
char **
NGsplit(p)
    register char	*p;
{
    static char		**groups;
    static int		oldlength;
    register char	**gp;
    register int	i;

    /* Get an array of character pointers. */
    i = strlen(p);
    if (groups == NULL) {
	groups = NEW(char*, i + 1);
	oldlength = i;
    }
    else if (oldlength < i) {
	RENEW(groups, char*, i + 1);
	oldlength = i;
    }

    /* Loop over text. */
    for (gp = groups; *p; *p++ = '\0') {
	/* Skip leading separators. */
	for (; NG_ISSEP(*p); p++)
	    continue;
	if (*p == '\0')
	    break;

	/* Mark the start of the newsgroup, move to the end of it. */
	for (*gp++ = p; *p && !NG_ISSEP(*p); p++)
	    continue;
	if (*p == '\0')
	    break;
    }
    *gp = NULL;
    return groups;
}


/*
**  Renumber a group.
*/
BOOL
NGrenumber(ngp)
    NEWSGROUP		*ngp;
{
    static char		NORENUMBER[] = "%s cant renumber %s %s too wide";
    static char		RENUMBER[] = "%s renumber %s %s from %ld to %ld";
    register DIR	*dp;
    register DIRENTRY	*ep;
    register char	*f2;
    register char	*p;
    char		*f3;
    char		*f4;
    char		*start;
    long		l;
    long		himark;
    long		lomark;
    char		*dummy;

    /* Get a valid offset into the active file. */
    if (ICDneedsetup) {
	syslog(L_ERROR, "%s unsynched must reload before renumber", LogName);
	return FALSE;
    }
    start = ICDreadactive(&dummy) + ngp->Start;

    /* Check the file format. */
    if ((f2 = strchr(start, ' ')) == NULL
     || (f3 = strchr(++f2, ' ')) == NULL
     || (f4 = strchr(++f3, ' ')) == NULL) {
	syslog(L_ERROR, "%s bad_format active %s",
	    LogName, MaxLength(start, start));
	return FALSE;
    }
    himark = atol(f2);
    lomark = himark + 1;

    /* Scan the directory. */
    if ((dp = opendir(ngp->Dir)) != NULL) {
	while ((ep = readdir(dp)) != NULL) {
	    p = ep->d_name;
	    if (!CTYPE(isdigit, p[0]) || strspn(p, "0123456789") != strlen(p)
	     || (l = atol(p)) == 0)
		continue;
	    if (l < lomark)
		lomark = l;
	    if (l > himark)
		himark = l;
	}
	(void)closedir(dp);
    }
    l = atol(f2);
    if (himark != l) {
	syslog(L_NOTICE, RENUMBER, LogName, ngp->Name, "hi", l, himark);
	if (!FormatLong(f2, himark, f3 - f2 - 1)) {
	    syslog(L_NOTICE, NORENUMBER, LogName, ngp->Name, "hi");
	    return FALSE;
	}
	ngp->Last = himark;
	ICDactivedirty++;
    }
    l = atol(f3);
    if (lomark != l) {
	if (lomark < l)
	    syslog(L_NOTICE, RENUMBER, LogName, ngp->Name, "lo", l, lomark);
	if (!FormatLong(f3, lomark, f4 - f3)) {
	    syslog(L_NOTICE, NORENUMBER, LogName, ngp->Name, "lo");
	    return FALSE;
	}
	ICDactivedirty++;
    }
    return TRUE;
}
