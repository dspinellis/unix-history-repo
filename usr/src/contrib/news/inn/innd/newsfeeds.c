/*  $Revision: 1.31 $
**
**  Routines for the in-core data structures for the newsfeeds file.
*/
#include "innd.h"


STATIC SITE	SITEnull;
STATIC char	SITEfeedspath[] = _PATH_NEWSFEEDS;


/*
**  Return a copy of an array of strings.
*/
STATIC char **
SITEcopystrings(av)
    char	**av;
{
    register char	**new;
    register char	**pp;
    char		**save;

    for (pp = av; *pp; pp++)
	continue;
    for (new = save = NEW(char*, pp - av + 1), pp = av; *pp; pp++)
	*new++ = COPY(*pp);
    *new = NULL;
    return save;
}


/*
**  Read the newsfeeds file, return a string array of entries.
*/
char **
SITEreadfile(ReadOnly)
    BOOL		ReadOnly;
{
    static char		**old_strings;
    static time_t	old_mtime;
    static ino_t	old_ino;
    static off_t	old_size;
    register char	*p;
    register char	*to;
    register char	*site;
    register int	i;
    struct stat		Sb;
    char		*data;

    if (old_strings != NULL) {
	/* If the file hasn't changed, return a copy of the old data. */
	if (stat(SITEfeedspath, &Sb) >= 0
	 && Sb.st_ino == old_ino
	 && Sb.st_size == old_size
	 && Sb.st_mtime == old_mtime)
	    return ReadOnly ? old_strings : SITEcopystrings(old_strings);

	/* Data's bad, toss it. */
	for (i = 0; old_strings[i] != NULL; i++)
	    DISPOSE(old_strings[i]);
	DISPOSE(old_strings);
    }

    /* Read in the file, note its statistics. */
    if ((data = ReadInFile(SITEfeedspath, &Sb)) == NULL) {
	syslog(L_FATAL, "%s cant read %s %m", LogName, SITEfeedspath);
	exit(1);
    }
    old_mtime = Sb.st_mtime;
    old_ino = Sb.st_ino;
    old_size = Sb.st_size;

    /* Get a gross count of the number of sites. */
    for (p = data, i = 0; (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;

    /* Scan the file, parse all multi-line entries. */
    for (old_strings = NEW(char*, i + 1), i = 0, to = p = data; *p; ) {
	for (site = to; *p; ) {
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
	if (*site == COMMENT_CHAR || *site == '\0')
	    continue;
	old_strings[i++] = COPY(site);
    }
    old_strings[i] = NULL;

    DISPOSE(data);
    return ReadOnly ? old_strings : SITEcopystrings(old_strings);
}


/*
**  Modify "subbed" according to the patterns in "patlist."
*/
STATIC void
SITEsetlist(patlist, subbed)
    char		**patlist;
    char		*subbed;
{
    register char	*pat;
    register char	*p;
    register char	subvalue;
    register NEWSGROUP	*ngp;
    register int	i;

    while ((pat = *patlist++) != NULL) {
	subvalue = *pat != SUB_NEGATE;
	if (!subvalue)
	    pat++;

	/* See if pattern is a simple newsgroup name.  If so, set the
	 * right subbed element for that one group (if found); if not,
	 * pattern-match against all the groups. */
	for (p = pat; *p; p++)
	    if (*p == '?' || *p == '*' || *p == '[')
		break;
	if (*p == '\0') {
	    /* Simple string; look it up, set it. */
	    if ((ngp = NGfind(pat)) != NULL)
		subbed[ngp - Groups] = subvalue;
	}
	else
	    for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
		if (wildmat(ngp->Name, pat))
		    *p = subvalue;
    }
}


/*
**  Parse an individual site entry.  Subbed is used to build the subscription
**  list.  Since this routine is called once for each site, the caller
**  allocates subbed once, and frees it after the last site has been parsed.
**  If subbed is NULL, we don't update the SITE array, since we're just
**  doing syntax checking.
*/
STRING
SITEparseone(Entry, sp, subbed)
    char		*Entry;
    SITE		*sp;
    char		*subbed;
{
    static char		BATCH[] = _PATH_BATCHDIR;
    register int	i;
    register int	j;
    register NEWSGROUP	*ngp;
    register char	*p;
    char		*f2;
    char		*f3;
    char		*f4;
    char		**save;
    char		**argv;
    BOOL		JustModerated;
    BOOL		JustUnmoderated;
    int			isp;
    SITE		*nsp;
    BUFFER		b;

    b = sp->Buffer;
    *sp = SITEnull;
    sp->Buffer = b;
    sp->Master = NOSITE;
    sp->Funnel = NOSITE;
    sp->Process = -1;
    sp->Entry = Entry;
    sp->FileFlags[0] = FEED_NAME;
    sp->FileFlags[1] = '\0';

    /* Nip off the first field, the site name. */
    if ((f2 = strchr(Entry, NF_FIELD_SEP)) == NULL)
	return "missing field 2";
    *f2++ = '\0';
    sp->Name = Entry;
    if ((p = strchr(sp->Name, NF_SUBFIELD_SEP)) != NULL) {
	/* Exclusions within the site field. */
	*p++ = '\0';
	sp->Exclusions = CommaSplit(p);
    }
    sp->NameLength = strlen(sp->Name);

    /* Parse the second field, the subscriptions. */
    if ((f3 = strchr(f2, NF_FIELD_SEP)) == NULL)
	return "missing field 3";
    *f3++ = '\0';
    if ((p = strchr(f2, NF_SUBFIELD_SEP)) != NULL) {
	/* Distributions within the subscription field. */
	*p++ = '\0';
	sp->Distributions = CommaSplit(p);
    }
    sp->Patterns = CommaSplit(f2);

    if (subbed) {
	/* Read the subscription patterns and set the bits. */
	(void)memset((POINTER)subbed, SUB_DEFAULT, (SIZE_T)nGroups);
	if (ME.Patterns)
	    SITEsetlist(ME.Patterns, subbed);
	SITEsetlist(sp->Patterns, subbed);
    }

    /* Get the third field, the flags. */
    if ((f4 = strchr(f3, NF_FIELD_SEP)) == NULL)
	return "missing field 4";
    *f4++ = '\0';
    JustModerated = FALSE;
    JustUnmoderated = FALSE;
    sp->Type = FTfile;
    for (save = argv = CommaSplit(f3); (p = *argv++) != NULL; )
	switch (*p) {
	default:
	    return "unknown field 3 flag";
	case '\0':
	    break;
	case '<':
	    if (*++p && CTYPE(isdigit, *p))
		sp->MaxSize = atol(p);
	    break;
	case 'A':
	    while (*++p)
		switch (*p) {
		default:
		    return "unknown A param in field 3";
		case 'd': sp->DistRequired = TRUE;	break;
		case 'p': sp->IgnorePath = TRUE;	break;
		}
	    break;
	case 'B':
	    if (*++p && CTYPE(isdigit, *p)) {
		sp->StartWriting = atoi(p);
		if ((p = strchr(p, NF_SUBFIELD_SEP)) != NULL
		 && *++p
		 && CTYPE(isdigit, *p))
		    sp->StopWriting = atoi(p);
	    }
	    break;
	case 'F':
	    if (*++p == '/')
		sp->SpoolName = COPY(p);
	    else {
		sp->SpoolName = NEW(char, STRLEN(BATCH) + 1 + strlen(p) + 1);
		FileGlue(sp->SpoolName, BATCH, '/', p);
	    }
	    break;
	case 'G':
	    if (*++p && CTYPE(isdigit, *p))
		sp->Groupcount = atoi(p);
	    else
		sp->Groupcount = 1;
	    break;
	case 'H':
	    if (*++p && CTYPE(isdigit, *p))
		sp->Hops = atoi(p);
	    else
		sp->Hops = 1;
	    break;
	case 'I':
	    if (*++p && CTYPE(isdigit, *p))
		sp->Flushpoint = atol(p);
	    break;
	case 'N':
	    while (*++p)
		switch (*p) {
		default:
		    return "unknown N param in field 3";
		case 'm': JustModerated = TRUE;		break;
		case 'u': JustUnmoderated = TRUE;	break;
		}
	    break;
	case 'S':
	    if (*++p && CTYPE(isdigit, *p))
		sp->StartSpooling = atol(p);
	    break;
	case 'T':
	    switch (*++p) {
	    default:
		return "unknown T param in field 3";
	    case 'c': sp->Type = FTchannel;	break;
	    case 'l': sp->Type = FTlogonly;	break;
	    case 'f': sp->Type = FTfile;	break;
	    case 'm': sp->Type = FTfunnel;	break;
	    case 'p': sp->Type = FTprogram;	break;
	    case 'x': sp->Type = FTexploder;	break;
	    }
	    break;
	case 'W':
	    for (i = 0; *++p && i < FEED_MAXFLAGS; ) {
		switch (*p) {
		default:
		    return "unknown W param in field 3";
		case FEED_FNLNAMES:		/* Funnel feed names	*/
		    sp->FNLwantsnames = TRUE;
		    break;
		case FEED_HEADERS:		/* Article headers	*/
		    NeedHeaders = TRUE;
		    break;
		case FEED_OVERVIEW:
		    NeedOverview = TRUE;	/* Overview data	*/
		    break;
		case FEED_BYTESIZE:		/* Size in bytes	*/
		case FEED_FULLNAME:		/* Full filename	*/
		case FEED_HDR_DISTRIB:		/* Distribution header	*/
		case FEED_HDR_NEWSGROUP:	/* Newsgroup header	*/
		case FEED_MESSAGEID:		/* Message-ID		*/
		case FEED_NAME:			/* Filename		*/
		case FEED_NEWSGROUP:		/* Newsgroup		*/
		case FEED_REPLIC:		/* Replication data	*/
		case FEED_SITE:			/* Site that gave it	*/
		case FEED_TIMERECEIVED:		/* When received	*/
		    break;
		}
		sp->FileFlags[i++] = *p;
	    }
	    if (*p)
		return "too many W param values";
	    sp->FileFlags[i] = '\0';
	    break;
	}
    DISPOSE(save);
    if (sp->Flushpoint && sp->Type != FTfile)
	return "I param with non-file feed";

    if (subbed) {
	/* Modify the subscription list based on the flags. */
	if (JustModerated)
	    for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
		if (ngp->Rest[0] != NF_FLAG_MODERATED)
		    *p = FALSE;
	if (JustUnmoderated)
	    for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++, p++)
		if (ngp->Rest[0] == NF_FLAG_MODERATED)
		    *p = FALSE;
    }

    /* Get the fourth field, the param. */
    if (*f4 == '\0' && sp != &ME) {
	if (sp->Type != FTfile && sp->Type != FTlogonly)
	    return "empty field 4";
	sp->Param = NEW(char, STRLEN(BATCH) + 1 + sp->NameLength + 1);
	FileGlue(sp->Param, BATCH, '/', sp->Name);
    }
    else if (sp->Type == FTfile && *f4 != '/') {
	sp->Param = NEW(char, STRLEN(BATCH) + 1 + strlen(f4) + 1);
	FileGlue(sp->Param, BATCH, '/', f4);
    }
    else
	sp->Param = COPY(f4);

    if (sp->SpoolName == NULL) {
	sp->SpoolName = NEW(char, STRLEN(BATCH) + 1 + strlen(sp->Name) + 1);
	FileGlue(sp->SpoolName, BATCH, '/', sp->Name);
    }

    /* Make sure there is only one %s, and only one *. */
    if (sp->Type == FTprogram) {
	if ((p = strchr(sp->Param, '%')) != NULL) {
	    while (*++p && *p != '*' && !CTYPE(isalpha, *p))
		continue;
	    if (*p != 's' || strchr(p, '%') != NULL)
		return "bad sprintf format for field 4";
	}
	if (sp->FNLwantsnames
	 && ((p = strchr(sp->Param, '*')) == NULL
	  || strchr(++p, '*') != NULL))
	    return "multiple or no *'s in field 4";
    }

    /* Now tell the groups this site gets that they should feed this site. */
    if (sp != &ME && subbed) {
	isp = sp - Sites;
	for (p = subbed, ngp = Groups, i = nGroups; --i >= 0; ngp++)
	    if (*p++) {
		for (j = 0; j < ngp->nSites; j++)
		    if (ngp->Sites[j] == NOSITE) {
			ngp->Sites[j] = isp;
			break;
		    }
		if (j == ngp->nSites)
		    ngp->Sites[ngp->nSites++] = isp;
	    }
    }

    /* If this is a duplicate name, find the master. */
    nsp = SITEfind(sp->Name);
    if (nsp == sp)
	nsp = SITEfindnext(sp->Name, nsp);
    if (nsp != NULL) {
	if (nsp->Master != NOSITE)
	    nsp = &Sites[nsp->Master];
	if (nsp != sp) {
	    sp->Master = nsp - Sites;
	    nsp->IsMaster = TRUE;
	}
    }

    return NULL;
}


/*
**  Patch up the funnel references.
*/
BOOL
SITEfunnelpatch()
{
    register int	i;
    register int	length;
    register SITE	*sp;
    SITE		*funnel;
    BOOL		result;

    /* Get worst-case length of all sitenames. */
    for (length = 0, i = nSites, sp = Sites; --i >= 0; sp++)
	if (sp->Name != NULL)
	    length += 1 + strlen(sp->Name);

    /* Loop over all funnel feeds. */
    for (result = TRUE, i = nSites, sp = Sites; --i >= 0; sp++) {
	if (sp->Type != FTfunnel)
	    continue;

	/* Find the entry they feed in to, give that entry a buffer. */
	if (sp->Param == NULL) {
	    syslog(L_FATAL, "%s funnel NULL", sp->Name);
	    SITEfree(sp);
	    result = FALSE;
	    continue;
	}
	if ((funnel = SITEfind(sp->Param)) == NULL) {
	    syslog(L_FATAL, "%s funnel_bad", sp->Name);
	    SITEfree(sp);
	    result = FALSE;
	    continue;
	}
	if (funnel->Type == FTfunnel) {
	    syslog(L_FATAL, "%s funnels to funnel %s", sp->Name, funnel->Name);
	    SITEfree(sp);
	    result = FALSE;
	    continue;
	}
	if (funnel->FNLnames.Data == NULL) {
	    funnel->FNLnames.Size = length;
	    funnel->FNLnames.Data = NEW(char, length);
	}
	else if (funnel->FNLnames.Size != length) {
	    funnel->FNLnames.Size = length;
	    RENEW(funnel->FNLnames.Data, char, length);
	}
	sp->Funnel = funnel - Sites;
    }

    return result;
}


/*
**  Read the entries in the newsfeeds file, and parse them one at a time.
*/
void
SITEparsefile(StartSite)
    BOOL		StartSite;
{
    register int	i;
    register char	*p;
    char		**strings;
    SITE		*sp;
    char		*subbed;
    STRING		error;
    int			errors;

    /* Free old sites info. */
    if (Sites) {
	for (i = nSites, sp = Sites; --i >= 0; sp++) {
	    SITEflush(sp, FALSE);
	    SITEfree(sp);
	}
	DISPOSE(Sites);
	SITEfree(&ME);
    }

    /* Count the number of sites. */
    for (strings = SITEreadfile(FALSE), nSites = 0; strings[nSites]; nSites++)
	continue;
    Sites = NEW(SITE, nSites);
    for (sp = Sites, i = 0; i < nSites; i++, sp++) {
	sp->Name = NULL;
	sp->Buffer.Data = NULL;
    }

    /* Set up scratch subscription list. */
    subbed = NEW(char, nGroups);

    for (sp = Sites, errors = 0, i = 0; i < nSites; i++) {
	p = strings[i];
	if (p[0] == 'M' && p[1] == 'E' && p[2] == NF_FIELD_SEP) {
	    if ((error = SITEparseone(p, &ME, subbed)) != NULL) {
		syslog(L_FATAL, "%s bad_newsfeeds %s", MaxLength(p, p), error);
		errors++;
	    }
	    continue;
	}
	if ((error = SITEparseone(p, sp, subbed)) != NULL) {
	    syslog(L_FATAL, "%s bad_newsfeeds %s", MaxLength(p, p), error);
	    errors++;
	    continue;
	}
	if (StartSite && !SITEsetup(sp)) {
	    syslog(L_FATAL, "%s cant setup %m", sp->Name);
	    errors++;
	    continue;
	}
	sp->Working = TRUE;
	sp++;
    }

    if (errors) {
	syslog(L_FATAL, "%s syntax_error %s", LogName, SITEfeedspath);
	JustCleanup();
	exit(1);
    }

    /* Free our scratch array, set up the funnel links. */
    nSites = sp - Sites;
    DISPOSE(subbed);
    DISPOSE(strings);
    if (!SITEfunnelpatch()) {
	JustCleanup();
	exit(1);
    }
    SITElinkall();
}
