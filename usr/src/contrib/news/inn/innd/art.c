/*  $Revision: 1.52 $
**
**  Article-processing.
*/
#include "innd.h"
#include "dbz.h"
#include <sys/uio.h>

typedef struct iovec	IOVEC;

/*
**  A way to index into the header table.
*/
#define HDR(_x)		(ARTheaders[(_x)].Value)


#if	defined(S_IXUSR)
#define EXECUTE_BITS	(S_IXUSR | S_IXGRP | S_IXOTH)
#else
#define EXECUTE_BITS	0111
#endif	/* defined(S_IXUSR) */


/*
**  Mark that the site gets this article.
*/
#define SITEmark(sp_, ngp_) \
    do { \
	SITE	*funnel; \
    \
	sp_->Sendit = TRUE; \
	if (sp_->ng == NULL) \
	    sp_->ng = ngp_; \
	if (sp_->Funnel != NOSITE) { \
	    funnel = &Sites[sp_->Funnel]; \
	    if (funnel->ng == NULL) \
		funnel->ng = ngp_; \
	} \
    } while (JUSTONCE)

/*
**  Header types.
*/
typedef enum _ARTHEADERTYPE {
    HTreq,			/* Drop article if this is missing	*/
    HTobs,			/* Delete this header if found		*/
    HTstd			/* Standard optional header		*/
} ARTHEADERTYPE;


/*
**  Entry in the header table.
*/
typedef struct _ARTHEADER {
    STRING		Name;
    ARTHEADERTYPE	Type;
    int			Size;			/* Length of Name	*/
    char		*Value;
    int			Length;			/* Length of Value	*/
    int			Found;
    BOOL		Allocated;
} ARTHEADER;


/*
**  For speed we build a binary tree of the headers, sorted by their
**  name.  We also store the header's Name fields in the tree to avoid
**  doing an extra indirection.
*/
typedef struct _TREE {
    STRING		Name;
    ARTHEADER		*Header;
    struct _TREE	*Before;
    struct _TREE	*After;
} TREE;

STATIC TREE		*ARTheadertree;


/*
**  For doing the overview database, we keep a list of the headers and
**  a flag saying if they're written in brief or full format.
*/
typedef struct _ARTOVERFIELD {
    ARTHEADER		*Header;
    BOOL		NeedHeader;
} ARTOVERFIELD;

STATIC ARTOVERFIELD		*ARTfields;


/*
**  General newsgroup we care about, and what we put in the Path line.
*/
STATIC char		ARTctl[] = "control";
STATIC char		ARTjnk[] = "junk";
STATIC char		*ARTpathme;


/*
**  Flag array, indexed by character.  Character classes for Message-ID's.
*/
STATIC char		ARTcclass[256];
#define CC_MSGID_ATOM	01
#define CC_MSGID_NORM	02
#define CC_HOSTNAME	04
#define ARTnormchar(c)	((ARTcclass[(c)] & CC_MSGID_NORM) != 0)
#define ARTatomchar(c)	((ARTcclass[(c)] & CC_MSGID_ATOM) != 0)
#define ARThostchar(c)	((ARTcclass[(c)] & CC_HOSTNAME) != 0)


/*
**  The header table.  Not necessarily sorted, but the first character
**  must be uppercase.
*/
STATIC ARTHEADER	ARTheaders[] = {
    /*	Name			Type	... */
    {	"Approved",		HTstd },
#define _approved		 0
    {	"Control",		HTstd },
#define _control		 1
    {	"Date",			HTreq },
#define _date			 2
    {	"Distribution",		HTstd },
#define _distribution		 3
    {	"Expires",		HTstd },
#define _expires		 4
    {	"From",			HTreq },
#define _from			 5
    {	"Lines",		HTstd },
#define _lines			 6
    {	"Message-ID",		HTreq },
#define _message_id		 7
    {	"Newsgroups",		HTreq },
#define _newsgroups		 8
    {	"Path",			HTreq },
#define _path			 9
    {	"Reply-To",		HTstd },
#define _reply_to		10
    {	"Sender",		HTstd },
#define _sender			11
    {	"Subject",		HTreq },
#define _subject		12
    {	"Supersedes",		HTstd },
#define _supersedes		13
    {	"Bytes",		HTstd },
#define _bytes			14
    {	"Also-Control",		HTstd },
#define _alsocontrol		15
    {	"References",		HTstd },
#define _references		16
    {	"Xref",			HTobs },
#define _xref			17
    {	"Date-Received",	HTobs },
    {	"Posted",		HTobs },
    {	"Posting-Version",	HTobs },
    {	"Received",		HTobs },
    {	"Relay-Version",	HTobs },
};


/*
**
*/
BOOL
ARTreadschema()
{
    static char			SCHEMA[] = _PATH_SCHEMA;
    register FILE		*F;
    register int		i;
    register char		*p;
    register ARTOVERFIELD	*fp;
    register ARTHEADER		*hp;
    BOOL			ok;
    char			buff[SMBUF];

    if (ARTfields != NULL) {
	DISPOSE(ARTfields);
	ARTfields = NULL;
    }

    /* Open file, count lines. */
    if ((F = fopen(SCHEMA, "r")) == NULL)
	return FALSE;
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    ARTfields = NEW(ARTOVERFIELD, i + 1);

    /* Parse each field. */
    for (ok = TRUE, fp = ARTfields; fgets(buff, sizeof buff, F) != NULL; ) {
	/* Ignore blank and comment lines. */
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, COMMENT_CHAR)) != NULL)
	    *p = '\0';
	if (buff[0] == '\0')
	    continue;
	if ((p = strchr(buff, ':')) != NULL) {
	    *p++ = '\0';
	    fp->NeedHeader = EQ(p, "full");
	}
	else
	    fp->NeedHeader = FALSE;
	for (hp = ARTheaders; hp < ENDOF(ARTheaders); hp++)
	    if (EQ(buff, hp->Name)) {
		fp->Header = hp;
		break;
	    }
	if (hp == ENDOF(ARTheaders)) {
	    syslog(L_ERROR, "%s bad_schema unknown header \"%s\"", buff);
	    ok = FALSE;
	    continue;
	}
	fp++;
    }
    fp->Header = NULL;

    (void)fclose(F);
    return ok;
}


/*
**  Build a balanced tree for the headers in subscript range [lo..hi).
**  This only gets called once, and the tree only has about 20 entries,
**  so we don't bother to unroll the recursion.
*/
static TREE *
ARTbuildtree(Table, lo, hi)
    ARTHEADER	**Table;
    int		lo;
    int		hi;
{
    int		mid;
    TREE	*tp;

    mid = lo + (hi - lo) / 2;
    tp = NEW(TREE, 1);
    tp->Header = Table[mid];
    tp->Name = tp->Header->Name;
    if (mid == lo)
	tp->Before = NULL;
    else
	tp->Before = ARTbuildtree(Table, lo, mid);
    if (mid == hi - 1)
	tp->After = NULL;
    else
	tp->After = ARTbuildtree(Table, mid + 1, hi);
    return tp;
}


/*
**  Sorting predicate for qsort call in ARTsetup.
*/
STATIC int
ARTcompare(p1, p2)
    POINTER	p1;
    POINTER	p2;
{
    ARTHEADER	**h1;
    ARTHEADER	**h2;

    h1 = CAST(ARTHEADER**, p1);
    h2 = CAST(ARTHEADER**, p2);
    return strcasecmp(h1[0]->Name, h2[0]->Name);
}


/*
**  Setup the article processing.
*/
void
ARTsetup()
{
    register STRING	p;
    register ARTHEADER	*hp;
    ARTHEADER		**table;
    register int	i;

    /* Set up the character class tables.  These are written a
     * little strangely to work around a GCC2.0 bug. */
    (void)memset((POINTER)ARTcclass, 0, sizeof ARTcclass);
    p = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    while ((i = *p++) != 0) {
        ARTcclass[i] = CC_HOSTNAME | CC_MSGID_ATOM | CC_MSGID_NORM;
    }
    p = "!#$%&'*+-/=?^_`{|}~";
    while ((i = *p++) != 0) {
	ARTcclass[i] = CC_MSGID_ATOM | CC_MSGID_NORM;
    }
    p = "\"(),.:;<@[\\]";
    while ((i = *p++) != 0) {
	ARTcclass[i] = CC_MSGID_NORM;
    }

    /* The RFC's don't require it, but we add underscore to the list of valid
     * hostname characters. */
    ARTcclass['.'] |= CC_HOSTNAME;
    ARTcclass['-'] |= CC_HOSTNAME;
    ARTcclass['_'] |= CC_HOSTNAME;

    /* Allocate space in the header table. */
    for (hp = ARTheaders; hp < ENDOF(ARTheaders); hp++) {
	hp->Size = strlen(hp->Name);
	hp->Allocated = hp->Value == NULL && hp->Type != HTobs
			&& hp != &ARTheaders[_bytes];
	if (hp->Allocated)
	    hp->Value = NEW(char, MAXHEADERSIZE + 1);
    }

    /* Build the header tree. */
    table = NEW(ARTHEADER*, SIZEOF(ARTheaders));
    for (i = 0; i < SIZEOF(ARTheaders); i++)
	table[i] = &ARTheaders[i];
    qsort((POINTER)table, SIZEOF(ARTheaders), sizeof *table, ARTcompare);
    ARTheadertree = ARTbuildtree(table, 0, SIZEOF(ARTheaders));
    DISPOSE(table);

    /* Get our Path name, kill trailing !. */
    ARTpathme = COPY(Path.Data);
    ARTpathme[Path.Used - 1] = '\0';

    /* Set up database; ignore errors. */
    (void)ARTreadschema();
}


STATIC void
ARTfreetree(tp)
    TREE	*tp;
{
    TREE	*next;

    for ( ; tp != NULL; tp = next) {
	if (tp->Before)
	    ARTfreetree(tp->Before);
	next = tp->After;
	DISPOSE(tp);
    }
}


void
ARTclose()
{
    register ARTHEADER	*hp;

    /* Free space in the header table. */
    for (hp = ARTheaders; hp < ENDOF(ARTheaders); hp++)
	if (hp->Allocated)
	    DISPOSE(hp->Value);

    if (ARTfields != NULL) {
	DISPOSE(ARTfields);
	ARTfields = NULL;
    }
    ARTfreetree(ARTheadertree);
}


/*
**  Read in a file, return a pointer to static space that is reused.
*/
STATIC char *
ARTreadfile(name)
    char		*name;
{
    static BUFFER	File;
    struct stat		Sb;
    int			fd;
    int			oerrno;

    /* Open the file, get its size. */
    if ((fd = open(name, O_RDONLY)) < 0)
	return NULL;
    if (fstat(fd, &Sb) < 0) {
	oerrno = errno;
	(void)close(fd);
	errno = oerrno;
	return NULL;
    }

    /* Make sure we have enough space. */
    if (File.Size == 0) {
	File.Size = Sb.st_size;
	File.Data = NEW(char, File.Size + 1);
    }
    else if (File.Size <= Sb.st_size) {
	File.Size = Sb.st_size + 16;
	RENEW(File.Data, char, File.Size + 1);
    }

    /* Read in the file. */
    if (xread(fd, File.Data, Sb.st_size) < 0) {
	oerrno = errno;
	(void)close(fd);
	errno = oerrno;
	return NULL;
    }

    /* Clean up and return the data. */
    File.Data[Sb.st_size] = '\0';
    (void)close(fd);
    return File.Data;
}


/*
**  Open the article file and return a copy of it.  The files parameter is
**  actually a whitespace-separated list of names.
*/
char *
ARTreadarticle(files)
    register char	*files;
{
    register char	*p;
    register BOOL	more;
    char		*art;

    if (files == NULL)
	return NULL;

    /* Loop over all filenames until we can open one. */
    for ( ; *files; files = p + 1) {
	/* Snip off next name, turn dots to slashes. */
	for (p = files; ISWHITE(*p); p++)
	    continue;
	for (files = p; *p && *p != ' '; p++)
	    if (*p == '.')
		*p = '/';
	more = *p == ' ';
	if (more)
	    *p = '\0';
	art = ARTreadfile(files);
	if (more)
	    *p = ' ';
	if (art != NULL)
	    return art;
	if (!more)
	    break;
    }
    return NULL;
}


/*
**  Open the article file and return a copy of the headers.
*/
char *
ARTreadheader(files)
    char		*files;
{
    register char	*p;
    register char	*head;

    if ((head = ARTreadarticle(files)) == NULL)
	return NULL;

    /* Find \n\n which means the end of the header. */
    for (p = head; (p = strchr(p, '\n')) != NULL; p++)
	if (p[1] == '\n') {
	    p[1] = '\0';
	    return head;
	}
    syslog(L_NOTICE, "%s bad_article %s is all headers", LogName, files);
    DISPOSE(head);
    return NULL;
}


/*
**  Parse a Path line, splitting it up into NULL-terminated array of strings.
**  The argument is modified!
*/
STATIC char **
ARTparsepath(p, countp)
    register char	*p;
    int			*countp;
{
    static char		*NULLPATH[1] = { NULL };
    static int		oldlength;
    static char		**hosts;
    register int	i;
    register char	**hp;

    /* We can be called with a non-existant or empty path. */
    if (p == NULL || *p == '\0') {
	*countp = 0;
	return NULLPATH;
    }

    /* Get an array of character pointers. */
    i = strlen(p);
    if (hosts == NULL) {
	oldlength = i;
	hosts = NEW(char*, oldlength + 1);
    }
    else if (oldlength <= i) {
	oldlength = i;
	RENEW(hosts, char*, oldlength + 1);
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
    *countp = hp - hosts;
    return hosts;
}


/*
**  Write an article using writev.  The article is split into pieces,
**  shown below separated by pipe signs.  The items in square brackets are
**  "inserted" by this routine.
**	|headers...
**	Path: |[Path.Data]|rest of path...
**	headers...
**	|[Lines header, if needed]|
**	|[Xref header]|
**
**	Article body.
**  Also, the Data->Size field is filled in.
*/
STATIC int
ARTwrite(name, Article, Data, CrossPosted)
    char		*name;
    BUFFER		*Article;
    ARTDATA		*Data;
    BOOL		CrossPosted;
{
    static char		WHEN[] = "article";
    static char		NL[] = "\n";
    static BUFFER	Headers;
    register int	fd;
    register IOVEC	*vp;
    register long	size;
    register char	*p;
    IOVEC		iov[7];
    IOVEC		*end;
    char		bytesbuff[SMBUF];
    int			i;

    if ((p = HeaderFind(Article->Data, "Path", 4)) == NULL
     || p == Article->Data) {
	/* This should not happen. */
	syslog(L_ERROR, "%s internal %s no Path header",
	    Data->MessageID, LogName);
	return -1;
    }

    /* Open the file. */
    if ((fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, ARTFILE_MODE)) < 0) {
	if (errno != ENOENT)
	    IOError(WHEN);
	return -1;
    }

    /* Set up the scatter/gather vectors. */
    vp = iov;
    size = 0;
    vp->iov_base = Article->Data;
    vp->iov_len  = p - Article->Data;
    size += (vp++)->iov_len;
    vp->iov_base = Path.Data;
    vp->iov_len  = Path.Used;
    size += (vp++)->iov_len;
    vp->iov_base = p;
    vp->iov_len  = Data->Body - p;
    size += (vp++)->iov_len;
    if (ARTheaders[_lines].Found == 0) {
	(void)sprintf(Data->Lines, "Lines: %d\n", Data->LinesValue);
	i = strlen(Data->Lines);
	vp->iov_base = Data->Lines;
	(vp++)->iov_len  = i;
	size += i;
	/* Install in header table; STRLEN("Lines: ") == 7. */
	(void)strcpy(ARTheaders[_lines].Value, Data->Lines + 7);
	ARTheaders[_lines].Length = i - 7;
	ARTheaders[_lines].Found = 1;
    }

    if (CrossPosted) {
	/* Install in header table; STRLEN("Xref: ") == 6. */
	HDR(_xref) = Xref.Data + 6;
	ARTheaders[_xref].Length = Xref.Used - 6;
	ARTheaders[_xref].Found = 1;

	vp->iov_base = Xref.Data;
	vp->iov_len  = Xref.Used;
	size += (vp++)->iov_len;
    }

    end = vp;
    vp->iov_base = NL;
    vp->iov_len  = 1;
    size += (vp++)->iov_len;

    vp->iov_base = Data->Body;
    vp->iov_len  = &Article->Data[Article->Used] - Data->Body;
    size += (vp++)->iov_len;
    Data->SizeValue = size;
    (void)sprintf(Data->Size, "%ld", Data->SizeValue);
    Data->SizeLength = strlen(Data->Size);
    HDR(_bytes) = Data->Size;
    ARTheaders[_bytes].Length = Data->SizeLength;
    ARTheaders[_bytes].Found = 1;

    /* Now do the write. */
    if (xwritev(fd, iov, vp - iov) < 0) {
	IOError(WHEN);
	syslog(L_ERROR, "%s cant write %s %m", LogName, name);
	(void)close(fd);
	if (unlink(name) < 0 && errno != ENOENT) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant unlink %s %m", LogName, name);
	}
	return -1;
    }
    if (close(fd) < 0) {
	IOError(WHEN);
	syslog(L_ERROR, "%s cant close %s %m", LogName, name);
	if (unlink(name) < 0 && errno != ENOENT) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant unlink %s %m", LogName, name);
	}
	return -1;
    }

    /* Set the owner. */
    if (AmRoot)
	xchown(name);

    /* Need the header data? */
    if (!NeedHeaders)
	return 0;

    /* Figure out how much space we'll need and get it. */
    (void)sprintf(bytesbuff, "Bytes: %ld\n", size);
    for (i = strlen(bytesbuff), vp = iov; vp < end; vp++)
      i += vp->iov_len;
    if (!CrossPosted)
      i += Xref.Used;

    if (Headers.Data == NULL) {
	Headers.Size = i;
	Headers.Data = NEW(char, Headers.Size + 1);
    }
    else if (Headers.Size <= i) {
	Headers.Size = i;
	RENEW(Headers.Data, char, Headers.Size + 1);
    }

    /* Add the data. */
    BUFFset(&Headers, bytesbuff, strlen(bytesbuff));
    if (!CrossPosted)
	BUFFappend(&Headers, Xref.Data, Xref.Used);
    for (vp = iov; vp < end; vp++)
	BUFFappend(&Headers, vp->iov_base, vp->iov_len);
    Data->Headers = &Headers;

    return 0;
}


/*
**  Parse a header that starts at in, copying it to out.  Return pointer to
**  the start of the next header and fill in *deltap with what should
**  get added to the output pointer.  (This nicely lets us clobber obsolete
**  headers by setting it to zero.)
*/
STATIC char *
ARTparseheader(in, out, deltap, errorp)
    register char	*in;
    register char	*out;
    int			*deltap;
    STRING		*errorp;
{
    static char		buff[SMBUF];
    static char		COLONSPACE[] = "No colon-space in \"%s\" header";
    register char	*start;
    register TREE	*tp;
    register ARTHEADER	*hp;
    register char	c;
    register char	*p;
    register char	*dest;
    register int	i;
    register char	*colon;

    /* Find a non-continuation line. */
    for (colon = NULL, start = out; ; ) {
	switch (*in) {
	case '\0':
	    *errorp = "EOF in headers";
	    return NULL;
	case ':':
	    if (colon == NULL)
		colon = out;
	    break;
	}
	if ((*out++ = *in++) == '\n' && !ISWHITE(*in))
	    break;
    }
    *deltap = out - start;
    if (colon == NULL || !ISWHITE(colon[1])) {
	if ((p = strchr(start, '\n')) != NULL)
	    *p = '\0';
	(void)sprintf(buff, COLONSPACE, MaxLength(start, start));
	*errorp = buff;
	return NULL;
    }

    /* See if this is a system header.  A fairly tightly-coded
     * binary search. */
    c = CTYPE(islower, *start) ? toupper(*start) : *start;
    for (*colon = '\0', tp = ARTheadertree; tp; ) {
	if ((i = c - tp->Name[0]) == 0
	 && (i = strcasecmp(start, tp->Name)) == 0)
	    break;
	if (i < 0)
	    tp = tp->Before;
	else
	    tp = tp->After;
    }
    *colon = ':';

    if (tp == NULL) {
	/* Not a system header, make sure we have <word><colon><space>. */
	for (p = colon; --p != start; )
	    if (ISWHITE(*p)) {
		(void)sprintf(buff, "Space before colon in \"%s\" header",
			MaxLength(start, start));
		*errorp = buff;
		return NULL;
	    }
	return in;
    }

    /* Found a known header; is it obsolete? */
    hp = tp->Header;
    if (hp->Type == HTobs) {
	*deltap = 0;
	return in;
    }

    /* If body of header is all blanks, drop the header. */
    for (p = colon + 1; ISWHITE(*p); p++)
	continue;
    if (*p == '\0' || *p == '\n') {
	*deltap = 0;
	return in;
    }

    hp->Found++;

    /* Zap in the canonical form of the header, undoing the \0 that
     * strcpy put out (strncpy() spec isn't trustable, unfortunately). */
    (void)strcpy(start, hp->Name);
    start[hp->Size] = ':';

    /* Copy the header if not too big. */
    i = (out - 1) - p;
    if (i >= MAXHEADERSIZE) {
	(void)sprintf(buff, "\"%s\" header too long", hp->Name);
	*errorp = buff;
	return NULL;
    }
    hp->Length = i;
    if (i > MEMCPY_THRESHOLD) {
	(void)memcpy((POINTER)hp->Value, (POINTER)p, (SIZE_T)i);
	hp->Value[i] = '\0';
    }
    else {
	for (dest = hp->Value, i++; --i > 0; )
	    *dest++ = *p++;
	*dest = '\0';
    }

    return in;
}


/*
**  Check Message-ID format based on RFC 822 grammar, except that (as per
**  RFC 1036) whitespace, non-printing, and '>' characters are excluded.
**  Based on code by Paul Eggert posted to news.software.b on 22-Nov-90
**  in <#*tyo2'~n@twinsun.com>, with additional email discussion.
**  Thanks, Paul.
*/
BOOL
ARTidok(save)
    char		*save;
{
    register int	c;
    register char	*p;

    /* Scan local-part:  "< atom|quoted [ . atom|quoted]" */
    p = save;
    if (*p++ != '<')
	return FALSE;
    for (; ; p++) {
	if (ARTatomchar(*p))
	    while (ARTatomchar(*++p))
		continue;
	else {
	    if (*p++ != '"')
		return FALSE;
	    for ( ; ; ) {
		switch (c = *p++) {
		case '\\':
		    c = *p++;
		    /* FALLTHROUGH */
		default:
		    if (ARTnormchar(c))
			continue;
		    return FALSE;
		case '"':
		    break;
		}
		break;
	    }
	}
	if (*p != '.')
	    break;
    }

    /* Scan domain part:  "@ atom|domain [ . atom|domain] > \0" */
    if (*p++ != '@')
	return FALSE;
    for ( ; ; p++) {
	if (ARTatomchar(*p))
	    while (ARTatomchar(*++p))
		continue;
	else {
	    if (*p++ != '[')
		return FALSE;
	    for ( ; ; ) {
		switch (c = *p++) {
		case '\\':
		    c = *p++;
		    /* FALLTHROUGH */
		default:
		    if (ARTnormchar(c))
			continue;
		    /* FALLTHROUGH */
		case '[':
		    return FALSE;
		case ']':
		    break;
		}
		break;
	    }
	}
	if (*p != '.')
	    break;
    }

    return *p == '>' && *++p == '\0' && p - save <= DBZMAXKEY;
}


/*
**  Clean up an article.  This is mainly copying in-place, stripping bad
**  headers.  Also fill in the article data block with what we can find.
**  Return NULL if the article is okay, or a string describing the error.
*/
STATIC STRING
ARTclean(Article, Data)
    BUFFER		*Article;
    ARTDATA		*Data;
{
    static char		buff[SMBUF];
    ARTHEADER		*hp;
    register char	*in;
    register char	*out;
    register int	i;
    register char	*p;
    STRING		error;
    int			delta;

    /* Read through the headers one at a time. */
    Data->Feedsite = "?";
    Data->Size[0] = '0';
    Data->Size[1] = '\0';
    for (hp = ARTheaders; hp < ENDOF(ARTheaders); hp++) {
	if (hp->Value && hp->Type != HTobs)
	    *hp->Value = '\0';
	hp->Found = 0;
    }
    for (error = NULL, in = out = Article->Data; ; out += delta, in = p) {
	if (*in == '\0') {
	    error = "No body";
	    break;
	}
	if (*in == '\n' && out > Article->Data && out[-1] == '\n')
	    /* Found a \n after another \n; break out. */
	    break;

	/* Check the validity of this header. */
	if ((p = ARTparseheader(in, out, &delta, &error)) == NULL)
	    break;
    }
    Data->Body = out;
    in++;

    /* Try to set this now, so we can report it in errors. */
    p = HDR(_message_id);
    if (*p) {
	Data->MessageID = p;
	Data->MessageIDLength = strlen(p);
	if (error == NULL) {
	    if (Data->MessageIDLength > DBZMAXKEY)
		error = "\"Message-ID\" header too long";
	    else if (!ARTidok(p))
		error = "Bad \"Message-ID\" header";
	}
    }

    if (error)
	return error;

    /* Make sure all the headers we need are there, and no duplicates. */
    for (hp = ARTheaders; hp < ENDOF(ARTheaders); hp++)
	if (hp->Type == HTreq) {
	    if (*hp->Value == '\0') {
		(void)sprintf(buff, "Missing \"%s\" header", hp->Name);
		return buff;
	    }
	    if (hp->Found > 1) {
		(void)sprintf(buff, "Duplicate \"%s\" header", hp->Name);
		return buff;
	    }
	}

    /* Scan the body, counting lines. */
    for (i = 0; *in; ) {
	if (*in == '\n')
	    i++;
	*out++ = *in++;
    }
    *out = '\0';
    Article->Used = out - Article->Data;
    Data->LinesValue = i;

#if	defined(DO_CHECK_LINECOUNT)
    p = HDR(_lines);
    if (*p && (delta = i - atoi(p)) != 0 && abs(delta) > LINECOUNT_FUZZ) {
	if ((in = strchr(p, '\n')) != NULL)
	    *in = '\0';
	(void)sprintf(buff, "Linecount %s != %d +- %d",
		MaxLength(p, p), i, LINECOUNT_FUZZ);
	return buff;
    }
#endif	/* defined(DO_CHECK_LINECOUNT) */

    /* Is article too old? */
    p = HDR(_date);
    if ((Data->Posted = parsedate(p, &Now)) == -1) {
	(void)sprintf(buff, "Bad \"Date\" header -- \"%s\"", MaxLength(p, p));
	return buff;
    }
    if (Cutoff && Data->Posted < Now.time - Cutoff) {
	(void)sprintf(buff, "Too old -- \"%s\"", MaxLength(p, p));
	return buff;
    }
    if (Data->Posted > Now.time + DATE_FUZZ) {
	(void)sprintf(buff, "Article posted in the future -- \"%s\"",
		MaxLength(p, p));
	return buff;
    }
    Data->Arrived = Now.time;
    p = HDR(_expires);
    Data->Expires = 0;
    if (*p != '\0' && (Data->Expires = parsedate(p, &Now)) == -1) {
#if	0
	(void)sprintf(buff, "Bad \"Expires\" header -- \"%s\"",
		MaxLength(p, p));
	return buff;
#endif
    }

    /* Whitespace in the Newsgroups header? */
    for (p = HDR(_newsgroups); *p; p++)
	if (ISWHITE(*p)) {
	    (void)sprintf(buff,
		    "Whitespace in \"Newsgroups\" header -- \"%s\"",
		    MaxLength(HDR(_newsgroups), p));
	    return buff;
	}

    /* If there is no control header, see if the article starts with
     * "cmsg ". */
    in = HDR(_control);
    if (*in == '\0') {
	p = HDR(_subject);
	if (*p == 'c' && EQn(p, "cmsg ", 5)) {
	    for (p += 5; *p && ISWHITE(*p); )
		p++;
	    if (*p)
		(void)strcpy(in, p);
	}
    }
    return NULL;
}


/*
**  Start a log message about an article.
*/
STATIC void
ARTlog(Data, code, text)
    ARTDATA		*Data;
    char		code;
    char		*text;
{
    int			i;
    BOOL		Done;

    /* We could be a bit faster by not dividing Now.usec by 1000,
     * but who really wants to log at the Microsec level? */
    Done = code == ART_ACCEPT || code == ART_JUNK;
    if (text)
	i = fprintf(Log, "%.15s.%03.3d %c %s %s %s%s",
		ctime(&Now.time) + 4, (int)(Now.usec / 1000),
		code, Data->Feedsite, Data->MessageID, text, Done ? "" : "\n");
    else
	i = fprintf(Log, "%.15s.%03.3d %c %s %s%s",
		ctime(&Now.time) + 4, (int)(Now.usec / 1000),
		code, Data->Feedsite, Data->MessageID, Done ? "" : "\n");
    if (i == EOF || (Done && !BufferedLogs && fflush(Log)) || ferror(Log)) {
	IOError("logging article");
	syslog(L_ERROR, "%s cant write log_start %m", LogName);
	clearerr(Log);
    }
}


/*
**  We are going to reject an article, record the reason and
**  and the article.  For now, this is just a placeholder.
*/
/* ARGSUSED0 */
STATIC void
ARTreject(buff, article)
    char	*buff;
    BUFFER	*article;
{
}


#if	defined(DO_VERIFY_CANCELS)
/*
**  Verify if a cancel message is valid.  If the user posting the cancel
**  matches the user who posted the article, return the list of filenames
**  otherwise return NULL.
*/
STATIC char *
ARTcancelverify(Data, MessageID)
    ARTDATA		*Data;
    char		*MessageID;
{
    register char	*files;
    register char	*p;
    register char	*local;
    char		*head;
    char		buff[SMBUF];

    files = HISfilesfor(MessageID);
    if ((head = ARTreadheader(files)) == NULL)
	return NULL;

    /* Get the author header. */
    if ((local = HeaderFind(head, "Sender", 6)) == NULL
     && (local = HeaderFind(head, "From", 4)) == NULL) {
	syslog(L_ERROR, "%s bad_article %s checking cancel",
	    LogName, MessageID);
	return NULL;
    }
    HeaderCleanFrom(local);

    /* Compare canonical forms. */
    p = COPY(Data->Poster);
    HeaderCleanFrom(p);
    if (!EQ(local, p)) {
	files = NULL;
	(void)sprintf(buff, "\"%.50s\" wants to cancel %s by \"%.50s\"",
		p, MaxLength(MessageID, MessageID), local);
	ARTlog(Data, ART_REJECT, buff);
    }
    DISPOSE(p);
    return files;
}
#endif	/* defined(DO_VERIFY_CANCELS) */


/*
**  Process a cancel message.
*/
/* ARGSUSED2 */
void
ARTcancel(Data, MessageID, Trusted)
    ARTDATA		*Data;
    char		*MessageID;
    BOOL		Trusted;
{
    register char	*files;
    register char	*p;
    register BOOL	more;
    STRING		save;
    char		buff[SMBUF];

    if (!HIShavearticle(MessageID)) {
	/* Article hasn't arrived here, so write a fake entry using
	 * most of the information from the cancel message. */
#if	defined(DO_VERIFY_CANCELS)
	if (!Trusted)
	    return;
#endif	/* defined(DO_VERIFY_CANCELS) */
	save = Data->MessageID;
	Data->MessageID = MessageID;
	(void)HISwrite(Data, (char *)NULL);
	Data->MessageID = save;
	(void)sprintf(buff, "Cancelling %s", MessageID);
	ARTlog(Data, ART_CANC, buff);
	return;
    }

#if	defined(DO_VERIFY_CANCELS)
    files = Trusted ? HISfilesfor(MessageID)
		    : ARTcancelverify(Data, MessageID);
#else
    files = HISfilesfor(MessageID);
#endif	/* !defined(DO_VERIFY_CANCELS) */
    if (files == NULL)
	return;

    /* Get the files where the message is stored and and zap them. */
    for ( ; *files; files = p + 1) {
	/* Snip off next name, turn dots to slashes. */
	for (p = files; ISWHITE(*p); p++)
	    continue;
	for (files = p; *p && *p != ' '; p++)
	    if (*p == '.')
		*p = '/';
	more = *p == ' ';
	if (more)
	    *p = '\0';

	/* Remove this file, go back for the next one if there's more. */
	if (unlink(files) < 0 && errno != ENOENT)
	    syslog(L_ERROR, "%s cant unlink %s %m", LogName, files);
	if (!more)
	    break;
    }
}


/*
**  Process a control message.  Cancels are handled here, but any others
**  are passed out to an external program in a specific directory that
**  has the same name as the first word of the control message.
*/
STATIC void
ARTcontrol(Data, Control)
    ARTDATA		*Data;
    char		*Control;
{
    static char		CTLBIN[] = _PATH_CONTROLPROGS;
    register char	*p;
    char		buff[SMBUF];
    char		*av[6];
    struct stat		Sb;
    register char	c;

    /* See if it's a cancel message. */
    c = *Control;
    if (c == 'c' && EQn(Control, "cancel", 6)) {
	for (p = &Control[6]; ISWHITE(*p); p++)
	    continue;
	if (*p)
	    ARTcancel(Data, p, FALSE);
	return;
    }

    /* Nip off the first word into lowercase. */
    for (p = Control; *p && !ISWHITE(*p); p++)
	if (CTYPE(isupper, *p))
	    *p = tolower(*p);
    if (*p)
	*p++ = '\0';

    /* Treat the control message as a place to send the article, if
     * the name is "safe" -- no slashes in the pathname. */
    if (p - Control + STRLEN( _PATH_BADCONTROLPROG) >= SMBUF-4
     || strchr(Control, '/') != NULL)
	FileGlue(buff, CTLBIN, '/', _PATH_BADCONTROLPROG);
    else {
	FileGlue(buff, CTLBIN, '/', Control);
	if (stat(buff, &Sb) < 0 || (Sb.st_mode & EXECUTE_BITS) == 0)
	    FileGlue(buff, CTLBIN, '/', _PATH_BADCONTROLPROG);
    }

    /* If it's an ihave or sendme, check the site named in the message. */
    if ((c == 'i' && EQ(Control, "ihave"))
     || (c == 's' && EQ(Control, "sendme"))) {
	while (ISWHITE(*p))
	    p++;
	if (*p == '\0') {
	    syslog(L_NOTICE, "%s malformed %s no site %s",
		    LogName, Control, Data->Name);
	    return;
	}
	if (EQ(p, ARTpathme)) {
	    /* Do nothing -- must have come from a replicant. */
	    syslog(L_NOTICE, "%s %s_from_me %s",
		Data->Feedsite, Control, Data->Name);
	    return;
	}
	if (!SITEfind(p)) {
	    if (c == 'i')
		syslog(L_ERROR, "%s bad_ihave in %s",
		    Data->Feedsite, Data->Newsgroups);
	    else
		syslog(L_ERROR, "%s bad_sendme dont feed %s",
		    Data->Feedsite, Control, Data->Name);
	    return;
	}
    }

    /* Build the command vector and execute it. */
    av[0] = buff;
    av[1] = COPY(Data->Poster);
    av[2] = COPY(Data->Replyto);
    av[3] = Data->Name;
    av[4] = (char *)Data->Feedsite;
    av[5] = NULL;
    HeaderCleanFrom(av[1]);
    HeaderCleanFrom(av[2]);
    if (Spawn(STDIN, (int)fileno(Errlog), (int)fileno(Errlog), av) < 0)
	/* We know the strrchr below can't fail. */
	syslog(L_ERROR, "%s cant spawn %s for %s %m",
	    LogName, MaxLength(av[0], strrchr(av[0], '/')), Data->Name);
    DISPOSE(av[1]);
    DISPOSE(av[2]);
}


/*
**  Split a Distribution header, making a copy and skipping leading and
**  trailing whitespace (which the RFC allows).
*/
STATIC void
DISTparse(list, Data)
    register char	**list;
    ARTDATA		*Data;
{
    static BUFFER	Dist;
    register char	*p;
    register char	*q;
    register int	i;
    register int	j;

    /* Get space to store the copy. */
    for (i = 0, j = 0; (p = list[i]) != NULL; i++)
	j += 1 + strlen(p);
    if (Dist.Data == NULL) {
	Dist.Size = j;
	Dist.Data = NEW(char, Dist.Size + 1);
    }
    else if (Dist.Size <= j) {
	Dist.Size = j + 16;
	RENEW(Dist.Data, char, Dist.Size + 1);
    }

    /* Loop over each element, skip and trim whitespace. */
    for (q = Dist.Data, i = 0, j = 0; (p = list[i]) != NULL; i++) {
	while (ISWHITE(*p))
	    p++;
	if (*p) {
	    if (j)
		*q++ = ',';
	    for (list[j++] = p; *p && !ISWHITE(*p); )
		*q++ = *p++;
	    *p = '\0';
	}
    }
    list[j] = NULL;

    *q = '\0';
    Data->Distribution = Dist.Data;
    Data->DistributionLength = q - Dist.Data;
}


/*
**  A somewhat similar routine, except that this handles negated entries
**  in the list and is used to check the distribution sub-field.
*/
STATIC BOOL
DISTwanted(list, p)
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
**  See if any of the distributions in the article are wanted by the site.
*/
STATIC BOOL
DISTwantany(site, article)
    char		**site;
    register char	**article;
{
    for ( ; *article; article++)
	if (DISTwanted(site, *article))
	    return TRUE;
    return FALSE;
}


/*
**  Sort an array of newsgroups for optimal disk access.  This may be
**  of marginal benefit.
*/
STATIC void
ARTsortfordisk()
{
    static NEWSGROUP	*save;
    register NEWSGROUP	**ngptr;

    if (save && GroupPointers[1] != NULL) {
	/* If one of the groups we want to access is the group we last
	 * wrote to, move it to the front of the list. */
	for (ngptr = GroupPointers; *++ngptr; )
	    if (*ngptr == save) {
		*ngptr = GroupPointers[0];
		GroupPointers[0] = save;
		return;
	    }
    }
    save = GroupPointers[0];
}


/*
**  Send the current article to all sites that would get it if the
**  group were created.
*/
STATIC void
ARTsendthegroup(name)
    register char	*name;
{
    register SITE	*sp;
    register int	i;
    NEWSGROUP		*ngp;

    for (ngp = NGfind(ARTctl), sp = Sites, i = nSites; --i >= 0; sp++)
	if (sp->Name != NULL && SITEwantsgroup(sp, name)) {
	    SITEmark(sp, ngp);
	}
}


/*
** Assign article numbers to the article and create the Xref line.
** If we end up not being able to write the article, we'll get "holes"
** in the directory and active file.
*/
STATIC void
ARTassignnumbers()
{
    register char	*p;
    register int	i;
    register NEWSGROUP	*ngp;

    p = &Xref.Data[Xref.Used];
    for (i = 0; (ngp = GroupPointers[i]) != NULL; i++) {
	/* If already went to this group (i.e., multiple groups are aliased
	 * into it), then skip it. */
	if (ngp->PostCount > 0)
	    continue;

	/* Bump the number. */
	ngp->PostCount++;
	ngp->Last++;
	if (!FormatLong(ngp->LastString, (long)ngp->Last, ngp->Lastwidth)) {
	    syslog(L_ERROR, "%s cant update_active %s", LogName, ngp->Name);
	    continue;
	}
	ngp->Filenum = ngp->Last;
	(void)sprintf(p, " %s:%lu", ngp->Name, ngp->Filenum);
	p += strlen(p);
    }
    Xref.Used = p - Xref.Data;
    Xref.Data[Xref.Used++] = '\n';
}


/*
**  Parse the data from the xreplic command and assign the numbers.
**  This involves replacing the GroupPointers entries.
*/
STATIC void
ARTreplic(Replic, CrossPostedp)
    BUFFER		*Replic;
    BOOL		*CrossPostedp;
{
    register char	*p;
    register char	*q;
    register char	*name;
    register char	*next;
    register NEWSGROUP	*ngp;
    register int	i;

    p = &Xref.Data[Xref.Used];
    for (i = 0, name = Replic->Data; *name; name = next) {
	/* Mark end of this entry and where next one starts. */
	if ((next = strchr(name, ',')) != NULL)
	    *next++ = '\0';
	else
	    next = "";

	/* Split into news.group/# */
	if ((q = strchr(name, '/')) == NULL) {
	    syslog(L_ERROR, "%s bad_format %s", LogName, name);
	    continue;
	}
	*q = '\0';
	if ((ngp = NGfind(name)) == NULL) {
	    syslog(L_ERROR, "%s bad_newsgroup %s", LogName, name);
	    continue;
	}
	ngp->Filenum = atol(q + 1);

	/* Update active file if we got a new high-water mark. */
	if (ngp->Last < ngp->Filenum) {
	    ngp->Last = ngp->Filenum;
	    if (!FormatLong(ngp->LastString, (long)ngp->Last,
		    ngp->Lastwidth)) {
		syslog(L_ERROR, "%s cant update_active %s",
		    LogName, ngp->Name);
		continue;
	    }
	}

	/* Mark that this group gets the article. */
	ngp->PostCount++;
	GroupPointers[i++] = ngp;

	/* Turn news.group/# into news.group:#, append to Xref. */
	*q = ':';
	*p++ = ' ';
	p += strlen(strcpy(p, name));
    }

    *CrossPostedp = i > 1 || AlwaysCrosspost;
    Xref.Used = p - Xref.Data;
    Xref.Data[Xref.Used++] = '\n';
}


/*
**  Return TRUE if a list of strings has a specific one.  This is a
**  generic routine, but is used for seeing if a host is in the Path line.
*/
STATIC BOOL
ListHas(list, p)
    register char	**list;
    register char	*p;
{
    register char	*q;
    register char	c;

    for (c = *p; (q = *list) != NULL; list++)
	if (c == *q && caseEQ(p, q))
	    return TRUE;
    return FALSE;
}


/*
**  Propagate an article to the sites have "expressed an interest."
*/
STATIC void
ARTpropagate(Data, hops, hopcount, list)
    ARTDATA		*Data;
    char		**hops;
    int			hopcount;
    char		**list;
{
    register SITE	*sp;
    register int	i;
    register int	j;
    register int	Groupcount;
    register char	*p;
    register SITE	*funnel;
    register BUFFER	*bp;

    /* Work out which sites should really get it. */
    Groupcount = Data->Groupcount;
    for (sp = Sites, i = nSites; --i >= 0; sp++) {
	if (sp->Seenit || !sp->Sendit)
	    continue;
	sp->Sendit = FALSE;

	if (sp->Master != NOSITE && Sites[sp->Master].Seenit)
	    continue;

	if (sp->MaxSize && sp->MaxSize < Data->SizeValue)
	    /* Too big for the site. */
	    continue;

	if ((!sp->IgnorePath && ListHas(hops, sp->Name))
	 || (sp->Hops && hopcount > sp->Hops)
	 || (sp->Groupcount && Groupcount > sp->Groupcount))
	    /* Site already saw the article; path too long; or too much
	     * cross-posting. */
	    continue;

	if (list
	 && sp->Distributions
	 && !DISTwantany(sp->Distributions, list))
	    /* Not in the site's desired list of distributions. */
	    continue;
	if (sp->DistRequired && list == NULL)
	    /* Site requires Distribution header and there isn't one. */
	    continue;

	if (sp->Exclusions) {
	    for (j = 0; (p = sp->Exclusions[j]) != NULL; j++)
		if (ListHas(hops, p))
		    break;
	    if (p != NULL)
		/* A host in the site's exclusion list was in the Path. */
		continue;
	}

	/* Write that the site is getting it, and flag to send it. */
	if (fprintf(Log, " %s", sp->Name) == EOF || ferror(Log)) {
	    IOError("logging site");
	    syslog(L_ERROR, "%s cant write log_site %m", LogName);
	    clearerr(Log);
	}
	sp->Sendit = TRUE;
	sp->Seenit = TRUE;
	if (sp->Master != NOSITE)
	    Sites[sp->Master].Seenit = TRUE;
    }
    if (putc('\n', Log) == EOF
     || (!BufferedLogs && fflush(Log))
     || ferror(Log)) {
	syslog(L_ERROR, "%s cant write log_end %m", LogName);
	clearerr(Log);
    }

    /* Handle funnel sites. */
    for (sp = Sites, i = nSites; --i >= 0; sp++)
	if (sp->Sendit && sp->Funnel != NOSITE) {
	    sp->Sendit = FALSE;
	    funnel = &Sites[sp->Funnel];
	    funnel->Sendit = TRUE;
	    if (funnel->FNLwantsnames) {
		bp = &funnel->FNLnames;
		p = &bp->Data[bp->Used];
		if (bp->Used) {
		    *p++ = ' ';
		    bp->Used++;
		}
		bp->Used += strlen(strcpy(p, sp->Name));
	    }
	}
}


/*
**  Build up the overview data.
*/
STATIC void
ARTmakeoverview(Data)
    ARTDATA			*Data;
{
    static char			SEP[] = "\t";
    static char			COLONSPACE[] = ": ";
    static BUFFER		Overview;
    register ARTOVERFIELD	*fp;
    register ARTHEADER		*hp;
    register char		*p;
    register int		i;

    /* Setup. */
    if (Overview.Data == NULL)
	Overview.Data = NEW(char, 1);
    Data->Overview = &Overview;
    BUFFset(&Overview, Xref.Data + Xrefbase + 1, Xref.Used - (Xrefbase + 2));
    for (i = Overview.Left, p = Overview.Data; --i >= 0; p++)
	if (*p == '.' || *p == ':')
	    *p = '/';

    if (ARTfields == NULL) {
	/* User error. */
	return;
    }

    /* Write the data, a field at a time. */
    for (fp = ARTfields; fp->Header; fp++) {
	BUFFappend(&Overview, SEP, STRLEN(SEP));
	hp = fp->Header;
	if (!hp->Found)
	    continue;
	 if (fp->NeedHeader) {
	      BUFFappend(&Overview, hp->Name, hp->Size);
	      BUFFappend(&Overview, COLONSPACE, STRLEN(COLONSPACE));
	 }
	 i = Overview.Left;
	 BUFFappend(&Overview, hp->Value, hp->Length);
	 for (p = &Overview.Data[i]; i < Overview.Left; p++, i++)
	     if (*p == '\t' || *p == '\n')
		 *p = ' ';
    }
}


/*
**  This routine is the heart of it all.  Take a full article, parse it,
**  file or reject it, feed it to the other sites.  Return the NNTP
**  message to send back.
*/
STRING
ARTpost(cp, Replic, ihave)
    CHANNEL		*cp;
    BUFFER		*Replic;
    char		*ihave;
{
    static char		errNOSPACE[] = NNTP_RESENDIT_NOSPACE;
    static char		errNOHIST[] = NNTP_RESENDIT_NOHIST;
    static BUFFER	Files;
    static BUFFER	Header;
    static char		buff[SPOOLNAMEBUFF];
    register char	*p;
    register int	i;
    register int	j;
    register NEWSGROUP	*ngp;
    register NEWSGROUP	**ngptr;
    register int	*isp;
    register SITE	*sp;
    ARTDATA		Data;
    BOOL		Approved;
    BOOL		Accepted;
    BOOL		LikeNewgroup;
    BOOL		CrossPosted;
    BOOL		ToGroup;
    BOOL		GroupMissing;
    BUFFER		*article;
    char		linkname[SPOOLNAMEBUFF];
    char		**groups;
    char		**hops;
    int			hopcount;
    char		**distributions;
    STRING		error;
    char		ControlWord[SMBUF];
    int			ControlHeader;

    /* Preliminary clean-ups. */
    article = &cp->In;
    Data.MessageID = ihave;
    error = ARTclean(article, &Data);

    /* Fill in other Data fields. */
    Data.Poster = HDR(_sender);
    if (*Data.Poster == '\0')
	Data.Poster = HDR(_from);
    Data.Replyto = HDR(_reply_to);
    if (*Data.Replyto == '\0')
	Data.Replyto = HDR(_from);
    hops = ARTparsepath(HDR(_path), &hopcount);
#if	defined(DO_IPADDR_LOG)
    Data.Feedsite = RChostname(cp);
    if (Data.Feedsite == NULL)
	Data.Feedsite = CHANname(cp);
#else
    Data.Feedsite = hops && hops[0] ? hops[0] : CHANname(cp);
#endif	/* defined(DO_IPADDRLOG) */
    Data.FeedsiteLength = strlen(Data.Feedsite);
    (void)sprintf(Data.TimeReceived, "%lu", Now.time);
    Data.TimeReceivedLength = strlen(Data.TimeReceived);

    /* A duplicate? */
    if (error == NULL && HIShavearticle(Data.MessageID))
	error = "Duplicate article";

    /* Now see if we got an error in the article. */
    if (error != NULL) {
	(void)sprintf(buff, "%d %s", NNTP_REJECTIT_VAL, error);
	ARTlog(&Data, ART_REJECT, buff);
	ARTreject(buff, article);
	return buff;
    }

    /* Stash a copy of the Newsgroups header. */
    p = HDR(_newsgroups);
    i = strlen(p);
    if (Header.Data == NULL) {
	Header.Size = i;
	Header.Data = NEW(char, Header.Size + 1);
    }
    else if (Header.Size <= i) {
	Header.Size = i + 16;
	RENEW(Header.Data, char, Header.Size + 1);
    }
    (void)strcpy(Header.Data, p);
    Data.Newsgroups = Header.Data;
    Data.NewsgroupsLength = i;

    /* If we limit what distributions we get, see if we want this one. */
    p = HDR(_distribution);
    distributions = *p ? CommaSplit(p) : NULL;
    if (distributions) {
	DISTparse(distributions, &Data);
	if (ME.Distributions
	 && !DISTwantany(ME.Distributions, distributions)) {
	    (void)sprintf(buff, "%d Unwanted distribution \"%s\"",
		    NNTP_REJECTIT_VAL,
		    MaxLength(distributions[0], distributions[0]));
	    ARTlog(&Data, ART_REJECT, buff);
	    DISPOSE(distributions);
	    ARTreject(buff, article);
	    return buff;
	}
    }
    else {
	Data.Distribution = "?";
	Data.DistributionLength = 1;
    }

    /* Clear all groups and sites -- assume nobody gets the article. */
    for (i = nGroups, ngp = Groups; --i >= 0; ngp++)
	ngp->PostCount = 0;
    for (i = nSites, sp = Sites; --i >= 0; sp++) {
	sp->Sendit = FALSE;
	sp->Seenit = FALSE;
	sp->FNLnames.Used = 0;
	sp->ng = NULL;
    }

    /* Parse the Control or Also-Control header. */
    groups = NGsplit(HDR(_newsgroups));
    for (i = 0; groups[i] != NULL; i++)
	continue;
    Data.Groupcount = i;

    if (HDR(_control)[0] != '\0')
	ControlHeader = _control;
    else if (HDR(_alsocontrol)[0] != '\0')
	ControlHeader = _alsocontrol;
    else {
	ControlHeader = -1;
	LikeNewgroup = FALSE;
    }
    if (ControlHeader >= 0) {
	/* Nip off the first word into lowercase. */
	(void)strncpy(ControlWord, HDR(ControlHeader), sizeof ControlWord);
	ControlWord[sizeof ControlWord - 1] = '\0';
	for (p = ControlWord; *p && !ISWHITE(*p); p++)
	    if (CTYPE(isupper, *p))
		*p = tolower(*p);
	*p = '\0';
	LikeNewgroup = EQ(ControlWord, "newgroup")
		    || EQ(ControlWord, "rmgroup");

	/* Control messages to "foo.ctl" are treated as if they were
	 * posted to "foo".  I should probably apologize for all the
	 * side-effects in the if. */
	for (i = 0; (p = groups[i++]) != NULL; )
	    if ((j = strlen(p) - 4) > 0
	     && *(p += j) == '.'
	     && p[1] == 'c' && p[2] == 't' && p[3] == 'l')
		*p = '\0';
    }

    /* Loop over the newsgroups, see which ones we want, and get the
     * total space needed for the Xref line.  At the end of this section
     * of code, j will have the needed length, the appropriate site
     * entries will have their Sendit and ng fields set, and GroupPointers
     * will have pointers to the relevant newsgroups. */
    ToGroup = FALSE;
    p = HDR(_approved);
    Approved = *p != '\0';
    ngptr = GroupPointers;
    j = 0;
    for (GroupMissing = Accepted = FALSE; (p = *groups) != NULL; groups++) {
	if (!RCcanpost(cp, p))
	    continue;
	if ((ngp = NGfind(p)) == NULL) {
	    GroupMissing = TRUE;
	    if (LikeNewgroup && Approved) {
		/* Newgroup/rmgroup being sent to a group that doesn't
		 * exist.  Assume it is being sent to the group being
		 * created or removed, nd send the group to all sites that
		 * would or would have had the group if it were created. */
		ARTsendthegroup(*groups);
		Accepted = TRUE;
	    }

#if	defined(DO_MERGE_TO_GROUPS)
	    /* Try to collapse all "to" newsgroups. */
	    if (*p != 't' || *++p != 'o' || *++p != '.' || *++p == '\0')
		continue;
	    ngp = NGfind("to");
	    ToGroup = TRUE;
	    if ((sp = SITEfind(p)) != NULL) {
		SITEmark(sp, ngp);
	    }
#else
	    continue;
#endif	/* defined(DO_MERGE_TO_GROUPS) */
	}

	/* Ignore this group? */
	if (ngp->Rest[0] == NF_FLAG_IGNORE)
	    continue;

	/* Basic validity check. */
	if (ngp->Rest[0] == NF_FLAG_MODERATED && !Approved) {
	    (void)sprintf(buff, "%d Unapproved for \"%s\"",
		    NNTP_REJECTIT_VAL, ngp->Name);
	    ARTlog(&Data, ART_REJECT, buff);
	    if (distributions)
		DISPOSE(distributions);
	    ARTreject(buff, article);
	    return buff;
	}

	/* Valid group, feed it to that group's sites. */
	Accepted = TRUE;
	for (isp = ngp->Sites, i = ngp->nSites; --i >= 0; isp++)
	    if (*isp >= 0) {
		sp = &Sites[*isp];
		SITEmark(sp, ngp);
	    }

	/* If it's excluded, don't file it. */
	if (ngp->Rest[0] == NF_FLAG_EXCLUDED)
	    continue;

	/* Expand aliases, mark the article as getting filed in the group. */
	if (ngp->Alias != NULL)
	    ngp = ngp->Alias;
	*ngptr++ = ngp;
	ngp->PostCount = 0;
	j += ngp->NameLength + 1 + MAXARTFNAME + 1;
    }

    /* Control messages not filed in "to" get filed only in controlname
     * or control. */
    if (ControlHeader >= 0 && Accepted && !ToGroup) {
	FileGlue(buff, "control", '.', ControlWord);
	if ((ngp = NGfind(buff)) == NULL)
	    ngp = NGfind(ARTctl);
	ngp->PostCount = 0;
	ngptr = GroupPointers;
	*ngptr++ = ngp;
	j = ngp->NameLength + 1 + MAXARTFNAME;
	for (isp = ngp->Sites, i = ngp->nSites; --i >= 0; isp++)
	    if (*isp >= 0) {
		sp = &Sites[*isp];
		SITEmark(sp, ngp);
	    }
    }

    /* If !Accepted, then none of the article's newgroups exist in our
     * active file.  Proper action is to drop the article on the floor.
     * If ngp == GroupPointers, then all the new articles newsgroups are
     * "j" entries in the active file.  In that case, we have to file it
     * under junk so that downstream feeds can get it. */
    if (!Accepted || ngptr == GroupPointers) {
	if (!Accepted) {
	    (void)sprintf(buff, "%d Unwanted newsgroup \"%s\"",
		NNTP_REJECTIT_VAL,
		MaxLength(HDR(_newsgroups), HDR(_newsgroups)));
	    ARTlog(&Data, ART_REJECT, buff);
#if	defined(DONT_WANT_TRASH)
#if	defined(DO_REMEMBER_TRASH)
	    if (Mode == OMrunning && !HISwrite(&Data, ""))
		syslog(L_ERROR, "%s cant write history %s %m",
		    LogName, Data.MessageID);
#endif	/* defined(DO_REMEMBER_TRASH) */
	    if (distributions)
		DISPOSE(distributions);
	    ARTreject(buff, article);
	    return buff;
#else
	    /* if !GroupMissing, then all the groups the article was posted
	     * to have a flag of "x" in our active file, and therefore
	     * we should throw the article away:  if you have define
	     * DO_WANT_TRASH, then you want all trash except that which
	     * you explicitly excluded in your active file. */
	    if (!GroupMissing) {
		if (distributions)
		    DISPOSE(distributions);
		ARTreject(buff, article);
		return buff;
	    }
#endif	/* defined(DONT_WANT_TRASH) */
	}
	ngp = NGfind(ARTjnk);
	*ngptr++ = ngp;
	ngp->PostCount = 0;
	j = STRLEN(ARTjnk) + 1 + MAXARTFNAME;

	/* Junk can be fed to other sites. */
	for (isp = ngp->Sites, i = ngp->nSites; --i >= 0; isp++)
	    if (*isp >= 0) {
		sp = &Sites[*isp];
		SITEmark(sp, ngp);
	    }
    }
    *ngptr = NULL;
    CrossPosted = ngptr > &GroupPointers[1] || AlwaysCrosspost;
    j++;
    if (Replic)
	j = Replic->Used + 1;

    /* Make sure the Xref buffer has room. */
    Xref.Used = Xrefbase;
    if (Xref.Size <= j + Xrefbase + 2) {
	Xref.Size = j + Xrefbase + 2;
	RENEW(Xref.Data, char, Xref.Size + 1);
    }

    /* Make sure the filename buffer has room. */
    if (Files.Data == NULL) {
	Files.Size = j;
	Files.Data = NEW(char, Files.Size + 1);
    }
    else if (Files.Size <= j) {
	Files.Size = j;
	RENEW(Files.Data, char, Files.Size + 1);
    }

    /* Assign article numbers, fill in Xref buffer. */
    if (Replic == NULL)
	ARTassignnumbers();
    else
	ARTreplic(Replic, &CrossPosted);

    /* Optimize how we place the article on the disk. */
    ARTsortfordisk();

    /* Now we can file it. */
    if (++ICDactivedirty >= ICD_SYNC_COUNT) {
	ICDwriteactive();
	ICDactivedirty = 0;
    }
    Data.Name[0] = '\0';
    p = Files.Data;
    *p = '\0';
    for (i = 0; (ngp = GroupPointers[i]) != NULL; i++) {
	if (!ngp->PostCount)
	    continue;
	ngp->PostCount = 0;

	if (Data.Name[0] == '\0') {
	    /* Write the article the first time. */
	    (void)sprintf(Data.Name, "%s/%lu", ngp->Dir, ngp->Filenum);
	    if (ARTwrite(Data.Name, article, &Data, CrossPosted) < 0
	     && (!MakeSpoolDirectory(ngp->Dir)
	      || ARTwrite(Data.Name, article, &Data, CrossPosted) < 0)) {
		syslog(L_ERROR, "%s cant write %s %m", LogName, Data.Name);
		ARTlog(&Data, ART_REJECT, errNOSPACE);
		if (distributions)
		    DISPOSE(distributions);
		ARTreject(buff, article);
		return errNOSPACE;
	    }
	    p += strlen(strcpy(p, Data.Name));
	    Data.NameLength = strlen(Data.Name);
	}
	else {
	    /* Link to the main article. */
	    (void)sprintf(linkname, "%s/%lu", ngp->Dir, ngp->Filenum);
	    if (link(Data.Name, linkname) < 0
	     && (!MakeSpoolDirectory(ngp->Dir)
	      || link(Data.Name, linkname) < 0)) {
#if	defined(DONT_HAVE_SYMLINK)
		IOError("linking article");
		syslog(L_ERROR, "%s cant link %s and %s %m",
		    LogName, Data.Name, linkname);
		continue;
#else
		/* Try to make a symbolic link to the full pathname. */
		FileGlue(buff, SPOOL, '/', Data.Name);
		if (symlink(buff, linkname) < 0
		 && (!MakeSpoolDirectory(ngp->Dir)
		  || symlink(buff, linkname) < 0)) {
		    IOError("symlinking article");
		    syslog(L_ERROR, "%s cant symlink %s and %s %m",
			LogName, buff, linkname);
		    continue;
		}
#endif	/* defined(DONT_HAVE_SYMLINK) */
	    }
	    *p++ = ' ';
	    p += strlen(strcpy(p, linkname));
	}
    }

    /* Update history if we didn't get too many I/O errors above. */
    if (Mode != OMrunning || !HISwrite(&Data, Files.Data)) {
	syslog(L_ERROR, "%s cant write history %s %m", LogName, Data.MessageID);
	ARTlog(&Data, ART_REJECT, errNOHIST);
	if (distributions)
	    DISPOSE(distributions);
	ARTreject(buff, article);
	return errNOHIST;
    }
    /* If we just flushed the active (above), now flush history. */
    if (ICDactivedirty == 0)
	HISsync();

    /* We wrote the history, so modify it and save it for output. */
    for (Data.Replic = Files.Data, p = (char *)Data.Replic; *p; p++)
	if (*p == ' ')
	    *p = ',';
    Data.ReplicLength = p - Data.Replic;

    /* Start logging, then propagate the article. */
    ARTlog(&Data, Accepted ? ART_ACCEPT : ART_JUNK, (char *)NULL);
#if	defined(DO_NNTPLINK_LOG)
    if (fprintf(Log, " (%s)", Data.Name) == EOF || ferror(Log)) {
	IOError("logging nntplink");
	syslog(L_ERROR, "%s cant write log_nntplink %m", LogName);
	clearerr(Log);
    }
#endif	/* defined(DO_NNTPLINK_LOG) */
    ARTpropagate(&Data, hops, hopcount, distributions);
    if (distributions)
	DISPOSE(distributions);

    /* Now that it's been written, process the control message.  This has
     * a small window, if we get a new article before the newgroup message
     * has been processed.  We could pause ourselves here, but it doesn't
     * seem to be worth it. */
    if (Accepted) {
	if (ControlHeader >= 0)
	    ARTcontrol(&Data, HDR(ControlHeader));
	p = HDR(_supersedes);
	if (*p)
	    ARTcancel(&Data, p, FALSE);
    }

    /* If we need the overview data, write it. */
    if (NeedOverview)
	ARTmakeoverview(&Data);

    /* And finally, send to everyone who should get it */
    for (sp = Sites, i = nSites; --i >= 0; sp++)
	if (sp->Sendit)
	    SITEsend(sp, &Data);

    return NNTP_TOOKIT;
}
