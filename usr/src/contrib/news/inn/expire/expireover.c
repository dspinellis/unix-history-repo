/*  $Revision: 1.3 $
**
**  Expire overview database.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <errno.h>
#include "clibrary.h"
#include "qio.h"
#include "mydir.h"
#include "libinn.h"
#include "macros.h"
#include "paths.h"


#define	START_LIST_SIZE	128


/*
**   Information about a line in the overview file.
*/
typedef struct _LINE {
    ARTNUM	Article;
    char	*Start;
    int		Length;
    int		Offset;
} LINE;


/*
**  A list of articles; re-uses space.
*/
typedef struct _LIST {
    int		Used;
    int		Size;
    ARTNUM	*Articles;
} LIST;


/*
**  A buffer; re-uses space.
*/
typedef struct _BUFFER {
    int		Used;
    int		Size;
    char	*Data;
} BUFFER;


/*
**  Information about the schema of the news overview files.
*/
typedef struct _ARTOVERFIELD {
    char	*Header;
    int		Length;
    BOOL	HasHeader;
} ARTOVERFIELD;


/*
**  Append an article to an LIST.
*/
#define LISTappend(L, a)	\
	if ((L).Size == (L).Used) {			\
	    (L).Size *= 2;				\
	    RENEW((L).Articles, ARTNUM, (L).Size);	\
	    (L).Articles[(L).Used++] = (a);		\
	}						\
	else						\
	    (L).Articles[(L).Used++] = (a)


/*
**  Global variables.
*/
STATIC char		SPOOL[] = _PATH_SPOOL;
STATIC char		*SCHEMA = _PATH_SCHEMA;
STATIC BOOL		InSpoolDir;
STATIC BOOL		Verbose;
STATIC BOOL		DoNothing;
STATIC ARTOVERFIELD	*ARTfields;
STATIC int		ARTfieldsize;


/*
**  Sorting predicate for qsort to put articles in numeric order.
*/
STATIC int
LISTcompare(p1, p2)
    POINTER	*p1;
    POINTER	*p2;
{
    ARTNUM	*ip1;
    ARTNUM	*ip2;

    ip1 = CAST(ARTNUM*, p1);
    ip2 = CAST(ARTNUM*, p2);
    return *ip1 - *ip2;
}


/*
**  If list is big enough, and out of order, sort it.
*/
STATIC void
LISTsort(lp)
    LIST	*lp;
{
    register int	i;
    register ARTNUM	*ap;

    for (ap = lp->Articles, i = lp->Used - 1; --i >= 0; ap++)
	if (ap[0] >= ap[1]) {
	    qsort((POINTER)lp->Articles, (SIZE_T)lp->Used,
		sizeof lp->Articles[0], LISTcompare);
	    break;
	}
}


/*
**  Unlock the group.
*/
STATIC void
UnlockGroup(lfd, lockfile)
    int		lfd;
    char	*lockfile;
{
    if (lfd > 0) {
	if (unlink(lockfile) < 0 && errno != ENOENT)
	    (void)fprintf(stderr, "expireover cant unlink %s %s\n",
		    lockfile, strerror(errno));
	if (close(lfd) < 0)
	    (void)fprintf(stderr, "expireover cant close %s %s\n",
		    lockfile, strerror(errno));
	lfd = -1;
    }
}


/*
**  Sorting predicate to put lines in numeric order.
*/
STATIC int
LINEcompare(p1, p2)
    POINTER	*p1;
    POINTER	*p2;
{
    LINE	*lp1;
    LINE	*lp2;

    lp1 = CAST(LINE*, p1);
    lp2 = CAST(LINE*, p2);
    return lp1->Article - lp2->Article;
}


/*
**  Take in a sorted list of count article numbers in group, and delete
**  them from the overview file.
*/
STATIC void
RemoveLines(group, Deletes)
    char			*group;
    LIST			*Deletes;
{
    static BUFFER		B;
    static LINE			*Lines;
    static int			LineSize;
    register struct iovec	*vp;
    register LINE		*lp;
    register LINE		*end;
    register char		*p;
    register char		*next;
    register ARTNUM		*ap;
    register int		i;
    struct stat			Sb;
    struct iovec		iov[8];
    char			file[SPOOLNAMEBUFF];
    char			lockfile[SPOOLNAMEBUFF];
    int				fd;
    int				count;
    int				lfd;

    if (Verbose) {
	for (ap = Deletes->Articles, i = Deletes->Used; --i >= 0; ap++)
	    (void)printf("- %s/%ld\n", group, *ap);
	if (DoNothing)
	    return;
    }

    /* Lock the group. */
    (void)sprintf(lockfile, "%s/.LCK%s", group, _PATH_OVERVIEW);
    lfd = open(lockfile, O_WRONLY | O_TRUNC | O_CREAT, ARTFILE_MODE);
    if (lfd < 0) {
	(void)fprintf(stderr, "Can't open %s, %s\n", lockfile, strerror(errno));
	return;
    }

    /* Open file, lock it. */
    (void)sprintf(file, "%s/%s", group, _PATH_OVERVIEW);
    for ( ; ; ) {
	if ((fd = open(file, O_RDWR)) < 0) {
	    (void)fprintf(stderr, "Can't open %s, %s\n", file, strerror(errno));
	    UnlockGroup(lfd, lockfile);
	    return;
	}
	if (LockFile(fd, FALSE) >= 0)
	    break;
	/* Wait for lock; close file -- might be unlinked -- and try again. */
	(void)LockFile(fd, TRUE);
	(void)close(fd);
    }

    if (fstat(fd, &Sb) < 0) {
	(void)fprintf(stderr, "Can't open %s, %s\n", file, strerror(errno));
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }
    if (Sb.st_size == 0) {
	/* Empty file; done deleting. */
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }

    /* Read in the whole file. */
    if (B.Size == 0) {
	B.Size = Sb.st_size + 1;
	B.Data = NEW(char, B.Size);
    }
    else if (B.Size < Sb.st_size) {
	B.Size = Sb.st_size + 1;
	RENEW(B.Data, char, B.Size);
    }
    if (xread(fd, B.Data, Sb.st_size) < 0) {
	(void)fprintf(stderr, "Can't read %s, %s\n", file, strerror(errno));
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }
    B.Data[Sb.st_size] = '\0';

    /* Count lines, get space. */
    for (i = 1, p = B.Data; (p = strchr(p, '\n')) != NULL && *++p; i++)
	continue;
    if (LineSize == 0) {
	LineSize = i;
	Lines = NEW(LINE, LineSize + 1);
    }
    else if (LineSize < i) {
	LineSize = i;
	RENEW(Lines, LINE, LineSize + 1);
    }

    /* Build line array. */
    for (lp = Lines, p = B.Data; ; p = next, lp++) {
	if ((next = strchr(p, '\n')) == NULL)
	    break;
	lp->Start = p;
	lp->Length = ++next - p;
	lp->Article = atol(p);
    }
    qsort((POINTER)Lines, (SIZE_T)(lp - Lines), sizeof lp[0], LINEcompare);

    /* Remove duplicates. */
    for (end = lp - 1, lp = Lines; lp < end; lp++)
	if (lp[0].Article == lp[1].Article)
	    lp->Article = 0;

    /* Scan through lines, collecting clumps and skipping holes. */
    ap = Deletes->Articles;
    count = Deletes->Used;
    iov[0].iov_len = 0;
    for (vp = iov, lp = Lines; lp < end + 1; lp++) {
	/* An already-removed article, or one that should be? */
	if (lp->Article == 0)
	    continue;

	/* Skip delete items before the current one. */
	while (*ap < lp->Article && count > 0) {
	    ap++;
	    count--;
	}

	if (count > 0 && lp->Article == *ap) {
	    while (*ap == lp->Article && count > 0) {
		ap++;
		count--;
	    }
	    continue;
	}

	/* We're keeping this entry; see if we can add it to any
	 * in-progress iov element. */
	if (vp->iov_len) {
	    if (vp->iov_base + vp->iov_len == lp->Start) {
		/* Contiguous. */
		vp->iov_len += lp->Length;
		continue;
	    }

	    /* Doesn't fit -- get a new element. */
	    if (++vp == ENDOF(iov)) {
		if (xwritev(lfd, iov, SIZEOF(iov)) < 0) {
		    (void)fprintf(stderr, "Can't write %s, %s\n",
			    lockfile, strerror(errno));
		    UnlockGroup(lfd, lockfile);
		    (void)close(fd);
		    return;
		}
		vp = iov;
	    }
	}

	/* Start new element. */
	vp->iov_base = lp->Start;
	vp->iov_len = lp->Length;
    }

    /* Write out remaining. */
    if (vp->iov_len)
	vp++;
    if (iov[0].iov_len && xwritev(lfd, iov, vp - iov) < 0) {
	(void)fprintf(stderr, "Can't write %s, %s\n",
		lockfile, strerror(errno));
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }

    if (rename(lockfile, file) < 0)
	(void)fprintf(stderr, "Can't rename %s, %s\n",
		lockfile, strerror(errno));

    /* Don't call UnlockGroup; do it inline. */
    if (close(lfd) < 0)
	(void)fprintf(stderr, "expireover cant close %s %s\n",
		file, strerror(errno));
    if (close(fd) < 0)
	(void)fprintf(stderr, "expireover cant close unlinked %s %s\n",
		file, strerror(errno));
}


/*
**  Read the overview schema.
*/
void
ARTreadschema()
{
    register FILE		*F;
    register char		*p;
    register ARTOVERFIELD	*fp;
    register int		i;
    char			buff[SMBUF];

    /* Open file, count lines. */
    if ((F = fopen(SCHEMA, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open %s, %s\n", SCHEMA, strerror(errno));
	exit(1);
    }
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    ARTfields = NEW(ARTOVERFIELD, i + 1);

    /* Parse each field. */
    for (fp = ARTfields; fgets(buff, sizeof buff, F) != NULL; ) {
	/* Ignore blank and comment lines. */
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, COMMENT_CHAR)) != NULL)
	    *p = '\0';
	if (buff[0] == '\0')
	    continue;
	if ((p = strchr(buff, ':')) != NULL) {
	    *p++ = '\0';
	    fp->HasHeader = EQ(p, "full");
	}
	else
	    fp->HasHeader = FALSE;
	fp->Header = COPY(buff);
	fp->Length = strlen(buff);
	fp++;
    }
    ARTfieldsize = fp - ARTfields;
    (void)fclose(F);
}


/*
**  Read an article and create an overview line without the trailing
**  newline.  Returns pointer to static space or NULL on error.
*/
STATIC char *
OVERgen(name)
    char			*name;
{
    static ARTOVERFIELD		*Headers;
    static BUFFER		B;
    register ARTOVERFIELD	*fp;
    register ARTOVERFIELD	*hp;
    register QIOSTATE		*qp;
    register char		*colon;
    register char		*line;
    register char		*p;
    register int		i;
    register int		size;
    register int		ov_size;
    register long		lines;
    struct stat			Sb;
    long			t;
    char			value[10];

    /* Open article. */
    if ((qp = QIOopen(name, QIO_BUFFER)) == NULL)
	return NULL;
    if ((p = strrchr(name, '/')) != NULL)
	name = p + 1;

    /* Set up place to store headers. */
    if (Headers == NULL) {
	Headers = NEW(ARTOVERFIELD, ARTfieldsize);
	for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++)
	    hp->Length = 0;
    }
    for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++)
	hp->HasHeader = FALSE;

    for ( ; ; ) {
	/* Read next line. */
	if ((line = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    /* Error or EOF (in headers!?); shouldn't happen. */
	    QIOclose(qp);
	    return NULL;
	}

	/* End of headers? */
	if (*line == '\0')
	    break;

	/* See if we want this header. */
	fp = ARTfields;
	for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++, fp++) {
	    colon = &line[fp->Length];
	    if (*colon != ':')
		continue;
	    *colon = '\0';
	    if (!caseEQ(line, fp->Header)) {
		*colon = ':';
		continue;
	    }
	    *colon = ':';
	    if (fp->HasHeader)
		p = line;
	    else
		/* Skip colon and whitespace, store value. */
		for (p = colon; *++p && ISWHITE(*p); )
		    continue;
	    size = strlen(p);
	    if (hp->Length == 0) {
		hp->Length = size;
		hp->Header = NEW(char, hp->Length + 1);
	    }
	    else if (hp->Length < size) {
		hp->Length = size;
		RENEW(hp->Header, char, hp->Length + 1);
	    }
	    (void)strcpy(hp->Header, p);
	    for (p = hp->Header; *p; p++)
		if (*p == '\t' || *p == '\n')
		    *p = ' ';
	    hp->HasHeader = TRUE;
	}
    }

    /* Read body of article, just to get lines. */
    for (lines = 0; ; lines++)
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    if (QIOerror(qp)) {
		QIOclose(qp);
		return NULL;
	    }
	    break;
	}

    /* Calculate total size, fix hardwired headers. */
    ov_size = strlen(name) + ARTfieldsize + 2;
    for (hp = Headers, fp = ARTfields, i = ARTfieldsize; --i >= 0; hp++, fp++) {
	if (caseEQ(fp->Header, "Bytes") || caseEQ(fp->Header, "Lines")) {
	    if (fp->Header[0] == 'B' || fp->Header[0] == 'b')
		t = fstat(QIOfileno(qp), &Sb) >= 0 ? (long)Sb.st_size : 0L;
	    else
		t = lines;

	    (void)sprintf(value, "%ld", t);
	    size = strlen(value);
	    if (hp->Length == 0) {
		 hp->Length = size;
		hp->Header = NEW(char, hp->Length + 1);
	    }
	    else if (hp->Length < size) {
		hp->Length = size;
		RENEW(hp->Header, char, hp->Length + 1);
	    }
	    (void)strcpy(hp->Header, value);
	    hp->HasHeader = TRUE;
       }
       if (hp->HasHeader)
	   ov_size += strlen(hp->Header);
    }

    /* Get space. */
    if (B.Size == 0) {
	B.Size = ov_size;
	B.Data = NEW(char, B.Size + 1);
    }
    else if (B.Size < ov_size) {
	B.Size = ov_size;
	RENEW(B.Data, char, B.Size + 1);
    }

    /* Glue all the fields together. */
    p = B.Data + strlen(strcpy(B.Data, name));
    for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++) {
	*p++ = '\t';
	if (hp->HasHeader)
	    p += strlen(strcpy(p, hp->Header));
    }
    *p = '\0';

    QIOclose(qp);
    return B.Data;
}


/*
**  Take in a sorted list of count article numbers in group, and add
**  them them to the overview file.
*/
STATIC void
AddLines(group, Adds)
    char			*group;
    LIST			*Adds;
{
    static BUFFER		New;
    static BUFFER		B;
    static LINE			*Lines;
    static int			LineSize;
    register LINE		*lp;
    register char		*next;
    register int		i;
    register struct iovec	*vp;
    register ARTNUM		*ap;
    LINE			*end;
    struct iovec		iov[8];
    struct stat			Sb;
    char			*p;
    char			file[SPOOLNAMEBUFF];
    char			lockfile[SPOOLNAMEBUFF];
    int				LineUsed;
    int				fd;
    int				lfd;

    if (Verbose) {
	for (ap = Adds->Articles, i = Adds->Used; --i >= 0; ap++)
	    (void)printf("+ %s/%ld\n", group, *ap);
	if (DoNothing)
	    return;
    }

    /* Get space. */
    if (New.Data == NULL) {
	New.Size = 1024;
	New.Data = NEW(char, New.Size);
	LineSize = Adds->Size + 1;
	Lines = NEW(LINE, LineSize);
    }
    else if (LineSize < Adds->Size) {
	LineSize = Adds->Size + 1;
	RENEW(Lines, LINE, LineSize);
    }

    New.Used = 0;
    for (lp = Lines, ap = Adds->Articles, i = Adds->Used; --i >= 0; ap++) {
	/* Get the overview data. */
	if (InSpoolDir)
	    (void)sprintf(file, "%s/%ld", group, *ap);
	else
	    (void)sprintf(file, "%s/%s/%ld", SPOOL, group, *ap);
	if ((p = OVERgen(file)) == NULL)
	    continue;

	/* Add it to the buffer and the lines array. */
	lp->Article = *ap;
	lp->Length = strlen(p);
	lp->Offset = New.Used;
	while (New.Size < New.Used + lp->Length + 1) {
	    New.Size *= 2;
	    RENEW(New.Data, char, New.Size);
	}
	(void)strcpy(&New.Data[New.Used], p);
	New.Used += lp->Length++;
	New.Data[New.Used++] = '\n';
	lp++;
    }
    LineUsed = lp - Lines;

    /* Turn offsets into real pointers. */
    for (i = 0, lp = Lines; i < LineUsed; i++, lp++)
	lp->Start = New.Data + lp->Offset;

    /* Lock the group. */
    (void)sprintf(lockfile, "%s/.LCK%s", group, _PATH_OVERVIEW);
    lfd = open(lockfile, O_WRONLY | O_TRUNC | O_CREAT, ARTFILE_MODE);
    if (lfd < 0) {
	(void)fprintf(stderr, "Can't open %s, %s\n", lockfile, strerror(errno));
	return;
    }

    /* Open file, lock it. */
    (void)sprintf(file, "%s/%s", group, _PATH_OVERVIEW);
    for ( ; ; ) {
	if ((fd = open(file, O_RDWR | O_CREAT, ARTFILE_MODE)) < 0) {
	    (void)fprintf(stderr, "Can't open %s, %s\n", file, strerror(errno));
	    UnlockGroup(lfd, lockfile);
	    return;
	}
	if (LockFile(fd, FALSE) >= 0)
	    break;
	/* Wait for lock; close file -- might be unlinked -- and try again. */
	(void)LockFile(fd, TRUE);
	(void)close(fd);
    }

    if (fstat(fd, &Sb) < 0) {
	(void)fprintf(stderr, "Can't open %s, %s\n", file, strerror(errno));
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }

    if (Sb.st_size != 0) {
	/* Read in the whole file. */
	if (B.Size == 0) {
	    B.Size = Sb.st_size + 1;
	    B.Data = NEW(char, B.Size);
	}
	else if (B.Size < Sb.st_size) {
	    B.Size = Sb.st_size + 1;
	    RENEW(B.Data, char, B.Size);
	}
	if (xread(fd, B.Data, Sb.st_size) < 0) {
	    (void)fprintf(stderr, "Can't read %s, %s\n",
		    file, strerror(errno));
	    UnlockGroup(lfd, lockfile);
	    (void)close(fd);
	    return;
	}
	B.Data[Sb.st_size] = '\0';

	/* Count lines, get space. */
	for (i = 1, p = B.Data; (p = strchr(p, '\n')) != NULL && *++p; i++)
	    continue;
	if (LineSize < i + 1 + LineUsed) {
	    LineSize = i + 1 + LineUsed;
	    RENEW(Lines, LINE, LineSize);
	}

	/* Add to lines array. */
	for (lp = Lines + LineUsed, p = B.Data; ; p = next, lp++) {
	    if ((next = strchr(p, '\n')) == NULL)
		break;
	    lp->Start = p;
	    lp->Length = ++next - p;
	    lp->Article = atol(p);
	}
	qsort((POINTER)Lines, (SIZE_T)(lp - Lines), sizeof lp[0],
	    LINEcompare);
	LineUsed = lp - Lines;
    }

    /* Remove duplicates. */
    for (end = lp - 1, lp = Lines; lp < end; lp++)
	if (lp[0].Article == lp[1].Article)
	    lp->Article = 0;

    /* Scan through lines, collecting rocks and holes. */
    iov[0].iov_len = 0;
    for (vp = iov, lp = Lines; lp < end + 1; lp++) {
	/* An already-removed article, or one that should be? */
	if (lp->Article == 0)
	    continue;

	/* We're keeping this entry; see if we can add it to any in-progress
	 * iov element. */
	if (vp->iov_len) {
	    if (vp->iov_base + vp->iov_len == lp->Start) {
		/* Contiguous. */
		vp->iov_len += lp->Length;
		continue;
	    }

	    /* Doesn't fit -- get a new element. */
	    if (++vp == ENDOF(iov)) {
		if (xwritev(lfd, iov, SIZEOF(iov)) < 0) {
		    (void)fprintf(stderr, "Can't write %s, %s\n",
			    lockfile, strerror(errno));
		    UnlockGroup(lfd, lockfile);
		    (void)close(fd);
		    return;
		}
		vp = iov;
	    }
	}

	/* Start new element. */
	vp->iov_base = lp->Start;
	vp->iov_len = lp->Length;
    }

    if (vp->iov_len)
	vp++;

    /* Write out remaining. */
    if (iov[0].iov_len && xwritev(lfd, iov, vp - iov) < 0) {
	(void)fprintf(stderr, "Can't write %s, %s\n",
		       lockfile, strerror(errno));
	UnlockGroup(lfd, lockfile);
	(void)close(fd);
	return;
    }

    if (rename(lockfile, file) < 0)
	(void)fprintf(stderr, "Can't rename %s, %s\n",
		       lockfile, strerror(errno));

    /* Don't call UnlockGroup; do it inline. */
    if (close(lfd) < 0)
	(void)fprintf(stderr, "expireover cant close %s %s\n",
		file, strerror(errno));
    if (close(fd) < 0)
	(void)fprintf(stderr, "expireover cant close unlinked %s %s\n",
		file, strerror(errno));
}


/*
**  Expire by batch, or line at a time.
*/
STATIC void
Expire(SortedInput, qp)
    BOOL		SortedInput;
    register QIOSTATE	*qp;
{
    static LIST		List;
    register char	*line;
    register char	*p;
    char		group[SPOOLNAMEBUFF];

    if (List.Articles == NULL) {
	List.Size = START_LIST_SIZE;
	List.Articles = NEW(ARTNUM, List.Size);
    }
    List.Used = 0;

    if (SortedInput) {
	for ( ; ; ) {
	    if ((line = QIOread(qp)) == NULL) {
		if (QIOerror(qp)) {
		    (void)fprintf(stderr, "Can't read input %s\n",
			    strerror(errno));
		    break;
		}
		if (QIOtoolong(qp))
		    continue;
		break;
	    }
	    if ((p = strrchr(line, '/')) == NULL)
		continue;
	    *p++ = '\0';
	    if (List.Used == 0) {
		(void)strcpy(group, line);
		List.Used = 0;
	    }
	    else if (!EQ(line, group)) {
		LISTsort(&List);
		RemoveLines(group, &List);
		(void)strcpy(group, line);
		List.Used = 0;
	    }
	    LISTappend(List, atol(p));
	}

	/* Do the last group. */
	if (List.Used) {
	    LISTsort(&List);
	    RemoveLines(group, &List);
	}
    }
    else {
	for (List.Used = 1; ; ) {
	    if ((line = QIOread(qp)) == NULL) {
		if (QIOerror(qp)) {
		    (void)fprintf(stderr, "Can't read input %s\n",
			    strerror(errno));
		    break;
		}
		if (QIOtoolong(qp))
		    continue;
		break;
	    }
	    if ((p = strrchr(line, '/')) == NULL)
		continue;
	    *p++ = '\0';
	    List.Articles[0] = atol(p);
	    RemoveLines(line, &List);
	}
    }

    QIOclose(qp);
}


/*
**  Read the overview file, return sorted list of all articles in it.
*/
STATIC LIST *
GetOverviewList(group)
    char		*group;
{
    static LIST		List;
    register QIOSTATE	*qp;
    register char	*p;
    char		file[SPOOLNAMEBUFF];

    /* Open the file. */
    (void)sprintf(file, "%s/%s", group, _PATH_OVERVIEW);
    if ((qp = QIOopen(file, QIO_BUFFER)) == NULL)
	return NULL;

    /* Setup the article list. */
    if (List.Articles == NULL) {
	List.Size = START_LIST_SIZE;
	List.Articles = NEW(ARTNUM, List.Size);
    }
    List.Used = 0;

    /* Read all lines, picking up the article number. */
    for ( ; ; ) {
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOerror(qp)) {
		(void)fprintf(stderr, "Can't read %s, %s\n",
			file, strerror(errno));
		QIOclose(qp);
		return NULL;
	    }
	    if (QIOtoolong(qp))
		continue;
	    break;
	}
	LISTappend(List, atol(p));
    }
    QIOclose(qp);

    if (List.Used == 0)
	return NULL;
    LISTsort(&List);
    return &List;
}


/*
**  Read spool directory and return sorted list of articles or NULL on error.
*/
STATIC LIST *
GetSpoolList(group)
    char		*group;
{
    static LIST		List;
    register DIR	*dp;
    register DIRENTRY	*ep;
    register char	*p;
    char		buff[SPOOLNAMEBUFF];

    /* Open directory. */
    if (InSpoolDir)
	(void)strcpy(buff, group);
    else
	(void)sprintf(buff, "%s/%s", SPOOL, group);
    if ((dp = opendir(buff)) == NULL)
	return NULL;

    /* Setup article list. */
    if (List.Articles == NULL) {
	List.Size = START_LIST_SIZE;
	List.Articles = NEW(ARTNUM, List.Size);
    }
    List.Used = 0;

    /* Get all articles. */
    while ((ep = readdir(dp)) != NULL) {
	p = ep->d_name;
	if (!CTYPE(isdigit, p[0]) || strspn(p, "0123456789") != strlen(p))
	    continue;
	LISTappend(List, atol(p));
    }
    (void)closedir(dp);

    if (List.Used == 0)
	return NULL;
    LISTsort(&List);
    return &List;
}


/*
**  Return a list of all articles in the Over list that are not in
**  the Spool list.  Both lists are sorted.  See SpoolUpdate for an
**  explanation of the names.
*/
STATIC LIST *
GetNotIn(Over, Spool)
    register LIST	*Over;
    register LIST	*Spool;
{
    static LIST		List;
    register ARTNUM	*oEnd;
    register ARTNUM	*sEnd;
    register ARTNUM	*o;
    register ARTNUM	*s;

    /* Setup the list. */
    if (List.Articles == NULL) {
	List.Size = START_LIST_SIZE;
	List.Articles = NEW(ARTNUM, List.Size);
    }
    List.Used = 0;

    o = Over->Articles;
    s = Spool->Articles;
    oEnd = o + Over->Used;
    sEnd = s + Spool->Used;
    while (o != oEnd && s != sEnd) {
	if (*o < *s) {
	    LISTappend(List, *o++);
	    continue;
	}
	if (*o == *s)
	    o++;
	s++;
    }

    /* If we hit the end of the Spool, then add everything else in the
     * Overview. */
    if (s == sEnd) {
	while (o != oEnd)
	    LISTappend(List, *o++);
    }

    return List.Used ? &List : NULL;
}


/*
**  Try to make one directory.  Return FALSE on error.
*/
STATIC BOOL
MakeDir(Name)
    char	*Name;
{
    struct stat	Sb;

    if (mkdir(Name, GROUPDIR_MODE) >= 0)
	return TRUE;

    /* See if it failed because it already exists. */
    return stat(Name, &Sb) >= 0 && S_ISDIR(Sb.st_mode);
}


/*
**  Given a directory, comp/foo/bar, create that directory and all
**  intermediate directories needed.  Return FALSE on error.
*/
BOOL
MakeOverDir(Name)
    register char	*Name;
{
    register char	*p;
    BOOL		made;

    /* Optimize common case -- parent almost always exists. */
    if (MakeDir(Name))
	return TRUE;

    /* Try to make each of comp and comp/foo in turn. */
    for (p = Name; *p; p++)
	if (*p == '/') {
	    *p = '\0';
	    made = MakeDir(Name);
	    *p = '/';
	    if (!made)
		return FALSE;
	}

    return MakeDir(Name);
}


/*
**  Update using the News Spool.  Either add or delete entries.
*/
STATIC void
SpoolUpdate(AddEntries, Name)
    BOOL		AddEntries;
    char		*Name;
{
    register QIOSTATE	*qp;
    register char	*line;
    register char	*p;
    LIST		*Over;
    LIST		*Spool;
    LIST		*Missing;

    /* Open file. */
    if (EQ(Name, "-"))
	qp = QIOfdopen(STDIN, QIO_BUFFER);
    else if ((qp = QIOopen(Name, QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't open %s, %s\n", Name, strerror(errno));
	exit(1);
    }
    if (AddEntries)
	ARTreadschema();

    for ( ; ; ) {
	if ((line = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp) || QIOerror(qp)) {
		(void)fprintf(stderr,
			"Line too long or error reading %s, %s\n",
			Name, strerror(errno));
		exit(1);
	    }
	    break;
	}

	/* Nip off newsgroup name, and turn it into a directory. */
	for (p = line; *p && !ISWHITE(*p) && *p != '\n'; p++)
	    if (*p == '.')
		*p = '/';
	*p = '\0';

	if (AddEntries) {
	    if ((Spool = GetSpoolList(line)) == NULL)
		continue;
	    if ((Over = GetOverviewList(line)) != NULL) {
		if ((Missing = GetNotIn(Spool, Over)) != NULL)
		    AddLines(line, Missing);
	    }
	    else if (!InSpoolDir) {
		if (MakeOverDir(line))
		    AddLines(line, Spool);
		else
		    (void)fprintf(stderr, "expireover: cant mkdir %s, %s\n",
			    line, strerror(errno));
	    }
	    else
		AddLines(line, Spool);
	    continue;
	}

	if ((Over = GetOverviewList(line)) == NULL)
	    continue;
	if ((Spool = GetSpoolList(line)) != NULL) {
	    if ((Missing = GetNotIn(Over, Spool)) != NULL)
		RemoveLines(line, Missing);
	}
	else
	    RemoveLines(line, Over);
    }

    QIOclose(qp);
    exit(0);
}



/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage:  expireover [flags] [file...]\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register int	i;
    QIOSTATE		*qp;
    BOOL		AddEntries;
    BOOL		ReadSpool;
    BOOL		SortedInput;
    char		*Dir;
    char		*Name;

    /* Set defaults. */
    Dir = _PATH_OVERVIEWDIR;
    Name = _PATH_ACTIVE;
    AddEntries = FALSE;
    ReadSpool = FALSE;
    SortedInput = FALSE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "aD:f:nO:svz")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'a':
	    AddEntries = TRUE;
	    ReadSpool = TRUE;
	    break;
	case 'D':
	    Dir = optarg;
	    break;
	case 'f':
	    Name = optarg;
	    break;
	case 'n':
	    DoNothing = TRUE;
	    break;
	case 'O':
	    SCHEMA = optarg;
	    break;
	case 's':
	    ReadSpool = TRUE;
	    break;
	case 'v':
	    Verbose = TRUE;
	    break;
	case 'z':
	    SortedInput = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if ((ReadSpool && ac) || (AddEntries && !ReadSpool))
	Usage();

    /* Setup. */
    if (chdir(Dir) < 0) {
	(void)fprintf(stderr, "Cant chdir to %s, %s\n", Dir, strerror(errno));
	exit(1);
    }
    InSpoolDir = EQ(Dir, SPOOL);

    /* Do work. */
    if (ReadSpool)
	SpoolUpdate(AddEntries, Name);
    if (ac == 0)
	Expire(SortedInput, QIOfdopen(STDIN, QIO_BUFFER));
    else {
	for ( ; *av; av++)
	    if (EQ(*av, "-"))
		Expire(SortedInput, QIOfdopen(STDIN, QIO_BUFFER));
	    else if ((qp = QIOopen(*av, QIO_BUFFER)) == NULL)
		(void)fprintf(stderr, "Can't open %s, %s\n",
			*av, strerror(errno));
	    else
		Expire(SortedInput, qp);
    }

    exit(0);
    /* NOTREACHED */
}

