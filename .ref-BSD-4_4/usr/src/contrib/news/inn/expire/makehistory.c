/*  $Revision: 1.14 $
**
**  Rebuild the history database.
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
#include "mydir.h"


typedef struct _BUFFER {
    char	*Data;
    int		Size;
    int		Used;
} BUFFER;


STATIC char		ACTIVE[] = _PATH_ACTIVE;
STATIC char		SPOOL[] = _PATH_SPOOL;
STATIC char		*HISTORYDIR;
STATIC char		HISTORY[] = _PATH_HISTORY;
STATIC char		MESSAGEID[] = "Message-ID:";
STATIC char		EXPIRES[] = "Expires:";
STATIC char		DATE[] = "Date:";
STATIC BOOL		INNDrunning;
STATIC char		*TextFile;
STATIC char		Reason[] = "makehistory is running";
STATIC TIMEINFO		Now;

extern long		dbztagmask();

/*
**  Change to a directory or exit out.
*/
STATIC void
xchdir(where)
    char	*where;
{
    if (chdir(where) < 0) {
	(void)fprintf(stderr, "Can't change to \"%s\", %s\n",
		where, strerror(errno));
	exit(1);
    }
}


/*
**  Remove the DBZ files for the specified base text file.
*/
STATIC void
RemoveDBZFiles(p)
    char	*p;
{
    static char	NOCANDO[] = "Can't remove \"%s\", %s\n";
    char	buff[SMBUF];

    (void)sprintf(buff, "%s.dir", p);
    if (unlink(buff) && errno != ENOENT)
	(void)fprintf(stderr, NOCANDO, buff, strerror(errno));
    (void)sprintf(buff, "%s.pag", p);
    if (unlink(buff) && errno != ENOENT)
	(void)fprintf(stderr, NOCANDO, buff, strerror(errno));
}


/*
**  Rebuild the DBZ file from the text file.
*/
STATIC void
Rebuild(size, IgnoreOld, Overwrite)
    long		size;
    BOOL		IgnoreOld;
    BOOL		Overwrite;
{
    register QIOSTATE	*qp;
    register char	*p;
    register char	*save;
    register long	count;
    long		where;
    datum		key;
    datum		value;
    char		temp[SMBUF];

    xchdir(HISTORYDIR);

    /* Open the text file. */
    qp = QIOopen(TextFile, QIO_BUFFER);
    if (qp == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\", %s\n",
		TextFile, strerror(errno));
	exit(1);
    }

    /* If using the standard history file, force DBZ to use history.n. */
    if (EQ(TextFile, HISTORY) && !Overwrite) {
	(void)sprintf(temp, "%s.n", HISTORY);
	if (link(HISTORY, temp) < 0) {
	    (void)fprintf(stderr, "Can't make temporary link to \"%s\", %s\n",
		    temp, strerror(errno));
	    exit(1);
	}
	RemoveDBZFiles(temp);
	p = temp;
    }
    else {
	temp[0] = '\0';
	RemoveDBZFiles(TextFile);
	p = TextFile;
    }

    /* Open the new database, using the old file if desired and possible. */
    (void)dbzincore(1);
    if (IgnoreOld) {
	if (dbzfresh(p, dbzsize(size), HIS_FIELDSEP, 'C', dbztagmask(size)) < 0) {
	    (void)fprintf(stderr, "Can't do dbzfresh, %s\n",
		    strerror(errno));
	    if (temp[0])
		(void)unlink(temp);
	    exit(1);
	}
    }
    else {
	if (dbzagain(p, HISTORY) < 0) {
	    (void)fprintf(stderr, "Can't do dbzagain, %s\n", strerror(errno));
	    if (temp[0])
		(void)unlink(temp);
	    exit(1);
	}
    }

    /* Set up the value pointer. */
    value.dptr = (char *)&where;
    value.dsize = sizeof where;

    /* Loop through all lines in the text file. */
    count = 0;
    for (where = QIOtell(qp); (p = QIOread(qp)) != NULL; where = QIOtell(qp)) {
	count++;
	if ((save = strchr(p, HIS_FIELDSEP)) == NULL) {
	    (void)fprintf(stderr, "Bad line #%ld \"%.30s...\"\n", count, p);
	    if (temp[0])
		(void)unlink(temp);
	    exit(1);
	}
	*save = '\0';
	key.dptr = p;
	key.dsize = save - p + 1;
	if (dbzstore(key, value) < 0) {
	    (void)fprintf(stderr, "Can't store \"%s\", %s\n",
		    p, strerror(errno));
	    if (temp[0])
		(void)unlink(temp);
	    exit(1);
	}
    }
    if (QIOerror(qp)) {
	(void)fprintf(stderr, "Can't read \"%s\" near line %ld, %s\n",
		TextFile, count, strerror(errno));
	if (temp[0])
	    (void)unlink(temp);
	exit(1);
    }
    if (QIOtoolong(qp)) {
	(void)fprintf(stderr, "Line %ld is too long\n", count);
	if (temp[0])
	    (void)unlink(temp);
	exit(1);
    }

    /* Close files. */
    QIOclose(qp);
    if (dbmclose() < 0) {
	(void)fprintf(stderr, "Can't close history, %s\n", strerror(errno));
	if (temp[0])
	    (void)unlink(temp);
	exit(1);
    }

    if (temp[0])
	(void)unlink(temp);
}


/*
**  Remove a bad article.
*/
STATIC void
Removeit(name)
    char	*name;
{
    char	*p;

    /* Already in the spool directory, so skip right past the name;
     * the strchr can't return NULL. */
    p = strchr(name, '/') + 1;
    if (unlink(p) < 0 && errno != ENOENT)
	(void)fprintf(stderr, "Can't unlink %s, %s\n",
		name, strerror(errno));
    else
	(void)fprintf(stderr, "Removing %s\n", name);
}


/*
**  Check and parse a Message-ID header line.  Return an allocated copy
**  or a static empty string on error.
*/
static char *
GetMessageID(p)
    register char	*p;
{
    static char		NIL[] = "";
    char		*save;

    while (ISWHITE(*p))
	p++;
    if (p[0] != '<' || p[strlen(p) - 1] != '>')
	return NIL;
    for (save = COPY(p), p = save; *p; p++)
	if (*p == HIS_FIELDSEP)
	    *p = HIS_BADCHAR;
    return save;
}


/*
**  Check and parse an date header line.  Return the new value or
**  zero on error.
*/
static long
GetaDate(p)
    register char	*p;
{
    time_t		t;

    while (ISWHITE(*p))
	p++;
    if ((t = parsedate(p, &Now)) == -1)
	return 0L;
    return (long)t;
}


/*
**  Process a single article.
*/
STATIC void
DoArticle(qp, Sbp, name, out, RemoveBad, Update)
    register QIOSTATE	*qp;
    struct stat		*Sbp;
    char		*name;
    FILE		*out;
    BOOL		RemoveBad;
    BOOL		Update;
{
    static char		IGNORE[] = "Ignoring duplicate %s header in %s\n";
    static char		BADHDR[] = "Bad %s header in %s\n";
    register char	*p;
    char		*MessageID;
    time_t		Arrived;
    time_t		Expires;
    time_t		Posted;
    int			i;
    datum		key;
    datum		value;

    /* Read the file for Message-ID and Expires header. */
    Arrived = Sbp->st_mtime;
    Expires = 0;
    MessageID = NULL;
    Posted = 0;
    while ((p = QIOread(qp)) != NULL && *p != '\0')
	switch (*p) {
	default:
	    break;
	case 'M': case 'm':
	    if (caseEQn(p, MESSAGEID, STRLEN(MESSAGEID))) {
		if (MessageID)
		    (void)fprintf(stderr, IGNORE, MESSAGEID, name);
		else {
		    MessageID = GetMessageID(&p[STRLEN(MESSAGEID)]);
		    if (*MessageID == '\0')
			(void)fprintf(stderr, BADHDR, MESSAGEID, name);
		}
	    }
	    break;
	case 'E': case 'e':
	    if (caseEQn(p, EXPIRES, STRLEN(EXPIRES))) {
		if (Expires > 0)
		    (void)fprintf(stderr, IGNORE, EXPIRES, name);
		else {
		    Expires = GetaDate(&p[STRLEN(EXPIRES)]);
		    if (Expires == 0)
			(void)fprintf(stderr, BADHDR, EXPIRES, name);
		}
	    }
	    break;
	case 'D': case 'd':
	    if (caseEQn(p, DATE, STRLEN(DATE))) {
		if (Posted > 0)
		    (void)fprintf(stderr, IGNORE, DATE, name);
		else {
		    Posted = GetaDate(&p[STRLEN(DATE)]);
		    if (Posted == 0)
			(void)fprintf(stderr, BADHDR, DATE, name);
		}
	    }
	    break;
	}

    /* Check for errors, close the input. */
    if (p == NULL) {
	if (QIOerror(qp)) {
	    (void)fprintf(stderr, "Can't read %s, %s\n",
		    name, strerror(errno));
	    return;
	}
	if (QIOtoolong(qp)) {
	    (void)fprintf(stderr, "Line too long in %s\n", name);
	    return;
	}
    }

    /* Make sure we have everything we need. */
    if (MessageID == NULL || *MessageID == '\0') {
	if (MessageID == NULL)
	    (void)fprintf(stderr, "No %s in %s\n", MESSAGEID, name);
	if (RemoveBad)
	    Removeit(name);
	return;
    }

    if (Update) {
	/* Server already know about this one? */
	key.dptr = MessageID;
	key.dsize = strlen(MessageID) + 1;
	value = dbzfetch(key);
	if (value.dptr != NULL)
	    return;
    }

    /* Output the line. */
    if (Posted == 0)
	Posted = Arrived;
    if (Expires > 0)
	i = fprintf(out, "%s%c%ld%c%ld%c%ld%c%s\n",
		MessageID, HIS_FIELDSEP,
		(long)Arrived, HIS_SUBFIELDSEP, (long)Expires,
		    HIS_SUBFIELDSEP, (long)Posted, HIS_FIELDSEP,
		name);
    else
	i = fprintf(out, "%s%c%ld%c%s%c%ld%c%s\n",
		MessageID, HIS_FIELDSEP,
		(long)Arrived, HIS_SUBFIELDSEP, HIS_NOEXP,
		    HIS_SUBFIELDSEP, (long)Posted, HIS_FIELDSEP,
		name);
    if (i == EOF || ferror(out)) {
	(void)fprintf(stderr, "Can't write history line, %s\n",
		strerror(errno));
	exit(1);
    }
    DISPOSE(MessageID);
}


/*
**  Process one newsgroup directory.
*/
STATIC void
DoNewsgroup(group, out, RemoveBad, Update)
    char		*group;
    FILE		*out;
    BOOL		RemoveBad;
    BOOL		Update;
{
    register DIR	*dp;
    register DIRENTRY	*ep;
    register QIOSTATE	*qp;
    register char	*p;
    register char	*q;
    struct stat		Sb;
    char		buff[SPOOLNAMEBUFF];
#if	defined(DO_HAVE_SYMLINK)
    char		linkbuff[SPOOLNAMEBUFF];
    int			oerrno;
#endif	/* defined(DO_HAVE_SYMLINK) */

    (void)strcpy(buff, group);
    for (p = group; *p; p++)
	if (*p == '.')
	    *p = '/';
    xchdir(SPOOL);
    if (chdir(group) < 0)
	return;

    if ((dp = opendir(".")) == NULL) {
	(void)fprintf(stderr, "Can't opendir %s, %s\n", group, strerror(errno));
	return;
    }

    q = &buff[strlen(buff)];
    *q++ = '/';

    /* Read all entries in the directory. */
    while ((ep = readdir(dp)) != NULL) {
	p = ep->d_name;
	if (!CTYPE(isdigit, *p) || strspn(p, "0123456789") != strlen(p))
	    continue;
	(void)strcpy(q, p);

	/* Is this a regular file? */
	if (stat(p, &Sb) < 0) {
	    (void)fprintf(stderr, "Can't stat %s, %s\n",
		    buff, strerror(errno));
#if	defined(DO_HAVE_SYMLINK)
	    /* Symlink to nowhere? */
	    oerrno = errno;
	    (void)memset((POINTER)linkbuff, '\0', sizeof linkbuff);
	    if (lstat(p, &Sb) >= 0
	     && readlink(p, linkbuff, sizeof linkbuff - 1) >= 0) {
		linkbuff[sizeof linkbuff - 1] = '\0';
		(void)fprintf(stderr, "Bad symlink %s -> %s, %s\n",
			buff, linkbuff, strerror(oerrno));
		if (RemoveBad)
		    Removeit(buff);
	    }
#endif	/* defined(DO_HAVE_SYMLINK) */
	    continue;
	}
	if (!S_ISREG(Sb.st_mode)) {
	    (void)fprintf(stderr, "%s is not a file\n", buff);
	    continue;
	}

	/* Open the article. */
	if ((qp = QIOopen(p, QIO_BUFFER)) == NULL) {
	    (void)fprintf(stderr, "Can't open %s, %s\n",
		    buff, strerror(errno));
	    continue;
	}
	DoArticle(qp, &Sb, buff, out, RemoveBad, Update);
	QIOclose(qp);
    }

    (void)closedir(dp);
}


/*
**  Tell innd to add a history line.
*/
STATIC BOOL
AddThis(line, Verbose)
    register char	*line;
    BOOL		Verbose;
{
    int			i;
    char		*arrive;
    char		*exp;
    char		*posted;
    char		*paths;
    char		*av[6];

    if ((arrive = strchr(line, HIS_FIELDSEP)) == NULL
     || (exp = strchr(arrive + 1, HIS_SUBFIELDSEP)) == NULL
     || (posted = strchr(exp + 1, HIS_SUBFIELDSEP)) == NULL
     || (paths = strchr(exp + 1, HIS_FIELDSEP)) == NULL) {
	(void)fprintf(stderr, "Got bad history line \"%s\"\n", line);
	return FALSE;
    }
    av[0] = line;
    *arrive = '\0';
    av[1] = arrive + 1;
    *exp = '\0';
    av[2] = exp + 1;
    *posted = '\0';
    av[3] = posted + 1;
    *paths = '\0';
    av[4] = paths + 1;
    av[5] = NULL;
    if (EQ(av[2], HIS_NOEXP))
        av[2] = "0";

    i = ICCcommand(SC_ADDHIST, av, (char **)NULL);
    *arrive = HIS_FIELDSEP;
    *exp = HIS_SUBFIELDSEP;
    *posted = HIS_SUBFIELDSEP;
    *paths = HIS_FIELDSEP;
    if (i < 0) {
        (void)fprintf(stderr, "Can't add history line \"%s\", %s\n",
               line, strerror(errno));
        return FALSE;
    }
    if (Verbose)
	(void)fprintf(stderr, "Added %s\n", line);
    return TRUE;
}



/*
**  Close the server link, and exit.
*/
STATIC NORETURN
ErrorExit(Updating, Stopped)
    BOOL	Updating;
    BOOL	Stopped;
{
    if (Updating) {
	if (!INNDrunning && Stopped && ICCgo(Reason) < 0)
	    (void)fprintf(stderr, "Can't restart server, %s\n",
		    strerror(errno));
	if (ICCclose() < 0)
	    (void)fprintf(stderr, "Can't close link, %s\n",
		    strerror(errno));
    }
    exit(1);
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr,
	    "Usage: makehistory [-f file] [-n] [-r] [-s size] [-u]\n");
    exit(1);
    /* NOTREACHED */
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register QIOSTATE	*qp;
    register FILE	*out;
    register char	*line;
    register char	*p;
    register char	*q;
    register long	count;
    BUFFER		B;
    long		size;
    int			i;
    BOOL		JustRebuild;
    BOOL		DoRebuild;
    BOOL		IgnoreOld;
    BOOL		Overwrite;
    BOOL		Update;
    BOOL		RemoveBad;
    BOOL		Verbose;
    char		temp[SMBUF];
    char		*TempTextFile;
    char		*tv[2];
    STRING		tmpdir;
    char		*Tflag;

    /* Set defaults. */
    TextFile = HISTORY;
    DoRebuild = TRUE;
    JustRebuild = FALSE;
    IgnoreOld = FALSE;
    Update = FALSE;
    RemoveBad = FALSE;
    Overwrite = FALSE;
    Verbose = FALSE;
    Tflag = "";
    size = 0;
    if ((tmpdir = getenv("TMPDIR")) == NULL)
	tmpdir = _PATH_TMP;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "bf:inors:T:uv")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'b':
	    RemoveBad = TRUE;
	    break;
	case 'f':
	    TextFile = optarg;
	    break;
	case 's':
	    size = atol(optarg);
	    /* FALLTHROUGH */
	case 'i':
	    IgnoreOld = TRUE;
	    break;
	case 'n':
	    DoRebuild = FALSE;
	    break;
	case 'o':
	    Overwrite = TRUE;
	    IgnoreOld = TRUE;
	    break;
	case 'r':
	    JustRebuild = TRUE;
	    break;
	case 'T':
	    tmpdir = optarg;
	    Tflag = NEW(char, 3 + strlen(optarg) + 1);
	    (void)sprintf(Tflag, "-T %s", optarg);
	    break;
	case 'u':
	    Update = TRUE;
	    break;
	case 'v':
	    Verbose = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac || (Overwrite && Update) || (Verbose && !Update))
	Usage();
    if ((p = strrchr(TextFile, '/')) == NULL)
	HISTORYDIR = _PATH_NEWSLIB;
    else {
	*p = '\0';
	HISTORYDIR = COPY(TextFile);
	*p = '/';
    }

    /* If we're not gonna scan the database, get out. */
    if (JustRebuild) {
	Rebuild(size, IgnoreOld, Overwrite);
	exit(0);
    }

    /* Get the time.  Only get it once, which is good enough. */
    if (GetTimeInfo(&Now) < 0) {
	(void)fprintf(stderr, "Can't get the time, %s\n", strerror(errno));
	exit(1);
    }

    /* Open history file. */
    xchdir(HISTORYDIR);

    if (Update || !Overwrite) {
	(void)sprintf(temp, "%s/histXXXXXX", tmpdir);
	(void)mktemp(temp);
	TempTextFile = COPY(temp);
    }
    else
	TempTextFile = NULL;
    if (Update) {
	if (ICCopen() < 0) {
	    (void)fprintf(stderr, "Can't talk to server, %s\n",
		    strerror(errno));
	    exit(1);
	}
	tv[0] = Reason;
	tv[1] = NULL;
	if (DoRebuild && ICCcommand(SC_THROTTLE, tv, (char **)NULL) < 0) {
	    (void)fprintf(stderr, "Can't throttle innd, %s\n",
		    strerror(errno));
	    exit(1);
	}
	if (dbminit(TextFile) == -1) {
	    (void)fprintf(stderr, "Can't open dbz file, %s\n",
		    strerror(errno));
	    ErrorExit(TRUE, DoRebuild);
	}
    }

    if ((out = fopen(TempTextFile ? TempTextFile : TextFile, "w")) == NULL) {
	(void)fprintf(stderr, "Can't write to history file, %s\n",
		strerror(errno));
	exit(1);
    }

    /* Start scanning the directories. */
    if ((qp = QIOopen(ACTIVE, QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't open %s, %s\n", ACTIVE, strerror(errno));
	exit(1);
    }
    for (count = 1; (line = QIOread(qp)) != NULL; count++) {
	if ((p = strchr(line, ' ')) == NULL) {
	    (void)fprintf(stderr, "Bad line %ld, \"%s\"\n", count, line);
	    continue;
	}
	*p = '\0';
	DoNewsgroup(line, out, RemoveBad, Update);
    }
    /* Test error conditions; QIOtoolong shouldn't happen. */
    if (QIOtoolong(qp)) {
	(void)fprintf(stderr, "Line %ld is too long\n", count);
	ErrorExit(Update, DoRebuild);
    }
    if (QIOerror(qp)) {
	(void)fprintf(stderr, "Can't read %s around line %ld, %s\n",
		ACTIVE, count, strerror(errno));
	ErrorExit(Update, DoRebuild);
    }
    if (fflush(out) == EOF || ferror(out) || fclose(out) == EOF) {
	(void)fprintf(stderr, "Can't close history file, %s\n",
		strerror(errno));
	ErrorExit(Update, DoRebuild);
    }

    /* Move. */
    xchdir(HISTORYDIR);

    if (Update) {
	INNDrunning = TRUE;
	if (dbmclose() < 0) {
	    (void)fprintf(stderr, "Can't close DBZ file, %s\n",
		    strerror(errno));
	    ErrorExit(Update, DoRebuild);
	}
	if (DoRebuild && ICCgo(Reason) < 0) {
	    (void)fprintf(stderr, "Can't restart innd, %s\n", strerror(errno));
	    ErrorExit(Update, DoRebuild);
	}
    }

    /* Make a temporary file, sort the text file into it. */
    (void)sprintf(temp, "%s/histXXXXXX", tmpdir);
    (void)mktemp(temp);
    i = 50 + strlen(TempTextFile ? TempTextFile : TextFile) + strlen(temp);
    p = NEW(char, i);
    (void)sprintf(p, "exec sort %s -t'%c' +1n -o %s %s",
	    Tflag, HIS_FIELDSEP, temp, TempTextFile ? TempTextFile : TextFile);

    i = system(p) >> 8;
    if (i != 0) {
	(void)fprintf(stderr, "Can't sort history file (exit %d), %s\n",
		i, strerror(errno));
	ErrorExit(Update, DoRebuild);
    }
    DISPOSE(p);

    if (TempTextFile) {
        if (unlink(TempTextFile) && errno != ENOENT)
	    (void)fprintf(stderr, "Can't remove \"%s\", %s\n",
		    TempTextFile, strerror(errno));
	DISPOSE(TempTextFile);
	TempTextFile = NULL;
    }

    /* Open the sorted file, get ready to write the final text file. */
    if ((qp = QIOopen(temp, QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't open work file \"%s\", %s\n",
		temp, strerror(errno));
	ErrorExit(Update, DoRebuild);
    }
    if (!Update && (out = fopen(TextFile, "w")) == NULL) {
	(void)fprintf(stderr, "Can't start writing %s, %s\n",
		TextFile, strerror(errno));
	(void)fprintf(stderr, "Work file %s untouched.\n", temp);
	ErrorExit(Update, DoRebuild);
    }

    /* Get space to keep the joined history lines. */
    B.Size = 100;
    B.Used = 0;
    B.Data = NEW(char, B.Size);
    p = COPY("");

    /* Read the sorted work file. */
    for (count = 0; (line = QIOread(qp)) != NULL; ) {
	count++;
	if ((q = strchr(line, HIS_FIELDSEP)) == NULL) {
	    (void)fprintf(stderr, "Work file line %ld had bad format\n",
		    count);
	    ErrorExit(Update, DoRebuild);
	}
	*q = '\0';
	if (EQ(p, line)) {
	    /* Same Message-ID as last time -- get filename */
	    if ((q = strchr(q + 1, HIS_FIELDSEP)) == NULL) {
		(void)fprintf(stderr, "Work file line %ld missing filename\n",
			count);
		ErrorExit(Update, DoRebuild);
	    }
	    i = strlen(q);
	    if (B.Size < B.Used + i + 3) {
		B.Size = B.Used + i + 3;
		B.Data = RENEW(B.Data, char, B.Size);
	    }
	    *q = ' ';
	    (void)strcpy(&B.Data[B.Used], q);
	    B.Used += i;
	}
	else {
	    /* Different Message-ID; end old line, start new one. */
	    if (*p) {
		if (!Update)
		    (void)fprintf(out, "%s\n", B.Data);
		else if (!AddThis(B.Data, Verbose))
		    ErrorExit(Update, DoRebuild);
	    }
	    DISPOSE(p);
	    p = COPY(line);

	    *q = HIS_FIELDSEP;
	    i = strlen(line);
	    if (B.Size < i) {
		B.Size = i + 2;
		B.Data = RENEW(B.Data, char, B.Size);
	    }
	    (void)strcpy(B.Data, line);
	    B.Used = i;
	}
	if (!Update && ferror(out)) {
	    (void)fprintf(stderr, "Can't write output from line %ld, %s\n",
		    count, strerror(errno));
	    ErrorExit(Update, DoRebuild);
	}
    }

    /* Check for errors and close. */
    if (QIOtoolong(qp)) {
	(void)fprintf(stderr, "Line %ld is too long\n", count);
	ErrorExit(Update, DoRebuild);
    }
    if (QIOerror(qp)) {
	(void)fprintf(stderr, "Can't read work file, %s\n", strerror(errno));
	ErrorExit(Update, DoRebuild);
    }
    QIOclose(qp);
    if (unlink(temp) && errno != ENOENT)
	(void)fprintf(stderr, "Can't remove \"%s\", %s\n",
		temp, strerror(errno));

    if (*p) {
	/* Add tail end of last line. */
	if (!Update)
	    (void)fprintf(out, "%s\n", B.Data);
	else if (!AddThis(B.Data, Verbose))
	    ErrorExit(Update, DoRebuild);
    }

    /* Close the output file. */
    if (!Update) {
        if (fflush(out) == EOF || fclose(out) == EOF) {
	    (void)fprintf(stderr, "Can't close history file, %s\n",
		    strerror(errno));
	    ErrorExit(Update, DoRebuild);
        }
	if (DoRebuild)
	    Rebuild(size ? size : count, TRUE, Overwrite);
    }
    else if (ICCclose() < 0)
	(void)fprintf(stderr, "Can't close link, %s\n", strerror(errno));

    exit(0);
    /* NOTREACHED */
}
