/*  $Revision: 1.13 $
**
**  Newsgroups and the active file.
*/
#include "nnrpd.h"
#include "mydir.h"


/*
**  Newsgroup hashing stuff.  See comments in innd/ng.c.
*/

#define GRP_HASH(Name, p, j)	\
	for (p = Name, j = 0; *p; ) j = (j << 5) + j + *p++
#define GRP_SIZE	512
#define GRP_BUCKET(j)	&GRPtable[j & (GRP_SIZE - 1)]

typedef struct _GRPHASH {
    int		Size;
    int		Used;
    GROUPENTRY	**Groups;
} GRPHASH;


STATIC GRPHASH		GRPtable[GRP_SIZE];
STATIC GROUPENTRY	*GRPentries;
STATIC int		GRPbuckets;
STATIC int		GRPsize;


/*
**  See if a given newsgroup exists.
*/
GROUPENTRY *
GRPfind(group)
    register char		*group;
{
    register char		*p;
    register unsigned int	j;
    register int		i;
    register GROUPENTRY		**gpp;
    GRPHASH			*htp;
    char			c;

    /* SUPPRESS 6 *//* Over/underflow from plus expression */
    GRP_HASH(group, p, j);
    htp = GRP_BUCKET(j);
    for (c = *group, gpp = htp->Groups, i = htp->Used; --i >= 0; gpp++)
	if (c == gpp[0]->Name[0] && EQ(group, gpp[0]->Name))
	    return gpp[0];
    return NULL;
}


STATIC void
GRPhash()
{
    register char		*p;
    register int		i;
    register GROUPENTRY		*gp;
    register unsigned int	j;
    register GRPHASH		*htp;

    /* Set up the default hash buckets. */
    GRPbuckets = GRPsize / GRP_SIZE;
    if (GRPbuckets == 0)
	GRPbuckets = 1;
    if (GRPtable[0].Groups)
	for (i = GRP_SIZE, htp = GRPtable; --i >= 0; htp++)
	    htp->Used = 0;
    else
	for (i = GRP_SIZE, htp = GRPtable; --i >= 0; htp++) {
	    htp->Size = GRPbuckets;
	    htp->Groups = NEW(GROUPENTRY*, htp->Size);
	    htp->Used = 0;
	}

    /* Now put all groups into the hash table. */
    for (i = GRPsize, gp = GRPentries; --i >= 0; gp++) {
	/* SUPPRESS 6 *//* Over/underflow from plus expression */
	GRP_HASH(gp->Name, p, j);
	htp = GRP_BUCKET(j);
	if (htp->Used >= htp->Size) {
	    htp->Size += GRPbuckets;
	    RENEW(htp->Groups, GROUPENTRY*, htp->Size);
	}
	htp->Groups[htp->Used++] = gp;
    }

    /* Note that we don't sort the buckets. */
}


/*
**  Read the active file into memory, sort it, and set the number of
**  newsgroups read in.  Return TRUE if okay, FALSE on error.
*/
BOOL
GetGroupList()
{
    static char			*active;
    register char		*p;
    register char		*q;
    register GROUPENTRY		*gp;
    register int		i;

    /* If re-scanning, free previous groups. */
    if (active != NULL) {
	DISPOSE(active);
	DISPOSE(GRPentries);
    }

    /* Get the new file. */
    active = ReadInFile(ACTIVE, (struct stat *)NULL);
    if (active == NULL) {
	syslog(L_ERROR, "%s cant read %s %m", ClientHost, ACTIVE);
	return FALSE;
    }

    /* Count lines. */
    for (p = active, i = 0; (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;

    /* Fill in the group array. */
    GRPentries = NEW(GROUPENTRY, i);
    for (i = 0, gp = GRPentries, p = active; *p; i++, gp++, p = q + 1) {
	gp->Name = p;
	if ((p = strchr(p, ' ')) == NULL) {
	    syslog(L_ERROR, "%s internal no_space1 \"%.20s...\"",
		ClientHost, gp->Name);
	    return FALSE;
	}
	*p++ = '\0';

	/* Get the high mark. */
	if ((q = strchr(p, ' ')) == NULL) {
	    syslog(L_ERROR, "%s internal no_space2 \"%.20s...\"",
		ClientHost, gp->Name);
	    return FALSE;
	}
	*q++ = '\0';
	gp->High = atol(p);

	/* Get the low mark. */
	if ((p = strchr(q, ' ')) == NULL) {
	    syslog(L_ERROR, "%s internal no_space3 \"%.20s...\"",
		ClientHost, gp->Name);
	    return FALSE;
	}
	*p++ = '\0';
	gp->Low = atol(q);

	/* Kill the newline. */
	if ((q = strchr(p, '\n')) == NULL) {
	    syslog(L_ERROR, "%s internal newline \"%.20s...\"",
		ClientHost, gp->Name);
	    return FALSE;
	}
	*q = '\0';
	gp->Flag = *p;
	gp->Alias = gp->Flag == NF_FLAG_ALIAS ? p + 1 : NULL;
    }

    GRPsize = i;
    GRPhash();
    return TRUE;
}


/*
**  Sorting predicate to put newsgroup names into numeric order.
*/
STATIC int
ARTcompare(p1, p2)
    POINTER	p1;
    POINTER	p2;
{
    ARTNUM	*i1;
    ARTNUM	*i2;

    i1 = CAST(ARTNUM*, p1);
    i2 = CAST(ARTNUM*, p2);
    return *i1 - *i2;
}


/*
**  Fill in ARTnumbers with the numbers of the articles in the current
**  group.
*/
STATIC void
GRPscandir(dir)
    char		*dir;
{
    static char		SPOOL[] = _PATH_SPOOL;
    static int		ARTarraysize;
    register DIRENTRY	*ep;
    register DIR	*dp;
    register char	*p;
    register ARTNUM	i;

    /* Go to the directory. */
    if (chdir(SPOOL) < 0) {
	syslog(L_FATAL, "%s cant cd %s %m", ClientHost, SPOOL);
	ExitWithStats(1);
    }

    if (ARTarraysize == 0) {
	ARTarraysize = 1024;
	ARTnumbers = NEW(ARTNUM, ARTarraysize);
    }

    /* The newsgroup directory might not exist; treat it as empty. */
    ARTsize = 0;
    GRPcount++;
    if (chdir(dir) < 0)
	return;
    dp = opendir(".");
    if (dp == NULL) {
	syslog(L_ERROR, "%s cant opendir %s %m", ClientHost, dir);
	return;
    }

    while ((ep = readdir(dp)) != NULL) {
	/* Get the numeric value of the filename, if it's all digits. */
	for (p = ep->d_name, i = 0; *p; p++) {
	    if (!CTYPE(isdigit, *p))
		break;
	    i = i * 10 + *p - '0';
	}
	if (*p || i == 0)
	    continue;

	if (ARTsize + 1 >= ARTarraysize) {
	    ARTarraysize += 1024;
	    RENEW(ARTnumbers, ARTNUM, ARTarraysize);
	}

	ARTnumbers[ARTsize++] = i;
    }
    (void)closedir(dp);

    ARTcache = NULL;
    qsort((POINTER)ARTnumbers, (SIZE_T)ARTsize, sizeof ARTnumbers[0],
	ARTcompare);
}


/*
**  Change to or list the specified newsgroup.  If invalid, stay in the old
**  group.
*/
FUNCTYPE
CMDgroup(ac, av)
    int			ac;
    char		*av[];
{
    static time_t	last_time;
    static char		NOSUCHGROUP[] = NNTP_NOSUCHGROUP;
    register char	*p;
    register int	i;
    time_t		now;
    char		*grplist[2];
    char		*group;
    char		buff[SPOOLNAMEBUFF];

    if (!PERMcanread) {
	Reply("%s\r\n", NOACCESS);
	return;
    }

    /* Parse arguments. */
    if (ac == 1) {
	if (GRPcount == 0) {
	    Printf("%d No group specified\r\n", NNTP_XGTITLE_BAD);
	    return;
	}
	(void)strcpy(buff, GRPlast);
	for (p = buff; *p; p++)
	    if (*p == '/')
		*p = '.';
	group = buff;
    }
    else
	group = av[1];
    if (GRPfind(group) == NULL) {
	Reply("%s\r\n", NOSUCHGROUP);
	return;
    }

    /* If permission is denied, pretend group doesn't exist. */
    if (PERMspecified) {
	grplist[0] = group;
	grplist[1] = NULL;
	if (!PERMmatch(PERMdefault, PERMlist, grplist)) {
	    Reply("%s\r\n", NOSUCHGROUP);
	    return;
	}
    }
    else if (!PERMdefault) {
	Reply("%s\r\n", NOSUCHGROUP);
	return;
    }

    /* Close out any existing article, report group stats. */
    ARTclose();
    ARTindex = 0;
    GRPreport();

    /* Make the group name a directory name. */
    (void)strcpy(buff, group);
    for (p = buff; *p; p++)
	if (*p == '.')
	    *p = '/';

    /* If we haven't been in the group recently, rescan. */
    (void)time(&now);
    if (!EQ(buff, GRPlast) || now > last_time + NNRP_RESCAN_DELAY) {
	GRPscandir(buff);
	(void)strcpy(GRPlast, buff);
	last_time = now;
    }

    /* Close down any overview file. */
    OVERclose();

    /* Doing a "group" command? */
    if (caseEQ(av[0], "group")) {
	if (ARTsize == 0)
	    Reply("%d 0 0 0 %s\r\n", NNTP_GROUPOK_VAL, group);
	else
	    Reply("%d %d %ld %ld %s\r\n",
		NNTP_GROUPOK_VAL,
		ARTsize, ARTnumbers[0], ARTnumbers[ARTsize - 1], group);
    }
    else {
	/* Must be doing a "listgroup" command. */
	Reply("%d Article list follows\r\n", NNTP_GROUPOK_VAL);
	for (i = 0; i < ARTsize; i++)
	    Printf("%ld\r\n", ARTnumbers[i]);
	Printf(".\r\n");
    }
}


/*
**  Report on the number of articles read in the group, and clear the count.
*/
void
GRPreport()
{
    register char	*p;
    char		buff[SPOOLNAMEBUFF];

    if (GRPlast[0] && GRParticles != 0) {
	(void)strcpy(buff, GRPlast);
	for (p = buff; *p; p++)
	    if (*p == '/')
		*p = '.';
	syslog(L_NOTICE, "%s group %s %ld", ClientHost, buff, GRParticles);
	GRParticles = 0;
    }
}


/*
**  Used by ANU-News clients.
*/
FUNCTYPE
CMDxgtitle(ac, av)
    int			ac;
    char		*av[];
{
    register QIOSTATE	*qp;
    register char	*line;
    register char	*p;
    register char	*q;
    char		save;

    /* Parse the arguments. */
    if (ac == 1) {
	if (GRPcount == 0) {
	    Printf("%d No group specified\r\n", NNTP_XGTITLE_BAD);
	    return;
	}
	p = GRPlast;
    }
    else
	p = av[1];

    /* Open the file, get ready to scan. */
    if ((qp = QIOopen(NEWSGROUPS, QIO_BUFFER)) == NULL) {
	syslog(L_ERROR, "%s cant open %s %m", ClientHost, NEWSGROUPS);
	Printf("%d Can't open %s\r\n", NNTP_XGTITLE_BAD, NEWSGROUPS);
	return;
    }
    Printf("%d list follows\r\n", NNTP_XGTITLE_OK);

    /* Print all lines with matching newsgroup name. */
    while ((line = QIOread(qp)) != NULL) {
	for (q = line; *q && !ISWHITE(*q); q++)
	    continue;
	save = *q;
	*q = '\0';
	if (wildmat(line, p)) {
	    *q = save;
	    Printf("%s\r\n", line);
	}
    }

    /* Done. */
    QIOclose(qp);
    Printf(".\r\n");
}


#if	defined(DO_DO_XTHREAD)
/*
**  XTHREAD command.  Based on code by Tim Iverson <iverson@xstor.com>,
**  Wayne Davison <davison@borland.com>, and Rob Robertson
**  <rob@violet.berkeley.edu>.  Usage:
**	xthread [thread]	Dump thread file for current group.
**	xthread dbinit		Dump db.init file.
**  This is a very ugly command -- data is raw binary.
*/
FUNCTYPE
CMDxthread(ac, av)
    int		ac;
    char	*av[];
{
    static char		NOTAVAIL[] = "%d %s not available.\r\n";
    static char		USAGE[] = "[dbinit|thread]";
    struct stat		Sb;
    register FILE	*F;
    register int	i;
    char		buff[BUFSIZ];
    char		*file;
#if	defined(THREAD_NAMES_FLAT)
    register char	*p;
    char		temp[SPOOLNAMEBUFF];
#endif	/* defined(THREAD_NAMES_FLAT) */

    if (!PERMcanread) {
	Reply("%s\r\n", NOACCESS);
	return;
    }

    /* Parse the arguments. */
    if (ac == 1 || (ac == 2 && caseEQ(av[1], "thread"))) {
	if (GRPcount == 0) {
	    Reply("%s\r\n", NNTP_NOTINGROUP);
	    return;
	}
#if	defined(THREAD_NAMES_FLAT)
	(void)strcpy(temp, GRPlast);
	for (p = temp; *p; p++)
	    if (*p == '/')
		*p = '.';
	(void)sprintf(buff, "%s/%s%s", THREAD_DIR, temp, THREAD_SUFFIX);
#else
	(void)sprintf(buff, "%s/%s%s", THREAD_DIR, GRPlast, THREAD_SUFFIX);
#endif	/* defined(THREAD_NAMES_FLAT) */
	file = buff;
    }
    else if (ac == 2 && caseEQ(av[1], "dbinit"))
	file = THREAD_DB;
    else {
	Reply("%d Usage: %s\r\n", NNTP_SYNTAX_VAL, USAGE);
	return;
    }

    /* Open the thread file, say what's coming. */
    if ((F = fopen(file, "r")) == NULL)  {
	syslog(L_ERROR, "%s cant fopen %s %m", ClientHost, file);
	Reply(NOTAVAIL, NNTP_TEMPERR_VAL, file);
	return;
    }

    /* Get file size. */
    if (fstat(fileno(F), &Sb) < 0) {
	syslog(L_ERROR, "%s cant fstat %s %m", ClientHost, file);
	Reply(NOTAVAIL, NNTP_TEMPERR_VAL, file);
	(void)fclose(F);
	return;
    }
    Reply("%d %ul binary bytes follow\r\n",
	THREAD_NNTP_CODE, (unsigned long)Sb.st_size);

    /* Send the data.  Ignore errors since there is no way to put
     * that info in the output stream -- symptomatic of binary
     * data formats. */
    while ((i = fread(buff, (SIZE_T)1, (SIZE_T)sizeof buff, F)) > 0)
	(void)fwrite(buff, (SIZE_T)i, (SIZE_T)1, stdout);
    (void)fclose(F);
    Printf("\r\n.\r\n");
}
#endif	/* defined(DO_DO_XTHREAD) */
