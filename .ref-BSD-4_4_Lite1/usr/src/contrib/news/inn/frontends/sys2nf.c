/*  $Revision: 1.3 $
**
**  Read a C news "sys" file and split it up into a set of INN
**  newsfeeds entries.  Also works with B news.
**
**  Once done, edit all files that have HELP or all in them.
**  Review all files, anyway.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "nntp.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"

#define TEMPFILE	":tmp"
static char		**Groups;


/*
**  Fill in the Groups array with the names of all active newsgroups.
*/
static void
ReadActive(act)
    char	*act;
{
    FILE	*F;
    int		i;
    char	buff[BUFSIZ];
    char	*p;

    /* Open file, count lines. */
    if ((F = fopen(act, "r")) == NULL) {
	perror(act);
	exit(1);
    }
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    Groups = NEW(char*, i + 2);

    /* Fill in each word. */
    rewind(F);
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++) {
	if ((p = strchr(buff, ' ')) != NULL)
	    *p = '\0';
	Groups[i] = COPY(buff);
    }
    Groups[i] = NULL;
    (void)fclose(F);
}


/*
**  Read in the sys file and turn it into an array of strings, one
**  per continued line.
*/
char **
ReadSys(sys)
    char		*sys;
{
    register char	*p;
    register char	*to;
    register char	*site;
    register int	i;
    char		*data;
    char		**strings;

    /* Read in the file, get rough count. */
    if ((data = ReadInFile(sys, (struct stat *)NULL)) == NULL) {
	perror(sys);
	exit(1);
    }
    for (p = data, i = 0; (p = strchr(p, '\n')) != NULL; p++, i++)
	continue;

    /* Scan the file, glue all multi-line entries. */
    for (strings = NEW(char*, i + 1), i = 0, to = p = data; *p; ) {
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
	if (*site == '\0')
	    continue;
	strings[i++] = COPY(site);
    }
    strings[i] = NULL;
    DISPOSE(data);
    return strings;
}


/*
**  Is this the name of a top-level group?  We want a simple name, "foo",
**  and should find a "foo." in the group list.
*/
static BOOL
Toplevel(p)
    char	*p;
{
    char	**gp;
    char	*g;
    int		i;

    if (strchr(p, '.') != NULL)
	return FALSE;
    for (i = strlen(p) - 1, gp = Groups; (g = *gp++) != NULL; )
	if (EQn(p, g, i) && g[i + 1] == '.')
	    return TRUE;
    return FALSE;
}


/*
**  Do we have a name that's a prefix for more then one newsgroup?
**  For "foo.bar", we must find more then one "foo.bar" or "foo.bar."
*/
static BOOL
GroupPrefix(p)
    char	*p;
{
    char	**gp;
    char	*g;
    int		count;
    int		i;

    if (strchr(p, '.') == NULL)
	return FALSE;
    for (i = strlen(p), count = 0, gp = Groups; (g = *gp++) != NULL; )
	if (EQ(p, g) || (EQn(p, g, i) && g[i] == '.'))
	    count++;
    return count > 1;
}


/*
**  Step through the old subscription list, try to update each one in
**  turn.
*/
static void
DoSub(F, p)
    register FILE	*F;
    char		*p;
{
    register char	*s;
    register int	len;
    register BOOL	SawBang;
    register BOOL	SawAll;

    if ((s = strtok(p, ",")) == NULL)
	return;

    (void)fprintf(F, "!*");
    len = 8 + 1 + 2;
    do {
	/* These are distributions, not newsgroups. */
	if (EQ(s, "world") || EQ(s, "na") || EQ(s, "usa") || EQ(s, "inet")
	 || EQ(s, "mod") || EQ(s, "net")
	 || EQ(s, "local")
	)
	    continue;

#if	defined(DO_MERGE_TO_GROUPS)
	if (EQ(s, "!to") || EQn(s, "to.", 3))
	    continue;
#endif	/* defined(DO_MERGE_TO_GROUPS) */

	(void)putc(',', F);
	len++;

	if (len + strlen(s) + 3 > 72) {
	    (void)fprintf(F,"\\\n\t    ");
	    len = 12;
	}

	SawBang = *s == '!';
	if (SawBang) {
	    (void)putc('!', F);
	    len++;
	    s++;
	}

	SawAll = EQ(s, "all");
	if (SawAll)
	    s = SawBang ? "*" : "*,!control";
	len += strlen(s);
	(void)fprintf(F, "%s", s);

	if (SawAll)
	    ;
	else if (
	    EQ(s, "comp") || EQ(s, "misc") || EQ(s, "news") || EQ(s, "rec")
	 || EQ(s, "sci") || EQ(s, "soc") || EQ(s, "talk")

	 || EQ(s, "alt") || EQ(s, "bionet") || EQ(s, "bit") || EQ(s, "biz")
	 || EQ(s, "clari") || EQ(s, "ddn") || EQ(s, "gnu") || EQ(s, "ieee")
	 || EQ(s, "k12") || EQ(s, "pubnet") || EQ(s, "trial") || EQ(s, "u3b")
	 || EQ(s, "vmsnet")

	 || EQ(s, "ba") || EQ(s, "ca") || EQ(s, "dc") || EQ(s, "ne")
	 || EQ(s, "ny") || EQ(s, "tx")

	 || EQ(s, "info") || EQ(s, "mail") || EQ(s, "opinions")
	 || EQ(s, "uunet")

	 || Toplevel(s)) {
	    (void)fprintf(F, ".*");
	    len += 2;
	}
	else if (GroupPrefix(s)) {
	    (void)putc('*', F);
	    len++;
	}
    } while ((s = strtok((char *)NULL, ",")) != NULL);
}


int
main(ac, av)
    int		 ac;
    char	*av[];
{
    FILE	*F;
    FILE	*out;
    char	**sites;
    char	*f2;
    char	*f3;
    char	*f4;
    char	*p;
    char	*q;
    char	*site;
    char	buff[256];
    char	*act;
    char	*dir;
    char	*sys;
    int		i;

    /* Set defaults. */
    act = "/usr/local/lib/newslib/active";
    sys = "sys";
    dir = "feeds";
    while ((i = getopt(ac, av, "a:s:d:")) != EOF)
    switch (i) {
    default:
	exit(1);
	/* NOTREACHED */
    case 'a':	act = optarg;	break;
    case 'd':	dir = optarg;	break;
    case 's':	sys = optarg;	break;
    }

    sites = ReadSys(sys);
    ReadActive(act);
    if (mkdir(dir, 0777) < 0 && errno != EEXIST)
	perror(dir), exit(1);
    if (chdir(dir) < 0)
	perror("chdir"), exit(1);
    for ( ; ; ) {
	/* Get next non-comment ilne. */
	if ((p = *sites++) == NULL)
	    break;
	for (F = fopen(TEMPFILE, "w"); p && *p == '#'; p = *sites++)
	    (void)fprintf(F, "%s\n", p);
	if (p == NULL) {
	    (void)fclose(F);
	    break;
	}
	site = COPY(p);
	if ((f2 = strchr(site, ':')) == NULL)
	    f2 = "HELP";
	else
	    *f2++ = '\0';
	if ((f3 = strchr(f2, ':')) == NULL)
	    f3 = "HELP";
	else
	    *f3++ = '\0';
	if ((f4 = strchr(f3, ':')) == NULL)
	    f4 = "HELP";
	else
	    *f4++ = '\0';

	/* Write the fields. */
	(void)fprintf(F, "%s\\\n", site);
	(void)fprintf(F, "\t:");
	DoSub(F, f2);
	(void)fprintf(F, "\\\n");
	if (EQ(f3, "n"))
	    (void)fprintf(F, "\t:Tf,Wnm\\\n", f3);
	else
	    (void)fprintf(F, "\t:HELP%s\\\n", f3);
	(void)fprintf(F, "\t:%s\n", f4);
	if (ferror(F) || fclose(F) == EOF)
	    perror(TEMPFILE), exit(1);

	DISPOSE(site);

	/* Find the sitename. */
	for (q = p; *q && *q != '/' && *q != ':'; q++)
	    continue;
	*q = '\0';

	/* Append temp file to site file. */
	if ((F = fopen(TEMPFILE, "r")) == NULL)
	    perror(TEMPFILE), exit(1);
	if ((out = xfopena(p)) == NULL)
	    perror(p), exit(1);
	while ((i = fread(buff, 1, sizeof buff, F)) > 0)
	    if (fwrite(buff, 1, i, out) != i)
		perror(p), exit(1);
	(void)fclose(F);
	if (fclose(out) == EOF)
	    perror(p), exit(1);

	if (unlink(TEMPFILE) < 0)
	    perror("can't unlink temp file");
    }

    exit(0);
    /* NOTREACHED */
}
