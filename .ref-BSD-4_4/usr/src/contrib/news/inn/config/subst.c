/*  $Revision: 1.12 $
**
**  A C version of Henry Spencer's "subst" script.
*/
#include <stdio.h>
#include <signal.h>
#include <errno.h>

#define LINESIZE		1024
#define FNAMESIZE		1024
#define PARAMSIZE		128
#define WHITE(c)		((c) == ' ' || (c) == '\t')


/*
**  AFS doesn't support hard links, so enable this #define.
#define USE_RENAME
*/

/*
**  If you don't have getopt in your C library, enable this #define.
#define NEED_GETOPT
*/


typedef struct _PAIR {
    char	*Name;
    int		Length;
    char	*Value;
} PAIR;

static char	*argv0;
extern char	*optarg;
extern int	optind;

extern void	exit();
extern char	*malloc();
extern char	*strcpy();



/*
**  Local implementations of common C library functions.
*/

/*
**  Return string represtation of errno.
*/
static char *
xstrerror()
{
    extern char *strerror();

    return strerror(errno);
}


/*
**  Return the first occurrence of 'c' in 'p' or NULL if not there.
*/
static char *
xstrchr(p, c)
    register char	*p;
    register char	c;
{
    extern char *strchr();

    return strchr(p, c);
}


/*
**  Return the last occurrence of 'c' in 'p' or NULL if not there.
*/
static char *
xstrrchr(p, c)
    register char	*p;
    register char	c;
{
    extern char *strrchr();

    return strrchr(p, c);
}


/*
**  Copy a string to malloc'd memory or exit.
*/
static char *
xstrdup(p)
    char	*p;
{
    char	*new;

    if ((new = malloc(strlen(p) + 1)) == NULL) {
	(void)fprintf(stderr, "%s: Can't copy \"%s\", %s\n",
		argv0, p, xstrerror());
	exit(1);
    }
    return strcpy(new, p);
}

#if	defined(NEED_GETOPT)

#define TYPE	int

#define ERR(s, c)					\
    if (opterr) {					\
	char buff[2];					\
	buff[0] = c; buff[1] = '\n';			\
	(void)write(2, av[0], (TYPE)strlen(av[0]));	\
	(void)write(2, s, (TYPE)strlen(s));		\
	(void)write(2, buff, 2);			\
    }

int	 opterr = 1;
int	 optind = 1;
int	 optopt;
char	*optarg;

/*
**  Return options and their values from the command line.
**  This comes from the AT&T public-domain getopt published in mod.sources
**  (i.e., comp.sources.unix before the great Usenet renaming).
*/
int
getopt(ac, av, opts)
    int		ac;
    char	*av[];
    char	*opts;
{
    static int	i = 1;
    char	*p;

    /* Move to next value from argv? */
    if (i == 1) {
	if (optind >= ac || av[optind][0] != '-' || av[optind][1] == '\0')
	    return EOF;
	if (strcmp(av[optind], "--") == 0) {
	    optind++;
	    return EOF;
	}
    }

    /* Get next option character. */
    if ((optopt = av[optind][i]) == ':' || (p = IDX(opts,  optopt)) == NULL) {
	ERR(": illegal option -- ", optopt);
	if (av[optind][++i] == '\0') {
	    optind++;
	    i = 1;
	}
	return '?';
    }

    /* Snarf argument? */
    if (*++p == ':') {
	if (av[optind][i + 1] != '\0')
	    optarg = &av[optind++][i + 1];
	else {
	    if (++optind >= ac) {
		ERR(": option requires an argument -- ", optopt);
		i = 1;
		return '?';
	    }
	    optarg = av[optind++];
	}
	i = 1;
    }
    else {
	if (av[optind][++i] == '\0') {
	    i = 1;
	    optind++;
	}
	optarg = NULL;
    }

    return optopt;
}
#endif	/* defined(NEED_GETOPT) */



/*
**  Simulate "mv $from $to" -- return no useful status.  We know that
**  the $from and $to are on the same filesystem.
*/
static void
mv(from, to)
    char	*from;
    char	*to;
{
    if (unlink(to) < 0 && errno != ENOENT) {
	(void)fprintf(stderr, "%s: Can't unlink %s, %s\n",
	    argv0, to, xstrerror());
	return;
    }
#if	defined(USE_RENAME)
    if (rename(from, to) < 0) {
	(void)fprintf(stderr, "%s: Can't rename %s to %s, %s\n",
		argv0, from, to, xstrerror());
	return;
    }
#else
    if (link(from, to) < 0) {
	(void)fprintf(stderr, "%s: Can't link %s to %s, %s\n",
		argv0, from, to, xstrerror());
	return;
    }
    if (unlink(from) < 0)
	(void)fprintf(stderr, "%s: Can't unlink %s, %s\n",
	    argv0, from, xstrerror());
#endif	/* defined(USE_RENAME) */
}


/*
**  Simulate "cmp -s $n1 $n2" -- return 0 if files are the same.
*/
static int
cmp(n1, n2)
    char	*n1;
    char	*n2;
{
    FILE	*f1;
    FILE	*f2;
    int		c;

    if ((f1 = fopen(n1, "r")) == NULL)
	return 1;
    if ((f2 = fopen(n2, "r")) == NULL) {
	(void)fclose(f1);
	return 1;
    }
    while ((c = getc(f1)) != EOF)
	if (getc(f2) != c) {
	    (void)fclose(f1);
	    (void)fclose(f2);
	    return 1;
	}
    if (getc(f2) != EOF) {
	(void)fclose(f1);
	(void)fclose(f2);
	return 1;
    }
    (void)fclose(f1);
    (void)fclose(f2);
    return 0;
}


/*
**  If line does not look like a template, return NULL, otherwise modify
**  it to delete the trailing gunk and return the start of the template.
*/
static char *
istemplate(line)
    char	*line;
{
    char	*p;
    char	*start;

    /* Find "=()<" and remember where it starts. */
    for (p = line; (p = xstrchr(p, '=')) != NULL; p++)
	if (p[1] == '(' && p[2] == ')' && p[3] == '<')
	    break;
    if (p == NULL)
	return NULL;
    start = &p[4];

    /* Now find ">()=" and nip it off. */
    for (p = start; (p = xstrchr(p, '>')) != NULL; p++)
	if (p[1] == '(' && p[2] == ')' && p[3] == '=') {
	    *p++ = '\n';
	    *p = '\0';
	    return start;
	}
    return NULL;
}


/*
**  Splice three strings together, returning an allocated copy.
*/
static char *
splice(s1, s2, s3)
    char	*s1;
    char	*s2;
    char	*s3;
{
    int		i;
    char	*new;

    i = strlen(s1) + strlen(s2) + strlen(s3) + 1;
    if ((new = malloc(i)) == NULL) {
	(void)fprintf(stderr, "%s: Can't splice %s+%s+%s, %s\n",
	    argv0, s1, s2, s3, xstrerror());
	exit(1);
    }
    (void)sprintf(new, "%s%s%s", s1, s2, s3);
    return new;
}


/*
**  Substitute all found patterns in the line and print it.  Using the goto
**  makes the code more clear than using do/while.
*/
static int
doline(f, out, line, tp, end)
    char	*f;
    FILE	*out;
    char	*line;
    PAIR	*tp;
    PAIR	*end;
{
    char	*p;
    char	*new;
    char	save;
    int		count;

    for (count = 0, line = xstrdup(line); tp < end; tp++) {
Again:
	for (p = line; (p = xstrchr(p, tp->Name[0])) != NULL; p++)
	    if (strncmp(p, tp->Name, tp->Length) == 0) {
		save = *p;
		*p = '\0';
		count++;
		new = splice(line, tp->Value, p + tp->Length);
		*p = save;
		if (strcmp(new, line) == 0) {
		    (void)fprintf(stderr, "%s:  subst loop in %s:\n\t%s\n",
			    argv0, f, line);
		    free(new);
		    break;
		}
		free(line);
		line = new;
		goto Again;
	    }
    }
    if (count > 0 && fputs(line, out) == EOF) {
	(void)fprintf(stderr, "%s:  can't write %s, %s\n",
		argv0, f, xstrerror());
	free(line);
	return -1;
    }
    free(line);
    return count;
}


/*
**  Process one file, carefully substituting it in place.
*/
static void
Process(f, Table, end)
    char	*f;
    PAIR	*Table;
    PAIR	*end;
{
    char	new[FNAMESIZE];
    char	old[FNAMESIZE];
    char	line[LINESIZE];
    int		bad;
    int		i;
    int		count;
    FILE	*in;
    FILE	*out;
    FILE	*temp;
    char	*p;

    /* First, figure out temporary names. */
    if ((p = xstrrchr(f, '/')) == NULL) {
	(void)strcpy(new, "substtmp.new");
	(void)strcpy(old, "substtmp.old");
    }
    else {
	*p = '\0';
	(void)sprintf(new, "%s/substtmp.new", f);
	(void)sprintf(old, "%s/substtmp.old", f);
	*p = '/';
    }

    /* Test existences. */
    if ((in = fopen(f, "r")) == NULL) {
	(void)fprintf(stderr, "%s: can't open %s, %s\n",
		argv0, f, xstrerror());
	return;
    }
    if ((temp = fopen(new, "r")) != NULL) {
	(void)fclose(in);
	(void)fprintf(stderr, "%s: %s exists, cannot proceed\n",
		argv0, new);
	exit(1);
    }
    if ((temp = fopen(old, "r")) != NULL) {
	(void)fprintf(stderr, "%s: %s exists, cannot proceed\n",
		argv0, old);
	exit(1);
    }
    temp = fopen(old, "w");
    out = fopen(new, "w");
    if (out == NULL || temp == NULL) {
	if (temp != NULL)
	    (void)fclose(temp);
	(void)unlink(old);
	if (out != NULL)
	    (void)fclose(out);
	(void)unlink(new);
	(void)fprintf(stderr, "%s: cannot create temporaries %s and %s\n",
		argv0, old, new);
	exit(1);
    }
    (void)fclose(temp);

    /* Generate the new version. */
    for (i = 1, bad = 0; fgets(line, sizeof line, in) != NULL; i++) {
	if ((p = xstrchr(line, '\n')) == NULL) {
	    (void)fprintf(stderr,
	      "%s: Line %d of %s is too long (or doesn't end with a newline)\n",
		    argv0, i, f);
	    bad++;
	    break;
	}
	(void)fputs(line, out);
	if ((p = istemplate(line)) != NULL) {
	    if ((count = doline(f, out, p, Table, end)) < 0) {
		bad++;
		break;
	    }
	    if (count > 0) {
		(void)fgets(line, sizeof line, in);
		i++;
	    }
	    else
		(void)fprintf(stderr,
			"%s: %s:%d unknown parameter or bad line:\n\t%s",
			argv0, f, i, p);
	}
    }
    (void)fclose(in);
    if (fflush(out) == EOF || fclose(out) == EOF) {
	(void)fprintf(stderr, "%s: can't close %s, %s\n",
		argv0, f, xstrerror());
	bad++;
    }

    if (bad || cmp(new, f) == 0) {
	(void)unlink(old);
	(void)unlink(new);
	(void)printf("%s: unchanged\n", f);
	return;
    }

    /* Substitute new for old the only safe way -- ignore signals. */
    (void)signal(SIGHUP, SIG_IGN);
    (void)signal(SIGINT, SIG_IGN);
    (void)signal(SIGTERM, SIG_IGN);
    mv(f, old);
    mv(new, f);
    (void)signal(SIGHUP, SIG_DFL);
    (void)signal(SIGINT, SIG_DFL);
    (void)signal(SIGTERM, SIG_DFL);
    (void)printf("%s: updated\n", f);
    (void)unlink(old);
}


/*
**  Print usage message and exit.
*/
static void
Usage()
{
    (void)fprintf(stderr, "Usage: %s -f file victims...\n", argv0);
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    static char	NIL[] = "";
    char	*ctlfile;
    char	*p;
    char	*dest;
    FILE	*F;
    int		i;
    char	buff[LINESIZE];
    char	name[PARAMSIZE];
    PAIR	*Table;
    PAIR	*tp;

    /* Set defaults. */
    ctlfile = NULL;
    argv0 = av[0];

    /* Parse JCL. */
    while ((i = getopt(ac, av, "f:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'f':
	    if (ctlfile != NULL)
		Usage();
	    ctlfile = optarg;
	    break;
	}
    ac -= optind;
    av += optind;

    /* Open control file, count lines, allocate table. */
    if ((F = fopen(ctlfile, "r")) == NULL) {
	(void)fprintf(stderr, "%s: Can't open %s to read it, %s\n",
		argv0, ctlfile, xstrerror());
	exit(1);
    }
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    if ((Table = (PAIR *)malloc(i * sizeof *Table)) == NULL) {
	(void)fprintf(stderr, "%s: Can't allocate %d table elements, %s\n",
		argv0, i, xstrerror());
	exit(1);
    }

    /* Now parse the table. */
    (void)fseek(F, 0L, 0);
    for (i = 1, tp = Table; fgets(buff, sizeof buff, F) != NULL; i++) {
	if ((p = xstrchr(buff, '\n')) == NULL) {
	    (void)fprintf(stderr, "%s: Line %d of %s is too long\n",
		    argv0, i, ctlfile);
	    exit(1);
	}
	*p = '\0';

	/* Skip empty lines, comment lines, and all-blank lines. */
	if (buff[0] == '\0' || buff[0] == '#')
	    continue;
	for (p = buff; WHITE(*p); p++)
	    continue;
	if (*p == '\0')
	    continue;

	/* Find end of first word, copy second word (or empty string) */
	for (p = buff; *p && !WHITE(*p); p++)
	    continue;
	if (*p == '\0')
	    tp->Value = NIL;
	else {
	    for (*p++ = '\0'; *p && WHITE(*p); p++)
		continue;
	    tp->Value = xstrdup(p);

	    /* Turn things like \& into &. */
	    for (p = dest = tp->Value; *p; p++)
		*dest++ = (*p == '\\' && p[1] != '\0') ? *++p : *p;
	    *dest = '\0';
	}

	/* Turn first word into something directly searchable. */
	if (strlen(buff) > sizeof name - 4) {
	    (void)fprintf(stderr, "%s: Parameter %s is too long\n",
		    argv0, buff);
	    exit(1);
	}
	(void)sprintf(name, "@<%s>@", buff);
	tp->Name = xstrdup(name);
	tp->Length = strlen(tp->Name);
	tp++;
    }
    (void)fclose(F);

    while (*av != NULL)
	Process(*av++, Table, tp);

    exit(0);
    /* NOTREACHED */
}
