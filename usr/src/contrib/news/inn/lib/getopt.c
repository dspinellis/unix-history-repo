/*  $Revision: 1.1 $
**
*/
#include <stdio.h>

#define ERR(s, c)					\
    if (opterr) {					\
	char buff[2];					\
	buff[0] = c; buff[1] = '\n';			\
	(void)write(2, av[0], strlen(av[0]));		\
	(void)write(2, s, strlen(s));			\
	(void)write(2, buff, 2);			\
    }


int	opterr = 1;
int	optind = 1;
int	optopt;
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
    extern char	*strchr();
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
    if ((optopt = av[optind][i]) == ':'
     || (p = strchr(opts,  optopt)) == NULL) {
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
