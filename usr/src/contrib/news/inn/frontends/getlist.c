/*  $Revision: 1.4 $
**
**  Get a file list from an NNTP server.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "clibrary.h"
#include "libinn.h"
#include "qio.h"
#include "paths.h"
#include "macros.h"


/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: getlist [-h host] [type [pat [groups]]\n");
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    FILE	*active;
    FILE	*FromServer;
    FILE	*ToServer;
    QIOSTATE	*qp;
    char	*field4;
    char	*types;
    char	*host;
    char	*line;
    char	*list;
    char	*p;
    char	*pattern;
    char	buff[512 + 1];
    int		i;

    /* Set defaults. */
    host = NULL;
    pattern = NULL;
    types = NULL;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "h:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'h':
	    host = optarg;
	    break;
	}
    ac -= optind;
    av += optind;

    /* Parse parameters. */
    switch (ac) {
    default:
	Usage();
	/* NOTREACHED */
    case 0:
    case 1:
	break;
    case 2:
	pattern = av[1];
	break;
    case 3:
	pattern = av[1];
	types = av[2];
	break;
    }
    if (av[0] == NULL)
	list = "active";
    else {
	list = av[0];
	if (EQ(list, "active") && (pattern != NULL || types != NULL))
	    Usage();
    }

    /* Open a connection to the server. */
    if (host == NULL
     && (host = GetConfigValue(_CONF_SERVER)) == NULL) {
	(void)fprintf(stderr, "Can't get server name, %s\n", strerror(errno));
	exit(1);
    }
    buff[0] = '\0';
    if (NNTPconnect(host, &FromServer, &ToServer, buff) < 0) {
	(void)fprintf(stderr, "Can't connect to server, %s\n",
		buff[0] ? buff : strerror(errno));
	exit(1);
    }

    /* Get the data from the server. */
    active = CAlistopen(FromServer, ToServer, EQ(list, "active") ? NULL : list);
    if (active == NULL) {
	(void)fprintf(stderr, "Can't retrieve data, %s\n", strerror(errno));
	(void)fclose(FromServer);
	(void)fclose(ToServer);
	exit(1);
    }

    /* Set up to read it quickly. */
    if ((qp = QIOfdopen((int)fileno(active), QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't read temp file, %s\n", strerror(errno));
	(void)fclose(FromServer);
	(void)fclose(ToServer);
	exit(1);
    }

    /* Scan server's output, displaying appropriate lines. */
    i = 1;
    while ((line = QIOread(qp)) != NULL) {
	i++;

	/* No pattern means print all. */
	if (pattern == NULL) {
	    (void)printf("%s\n", line);
	    continue;
	}

	/* Get the group name, see if it's one we want. */
	if ((p = strchr(line, ' ')) == NULL) {
	    (void)fprintf(stderr, "Line %d is malformed\n", i);
	    continue;
	}
	*p = '\0';
	if (!wildmat(line, pattern))
	    continue;
	*p = ' ';

	/* If no group types, we want them all. */
	if (types == NULL) {
	    (void)printf("%s\n", line);
	    continue;
	}

	/* Find the fourth field. */
	if ((p = strchr(p + 1, ' ')) == NULL) {
	    (void)fprintf(stderr, "Line %d (field 2) is malformed.\n", i);
	    continue;
	}
	if ((p = strchr(p + 1, ' ')) == NULL) {
	    (void)fprintf(stderr, "Line %d (field 3) is malformed.\n", i);
	    continue;
	}
	field4 = p + 1;
	if ((p = strchr(field4, ' ')) != NULL) {
	    (void)fprintf(stderr, "Line %d has more than 4 fields\n", i);
	    continue;
	}

	/* Is this the type of line we want? */
	if (strchr(types, field4[0]) != NULL)
	    (void)printf("%s\n", line);
    }

    /* Determine why we stopped */
    if (QIOerror(qp)) {
	(void)fprintf(stderr, "Can't read temp file at line %d, %s\n",
	    i, strerror(errno));
	i = 1;
    }
    else if (QIOtoolong(qp)) {
	(void)fprintf(stderr, "Line %d is too long\n", i);
	i = i;
    }
    else
	i = 0;

    /* All done. */
    CAclose();
    (void)fprintf(ToServer, "quit\r\n");
    (void)fclose(ToServer);
    (void)fgets(buff, sizeof buff, FromServer);
    (void)fclose(FromServer);
    exit(i);
    /* NOTREACHED */
}
