/*  $Revision: 1.13 $
**
**  Connect to the NNTP server and feed one article.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "nntp.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


STATIC FILE	*FromServer;
STATIC FILE	*ToServer;
STATIC int	Tracing;


/*
**  Print and error message (with errno) and exit with an error code.
*/
STATIC NORETURN
PerrorExit(s)
    char	*s;
{
    (void)fprintf(stderr, "%s, %s.\n", s, strerror(errno));
    exit(1);
}


/*
**  Read a line from the server or die trying.
*/
STATIC NORETURN
GetFromServer(buff, size, text)
    char	*buff;
    int		size;
    char	*text;
{
    if (fgets(buff, size, FromServer) == NULL)
	PerrorExit(text);
    if (Tracing)
	printf("S: %s", buff);
}


/*
**  Flush a stdio FILE; exit if there are any errors.
*/
STATIC void
SafeFlush(F)
    FILE	*F;
{
    if (fflush(F) == EOF || ferror(F))
	PerrorExit("Can't send text to server");
}


STATIC NORETURN
SendQuit(x)
    int		x;
{
    char	buff[BUFSIZ];

    /* Close up. */
    (void)fprintf(ToServer, "quit\r\n");
    SafeFlush(ToServer);
    (void)fclose(ToServer);
    GetFromServer(buff, sizeof buff, "Can't get reply to quit");
    exit(x);
}


STATIC NORETURN
Usage()
{
    (void)fprintf(stderr,
	    "Usage: feedone [-r|-m msgid] [-p] [-t] articlefile\n");
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    static char	MESGIDHDR[] = "Message-ID:";
    int		i;
    FILE	*F;
    char	buff[BUFSIZ];
    char	mesgid[SMBUF];
    char	*p;
    char	*q;
    BOOL	PostMode;

    /* Set defaults. */
    mesgid[0] = '\0';
    PostMode = FALSE;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "m:prt")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'm':			/* Specified Message-ID */
	    if (*optarg == '<')
		(void)strcpy(mesgid, optarg);
	    else
		(void)sprintf(mesgid, "<%s>", optarg);
	    break;
	case 'p':			/* Use Post, not ihave	*/
	    PostMode = TRUE;
	    break;
	case 'r':			/* Random Message-ID	*/
	    (void)sprintf(mesgid, "<%d@%ld>",
		    getpid(), (long)time((time_t *)NULL));
	    break;
	case 't':
	    Tracing = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;

    /* One argument; the input filename. */
    if (ac != 1)
	Usage();
    if ((F = fopen(av[0], "r")) == NULL)
	PerrorExit("Can't open input");

    /* Scan for the message-id. */
    if (mesgid[0] == '\0') {
	while (fgets(buff, sizeof buff, F) != NULL)
	    if (caseEQn(buff, MESGIDHDR, STRLEN(MESGIDHDR))) {
		if ((p = strchr(buff, '<')) == NULL
		 || (q = strchr(p, '>')) == NULL) {
		    (void)fprintf(stderr, "Bad mesgid line.\n");
		    exit(1);
		}
		q[1] = '\0';
		(void)strcpy(mesgid, p);
		break;
	    }
	if (mesgid[0] == '\0') {
	    (void)fprintf(stderr, "No Message-ID.\n");
	    exit(1);
	}
    }

    /* Connect to the server. */
    if (NNTPremoteopen(&FromServer, &ToServer, buff) < 0
     || FromServer == NULL
     || ToServer == NULL) {
	if (buff[0])
	    (void)fprintf(stderr, "Server says: %s\n", buff);
	PerrorExit("Can't connect to server");
    }

    /* Does the server want this article? */
    if (PostMode) {
	(void)fprintf(ToServer, "post\r\n");
	i = NNTP_START_POST_VAL;
    }
    else {
	(void)fprintf(ToServer, "ihave %s\r\n", mesgid);
	i = NNTP_SENDIT_VAL;
    }
    SafeFlush(ToServer);
    GetFromServer(buff, sizeof buff, "Can't offer article to server");
    if (atoi(buff) != i) {
	(void)fprintf(stderr, "Server doesn't want the article:\n\t%s\n",
		buff);
	SendQuit(1);
    }

    /* Send the file over. */
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    while (fgets(buff, sizeof buff, F) != NULL) {
	if (caseEQn(buff, MESGIDHDR, STRLEN(MESGIDHDR))) {
	    (void)fprintf(ToServer, "%s %s\r\n", MESGIDHDR, mesgid);
	    continue;
	}
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	(void)fprintf(ToServer, buff[0] == '.' ? ".%s\r\n" : "%s\r\n",
		buff);
	SafeFlush(ToServer);
    }
    (void)fprintf(ToServer, ".\r\n");
    SafeFlush(ToServer);
    (void)fclose(F);

    /* How did the server respond? */
    GetFromServer(buff, sizeof buff,
	"No reply from server after sending the article");
    i = PostMode ? NNTP_POSTEDOK_VAL : NNTP_TOOKIT_VAL;
    if (atoi(buff) != i) {
	(void)fprintf(stderr, "Can't send article to the server:\n\t%s\n",
		buff);
	exit(1);
    }

    SendQuit(0);
    /* NOTREACHED */
}
