/*
 * nntpxfer
 *
 * Connects to the specified nntp server, and transfers all new news
 * since the last successful invocation.
 *
 * last successful invocation date and time are stored in a file at
 * /usr/spool/news/nntp.<hostname> as 
 *	groups YYMMDD HHMMSS distributions\n
 * in case you need to edit it.  You can also override this on 
 * the command line in the same format, in which case the file won't
 * be updated.
 *
 *	Brian Kantor, UCSD 1986
 * (some bug fixes by ambar@athena.mit.edu)
 */

#define DEBUG

/* you'd think that 4096 articles at one go is enough.... */
#define MAXARTS	4096

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>

#include <net/if.h>
#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>
#include <signal.h>
#include <dbm.h>

#define INEWS	"/usr/lib/news/inews -p"
#define HIST	"/usr/lib/news/history"

char	*malloc();
char	*strcpy();
char	*strcat();
long	time();
u_long	inet_addr();

extern int errno;
char *artlist[MAXARTS];
int server;			/* stream socket to the nntp server */
int newart, dupart, misart;

main(argc, argv)
int argc;
char *argv[];
	{
	FILE *dtfile;		/* where last xfer date/time stored */
	char buf[BUFSIZ];
	char lastdate[16];
	char distributions[BUFSIZ];
	char dtname[128];
	char newsgroups[BUFSIZ];
	char lasttime[16];
	int connected = 0;		/* 1 = connected */
	int i;
	int omitupdate = 0;		/* 1 = don't update datetime */
	long clock;
	long newdate, newtime;
	struct hostent *hp;
	struct servent *sp;
	struct sockaddr_in sin;
	struct tm *now;

	/* OPTIONS
		argv[1] MUST be the host name
		argv[2-4] MAY be "newsgroups YYMMDD HHMMSS"
			argv[5] MAY be distributions
		(otherwise use 2-4/5 from the file
		"/usr/spool/news/nntp.hostname")
	*/

	if (argc != 2 && argc != 5 && argc != 6)
		{
		(void) printf("Usage: %s host [groups YYMMDD HHMMSS [<dist>]]\n",
			argv[0]);
		exit(1);
		}
	
	if (argc > 2)
		{
		omitupdate++;
		(void) strcpy(newsgroups, argv[2]);
		(void) strcpy(lastdate, argv[3]);
		(void) strcpy(lasttime, argv[4]);
		(void) strcpy(distributions, "");
		if (argc > 5)
			(void) strcpy(distributions, argv[5]);
		}
	else
		{
		(void) strcpy(dtname, "/usr/spool/news/nntp.");
		(void) strcat(dtname, argv[1]);
		dtfile = fopen(dtname, "r");
		if (dtfile == NULL)
			{
			(void) printf("%s not found; using * 860101 000000 \n", 
				dtname);
			(void) strcpy(newsgroups, "*");
			(void) strcpy(lastdate, "860101");
			(void) strcpy(lasttime, "000000");
			(void) strcpy(distributions, "");
			}
		else
			{
			if (fscanf(dtfile, "%s %s %s %s",
				newsgroups, lastdate, lasttime, distributions) < 3)
				{
				(void) printf("%s invalid; using * 860101 000000\n",
					dtname);
				(void) strcpy(newsgroups, "*");
				(void) strcpy(lastdate, "860101");
				(void) strcpy(lasttime, "000000");
				(void) strcpy(distributions, "");
				}
			(void) fclose(dtfile);
			}
		clock = time((long *)0);
		now = gmtime(&clock);
		newdate = (now->tm_year * 10000) +
			((now->tm_mon + 1) * 100) + now->tm_mday;
		newtime = (now->tm_hour * 10000) +
			(now->tm_min * 100) + now->tm_sec;
		}

#ifdef DEBUG
	(void) printf("newsgroups = '%s'\n", newsgroups);
	(void) printf("date = '%s'\n", lastdate);
	(void) printf("time = '%s'\n", lasttime);
	(void) printf("distributions = '%s'\n", distributions);
	(void) printf("now is = %06d %06d\n", newdate, newtime);
#endif

	if (dbminit(HIST) < 0)
		{
		perror("couldn't open history file");
		exit(1);
		}

	sin.sin_addr.s_addr = inet_addr(argv[1]);
	if (sin.sin_addr.s_addr != -1) 
		{
		sin.sin_family = AF_INET;
		}
	else 
		{
		hp = gethostbyname(argv[1]);
		if (hp == NULL) 
			{
			(void) printf("%s: unknown host\n", argv[1]);
			exit(1);
			}

		sin.sin_family = hp->h_addrtype;
#ifdef	BSD43
		bcopy(hp->h_addr_list[0], (caddr_t)&sin.sin_addr,
			hp->h_length);
#else	BSD43
		bcopy(hp->h_addr, (caddr_t)&sin.sin_addr,
			hp->h_length);
#endif	BSD43
		}
	
	sp = getservbyname("nntp", "tcp");
	if (sp == NULL)
		{
		perror("nntp/tcp");
		exit(1);
		}

	sin.sin_port = sp->s_port;

	do	{
		server = socket(AF_INET, SOCK_STREAM, 0);
		if (server < 0) 
			{
			perror("nntpxfer: socket");
			exit(1);
			}

		if (connect(server, (struct sockaddr *)&sin, sizeof (sin)) < 0) 
			{
#ifdef	BSD43
			if (hp && hp->h_addr_list[1]) 
				{
				hp->h_addr_list++;
				bcopy(hp->h_addr_list[0],
				    (caddr_t)&sin.sin_addr, hp->h_length);
				(void) close(server);
				continue;
				}
#endif	BSD43
			perror("nntpxfer: connect");
			exit(1);
			}
		connected++;
		}
	while (connected == 0);

#ifdef DEBUG
	(void) printf("connected to nntp server at %s\n", argv[1]);
#endif
	/*
	* ok, at this point we're connected to the nntp daemon 
	* at the distant host.
	*/

	/* get the greeting herald */
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
		(void) printf("protocol error: got '%s'\n", buf);
		(void) close(server);
		exit(1);
		}


	/* first, tell them we're a slave process to get priority */
	sockwrite("SLAVE");
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
		(void) printf("protocol error: got '%s'\n", buf);
		(void) close(server);
		exit(1);
		}
	
	/* now, ask for a list of new articles */
	if (strlen(distributions))
		(void) sprintf(buf,"NEWNEWS %s %s %s GMT <%s>", 
			newsgroups, lastdate, lasttime, distributions);
	else
		(void) sprintf(buf,"NEWNEWS %s %s %s GMT", 
			newsgroups, lastdate, lasttime);
	sockwrite(buf);
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
		(void) printf("protocol error: got '%s'\n", buf);
		(void) close(server);
		exit(1);
		}
	/* and here comes the list, terminated with a "." */
#ifdef DEBUG
	(void) printf("data\n");
#endif
	while (1)
		{
		(void) sockread(buf);
		if (!strcmp(buf,"."))
			break;
		if (wewant(buf))
			{
			if (newart > MAXARTS)
				{
				omitupdate++;
				continue;
				}
			artlist[newart] = malloc((unsigned)(strlen(buf)+1));
			(void) strcpy(artlist[newart], buf);
			newart++;
			}
		else
			dupart++;
		}
#ifdef DEBUG
	(void) printf(".\n%d new, %d dup articles\n", newart, dupart);
#endif

	/* now that we know which articles we want, retrieve them */
	for (i=1; i < newart; i++)
		(void) artfetch(artlist[i]);

#ifdef DEBUG
	(void) printf("%d missing articles\n", misart);
#endif
	/* we're all done, so tell them goodbye */
	sockwrite("QUIT");
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
		(void) printf("error: got '%s'\n", buf);
		(void) close(server);
		exit(1);
		}
	(void) close(server);

	/* do we want to update the timestamp file? */
	if (!omitupdate)
		{
		(void) sprintf(buf, "%s %06d %06d %s\n",
			newsgroups, newdate, newtime, distributions);
#ifdef DEBUG
		(void) printf("updating %s:\n\t%s\n", dtname, buf);
#endif
		dtfile = fopen(dtname, "w");
		if (dtfile == NULL)
			{
			perror(dtname);
			exit(1);
			}
		(void) fputs(buf,dtfile);
		(void) fclose(dtfile);
		}
	exit(0);
}

artfetch(articleid)
char *articleid;
	{
	int lines = 0;
	char buf[BUFSIZ];
	FILE *inews;

	/* now, ask for the article */
	(void) sprintf(buf,"ARTICLE %s", articleid);
	sockwrite(buf);
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] == '4')	/* missing article, just skipit */
		{
		misart++;
		return(0);
		}

	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
		(void) printf("protocol error: got '%s'\n", buf);
		(void) close(server);
		exit(1);
		}
#ifdef DEBUG
	(void) printf("command: %s\n", INEWS);
#endif
	if ( (inews = popen(INEWS, "w")) == NULL)
		{
		perror(INEWS);
		exit(1);
		}

	/* and here comes the article, terminated with a "." */
#ifdef DEBUG
	(void) printf("data\n");
#endif
	while (1)
		{
		(void) sockread(buf);
		if (buf[0] == '.' && buf[1] == '\0')
			break;
		lines++;
		(void) strcat(buf,"\n");
		(void) fputs(((buf[0] == '.') ? buf + 1 : buf),
			   inews);
		}
#ifdef DEBUG
	(void) printf(".\n%d lines\n", lines);
#endif
	(void) fflush(inews);
	(void) pclose(inews);
	return(0);
        }

int
sockread(buf)
char *buf;
	{
	char c;
	int j = 0;
#ifdef BSD43
	fd_set rf;
#else BSD43
	int rf;
#endif BSD43
	struct timeval tv;
	int r;
	char *p = buf;

	while ( 1 )
		{
		tv.tv_sec = 1800;	/* 15 minutes */
		tv.tv_usec = 0L;
#ifdef BSD43
		FD_ZERO(&rf);
		FD_SET(server, &rf);
#else BSD43
		rf = 1 << server;
#endif BSD43
		r = select(20, (fd_set *)&rf, (fd_set *)0, (fd_set *)&rf, &tv);

		if (r < 0)
			{
			if (errno == EINTR)
				continue;
			perror("getsock select");
			exit(1);
			}
		if (r == 0)
			{
			printf("read timed out.\n");
			exit(1);
			}

		if (read(server, &c, 1) <= 0)
			break;

		/* mask off any chance parity bits */
		*p = c & 0x7f;

		/* look for end of line (== LF) */
		if (c == 0x0a)
			{
			if (j > 0 && *(p-1) == 0x0d)
				*(p-1) = '\0';
			else
				*p = '\0';
			return(strlen(buf));
			}
		j++; p++;
		}
	perror("sockread");
	(void) close(server);
	exit(1);
	/* NOTREACHED */
	}

sockwrite(buf)
char *buf;
	{
	register int sz;
	char buf2[BUFSIZ];
#ifdef DEBUG
	(void) printf(">>> %s\n", buf);
#endif
	(void) strcpy(buf2,buf);
	(void) strcat(buf2,"\r\n");
	sz = strlen(buf2);
	if (write(server,buf2,sz) != sz)
		{
		(void) printf("write error on server socket\n");
		(void) close(server);
		exit(1);
		}
	}

int
wewant(articleid)
char *articleid;
	{
	datum k, d;
	char id[BUFSIZ];
	char *p;

	/* remove any case sensitivity */
	(void) strcpy(id, articleid);
	p = id;
	while (*p)
		{
		if (isupper(*p))
			*p = tolower(*p);
		p++;
		}

	k.dptr = id;
	k.dsize = strlen(articleid) + 1;

	d = fetch(k);

	if (d.dptr)
		{
#ifdef DEBUG
		(void) printf("dup: '%s'\n", articleid);
#endif
		return(0);
		}

#ifdef DEBUG
	(void) printf("new: '%s'\n", articleid);
#endif
	return(1);
	}
