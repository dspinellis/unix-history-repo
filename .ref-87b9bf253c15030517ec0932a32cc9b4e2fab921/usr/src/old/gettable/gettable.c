#ifndef lint
static char sccsid[] = "@(#)gettable.c	4.1 (Bekeley) %G%";
#endif

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <netdb.h>

#define	OUTFILE		"hosts.txt"	/* default output file */
#define	QUERY		"ALL\r\n"	/* query to hostname server */

#define	equaln(s1, s2, n)	(!strncmp(s1, s2, n))

struct	sockaddr_in sin;
char	buf[BUFSIZ];
char	*outfile = OUTFILE;

main(argc, argv)
	int argc;
	char *argv[];
{
	int s;
	register len;
	register FILE *sfi, *sfo, *hf;
	register char *p;
	char *host;
	register struct hostent *hp;
	struct servent *sp;

	argv++, argc--;
	if (argc < 1 || argc > 2) {
		fprintf(stderr, "usage: gettable host [ file ]\n");
		exit(1);
	}
	sp = getservbyname("nicname", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "gettable: nicname/tcp: unknown service\n");
		exit(3);
	}
	host = *argv;
	argv++, argc--;
	hp = gethostbyname(host);
	if (hp == NULL) {
		fprintf(stderr, "gettable: %s: host unknown\n", host);
		exit(2);
	}
	host = hp->h_name;
	if (argc > 0)
		outfile = *argv;
	sin.sin_family = hp->h_addrtype;
	s = socket(hp->h_addrtype, SOCK_STREAM, 0, 0);
	if (s < 0) {
		perror("gettable: socket");
		exit(4);
	}
	if (bind(s, &sin, sizeof (sin), 0) < 0) {
		perror("gettable: bind");
		exit(5);
	}
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	sin.sin_port = sp->s_port;
	if (connect(s, &sin, sizeof (sin), 0) < 0) {
		perror("gettable: connect");
		exit(6);
	}
	fprintf(stderr, "Connection to %s opened.\n", host);
	sfi = fdopen(s, "r");
	sfo = fdopen(s, "w");
	if (sfi == NULL || sfo == NULL) {
		perror("gettable: fdopen");
		close(s);
		exit(1);
	}
	hf = fopen(outfile, "w");
	if (hf == NULL) {
		fprintf(stderr, "gettable: "); perror(outfile);
		close(s);
		exit(1);
	}
	fprintf(sfo, QUERY);
	fflush(sfo);
	while (fgets(buf, sizeof(buf), sfi) != NULL) {
		len = strlen(buf);
		buf[len-2] = '\0';
		if (equaln(buf, "BEGIN", 5) || equaln(buf, "END", 3)) {
			continue;
		}
		if (equaln(buf, "ERR", 3)) {
			fprintf(stderr, "gettable: nicname error: %s", buf);
			continue;
		}
		fprintf(hf, "%s\n", buf);
	}
	fclose(hf);
	fprintf(stderr, "Host table received.\n");
	close(s);
	fprintf(stderr, "Connection to %s closed\n", host);
}
