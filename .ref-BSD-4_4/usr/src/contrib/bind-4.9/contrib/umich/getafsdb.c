#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <resolv.h>

#define MAXSERVERS	35

static char hostbuf[BUFSIZ+1];
static char *servers[MAXSERVERS];

#if PACKETSZ > 1024
#define	MAXPACKET	PACKETSZ
#else
#define	MAXPACKET	1024
#endif

typedef union {
    HEADER hdr;
    u_char buf[MAXPACKET];
} querybuf;

static char ** getanswer(answer, anslen, wanted)
querybuf *answer;
int anslen, wanted;
{
	register HEADER *hp;
	register u_char *cp;
	register int n, i = 0;
	u_char *eom;
	char *bp;
	int fs, type, class, blen, ancount, qdcount, haveanswer;

	eom = answer->buf + anslen;
	hp = &answer->hdr;
	ancount = ntohs(hp->ancount);
	qdcount = ntohs(hp->qdcount);
	bp = hostbuf;
	blen = sizeof(hostbuf);
	cp = answer->buf + sizeof(HEADER);
	if (qdcount) {
		cp += dn_skipname(cp, eom) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
	}
	haveanswer = 0;
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand((char *)answer->buf, eom, cp, bp, blen)) < 0)
			break;
		cp += n;
		type = _getshort(cp);
 		cp += sizeof(u_short);
		class = _getshort(cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		n = _getshort(cp);
		cp += sizeof(u_short);
		fs = _getshort(cp);
		if ((type != T_AFSDB) || (class != C_IN) || (fs != wanted)) {
			cp += n;
			continue;
		}
		cp += sizeof(u_short);
		if ((n = dn_expand((char *)answer->buf, eom, cp, bp, blen)) < 0)
			break;
		if (bp[0] == '\0') {
			bp[0] = '.';
			bp[1] = '\0';
		}
		servers[i++] = strdup(bp);
		cp += n;
		haveanswer++;
	}
	if (haveanswer) {
		servers[i] = NULL;
		return (servers);
	}
	return ((char **) NULL);
}

char ** getafsdb(name, type)
char *name;
int type;
{
	querybuf buf;
	int n;

	if ((n = res_search(name, C_IN, T_AFSDB, buf.buf, sizeof(buf))) < 0)
			return ((char **) NULL);
	return (getanswer(&buf, n, type));
}
