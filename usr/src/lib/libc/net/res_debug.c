/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)res_debug.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <nameser.h>

extern char *p_cdname(), *p_rr(), *p_type(), *p_class();
extern char *inet_ntoa();

char *opcodes[] = {
	"QUERY",
	"IQUERY",
	"CQUERYM",
	"CQUERYU",
	"4",
	"5",
	"6",
	"7",
	"8",
	"9",
	"10",
	"UPDATEA",
	"UPDATED",
	"UPDATEM",
	"ZONEINIT",
	"ZONEREF",
};

char *rcodes[] = {
	"NOERROR",
	"FORMERR",
	"SERVFAIL",
	"NXDOMAIN",
	"NOTIMP",
	"REFUSED",
	"6",
	"7",
	"8",
	"9",
	"10",
	"11",
	"12",
	"13",
	"14",
	"NOCHANGE",
};

/*
 * Print the contents of a query.
 * This is intended to be primarily a debugging routine.
 */
p_query(msg)
	char *msg;
{
	register char *cp;
	register HEADER *hp;
	register int n;

	/*
	 * Print header fields.
	 */
	hp = (HEADER *)msg;
	cp = msg + sizeof(HEADER);
	printf("HEADER:\n");
	printf("\topcode = %s", opcodes[hp->opcode]);
	printf(", id = %d", ntohs(hp->id));
	printf(", rcode = %s\n", rcodes[hp->rcode]);
	printf("\theader flags: ");
	if (hp->qr)
		printf(" qr");
	if (hp->aa)
		printf(" aa");
	if (hp->tc)
		printf(" tc");
	if (hp->rd)
		printf(" rd");
	if (hp->ra)
		printf(" ra");
	if (hp->pr)
		printf(" pr");
	printf("\n\tqdcount = %d", ntohs(hp->qdcount));
	printf(", ancount = %d", ntohs(hp->ancount));
	printf(", nscount = %d", ntohs(hp->nscount));
	printf(", arcount = %d\n\n", ntohs(hp->arcount));
	/*
	 * Print question records.
	 */
	if (n = ntohs(hp->qdcount)) {
		printf("QUESTIONS:\n");
		while (--n >= 0) {
			printf("\t");
			cp = p_cdname(cp, msg);
			if (cp == NULL)
				return;
			printf(", type = %s", p_type(getshort(cp)));
			cp += sizeof(u_short);
			printf(", class = %s\n\n", p_class(getshort(cp)));
			cp += sizeof(u_short);
		}
	}
	/*
	 * Print authoritative answer records
	 */
	if (n = ntohs(hp->ancount)) {
		printf("ANSWERS:\n");
		while (--n >= 0) {
			printf("\t");
			cp = p_rr(cp, msg);
			if (cp == NULL)
				return;
		}
	}
	/*
	 * print name server records
	 */
	if (n = ntohs(hp->nscount)) {
		printf("NAME SERVERS:\n");
		while (--n >= 0) {
			printf("\t");
			cp = p_rr(cp, msg);
			if (cp == NULL)
				return;
		}
	}
	/*
	 * print additional records
	 */
	if (n = ntohs(hp->arcount)) {
		printf("ADDITIONAL RECORDS:\n");
		while (--n >= 0) {
			printf("\t");
			cp = p_rr(cp, msg);
			if (cp == NULL)
				return;
		}
	}
}

char *
p_cdname(cp, msg)
	char *cp, *msg;
{
	char name[MAXDNAME];
	int n;

	if ((n = dn_expand(msg, cp, name, sizeof(name))) < 0)
		return (NULL);
	if (name[0] == '\0') {
		name[0] = '.';
		name[1] = '\0';
	}
	fputs(name, stdout);
	return (cp + n);
}

/*
 * Print resource record fields in human readable form.
 */
char *
p_rr(cp, msg)
	char *cp, *msg;
{
	int type, class, dlen, n, c;
	struct in_addr inaddr;
	char *cp1;

	if ((cp = p_cdname(cp, msg)) == NULL)
		return (NULL);			/* compression error */
	printf("\n\ttype = %s", p_type(type = getshort(cp)));
	cp += sizeof(u_short);
	printf(", class = %s", p_class(class = getshort(cp)));
	cp += sizeof(u_short);
	printf(", ttl = %ld", getlong(cp));
	cp += sizeof(u_long);
	printf(", dlen = %d\n", dlen = getshort(cp));
	cp += sizeof(u_short);
	cp1 = cp;
	/*
	 * Print type specific data, if appropriate
	 */
	switch (type) {
	case T_A:
		switch (class) {
		case C_IN:
			bcopy(cp, (char *)&inaddr, sizeof(inaddr));
			if (dlen == 4) {
				printf("\tinternet address = %s\n",
					inet_ntoa(inaddr));
				cp += dlen;
			} else if (dlen == 7) {
				printf("\tinternet address = %s",
					inet_ntoa(inaddr));
				printf(", protocol = %d", cp[4]);
				printf(", port = %d\n",
					(cp[5] << 8) + cp[6]);
				cp += dlen;
			}
			break;
		}
		break;
	case T_CNAME:
	case T_MB:
	case T_MD:
	case T_MF:
	case T_MG:
	case T_MR:
	case T_NS:
	case T_PTR:
		printf("\tdomain name = ");
		cp = p_cdname(cp, msg);
		printf("\n");
		break;

	case T_HINFO:
		if (n = *cp++) {
			printf("\tCPU=%.*s\n", n, cp);
			cp += n;
		}
		if (n = *cp++) {
			printf("\tOS=%.*s\n", n, cp);
			cp += n;
		}
		break;

	case T_SOA:
		printf("\torigin = ");
		cp = p_cdname(cp, msg);
		printf("\n\tmail addr = ");
		cp = p_cdname(cp, msg);
		printf("\n\tserial=%ld", getlong(cp));
		cp += sizeof(u_long);
		printf(", refresh=%ld", getlong(cp));
		cp += sizeof(u_long);
		printf(", retry=%ld", getlong(cp));
		cp += sizeof(u_long);
		printf(", expire=%ld", getlong(cp));
		cp += sizeof(u_long);
		printf(", min=%ld\n", getlong(cp));
		cp += sizeof(u_long);
		break;

	case T_MINFO:
		printf("\trequests = ");
		cp = p_cdname(cp, msg);
		printf("\n\terrors = ");
		cp = p_cdname(cp, msg);
		break;

	case T_UINFO:
		printf("\t%s\n", cp);
		cp += dlen;
		break;

	case T_UID:
	case T_GID:
		if (dlen == 4) {
			printf("\t%ld\n", getlong(cp));
			cp += sizeof(int);
		}
		break;

	case T_WKS:
		if (dlen < sizeof(u_long) + 1)
			break;
		bcopy(cp, (char *)&inaddr, sizeof(inaddr));
		cp += sizeof(u_long);
		printf("\tinternet address = %s, protocol = %d\n\t",
			inet_ntoa(inaddr), *cp++);
		n = 0;
		while (cp < cp1 + dlen) {
			c = *cp++;
			do {
				if (c & 1)
					printf(" %d", n);
				c >>= 1;
			} while (++n & 07);
		}
		putchar('\n');
		break;

	default:
		printf("\t???\n");
		cp += dlen;
	}
	if (cp != cp1 + dlen)
		printf("packet size error (%#x != %#x)\n", cp, cp1+dlen);
	printf("\n");
	return (cp);
}

static	char nbuf[20];
extern	char *sprintf();

/*
 * Return a string for the type
 */
char *
p_type(type)
	int type;
{

	switch (type) {
	case T_A:
		return("A");
	case T_NS:		/* authoritative server */
		return("NS");
	case T_MD:		/* mail destination */
		return("MD");
	case T_MF:		/* mail forwarder */
		return("MF");
	case T_CNAME:		/* connonical name */
		return("CNAME");
	case T_SOA:		/* start of authority zone */
		return("SOA");
	case T_MB:		/* mailbox domain name */
		return("MB");
	case T_MG:		/* mail group member */
		return("MG");
	case T_MR:		/* mail rename name */
		return("MR");
	case T_NULL:		/* null resource record */
		return("NULL");
	case T_WKS:		/* well known service */
		return("WKS");
	case T_PTR:		/* domain name pointer */
		return("PTR");
	case T_HINFO:		/* host information */
		return("HINFO");
	case T_MINFO:		/* mailbox information */
		return("MINFO");
	case T_AXFR:		/* zone transfer */
		return("AXFR");
	case T_MAILB:		/* mail box */
		return("MAILB");
	case T_MAILA:		/* mail address */
		return("MAILA");
	case T_ANY:		/* matches any type */
		return("ANY");
	case T_UINFO:
		return("UINFO");
	case T_UID:
		return("UID");
	case T_GID:
		return("GID");
	default:
		return (sprintf(nbuf, "%d", type));
	}
}

/*
 * Return a mnemonic for class
 */
char *
p_class(class)
	int class;
{

	switch (class) {
	case C_IN:		/* internet class */
		return("IN");
	case C_CS:		/* csnet class */
		return("CS");
	case C_ANY:		/* matches any class */
		return("ANY");
	default:
		return (sprintf(nbuf, "%d", class));
	}
}
