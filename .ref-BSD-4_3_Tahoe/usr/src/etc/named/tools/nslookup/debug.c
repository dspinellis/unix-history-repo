/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)debug.c	5.11 (Berkeley) 4/5/88";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  debug.c --
 *
 *	Routines to print out packets received from a name server query.
 *
 *      Modified version of 4.3BSD BIND res_debug.c 5.6 9/14/85
 *
 *******************************************************************************
 */

#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include "res.h"

extern char ctime();

/*
 *  Imported from res_debug.c
 */
extern char *_res_resultcodes[];
extern char *_res_opcodes[];

/*
 *  Used to highlight the start of a record when printing it.
 */
#define INDENT "    ->  "



/*
 * Print the contents of a query.
 * This is intended to be primarily a debugging routine.
 */

Print_query(msg, eom, printHeader)
	char *msg, *eom;
	int printHeader;
{
	Fprint_query(msg, eom, printHeader,stdout);
}

Fprint_query(msg, eom, printHeader,file)
	char *msg, *eom;
	int printHeader;
	FILE *file;
{
	register char *cp;
	register HEADER *hp;
	register int n;
	short class;
	short type;

	/*
	 * Print header fields.
	 */
	hp = (HEADER *)msg;
	cp = msg + sizeof(HEADER);
	if (printHeader || (_res.options & RES_DEBUG2)) {
	    fprintf(file,"    HEADER:\n");
	    fprintf(file,"\topcode = %s", _res_opcodes[hp->opcode]);
	    fprintf(file,", id = %d", ntohs(hp->id));
	    fprintf(file,", rcode = %s\n", _res_resultcodes[hp->rcode]);
	    fprintf(file,"\theader flags: ");
	    if (hp->qr) {
		    fprintf(file," response");
	    } else {
		    fprintf(file," query");
	    }
	    if (hp->aa)
		    fprintf(file,", auth. answer");
	    if (hp->tc)
		    fprintf(file,", truncation");
	    if (hp->rd)
		    fprintf(file,", want recursion");
	    if (hp->ra)
		    fprintf(file,", recursion avail.");
	    if (hp->pr)
		    fprintf(file,", primary");
	    fprintf(file,"\n\tquestions = %d", ntohs(hp->qdcount));
	    fprintf(file,",  answers = %d", ntohs(hp->ancount));
	    fprintf(file,",  auth. records = %d", ntohs(hp->nscount));
	    fprintf(file,",  additional = %d\n\n", ntohs(hp->arcount));
	}

	/*
	 * Print question records.
	 */
	if (n = ntohs(hp->qdcount)) {
		fprintf(file,"    QUESTIONS:\n");
		while (--n >= 0) {
			fprintf(file,"\t");
			cp = Print_cdname(cp, msg, eom, file);
			if (cp == NULL)
				return;
			type = _getshort(cp);
			cp += sizeof(u_short);
			class = _getshort(cp);
			cp += sizeof(u_short);
			fprintf(file,", type = %s", p_type(type));
			fprintf(file,", class = %s\n", p_class(class));
		}
	}
	/*
	 * Print authoritative answer records
	 */
	if (n = ntohs(hp->ancount)) {
		fprintf(file,"    ANSWERS:\n");
		while (--n >= 0) {
			fprintf(file, INDENT);
			cp = Print_rr(cp, msg, eom, file);
			if (cp == NULL)
				return;
		}
	}
	/*
	 * print name server records
	 */
	if (n = ntohs(hp->nscount)) {
		fprintf(file,"    AUTHORITY RECORDS:\n");
		while (--n >= 0) {
			fprintf(file, INDENT);
			cp = Print_rr(cp, msg, eom, file);
			if (cp == NULL)
				return;
		}
	}
	/*
	 * print additional records
	 */
	if (n = ntohs(hp->arcount)) {
		fprintf(file,"    ADDITIONAL RECORDS:\n");
		while (--n >= 0) {
			fprintf(file, INDENT);
			cp = Print_rr(cp, msg, eom, file);
			if (cp == NULL)
				return;
		}
	}
	fprintf(file,"\n------------\n");

}


char *
Print_cdname_sub(cp, msg, eom, file, format)
	char *cp, *msg, *eom;
	FILE *file;
	int format;
{
	int n;
	char name[MAXDNAME];
	extern char *strcpy();

	if ((n = dn_expand(msg, eom, cp, name, sizeof(name))) < 0)
		return (NULL);
	if (name[0] == '\0') {
	    (void) strcpy(name, "(root)");
	}
	if (format) {
	    fprintf(file, "%-30s", name);
	} else {
	    fputs(name, file);
	}
	return (cp + n);
}

char *
Print_cdname(cp, msg, eom, file)
	char *cp, *msg, *eom;
	FILE *file;
{
    return(Print_cdname_sub(cp, msg, eom, file, 0));
}

char *
Print_cdname2(cp, msg, eom, file)
	char *cp, *msg, *eom;
	FILE *file;
{
    return(Print_cdname_sub(cp, msg, eom, file, 1));
}

/*
 * Print resource record fields in human readable form.
 */
char *
Print_rr(cp, msg, eom, file)
	char *cp, *msg, *eom;
	FILE *file;
{
	int type, class, dlen, n, c;
	long ttl;
	struct in_addr inaddr;
	char *cp1;

	if ((cp = Print_cdname(cp, msg, eom, file)) == NULL) {
		fprintf(file, "(name truncated?)\n");
		return (NULL);			/* compression error */
	}

	type = _getshort(cp);
	cp += sizeof(u_short);
	class = _getshort(cp);
	cp += sizeof(u_short);
	ttl = _getlong(cp);
	cp += sizeof(u_long);
	dlen = _getshort(cp);
	cp += sizeof(u_short);

	if (_res.options & RES_DEBUG2) {
	    fprintf(file,"\n\ttype = %s, class = %s, ttl = %u, dlen = %d",
			p_type(type), p_class(class), ttl, dlen);
	    fprintf(file,"\n");
	}

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
				fprintf(file,"\tinet address = %s\n",
					inet_ntoa(inaddr));
				cp += dlen;
			} else if (dlen == 7) {
				fprintf(file,"\tinet address = %s",
					inet_ntoa(inaddr));
				fprintf(file,", protocol = %d", cp[4]);
				fprintf(file,", port = %d\n",
					(cp[5] << 8) + cp[6]);
				cp += dlen;
			}
			break;
		default:
			fprintf(file,"\taddress, class = %d, len = %d\n",
			    class, dlen);
		}
		break;

	case T_CNAME:
		fprintf(file,"\tcanonical name = ");
		goto doname;

	case T_MX:
		fprintf(file,"\tpreference = %d",_getshort(cp));
		cp += sizeof(u_short);
		fprintf(file,", mail exchanger = ");
		goto doname;

	case T_MG:
		fprintf(file,"\tmail group member = ");
		goto doname;
	case T_MB:
		fprintf(file,"\tmail box = ");
		goto doname;
	case T_MR:
		fprintf(file,"\tmailbox rename = ");
		goto doname;
	case T_NS:
		fprintf(file,"\tnameserver = ");
		goto doname;
	case T_PTR:
		fprintf(file,"\thost name = ");
doname:
		cp = Print_cdname(cp, msg, eom, file);
		fprintf(file,"\n");
		break;

	case T_HINFO:
		if (n = *cp++) {
			fprintf(file,"\tCPU=%.*s", n, cp);
			cp += n;
		}
		if (n = *cp++) {
			fprintf(file,"\tOS=%.*s\n", n, cp);
			cp += n;
		}
		break;

	case T_SOA:
		fprintf(file,"\torigin = ");
		cp = Print_cdname(cp, msg, eom, file);
		fprintf(file,"\n\tmail addr = ");
		cp = Print_cdname(cp, msg, eom, file);
		fprintf(file,"\n\tserial=%ld", _getlong(cp));
		cp += sizeof(u_long);
		fprintf(file,", refresh=%ld", _getlong(cp));
		cp += sizeof(u_long);
		fprintf(file,", retry=%ld", _getlong(cp));
		cp += sizeof(u_long);
		fprintf(file,", expire=%ld", _getlong(cp));
		cp += sizeof(u_long);
		fprintf(file,", min=%ld\n", _getlong(cp));
		cp += sizeof(u_long);
		break;

	case T_MINFO:
		fprintf(file,"\trequests = ");
		cp = Print_cdname(cp, msg, eom, file);
		fprintf(file,"\n\terrors = ");
		cp = Print_cdname(cp, msg, eom, file);
		break;

	case T_UINFO:
		fprintf(file,"\t%s\n", cp);
		cp += dlen;
		break;

	case T_UID:
	case T_GID:
		if (dlen == 4) {
			fprintf(file,"\t%cid %ld\n", type == T_UID ? 'u' : 'g',
			    _getlong(cp));
			cp += sizeof(int);
		} else
			fprintf(file,"\t%cid of length %ld?\n",
			    type == T_UID ? 'u' : 'g', dlen);
		break;

	case T_WKS:
		if (dlen < sizeof(u_long) + 1)
			break;
		bcopy(cp, (char *)&inaddr, sizeof(inaddr));
		cp += sizeof(u_long);
		fprintf(file,"\tinet address = %s, protocol = %d\n\t",
			inet_ntoa(inaddr), *cp++);
		n = 0;
		while (cp < cp1 + dlen) {
			c = *cp++;
			do {
 				if (c & 0200)
					fprintf(file," %d", n);
 				c <<= 1;
			} while (++n & 07);
		}
		putc('\n',file);
		break;

	case T_NULL:
		fprintf(file, "(type NULL, dlen %d)\n", dlen);
		break;

	default:
		fprintf(file,"\t???\n");
		cp += dlen;
	}
	if (cp != cp1 + dlen)
		fprintf(file,"packet size error (%#x != %#x)\n", cp, cp1+dlen);
	return (cp);
}
