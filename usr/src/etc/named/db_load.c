#ifndef lint
static char sccsid[] = "@(#)db_load.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

/*
 * Load data base from ascii backupfile.  Format similar to RFC 883.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <stdio.h>
#include <syslog.h>
#include <ctype.h>
#include <netdb.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

extern char *index();

/*
 * Map class and type names to number
 */
struct map {
	char	token[8];
	int	val;
};

struct map m_class[] = {
	"in",		C_IN,
	"any",		C_ANY,
};
#define NCLASS (sizeof(m_class)/sizeof(struct map))

struct map m_type[] = {
	"a",		T_A,
	"ns",		T_NS,
	"cname",	T_CNAME,
	"soa",		T_SOA,
	"mb",		T_MB,
	"mg",		T_MG,
	"mr",		T_MR,
	"null",		T_NULL,
	"wks",		T_WKS,
	"ptr",		T_PTR,
	"hinfo",	T_HINFO,
	"minfo",	T_MINFO,
	"mx",		T_MX,
	"uinfo",	T_UINFO,
	"uid",		T_UID,
	"gid",		T_GID,
	"any",		T_ANY,
};
#define NTYPE (sizeof(m_type)/sizeof(struct map))

/*
 * Parser token values
 */
#define CURRENT	1
#define DOT	2
#define AT	3
#define DNAME	4
#define INCLUDE	5
#define ORIGIN	6

int	lineno = 1;		/* current line number */

/*
 * Load the database from 'filename'. Origin is appended to all domain
 * names in the file.
 */
db_load(filename, in_origin, zone)
	char *filename, *in_origin;
	int zone;
{
	register char *cp;
	register struct map *mp;
	char domain[MAXDNAME];
	char origin[MAXDNAME];
	char buf[BUFSIZ];
	char data[MAXDATA];
	char *op;
	int class, type, ttl;
	struct databuf *dp;
	FILE *fp;
	int slineno, i;
	long n;

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"db_load(%s, %s, %d)\n", filename, in_origin, zone);
#endif

	(void) strcpy(origin, in_origin);
	if ((fp = fopen(filename, "r")) == NULL) {
		syslog(LOG_ERR, "%s: %m", filename);
		return (NODBFILE);
	}
	lineno = 1;
	domain[0] = '\0';
	class = C_IN;
	while ((n = gettoken(fp)) != EOF) {
		switch ((int)n) {
		case INCLUDE:
			if (!getword(buf, sizeof(buf), fp)) /* file name */
				break;
			slineno = lineno;
			(void) db_load(buf, origin, zone);
			lineno = slineno;
			continue;

		case ORIGIN:
			(void) strcpy(buf, origin);
			if (!getword(origin, sizeof(origin), fp))
				break;
			makename(origin, buf);
			continue;

		case DNAME:
			if (!getword(domain, sizeof(domain), fp))
				break;
			n = strlen(domain) - 1;
			if (domain[n] == '.')
				domain[n] = '\0';
			else if (*origin) {
				(void) strcat(domain, ".");
				(void) strcat(domain, origin);
			}
			goto gotdomain;

		case AT:
			(void) strcpy(domain, origin);
			goto gotdomain;

		case DOT:
			domain[0] = '\0';
			/* fall thru ... */
		case CURRENT:
		gotdomain:
			if (!getword(buf, sizeof(buf), fp))
				break;
			cp = buf;
			if (isdigit(*cp)) {
				n = 0;
				do
					n = n * 10 + (*cp++ - '0');
				while (isdigit(*cp));
				if (zone == 0) {
				    if (gettimeofday(&tt,
					(struct timezone *)0) < 0)
					syslog(LOG_ERR,
					   "gettimeofday failed: %m");
					n += (long) tt.tv_sec;
				}
				ttl = n;
				if (!getword(buf, sizeof(buf), fp))
					break;
			} else
				ttl = 0;
			for (mp = m_class; mp < m_class+NCLASS; mp++)
				if (!cistrcmp(buf, mp->token)) {
					class = mp->val;
					(void) getword(buf, sizeof(buf), fp);
					break;
				}
			for (mp = m_type; mp < m_type+NTYPE; mp++)
				if (!cistrcmp(buf, mp->token)) {
					type = mp->val;
					goto fndtype;
				}
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"Unknown type: %s.\n", buf);
#endif
			break;
		fndtype:
			if (!getword(buf, sizeof(buf), fp))
				break;
#ifdef DEBUG
			if (debug >= 3)
			    fprintf(ddt,"d='%s',c=%d,t=%d, ttl=%d, data='%s'\n",
					domain, class, type, ttl, buf);
#endif
			/*
			 * Convert the ascii data 'buf' to the proper format
			 * based on the type and pack into 'data'.
			 */
			switch (type) {
			case T_A:
				n = ntohl((u_long)inet_addr((char *)buf));
				putlong((u_long)n, data);
				n = sizeof(u_long);
				break;

			case T_HINFO:
				n = strlen(buf);
				if (n > 255) {
				    syslog(LOG_WARNING, "CPU too large (%d)",n);
				    n = 255;
				}
				data[0] = n;
				bcopy(buf, (char *)data + 1, (int)n);
				n++;
				if (!getword(buf, sizeof(buf), fp))
					break;
				i = strlen(buf);
				if (i > 255) {
				    syslog(LOG_WARNING, "OS too large (%d)", i);
				    i = 255;
				}
				data[n] = i;
				bcopy(buf, data + n + 1, i);
				n += i + 1;
				break;

			case T_SOA:
			case T_MINFO:
				(void) strcpy(data, buf);
				makename(data, origin);
				cp = data + strlen(data) + 1;
				if (!getword(cp, sizeof(data) - (cp - data),fp))
					break;
				makename(cp, origin);
				cp += strlen(cp) + 1;
				if (type == T_MINFO) {
					n = cp - data;
					break;
				}
				if (getnonblank(fp) != '(')
					goto err;
				putlong((u_long)(zones[zone].z_serial =
				    getnum(fp)), cp);
				cp += sizeof(u_long);
				putlong((u_long)(zones[zone].z_refresh =
				    getnum(fp)), cp);
				cp += sizeof(u_long);
				putlong((u_long)(zones[zone].z_retry =
				    getnum(fp)), cp);
				cp += sizeof(u_long);
				putlong ((u_long)(zones[zone].z_expire =
				    getnum(fp)), cp);
				cp += sizeof(u_long);
				putlong ((u_long)(zones[zone].z_minimum =
				    getnum(fp)), cp);
				cp += sizeof(u_long);
				n = cp - data;
				if (getnonblank(fp) != ')')
					goto err;
				endline(fp);
				break;

			case T_UID:
			case T_GID:
				n = 0;
				cp = buf;
				while (isdigit(*cp))
					n = n * 10 + (*cp++ - '0');
				if (cp == buf)
					goto err;
				putlong((u_long)n, data);
				n = sizeof(long);
				break;

			case T_WKS:
				/* Address */
				n = ntohl((u_long)inet_addr((char *)buf));
				putlong((u_long)n, data);
				data[sizeof(u_long)] = getprotocol(fp);
				/* Protocol */
				n = sizeof(u_long) + sizeof(char);
				/* Services */
				n = getservices((int)n, data, fp);
				break;

			case T_NS:
			case T_CNAME:
			case T_MB:
			case T_MG:
			case T_MR:
			case T_PTR:
				(void) strcpy(data, buf);
				makename(data, origin);
				n = strlen(data) + 1;
				break;

			case T_UINFO:
				cp = index(buf, '&');
				bzero(data, sizeof(data));
				if ( cp != NULL) {
					(void) strncpy(data, buf, cp - buf);
					op = index(domain, '.');
					if ( op != NULL)
					    (void) strncat(data,
						domain,op-domain);
					else
						(void) strcat(data, domain);
					(void) strcat(data, ++cp);
				} else
					(void) strcpy(data, buf);
				n = strlen(data) + 1;
				break;
			case T_MX:
				n = 0;
				cp = buf;
				while (isdigit(*cp))
					n = n * 10 + (*cp++ - '0');
				/* catch bad values */
				if ((cp == buf) || (n & ~0xffff))
					goto err;

				putshort((u_short)n, data);
				cp = data + sizeof(u_short);

				if (!getword(buf, sizeof(buf), fp))
					    break;
				(void) strcpy(cp,buf);
				makename(cp, origin);
				/* get pointer to place in data */
				cp += strlen(cp) +1;

				/* now save length */
				n = (cp - data);
				break;

			default:
				goto err;
			}
			dp = savedata(class, type, (u_long)ttl, data, (int)n);
			dp->d_zone = zone;
			if ((n = db_update(domain, dp, dp, DB_NODATA)) < 0) {
#ifdef DEBUG
				if (debug && (n != DATAEXISTS))
					fprintf(ddt,"update failed\n");
#endif
			}
			continue;
		}
	err:
		syslog(LOG_ERR, "%s: line %d: database format error (%s)",
			filename, lineno, buf);
		(void) fclose(fp);
		return (-1);
	}
	(void) fclose(fp);
	return (OK);
}

int gettoken(fp)
	register FILE *fp;
{
	register int c;
	char op[32];

	for (;;) {
		c = getc(fp);
	top:
		switch (c) {
		case EOF:
			return (EOF);

		case '$':
			if (!getword(op, sizeof(op), fp))
				return (EOF);
			if (!cistrcmp("include", op))
				return (INCLUDE);
			if (!cistrcmp("origin", op))
				return (ORIGIN);
			printf("Unknown $ option: %s\n", op);
			/* fall through... */

		case ';':
			while ((c = getc(fp)) != EOF && c != '\n')
				;
			goto top;

		case ' ':
		case '\t':
			return (CURRENT);

		case '.':
			return (DOT);

		case '@':
			return (AT);

		case '\n':
			lineno++;
			continue;

		default:
			(void) ungetc(c, fp);
			return (DNAME);
		}
	}
}

/*
 * Get next word, skipping blanks & comments.
 */
getword(buf, size, fp)
	char *buf;
	int size;
	FILE *fp;
{
	register char *cp;
	register int c;

	for (cp = buf; (c = getc(fp)) != EOF; ) {
		if (isspace(c)) {
			if (c == '\n')
				lineno++;
			if (cp != buf)
				break;
			continue;
		}
		if (c == ';') {
			while ((c = getc(fp)) != EOF && c != '\n')
				;
			if (c == '\n')
				lineno++;
			if (cp != buf)
				break;
			continue;
		}
		if (c == '"') {
			while ((c = getc(fp)) != EOF && c != '"' && c != '\n') {
				if (c == '\\') {
					if ((c = getc(fp)) == EOF)
						c = '\\';
					if (c == '\n')
						lineno++;
				}
				if (cp >= buf+size-1)
					break;
				*cp++ = c;
			}
			if (c == '\n') {
				lineno++;
				break;
			}
			continue;
		}
		if (c == '\\') {
			if ((c = getc(fp)) == EOF)
				c = '\\';
			if (c == '\n')
				lineno++;
		}
		if (cp >= buf+size-1)
			break;
		*cp++ = c;
	}
	*cp = '\0';
	return (cp != buf);
}

getw_b_nl(buf, size, fp)
	char *buf;
	int size;
	FILE *fp;
{
	register char *cp;
	register int c;

	for (cp = buf; (c = getc(fp)) != EOF; ) {
		if (isspace(c)) {
			if (c == '\n') {
				*cp = '\0';
				return (0);
			}
			if (cp != buf)
				break;
			continue;
		}
		if (c == ';') {
			while ((c = getc(fp)) != EOF && c != '\n')
				;
			if (c == '\n')
				lineno++;
			if (cp != buf)
				break;
			continue;
		}
		if (cp >= buf+size-1)
			break;
		*cp++ = c;
	}
	*cp = '\0';
	return (cp != buf);
}

getnum(fp)
	FILE *fp;
{
	register int c, n;
	int seendigit = 0;
	int seendecimal = 0;

	for (n = 0; (c = getc(fp)) != EOF; ) {
		if (isspace(c)) {
			if (c == '\n')
				lineno++;
			if (seendigit)
				break;
			continue;
		}
		if (c == ';') {
			while ((c = getc(fp)) != EOF && c != '\n')
				;
			if (c == '\n')
				lineno++;
			if (seendigit)
				break;
			continue;
		}
		if (!isdigit(c)) {
			if (c != '.') {
				syslog(LOG_ERR, "line %d: expected a number",
				lineno);
				exit(1);
			}
			if (!seendigit)
				n = 1;
			if (seendecimal) {
				syslog(LOG_ERR, "line %d: expected a number",
				lineno);
				exit(1);
			} else {
				n = n * 1000 ;
				seendigit = 1;
				seendecimal = 1;
			}
			continue;
		}
		n = n * 10 + (c - '0');
		seendigit = 1;
	}
	return (n);
}

getnonblank(fp)
	FILE *fp;
{
	register int c;

	while ( (c = getc(fp)) != EOF ) {
		if (isspace(c)) {
			if (c == '\n')
				lineno++;
			continue;
		}
		if (c == ';') {
			while ((c = getc(fp)) != EOF && c != '\n')
				;
			if (c == '\n')
				lineno++;
			continue;
		}
		return(c);
	}
	syslog(LOG_ERR, "line %d: unexpected EOF", lineno);
	return (EOF);
}

/*
 * Take name and fix it according to following rules:
 * "." means root.
 * "@" means current origin.
 * "name." means no changes.
 * "name" means append origin.
 */
makename(name, origin)
	char *name, *origin;
{
	int n;

	if (origin[0] == '.')
		origin++;
	n = strlen(name);
	if (n == 1) {
		if (name[0] == '.') {
			name[0] = '\0';
			return;
		}
		if (name[0] == '@') {
			(void) strcpy(name, origin);
			return;
		}
	}
	if (n > 0) {
		if (name[n - 1] == '.')
			name[n - 1] = '\0';
		else if (origin[0] != '\0') {
			name[n] = '.';
			(void) strcpy(name + n + 1, origin);
		}
	}
}

endline(fp)
     register FILE *fp;
{
     register int c;
     while (c = getc(fp))
 	if ((c == EOF) || (c == '\n')) {
 	    (void) ungetc(c,fp);
 	    break;
 	}
}

#define MAXPORT 256
#define MAXLEN 24

getprotocol(fp)
    FILE *fp;
{
	int  k;
	char b[MAXLEN];
	struct protoent *proto;

	(void) getword(b, sizeof(b), fp);
		
	proto = getprotobyname(b);	
	if(proto == NULL) {
		k = 0;
		(void) sscanf(b,"%d",&k);
	}
	else
		k = proto->p_proto;
	return(k);
}

int getservices(n, data, fp)
    int n;
    char *data;
    FILE *fp;
{
	int j, ch;
	u_short k;
	int maxl;
	int bracket, eol;
	char b[MAXLEN];
	char bm[MAXPORT/8];
	struct servent *service;

	for (j = 0; j < MAXPORT/8; j++)
		bm[j] = 0;
	maxl = 0;
	bracket = eol = 0;
	while (1) {
		if (!getw_b_nl(b, sizeof(b), fp) && (!bracket)) {
			if (strlen(b) == 0)
				break;
			eol++;
		}	
		if ( b[0] == '(') {
			bracket++;
			continue;
		}
		if ( b[0] == ')') {
			bracket = 0;
			while ((ch = getc(fp)) != EOF && ch != '\n')
				;
			break;
		}
		service = getservbyname(b, (char *)NULL);
		if (service == NULL) {
			k=0;
			(void) sscanf(b,"%d",&k);
			if (k == 0)
				continue;
		}
		else
			k = ntohs((u_short)service->s_port);
		if ((k < MAXPORT) && (k)) {
			bm[k/8] |= (0x80>>(k%8));
			if (k > maxl)
				maxl=k;
		}
		else
			syslog(LOG_WARNING,"port no.(%d) too big\n",k);
		if(eol)
			break;
	}
	maxl = maxl/8+1;
	bcopy(bm, data+n, maxl);
	return(maxl+n);
}
