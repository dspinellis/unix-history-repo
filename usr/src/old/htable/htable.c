#ifndef lint
static char sccsid[] = "@(#)htable.c	4.2 (Berkeley) %G%";
#endif

/*
 * htable - convert NIC host table into a UNIX format.
 * NIC format is described in RFC 810, 1 March 1982.
 */
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include "htable.h"		/* includes <sys/types.h> */

#include <netinet/in.h>

FILE	*hf;			/* hosts file */
FILE	*gf;			/* gateways file */
FILE	*nf;			/* networks file */

main(argc, argv)
	int argc;
	char *argv[];
{
	if (argc > 2) {
		fprintf(stderr, "usage: %s [ input-file ]\n",
			argv[0]);
		exit(1);
	}
	infile = "(stdin)";
	if (argc == 2) {
		infile = argv[1];
		if (freopen(infile, "r", stdin) == NULL) {
			perror(infile);
			exit(1);
		}
	}
	hf = fopen("hosts", "w");
	if (hf == NULL) {
		perror("hosts");
		exit(1);
	}
	copylocal(hf, "localhosts");
	gf = fopen("gateways", "w");
	if (gf == NULL) {
		perror("gateways");
		exit(1);
	}
	copylocal(gf, "localgateways");
	nf = fopen("networks", "w");
	if (nf == NULL) {
		perror("networks");
		exit(1);
	}
	copylocal(nf, "localnetworks");
	exit(yyparse());
}

struct name *
newname(str)
	char *str;
{
	char *p;
	struct name *nm;

	p = malloc(strlen(str) + 1);
	strcpy(p, str);
	nm = (struct name *)malloc(sizeof (struct name));
	nm->name_val = p;
	nm->name_link = NONAME;
	return (nm);
}

char *
lower(str)
	char *str;
{
	register char *cp = str;

	while (*cp) {
		if (isupper(*cp))
			*cp = tolower(*cp);
		cp++;
	}
	return (str);
}

do_entry(keyword, addrlist, namelist, cputype, opsys, protos)
	int keyword;
	struct addr *addrlist;
	struct name *namelist, *cputype, *opsys, *protos;
{
	register struct addr *al, *al2;
	register struct name *nl;

	switch (keyword) {

	case KW_NET:
		nl = namelist;
		if (nl == NONAME) {
			fprintf(stderr, "htable: net");
			putnet(stderr, addrlist->addr_val);
			fprintf(stderr, " missing names.\n");
			break;
		}
		fprintf(nf, "%-16.16s", lower(nl->name_val));
		al2 = addrlist;
		while (al = al2) {
			char *cp;

			putnet(nf, al->addr_val);
			cp = "\t%s";
			while (nl = nl->name_link) {
				fprintf(nf, cp, lower(nl->name_val));
				cp = " %s";
			}
			putc('\n', nf);
			al2 = al->addr_link;
			free((char *)al);
		}
		goto alreadyfree;

	case KW_GATEWAY:
		for (al = addrlist; al; al = al->addr_link) {
			register int net = inet_netof(al->addr_val);

			/* suppress duplicates -- not optimal */
			if (checkgateway(net))
				break;
			fprintf(gf, "net ");
			putnet(gf, net);
			/* this is a kludge */
			fprintf(gf, " destination %s metric 1 passive\n",
				lower(namelist->name_val));
			putaddr(hf, al->addr_val);
			fprintf(hf, "%s\t# gateway\n",
				lower(namelist->name_val));
			savegateway(net);
		}
		break;

	case KW_HOST:
		al2 = addrlist;
		while (al = al2) {
			if (inet_netof(al->addr_val) != LOCALNET) {
				char *cp;

				putaddr(hf, al->addr_val);
				cp = "%s";
				for (nl = namelist; nl; nl = nl->name_link) {
					fprintf(hf, cp, lower(nl->name_val));
					cp = " %s";
				}
				putc('\n', hf);
			}
			al2 = al->addr_link;
			free((char *)al);
		}
		goto alreadyfree;

	default:
		fprintf(stderr, "Unknown keyword: %d.\n", keyword);
	}
	al2 = addrlist;
	while (al = al2)
		al2 = al->addr_link, free((char *)al);
alreadyfree:
	freenames(namelist);
	freenames(protos);
}

copylocal(f, filename)
	FILE *f;
	char *filename;
{
	register FILE *lhf;
	register cc;
	char buf[BUFSIZ];
	extern int errno;

	lhf = fopen(filename, "r");
	if (lhf == NULL) {
		if (errno != ENOENT) {
			perror(filename);
			exit(1);
		}
		fprintf(stderr, "Warning, no %s file.\n", filename);
		return;
	}
	while (cc = fread(buf, 1, sizeof(buf), lhf))
		fwrite(buf, 1, cc, f);
	fclose(lhf);
}

#define	UC(b)	(((int)(b))&0xff)

putnet(f, v)
	FILE *f;
	u_long v;
{
	register char *a = (char *)&v;

	if (UC(a[0]&0x80) == 0)
		fprintf(f, "%d", UC(a[0]));
	else if ((UC(a[0])&0x40) == 0)
		fprintf(f, "%d.%d", UC(a[0]), UC(a[1]));
	else
		fprintf(f, "%d.%d.%d", UC(a[0]), UC(a[1]), UC(a[2]));
}

putaddr(f, v)
	FILE *f;
	u_long v;
{
	register char *a = (char *)&v;
	char buf[32];

	sprintf(buf,"%d.%d.%d.%d", UC(a[0]), UC(a[1]), UC(a[2]), UC(a[3]));
	fprintf(f, "%-16.16s", buf);
}

freenames(list)
	struct name *list;
{
	register struct name *nl, *nl2;

	nl2 = list;
	while (nl = nl2) {
		nl2 = nl->name_link;
		free(nl->name_val);
		free((char *)nl);
	}
}
struct gateway {
	struct	gateway *g_link;
	int	g_net;
};

struct gateway *gateways = 0;

checkgateway(net)
	register int net;
{
	register struct gateway *gp;

	for (gp = gateways; gp; gp = gp->g_link)
		if (gp->g_net == net)
			return (1);
	return (0);
}

savegateway(net)
	int net;
{
	register struct gateway *gp;

	gp = (struct gateway *)malloc(sizeof (struct gateway));
	if (gp == 0) {
		fprintf(stderr, "htable: out of memory\n");
		exit(1);
	}
	gp->g_link = gateways;
	gp->g_net = net;
	gateways = gp;
}
	
