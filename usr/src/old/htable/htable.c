#ifndef lint
static char sccsid[] = "@(#)htable.c	4.1 (Berkeley) %G%";
#endif

/*
 * htable - convert NIC host table into a UNIX format.
 * NIC format is described in RFC 810, 1 March 1982.
 */
#include <stdio.h>
#include <ctype.h>
#include "htable.h"

FILE *hf;
FILE *gf;

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
	copylocal();
#ifdef GATEWAYS
	hf = fopen("gateways", "w");
	if (hf == NULL) {
		perror("gateways");
		exit(1);
	}
#endif
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
	nm = alloc_name();
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
	register struct name *nl, *nl2;
	register flag;

	switch (keyword) {

	case KW_NET:
		break;

	case KW_GATEWAY:
		break;

	case KW_HOST:
		for (al = addrlist; al; al = al2) {
			if (net(al->addr_val) != LOCALNET) {
				fprintf(hf, "%d.%d.%d.%d\t",
					net(al->addr_val), host(al->addr_val),
					lhost(al->addr_val), imp(al->addr_val));
				for (nl = namelist; nl; nl = nl->name_link)
					fprintf(hf, " %s", lower(nl->name_val));
				putc('\n', hf);
			}
			al2 = al->addr_link;
			free_addr(al);
		}
		break;

	default:
		fprintf(stderr, "Unknown keyword: %d.\n", keyword);
	}
	for (nl = namelist; nl; nl = nl2) {
		nl2 = nl->name_link;
		free_name(nl);
	}
	for (nl = protos; nl; nl = nl2) {
		nl2 = nl->name_link;
		free_name(nl);
	}
}

copylocal()
{
	register FILE *lhf;
	register cc;
	char buf[BUFSIZ];

	lhf = fopen("localhosts", "r");
	if (lhf == NULL) {
		perror("localhosts");
		fprintf(stderr, "(continuing)\n");
		return;
	}
	while (cc = fread(buf, 1, sizeof(buf), lhf))
		fwrite(buf, 1, cc, hf);
	fclose(lhf);
}
