#ifndef lint
static	char *sccsid = "@(#)arp.c	1.2 (Berkeley) %G%";
#endif

/*
 * arp - display, set, and delete arp table entries
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <netdb.h>
#include <nlist.h>
#include <net/if.h>
#include <netinet/if_ether.h>

extern int errno;

main(argc, argv)
	char **argv;
{
	if (argc >= 2 && strcmp(argv[1], "-a") == 0) {
		char *kernel = "/vmunix", *mem = "/dev/kmem";

		if (argc >= 3)
			kernel = argv[2];
		if (argc >= 4)
			mem = argv[3];
		dump(kernel, mem);
		exit(0);
	}
	if (argc == 2) {
		get(argv[1]);
		exit(0);
	}
	if (argc >= 4 && strcmp(argv[1], "-s") == 0) {
		set(argc-2, &argv[2]);
		exit(0);
	}
	if (argc == 3 && strcmp(argv[1], "-d") == 0) {
		delete(argv[2]);
		exit(0);
	}
	if (argc == 3 && strcmp(argv[1], "-f") == 0) {
		file(argv[2]);
		exit(0);
	}
	usage();
	exit(1);
}

/*
 * Process a file to set standard arp entries
 */
file(name)
	char *name;
{
	FILE *fp;
	int i;
	char line[100], arg[4][50], *args[4];

	if ((fp = fopen(name, "r")) == NULL) {
		fprintf(stderr, "arp: cannot open %s\n", name);
		exit(1);
	}
	args[0] = &arg[0][0];
	args[1] = &arg[1][0];
	args[2] = &arg[2][0];
	args[3] = &arg[3][0];
	while(fgets(line, 100, fp) != NULL) {
		i = sscanf(line, "%s %s %s %s", arg[0], arg[1], arg[2], arg[3]);
		if (i < 2) {
			fprintf(stderr, "arp: bad line: %s\n", line);
			continue;
		}
		set(i, args);
	}
	fclose(fp);
}

/*
 * Set an individual arp entry 
 */
set(argc, argv)
	char **argv;
{
	struct arpreq ar;
	struct hostent *hp;
	struct sockaddr_in *sin;
	u_char *ea;
	int s;
	char *host = argv[0], *eaddr = argv[1];

	argc -= 2;
	argv += 2;
	hp = gethostbyname(host);
	if (hp == NULL) {
		fprintf(stderr, "arp: %s: unknown host\n", host);
		return (1);
	}
	bzero((caddr_t)&ar, sizeof ar);
	ar.arp_pa.sa_family = AF_INET;
	sin = (struct sockaddr_in *)&ar.arp_pa;
	sin->sin_addr = *(struct in_addr *)hp->h_addr;
	ea = (u_char *)ar.arp_ha.sa_data;
	if (ether_aton(eaddr, ea))
		return;
	ar.arp_flags = ATF_PERM;
	while(argc-- > 0) {
		if (strncmp(argv[0], "temp", 4) == 0)
			ar.arp_flags &= ~ATF_PERM;
		if (strncmp(argv[0], "pub", 3) == 0)
			ar.arp_flags |= ATF_PUBL;
		argv++;
	}
	
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
                perror("arp: socket");
                exit(1);
        }
	if (ioctl(s, SIOCSARP, (caddr_t)&ar) < 0) {
		perror(host);
		exit(1);
	}
	close(s);
}


/*
 * Display an individual arp entry
 */
get(host)
	char *host;
{
	struct arpreq ar;
	struct hostent *hp;
	struct sockaddr_in *sin;
	u_char *ea;
	int s;

	hp = gethostbyname(host);
	if (hp == NULL) {
		fprintf(stderr, "arp: %s: unknown host\n", host);
		exit(1);
	}
	bzero((caddr_t)&ar, sizeof ar);
	ar.arp_pa.sa_family = AF_INET;
	sin = (struct sockaddr_in *)&ar.arp_pa;
	sin->sin_addr = *(struct in_addr *)hp->h_addr;
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
                perror("arp: socket");
                exit(1);
        }
	if (ioctl(s, SIOCGARP, (caddr_t)&ar) < 0) {
		if (errno == ENXIO)
			printf("%s (%s) -- no entry\n",
			    host, inet_ntoa(sin->sin_addr));
		else
			perror("SIOCGARP");
		exit(1);
	}
	close(s);
	ea = (u_char *)ar.arp_ha.sa_data;
	printf("%s (%s) at ", host, inet_ntoa(sin->sin_addr));
	if (ar.arp_flags & ATF_COM)
		ether_print(ea);
	else
		printf("(incomplete)");
	if (!(ar.arp_flags & ATF_PERM)) printf(" temporary");
	if (ar.arp_flags & ATF_PUBL) printf(" published");
	printf("\n");
}

/*
 * Delete an arp entry 
 */
delete(host)
	char *host;
{
	struct arpreq ar;
	struct hostent *hp;
	struct sockaddr_in *sin;
	int s;

	hp = gethostbyname(host);
	if (hp == NULL) {
		fprintf(stderr, "arp: %s: unknown host\n", host);
		exit(1);
	}
	bzero((caddr_t)&ar, sizeof ar);
	ar.arp_pa.sa_family = AF_INET;
	sin = (struct sockaddr_in *)&ar.arp_pa;
	sin->sin_addr = *(struct in_addr *)hp->h_addr;
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
                perror("arp: socket");
                exit(1);
        }
	if (ioctl(s, SIOCDARP, (caddr_t)&ar) < 0) {
		if (errno == ENXIO)
			printf("%s (%s) -- no entry\n",
			    host, inet_ntoa(sin->sin_addr));
		else
			perror("SIOCDARP");
		exit(1);
	}
	close(s);
	printf("%s (%s) deleted\n", host, inet_ntoa(sin->sin_addr));
}

struct nlist nl[] = {
#define	X_ARPTAB	0
	{ "_arptab" },
#define	X_ARPTAB_SIZE	1
	{ "_arptab_size" },
	{ "" },
};

/*
 * Dump the entire arp table
 */
dump(kernel, mem)
	char *kernel, *mem;
{
	int mf, arptab_size, sz;
	struct arptab *at;
	struct hostent *hp;
	char *host;

	nlist(kernel, nl);
	if(nl[X_ARPTAB_SIZE].n_type == 0) {
		fprintf(stderr, "arp: %s: bad namelist\n", kernel);
		exit(1);
	}
	mf = open(mem, 0);
	if(mf < 0) {
		fprintf(fprintf, "arp: cannot open %s\n", mem);
		exit(1);
	}
	lseek(mf, (long)nl[X_ARPTAB_SIZE].n_value, 0);
	read(mf, &arptab_size, sizeof arptab_size);
	if (arptab_size <=0 || arptab_size > 1000) {
		fprintf(stderr, "arp: %s: namelist wrong\n", kernel);
		exit(1);
	}
	sz = arptab_size * sizeof (struct arptab);
	at = (struct arptab *)malloc(sz);
	if (at == NULL) {
		fprintf(stderr, "arp: can't get memory for arptab\n");
		exit(1);
	}
	lseek(mf, (long)nl[X_ARPTAB].n_value, 0);
	if (read(mf, (char *)at, sz) != sz) {
		perror("arp: error reading arptab");
		exit(1);
	}
	close(mf);
	for (; arptab_size-- > 0; at++) {
		if (at->at_iaddr.s_addr == 0 || at->at_flags == 0)
			continue;
		hp = gethostbyaddr((caddr_t)&at->at_iaddr, sizeof at->at_iaddr,
			AF_INET);
		if (hp)
			host = hp->h_name;
		else
			host = "?";
		printf("%s (%s) at ", host, inet_ntoa(at->at_iaddr));
		if (at->at_flags & ATF_COM)
			ether_print(at->at_enaddr);
		else
			printf("(incomplete)");
		if (!(at->at_flags & ATF_PERM)) printf(" temporary");
		if (at->at_flags & ATF_PUBL) printf(" published");
		printf("\n");
	}
}

ether_print(cp)
	u_char *cp;
{
	printf("%x:%x:%x:%x:%x:%x", cp[0], cp[1], cp[2], cp[3], cp[4], cp[5]);
}

ether_aton(a, n)
	char *a;
	u_char *n;
{
	int i, o[6];

	i = sscanf(a, "%x:%x:%x:%x:%x:%x", &o[0], &o[1], &o[2],
					   &o[3], &o[4], &o[5]);
	if (i != 6) {
		fprintf(stderr, "arp: invalid Ethernet address '%s'\n", a);
		return (1);
	}
	for (i=0; i<6; i++)
		n[i] = o[i];
	return (0);
}

usage()
{
	printf("Usage: arp hostname\n");
	printf("       arp -a [/vmunix] [/dev/kmem]\n");
	printf("       arp -d hostname\n");
	printf("       arp -s hostname ether_addr [temp] [pub]\n");
	printf("       arp -f filename\n");
}
