#include <stdio.h>
#include <sys/types.h>
#include <net/hyroute.h>
#include <sys/socket.h>
#include <net/in.h>
#include <net/in_systm.h>
#include <ctype.h>
#define MAIN
#include "hyr_sym.h"

struct hyroute hy_route;
struct hyroute ker_hy_route;
int cflag = 0;
int sflag = 0;
int pflag = 0;
int dflag = 0;
int lexdebug;
int lex_error;
int maxgate = 0;
char *progname = "hy-route";
static char sccsid[] = "@(#)hyr_main.c	2.1 Hyperchannel Routing Daemon 82/11/29";

/*
 * Return the local network address portion of an
 * internet address; handles class a/b/c network
 * number formats.
 */
long
in_lnaof(in)
	struct in_addr in;
{
#if vax || pdp11
#define	IN_LNAOF(in) \
	(((in).s_addr&IN_CLASSA) == 0 ? (in).s_addr&IN_CLASSA_LNA : \
		((in).s_addr&IN_CLASSB) == 0 ? (in).s_addr&IN_CLASSB_LNA : \
			(in).s_addr&IN_CLASSC_LNA)
	return ((int)htonl((u_long)IN_LNAOF(in)));
#else
	return (IN_LNAOF(in));
#endif
}

/*
 * hash a hyperchannel address into the table
 * return NULL if table is full or entry not found and adding a new one
 */
struct hy_hash *
rhash(key, new, r)
	unsigned key;
	int new;
	register struct hyroute *r;
{
	register struct hy_hash *rh = &r->hyr_hash[HYRHASH(key)];
	int n = 0;

	while (rh->hyr_key != key) {
		if ((rh->hyr_flags & HYR_INUSE) == 0)
			return(new ? rh : NULL);
		if (n++ > HYRSIZE) {
			return(NULL);
		} else {
			if (rh++ >= &r->hyr_hash[HYRSIZE])
				rh = &r->hyr_hash[0];
		}
	}
	return(rh);
}

/*
 * add a direct entry to the hash table using the specified key,
 * destination, control and access fields, return 0 if successful
 */
int
add_direct(key, dst, ctl, access, r)
	unsigned key;
	unsigned dst;
	unsigned ctl;
	unsigned access;
	register struct hyroute *r;
{
	register struct hy_hash *kh = rhash(key, 1, r);

	if ((kh->hyr_flags & HYR_INUSE) == 0) {
		kh->hyr_flags = (HYR_INUSE | HYR_DIR);
		kh->hyr_key = key;
		kh->hyr_dst = dst;
		kh->hyr_ctl = ctl;
		kh->hyr_access = access;
		return(0);
	}
	return(1);
}

/*
 * compare function for the qsort in add_gates, see below
 */
int
compare_gates(a, b)
	unsigned *a, *b;
{
	if (*a < *b)
		return(-1);
	else if (*a > *b)
		return(1);
	else
		return(0);
}

/*
 * add a gatewayed entry to the hash table using the sicified array of
 * gateway keys.  reuse space so as to make the gateway table small as
 * possible.  return 0 if successful
 */
int
add_gates(key, numgates, gates, r)
	unsigned key;
	unsigned numgates;
	unsigned gates[256];
	register struct hyroute *r;
{
	register struct hy_hash *kh = rhash(key, 1, r);
	register struct hy_hash *rh;
	int i, j;

	for (i = 0; i < numgates; i++) {
		rh = rhash(gates[i], 1, r);
		if (rh == NULL)
			return(1);
		gates[i] = rh - &r->hyr_hash[0];
	}
	qsort(gates, numgates, sizeof(unsigned), compare_gates);
	/*
	 * loop through all existing hash table entries to find one that
	 * matches the currently requested list
	 */
	for (rh = &r->hyr_hash[0]; rh < &r->hyr_hash[HYRSIZE]; rh++) {
		if (rh->hyr_flags & HYR_GATE) {
			if ((rh->hyr_egate - rh->hyr_pgate + 1) == numgates) {
				for (i = 0, j = rh->hyr_pgate; i < numgates ; i++, j++) {
					if (gates[i] != r->hyr_gateway[j])
						goto skipit;
				}
				/*
				 * found a match, just use it
				 */
				kh->hyr_flags = (HYR_INUSE | HYR_GATE);
				kh->hyr_key = key;
				kh->hyr_pgate = rh->hyr_pgate;
				kh->hyr_egate = rh->hyr_egate;
				kh->hyr_nextgate = rh->hyr_nextgate;
				return(0);
			}
		}
	skipit:
		;
	}
	/*
	 * didn't find anything, if there is room add a new entry
	 */
	if (numgates + maxgate > 256)
		return(1);
	kh->hyr_flags = (HYR_INUSE | HYR_GATE);
	kh->hyr_key = key;
	kh->hyr_pgate = maxgate;
	kh->hyr_egate = maxgate + numgates - 1;
	kh->hyr_nextgate = maxgate;
	for (i = 0; i < numgates; i++, maxgate++)
		r->hyr_gateway[maxgate] = gates[i];
	return(0);
}
/*
 * set the kernel table
 */
settable(r)
	struct hyroute *r;
{
	int fd;

	if ((fd = open("/dev/hy", 0)) < 0) {
		perror("/dev/hy open in settable");
		exit(1);
	}
	if (ioctl(fd, HYSETROUTE, (char *)r) < 0) {
		perror("/dev/hy HYSETROUTE ioctl in settable");
		exit(1);
	}
	if (close(fd) < 0) {
		perror("/dev/hy close in settable");
		exit(1);
	}
}

/*
 * get the kernel table
 */
gettable(r)
	struct hyroute *r;
{
	int fd;

	if ((fd = open("/dev/hy", 0)) < 0) {
		perror("/dev/hy open in gettable");
		exit(1);
	}
	if (ioctl(fd, HYGETROUTE, (char *)r) < 0) {
		perror("/dev/hy HYGETROUTE ioctl in gettable");
		exit(1);
	}
	if (close(fd) < 0) {
		perror("/dev/hy close in gettable");
		exit(1);
	}
}


/*
 * print a somewhat readable version of the routine table
 * that the kernel uses (mostly for debugging)
 */
print_table(r)
	register struct hyroute *r;
{
	register struct hy_hash *rh;
	register int i;
	extern char *ctime();

	if (r->hyr_lasttime)
		printf("table set time: %s", ctime(&r->hyr_lasttime));
	else
		printf("time not set\n");

	for (i = 0; i < HYRSIZE; i++) {
		rh = &r->hyr_hash[i];
		if (rh->hyr_flags & HYR_INUSE) {
			printf("hash %d key %04x flags %x\n", i, rh->hyr_key, rh->hyr_flags);
			if (rh->hyr_flags & HYR_DIR)
				printf("\tdst %04x ctl %04x access %04x\n",
					ntohs(rh->hyr_dst),
					ntohs(rh->hyr_ctl),
					ntohs(rh->hyr_access));
			else if (rh->hyr_flags & HYR_GATE)
				printf("\tpgate %d egate %d nextgate %d\n",
					rh->hyr_pgate,
					rh->hyr_egate,
					rh->hyr_nextgate);
		}
	}

	for (i = 0; i < 256; i++) { 
		printf("gate[%d] = %d\n", i, r->hyr_gateway[i]);
		if (r->hyr_gateway[i] == 0 && r->hyr_gateway[i+1] == 0)
			break;
	}
}

/*
 * comnpare teo routing tables tom insure that they are the same
 */
compare_table(r1, r2)
	register struct hyroute *r1, *r2;
{
	register struct hy_hash *rh1, *rh2;
	register int i;
	int ndiffs = 0;

	for (i = 0; i < HYRSIZE; i++) {
		rh1 = &r1->hyr_hash[i];
		rh2 = &r2->hyr_hash[i];
		if (rh1->hyr_flags != rh2->hyr_flags) {
			fprintf(stderr, "%s: hash entry %d - flags differ (%x vs %x)\n", progname, i, rh1->hyr_flags, rh2->hyr_flags);
			ndiffs++;
		} else if ((rh1->hyr_flags & HYR_INUSE) && (rh1->hyr_flags & HYR_DIR)) {
			if (rh1->hyr_dst != rh1->hyr_dst ||
			    rh1->hyr_ctl != rh1->hyr_ctl ||
			    rh1->hyr_access != rh1->hyr_access) {
				fprintf(stderr, "%s: direct hash entry %d - fields differ\n", progname, i);
				fprintf(stderr, "\tdst: %04x vs %04x\tctl: %04x vs %04x\taccess: %04x vs %04x\n",
					ntohs(rh1->hyr_dst), ntohs(rh2->hyr_dst),
					ntohs(rh1->hyr_ctl), ntohs(rh2->hyr_ctl),
					ntohs(rh1->hyr_access), ntohs(rh2->hyr_access));
				ndiffs++;
			}
		} else if ((rh1->hyr_flags & HYR_INUSE) && (rh1->hyr_flags & HYR_GATE)) {
			if (rh1->hyr_pgate != rh1->hyr_pgate ||
			    rh1->hyr_egate != rh1->hyr_egate ||
			    rh1->hyr_nextgate < rh1->hyr_pgate ||
			    rh1->hyr_nextgate > rh1->hyr_egate ||
			    rh2->hyr_nextgate < rh2->hyr_pgate ||
			    rh2->hyr_nextgate > rh2->hyr_egate) {
				fprintf(stderr, "%s: direct hash entry %d - fields differ\n", progname, i);
				fprintf(stderr, "\tpgate: %04x vs %04x\tegate: %04x vs %04x\tnextgate: %04x vs %04x\n",
					rh1->hyr_pgate, rh2->hyr_pgate,
					rh1->hyr_egate, rh2->hyr_egate,
					rh1->hyr_nextgate, rh2->hyr_nextgate);
				ndiffs++;
			}
		}
	}
	for (i = 0; i < 256; i++) {
		if (r1->hyr_gateway[i] != r2->hyr_gateway[i]) {
			fprintf(stderr, "%s: gate[%d] = %d v2 %d\n", progname, i,
				r1->hyr_gateway[i], r2->hyr_gateway[i]);
		}
	}
	return(ndiffs);
}

main(argc, argv)
	int argc;
	char *argv[];
{
	char *filename = NULL;		/* input file name (default stdin) */
	char *cp;

	if (argc)
		progname = argv[0];
	else
		progname = "hy-route";

	argc--; argv++;
	while (argc) {
		if (argv[0][0] == '-' && argv[0][1] != '\0') {
			cp = &argv[0][0];
			switch(*++cp) {

			case 's':		/* set the kernel table */
				sflag++;	
				break;

			case 'd':		/* dump the kernel table */
				dflag++;
				break;

			case 'p':		/* print symbol table */
				pflag++;
				break;

			case 'c':		/* compare with kernel table */
				cflag++;
				break;

			case 'l':		/* check the parser */
				lexdebug++;
				break;

			default:
				fprintf(stderr, "%s: unrecognized switch -%c\n", progname, *cp);
				exit(1);
			}
		} else if (filename == NULL) {
			filename = argv[0];
		} else {
			fprintf(stderr, "%s: extra arguments starting with %s\n", progname, argv[0]);
			exit(1);
		}
		argc--; argv++;
	}

	if (filename != NULL || sflag || cflag)
		readin(filename, &hy_route);

	if (pflag)
		symtab_print();

	if (sflag)
		settable(&hy_route);

	if (dflag || cflag)
		gettable(&ker_hy_route);

	if (dflag)
		print_table(filename == NULL ? &ker_hy_route : &hy_route);

	if (cflag)
		compare_table(&hy_route, &ker_hy_route);
}

/*
 * read in the control file named filename into structure r
 */
readin(filename, r)
	char *filename;
	register struct hyroute *r;
{
	register char *cp;
	register struct sym *s;
	unsigned gates[256];
	char buf[512];
	unsigned i;
	extern FILE *yyin;

	if (filename == NULL || *filename == '\0' || strcmp(filename, "-") == 0) {
		yyin = stdin;
	} else {
		yyin = fopen(filename, "r");
		if (yyin == NULL) {
			perror("/etc/hy-route");
			exit(1);
		}
	}

	maxgate = 0;
	bzero((char *)r, sizeof(*r));

	lex_error = 0;
	yylex();
	if (lex_error) {
		fprintf(stderr, "hyroute: syntax errors, aborting operation\n");
		exit(1);
	}

	for (s = sym_head; s != NULL; s = s->s_next) {
		if (s->s_flags & HS_DIR) {
			add_direct(in_lnaof(s->s_fulladdr), s->s_dst, s->s_ctl, s->s_access, r);
		} else if (s->s_flags & HS_INDIR) {
			for (i = 0; i < s->s_ngate; i++)
				gates[i] = in_lnaof(s->s_gate[i]->s_fulladdr);
			add_gates(in_lnaof(s->s_fulladdr), s->s_ngate, gates, r);
		}
	}
}
