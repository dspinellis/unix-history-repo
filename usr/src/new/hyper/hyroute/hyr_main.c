/*
 * Hyperchannel routing program
 *
 * Copyright (c) 1983, Tektronix Inc.
 * All Rights Reserved
 *
 */

static char rcsid[] = "$Header: hyr_main.c,v 2.3 84/05/04 12:15:59 steveg Exp $$Locker:  $";

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <net/if.h>

#include <vaxif/if_hy.h>
#include <ctype.h>
#define MAIN
#include "hyr_sym.h"

struct hy_route hy_route;
struct hy_route ker_hy_route;
int comp_flag = 0;
int set_flag = 0;
int print_flag = 0;
int dump_flag = 0;
int debug_flag = 0;
int lexdebug;
int lex_error;
int maxgate = 0;
char *progname = "hyroute";
char *devname = "hy0";

/*
 * hash a hyperchannel address into the table
 * return NULL if table is full or entry not found and adding a new one
 */
struct hyr_hash *
rhash(key, new, r)
	u_long key;
	int new;
	register struct hy_route *r;
{
	register struct hyr_hash *rh;
	register struct hyr_hash *ret = NULL;
	int n = HYRHASH(key);

	if (debug_flag)
		printf("%s hashing key %6x initial %d ", new? "new": "old", key, n);
	rh = &r->hyr_hash[n];
	n = 0;
	while (rh->hyr_key != key) {
		if ((rh->hyr_flags & HYR_INUSE) == 0) {
			if (new)
				ret = rh;
			goto out;
		}
		if (n++ > HYRSIZE) {
			goto out;
		}
		if (++rh >= &r->hyr_hash[HYRSIZE]) {
			rh = &r->hyr_hash[0];
			if (debug_flag) printf("|");
		}
		if (debug_flag) printf(".");
	}
	ret = rh;
out:
	if (ret == NULL) {
		if (new) {
			fprintf(stderr, "%s: %s add_gates, hash table full\n", progname, devname);
			exit(1);
		}
	}
	if (debug_flag) {
		if (ret == NULL)
			printf(" returning NULL\n");
		else
			printf(" returning %d\n", ret - &r->hyr_hash[0]);
	}
	return(ret);
}

/*
 * add a direct entry to the hash table using the specified key,
 * destination, control and access fields, and loopback flags.
 */
add_direct(key, dst, ctl, access, flags, r)
	u_long key;
	unsigned dst;
	unsigned ctl;
	unsigned access;
	unsigned flags;
	register struct hy_route *r;
{
	register struct hyr_hash *kh = rhash(key, 1, r);

	if ((kh->hyr_flags & HYR_INUSE) == 0) {
		kh->hyr_flags = (HYR_INUSE | HYR_DIR);
		kh->hyr_key = key;
		kh->hyr_dst = dst;
		kh->hyr_ctl = ctl;
		kh->hyr_access = access;
		if (flags & HS_LOOP)
			kh->hyr_flags |= HYR_LOOP;
		if (flags & HS_RLOOP)
			kh->hyr_flags |= HYR_RLOOP;
		return;
	}
	fprintf(stderr, "%s: %s add_direct, hash table full\n", progname, devname);
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
 * possible.
 */
add_gates(key, numgates, gates, r)
	u_long key;
	unsigned numgates;
	unsigned gates[256];
	register struct hy_route *r;
{
	register struct hyr_hash *kh = rhash(key, 1, r);
	register struct hyr_hash *rh;
	int i, j;

	for (i = 0; i < numgates; i++) {
		rh = rhash(gates[i], 1, r);
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
				return;
			}
		}
	skipit:
		;
	}
	/*
	 * didn't find anything, if there is room add a new entry
	 */
	if (numgates + maxgate > 256) {
		fprintf(stderr, "%s: %s add_gates, gateway table full\n", progname, devname);
		exit(1);
	}
	kh->hyr_flags = (HYR_INUSE | HYR_GATE);
	kh->hyr_key = key;
	kh->hyr_pgate = maxgate;
	kh->hyr_egate = maxgate + numgates - 1;
	kh->hyr_nextgate = maxgate;
	for (i = 0; i < numgates; i++, maxgate++)
		r->hyr_gateway[maxgate] = gates[i];
}

/*
 * set the kernel table
 */
settable(r)
	struct hy_route *r;
{
	int s;
	struct hyrsetget sg;

	sg.hyrsg_ptr = r;
	sg.hyrsg_len = sizeof(*r);
	strncpy(sg.hyrsg_name, devname, sizeof(sg.hyrsg_name));

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket create in settable");
		exit(1);
	}
	if (ioctl(s, HYSETROUTE, (char *)&sg) < 0) {
		perror("HYSETROUTE ioctl in settable");
		exit(1);
	}
	if (close(s) < 0) {
		perror("socket close in settable");
		exit(1);
	}
}

/*
 * get the kernel table
 */
gettable(r)
	struct hy_route *r;
{
	int s;
	struct hyrsetget sg;

	sg.hyrsg_ptr = r;
	sg.hyrsg_len = sizeof(*r);
	strncpy(sg.hyrsg_name, devname, sizeof(sg.hyrsg_name));

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket create in gettable");
		exit(1);
	}
	if (ioctl(s, HYGETROUTE, (char *)&sg) < 0) {
		perror("HYGETROUTE ioctl in gettable");
		exit(1);
	}
	if (close(s) < 0) {
		perror("socket close in gettable");
		exit(1);
	}
}


/*
 * print a somewhat readable version of the routine table
 * that the kernel uses (mostly for debugging)
 */
print_table(r)
	register struct hy_route *r;
{
	register struct hyr_hash *rh;
	register int i;
	extern char *ctime();

	if (r->hyr_lasttime != 0)
		printf("table set time: %s", ctime(&r->hyr_lasttime));
	else
		printf("time not set\n");

	for (i = 0; i < HYRSIZE; i++) {
		rh = &r->hyr_hash[i];
		if (rh->hyr_flags & HYR_INUSE) {
			printf("hash %d key %06x flags %x\n", i, rh->hyr_key, rh->hyr_flags);
			if (rh->hyr_flags & HYR_DIR)
				printf("\tdst %04x ctl %04x access %04x",
					ntohs(rh->hyr_dst),
					ntohs(rh->hyr_ctl),
					ntohs(rh->hyr_access));
			else if (rh->hyr_flags & HYR_GATE)
				printf("\tpgate %d egate %d nextgate %d",
					rh->hyr_pgate,
					rh->hyr_egate,
					rh->hyr_nextgate);
			if (rh->hyr_flags & HYR_LOOP)
				printf(" LOOP");
			if (rh->hyr_flags & HYR_RLOOP)
				printf(" REMLOOP");
			printf("\n");
		}
	}

	for (i = 0; i < 256; i++) { 
		printf("gate[%d] = %d\n", i, r->hyr_gateway[i]);
		if (r->hyr_gateway[i] == 0 && r->hyr_gateway[i+1] == 0)
			break;
	}
}

/*
 * comnpare two routing tables tom insure that they are the same
 */
compare_table(r1, r2)
	register struct hy_route *r1, *r2;
{
	register struct hyr_hash *rh1, *rh2;
	register int i;
	int ndiffs = 0;

	for (i = 0; i < HYRSIZE; i++) {
		rh1 = &r1->hyr_hash[i];
		rh2 = &r2->hyr_hash[i];
		if (rh1->hyr_flags != rh2->hyr_flags) {
			fprintf(stderr, "%s: hash entry %d - flags differ (%x vs %x)\n", progname, i, rh1->hyr_flags, rh2->hyr_flags);
			ndiffs++;
		}
		if ((rh1->hyr_flags & HYR_INUSE) && (rh1->hyr_flags & HYR_DIR)) {
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
		}
		if ((rh1->hyr_flags & HYR_INUSE) && (rh1->hyr_flags & HYR_GATE)) {
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
		progname = "hyroute";

	argc--; argv++;
	while (argc) {
		if (argv[0][0] == '-' && argv[0][1] != '\0') {
			cp = &argv[0][0];
			switch(*++cp) {

			case 's':		/* set the kernel table */
				set_flag++;	
				break;

			case 'd':		/* print the kernel table */
				dump_flag++;
				break;

			case 'p':		/* print symbol table */
				print_flag++;
				break;

			case 'c':		/* compare with kernel table */
				comp_flag++;
				break;

			case 'l':		/* check the parser */
				lexdebug++;
				break;

			default:
				fprintf(stderr, "%s: unrecognized switch -%c\n", progname, *cp);
				exit(1);
			}
		} else if (devname == NULL) {
			devname = argv[0];
		} else if (filename == NULL) {
			filename = argv[0];
		} else {
			fprintf(stderr, "%s: extra arguments starting with %s\n", progname, argv[0]);
			exit(1);
		}
		argc--; argv++;
	}

	if (filename != NULL || set_flag || comp_flag)
		readin(filename, &hy_route);

	if (print_flag)
		symtab_print();

	if (set_flag)
		settable(&hy_route);

	if (dump_flag) {
		if (filename == NULL) {
			gettable(&ker_hy_route);
			print_table(&ker_hy_route);
		} else {
			print_table(&hy_route);
		}
	}

	if (comp_flag) {
		gettable(&ker_hy_route);
		compare_table(&hy_route, &ker_hy_route);
	}
}

/*
 * read in the control file named filename into structure r
 */
readin(filename, r)
	char *filename;
	register struct hy_route *r;
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
			perror(filename);
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

	for (s = sym_head; s != NULL; s = s->sym_next) {
		if (s->sym_flags & HS_DIR) {
			add_direct(inet_lnaof(s->sym_inaddr), s->sym_dst, s->sym_ctl, s->sym_access, s->sym_flags, r);
		} else if (s->sym_flags & HS_INDIR) {
			for (i = 0; i < s->sym_ngate; i++)
				gates[i] = inet_lnaof(s->sym_gate[i]->sym_inaddr);
			add_gates(inet_lnaof(s->sym_inaddr), s->sym_ngate, gates, r);
		}
	}
}
