static char sccsid[] = "@(#)vpac.c	1.2	(Berkeley)	%G%";

#include <stdio.h>

/*
 * Do Versatec and Varian accounting summary.
 * Currently, usage is
 *	vpac [-srW] [user] ...
 * to print the usage information for the named people.
 */

int	errs;

#define VAACCT		"/usr/adm/vaacct"
#define VASUM		"/usr/adm/va_sum"
#define VPACCT		"/usr/adm/vpacct"
#define VPSUM		"/usr/adm/vp_sum"
char	*acctfile;
char	*sumfile;

#define	VAPRICE	0.02			/* Dollars per page */
#define	VPPRICE	0.08			/* Dollars per foot of paper */
float price;

/*
 * Grossness follows:
 *  Names to be accumulated are hashed into the following
 *  table.
 */

#define	HSHSIZE	97			/* Number of hash buckets */

struct hent {
	struct	hent *h_link;		/* Forward hash link */
	char	h_name[9];		/* Name of this user */
	float	h_feetpages;		/* Feet or pages of paper */
	int	h_count;		/* Number of runs */
};

struct	hent	*hashtab[HSHSIZE];	/* Hash table proper */
struct	hent	*enter();
struct	hent	*lookup();

#define	NIL	((struct hent *) 0)	/* The big zero */

int	allflag;			/* Get stats on everybody */
int	sort;				/* Sort by cost */
int	summarize;			/* Compress accounting file */
int	reverse;			/* Reverse sort order */
int	wide;				/* wide paper (Versatec) accounting. */

int	hcount;				/* Count of hash entries */

main(argc, argv)
	char **argv;
{
	register FILE *acct;
	register char *cp;
	register int gotcha = 0;

	acctfile = VAACCT;
	sumfile = VASUM;
	price = VAPRICE;
	if (argc >= 2)
		while (--argc) {
			cp = *++argv;
			if (*cp++ == '-') {
				while (*cp) switch(*cp++) {
				case 's':
					/*
					 * Summarize and compress
					 * accounting file.
					 */
					summarize++;
					break;

				case 't':
					/*
					 * Sort by feet of typesetter film.
					 */
					sort++;
					break;

				case 'r':
					/*
					 * Reverse sorting order.
					 */
					reverse++;
					break;

				case 'W':
					/*
					 * Versatec, not Varian accounting.
					 */
					wide++;
					acctfile = VPACCT;
					sumfile = VPSUM;
					price = VPPRICE;
					break;
				default:
					fprintf(stderr, "%s?\n", *argv);
					exit(1);
				}
				continue;
			}
			ignore(enter(--cp));
			gotcha++;
		}
	allflag = gotcha == 0;

	if ((acct = fopen(acctfile, "r")) == NULL) {
		perror(acctfile);
		exit(1);
	}
	account(acct);
	fclose(acct);
	if ((acct = fopen(sumfile, "r")) != NULL) {
		account(acct);
		fclose(acct);
	}
	if (summarize)
		rewrite();
	else
		dumpit();
	exit(errs);
}

/*
 * Read the entire accounting file, accumulating statistics
 * for the users that we have in the hash table.  If allflag
 * is set, then just gather the facts on everyone.
 * Note that we must accomodate both the active and summary file
 * formats here.
 */

account(acct)
	register FILE *acct;
{
	char linebuf[BUFSIZ];
	float t, atof();
	register char *cp, *cp2;
	register struct hent *hp;
	register int ic;

	while (fgets(linebuf, BUFSIZ, acct) != NULL) {
		cp = linebuf;
		while (any(*cp, " t\t"))
			cp++;
		t = atof(cp);
		while (any(*cp, ".0123456789"))
			cp++;
		while (any(*cp, " \t"))
			cp++;
		for (cp2 = cp; !any(*cp2, " \t\n"); cp2++)
			;
		ic = atoi(cp2);
		*cp2 = '\0';
		hp = lookup(cp);
		if (hp == NIL && !allflag)
			continue;
		if (hp == NIL)
			hp = enter(cp);
		hp->h_feetpages += t;
		if (ic)
			hp->h_count += ic;
		else
			hp->h_count++;
	}
}

/*
 * Sort the hashed entries by name or footage
 * and print it all out.
 */

dumpit()
{
	struct hent **base;
	register struct hent *hp, **ap;
	register int hno, c, runs;
	float feet;
	int qucmp();

	hp = hashtab[0];
	hno = 1;
	base = (struct hent **) calloc(sizeof hp, hcount+4);
	for (ap = base, c = hcount; c--; ap++) {
		while (hp == NIL)
			hp = hashtab[hno++];
		*ap = hp;
		hp = hp->h_link;
	}
	qsort(base, hcount, sizeof hp, qucmp);
	printf(wide ? " Login    feet   runs    price\n"
		    : " Login   pages   runs    price\n");
	feet = 0.0;
	runs = 0;
	for (ap = base, c = hcount; c--; ap++) {
		hp = *ap;
		runs += hp->h_count;
		feet += hp->h_feetpages;
		printf("%-8s %7.2f %4d   $%6.2f\n", hp->h_name, hp->h_feetpages,
		    hp->h_count, hp->h_feetpages * price);
	}
	if (allflag) {
		printf("\n");
		printf("%-8s %7.2f %4d   $%6.2f\n", "total", feet, 
		    runs, feet * price);
	}
}

/*
 * Rewrite the summary file with the summary information we have accumulated.
 */

rewrite()
{
	register struct hent *hp;
	register int i;
	register FILE *acctf;

	if ((acctf = fopen(sumfile, "w")) == NULL) {
		perror(sumfile);
		errs++;
		return;
	}
	for (i = 0; i < HSHSIZE; i++) {
		hp = hashtab[i];
		while (hp != NULL) {
			fprintf(acctf, "%7.2f\t%s\t%d\n", hp->h_feetpages,
			    hp->h_name, hp->h_count);
			hp = hp->h_link;
		}
	}
	fflush(acctf);
	if (ferror(acctf)) {
		perror(sumfile);
		errs++;
	}
	fclose(acctf);
	if ((acctf = fopen(acctfile, "w")) == NULL)
		perror(acctfile);
	else
		fclose(acctf);
}

/*
 * Hashing routines.
 */

/*
 * Enter the passed name into the hash table
 * and returns the pointer allocated.
 */

struct hent *
enter(name)
	char name[];
{
	register struct hent *hp;
	register int h;

	if ((hp = lookup(name)) != NIL)
		return(hp);
	h = hash(name);
	hcount++;
	hp = (struct hent *) calloc(sizeof *hp, 1);
	strcpy(hp->h_name, name);
	hp->h_feetpages = 0.0;
	hp->h_count = 0;
	hp->h_link = hashtab[h];
	hashtab[h] = hp;
	return(hp);
}

/*
 * Lookup a name in the hash table and return a pointer
 * to it.
 */

struct hent *
lookup(name)
	char name[];
{
	register int h;
	register struct hent *hp;

	h = hash(name);
	for (hp = hashtab[h]; hp != NIL; hp = hp->h_link)
		if (strcmp(hp->h_name, name) == 0)
			return(hp);
	return(NIL);
}

/*
 * Hash the passed name and return the index in
 * the hash table to begin the search.
 */

hash(name)
	char name[];
{
	register int h;
	register char *cp;

	for (cp = name, h = 0; *cp; h = (h << 2) + *cp++)
		;
	if (h < 0)
		h = -h;
	if (h < 0)
		h = 0;
	return(h % HSHSIZE);
}

/*
 * Other stuff
 */

any(ch, str)
	char str[];
{
	register int c = ch;
	register char *cp = str;

	while (*cp)
		if (*cp++ == c)
			return(1);
	return(0);
}

/*
 * Throw away a hash pointer.
 */

ignore(p)
	struct hent *p;
{;}

/*
 * The qsort comparison routine.
 * The comparison is ascii collating order
 * or by feet of typesetter film, according to sort.
 */

qucmp(left, right)
	struct hent **left, **right;
{
	register struct hent *h1, *h2;
	register int r;

	h1 = *left;
	h2 = *right;
	if (sort)
		r = h1->h_feetpages < h2->h_feetpages ? -1 : h1->h_feetpages > h2->h_feetpages;
	else
		r = strcmp(h1->h_name, h2->h_name);
	return(reverse ? -r : r);
}
