static char *sccsid = "@(#)rmail.c	4.1 (Berkeley) 10/1/80";
/*
 * rmail: front end for mail to stack up those stupid >From ... remote from ...
 * lines and make a correct return address.  This works with the -f option
 * to /etc/delivermail so it won't work on systems without delivermail.
 * However, it ought to be easy to modify a standard /bin/mail to do the
 * same thing.
 *
 * NOTE: Rmail is SPECIFICALLY INTENDED for ERNIE COVAX because of its
 * physical position as a gateway between the uucp net and the arpanet.
 * By default, other sites will probably want /bin/rmail to be a link
 * to /bin/mail, as it was intended by BTL.  However, other than the
 * (somewhat annoying) loss of information about when the mail was
 * originally sent, rmail should work OK on other systems running uucp.
 * If you don't run uucp you don't even need any rmail.
 */

#include <stdio.h>
FILE *popen();
char *index();

#define MAILER	"/etc/delivermail"

main(argc, argv)
char **argv;
{
	FILE *out;	/* output to delivermail */
	char lbuf[512];	/* one line of the message */
	char from[512];	/* accumulated path of sender */
	char ufrom[64];	/* user on remote system */
	char sys[64];	/* a system in path */
	char junk[512];	/* scratchpad */
	char cmd[512];
	char *to, *cp;

	to = argv[1];
	if (argc != 2) {
		fprintf(stderr, "Usage: rmail user\n");
		exit(1);
	}

	for (;;) {
		fgets(lbuf, sizeof lbuf, stdin);
		if (strncmp(lbuf, "From ", 5) && strncmp(lbuf, ">From ", 6))
			break;
		/* sscanf(lbuf, "%s %s %s %s %s %s %s remote from %s", junk, ufrom, junk, junk, junk, junk, junk, sys); */
		sscanf(lbuf, "%s %s", junk, ufrom);
		cp = lbuf;
		for (;;) {
			cp = index(cp+1, 'r');
			if (cp == NULL)
				cp = "remote from somewhere";
#ifdef DEBUG
			printf("cp='%s'\n", cp);
#endif
			if (strncmp(cp, "remote from ", 12)==0)
				break;
		}
		sscanf(cp, "remote from %s", sys);
		strcat(from, sys);
		strcat(from, "!");
#ifdef DEBUG
		printf("ufrom='%s', sys='%s', from now '%s'\n", ufrom, sys, from);
#endif
	}
	strcat(from, ufrom);

	sprintf(cmd, "%s -r%s %s", MAILER, from, to);
#ifdef DEBUG
	printf("cmd='%s'\n", cmd);
#endif
	out = popen(cmd, "w");
	fputs(lbuf, out);
	while (fgets(lbuf, sizeof lbuf, stdin))
		fputs(lbuf, out);
	pclose(out);
}

/*
 * Return the ptr in sp at which the character c appears;
 * NULL if not found
 */

char *
index(sp, c)
register char *sp, c;
{
	do {
		if (*sp == c)
			return(sp);
	} while (*sp++);
	return(NULL);
}
