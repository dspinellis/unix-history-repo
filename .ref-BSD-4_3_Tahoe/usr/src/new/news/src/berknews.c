/*
 * berknews - send news article via Berknet
 * 
 * Synopsis:
 *	berknews [-o] [-n newsgroup] host_net_command machine remote_rnews
 */

#ifdef SCCSID
static char *SccsId = "@(#)berknews.c	2.5	4/16/85";
#endif /* SCCSID */

#include <stdio.h>
#include <ctype.h>
#ifndef USG
#include <whoami.h>
struct utsname {
	char	Sysname[9];
	char	nodename[33];
	char	release[9];
	char	version[9];
};
#else /* USG */
#include <sys/utsname.h>
#endif /* USG */


struct network {
	char *uucpname;
	char *berkname;
} berknet[] = {
/*	UUCP Net Name	BerkNet Name
	-------------	------------	*/	
	"ucbvax",	"CSVAX",
	"populi",	"G",
	"ucbarpa",	"ARPAVAX",
	"ucbcfo-c",	"C",
	"ucbopt",	"ESVAX",
	"ucbcad",	"ucbcad",
	"ucbcory",	"Cory",
	"ucb",		"C70",
	"ucbmathstat",	"MathStat",
	"ucbonyx",	"Onyx",
	"ucbkim",	"Kim",
	"ucbcfo-a",	"A",
	"ucbcfo-b",	"B",
	"ucbcfo-d",	"D",
	"ucbcfo-e",	"E",
	"ucbcfo-f",	"F",
	"ucbingvax",	"IngVAX",
	"ucbingres",	"Ingres",
	"ucbeecs40",	"EECS40",
	"ucbvlsi",	"VLSI",
	"ucbsrc",	"SRC",
	"ucbimage",	"Image",
	'\0',		'\0'
};

char *index();
char buffer[BUFSIZ];
int linecount;

FILE *popen();
main(argc, argv)
int argc;
char **argv;
{
	FILE *out;
	char sender[BUFSIZ],newsgroup[100];
	char *punct;
	char sysn[20];
	int sysnl;
	struct utsname ubuf;

	if (argc < 4) {
		fprintf(stderr, "Too few arguments.\n");
		exit(1);
	}

#ifdef debug
	printf("%s - -m%s %s\n", argv[1], argv[2], argv[3]);
	sprintf(buffer, "cat");
#else
	sprintf(buffer, "%s - -m%s %s", argv[1], argv[2], argv[3]);
#endif
	out = popen(buffer, "w");
	uname(&ubuf);
	strcpy(sysn, ubuf.nodename);
	strcat(sysn, "!");
	sysnl = strlen(sysn);

	while (fgets(buffer, sizeof buffer, stdin)) {
		if (fromline()) {
			punct = index(buffer, '!');
			if (punct == NULL)
				printf("Bad from line: '%s'\n", buffer);
			else {
				*punct = ':';	/* berknet mail delimiter */
				if (!strncmp("From: ", buffer, 6))
					punct = &buffer[6];
				else if (!strncmp("From ",buffer,5))
					punct = &buffer[5];
				else
					punct = buffer;
				fiddle(punct);
			}
		}
		fputs(buffer, out);
	}
	pclose(out);
	exit(0);
}

fromline() {
	if (!linecount && (!strncmp("From: ", buffer, 6) || !strncmp("From ", buffer, 5)))
		return ++linecount;
	return 0;
}

/*
 * make sure the host name is a correct berknet address, since the
 * internal names are not the berknet host names.
 */
fiddle(buf)
char *buf;
{
	char uucpname[100];
	register struct network *netptr;
	char *rest;

	strcpy(uucpname, buf);
	rest = index(uucpname, ':');
	*rest++ = 0;
#ifdef debug
	printf("uucpname = '%s', buf = '%s', rest = '%s'\n", uucpname, buf, rest);
#endif
	for (netptr = &berknet[0]; strcmp(netptr->uucpname, uucpname) && netptr->uucpname; netptr++)
		;
	if (netptr->uucpname)
		sprintf(buf, "%s:%s", netptr->berkname, rest);
	else
		sprintf(buf, "UNKNOWN:%s", rest);
}
