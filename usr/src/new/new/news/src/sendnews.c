/*
 * sendnews - send news article by mail.
 */

static char *SccsId = "@(#)sendnews.c	2.6	5/4/83";

#include <stdio.h>
#include <ctype.h>
#ifndef USG
struct utsname {
	char	Sysname[9];
	char	nodename[33];
	char	release[9];
	char	version[9];
};
#else
#include <sys/utsname.h>
#endif

#define	eq(a,b)	(strcmp(a,b) == 0)
#define	LNLEN	7			/* strlen("ucbvax!") */

char *index();
char buffer[BUFSIZ];
int linecount, oflag = 0, aflag = 0, bflag = 0, toflag = 0;

FILE *popen();
main(argc, argv)
char **argv;
{
	FILE *out;
	char sender[BUFSIZ],newsgroup[100];
	char *punct;
	char sysn[20];
	int sysnl;
	char *bnkludge;
	struct utsname ubuf;

	while (**(++argv) == '-') {
		if (*++*argv == 'o')
			oflag++;
		else if (**argv == 'a')
			aflag++;
		else if (**argv == 'b')
			bflag++;
		else if (**argv == 'n')
			strcpy(newsgroup, *(++argv));
	}
	if (aflag && bflag) {
		fprintf(stderr, "'-a' and '-b' options mutually exclusive.\n");
		exit(1);
	}

#ifdef debug
	printf("mail %s\n", *argv);
	sprintf(buffer, "cat");
#else
	sprintf(buffer, "mail %s", *argv);
#endif
	out = popen(buffer, "w");
	uname(&ubuf);
	strcpy(sysn, ubuf.nodename);
	strcat(sysn, "!");
	sysnl = strlen(sysn);
	bnkludge = "";
#ifdef BERKELEY
	/*
	 * ucbvax isn't really on the arpanet, but is berknetted to
	 * it through ingres.  Hence the following kludge.
	 */
	bnkludge = "ARPAVAX.";
#define HOSTNAME "Berkeley"
#endif
#ifndef HOSTNAME
#define HOSTNAME "OuterSpace"
#endif

	/* Standard mail prelude to make the formatters happy */
	fprintf(out, "To: %s\n", *argv);
	fprintf(out, "Subject: network news article\n");
	fprintf(out, "\n");

	while (fgets(buffer, sizeof buffer, stdin)) {
#ifdef notdef
		if (fromline()) {
				if (aflag) {
					if (!strncmp(buffer, "From ", 5)) {
						punct = &buffer[4];
						while (isspace(*punct++))
							;
						punct--;
					}
					else if (!strncmp(buffer, "From: ",6)) {
						punct = &buffer[6];
						while (isspace(*punct++))
							;
						punct--;
					}
					else
						punct = buffer;
					if (strncmp(punct, sysn, sysnl))
						printf("Bad from line: '%s'\n", buffer);
					strcpy(sender, punct+sysnl);
					sender[strlen(sender)-1] = 0;
					sprintf(punct, "%s%s@%s\n",
						bnkludge, sender, HOSTNAME);
				} else if (bflag) {
					punct = index(buffer, '!');
					if (punct == NULL)
						printf("Bad from line: '%s'\n", buffer);
					else {
						*punct = ':';	/* berknet mail delimeter */
						if (!strncmp("From: ", buffer, 6))
							punct = &buffer[6];
						else if (!strncmp("From ",buffer,5))
							punct = &buffer[5];
						else
							punct = buffer;
						fiddle(punct);
					}
				}
		}
#endif
		if (*newsgroup && ngline()) {
			if (oflag)
				sprintf(buffer, "%s\n", newsgroup);
			else
				sprintf(buffer, "Newsgroups: %s\n", newsgroup);
		}
		putc('N', out);
		fputs(buffer, out);
	}
	pclose(out);
	exit(0);
}

fromline() {
	if (oflag)
		return ++linecount == 3;
	if (!linecount && (!strncmp("From: ", buffer, 6) || !strncmp("From ", buffer, 5)))
		return ++linecount;
	return 0;
}

ngline() {
	if (oflag)
		return linecount == 2;
	if (!toflag && (!strncmp("Newsgroups: ", buffer, 12) || !strncmp("To: ",buffer, 4)))
		return ++toflag;
	return 0;
}

/*
 * make sure the host name is a correct berknet address, since the
 * internal names are not the berknet host names.
 */
fiddle(buf)
char *buf;
{
	char berkname[10];
	char uucpname[100];
	char *rest;

	strcpy(uucpname, buf);
	rest = index(uucpname, ':');
	*rest++ = 0;
#ifdef debug
	printf("uucpname='%s', buf='%s', rest='%s',...", uucpname, buf, rest);
#endif
	if (eq(uucpname, "ucbvax"))
		strcpy(berkname, "CSVAX");
	else if (eq(uucpname, "ucbcory"))
		strcpy(berkname, "Cory");
	else if (eq(uucpname, "ucbopt"))
		strcpy(berkname, "ESVAX");
		/*
		 * The uucp names from here down are guesswork.
		 * They may have to be changed later.  But we can't
		 * allow names like a, b, etc to get outside the berknet.
		 */
	else if (eq(uucpname, "ucbing70"))
		strcpy(berkname, "Ing70");
	else if (eq(uucpname, "ucbingvax"))
		strcpy(berkname, "IngVAX");
	else if (eq(uucpname, "ucbcfo_a"))
		strcpy(berkname, "A");
	else if (eq(uucpname, "ucbcfo_b"))
		strcpy(berkname, "B");
	else if (eq(uucpname, "ucbcfo_c"))
		strcpy(berkname, "C");
	else if (eq(uucpname, "ucbcfo_d"))
		strcpy(berkname, "D");
	else if (eq(uucpname, "ucbcfo_e"))
		strcpy(berkname, "E");
	else if (eq(uucpname, "ucbcfo_f"))
		strcpy(berkname, "F");
	else if (eq(uucpname, "ucbcfo_g"))
		strcpy(berkname, "G");
	else if (eq(uucpname, "ucbeecs40"))
		strcpy(berkname, "EECS40");
	else if (eq(uucpname, "ucbimage"))
		strcpy(berkname, "Image");
	else if (eq(uucpname, "ucbsrc"))
		strcpy(berkname, "SRC");
	else if (eq(uucpname, "ucbarpa"))
		strcpy(berkname, "ARPAVAX");
	else if (eq(uucpname, "ucbonyx"))
		strcpy(berkname, "Onyx");
	else if (eq(uucpname, "ucb"))
		strcpy(berkname, "C70");
	else if (eq(uucpname, "populi"))
		strcpy(berkname, "G");
	else if (eq(uucpname, "ucbcad"))
		strcpy(berkname, "CAD");
	else
		strcpy(berkname, "UNKNOWN");
	sprintf(buf, "%s:%s", berkname, rest);
#ifdef debug
	printf("berkname='%s', buf='%s'\n", berkname, buf);
#endif
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
