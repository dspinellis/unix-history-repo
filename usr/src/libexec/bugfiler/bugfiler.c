/*	bugfiler.c	4.1	83/05/11	*/
/*
 * Bug report processing program.
 * It is designed to be invoked by alias(5) and to be compatible with mh.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dir.h>

char	deliver[] = "/usr/local/lib/mh/deliver";
char	unixtomh[] = "/usr/local/lib/mh/unixtomh";
char	*maildir = "/ra/bugs/mail";
char	ackfile[] = ".ack";
char	errfile[] = ".format";
char	sumfile[] = "summary";
char	logfile[] = "errors/log";
char	tmpname[] = "BfXXXXXX";
char	draft[] = "RpXXXXXX";

char	line[BUFSIZ];
char	folder[MAXNAMLEN];
int	num;
int	msg_prot = 0664;

int	debug;

char	*index();
char	*rindex();
char	*fixaddr();

main(argc, argv)
	char *argv[];
{
	register char *cp;

	if (argc > 3) {
	usage:
		fprintf(stderr, "Usage: bugfiler [-d] [maildir]\n");
		exit(1);
	}
	while (--argc > 0) {
		cp = *++argv;
		if (*cp == '-') while (*++cp)
			switch (*cp) {
			case 'd':
				debug++;
				break;
			default:
				goto usage;
			}
		else
			maildir = cp;
	}
	if (chdir(maildir) < 0) {
		fprintf(stderr, "can't chdir to %s\n", maildir);
		exit(1);
	}
	if (freopen(logfile, "a", stderr) == NULL)
		freopen("/dev/null", "w", stderr);
	exit(process());
}

/* defines used for tag attributes */

#define H_REQ 01
#define H_OPT 02
#define H_SAV 04

#define FROM_I headers[0].h_info
#define SUBJECT_I headers[1].h_info
#define INDEX &headers[2]
#define INDEX_I headers[2].h_info
#define DATE_I headers[3].h_info
#define MSGID_I headers[4].h_info
#define REPLYTO_I headers[5].h_info
#define RETURNPATH_I headers[6].h_info
#define TO_I headers[7].h_info
#define CC_I headers[8].h_info
#define FIX headers[11]

struct header {
	char	*h_tag;
	int	h_flags;
	char	*h_info;
} headers[] = {
	"From",		H_REQ|H_SAV, 0,
	"Subject",	H_REQ|H_SAV, 0,
	"Index",	H_REQ|H_SAV, 0,
	"Date",		H_OPT|H_SAV, 0,
	"Message-Id",	H_OPT|H_SAV, 0,
	"Reply-To",	H_OPT|H_SAV, 0,
	"Return-Path",	H_OPT|H_SAV, 0,
	"To",		H_OPT|H_SAV, 0,
	"Cc",		H_OPT|H_SAV, 0,
	"Description",	H_REQ,       0,
	"Repeat-By",	H_REQ,	     0,
	"Fix",		H_OPT,	     0,
	0,	0,	0,
};

process()
{
	register struct header *hp;
	register char *cp;
	char *info;
	int tmp, pfd[2];
	FILE *fs;

	/*
	 * Insure all headers are in a consistent
	 * state.  Anything left there is free'd.
	 */
	for (hp = headers; hp->h_tag; hp++) {
		if (hp->h_info) {
			if (hp->h_info != (char *) 1)
				free(hp->h_info);
			hp->h_info = 0;
		}
	}
#ifdef UNIXCOMP
	/*
	 * Convert UNIX style mail to mh style by filtering stdin through
	 * unixtomh.
	 */
	if (pipe(pfd) >= 0) {
		register int n;

		while ((n = fork()) == -1)
			sleep(5);
		if (n == 0) {
			close(pfd[0]);
			dup2(pfd[1], 1);
			close(pfd[1]);
			execl(unixtomh, "unixtomh", 0);
			_exit(127);
		}
		close(pfd[1]);
		dup2(pfd[0], 0);
		close(pfd[0]);
	}
#endif
	/*
	 * Read the report and make a copy.  Must conform to RFC822 and
	 * be of the form... <tag>: <info>
	 */
	mktemp(tmpname);
	if ((tmp = creat(tmpname, msg_prot)) < 0)
		return(1);
	while ((cp = fgets(line, sizeof(line), stdin)) != NULL) {
		if (line[0] == '\01')
			continue;
		write(tmp, cp, strlen(cp));
		cp = index(cp, ':');
		if (cp == 0)
			continue;
		*cp++ = '\0';
		for (hp = headers; hp->h_tag; hp++)
			if (streq(hp->h_tag, line))
				break;
		if (hp->h_tag == 0)
			continue;
		if (!(hp->h_flags & H_SAV)) {
			hp->h_info = (char *) 1;
			continue;
		}
		while (isspace(*cp))
			cp++;
		if (*cp) {
			info = cp;
			while (*cp++);
			cp--;
			while (isspace(cp[-1]))
				*--cp = '\0';
			hp->h_info = (char *) malloc(strlen(info) + 1);
			if (hp->h_info == NULL)
				continue;
			strcpy(hp->h_info, info);
			if (hp == INDEX)
				chkindex(hp);
		}
	}
	close(tmp);
	/*
	 * Verify all the required pieces of information
	 * are present.
	 */
	for (hp = headers; hp->h_tag; hp++)
		if ((hp->h_flags & H_REQ) && !hp->h_info)
			break;
	if (hp->h_tag) {
		/*
		 * Mail the bug report back to the sender with a note
		 * explaining they must conform to the specification.
		 */
		if (debug)
			fprintf(stderr, "Missing %s\n", hp->h_tag);
		reply(FROM_I, errfile, tmpname);
		file(tmpname, "errors");
		return(0);
	}
	else {	/* Acknowledge receipt */
		reply(FROM_I, ackfile, (char *)0);
		file(tmpname, folder);
	}
	/*
	 * Append information about the new bug report
	 * to the summary file.
	 */
	if ((fs = fopen(sumfile, "a")) == NULL) {
		fprintf(stderr, "Can't open %s\n", sumfile);
		return(1);
	}
	fprintf(fs, "%14.14s/%-3d  %s\n\t\t    %s\n", folder, num, INDEX_I, SUBJECT_I);
	fclose(fs);
	return(0);
}

/*
 * Check the format of the Index information.
 * A side effect is to set the name of the folder if all is well.
 */

chkindex(hp)
	struct header *hp;
{
	register char *cp1, *cp2, *cp3, *cp4;
	register char c;
	struct stat stbuf;

	if (debug)
		fprintf(stderr, "chkindex(%s)\n", hp->h_info);
	/*
	 * Read the folder name and remove it from the index line.
	 */
	for (cp1 = hp->h_info, cp2 = NULL, cp3 = folder, cp4 == NULL; ;) {
		c = *cp1++;
		if (c == '\0' || isspace(c) || cp3 >= folder+sizeof(folder)-1) {
			if (cp4 == NULL)
				*cp3 = '\0';
			else
				*cp4 = '\0';
			if (cp2 == NULL) {
				cp2 = cp1 - 1;
				while (isspace(*cp2))
					cp2++;
			}
			for (cp3 = hp->h_info; *cp3++ = *cp2++; );
			break;
		} else {
			if (c == '/') {
				cp2 = cp1;
				cp4 = cp3;
			}
			*cp3++ = c;
		}
	}
	/*
	 * Check to see if a Fix is included.
	if ((cp1 = rindex(hp->h_info, ' ')) == NULL) {
		if ((cp1 = rindex(hp->h_info, '\t')) != NULL)
			cp1++;
	} else
		cp1++;
	if (cp1 != NULL && streq(cp1, FIX.h_tag))
		FIX.h_flags = H_REQ;
	else
		FIX.h_flags = 0;
	 */
	/*
	 * Check to make sure we have a valid folder name
	 */
	if (stat(folder, &stbuf) == 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR)
		return;
	/*
	 * The Index line is not in the correct format so clear
	 * the h_info line to mail back the correct format.
	 */
	hp->h_info = 0;
}

/*
 * Move or copy the file msg to the folder (directory).
 * A side effect is to set num to the number of the file in folder.
 */

file(fname, folder)
	char *fname, *folder;
{
	register char *cp, n;
	char msgname[MAXNAMLEN*2+2];
	struct stat stbuf;
	DIR *dirp;
	struct direct *d;

	if (debug)
		fprintf(stderr, "file(%s, %s)\n", fname, folder);
	/*
	 * Get the next number to use by finding the last message number
	 * in folder and adding one.
	 */
	if ((dirp = opendir(folder)) == NULL) {
		fprintf(stderr, "Cannot open %s/%s\n", maildir, folder);
		return;
	}
	num = 0;
	while ((d = readdir(dirp)) != NULL) {
		cp = d->d_name;
		n = 0;
		while (isdigit(*cp))
			n = n * 10 + (*cp++ - '0');
		if (*cp == '\0' && n > num)
			num = n;
	}
	closedir(dirp);
	num++;
	/*
	 * Create the destination file "folder/num" and copy fname to it.
	 */
	sprintf(msgname, "%s/%d", folder, num);
	if (link(fname, msgname) < 0) {
		int fin, fout;

		if ((fin = open(fname, 0)) < 0)
			return;
		if ((fout = open(msgname, 1)) < 0)
			return;
		while ((n = read(fin, line, sizeof(line))) > 0)
			write(fout, line, n);
		close(fin);
		close(fout);
	}
	unlink(fname);
}

/*
 * Mail file1 and file2 back to the sender.
 */

reply(to, file1, file2)
	char	*to, *file1, *file2;
{
	int (*istat)(), (*qstat)();
	int pid, w, status, pfd[2], in;
	FILE *fout;

	if (debug)
		fprintf(stderr, "reply(%s, %s, %s)\n", to, file1, file2);
	/*
	 * Create a temporary file to put the message in.
	 */
	mktemp(draft);
	if ((fout = fopen(draft, "w")) == NULL) {
		fprintf(stderr, "Can't create %s\n", draft);
		return;
	}
	/*
	 * Output the proper header information.
	 */
	fprintf(fout, "Reply-To: 4bsd-bugs@BERKELEY\n");
	if (RETURNPATH_I != NULL)
		to = RETURNPATH_I;
	if (REPLYTO_I != NULL)
		to = REPLYTO_I;
	if ((to = fixaddr(to)) == 0) {
		fprintf(stderr, "No one to reply to\n");
		return;
	}
	fprintf(fout, "To: %s\n", to);
	if (SUBJECT_I) {
		fprintf(fout, "Subject: ");
		if ((SUBJECT_I[0] != 'R' && SUBJECT_I[0] != 'r') ||
		    (SUBJECT_I[1] != 'E' && SUBJECT_I[1] != 'e') ||
		    SUBJECT_I[2] != ':')
			fprintf(fout, "Re: ");
		fprintf(fout, "%s\n", SUBJECT_I);
	}
	if (DATE_I) {
		fprintf(fout, "In-Acknowledgement-Of: Your message of ");
		fprintf(fout, "%s.\n", DATE_I);
		if (MSGID_I)
			fprintf(fout, "             %s\n", MSGID_I);
	}
	fprintf(fout, "----------\n");
	if ((in = open(file1, 0)) >= 0) {
		while ((w = read(in, line, sizeof(line))) > 0)
			fwrite(line, 1, w, fout);
		close(in);
	}
	if (file2 && (in = open(file2, 0)) >= 0) {
		while ((w = read(in, line, sizeof(line))) > 0)
			fwrite(line, 1, w, fout);
		close(in);
	}
	fclose(fout);
	while ((pid = fork()) == -1)
		sleep(5);
	if (pid == 0) {
		execl(deliver, "deliver", draft, 0);
		_exit(127);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((w = wait(&status)) != -1 && w != pid);
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	if (w != -1 && status == 0)
		unlink(draft);
}

/*
 * fix names like "xxx (something)" to "xxx" and
 * "xxx <something>" to "something".
 */

char *
fixaddr(text)
	char *text;
{
	register char *cp, *lp, c;
	char *tp;

	if (!text)
		return(0);
	for (lp = cp = text; ; ) {
		switch (c = *cp++) {
		case '(':
			while (*cp && *cp++ != ')');
			continue;
		case '<':
			lp = text;
		case '>':
			continue;
		case '\0':
			while (lp != text && (*lp == ' ' || *lp == '\t'))
				lp--;
			*lp = c;
			return(text);
		}
		*lp++ = c;
	}
}

/*
 * Compare two strings and convert any upper case letters to lower case.
 */

streq(c1, c2)
	register char *c1, *c2;
{
	register int c;

	while (c = *c1++)
		if ((c | 040) != (*c2++ | 040))
			return(0);
	return(*c2 == '\0');
}
