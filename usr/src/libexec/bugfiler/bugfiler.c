/*	bugfiler.c	4.2	83/05/23	*/
/*
 * Bug report processing program.
 * It is designed to be invoked by alias(5) and to be compatible with mh.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

char	deliver[] = "/usr/local/lib/mh/deliver";
char	unixtomh[] = "/usr/local/lib/mh/unixtomh";
char	*maildir = "/ra/bugs/mail";
char	ackfile[] = ".ack";
char	errfile[] = ".format";
char	sumfile[] = "summary";
char	logfile[] = "errors/log";
char	tmpname[] = "BfXXXXXX";
char	draft[] = "RpXXXXXX";

char	buf[8192];
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
	register int n;
	int pfd[2];

	if (argc > 3) {
	usage:
		fprintf(stderr, "Usage: bugfiler [-d] [-mmsg_mode] [maildir]\n");
		exit(1);
	}
	while (--argc > 0) {
		cp = *++argv;
		if (*cp == '-')
			switch (cp[1]) {
			case 'd':
				debug++;
				break;

			case 'm':	/* set message protection */
				n = 0;
				for (cp += 2; *cp >= '0' && *cp <= '7'; )
					n = (n << 3) + (*cp++ - '0');
				msg_prot = n & 0777;
				break;

			default:
				goto usage;
			}
		else
			maildir = cp;
	}
	if (!debug)
		freopen(logfile, "a", stderr);

	if (chdir(maildir) < 0) {
		fprintf(stderr, "can't chdir to %s\n", maildir);
		exit(1);
	}
	umask(0);

#ifdef UNIXCOMP
	/*
	 * Convert UNIX style mail to mh style by filtering stdin through
	 * unixtomh.
	 */
	if (pipe(pfd) >= 0) {
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
	while (process())
		;
	exit(0);
}

/* states */

#define EOM	0	/* End of message seen */
#define FLD	1	/* Looking for header lines */
#define BODY	2	/* Looking for message body lines */

/* defines used for tag attributes */

#define H_REQ 01
#define H_SAV 02
#define H_HDR 04
#define H_FND 010

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
	"From",		H_REQ|H_SAV|H_HDR, 0,
	"Subject",	H_REQ|H_SAV|H_HDR, 0,
	"Index",	H_REQ|H_SAV, 0,
	"Date",		H_SAV|H_HDR, 0,
	"Message-Id",	H_SAV|H_HDR, 0,
	"Reply-To",	H_SAV|H_HDR, 0,
	"Return-Path",	H_SAV|H_HDR, 0,
	"To",		H_SAV|H_HDR, 0,
	"Cc",		H_SAV|H_HDR, 0,
	"Description",	H_REQ,       0,
	"Repeat-By",	H_REQ,	     0,
	"Fix",		0,	     0,
	0,	0,	0,
};

struct header *findheader();

process()
{
	register struct header *hp;
	register char *cp;
	register int c;
	char *info;
	int state, tmp;
	FILE *tfp, *fs;

	/*
	 * Insure all headers are in a consistent
	 * state.  Anything left there is free'd.
	 */
	for (hp = headers; hp->h_tag; hp++) {
		hp->h_flags &= ~H_FND;
		if (hp->h_info) {
			free(hp->h_info);
			hp->h_info = 0;
		}
	}
	/*
	 * Read the report and make a copy.  Must conform to RFC822 and
	 * be of the form... <tag>: <info>
	 * Note that the input is expected to be in mh mail format
	 * (i.e., messages are separated by lines of ^A's).
	 */
	while ((c = getchar()) == '\001' && peekc(stdin) == '\001')
		while (getchar() != '\n')
			;
	if (c == EOF)
		return(0);	/* all done */

	mktemp(tmpname);
	if ((tmp = creat(tmpname, msg_prot)) < 0) {
		fprintf(stderr, "cannont create %s\n", tmpname);
		exit(1);
	}
	if ((tfp = fdopen(tmp, "w")) == NULL) {
		fprintf(stderr, "cannot fdopen temp file\n");
		exit(1);
	}

	for (state = FLD; state != EOF && state != EOM; c = getchar()) {
		switch (state) {
		case FLD:
			if (c == '\n' || c == '-')
				goto body;
			for (cp = buf; c != ':'; c = getchar()) {
				if (cp < buf+sizeof(buf)-1 && c != '\n' && c != EOF) {
					*cp++ = c;
					continue;
				}
				*cp = '\0';
				fputs(buf, tfp);
				state = EOF;
				while (c != EOF) {
					if (c == '\n')
						if ((tmp = peekc(stdin)) == EOF)
							break;
						else if (tmp == '\001') {
							state = EOM;
							break;
						}
					putc(c, tfp);
					c = getchar();
				}
				fclose(tfp);
				goto badfmt;
			}
			*cp = '\0';
			fprintf(tfp, "%s:", buf);
			hp = findheader(buf, state);

			for (cp = buf; ; ) {
				if (cp >= buf+sizeof(buf)-1) {
					fprintf(stderr, "field truncated\n");
					while ((c = getchar()) != EOF && c != '\n')
						putc(c, tfp);
				}
				if ((c = getchar()) == EOF) {
					state = EOF;
					break;
				}
				putc(c, tfp);
				*cp++ = c;
				if (c == '\n')
					if ((c = peekc(stdin)) != ' ' && c != '\t') {
						if (c == EOF)
							state = EOF;
						else if (c == '\001')
							state = EOM;
						break;
					}
			}
			*cp = '\0';
			cp = buf;
			break;

		body:
			state = BODY;
		case BODY:
			for (cp = buf; ; c = getchar()) {
				if (c == EOF) {
					state = EOF;
					break;
				}
				if (c == '\001' && peekc(stdin) == '\001') {
					state = EOM;
					break;
				}
				putc(c, tfp);
				*cp++ = c;
				if (cp >= buf+sizeof(buf)-1 || c == '\n')
					break;
			}
			*cp = '\0';
			if ((cp = index(buf, ':')) == NULL)
				continue;
			*cp++ = '\0';
			hp = findheader(buf, state);
		}

		/*
		 * Don't save the info if the header wasn't found, we don't
		 * care about the info, or the header is repeated.
		 */
		if (hp == NULL || !(hp->h_flags & H_SAV) || hp->h_info)
			continue;
		while (isspace(*cp))
			cp++;
		if (*cp) {
			info = cp;
			while (*cp++);
			cp--;
			while (isspace(cp[-1]))
				*--cp = '\0';
			hp->h_info = (char *) malloc(strlen(info) + 1);
			if (hp->h_info == NULL) {
				fprintf(stderr, "ran out of memory\n");
				continue;
			}
			strcpy(hp->h_info, info);
			if (hp == INDEX)
				chkindex(hp);
		}
	}
	fclose(tfp);
	/*
	 * Verify all the required pieces of information
	 * are present.
	 */
	for (hp = headers; hp->h_tag; hp++) {
		/*
		 * Mail the bug report back to the sender with a note
		 * explaining they must conform to the specification.
		 */
		if ((hp->h_flags & H_REQ) && !(hp->h_flags & H_FND)) {
			if (debug)
				printf("Missing %s\n", hp->h_tag);
		badfmt:
			reply(FROM_I, errfile, tmpname);
			file(tmpname, "errors");
			return(state == EOM);
		}
	}
	/*
	 * Acknowledge receipt.
	 */
	reply(FROM_I, ackfile, (char *)0);
	file(tmpname, folder);
	/*
	 * Append information about the new bug report
	 * to the summary file.
	 */
	if ((fs = fopen(sumfile, "a")) == NULL)
		fprintf(stderr, "Can't open %s\n", sumfile);
	else {
		fprintf(fs, "%14.14s/%-3d  ", folder, num);
		fprintf(fs, "%-51.51s Recv\n", INDEX_I);
		fprintf(fs, "\t\t    %-51.51s\n", SUBJECT_I);
	}
	fclose(fs);
	return(state == EOM);
}

/*
 * Lookup the string in the list of headers and return a pointer
 * to the entry or NULL.
 */

struct header *
findheader(name, state)
	char *name;
	int state;
{
	register struct header *hp;

	if (debug)
		printf("findheader(%s, %d)\n", name, state);

	for (hp = headers; hp->h_tag; hp++) {
		if (!streq(hp->h_tag, buf))
			continue;
		if ((hp->h_flags & H_HDR) && state != FLD)
			continue;
		hp->h_flags |= H_FND;
		return(hp);
	}
	return(NULL);
}

/*
 * Check the format of the Index information.
 * A side effect is to set the name of the folder if all is well.
 */

chkindex(hp)
	struct header *hp;
{
	register char *cp1, *cp2;
	register char c;
	struct stat stbuf;

	if (debug)
		printf("chkindex(%s)\n", hp->h_info);
	/*
	 * Strip of leading "/", "usr/", "src/" or "sys/".
	 */
	cp1 = hp->h_info;
	while (*cp1 == '/')
		cp1++;
	while (substr(cp1, "usr/") || substr(cp1, "src/") || substr(cp1, "sys/"))
		cp1 += 4;
	/*
	 * Read the folder name and remove it from the index line.
	 */
	for (cp2 = folder; ;) {
		switch (c = *cp1++) {
		case '/':
			if (cp2 == folder)
				continue;
			break;
		case '\0':
			cp1--;
			break;
		case ' ':
		case '\t':
			while (isspace(*cp1))
				cp1++;
			break;
		default:
			if (cp2 < folder+sizeof(folder)-1)
				*cp2++ = c;
			continue;
		}
		*cp2 = '\0';
		for (cp2 = hp->h_info; *cp2++ = *cp1++; )
			;
		break;
	}
	if (debug)
		printf("folder = %s\n", folder);
	/*
	 * Check to make sure we have a valid folder name
	 */
	if (stat(folder, &stbuf) == 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR)
		return;
	/*
	 * The Index line is not in the correct format so clear
	 * the H_FND flag to mail back the correct format.
	 */
	hp->h_flags &= ~H_FND;
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
		printf("file(%s, %s)\n", fname, folder);
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

		if ((fin = open(fname, 0)) < 0) {
			fprintf(stderr, "cannot open %s\n", fname);
			return;
		}
		if ((fout = creat(msgname, msg_prot)) < 0) {
			fprintf(stderr, "cannot create %s\n", msgname);
			return;
		}
		while ((n = read(fin, buf, sizeof(buf))) > 0)
			write(fout, buf, n);
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
		printf("reply(%s, %s, %s)\n", to, file1, file2);

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
	fprintf(fout, "Reply-To: 4bsd-bugs%ucbarpa@BERKELEY\n");
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
		while ((w = read(in, buf, sizeof(buf))) > 0)
			fwrite(buf, 1, w, fout);
		close(in);
	}
	if (file2 && (in = open(file2, 0)) >= 0) {
		while ((w = read(in, buf, sizeof(buf))) > 0)
			fwrite(buf, 1, w, fout);
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

streq(s1, s2)
	register char *s1, *s2;
{
	register int c;

	while (c = *s1++)
		if ((c | 040) != (*s2++ | 040))
			return(0);
	return(*s2 == '\0');
}

/*
 * Return true if string s2 matches the first part of s1.
 */

substr(s1, s2)
	register char *s1, *s2;
{
	register int c;

	while (c = *s2++)
		if (c != *s1++)
			return(0);
	return(1);
}

peekc(fp)
FILE *fp;
{
	register c;

	c = getc(fp);
	ungetc(c, fp);
	return(c);
}
