/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Recovers JOVE files after a system/editor crash.
   Usage: recover [-d directory] [-syscrash]
   The -syscrash option is specified in /etc/rc.  It directs recover to
   move all the jove tmp files from TMP_DIR (/tmp) to REC_DIR (/usr/preserve).
   recover -syscrash must be invoked in /ect/rc BEFORE /tmp gets cleared out.
   (about the same place as expreserve gets invoked to save ed/vi/ex files.

   The -d option lets you specify the directory to search for tmp files when
   the default isn't the right one.

   Look in Makefile to change the default directories. */

#include <stdio.h>	/* Do stdio first so it doesn't override OUR
			   definitions. */
#include "jove.h"
#include "temp.h"
#include "rec.h"
#include "rectune.h"
#include "wait.h"

#ifdef UNIX
# include <signal.h>
# include <sys/file.h>
# include <pwd.h>
# include <time.h>
#endif

#ifdef	SYSV
# include <sys/utsname.h>
#endif

#ifndef	L_SET
# define L_SET	0
# define L_INCR	1
#endif

extern char	*ctime proto((const time_t *));

private char	blk_buf[JBUFSIZ];
private int	nleft;
private FILE	*ptrs_fp;
private int	data_fd;
private struct rec_head	Header;
private long	Nchars,
	Nlines;
private char	tty[] = "/dev/tty";
private int	UserID,
	Verbose = 0;
private char	*Directory = NULL;		/* the directory we're looking in */

private struct file_pair {
	char	*file_data,
		*file_rec;
#define INSPECTED	01
	int	file_flags;
	struct file_pair	*file_next;
} *First = NULL;

private struct rec_entry	*buflist[100];	/* system initializes to 0 */

#ifndef F_COMPLETION
# define F_COMPLETION	/* since scandir.c is surrounded by an ifdef */
#endif

/* simpler version of one in util.c, needed by scandir.c */
UnivPtr
emalloc(size)
size_t size;
{
	register UnivPtr ptr;

	if ((ptr = malloc(size)) == NULL) {
		fprintf(stderr, "couldn't malloc(%d)\n", size);
		exit(1);
	}
	return ptr;
}
	    
/* simpler version of one in util.c, needed by scandir.c */
UnivPtr
erealloc(ptr, size)
UnivPtr ptr;
size_t size;
{
	if ((ptr = realloc(ptr, size)) == NULL) {
		fprintf(stderr, "couldn't realloc(%d)\n", size);
		exit(1);
	}
	return ptr;
}

/* duplicated in util.c, needed by scandir.c */
void
null_ncpy(to, from, n)
char	*to,
	*from;
size_t	n;
{
	(void) strncpy(to, from, n);
	to[n] = '\0';
}

#define complain printf	/* kludge! needed by scandir.c */
#include "scandir.c"	/* to get dirent simulation and jscandir */

/* Get a line at `tl' in the tmp file into `buf' which should be LBSIZE
   long. */

private char	*getblock proto((daddr atl));

void
getline(tl, buf)
daddr	tl;
char	*buf;
{
	register char	*bp,
			*lp;
	register int	nl;

	lp = buf;
	bp = getblock(tl >> 1);
	nl = nleft;
	tl = blk_round(tl);

	while ((*lp++ = *bp++) != '\0') {
		if (--nl == 0) {
			tl = forward_block(tl);
			bp = getblock(tl >> 1);
			nl = nleft;
		}
	}
}

private char *
getblock(atl)
daddr	atl;
{
	int	bno,
		off;
	static int	curblock = -1;

	bno = da_to_bno(atl);
	off = da_to_off(atl);
	nleft = JBUFSIZ - off;

	if (bno != curblock) {
		lseek(data_fd, (long) bno * JBUFSIZ, L_SET);
		read(data_fd, (UnivPtr)blk_buf, (size_t)JBUFSIZ);
		curblock = bno;
	}
	return blk_buf + off;
}

char *
copystr(s)
char	*s;
{
	char	*str;

	str = malloc((size_t) (strlen(s) + 1));
	strcpy(str, s);

	return str;
}

private char	*CurDir;

/* Scan the DIRNAME directory for jove tmp files, and make a linked list
   out of them. */

private int	add_name proto((char *));

private void
get_files(dirname)
char	*dirname;
{
	char **nmptr;

	CurDir = dirname;
	First = NULL;
	jscandir(dirname, &nmptr, add_name,
		(int (*) proto((UnivConstPtr, UnivConstPtr)))NULL);
}

private int
add_name(fname)
char *fname;
{
	char	dfile[128],
		rfile[128];
	struct file_pair	*fp;
	struct rec_head		header;
	int	fd;

	if (strncmp(fname, "jrec", (size_t)4) != 0)
		return 0;
	/* If we get here, we found a "recover" tmp file, so now
	   we look for the corresponding "data" tmp file.  First,
	   though, we check to see whether there is anything in
	   the "recover" file.  If it's 0 length, there's no point
	   in saving its name. */
	(void) sprintf(rfile, "%s/%s", CurDir, fname);
	(void) sprintf(dfile, "%s/jove%s", CurDir, fname + 4);
	if ((fd = open(rfile, 0)) != -1) {
		if ((read(fd, (UnivPtr) &header, sizeof header) != sizeof header)) {
			close(fd);
			return 0;
		} else
			close(fd);
	}
	if (access(dfile, 0) != 0) {
		fprintf(stderr, "recover: can't find the data file for %s/%s\n", Directory, fname);
		fprintf(stderr, "so deleting...\n");
		(void) unlink(rfile);
		(void) unlink(dfile);
		return 0;
	}
	/* If we get here, we've found both files, so we put them
	   in the list. */
	fp = (struct file_pair *) malloc (sizeof *fp);
	if (fp == NULL) {
		fprintf(stderr, "recover: cannot malloc for file_pair.\n");
		exit(-1);
	}
	fp->file_data = copystr(dfile);
	fp->file_rec = copystr(rfile);
	fp->file_flags = 0;
	fp->file_next = First;
	First = fp;

	return 1;
}

private void
options()
{
	printf("Options are:\n");
	printf("	?		list options.\n");
	printf("	get		get a buffer to a file.\n");
	printf("	list		list known buffers.\n");
	printf("	print		print a buffer to terminal.\n");
	printf("	quit		quit and delete jove tmp files.\n");
	printf("	restore		restore all buffers.\n");
}

/* Returns a legitimate buffer # */

private void	tellme proto((char *, char *)),
	list proto((void));

private struct rec_entry **
getsrc()
{
	char	name[128];
	int	number;

	for (;;) {
		tellme("Which buffer ('?' for list)? ", name);
		if (name[0] == '?')
			list();
		else if (name[0] == '\0')
			return NULL;
		else if ((number = atoi(name)) > 0 && number <= Header.Nbuffers)
			return &buflist[number];
		else {
			int	i;

			for (i = 1; i <= Header.Nbuffers; i++)
				if (strcmp(buflist[i]->r_bname, name) == 0)
					return &buflist[i];
			printf("%s: unknown buffer.\n", name);
		}
	}
}

/* Get a destination file name. */

static char *
getdest()
{
	static char	filebuf[256];

	tellme("Output file: ", filebuf);
	if (filebuf[0] == '\0')
		return NULL;
	return filebuf;
}

#include "ctype.h"

private char *
readword(buf)
char	*buf;
{
	int	c;
	char	*bp = buf;

	do ; while (strchr(" \t\n", c = getchar()));

	do {
		if (strchr(" \t\n", c))
			break;
		*bp++ = c;
	} while ((c = getchar()) != EOF);
	*bp = '\0';

	return buf;
}

private void
tellme(quest, answer)
char	*quest,
	*answer;
{
#ifndef BSD386
	/* this is naughty anyway */
	if (stdin->_cnt <= 0) {
		printf("%s", quest);
		fflush(stdout);
	}
#else
	printf("%s", quest);
	fflush(stdout);
#endif
	readword(answer);
}

/* Print the specified file to standard output. */

private jmp_buf	int_env;

private SIGRESULT
catch(junk)
int	junk;
{
	longjmp(int_env, 1);
	/*NOTREACHED*/
}

private void	get proto((struct rec_entry **src, char *dest));

private void
restore()
{
	register int	i;
	char	tofile[100],
		answer[30];
	int	nrecovered = 0;

	for (i = 1; i <= Header.Nbuffers; i++) {
		(void) sprintf(tofile, "#%s", buflist[i]->r_bname);
tryagain:
		printf("Restoring %s to %s, okay?", buflist[i]->r_bname,
						     tofile);
		tellme(" ", answer);
		switch (answer[0]) {
		case 'y':
			break;

		case 'n':
			continue;

		default:
			tellme("What file should I use instead? ", tofile);
			goto tryagain;
		}
		get(&buflist[i], tofile);
		nrecovered += 1;
	}
	printf("Recovered %d buffers.\n", nrecovered);
}

private void	dump_file proto((int which, FILE *out));

private void
get(src, dest)
struct rec_entry	**src;
char	*dest;
{
	FILE	*outfile;

	if (src == NULL || dest == NULL)
		return;
	(void) signal(SIGINT, catch);
	if (setjmp(int_env) == 0) {
		if (dest == tty)
			outfile = stdout;
		else {
			if ((outfile = fopen(dest, "w")) == NULL) {
				printf("recover: cannot create %s.\n", dest);
				(void) signal(SIGINT, SIG_DFL);
				return;
			}
			printf("\"%s\"", dest);
		}
		dump_file(src - buflist, outfile);
	} else
		printf("\nAborted!\n");
	(void) signal(SIGINT, SIG_DFL);
	if (dest != tty) {
		fclose(outfile);
		printf(" %ld lines, %ld characters.\n", Nlines, Nchars);
	}
}

private char **
scanvec(args, str)
register char	**args,
		*str;
{
	while (*args) {
		if (strcmp(*args, str) == 0)
			return args;
		args += 1;
	}
	return NULL;
}

private void
read_rec(recptr)
struct rec_entry	*recptr;
{
	if (fread((UnivPtr) recptr, sizeof *recptr, (size_t)1, ptrs_fp) != 1)
		fprintf(stderr, "recover: cannot read record.\n");
}

private void
seekto(which)
int	which;
{
	long	offset;
	int	i;

	offset = sizeof (Header) + (Header.Nbuffers * sizeof (struct rec_entry));
	for (i = 1; i < which; i++)
		offset += buflist[i]->r_nlines * sizeof (daddr);
	fseek(ptrs_fp, offset, L_SET);
}

private void
makblist()
{
	int	i;

	fseek(ptrs_fp, (long) sizeof (Header), L_SET);
	for (i = 1; i <= Header.Nbuffers; i++) {
		if (buflist[i] == NULL)
			buflist[i] = (struct rec_entry *) malloc (sizeof (struct rec_entry));
		read_rec(buflist[i]);
	}
	while (buflist[i]) {
		free((UnivPtr) buflist[i]);
		buflist[i] = NULL;
		i += 1;
	}
}

private daddr
getaddr(fp)
register FILE	*fp;
{
	register int	nchars = sizeof (daddr);
	daddr	addr;
	register char	*cp = (char *) &addr;

	while (--nchars >= 0)
		*cp++ = getc(fp);

	return addr;
}

private void
dump_file(which, out)
int	which;
FILE	*out;
{
	register int	nlines;
	register daddr	addr;
	char	buf[JBUFSIZ];

	seekto(which);
	nlines = buflist[which]->r_nlines;
	Nchars = Nlines = 0L;
	while (--nlines >= 0) {
		addr = getaddr(ptrs_fp);
		getline(addr, buf);
		Nlines += 1;
		Nchars += 1 + strlen(buf);
		fputs(buf, out);
		if (nlines > 0)
			fputc('\n', out);
	}
}

/* List all the buffers. */

private void
list()
{
	int	i;

	for (i = 1; i <= Header.Nbuffers; i++)
		printf("%d) buffer %s  \"%s\" (%d lines)\n", i,
			buflist[i]->r_bname,
			buflist[i]->r_fname,
			buflist[i]->r_nlines);
}

private void	ask_del proto((char *prompt, struct file_pair *fp));

private int
doit(fp)
struct file_pair	*fp;
{
	char	answer[30];
	char	*datafile = fp->file_data,
		*pntrfile = fp->file_rec;

	ptrs_fp = fopen(pntrfile, "r");
	if (ptrs_fp == NULL) {
		if (Verbose)
			fprintf(stderr, "recover: cannot read rec file (%s).\n", pntrfile);
		return 0;
	}
	fread((UnivPtr) &Header, sizeof Header, (size_t)1, ptrs_fp);
	if (Header.Uid != UserID)
		return 0;

	/* Don't ask about JOVE's that are still running ... */
#ifdef	KILL0
	if (kill(Header.Pid, 0) == 0)
		return 0;
#endif	/* KILL0 */

	if (Header.Nbuffers == 0) {
		printf("There are no modified buffers in %s; should I delete the tmp file?", pntrfile);
		ask_del(" ", fp);
		return 1;
	}

	if (Header.Nbuffers < 0) {
		fprintf(stderr, "recover: %s doesn't look like a jove file.\n", pntrfile);
		ask_del("Should I delete it? ", fp);
		return 1;	/* We'll, we sort of found something. */
	}
	printf("Found %d buffer%s last updated: %s",
		Header.Nbuffers,
		Header.Nbuffers != 1 ? "s" : "",
		ctime(&Header.UpdTime));
	data_fd = open(datafile, 0);
	if (data_fd == -1) {
		fprintf(stderr, "recover: but I can't read the data file (%s).\n", datafile);
		ask_del("Should I delete the tmp files? ", fp);
		return 1;
	}
	makblist();
	list();

	for (;;) {
		tellme("(Type '?' for options): ", answer);
		switch (answer[0]) {
		case '\0':
			continue;

		case '?':
			options();
			break;

		case 'l':
			list();
			break;

		case 'p':
			get(getsrc(), tty);
			break;

		case 'q':
			ask_del("Shall I delete the tmp files? ", fp);
			return 1;

		case 'g':
		    {	/* So it asks for src first. */
			char	*dest;
			struct rec_entry	**src;

			if ((src = getsrc()) == NULL)
				break;
			dest = getdest();
			get(src, dest);
			break;
		    }

		case 'r':
			restore();
			break;

		default:
			printf("I don't know how to \"%s\"!\n", answer);
			break;
		}
	}
}

private void	del_files proto((struct file_pair *fp));

private void
ask_del(prompt, fp)
char	*prompt;
struct file_pair	*fp;
{
	char	yorn[20];

	tellme(prompt, yorn);
	if (yorn[0] == 'y')
		del_files(fp);
}

private void
del_files(fp)
struct file_pair	*fp;
{
	(void) unlink(fp->file_data);
	(void) unlink(fp->file_rec);
}



private void
MailUser(rec)
struct rec_head *rec;
{
#ifdef	SYSV
	struct utsname mach;
#else
	char mach[BUFSIZ];
#endif
	char mail_cmd[BUFSIZ];
	char *last_update;
	char *buf_string;
	FILE *mail_pipe;
	struct passwd *pw;

	if ((pw = getpwuid(rec->Uid))== NULL)
		return;
#ifdef	SYSV
	if (uname(&mach) < 0)
		strcpy(mach.sysname, "unknown");
#else
	{
#ifndef BSD386
		extern int	gethostname proto((const char *, size_t));
#endif

		gethostname(mach, sizeof(mach));
	}
#endif
	last_update = ctime(&(rec->UpdTime));
	/* Start up mail */
	sprintf(mail_cmd, "/bin/mail %s", pw->pw_name);
	setuid(getuid());
	if ((mail_pipe = popen(mail_cmd, "w")) == NULL)
		return;
	setbuf(mail_pipe, mail_cmd);
	/* Let's be grammatically correct! */
	if (rec->Nbuffers == 1)
		buf_string = "buffer";
	else
		buf_string = "buffers";
	fprintf(mail_pipe, "Subject: System crash\n");
	fprintf(mail_pipe, " \n");
	fprintf(mail_pipe, "Jove saved %d %s when the system \"%s\"\n",
	 rec->Nbuffers, buf_string,
#ifdef	SYSV
	 mach.sysname
#else
	 mach
#endif
	 );
	fprintf(mail_pipe, "crashed on %s\n\n", last_update);
	fprintf(mail_pipe, "You can retrieve the %s using Jove's -r\n",
	 buf_string);
	fprintf(mail_pipe, "(recover option) i.e. give the command.\n");
	fprintf(mail_pipe, "\tjove -r\n");
	fprintf(mail_pipe, "See the Jove manual for more details\n");
	pclose(mail_pipe);
}


private void
savetmps()
{
	struct file_pair	*fp;
	union wait	status;
	int	pid,
		fd;
	struct rec_head		header;
	char	buf[BUFSIZ];
	char	*fname;
	struct stat		stbuf;

	if (strcmp(TMP_DIR, REC_DIR) == 0)
		return;		/* Files are moved to the same place. */
	get_files(TMP_DIR);
	for (fp = First; fp != NULL; fp = fp->file_next) {
		stat(fp->file_data, &stbuf);
		switch (pid = fork()) {
		case -1:
			fprintf(stderr, "recover: can't fork\n!");
			exit(-1);
			/*NOTREACHED*/

		case 0:
			fprintf(stderr, "Recovering: %s, %s\n", fp->file_data,
			 fp->file_rec);
			if ((fd = open(fp->file_rec, 0)) != -1) {
				if ((read(fd, (UnivPtr) &header, sizeof header) != sizeof header)) {
					close(fd);
					return;
				} else
					close(fd);
			}
			MailUser(&header);
			execl("/bin/mv", "mv", fp->file_data, fp->file_rec,
				  REC_DIR, (char *)NULL);
			fprintf(stderr, "recover: cannot execl /bin/mv.\n");
			exit(-1);
			/*NOTREACHED*/

		default:
			do ; while (wait(&status) != pid);
			if (status.w_status != 0)
				fprintf(stderr, "recover: non-zero status (%d) returned from copy.\n", status.w_status);
			fname = fp->file_data + strlen(TMP_DIR);
			strcpy(buf, REC_DIR);
			strcat(buf, fname);
			if (chown(buf, (int) stbuf.st_uid, (int) stbuf.st_gid) != 0)
				perror("recover: chown failed.");
			fname = fp->file_rec + strlen(TMP_DIR);
			strcpy(buf, REC_DIR);
			strcat(buf, fname);
			if (chown(buf, (int) stbuf.st_uid, (int) stbuf.st_gid) != 0)
				perror("recover: chown failed.");
		}
	}
}

private int
lookup(dir)
char	*dir;
{
	struct file_pair	*fp;
	int	nfound = 0;

	printf("Checking %s ...\n", dir);
	Directory = dir;
	get_files(dir);
	for (fp = First; fp != NULL; fp = fp->file_next) {
		nfound += doit(fp);
		if (ptrs_fp)
			(void) fclose(ptrs_fp);
		if (data_fd > 0)
			(void) close(data_fd);
	}
	return nfound;
}

int
main(argc, argv)
int	argc;
char	*argv[];
{
	int	nfound;
	char	**argvp;
	char	*tmp_dir;

	UserID = getuid();

	if (scanvec(argv, "-help")) {
		printf("recover: usage: recover [-d directory] [-syscrash]\n");
		printf("Use \"jove -r\" after JOVE has died for some\n");
		printf("unknown reason.\n\n");
		printf("Use \"%s -syscrash\"\n", Recover);
		printf("when the system is in the process of rebooting.");
		printf("This is done automatically at reboot time\n");
		printf("and so most of you don't have to worry about that.\n\n");
		printf("Use \"recover -d directory\" when the tmp files are store\n");
		printf("in DIRECTORY instead of the default one (/tmp).\n");
		exit(0);
	}
	if (scanvec(argv, "-v"))
		Verbose = YES;
	if (scanvec(argv, "-syscrash")) {
		printf("Recovering jove files ... ");
		savetmps();
		printf("Done.\n");
		exit(0);
	}
	if ((argvp = scanvec(argv, "-uid")) != NULL)
		UserID = atoi(argvp[1]);
	if ((argvp = scanvec(argv, "-d")) != NULL)
		tmp_dir = argvp[1];
	else
		tmp_dir = TmpFilePath;
	/* Check default directory */
	nfound = lookup(tmp_dir);
	/* Check whether anything was saved when system died? */
	if (strcmp(tmp_dir, REC_DIR) != 0)
		nfound += lookup(REC_DIR);
	if (nfound == 0)
		printf("There's nothing to recover.\n");
	return 0;
}
