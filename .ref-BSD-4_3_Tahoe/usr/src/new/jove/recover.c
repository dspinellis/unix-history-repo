/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Recovers JOVE files after a system/editor crash.
   Usage: recover [-d directory] [-syscrash]
   The -syscrash option is specified in /etc/rc and what it does is
   move all the jove tmp files from TMP_DIR to REC_DIR.

   The -d option lets you specify the directory to search for tmp files when
   the default isn't the right one.

   Look in Makefile to change the default directories. */

#include <stdio.h>	/* Do stdio first so it doesn't override OUR
			   definitions. */
#undef EOF
#undef BUFSIZ
#undef putchar
#undef getchar

#define STDIO

#include "jove.h"
#include "temp.h"
#include "rec.h"
#include <signal.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/dir.h>

#ifndef L_SET
#	define L_SET	0
#	define L_INCR	1
#endif

char	blk_buf[BUFSIZ];
int	nleft;
FILE	*ptrs_fp;
int	data_fd;
struct rec_head	Header;
char	datafile[40],
	pntrfile[40];
long	Nchars,
	Nlines;
char	tty[] = "/dev/tty";
int	UserID,
	Verbose = 0;
char	*Directory = 0;		/* the directory we're looking in */

struct file_pair {
	char	*file_data,
		*file_rec;
#define INSPECTED	01
	int	file_flags;
	struct file_pair	*file_next;
} *First = 0,
  *Last = 0;

struct rec_entry	*buflist[100] = {0};

#ifndef BSD4_2

typedef struct {
	int	d_fd;		/* File descriptor for this directory */
} DIR;

DIR *
opendir(dir)
char	*dir;
{
	DIR	*dp = (DIR *) malloc(sizeof *dp);

	if ((dp->d_fd = open(dir, 0)) == -1)
		return NULL;
	return dp;
}

closedir(dp)
DIR	*dp;
{
	(void) close(dp->d_fd);
	free(dp);
}

struct direct *
readdir(dp)
DIR	*dp;
{
	static struct direct	dir;

	do
		if (read(dp->d_fd, &dir, sizeof dir) != sizeof dir)
			return NULL;
#if defined(elxsi) && defined(SYSV)
	/*
	 * Elxsi has a BSD4.2 implementation which may or may not use
	 * `twisted inodes' ...  Anyone able to check?
	 */
	while (*(unsigned short *)&dir.d_ino == 0);
#else
	while (dir.d_ino == 0);
#endif

	return &dir;
}

#endif /* BSD4_2 */

/* Get a line at `tl' in the tmp file into `buf' which should be LBSIZE
   long. */

getline(tl, buf)
disk_line	tl;
char	*buf;
{
	register char	*bp,
			*lp;
	register int	nl;
	char	*getblock();

	lp = buf;
	bp = getblock(tl >> 1);
	nl = nleft;
	tl = blk_round(tl);

	while (*lp++ = *bp++) {
		if (--nl == 0) {
			tl = forward_block(tl);
			bp = getblock(tl >> 1);
			nl = nleft;
		}
	}
}

char *
getblock(atl)
disk_line	atl;
{
	int	bno,
		off;
	static int	curblock = -1;

	bno = da_to_bno(atl);
	off = da_to_off(atl);
	nleft = BUFSIZ - off;

	if (bno != curblock) {
		lseek(data_fd, (long) bno * BUFSIZ, L_SET);
		read(data_fd, blk_buf, BUFSIZ);
		curblock = bno;
	}
	return blk_buf + off;
}

char *
copystr(s)
char	*s;
{
	char	*str;

	str = malloc(strlen(s) + 1);
	strcpy(str, s);

	return str;
}

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

scandir(dir, nmptr, qualify, sorter)
char	*dir;
struct direct	***nmptr;
int	(*qualify)();
struct direct	*(*sorter)();
{
	DIR	*dirp;
	struct direct	*entry,
			**ourarray;
	int	nalloc = 10,
		nentries = 0;

	if ((dirp = opendir(dir)) == NULL)
		return -1;
	ourarray = (struct direct **) malloc(nalloc * sizeof (struct direct *));
	while ((entry = readdir(dirp)) != NULL) {
		if (qualify != 0 && (*qualify)(entry) == 0)
			continue;
		if (nentries == nalloc) {
			ourarray = (struct direct **) realloc(ourarray, (nalloc += 10) * sizeof (struct direct));
			if (ourarray == NULL)
				return -1;
		}
		ourarray[nentries] = (struct direct *) malloc(sizeof *entry);
		*ourarray[nentries] = *entry;
		nentries += 1;
	}
	closedir(dirp);
	if (nentries != nalloc)
		ourarray = (struct direct **) realloc(ourarray,
					(nentries * sizeof (struct direct)));
	if (sorter != 0)
		qsort(ourarray, nentries, sizeof (struct direct **), sorter);
	*nmptr = ourarray;

	return nentries;
}

alphacomp(a, b)
struct direct	**a,
		**b;
{
	return strcmp((*a)->d_name, (*b)->d_name);
}

char	*CurDir;

/* Scan the DIRNAME directory for jove tmp files, and make a linked list
   out of them. */

get_files(dirname)
char	*dirname;
{
	int	add_name();
	struct direct	**nmptr;

	CurDir = dirname;
	scandir(dirname, &nmptr, add_name, (int (*)())0);
}

add_name(dp)
struct direct	*dp;
{
	char	dfile[128],
		rfile[128];
	struct file_pair	*fp;
	struct rec_head		header;
	int	fd;

	if (strncmp(dp->d_name, "jrec", 4) != 0)
		return 0;
	/* If we get here, we found a "recover" tmp file, so now
	   we look for the corresponding "data" tmp file.  First,
	   though, we check to see whether there is anything in
	   the "recover" file.  If it's 0 length, there's no point
	   in saving its name. */
	(void) sprintf(rfile, "%s/%s", CurDir, dp->d_name);
	(void) sprintf(dfile, "%s/jove%s", CurDir, dp->d_name + 4);
	if ((fd = open(rfile, 0)) != -1) {
		if ((read(fd, (char *) &header, sizeof header) != sizeof header)) {
			close(fd);
		    	return 0;
		} else
			close(fd);
	}
	if (access(dfile, 0) != 0) {
		fprintf(stderr, "recover: can't find the data file for %s/%s\n", Directory, dp->d_name);
		fprintf(stderr, "so deleting...\n");
		(void) unlink(rfile);
		(void) unlink(dfile);
		return 0;
	}
	/* If we get here, we've found both files, so we put them
	   in the list. */
	fp = (struct file_pair *) malloc (sizeof *fp);
	if ((char *) fp == 0) {
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

struct rec_entry **
getsrc()
{
	char	name[128];
	int	number;

	for (;;) {
		tellme("Which buffer ('?' for list)? ", name);
		if (name[0] == '?')
			list();
		else if (name[0] == '\0')
			return 0;
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
		return 0;
	return filebuf;
}

#include "ctype.h"

char *
readword(buf)
char	*buf;
{
	int	c;
	char	*bp = buf;

	while (index(" \t\n", c = getchar()))
		;

	do {
		if (index(" \t\n", c))
			break;
		*bp++ = c;
	} while ((c = getchar()) != EOF);
	*bp = 0;

	return buf;
}

tellme(quest, answer)
char	*quest,
	*answer;
{
	if (stdin->_cnt <= 0) {
		printf("%s", quest);
		fflush(stdout);
	}
	readword(answer);
}

/* Print the specified file to strandard output. */

jmp_buf	int_env;

catch()
{
	longjmp(int_env, 1);
}

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

get(src, dest)
struct rec_entry	**src;
char	*dest;
{
	FILE	*outfile;

	if (src == 0 || dest == 0)
		return;
	(void) signal(SIGINT, catch);
	if (setjmp(int_env) == 0) {
		if ((outfile = fopen(dest, "w")) == NULL) {
			printf("recover: cannot create %s.\n", dest);
			return;
		}
		if (dest != tty)
			printf("\"%s\"", dest);
		dump_file(src - buflist, outfile);
	} else
		printf("\nAborted!\n");
	fclose(outfile);
	if (dest != tty)
		printf(" %ld lines, %ld characters.\n", Nlines, Nchars);
	(void) signal(SIGINT, SIG_DFL);
}

char **
scanvec(args, str)
register char	**args,
		*str;
{
	while (*args) {
		if (strcmp(*args, str) == 0)
			return args;
		args += 1;
	}
	return 0;
}

read_rec(recptr)
struct rec_entry	*recptr;
{
	if (fread((char *) recptr, sizeof *recptr, 1, ptrs_fp) != 1)
		fprintf(stderr, "recover: cannot read record.\n");
}

seekto(which)
{
	struct rec_entry	rec;
	long	offset;
	int	i;

	offset = sizeof (Header) + (Header.Nbuffers * sizeof (rec));
	for (i = 1; i < which; i++)
		offset += buflist[i]->r_nlines * sizeof (disk_line);
	fseek(ptrs_fp, offset, L_SET);
}

makblist()
{
	int	i;

	fseek(ptrs_fp, (long) sizeof (Header), L_SET);
	for (i = 1; i <= Header.Nbuffers; i++) {
		if (buflist[i] == 0)
			buflist[i] = (struct rec_entry *) malloc (sizeof (struct rec_entry));
		read_rec(buflist[i]);
	}
	while (buflist[i]) {
		free((char *) buflist[i]);
		buflist[i] = 0;
		i += 1;
	}
}

disk_line
getaddr(fp)
register FILE	*fp;
{
	register int	nchars = sizeof (disk_line);
	disk_line	addr;
	register char	*cp = (char *) &addr;

	while (--nchars >= 0)
		*cp++ = getc(fp);

	return addr;
}

dump_file(which, out)
FILE	*out;
{
	register int	nlines;
	register disk_line	daddr;
	char	buf[BUFSIZ];

	seekto(which);
	nlines = buflist[which]->r_nlines;
	Nchars = Nlines = 0L;
	while (--nlines >= 0) {
		daddr = getaddr(ptrs_fp);
		getline(daddr, buf);
		Nlines += 1;
		Nchars += 1 + strlen(buf);
		fputs(buf, out);
		if (nlines > 0)
			fputc('\n', out);
	}
	if (out != stdout)
		fclose(out);
}

/* List all the buffers. */

list()
{
	int	i;

	for (i = 1; i <= Header.Nbuffers; i++)
		printf("%d) buffer %s  \"%s\" (%d lines)\n", i,
			buflist[i]->r_bname,
			buflist[i]->r_fname,
			buflist[i]->r_nlines);
}

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
	fread((char *) &Header, sizeof Header, 1, ptrs_fp);
	if (Header.Uid != UserID)
		return 0;

	/* Don't ask about JOVE's that are still running ... */
#ifdef KILL0
	if (kill(Header.Pid, 0) == 0)
		return 0;
#endif /* KILL0 */

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

		    	if ((src = getsrc()) == 0)
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

ask_del(prompt, fp)
char	*prompt;
struct file_pair	*fp;
{
	char	yorn[20];

	tellme(prompt, yorn);
	if (yorn[0] == 'y')
		del_files(fp);
}

del_files(fp)
struct file_pair	*fp;
{
	(void) unlink(fp->file_data);
	(void) unlink(fp->file_rec);
}

#ifdef notdef
savetmps()
{
	struct file_pair	*fp;
	int	status,
		pid;

	if (strcmp(TMP_DIR, REC_DIR) == 0)
		return;		/* Files are moved to the same place. */
	get_files(TMP_DIR);
	for (fp = First; fp != 0; fp = fp->file_next) {
		switch (pid = fork()) {
		case -1:
			fprintf(stderr, "recover: can't fork\n!");
			exit(-1);

		case 0:
			execl("/bin/cp", "cp", fp->file_data, fp->file_rec, 
				  REC_DIR, (char *)0);
			fprintf(stderr, "recover: cannot execl /bin/cp.\n");
			exit(-1);

		default:
			while (wait(&status) != pid)
				;
			if (status != 0)
				fprintf(stderr, "recover: non-zero status (%d) returned from copy.\n", status);
		}
	}
}
#endif

lookup(dir)
char	*dir;
{
	struct file_pair	*fp;
	struct rec_head		header;
	char	yorn[20];
	int	nfound = 0,
		this_one;

	printf("Checking %s ...\n", dir);
	Directory = dir;
	get_files(dir);
	for (fp = First; fp != 0; fp = fp->file_next) {
		nfound += doit(fp);
		if (ptrs_fp)
			(void) fclose(ptrs_fp);
		if (data_fd > 0)
			(void) close(data_fd);
	}
	return nfound;
}

main(argc, argv)
int	argc;
char	*argv[];
{
	int	nfound;
	char	**argvp;

	UserID = getuid();

	if (scanvec(argv, "-help")) {
		printf("recover: usage: recover [-d directory]\n");
		printf("Use \"recover\" after JOVE has died for some\n");
		printf("unknown reason.\n\n");
/*		printf("Use \"recover -syscrash\" when the system is in the process\n");
		printf("of rebooting.  This is done automatically at reboot time\n");
		printf("and so most of you don't have to worry about that.\n\n");
 */
		printf("Use \"recover -d directory\" when the tmp files are store\n");
		printf("in DIRECTORY instead of the default one (/tmp).\n");
		exit(0);
	}
	if (scanvec(argv, "-v"))
		Verbose = YES;
/*	if (scanvec(argv, "-syscrash")) {
		printf("Recovering jove files ... ");
		savetmps();
		printf("Done.\n");
		exit(0);
	} */
	if (argvp = scanvec(argv, "-uid"))
		UserID = atoi(argvp[1]);
	if (argvp = scanvec(argv, "-d"))
		nfound = lookup(argvp[1]);
	else
		nfound = lookup(TmpFilePath);
	if (nfound == 0)
		printf("There's nothing to recover.\n");
}
