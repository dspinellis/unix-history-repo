#ifndef lint
static char	*sccsid = "@(#)misc.c	1.4	(Berkeley) 3/8/86";
#endif

#include "common.h"

/*
 * open_valid_art -- determine if a given article name is valid;
 *		if it is, return a file pointer to the open article,
 *		along with a unique id of the article.
 *
 *	Parameters:	"artname" is a string containing the
 *			name of the article.
 *			"id" is space for us to put the article
 *			id in.
 *
 *	Returns:	File pointer to the open article if the
 *			article is valid; NULL otherwise
 *
 *	Side effects:	None.
 */

FILE *
open_valid_art(artname, id)
char	*artname;
char	*id;
{
	static	int crnt_art_num;
	static	char crnt_art_id[MAX_STRLEN];
	int	fd;
	struct stat	 statbuf;

	if (art_fp != NULL) {
		if (crnt_art_num == atoi(artname)) {
			if (fseek(art_fp, (long) 0, 0) < 0)
				close_crnt();
			else {
				(void) strcpy(id, crnt_art_id);
				return(art_fp);
			}
		} else 
			close_crnt();
	}

	art_fp = fopen(artname, "r");

	if (art_fp == NULL)
		return(NULL);

	fd = fileno(art_fp);

	if (fstat(fd, (struct stat *) &statbuf) < 0) {
		close_crnt();
		return(NULL);
	}

	if ((statbuf.st_mode & S_IFREG) != S_IFREG) {
		close_crnt();
		return(NULL);
	}

	get_id(art_fp, id);
	(void) strcpy(crnt_art_id, id);
	crnt_art_num = atoi(artname);
	return(art_fp);
}


/*
 * openartbyid -- open an article by message-id.
 *
 *	Parameters:	"msg_id" is the message ID of the
 *			article, enclosed in <>'s.
 *
 *	Returns:	A file pointer to the open article,
 *			or NULL if the article doesn't exist.
 *
 *	Side effects:	Displays article, opens dbm database
 *			(only once, keeps it open after that).
 *			Converts "msg_id" to lower case.
 */

FILE *
openartbyid(msg_id)
char	*msg_id;
{
	char	line[MAX_STRLEN], path[MAX_STRLEN];
	char	*tmp;
	register char	*cp;
	FILE	*art_fp;
#ifdef DBM
	static	int	dbopen;
	datum	fetch();
#else
	static	DBM	*db;		/* History file, dbm version */
#endif
	static	FILE	*hfp;		/* history file, text version */
	datum	key, content;

#ifdef DBM
	if (!dbopen) {
		if (dbminit(HISTORY_FILE) < 0) {
			syslog(LOG_ERR, "nntpd: openartbyid: dbminit %s: %m\n",
				HISTORY_FILE);
			return (NULL);
		} else
			dbopen = 1;
	}
#else
	if (db == NULL) {
		db = dbm_open(HISTORY_FILE, O_RDONLY, 0);
		if (db == NULL) {
			syslog(LOG_ERR, "nntpd: openartbyid: dbm_open %s: %m\n",
				HISTORY_FILE);
			return (NULL);
		}
	}
#endif

	for (cp = msg_id; *cp != '\0'; ++cp)
		if (isupper(*cp))
			*cp = tolower(*cp);

	key.dptr = msg_id;
	key.dsize = strlen(msg_id) + 1;

#ifdef DBM
	content = fetch(key);
#else
	content = dbm_fetch(db, key);
#endif
	if (content.dptr == NULL)
		return (NULL);

	if (hfp == NULL) {
		hfp = fopen(HISTORY_FILE, "r");
		if (hfp == NULL) {
			syslog(LOG_ERR, "nntpd: message: fopen %s: %m\n",
				HISTORY_FILE);
			return (NULL);
		}
	}

	if (fseek(hfp, (long) *(int *)content.dptr, 0) < 0) {
		syslog(LOG_ERR, "nntpd: message: fseek: %m\n");
		return (NULL);
	}

	(void) fgets(line, sizeof(line), hfp);
	if ((cp = index(line, '\n')) != NULL)
		*cp = '\0';
	cp = index(line, '\t');
	if (cp != NULL)
		cp = index(cp+1, '\t');
	if (cp == NULL) {
		syslog(LOG_ERR,
		"nntpd: message: malformed line in history file (%d bytes)\n",
		(int) *(int *)content.dptr);
		return (NULL);
	}
	tmp = cp+1;

	if ((cp = index(tmp, ' ')) != NULL)
		*cp = '\0';
	
	while ((cp = index(tmp, '.')) != NULL)
		*cp = '/';

	(void) strcpy(path, homedir);
	(void) strcat(path, "/");
	(void) strcat(path, tmp);

	art_fp = fopen(path, "r");
	return (art_fp);

}


/*
 * spew -- spew out the contents of a file to stdout, doing
 * the necessary cr-lf additions at the end.  Finish with
 * a "." on a line by itself, and an fflush(stdout).
 *
 *	Parameters:	"how" tells what part of the file we
 *			want spewed:
 *				ARTICLE   The entire thing.
 *				HEAD	  Just the first part.
 *				BODY	  Just the second part.
 *			"fp" is the open file to spew from.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Changes current position in file.
 */

spew(fp, how)
FILE	*fp;
int	how;
{
	char	line[512];
	register char	*cp;

#ifdef LOG
	++arts_acsd;
#endif

	if (how == STAT) {
		(void) fflush(stdout);
		return;
	}

	while (fgets(line, sizeof(line)-6, fp) != NULL && *line != '\n') {
		if (how == BODY)	/* We need to skip this anyway */
			continue;
		cp = index(line, '\n');
		if (cp != NULL)
			*cp = '\0';
		if (*line == '.')
			putchar('.');
		printf("%s\r\n", line);
		if (cp == NULL) {
			for (;;) {
				if ((fgets(line, sizeof(line)-6, fp) == NULL)
				    || (index(line, '\n') != NULL))
					break;
			}
		}
	}

	if (how == HEAD) {
		putchar('.');
		putchar('\r');
		putchar('\n');
		(void) fflush(stdout);
		return;
	} else if (how == ARTICLE) {
		putchar('\r');
		putchar('\n');
	}

	while (fgets(line, sizeof(line)-6, fp) != NULL) {
		cp = index(line, '\n');
		if (cp != NULL)
			*cp = '\0';
		if (*line == '.')
			putchar('.');
		printf("%s\r\n", line);

		if (cp == NULL) {
			for (;;) {
				if ((fgets(line, sizeof(line)-6, fp) == NULL)
				    || (index(line, '\n') != NULL))
					break;
			}
		}
	}
	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
}


/*
 * get_id -- get the message id of the current article.
 *
 *	Parameters:	"art_fp" is a pointer to the open file.
 *			"id" is space for the message ID.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Seeks and rewinds on "art_fp".
 *			Changes space pointed to by "id".
 */

get_id(art_fp, id)
register FILE	*art_fp;
char	*id;
{
	char	line[MAX_STRLEN];
	register char	*cp;

	while (fgets(line, sizeof(line), art_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if (*line == '\0')
			break;
		if ((cp = index(line, ' ')) != NULL) {
			*cp = '\0';
			if (streql(line, "Message-ID:")) {
				(void) strcpy(id, cp + 1);
				(void) rewind(art_fp);
				return;
			}
		}
	}
	(void) strcpy(id, "<0>");
	(void) rewind(art_fp);
}
		
/*
 * close_crnt -- close the current article file pointer, if it's
 *	open.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Closes "art_fp" if it's open; sets "art_fp" to NULL.
 */

close_crnt()
{
	if (art_fp != NULL)
		(void) fclose(art_fp);
	art_fp = NULL;
}


/*
 * findart -- find an article number in the article array.
 *
 *	Parameters:	"artname" is a string containing
 *			the name of the article.
 *
 *	Returns:	An index into "art_array",
 *			or -1 if "artname" isn't in "art_array".
 *			
 *	Side effects:	None.
 *
 *	Improvement:	Replace this linear search with a binary one.
 */

findart(artname)
char	*artname;
{
	register int i, artnum;

	artnum = atoi(artname);

	for (i = 0; i < num_arts; ++i)
		if (art_array[i] == artnum)
			return(i);

	return(-1);
}


/*
 * get_distlist -- return a nicely set up array of distribution groups
 * along with a count, when given an NNTP-spec distribution list
 * in the form <dist1,dist2,...,distn>.
 *
 *	Parameters:		"array" is storage for our array,
 *				set to point at some static data.
 *				"list" is the NNTP distribution list.
 *
 *	Returns:		Number of distributions found.
 *				-1 on error.
 *
 *	Side effects:		Changes static data area.
 */

get_distlist(array, list)
char	***array;
char	*list;
{
	char	*cp;
	int	distcount;
	static	char	**dist_list = (char **) NULL;

	if (list[0] != '<')
		return (-1);

	cp = index(list + 1, '>');
	if (cp != NULL)
		*cp = '\0';
	else
		return (-1);

	for (cp = list + 1; *cp != '\0'; ++cp)
		if (*cp == ',')
			*cp = ' ';
	distcount = parsit(list + 1, &dist_list);
	*array = dist_list;
	return (distcount);
}


/*
 * spawn -- create a child process with the input from the client
 * as stdin.
 *
 *	Parameters:	"path" is the path of the program to invoke.
 *			"name" is the name to call the program.
 *			"flag" is a single flag to be passed to the program.
 *			"cont_code" is the response code to transmit
 *			on successful startup.
 *			"err_code" is the response code to transmit when
 *			something goes wrong.
 *
 *	Returns:	-1 on non-zero return from child,
 *			0 on error before fork/exec,
 *			1 otherwise.
 *
 *	Side effects:	Creates and removes temporary file;
 *			accepts input from client; forks and execs.
 */

spawn(path, name, flag, cont_code, err_code)
char	*path;
char	*name;
char	*flag;
int	cont_code;
int	err_code;
{
	char		tempfile[256], line[MAX_STRLEN];
	register char	*cp;
	int		i, nds, fd;
	int		exit_status;
	union wait	status;
	register FILE	*fp;

	(void) strcpy(tempfile, "/tmp/rpostXXXXXX");
	(void) mktemp(tempfile);

	fp = fopen(tempfile, "w");
	if (fp == NULL) {
		printf("%d Cannot create temporary file.\r\n", err_code);
		(void) fflush(stdout);
		return (0);
	} else {
		printf("%d Enter news, period on a line by itself to end.\r\n",
			cont_code);
		(void) fflush(stdout);
	}

	while (fgets(line, sizeof(line), stdin) != NULL) {
		if ((cp = index(line, '\r')) != NULL)
			*cp = '\0';
		else if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';

		if (strcmp(line, ".") == 0)
			break;

		if (line[0] == '.')
			fprintf(fp, "%s\n", line+1);
		else
			fprintf(fp, "%s\n", line);
	}
	(void) fclose(fp);

	/*
	 * Ok, now we have the article in "tempfile".  We
	 * should be able to fork off, close fd's 0 to 31 (or
	 * whatever), open "tempfile" for input, thus making
	 * it stdin, and then execl the inews.  We think.
	 */

	if (fork() == 0) {		/* We're in child */
#ifdef POSTER
		(void) setuid(uid_poster);
		(void) setgid(gid_poster);
#endif

		nds = getdtablesize();
		for (i = 0; i < nds; ++i)
			(void) close(i);
		fd = open(tempfile, O_RDONLY);
		if (fd != 0) {
			(void) dup2(fd, 0);
			(void) close(fd);
		}
		fd = open("/", O_RDONLY);
		if (fd != 1) {
			(void) dup2(fd, 1);
			(void) close(fd);
		}
		(void) dup2(1, 2);

		execl(path, name, flag, (char *) NULL);
		exit(-1);	/* Error */
	} else {
		while (wait(&status) > 0)
			exit_status = status.w_T.w_Retcode;
		(void) unlink(tempfile);
		(void) fflush(stdout);
		return (exit_status ? -1 : 1);
	}
}


/*
 * streql -- determine if two strings are equal, ignoring case.
 *
 *	Parameters:	"a" and "b" are the pointers
 *			to characters to be compared.
 *
 *	Returns:	1 if the strings are equal, 0 otherwise.
 *
 *	Side effects:	None.
 */

streql(a, b)
register char *a, *b;
{
	char lower();

	while (lower(*a) == lower(*b)) {
		if (*a == '\0')
			return (1);
		a++;
		b++;
	}
	return (0);
}

/*
 * lower -- convert a character to lower case, if it's
 *	upper case.
 *
 *	Parameters:	"c" is the character to be
 *			converted.
 *
 *	Returns:	"c" if the character is not
 *			upper case, otherwise the lower
 *			case eqivalent of "c".
 *
 *	Side effects:	None.
 */

char lower(c)
register char c;
{
	if (isascii(c) && isupper(c))
		c = c - 'A' + 'a';
	return(c);
}
