/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Nested input source file reader.
 * For terminal input, this also provides a simple command stack.
 */

#include <ctype.h>
#include <pwd.h>
#include "calc.h"
#include "config.h"
#include "hist.h"


#define TTYSIZE		100	/* reallocation size for terminal buffers */
#define DEPTH		10	/* maximum depth of input */
#define IS_READ		1	/* reading normally */
#define IS_REREAD	2	/* reread current character */
#define chartoint(ch)	((ch) & 0xff)	/* make sure char is not negative */
#define READSET_ALLOC	8	/* readset to allocate chunk size */


typedef struct {
	int i_state;		/* state (read, reread) */
	int i_char;		/* currently read char */
	long i_line;		/* line number */
	char *i_str;		/* current string for input (if not NULL) */
	char *i_origstr;	/* original string so it can be freed */
	char *i_ttystr;		/* current character of tty line (or NULL) */
	FILE *i_fp;		/* current file for input (if not NULL) */
	char *i_name;		/* file name if known */
} INPUT;


/* files that calc has read or included */
typedef struct {
	int active;		/* != 0 => active entry, 0 => unused entry */
	char *name;		/* name used to read file */
	char *path;		/* real path used to open file */
	struct stat inode;	/* inode information for file */
} READSET;

static READSET *readset = NULL;		/* array of files read */
static int maxreadset = 0;		/* length of readset */

static int linesize;		/* current max size of input line */
static char *linebuf;		/* current input line buffer */
static char *prompt;		/* current prompt for terminal */
static BOOL noprompt;		/* TRUE if should not print prompt */

static int depth;		/* current input depth */
static INPUT *cip;		/* current input source */
static INPUT inputs[DEPTH];	/* input sources */


static int openfile MATH_PROTO((char *name));
static int ttychar MATH_PROTO((void));
static void closeinput MATH_PROTO((void));
static int isinoderead MATH_PROTO((struct stat *sbuf));
static int findfreeread MATH_PROTO((void));
static int addreadset MATH_PROTO((char *name, char *path, struct stat *sbuf));


/*
 * Open an input file by possibly searching through a path list
 * and also possibly applying the specified extension.  For example:
 * opensearchfile("barf", ".:/tmp", ".c") searches in order for the
 * files "./barf", "./barf.c", "/tmp/barf", and "/tmp/barf.c".
 *
 * Returns -1 if we could not open a file or error.  
 * Returns 1 if file was opened and added to/updated in the readset.   
 * Returns 0 if file was already in the readset and reopen was 0.
 */
int
opensearchfile(name, pathlist, extension, rd_once)
	char *name;		/* file name to be read */
	char *pathlist;		/* list of colon separated paths (or NULL) */
	char *extension;	/* extra extension to try (or NULL) */
	int rd_once;		/* TRUE => do not reread a file */
{
	int i;
	char *cp;
	char path[PATHSIZE+1];	/* name being searched for */
	struct stat statbuf;	/* stat of the path */

	/*
	 * Don't try the extension if the filename already contains it.
	 */
	if (extension) {
		i = strlen(name) - strlen(extension);
		if ((i >= 0) && (strcmp(&name[i], extension) == 0))
			extension = NULL;
	}
	/*
	 * If the name is absolute, or if there is no path list, then
	 * make one which just searches for the name straight.  Then
	 * search through the path list for the file, without and with
	 * the specified extension.
	 */
	if (name[0] == PATHCHAR || 
	    name[0] == HOMECHAR || 
	    (name[0] == DOTCHAR && name[1] == PATHCHAR) || 
	    pathlist == NULL) {
		pathlist = "";
	}
	pathlist--;
	do {
		pathlist++;
		cp = path;
		while (*pathlist && (*pathlist != LISTCHAR))
			*cp++ = *pathlist++;
		if (cp != path)
			*cp++ = PATHCHAR;
		strcpy(cp, name);
		i = openfile(path);
		if ((i < 0) && extension) {
			strcat(path, extension);
			i = openfile(path);
		}
	} while ((i < 0) && *pathlist);

	/* examine what our search produced */
	if (i < 0 || cip->i_fp == NULL) {
		/* cannot find a file to open */
		return -1;
	}
	if (fstat(fileno(cip->i_fp), &statbuf) < 0) {
		/* unable to fstat the open file */
		return -1;
	}

	/* note if we will reopen a file and if that is allowed */
	if (rd_once == TRUE && isinoderead(&statbuf) >= 0) {
		/* file is in readset and reopen is false */
		closeinput();
		return 1;
	}

	/* add this name to the readset */
	if (addreadset(name, path, &statbuf) < 0) {
		/* cannot add to readset */
		closeinput();
		return -1;
	}

	/* file was added to/updated in readset */
	return 0;
}


/*
 * Given a filename with a leading ~, expand it into a home directory for 
 * that user.  This function will malloc the space for the expanded path.
 *
 * If the path is just ~, or begins with ~/, expand it to the home
 * directory of the current user.  If the environment variable $HOME
 * is known, it will be used, otherwise the password file will be
 * consulted.
 *
 * If the path is just ~username, or ~username/, expand it to the home
 * directory of that user by looking it up in the password file.
 *
 * If the password file must be consulted and the username is not found
 * a NULL pointer is returned.
 */
static char *
homeexpand(name)
	char *name;		/* a filename with a leading ~ */
{
	struct passwd *ent;	/* password entry */
	char *home2;		/* fullpath of the home directory */
	char *fullpath;		/* the malloced expanded path */
	char *after;		/* after the ~user or ~ */
	char username[PATHSIZE+1];	/* extratced username */

	/* firewall */
	if (name[0] != HOMECHAR)
		return NULL;

	/*
	 * obtain the home directory component
	 */
	switch (name[1]) {
	case PATHCHAR:		/* ~/... */
	case '\0':		/* ~ */
		home2 = home;
		after = name+1;
		break;
	default:		/* ~username or ~username/... */

		/* extract the username after the ~ */
		after = (char *)strchr(name+2, PATHCHAR);
		if (after == NULL) {
			/* path is just ~username */
			ent = (struct passwd *)getpwnam(name+1);
			if (ent == NULL) {
				/* unknown user */
				return NULL;
			}
			/* just malloc the home directory and return it */
			fullpath = (char *)malloc(strlen(ent->pw_dir)+1);
			strcpy(fullpath, ent->pw_dir);
			return fullpath;
		}
		if (after-name > PATHSIZE+1) {
			/* username is too big */
			return NULL;
		}
		strncpy(username, name+1, after-name-1);
		username[after-name-1] = '\0';

		/* get that user's home directory */
		ent = (struct passwd *)getpwnam(username);
		if (ent == NULL) {
			/* unknown user */
			return NULL;
		}
		home2 = ent->pw_dir;
		break;
	}

	/*
	 * build the fullpath given the home directory
	 */
	fullpath = (char *)malloc(strlen(home2)+strlen(after)+1);
	sprintf(fullpath, "%s%s", home2, after);
	return fullpath;
}


/*
 * f_open - ~-expand a filename and fopen() it
 */
FILE *
f_open(name, mode)
	char *name;		/* the filename to open */
	char *mode;		/* the fopen mode to use */
{
	FILE *fp;		/* open file descriptor */
	char *fullname;		/* file name with HOMECHAR expansion */

	/*
	 * expand ~ if needed
	 */
	if (name[0] == HOMECHAR) {
		fullname = homeexpand(name);
		if (fullname == NULL)
			return NULL;
		fp = fopen(fullname, mode);
		free(fullname);
	} else {
		fp = fopen(name, mode);
	}
	return fp;
}


/*
 * Setup for reading from a input file.
 * Returns -1 if file could not be opened.
 */
static int
openfile(name)
	char *name;		/* file name to be read */
{
	FILE *fp;		/* open file descriptor */

	if (depth >= DEPTH)
		 return -1;
	fp = f_open(name, "r");
	if (fp == NULL)
		 return -1;
	cip++;
	cip->i_state = IS_READ;
	cip->i_char = '\0';
	cip->i_str = NULL;
	cip->i_origstr = NULL;
	cip->i_ttystr = NULL;
	cip->i_fp = fp;
	cip->i_line = 1;
	cip->i_name = (char *)malloc(strlen(name) + 1);
	strcpy(cip->i_name, name);
	depth++;
	return 0;
}


/*
 * Open a string for scanning. String is ended by a null character.
 * String is copied into local memory so it can be trashed afterwards.
 * Returns -1 if cannot open string.
 */
int
openstring(str)
	char *str;		/* string to be opened */
{
	char *cp;		/* copied string */

	if ((depth >= DEPTH) || (str == NULL))
		 return -1;
	cp = (char *)malloc(strlen(str) + 1);
	if (cp == NULL)
		 return -1;
	strcpy(cp, str);
	cip++;
	cip->i_state = IS_READ;
	cip->i_char = '\0';
	cip->i_str = cp;
	cip->i_origstr = cp;
	cip->i_fp = NULL;
	cip->i_name = NULL;
	cip->i_ttystr = NULL;
	cip->i_line = 1;
	depth++;
	return 0;
}


/*
 * Set to read input from the terminal.
 * Returns -1 if there is no more depth for input.
 */
int
openterminal()
{
	if (depth >= DEPTH)
		 return -1;
	cip++;
	cip->i_state = IS_READ;
	cip->i_char = '\0';
	cip->i_str = NULL;
	cip->i_origstr = NULL;
	cip->i_ttystr = NULL;
	cip->i_fp = NULL;
	cip->i_name = NULL;
	cip->i_line = 1;
	depth++;
	return 0;
}


/*
 * Close the current input source.
 */
static void
closeinput()
{
	if (depth <= 0)
		return;
	if (cip->i_origstr)
		free(cip->i_origstr);
	if (cip->i_fp)
		fclose(cip->i_fp);
	if (cip->i_name)
		free(cip->i_name);
	cip--;
	depth--;
}


/*
 * Reset the input sources back to the initial state.
 */
void
resetinput()
{
	while (depth > 0)
		closeinput();
	cip = inputs;
	noprompt = FALSE;
}


/*
 * Set the prompt for terminal input.
 */
void
setprompt(str)
	char *str;
{
	prompt = str;
	noprompt = FALSE;
}


/*
 * Read the next character from the current input source.
 * End of file closes current input source, and returns EOF character.
 */
int
nextchar()
{
	int ch;			/* current input character */

	if (depth == 0)		/* input finished */
		 return EOF;
	if (cip->i_state == IS_REREAD) {	/* rereading current char */
		 ch = cip->i_char;
		 cip->i_state = IS_READ;
		 if (ch == '\n')
			cip->i_line++;
		 return ch;
	}
	if (cip->i_str) {		/* from string */
		ch = chartoint(*cip->i_str++);
		if (ch == '\0')
			ch = EOF;
	} else if (cip->i_fp) {		/* from file */
		ch = fgetc(cip->i_fp);
	} else {			/* from terminal */
		ch = ttychar();
	}
	if (ch == EOF) {		/* fix up end of file */
		closeinput();
		ch = EOF;
	}
	if (depth > 0)
		cip->i_char = ch;	/* save for rereads */
	if (ch == '\n')
		cip->i_line++;
	return ch;
}


/*
 * Read in the next line of input from the current input source.
 * The line is terminated with a null character, and does not contain
 * the final newline character.  The returned string is only valid
 * until the next such call, and so must be copied if necessary.
 * Returns NULL on end of file.
 */
char *
nextline()
{
	char *cp;
	int ch;
	int len;

	cp = linebuf;
	if (linesize == 0) {
		cp = (char *)malloc(TTYSIZE + 1);
		if (cp == NULL)
			math_error("Cannot allocate line buffer");
		linebuf = cp;
		linesize = TTYSIZE;
	}
	len = 0;
	for (;;) {
		noprompt = TRUE;
		ch = nextchar();
		noprompt = FALSE;
		if (ch == EOF)
			return NULL;
		if (ch == '\0')
			continue;
		if (ch == '\n')
			break;
		if (len >= linesize) {
			cp = (char *)realloc(cp, linesize + TTYSIZE + 1);
			if (cp == NULL)
				math_error("Cannot realloc line buffer");
			linebuf = cp;
			linesize += TTYSIZE;
		}
		cp[len++] = (char)ch;
	}
	cp[len] = '\0';
	return linebuf;
}


/*
 * Read the next character from the terminal.
 * The routines in the history module are called so that the user
 * can use a command history and emacs-like editing of the line.
 */
static int
ttychar()
{
	int ch;			/* current char */
	int len;		/* length of current command */
	static char charbuf[1000];

	/*
	 * If we have more to read from the saved command line, then do that.
	 * When we see a newline character, then clear the pointer so we will
	 * read a new line on the next call.
	 */
	if (cip->i_ttystr) {
		ch = chartoint(*cip->i_ttystr++);
		if (ch == '\n')
			cip->i_ttystr = NULL;
		return ch;
	}

	/*
	 * We need another complete line.
	 */
	abortlevel = 0;
	inputwait = TRUE;
	len = hist_getline(noprompt ? "" : prompt, charbuf, sizeof(charbuf));
	if (len == 0) {
		inputwait = FALSE;
		return EOF;
	}
	inputwait = FALSE;

	/*
	 * Handle shell escape if present
	 */
	if (charbuf[0] == '!') {		/* do a shell command */
		char *cmd;

		cmd = charbuf + 1;
		if (*cmd == '\0' || *cmd == '\n')
			cmd = shell;
		system(cmd);
		return '\n';
	}
	hist_saveline(charbuf, len);

	/*
	 * Return the first character of the line, and set up to
	 * return the rest of it with later calls.
	 */
	ch = chartoint(charbuf[0]);
	if (ch != '\n')
		cip->i_ttystr = charbuf + 1;
	return ch;
}


/*
 * Return whether or not the input source is the terminal.
 */
BOOL
inputisterminal()
{
	return ((depth <= 0) || ((cip->i_str == NULL) && (cip->i_fp == NULL)));
}


/*
 * Return the name of the current input file.
 * Returns NULL for terminal or strings.
 */
char *
inputname()
{
	if (depth <= 0)
		return NULL;
	return cip->i_name;
}


/*
 * Return the current line number.
 */
long
linenumber()
{
	if (depth > 0)
		return cip->i_line;
	return 1;
}


/*
 * Restore the next character to be read again on the next nextchar call.
 */
void
reread()
{
	if ((depth <= 0) || (cip->i_state == IS_REREAD))
		return;
	cip->i_state = IS_REREAD;
	if (cip->i_char == '\n')
		cip->i_line--;
}


/*
 * Process all startup files found in the $CALCRC path.
 */
void
runrcfiles()
{
	char path[PATHSIZE+1];	/* name being searched for */
	char *cp;
	char *newcp;
	char *p;
	int i;

	/* execute each file in the list */
	for (cp=calcrc, newcp=(char *)strchr(calcrc, LISTCHAR);
	     cp != NULL && *cp;
	     cp = newcp, 
		 newcp=(newcp) ? (char *)strchr(newcp+1, LISTCHAR) : NULL) {

		/* load file name into the path */
		if (newcp == NULL) {
			strcpy(path, cp);
		} else {
			strncpy(path, cp, newcp-cp);
			path[newcp-cp] = '\0';
		}

		/* find the start of the path */
		p = (path[0] == ':') ? path+1 : path;
		if (p[0] == '\0') {
			continue;
		}

		/* process the current file in the list */
		i = openfile(p);
		if (i < 0)
			continue;
		getcommands(FALSE);
	}
}


/*
 * isinoderead - determine if we have read a given dev/inode
 *
 * This function returns the index of the readset element that matches
 * a given device/inode, -1 otherwise.
 */
static int
isinoderead(sbuf)
	struct stat *sbuf;		/* stat of the inode in question */
{
	int i;

	/* deal with the empty case */
	if (readset == NULL || maxreadset <= 0) {
		/* readset is empty */
		return -1;
	}

	/* scan the entire readset */
	for (i=0; i < maxreadset; ++i) { 
		if (readset[i].active && 
		    sbuf->st_dev == readset[i].inode.st_dev &&
		    sbuf->st_ino == readset[i].inode.st_ino) {
			/* found a match */
			return i;
		}
	}

	/* no match found */
	return -1;
}


/*
 * findfreeread - find the next free readset element
 *
 * This function will return the index of the next free readset element.
 * If needed, this function will allocate new readset elements.
 *
 * This function returns the index of the next free element, or -1.
 */
static int
findfreeread()
{
	int i;

	/* deal with an empty readset case */
	if (readset == NULL || maxreadset <= 0) {

		/* malloc a new readset */
		readset = (READSET *)malloc((READSET_ALLOC+1)*sizeof(READSET));
		if (readset == NULL) {
			return -1;
		}
		maxreadset = READSET_ALLOC;
		for (i=0; i < READSET_ALLOC; ++i) {
			readset[i].active = 0;
		}

		/* return first entry */
		return 0;
	}

	/* try to find a free readset entry */
	for (i=0; i < maxreadset; ++i) {
		if (readset[i].active == 0) {
			/* found a free readset entry */
			return i;
		}
	}

	/* all readset entries are in use, allocate more */
	readset = (READSET *)realloc(readset, 
	    (maxreadset+READSET_ALLOC) * sizeof(READSET));
	if (readset == NULL) {
		return -1;
	}
	for (i=0; i < READSET_ALLOC; ++i) {
		readset[i+maxreadset].active = 0;
	}
	maxreadset += READSET_ALLOC;

	/* return the furst newly allocated free entry */
	return maxreadset-READSET_ALLOC;
}


/*
 * addreadset - add a entry to the readset array if it is not already there
 *
 * This function attempts to add a file into the readset.  If the readset
 * has an entry with a matching dev/inode, then that entry is updated with
 * the new name and path.  If no such readset entry is found, a new entry
 * is added.
 *
 * This function returns the index of the readset entry, or -1 if error.
 */
static int
addreadset(name, path, sbuf)
	char *name;	/* name given to read or include */
	char *path;	/* full pathname of file */
	struct stat *sbuf;	/* stat of the path */
{
	int ret;		/* index to return */

	/* find the inode */
	ret = isinoderead(sbuf);
	if (ret < 0) {
		/* not in readset, find a free node */
		ret = findfreeread();
		if (ret < 0) {
			/* cannot find/form a free readset entry */
			return -1;
		}
	} else {
		/* found an readset entry, free old readset data */
		if (readset[ret].name != NULL) {
			free(readset[ret].name);
		}
		if (readset[ret].path != NULL) {
			free(readset[ret].path);
		}
	}

	/* load our information into the readset entry */
	readset[ret].name = (char *)malloc(strlen(name)+1);
	if (readset[ret].name == NULL) {
		return -1;
	}
	strcpy(readset[ret].name, name);
	readset[ret].path = (char *)malloc(strlen(path)+1);
	if (readset[ret].path == NULL) {
		return -1;
	}
	strcpy(readset[ret].path, path);
	readset[ret].inode = *sbuf;
	readset[ret].active = 1;

	/* return index of the newly added entry */
	return ret;
}


/* END CODE */
