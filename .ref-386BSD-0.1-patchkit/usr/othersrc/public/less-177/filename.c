/*
 * Routines to mess around with filenames (and files).
 * Much of this is very OS dependent.
 */

#include <stdio.h>
#include "less.h"

extern char *getenv();

extern int force_open;
extern IFILE curr_ifile;
extern IFILE old_ifile;

/*
 * Return the full pathname of the given file in the "home directory".
 */
	public char *
homefile(filename)
	char *filename;
{
	register char *pathname;
	register char *homedir;

	homedir = getenv("HOME");
#if __MSDOS__
	/*
	 * Most MSDOS users do not have $HOME defined,
	 * so if no $HOME then look for "_less" anywhere 
	 * on search path (always begins at current directory).
	 */
	if (homedir == NULL)
	{
		extern char *searchpath();
		pathname = searchpath(filename);
		if (pathname == NULL)
			return (NULL);
		pathname = save(pathname);
	} else
	{
		pathname = (char *) calloc(strlen(homedir)+strlen(filename)+2, 
					sizeof(char));
		if (pathname == NULL)
			return (NULL);
		sprintf(pathname, "%s\\%s", homedir, filename);
	}
#else
	if (homedir == NULL)
		return (NULL);
	pathname = (char *) calloc(strlen(homedir)+strlen(filename)+2,
				sizeof(char));
	if (pathname == NULL)
		return (NULL);
	sprintf(pathname, "%s/%s", homedir, filename);
#endif
	return (pathname);
}

/*
 * Find out where the help file is.
 */
	public char *
find_helpfile()
{
	register char *helpfile;
#if __MSDOS__
	extern char *searchpath();

	/*
	 * Look in current directory.
	 */
	if (access(HELPFILE,0) == 0)
		return (HELPFILE);
	/*
	 * Find the basename of HELPFILE,
	 * and look for it in each directory in the search path.
	 */
	if ((helpfile = strrchr(HELPFILE, '\\')) == NULL)
		helpfile = HELPFILE;
	else
		helpfile++;
	return (save(searchpath(helpfile)));
#else
	if ((helpfile = getenv("LESSHELP")) != NULL)
		return (save(helpfile));
	return (save(HELPFILE));
#endif
}

/*
 * Expand a string, substituting any "%" with the current filename,
 * and any "#" with the previous filename.
 */
	public char *
fexpand(s)
	char *s;
{
	register char *fr, *to;
	register int n;
	register char *e;

	/*
	 * Make one pass to see how big a buffer we 
	 * need to allocate for the expanded string.
	 */
	n = 0;
	for (fr = s;  *fr != '\0';  fr++)
	{
		switch (*fr)
		{
		case '%':
			n += strlen(get_filename(curr_ifile));
			break;
		case '#':
			if (old_ifile == NULL_IFILE)
			{
				error("No previous file", NULL_PARG);
				return (NULL);
			}
			n += strlen(get_filename(old_ifile));
			break;
		default:
			n++;
			break;
		}
	}

	e = (char *) ecalloc(n+1, sizeof(char));

	/*
	 * Now copy the string, expanding any "%" or "#".
	 */
	to = e;
	for (fr = s;  *fr != '\0';  fr++)
	{
		switch (*fr)
		{
		case '%':
			strcpy(to, get_filename(curr_ifile));
			to += strlen(to);
			break;
		case '#':
			strcpy(to, get_filename(old_ifile));
			to += strlen(to);
			break;
		default:
			*to++ = *fr;
			break;
		}
	}
	*to = '\0';
	return (e);
}

/*
 * Try to determine if a file is "binary".
 * This is just a guess, and we need not try too hard to make it accurate.
 */
	int
bin_file(f)
	int f;
{
	int i;
	int n;
	char data[64];

	n = read(f, data, sizeof(data));
	for (i = 0;  i < n;  i++)
		if (binary_char(data[i]))
			return (1);
	return (0);
}

/*
 * Try to determine the size of a file by seeking to the end.
 */
	static POSITION
seek_filesize(f)
	int f;
{
	offset_t spos;

	spos = lseek(f, (offset_t)0, 2);
	if (spos == BAD_LSEEK)
		return (NULL_POSITION);
	return ((POSITION) spos);
}

/*
 * Expand a filename, substituting any environment variables, etc.
 */
#if GLOB

FILE *popen();

	public char *
glob(filename)
	char *filename;
{
	FILE *f;
	char *p;
	int ch;
	int len;
	char *cmd;
	char *gfilename;

	filename = fexpand(filename);
	if (filename == NULL)
		return (NULL);

	/*
	 * We get the shell to expand the filename for us by passing
	 * an "echo" command to the shell and reading its output.
	 */
	p = getenv("SHELL");
	if (p == NULL || *p == '\0')
	{
		/*
		 * Read the output of <echo filename>.
		 */
		cmd = (char *) ecalloc(strlen(filename)+6, sizeof(char));
		sprintf(cmd, "echo %s", filename);
	} else
	{
		/*
		 * Read the output of <$SHELL -c "echo filename">.
		 */
		cmd = (char *) ecalloc(strlen(p)+strlen(filename)+12, sizeof(char));
		sprintf(cmd, "%s -c \"echo %s\"", p, filename);
	}

	f = popen(cmd, "r");
	free(cmd);
	if (f == NULL)
		return (filename);
	free(filename);

	len = 100;
	gfilename = (char *) ecalloc(len, sizeof(char));
	for (p = gfilename;  ;  p++)
	{
		if ((ch = getc(f)) == '\n' || ch == EOF)
			break;
		if (p - gfilename >= len-1)
		{
			len *= 2;
			*p = '\0';
			p = (char *) ecalloc(len, sizeof(char));
			strcpy(p, gfilename);
			free(gfilename);
			gfilename = p;
			p = gfilename + strlen(gfilename);
		}
		*p = ch;
	}
	*p = '\0';
	pclose(f);
	if (*gfilename == '\0')
		return (NULL);
	return (gfilename);
}

#else

	public char *
glob(filename)
	char *filename;
{
	return (fexpand(filename));
}

#endif


#if STAT

#include <sys/types.h>
#include <sys/stat.h>

/*
 * Returns NULL if the file can be opened and
 * is an ordinary file, otherwise an error message
 * (if it cannot be opened or is a directory, etc.)
 */
	public char *
bad_file(filename)
	char *filename;
{
	register char *m;
	struct stat statbuf;

	if (stat(filename, &statbuf) < 0)
		return (errno_message(filename));

	if (force_open)
		return (NULL);

	if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
	{
		static char is_dir[] = " is a directory";
		m = (char *) ecalloc(strlen(filename) + sizeof(is_dir), 
			sizeof(char));
		strcpy(m, filename);
		strcat(m, is_dir);
		return (m);
	}
	if ((statbuf.st_mode & S_IFMT) != S_IFREG)
	{
		static char not_reg[] = " is not a regular file";
		m = (char *) ecalloc(strlen(filename) + sizeof(not_reg), 
			sizeof(char));
		strcpy(m, filename);
		strcat(m, not_reg);
		return (m);
	}

	return (NULL);
}

/*
 * Return the size of a file, as cheaply as possible.
 * In Unix, we can stat the file.
 */
	public POSITION
filesize(f)
	int f;
{
	struct stat statbuf;

	if (fstat(f, &statbuf) < 0)
		/*
		 * Can't stat; try seeking to the end.
		 */
		return (seek_filesize(f));

	return ((POSITION) statbuf.st_size);
}

#else

/*
 * If we have no way to find out, just say the file is good.
 */
	public char *
bad_file(filename)
	char *filename;
{
	return (NULL);
}

/*
 * We can find the file size by seeking.
 */
	public POSITION
filesize(f)
	int f;
{
	return (seek_filesize(f));
}

#endif
