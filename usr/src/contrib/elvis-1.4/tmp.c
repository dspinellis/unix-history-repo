/* tmpfile.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains functions which create & readback a TMPFILE */


#include "config.h"
#include <ctype.h>
#include "vi.h"
#if TOS
# include <stat.h>
#else
# if OSK
#  include "osk.h"
# else
#  include <sys/stat.h>
# endif
#endif


#ifndef NO_MODELINE
static void do_modeline(l, stop)
	long	l;	/* line number to start at */
	long	stop;	/* line number to stop at */
{
	char	*str;	/* used to scan through the line */
	char	*start;	/* points to the start of the line */
	char	buf[80];

	/* if modelines are disabled, then do nothing */
	if (!*o_modeline)
	{
		return;
	}

	/* for each line... */
	for (l = 1; l <= stop; l++)
	{
		/* for each position in the line.. */
		for (str = fetchline(l); *str; str++)
		{
			/* if it is the start of a modeline command... */
			if ((str[0] == 'e' && str[1] == 'x'
			  || str[0] == 'v' && str[1] == 'i')
			  && str[2] == ':')
			{
				start = str += 3;

				/* find the end */
				while (*str && *str != ':')
				{
					str++;
				}

				/* if it is a well-formed modeline, execute it */
				if (*str && str - start < sizeof buf)
				{
					strncpy(buf, start, (int)(str - start));
					buf[str - start] = '\0';
					doexcmd(buf);
					break;
				}
			}
		}
	}
}
#endif


/* The FAIL() macro prints an error message and then exits. */
#define FAIL(why,arg)	mode = MODE_EX; msg(why, arg); endwin(); exit(9)

/* This is the name of the temp file */
static char	tmpname[80];

/* This function creates the temp file and copies the original file into it.
 * Returns if successful, or stops execution if it fails.
 */
int tmpstart(filename)
	char		*filename; /* name of the original file */
{
	int		origfd;	/* fd used for reading the original file */
	struct stat	statb;	/* stat buffer, used to examine inode */
	REG BLK		*this;	/* pointer to the current block buffer */
	REG BLK		*next;	/* pointer to the next block buffer */
	int		inbuf;	/* number of characters in a buffer */
	int		nread;	/* number of bytes read */
	REG int		j, k;
	int		i;
	int		sum;	/* used for calculating a checksum for this */
	char		*scan;
	long		nbytes;

	/* switching to a different file certainly counts as a change */
	changes++;
	redraw(MARK_UNSET, FALSE);

	/* open the original file for reading */
	*origname = '\0';
	if (filename && *filename)
	{
		strcpy(origname, filename);
		origfd = open(origname, O_RDONLY);
		if (origfd < 0 && errno != ENOENT)
		{
			msg("Can't open \"%s\"", origname);
			return tmpstart("");
		}
		if (origfd >= 0)
		{
			if (stat(origname, &statb) < 0)
			{
				FAIL("Can't stat \"%s\"", origname);
			}
#if TOS
			if (origfd >= 0 && (statb.st_mode & S_IJDIR))
#else
# if OSK
			if (origfd >= 0 && (statb.st_mode & S_IFDIR))
# else
			if (origfd >= 0 && (statb.st_mode & S_IFMT) != S_IFREG)
# endif
#endif
			{
				msg("\"%s\" is not a regular file", origname);
				return tmpstart("");
			}
		}
		else
		{
			stat(".", &statb);
		}
		if (origfd >= 0)
		{
			origtime = statb.st_mtime;
#if MSDOS || OSK
			if (*o_readonly || !(statb.st_mode & S_IWRITE))
#endif
#if TOS
			if (*o_readonly || (statb.st_mode & S_IJRON))
#endif
#if ANY_UNIX
			if (*o_readonly || !(statb.st_mode &
				  (statb.st_uid != geteuid() ? 0022 : 0200)))
#endif
			{
				setflag(file, READONLY);
			}
		}
		else
		{
			origtime = 0L;
		}
	}
	else
	{
		setflag(file, NOFILE);
		origfd = -1;
		origtime = 0L;
		stat(".", &statb);
	}

	/* generate a checksum from the file's name */
	for (sum = 0, scan = origname + strlen(origname);
	     --scan >= origname && (isascii(*scan) && isalnum(*scan) || *scan == '.');
	     sum = sum + *scan)
	{
	}
	sum &= 0xf;

	/* make a name for the tmp file */
#if MSDOS || TOS
	/* MS-Dos doesn't allow multiple slashes, but supports drives
	 * with current directories.
	 * This relies on TMPNAME beginning with "%s\\"!!!!
	 */
	strcpy(tmpname, o_directory);
	if ((i = strlen(tmpname)) && !strchr(":/\\", tmpname[i-1]))
		tmpname[i++]=SLASH;
	sprintf(tmpname+i, TMPNAME+3, sum, statb.st_ino, statb.st_dev);
#else
	sprintf(tmpname, TMPNAME, o_directory, sum, statb.st_ino, statb.st_dev);
#endif

	/* make sure nobody else is editing the same file */
	if (access(tmpname, 0) == 0)
	{
		if (*origname)
		{
			msg("\"%s\" is busy", filename);
			return tmpstart("");
		}
		FAIL("\"%s\" is busy", filename);
	}

	/* create the temp file */
#if ANY_UNIX
	close(creat(tmpname, 0600));		/* only we can read it */
#else
	close(creat(tmpname, FILEPERMS));	/* anybody body can read it, alas */
#endif
	tmpfd = open(tmpname, O_RDWR | O_BINARY);
	if (tmpfd < 0)
	{
		FAIL("Can't create temporary file, errno=%d", errno);
		return 1;
	}

	/* allocate space for the header in the file */
	write(tmpfd, hdr.c, (unsigned)BLKSIZE);

#ifndef NO_RECYCLE
	/* initialize the block allocator */
	/* This must already be done here, before the first attempt
	 * to write to the new file! GB */
	garbage();
#endif

	/* initialize lnum[] */
	for (i = 1; i < MAXBLKS; i++)
	{
		lnum[i] = INFINITY;
	}
	lnum[0] = 0;

	/* if there is no original file, then create a 1-line file */
	if (origfd < 0)
	{
		hdr.n[0] = 0;	/* invalid inode# denotes new file */

		this = blkget(1); 	/* get the new text block */
		strcpy(this->c, "\n");	/* put a line in it */

		lnum[1] = 1L;	/* block 1 ends with line 1 */
		nlines = 1L;	/* there is 1 line in the file */
		nbytes = 1L;

		if (*origname)
		{
			msg("\"%s\" [NEW FILE]  1 line, 1 char", origname);
		}
		else
		{
			msg("\"[NO FILE]\"  1 line, 1 char");
		}
	}
	else /* there is an original file -- read it in */
	{
		hdr.n[0] = statb.st_ino;
		nbytes = nlines = 0;

		/* preallocate 1 "next" buffer */
		i = 1;
		next = blkget(i);
		inbuf = 0;

		/* loop, moving blocks from orig to tmp */
		for (;;)
		{
			/* "next" buffer becomes "this" buffer */
			this = next;

			/* read [more] text into this block */
			nread = tread(origfd, &this->c[inbuf], BLKSIZE - 1 - inbuf);
			if (nread < 0)
			{
				close(origfd);
				close(tmpfd);
				tmpfd = -1;
				unlink(tmpname);
				FAIL("Error reading \"%s\"", origname);
			}

			/* convert NUL characters to something else */
			for (k = inbuf; k < inbuf + nread; k++)
			{
				if (!this->c[k])
				{
					setflag(file, HADNUL);
					this->c[k] = 0x80;
				}
			}
			inbuf += nread;

			/* if the buffer is empty, quit */
			if (inbuf == 0)
			{
				goto FoundEOF;
			}

#if MSDOS || TOS
/* BAH! MS text mode read fills inbuf, then compresses eliminating \r
   but leaving garbage at end of buf. The same is true for TURBOC. GB. */

			memset(this->c + inbuf, '\0', BLKSIZE - inbuf);
#endif

			/* search backward for last newline */
			for (k = inbuf; --k >= 0 && this->c[k] != '\n';)
			{
			}
			if (k++ < 0)
			{
				if (inbuf >= BLKSIZE - 1)
				{
					k = 80;
				}
				else
				{
					k = inbuf;
				}
			}

			/* allocate next buffer */
			next = blkget(++i);

			/* move fragmentary last line to next buffer */
			inbuf -= k;
			for (j = 0; k < BLKSIZE; j++, k++)
			{
				next->c[j] = this->c[k];
				this->c[k] = 0;
			}

			/* if necessary, add a newline to this buf */
			for (k = BLKSIZE - inbuf; --k >= 0 && !this->c[k]; )
			{
			}
			if (this->c[k] != '\n')
			{
				setflag(file, ADDEDNL);
				this->c[k + 1] = '\n';
			}

			/* count the lines in this block */
			for (k = 0; k < BLKSIZE && this->c[k]; k++)
			{
				if (this->c[k] == '\n')
				{
					nlines++;
				}
				nbytes++;
			}
			lnum[i - 1] = nlines;
		}
FoundEOF:

		/* if this is a zero-length file, add 1 line */
		if (nlines == 0)
		{
			this = blkget(1); 	/* get the new text block */
			strcpy(this->c, "\n");	/* put a line in it */

			lnum[1] = 1;	/* block 1 ends with line 1 */
			nlines = 1;	/* there is 1 line in the file */
			nbytes = 1;
		}

#if MSDOS || TOS
		/* each line has an extra CR that we didn't count yet */
		nbytes += nlines;
#endif

		/* report the number of lines in the file */
		msg("\"%s\" %s %ld line%s, %ld char%s",
			origname,
			(tstflag(file, READONLY) ? "[READONLY]" : ""),
			nlines,
			nlines == 1 ? "" : "s",
			nbytes,
			nbytes == 1 ? "" : "s");
	}

	/* initialize the cursor to start of line 1 */
	cursor = MARK_FIRST;

	/* close the original file */
	close(origfd);

	/* any other messages? */
	if (tstflag(file, HADNUL))
	{
		msg("This file contained NULs.  They've been changed to \\x80 chars");
	}
	if (tstflag(file, ADDEDNL))
	{
		msg("Newline characters have been inserted to break up long lines");
	}

#ifndef NO_MODELINE
	if (nlines > 10)
	{
		do_modeline(1L, 5L);
		do_modeline(nlines - 4L, nlines);
	}
	else
	{
		do_modeline(1L, nlines);
	}
#endif
	return 0;
}



/* This function copies the temp file back onto an original file.
 * Returns TRUE if successful, or FALSE if the file could NOT be saved.
 */
int tmpsave(filename, bang)
	char	*filename;	/* the name to save it to */
	int	bang;		/* forced write? */
{
	int		fd;	/* fd of the file we're writing to */
	REG int		len;	/* length of a text block */
	REG BLK		*this;	/* a text block */
	long		bytes;	/* byte counter */
	REG int		i;

	/* if no filename is given, assume the original file name */
	if (!filename || !*filename)
	{
		filename = origname;
	}

	/* if still no file name, then fail */
	if (!*filename)
	{
		msg("Don't know a name for this file -- NOT WRITTEN");
		return FALSE;
	}

	/* can't rewrite a READONLY file */
	if (!strcmp(filename, origname) && *o_readonly && !bang)
	{
		msg("\"%s\" [READONLY] -- NOT WRITTEN", filename);
		return FALSE;
	}

	/* open the file */
	if (*filename == '>' && filename[1] == '>')
	{
		filename += 2;
		while (*filename == ' ' || *filename == '\t')
		{
			filename++;
		}
#ifdef O_APPEND
		fd = open(filename, O_WRONLY|O_APPEND);
#else
		fd = open(filename, O_WRONLY);
		lseek(fd, 0L, 2);
#endif
	}
	else
	{
		/* either the file must not exist, or it must be the original
		 * file, or we must have a bang
		 */
		if (strcmp(filename, origname) && access(filename, 0) == 0 && !bang)
		{
			msg("File already exists - Use :w! to overwrite");
			return FALSE;
		}
		fd = creat(filename, FILEPERMS);
	}
	if (fd < 0)
	{
		msg("Can't write to \"%s\" -- NOT WRITTEN", filename);
		return FALSE;
	}

	/* write each text block to the file */
	bytes = 0L;
	for (i = 1; i < MAXBLKS && (this = blkget(i)) && this->c[0]; i++)
	{
		for (len = 0; len < BLKSIZE && this->c[len]; len++)
		{
		}
		twrite(fd, this->c, len);
		bytes += len;
	}

	/* reset the "modified" flag */
	clrflag(file, MODIFIED);
	significant = FALSE;

	/* report lines & characters */
#if MSDOS || TOS
	bytes += nlines; /* for the inserted carriage returns */
#endif
	if (strncmp(filename, o_directory, strlen(o_directory)))
	{
		msg("Wrote \"%s\"  %ld lines, %ld characters", filename, nlines, bytes);
	}

	/* close the file */
	close(fd);

	return TRUE;
}


/* This function deletes the temporary file.  If the file has been modified
 * and "bang" is FALSE, then it returns FALSE without doing anything; else
 * it returns TRUE.
 *
 * If the "autowrite" option is set, then instead of returning FALSE when
 * the file has been modified and "bang" is false, it will call tmpend().
 */
int tmpabort(bang)
	int	bang;
{
	/* if there is no file, return successfully */
	if (tmpfd < 0)
	{
		return TRUE;
	}

	/* see if we must return FALSE -- can't quit */
	if (!bang && tstflag(file, MODIFIED))
	{
		/* if "autowrite" is set, then act like tmpend() */
		if (*o_autowrite)
			return tmpend(bang);
		else
			return FALSE;
	}

	/* delete the tmp file */
	cutswitch(tmpname);
	close(tmpfd);
	tmpfd = -1;
	unlink(tmpname);
	strcpy(prevorig, origname);
	prevline = markline(cursor);
	*origname = '\0';
	origtime = 0L;
	blkinit();
	nlines = 0;
	initflags();
	return TRUE;
}

/* This function saves the file if it has been modified, and then deletes
 * the temporary file. Returns TRUE if successful, or FALSE if the file
 * needs to be saved but can't be.  When it returns FALSE, it will not have
 * deleted the tmp file, either.
 */
int tmpend(bang)
	int	bang;
{
	/* save the file if it has been modified */
	if (tstflag(file, MODIFIED) && !tmpsave((char *)0, FALSE) && !bang)
	{
		return FALSE;
	}

	/* delete the tmp file */
	tmpabort(TRUE);

	return TRUE;
}


/* If the tmp file has been changed, then this function will force those
 * changes to be written to the disk, so that the tmp file will survive a
 * system crash or power failure.
 */
#if MSDOS || TOS || OSK
sync()
{
# if OSK
	/* OS9 doesn't need an explicit sync operation, but the linker
	 * demands something called sync(), so this is a dummy function.
	 */
#else
	/* MS-DOS and TOS don't flush their buffers until the file is closed,
	 * so here we close the tmp file and then immediately reopen it.
	 */
	close(tmpfd);
	tmpfd = open(tmpname, O_RDWR | O_BINARY);
#endif
}
#endif
