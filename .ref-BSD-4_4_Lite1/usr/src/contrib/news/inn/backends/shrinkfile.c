/*  $Revision: 1.4 $
**  Shrink files on line boundaries.
**  Written by Landon Curt Noll <chongo@toad.com>, and placed in the
**  public domain.  Rewritten for INN by Rich Salz.
**  Usage:
**	shrinkfile [-s size] [-v] file...
**	-s size		Truncation size (0 default); suffix may be k, m,
**			or g to scale.  Must not be larger than 2^31 - 1.
**	-v		Print status line.
**  Files will be shrunk an end of line boundary.  In no case will the
**  file be longer than size bytes.  If the first line is longer than
**  the absolute value of size, the file will be truncated to zero
**  length.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


#define MAX_SIZE	(OFFSET_T)0x7fffffff



/*
**  Open a safe unique temporary file that will go away when closed.
*/
STATIC FILE *
OpenTemp()
{
    FILE	*F;
    char	*p;
    char	buff[SMBUF];
    int		i;

    /* Get filename. */
    if ((p = getenv("TMPDIR")) == NULL)
	p = "/usr/tmp";
    (void)sprintf(buff, "%s/shrinkXXXXXX", p);
    (void)mktemp(buff);

    /* Open the file. */
    if ((i = open(buff, O_RDWR | O_CREAT | O_EXCL | O_TRUNC, 0600)) < 0) {
	(void)fprintf(stderr, "Can't make temporary file, %s\n",
		strerror(errno));
	exit(1);
    }
    if ((F = fdopen(i, "w+")) == NULL) {
	(void)fprintf(stderr, "Can't fdopen %d, %s\n", i, strerror(errno));
	exit(1);
    }
    (void)unlink(buff);
    return F;
}


/*
**  Does file end with \n?  Assume it does on I/O error, to avoid doing I/O.
*/
STATIC int
EndsWithNewline(F)
    FILE	*F;
{
    int		c;

    if (fseek(F, (OFFSET_T)1, SEEK_END) < 0) {
	(void)fprintf(stderr, "Can't seek to end of file, %s\n",
		strerror(errno));
	return TRUE;
    }

    /* return the actual character or EOF */
    if ((c = fgetc(F)) == EOF) {
	if (ferror(F))
	    (void)fprintf(stderr, "Can't read last byte, %s\n",
		    strerror(errno));
	return TRUE;
    }
    return c == '\n';
}


/*
**  Add a newline to location of a file.
*/
STATIC BOOL
AppendNewline(name)
    char	*name;
{
    FILE	*F;

    if ((F = xfopena(name)) == NULL) {
	(void)fprintf(stderr, "Can't add newline, %s\n", strerror(errno));
	return FALSE;
    }

    if (fputc('\n', F) == EOF
     || fflush(F) == EOF
     || ferror(F)
     || fclose(F) == EOF) {
	(void)fprintf(stderr, "Can't add newline, %s\n", strerror(errno));
	return FALSE;
    }

    return TRUE;
}

/*
**  This routine does all the work.
*/
STATIC BOOL
Process(F, name, size, Changedp)
    FILE	*F;
    char	*name;
    OFFSET_T	size;
    BOOL	*Changedp;
{
    OFFSET_T	len;
    FILE	*tmp;
    struct stat	Sb;
    char	buff[BUFSIZ + 1];
    int		c;
    int		i;

    /* Get the file's size. */
    if (fstat((int)fileno(F), &Sb) < 0) {
	(void)fprintf(stderr, "Can't fstat, %s\n", strerror(errno));
	return FALSE;
    }
    len = Sb.st_size;

    /* Process a zero size request. */
    if (size == 0) {
	if (len > 0) {
	    (void)fclose(F);
	    if ((F = fopen(name, "w")) == NULL) {
		(void)fprintf(stderr, "Can't overwrite, %s\n", strerror(errno));
		return FALSE;
	    }
	    (void)fclose(F);
	    *Changedp = TRUE;
	}
	return TRUE;
    }

    /* See if already small enough. */
    if (len < size) {
	/* Newline already present? */
	if (EndsWithNewline(F)) {
	    (void)fclose(F);
	    return TRUE;
	}

	/* No newline, add it if it fits. */
	if (len < size - 1) {
	    (void)fclose(F);
	    *Changedp = TRUE;
	    return AppendNewline(name);
	}
    }
    else if (!EndsWithNewline(F)) {
	if (!AppendNewline(name)) {
	    (void)fclose(F);
	    return FALSE;
	}
    }

    /* We now have a file that ends with a newline that is bigger than
     * we want.  Starting from {size} bytes from end, move forward
     * until we get a newline. */
    if (fseek(F, -size, SEEK_END) < 0) {
	(void)fprintf(stderr, "Can't fseek, %s\n", strerror(errno));
	(void)fclose(F);
	return FALSE;
    }

    while ((c = getc(F)) != '\n')
	if (c == EOF) {
	    (void)fprintf(stderr, "Can't read, %s\n", strerror(errno));
	    (void)fclose(F);
	    return FALSE;
	}

    /* Copy rest of file to temp. */
    tmp = OpenTemp();
    while ((i = fread((POINTER)buff, (SIZE_T)1, (SIZE_T)sizeof buff, F)) > 0)
	if (fwrite((POINTER)buff, (SIZE_T)1, (SIZE_T)i, tmp) != i) {
	    i = -1;
	    break;
	}
    if (i < 0) {
	(void)fprintf(stderr, "Can't copy to temp file, %s\n",
		strerror(errno));
	(void)fclose(F);
	(void)fclose(tmp);
	return FALSE;
    }

    /* Now copy temp back to original file. */
    (void)fclose(F);
    if ((F = fopen(name, "w")) == NULL) {
	(void)fprintf(stderr, "Can't overwrite, %s\n", strerror(errno));
	(void)fclose(tmp);
	return FALSE;
    }
    (void)fseek(tmp, (OFFSET_T)0, SEEK_SET);

    while ((i = fread((POINTER)buff, (SIZE_T)1, (SIZE_T)sizeof buff, tmp)) > 0)
	if (fwrite((POINTER)buff, (SIZE_T)1, (SIZE_T)i, F) != i) {
	    i = -1;
	    break;
	}
    if (i < 0) {
	(void)fprintf(stderr, "Can't overwrite file, %s\n", strerror(errno));
	(void)fclose(F);
	(void)fclose(tmp);
	return FALSE;
    }

    (void)fclose(F);
    (void)fclose(tmp);
    *Changedp = TRUE;
    return TRUE;
}


/*
**  Convert size argument to numeric value.  Return -1 on error.
*/
STATIC OFFSET_T
ParseSize(p)
    char	*p;
{
    OFFSET_T	scale;
    long	str_num;
    char	*q;

    /* Skip leading spaces */
    while (ISWHITE(*p))
	p++;
    if (*p == '\0')
	return -1;

    /* determine the scaling factor */
    q = &p[strlen(p) - 1];
    switch (*q) {
    default:
	return -1;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	scale = 1;
	break;
    case 'k': case 'K':
	scale = 1024;
	*q = '\0';
	break;
    case 'm': case 'M':
	scale = 1024 * 1024;
	*q = '\0';
	break;
    case 'g': case 'G':
	scale = 1024 * 1024 * 1024;
	*q = '\0';
	break;
    }

    /* Convert string to number. */
    if (sscanf(p, "%ld", &str_num) != 1)
	return -1;
    if (str_num > MAX_SIZE / scale) {
	(void)fprintf(stderr, "Size is too big\n");
	exit(1);
    }

    return scale * str_num;
}


/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: shrinkfile [-s size] [-v] file...\n");
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    BOOL	Changed;
    BOOL	Verbose;
    FILE	*F;
    char	*p;
    int		i;
    OFFSET_T	size;

    /* Set defaults. */
    Verbose = FALSE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "s:v")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 's':
	    if ((size = ParseSize(optarg)) < 0)
		Usage();
	    break;
	case 'v':
	    Verbose = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac == 0)
	Usage();

    while ((p = *av++) != NULL) {
	if ((F = fopen(p, "r")) == NULL) {
	    (void)fprintf(stderr, "Can't open %s, %s\n", p, strerror(errno));
	    continue;
	}

	Changed = FALSE;
	if (!Process(F, p, size, &Changed))
	    (void)fprintf(stderr, "Can't shrink %s\n", p);
	else if (Verbose && Changed)
	    (void)printf("Shrunk %s\n", p);
    }

    exit(0);
    /* NOTREACHED */
}
