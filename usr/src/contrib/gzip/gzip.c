/* gzip (GNU zip) -- compress files with zip algorithm and 'compress' interface
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * The unzip code was written and put in the public domain by Mark Adler.
 * Portions of the lzw code are derived from the public domain 'compress'
 * written by Spencer Thomas, Joe Orost, James Woods, Jim McKie, Steve Davies,
 * Ken Turkowski, Dave Mack and Peter Jannesen.
 *
 * See the license_msg below and the file COPYING for the software license.
 * See the file algorithm.doc for the compression algorithms and file formats.
 */

static char  *license_msg[] = {
"   Copyright (C) 1992-1993 Jean-loup Gailly",
"   This program is free software; you can redistribute it and/or modify",
"   it under the terms of the GNU General Public License as published by",
"   the Free Software Foundation; either version 2, or (at your option)",
"   any later version.",
"",
"   This program is distributed in the hope that it will be useful,",
"   but WITHOUT ANY WARRANTY; without even the implied warranty of",
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
"   GNU General Public License for more details.",
"",
"   You should have received a copy of the GNU General Public License",
"   along with this program; if not, write to the Free Software",
"   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.",
0};

/* Compress files with zip algorithm and 'compress' interface.
 * See usage() and help() functions below for all options.
 * Outputs:
 *        file.z:   compressed file with same mode, owner, and utimes
 *        file.Z:   same with -Z option (old compress format)
 *     or stdout with -c option or if stdin used as input.
 * If the OS does not support file names with multiple dots (MSDOS, VMS) or
 * if the output file name had to be truncated, the original name is kept
 * in the compressed .z file. (Feature not available in old compress format.)
 * On MSDOS, file.tmp -> file.tmz. On VMS, file.tmp -> file.tmp-z.
 *
 * For the meaning of all compilation flags, see comments in Makefile.in.
 */

#ifndef lint
static char rcsid[] = "$Id: gzip.c,v 0.17 1993/03/18 18:14:56 jloup Exp $";
#endif

#include "tailor.h"
#include "gzip.h"
#include "lzw.h"
#include "revision.h"
#include "getopt.h"

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/stat.h>
#include <errno.h>

		/* configuration */

#ifndef NO_FCNTL_H
#  include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#if defined(STDC_HEADERS) || !defined(NO_STDLIB_H)
#  include <stdlib.h>
#else
   extern int errno;
#endif

#if defined(DIRENT) || defined(_POSIX_VERSION)
#  include <dirent.h>
   typedef struct dirent dir_type;
#  define NLENGTH(dirent) ((int)strlen((dirent)->d_name))
#  define DIR_OPT "DIRENT"
#else
#  define NLENGTH(dirent) ((dirent)->d_namlen)
#  ifdef SYSDIR
#    include <sys/dir.h>
     typedef struct direct dir_type;
#    define DIR_OPT "SYSDIR"
#  else
#    ifdef SYSNDIR
#      include <sys/ndir.h>
       typedef struct direct dir_type;
#      define DIR_OPT "SYSNDIR"
#    else
#      ifdef NDIR
#        include <ndir.h>
         typedef struct direct dir_type;
#        define DIR_OPT "NDIR"
#      else
#        define NO_DIR
#        define DIR_OPT "NO_DIR"
#      endif
#    endif
#  endif
#endif

#ifndef NO_UTIME
#  ifndef NO_UTIME_H
#    include <utime.h>
#    define TIME_OPT "UTIME"
#  else
#    ifdef HAVE_SYS_UTIME_H
#      include <sys/utime.h>
#      define TIME_OPT "SYS_UTIME"
#    else
       struct utimbuf {
         time_t actime;
         time_t modtime;
       };
#      define TIME_OPT ""
#    endif
#  endif
#else
#  define TIME_OPT "NO_UTIME"
#endif

#if !defined(S_ISDIR) && defined(S_IFDIR)
#  define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
#  define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

typedef RETSIGTYPE (*sig_type)();

#ifndef	O_BINARY
#  define  O_BINARY  0  /* creation mode for open() */
#endif

#ifndef O_CREAT
   /* Pure BSD system? */
#  include <sys/file.h>
#  ifndef O_CREAT
#    define O_CREAT FCREAT
#  endif
#  ifndef O_EXCL
#    define O_EXCL FEXCL
#  endif
#endif

#define RW_USER 0600    /* creation mode for open() */

#ifndef MAX_PATH_LEN
#  define MAX_PATH_LEN   1024 /* max pathname length */
#endif

#define MAX_HEADER_LEN   16
/* max length of a compressed file header, fixed part only */

		/* global buffers */

DECLARE(uch, inbuf,  INBUFSIZ +INBUF_EXTRA);
DECLARE(uch, outbuf, OUTBUFSIZ+OUTBUF_EXTRA);
DECLARE(ush, d_buf,  DIST_BUFSIZE);
DECLARE(uch, window, 2L*WSIZE);
#ifndef MAXSEG_64K
    DECLARE(ush, tab_prefix, 1L<<BITS);
#else
    DECLARE(ush, tab_prefix0, 1L<<(BITS-1));
    DECLARE(ush, tab_prefix1, 1L<<(BITS-1));
#endif

		/* local variables */

int to_stdout = 0;    /* output to stdout (-c) */
int decompress = 0;   /* decompress (-d) */
int force = 0;        /* don't ask questions, compress links (-f) */
int recursive = 0;    /* recurse through directories (-r) */
int verbose = 0;      /* be verbose (-v) */
int quiet = 0;        /* be very quiet (-q) */
int quit_on_tty = 0;  /* quit if compressing to or decompressing from a tty */
int do_lzw = 0;       /* generate output compatible with old compress (-Z) */
int test = 0;         /* test .z file integrity */
int foreground;       /* set if program run in foreground */
char *progname;       /* program name */
int maxbits = BITS;   /* max bits per code for LZW */
int method = DEFLATED;/* compression method */
int level = 5;        /* compression level */
int exit_code = OK;   /* program exit code */
int save_orig_name;   /* set if original name must be saved */
int last_member;      /* set for .zip and .Z files */
int part_nb;          /* number of parts in .z file */
ulg time_stamp;       /* original time stamp (modification time) */
long ifile_size;      /* input file size, -1 for devices (debug only) */
char *env;            /* contents of GZIP env variable */
char **args = NULL;   /* argv pointer if GZIP env variable defined */

long bytes_in;             /* number of input bytes */
long bytes_out;            /* number of output bytes */
char ifname[MAX_PATH_LEN]; /* input filename */
char ofname[MAX_PATH_LEN]; /* output filename */
int  remove_ofname = 0;	   /* remove output file on error */
struct stat istat;         /* status for input file */
int  ifd;                  /* input file descriptor */
int  ofd;                  /* output file descriptor */
unsigned insize;           /* valid bytes in inbuf */
unsigned inptr;            /* index of next byte to be processed in inbuf */
unsigned outcnt;           /* bytes in output buffer */

struct option longopts[] =
{
 /* { name  has_arg  *flag  val } */
 /* {"ascii",      0, 0, 'a'},  ascii text mode */
    {"stdout",     0, 0, 'c'}, /* write output on standard output */
    {"decompress", 0, 0, 'd'}, /* decompress */
    {"uncompress", 0, 0, 'd'}, /* decompress */
 /* {"encrypt",    0, 0, 'e'},    encrypt */
    {"force",      0, 0, 'f'}, /* force overwrite of output file */
    {"help",       0, 0, 'h'}, /* give help */
 /* {"pkzip",      0, 0, 'k'},    force output in pkzip format */
 /* {"list",       0, 0, 'l'},    list .z file contents */
    {"license",    0, 0, 'L'}, /* display software license */
    {"quiet",      0, 0, 'q'}, /* quiet mode */
    {"recurse",    0, 0, 'r'}, /* recurse through directories */
    {"test",       0, 0, 't'}, /* test compressed file integrity */
    {"verbose",    0, 0, 'v'}, /* verbose mode */
    {"version",    0, 0, 'V'}, /* display version number */
    {"fast",       0, 0, '1'}, /* compress faster */
    {"best",       0, 0, '9'}, /* compress better */
    {"lzw",        0, 0, 'Z'}, /* make output compatible with old compress */
    {"bits",       1, 0, 'b'}, /* max number of bits per code (implies -Z) */
    { 0, 0, 0, 0 }
};

/* local functions */

local void usage        OF((void));
local void help         OF((void));
local void license      OF((void));
local void version      OF((void));
local void treat_stdin  OF((void));
local void treat_file   OF((char *iname));
local int create_outfile OF((void));
local int  do_stat      OF((char *name, struct stat *sbuf));
local char *get_suffix  OF((char *name));
local int  get_istat    OF((char *iname, struct stat *sbuf));
local int  make_ofname  OF((void));
local int  same_file    OF((struct stat *stat1, struct stat *stat2));
local int name_too_long OF((char *name, struct stat *statb));
local int  get_method   OF((int in));
local int  check_ofname OF((void));
local void reset_times  OF((char *name, struct stat *statb));
local void copy_stat    OF((struct stat *ifstat));
local void treat_dir    OF((char *dir));
local void do_exit      OF((int exitcode));
      int main          OF((int argc, char **argv));

void (*work) OF((int infile, int outfile)) = zip; /* function to call */

#define strequ(s1, s2) (strcmp((s1),(s2)) == 0)

/* ======================================================================== */
local void usage()
{
    fprintf(stderr,
#ifdef LZW
#  ifdef NO_DIR
            "usage: %s [-cdfhLtvVZ19] [-b maxbits] [file ...]\n",
#  else
            "usage: %s [-cdfhLrtvVZ19] [-b maxbits] [file ...]\n",
#  endif
#else /* !LZW */
#  ifdef NO_DIR
            "usage: %s [-cdfhLtvV19] [file ...]\n",
#  else
            "usage: %s [-cdfhLrtvV19] [file ...]\n",
#  endif
#endif /* LZW */
             progname);
}
/* ======================================================================== */
local void help()
{
    static char  *help_msg[] = {
/* -a --ascii       ascii text; convert end-of-lines to local OS conventions */
 " -c --stdout      write on standard output, keep original files unchanged",
 " -d --decompress  decompress",
/* -e --encrypt     encrypt */
 " -f --force       force overwrite of output file and compress links",
 " -h --help        give this help",
/* -k --pkzip       force output in pkzip format */
/* -l --list        list .z file contents */
 " -L --license     display software license",
 " -q --quiet       suppress all warnings",
#ifndef NO_DIR
 " -r --recurse     recurse through directories",
#endif
 " -t --test        test compressed file integrity",
 " -v --verbose     verbose mode",
 " -V --version     display version number",
 " -1 --fast        compress faster",
 " -9 --best        compress better",
#ifdef LZW
 " -Z --lzw         produce output compatible with old compress",
 " -b --bits maxbits   max number of bits per code (implies -Z)",
#endif
 " file...          files to (de)compress. If none given, use standard input.",
  0};
    char **p = help_msg;

    fprintf(stderr,"%s %s (%s)\n", progname, VERSION, REVDATE);
    usage();
    while (*p) fprintf(stderr, "%s\n", *p++);
}

/* ======================================================================== */
local void license()
{
    char **p = license_msg;

    fprintf(stderr,"%s %s (%s)\n", progname, VERSION, REVDATE);
    while (*p) fprintf(stderr, "%s\n", *p++);
}

/* ======================================================================== */
local void version()
{
    fprintf(stderr,"%s %s (%s)\n", progname, VERSION, REVDATE);

    fprintf(stderr, "Compilation options:\n%s %s ", DIR_OPT, TIME_OPT);
#ifdef STDC_HEADERS
    fprintf(stderr, "STDC_HEADERS ");
#endif
#ifdef HAVE_UNISTD_H
    fprintf(stderr, "HAVE_UNISTD_H ");
#endif
#ifdef NO_MEMORY_H
    fprintf(stderr, "NO_MEMORY_H ");
#endif
#ifdef NO_STRING_H
    fprintf(stderr, "NO_STRING_H ");
#endif
#ifdef NO_SYMLINK
    fprintf(stderr, "NO_SYMLINK ");
#endif
#ifdef NO_MULTIPLE_DOTS
    fprintf(stderr, "NO_MULTIPLE_DOTS ");
#endif
#ifdef NO_CHOWN
    fprintf(stderr, "NO_CHOWN ");
#endif
#ifdef PROTO
    fprintf(stderr, "PROTO ");
#endif
#ifdef ASMV
    fprintf(stderr, "ASMV ");
#endif
#ifdef DEBUG
    fprintf(stderr, "DEBUG ");
#endif
#ifdef DYN_ALLOC
    fprintf(stderr, "DYN_ALLOC ");
#endif
#ifdef MAXSEG_64K
    fprintf(stderr, "MAXSEG_64K");
#endif
    fprintf(stderr, "\n");
}

/* ======================================================================== */
int main (argc, argv)
    int argc;
    char **argv;
{
    int file_count = 0; /* number of files to precess */
    int proglen;        /* length of progname */
    int optc;           /* current option */

    EXPAND(argc, argv); /* wild card expansion if necessary */

    progname = basename(argv[0]);
    proglen = strlen(progname);

    /* Suppress .exe for MSDOS, OS/2 and VMS: */
    if (proglen > 4 && strequ(progname+proglen-4, ".exe")) {
        progname[proglen-4] = '\0';
    }

    /* Add options in GZIP environment variable if there is one */
    env = add_envopt(&argc, &argv, OPTIONS_VAR);
    if (env != NULL) args = argv;

    foreground = signal(SIGINT, SIG_IGN) != SIG_IGN;
    if (foreground) {
	signal (SIGINT, (sig_type)abort_gzip);
    }
#ifdef SIGTERM
    signal(SIGTERM, (sig_type)abort_gzip);
#endif
#ifdef SIGHUP
    signal(SIGHUP,  (sig_type)abort_gzip);
#endif

#ifndef GNU_STANDARD
    /* For compatibility with old compress, use program name as an option.
     * If you compile with -DGNU_STANDARD, this program will behave as
     * gzip even if it is invoked under the name gunzip or zcat.
     *
     * Systems which do not support links can still use -d or -dc.
     * Ignore an .exe extension for MSDOS, OS/2 and VMS.
     */
    if (  strncmp(progname, "un",  2) == 0     /* ungzip, uncompress */
       || strncmp(progname, "gun", 3) == 0) {  /* gunzip */
	decompress = 1;
    } else if (strequ(progname+1, "cat")       /* zcat, pcat */
	    || strequ(progname, "gzcat")) {    /* gzcat */
	decompress = to_stdout = 1;
    }
#endif

    while ((optc = getopt_long (argc, argv, "b:cdfhLqrtvVZ123456789",
				longopts, (int *)0)) != EOF) {
	switch (optc) {
	case 'b':
	    maxbits = atoi(optarg);
	    break;
	case 'c':
	    to_stdout = 1; break;
	case 'd':
	    decompress = 1; break;
	case 'f':
	    force++; break;
	case 'h':
	    help(); do_exit(OK); break;
	case 'L':
	    license(); do_exit(OK); break;
	case 'q':
	    quiet = 1; verbose = 0; break;
	case 'r':
#ifdef NO_DIR
	    fprintf(stderr, "%s: -r not supported on this system\n", progname);
	    usage();
	    do_exit(ERROR); break;
#else
	    recursive = 1; break;
#endif
	case 't':
	    test = decompress = to_stdout = 1;
	    break;
	case 'v':
	    verbose++; quiet = 0; break;
	case 'V':
	    version(); quit_on_tty = 1; break;
	case 'Z':
#ifdef LZW
	    do_lzw = 1; break;
#else
	    fprintf(stderr, "%s: -Z not supported in this version\n",
		    progname);
	    usage();
	    do_exit(ERROR); break;
#endif
	case '1':  case '2':  case '3':  case '4':
	case '5':  case '6':  case '7':  case '8':  case '9':
	    level = optc - '0';
	    break;
	default:
	    /* Error message already emitted by getopt_long. */
	    usage();
	    do_exit(ERROR);
	}
    } /* loop on all arguments */

    file_count = argc - optind;

    if (do_lzw && !decompress) work = lzw;

    /* Allocate all global buffers (for DYN_ALLOC option) */
    ALLOC(uch, inbuf,  INBUFSIZ +INBUF_EXTRA);
    ALLOC(uch, outbuf, OUTBUFSIZ+OUTBUF_EXTRA);
    ALLOC(ush, d_buf,  DIST_BUFSIZE);
    ALLOC(uch, window, 2L*WSIZE);
#ifndef MAXSEG_64K
    ALLOC(ush, tab_prefix, 1L<<BITS);
#else
    ALLOC(ush, tab_prefix0, 1L<<(BITS-1));
    ALLOC(ush, tab_prefix1, 1L<<(BITS-1));
#endif

    /* And get to work */
    if (file_count != 0) {
	if (to_stdout && !test) {
	    SET_BINARY_MODE(fileno(stdout));
	}
        while (optind < argc) {
	    treat_file(argv[optind++]);
	}
    } else {  /* Standard input */
	treat_stdin();
    }
    do_exit(exit_code);
    return exit_code; /* just to avoid lint warning */
}

/* ========================================================================
 * Compress or decompress stdin
 */
local void treat_stdin()
{
    if (isatty(fileno((FILE *)(decompress ? stdin : stdout)))) {
	/* Do not send compressed data to the terminal or read it from
	 * the terminal. We get here when user invoked the program
	 * without parameters, so be helpful. However, on systems supporting
         * pseudo ttys, let the beginner stare at the 'hung' program
	 * (explicity request from Noah Friedman). Don't give an error message
	 * if the user only wanted the version number (gzip -V).
	 */
	if (quit_on_tty) do_exit(OK);
#ifdef NO_PTY
	fprintf(stderr,
	  "%s: compressed data not %s a terminal. Redirect %s file or pipe.\n",
		progname, decompress ? "read from" : "written to",
		decompress ? "from" : "to");
	fprintf(stderr,"For help, type: %s -h\n", progname);
	do_exit(ERROR);
#endif
    }

    SET_BINARY_MODE(fileno(stdin));
    if (!test) SET_BINARY_MODE(fileno(stdout));

    strcpy(ifname, "stdin");
    strcpy(ofname, "stdout");

    /* Get the time stamp on the input file */
#ifdef NO_STDIN_FSTAT
    time_stamp = 0; /* time unknown */
#else
    if (fstat(fileno(stdin), &istat) != 0) {
	error("fstat(stdin)");
    } 
    time_stamp = istat.st_mtime;
#endif
    ifile_size = -1L; /* convention for unknown size */

    clear_bufs(); /* clear input and output buffers */
    to_stdout = 1;
    part_nb = 0;

    if (decompress) {
	method = get_method(ifd);
	if (method < 0) {
	    do_exit(exit_code); /* error message already emitted */
	}
    }

    /* Actually do the compression/decompression. Loop over zipped members.
     */
    for (;;) {
	(*work)(fileno(stdin), fileno(stdout));

	if (!decompress || last_member || inptr == insize) break;
	/* end of file */

	method = get_method(ifd);
	if (method == -1) return; /* error message already emitted */
	bytes_out = 0;            /* required for length check */
    }

    if (verbose) {
	if (test) {
	    fprintf(stderr, " OK");

	} else if (!decompress) {
	    fprintf(stderr, "Compression: ");
	    display_ratio(bytes_in-bytes_out-overhead, bytes_in);
	}
	fprintf(stderr, "\n");
    }
}

/* ========================================================================
 * Compress or decompress the given file
 */
local void treat_file(iname)
    char *iname;
{
    /* Check if the input file is present, set ifname and istat: */
    if (get_istat(iname, &istat) != 0) return;

    /* If the input name is that of a directory, recurse or ignore: */
    if (S_ISDIR(istat.st_mode)) {
#ifndef NO_DIR
	if (recursive) {
	    struct stat st;
	    st = istat;
	    treat_dir(iname);
	    /* Warning: ifname is now garbage */
	    reset_times (iname, &st);
	} else
#endif
	WARN((stderr,"%s: %s is a directory -- ignored\n", progname, ifname));
	return;
    }
    if (!S_ISREG(istat.st_mode)) {
	WARN((stderr,
	      "%s: %s is not a directory or a regular file - ignored\n",
	      progname, ifname));
	return;
    }
    if (istat.st_nlink > 1 && !to_stdout && !force) {
	WARN((stderr, "%s: %s has %d other link%c -- unchanged\n",
	      progname, ifname,
	      (int)istat.st_nlink - 1, istat.st_nlink > 2 ? 's' : ' '));
	return;
    }

    ifile_size = istat.st_size;
    time_stamp = istat.st_mtime;

    /* Generate output file name */
    if (to_stdout) {
	strcpy(ofname, "stdout");

    } else if (make_ofname() != 0) {
	return;
    }

    /* Open the input file and determine compression method. The mode
     * parameter is ignored but required by some systems (VMS).
     */
    ifd = open(ifname, O_RDONLY | O_BINARY, RW_USER);
    if (ifd == -1) {
	perror(ifname);
	exit_code = ERROR;
	return;
    }
    clear_bufs(); /* clear input and output buffers */
    part_nb = 0;

    if (decompress) {
	method = get_method(ifd); /* updates ofname if original given */
	if (method < 0) {
	    close(ifd);
	    return;               /* error message already emitted */
	}
    }

    /* If compressing to a file, check if ofname is not ambiguous
     * because the operating system truncates names. Otherwise, generate
     * a new ofname and save the original name in the compressed file.
     */
    if (to_stdout) {
	ofd = fileno(stdout);
	/* keep remove_ofname as zero */
    } else {
	if (create_outfile() == -1) return;

	if (save_orig_name && !verbose && !quiet) {
	    fprintf(stderr, "%s: %s compressed to %s\n",
		    progname, ifname, ofname);
	}
    }
    if (verbose) {
	fprintf(stderr, "%s:\t%s", ifname, (int)strlen(ifname) >= 15 ? 
		"" : ((int)strlen(ifname) >= 7 ? "\t" : "\t\t"));
    }

    /* Actually do the compression/decompression. Loop over zipped members.
     */
    for (;;) {
	(*work)(ifd, ofd);

	if (!decompress || last_member || inptr == insize) break;
	/* end of file */

	method = get_method(ifd);
	if (method < 0) break;    /* error message already emitted */
	bytes_out = 0;            /* required for length check */
    }

    close(ifd);
    if (!to_stdout && close(ofd)) {
	write_error();
    }
    if (method == -1) return;     /* error, don't display success msg */

    /* Display statistics */
    if(verbose) {
	if (!decompress) {
	    display_ratio(bytes_in-bytes_out-overhead, bytes_in);
	}
	if (test) {
	    fprintf(stderr, " OK");
	} else if (!to_stdout) {
	    fprintf(stderr, " -- replaced with %s", ofname);
	}
	fprintf(stderr, "\n");
    }
    /* Copy modes, times, ownership */
    if (!to_stdout) {
	copy_stat(&istat);
    }
}

/* ========================================================================
 * Create the output file. Return 0 for success, -1 for error.
 * Try twice if ofname is exactly one beyond the name limit, to avoid
 * creating a compressed file of name "1234567890123."
 * We could actually loop more than once if the user gives an extra long
 * name, but I prefer generating an error then. (Posix forbids the system
 * to truncate names.) The error message is generated by check_ofname()
 * in this case.
 * IN assertions: the input file has already been open (ifd is set) and
 *   ofname has already been updated if there was an original name.
 * OUT assertions: ifd and ofd are closed in case of error.
 */
local int create_outfile()
{
    struct stat	ostat; /* stat for ofname */
    int n;             /* loop counter */

    for (n = 1; n <= 2; n++) {
	if (check_ofname() == -1) {
	    close(ifd);
	    return -1;
	}
	/* Create the output file */
	remove_ofname = 1;
	ofd = open(ofname, O_WRONLY|O_CREAT|O_EXCL|O_BINARY, RW_USER);
	if (ofd == -1) {
	    perror(ofname);
	    close(ifd);
	    exit_code = ERROR;
	    return -1;
	}

	/* Check for name truncation on new file (1234567890123.z) */
	if (fstat(ofd, &ostat) != 0) {
	    fprintf(stderr, "%s: ", progname);
	    perror(ofname);
	    close(ifd); close(ofd);
	    unlink(ofname);
	    exit_code = ERROR;
	    return -1;
	}
	if (!name_too_long(ofname, &ostat)) return 0;

	if (decompress) {
	    /* name might be too long if an original name was saved */
	    WARN((stderr, "%s: %s: warning, name truncated\n",
		  progname, ofname));
	    return 0;
	} else {
#ifdef NO_MULTIPLE_DOTS
	    /* Should never happen, see check_ofname() */
	    fprintf(stderr, "%s: %s: name too long\n", progname, ofname);
	    do_exit(ERROR);
#else
	    close(ofd);
	    unlink(ofname);
	    save_orig_name = 1;
	    strcpy(ofname+strlen(ofname)-Z_LEN-1, Z_SUFFIX);
            /* 1234567890123.z -> 123456789012.z */
#endif
	} /* decompress ? */
    } /* for (n) */

    close(ifd);
    fprintf(stderr, "%s: %s: name too long\n", progname, ofname);
    exit_code = ERROR;
    return -1;
}

/* ========================================================================
 * Use lstat if available, except for -c or -f. Use stat otherwise.
 * This allows links when not removing the original file.
 */
local int do_stat(name, sbuf)
    char *name;
    struct stat *sbuf;
{
#if (defined(S_IFLNK) || defined (S_ISLNK)) && !defined(NO_SYMLINK)
    if (!to_stdout && !force) {
	return lstat(name, sbuf);
    }
#endif
    return stat(name, sbuf);
}

/* ========================================================================
 * Return a pointer to the 'z' suffix of a file name, or NULL.
 * For all systems, ".z", ".Z", ".taz", ".tgz", "-z" are accepted suffixes.
 * ".tgz" is a useful convention for tar.z files on systems limited
 * to 3 characters extensions. On such systems, ".?z" and ".??z" are
 * also accepted suffixes. For Unix, we do not want to accept any
 * .??z suffix as indicating a compressed file; some people use .xyz
 * to denote volume data.
 */
local char *get_suffix(name)
    char *name;
{
    int len;
    char *p = strrchr(name, '.');
    char suffix[10];       /* last few chars of name, forced to lower case */

    if (p == NULL || p == name || strchr(p-1, PATH_SEP) != NULL) return NULL;
    strncpy(suffix, p, sizeof(suffix));
    suffix[sizeof(suffix)-1] = '\0';    /* Force null termination */

#ifdef SUFFIX_SEP
    /* strip a version number from the file name */
    {
	char *v = strrchr(suffix, SUFFIX_SEP);
	if (v != NULL) *v = '\0';
    }
#endif
    strlwr(suffix);
    if (strequ(suffix, ".z") || strequ(suffix, ".zip")
	|| strequ(suffix, ".tgz") || strequ(suffix, ".taz")) {
	return p;
    }
    len = strlen(suffix);
    if (len <= 2) return NULL;

    if (strequ(suffix+len-2, "-z")) return p+len-2;
#ifdef MAX_EXT_CHARS
    if (suffix[len-1] == 'z') return p+len-1;
#endif
    return NULL;
}


/* ========================================================================
 * Set ifname to the input file name (with .z appended if necessary)
 * and istat to its stats. Return 0 if ok, -1 if error.
 */
local int get_istat(iname, sbuf)
    char *iname;
    struct stat *sbuf;
{
    int iexists; /* set if iname exists */
    int ilen = strlen(iname);
    char *suff;

    strcpy(ifname, iname);
    errno = 0;

    /* If input file exists, return OK. */
    if (do_stat(ifname, sbuf) == 0) return 0;

    if (!decompress || errno != ENOENT) {
	perror(ifname);
	exit_code = ERROR;
	return -1;
    }
    /* file.ext doesn't exist, try file.ext.z and file.ext.Z. For MSDOS
     * try file.exz, for VMS try file.ext-z.
     */
    suff = get_suffix(ifname);
    if (suff != NULL) {
	perror(ifname); /* ifname already has z suffix and does not exist */
	exit_code = ERROR;
	return -1;
    }
#ifdef SUFFIX_SEP
    /* strip a version number from the input file name */
    if ((suff = strrchr(ifname, SUFFIX_SEP)) != NULL) *suff = '\0';
#endif
    if (strrchr(ifname, '.') != NULL) {
       strcat(ifname, Z_SUFFIX);
       ilen += Z_LEN;
    } else {
       strcat(ifname, ".z");
       ilen += 2;
    }
    errno = 0;
    iexists = !do_stat(ifname, sbuf);
    if (!iexists) {
	errno = 0;
	ifname[ilen-1] = 'Z';
	iexists = !do_stat(ifname, sbuf);
    }
#ifdef NO_MULTIPLE_DOTS
    /* One more try just to be nice to you */
    if (!iexists) {
	char c = ifname[ilen-2];
	errno = 0;
	strcpy(ifname+ilen-2, "z");
	iexists = !do_stat(ifname, sbuf);
	if (!iexists) {
	    ifname[ilen-2] = c;
	}
    }
#endif
    if (!iexists) {
	ifname[ilen-1] = 'z';
	perror(ifname);
	exit_code = ERROR;
	return -1;
    }
    if (!S_ISREG (sbuf->st_mode)) {
	WARN((stderr, "%s: %s: not a regular file -- ignored\n",
	      progname, ifname));
	return -1;
    }
    return 0; /* ok */
}

/* ========================================================================
 * Generate ofname given ifname. Return 0 if ok, -1 if file must be skipped.
 * Initializes save_orig_name.
 * IN assertion: this function is not called if to_stdout is true.
 */
local int make_ofname()
{
    char *suff;            /* ofname z suffix */

    strcpy(ofname, ifname);
    suff = get_suffix(ofname);

    if (decompress) {
	if (suff == NULL) {
	    WARN((stderr,"%s: %s: no z suffix -- ignored\n",
		  progname, ifname));
	    return -1;
	}
	/* Make a special case for .tgz and .taz: */
	strlwr(suff);
	if (strequ(suff, ".tgz") || strequ(suff, ".taz")) {
	    strcpy(suff, ".tar");
	} else {
	    *suff = '\0'; /* strip z suffix and optional version number */
	}
        /* ofname might be changed later if infile contains an original name */

    } else if (suff != NULL) {
	/* Avoid annoying messages with -r (see treat_dir()) */
	if (verbose || (!recursive && !quiet)) {
	    fprintf(stderr, "%s: %s already has %s suffix -- unchanged\n",
		    progname, ifname, suff);
	}
	if (exit_code == OK) exit_code = WARNING;
	return -1;
    } else {
        save_orig_name = 0;

#ifdef SUFFIX_SEP
	/* strip a version number from the file name */
	if ((suff = strrchr(ofname, SUFFIX_SEP)) != NULL) *suff = '\0';
#endif

#ifdef NO_MULTIPLE_DOTS
	suff = strrchr(ofname, '.');
	if (suff != NULL) {
#  ifdef MAX_EXT_CHARS
	    /* On the Atari and some versions of MSDOS, name_too_long()
	     * does not work correctly because of a bug in stat(). So we
	     * must truncate here.
	     */
	    if (strlen(suff) > MAX_EXT_CHARS) {
		strcpy(suff + MAX_EXT_CHARS, do_lzw ? "Z" : "z");
		save_orig_name = 1;
		return 0;
	    }
#  endif
	    strcat(ofname, Z_SUFFIX);
	    return 0;
	}
#endif
	strcat(ofname, do_lzw ? ".Z" : ".z");

    } /* decompress ? */
    return 0;
}


/* ========================================================================
 * Check the magic number of the input file and update ofname if an
 * original name was given and to_stdout is not set.
 * Return the compression method, -1 for error, -2 for warning.
 * Set inptr to the offset of the next byte to be processed.
 * This function may be called repeatedly for an input file consisting
 * of several contiguous gzip'ed members.
 * IN assertions: there is at least one remaining compressed member.
 *   If the member is a zip file, it must be the only one.
 */
local int get_method(in)
    int in;        /* input file descriptor */
{
    uch flags;
    char magic[2]; /* magic header */

    magic[0] = (char)get_byte();
    magic[1] = (char)get_byte();

    time_stamp = istat.st_mtime; /* may be modified later for some methods */
    method = -1;                 /* unknown yet */
    part_nb++;                   /* number of parts in gzip file */
    last_member = RECORD_IO;
    /* assume multiple members in gzip file except for record oriented I/O */

    if (memcmp(magic, GZIP_MAGIC, 2) == 0
        || memcmp(magic, OLD_GZIP_MAGIC, 2) == 0) {

	work = unzip;
	method = (int)get_byte();
	flags  = (uch)get_byte();

	if ((flags & ENCRYPTED) != 0) {
	    fprintf(stderr,
		    "%s: %s is encrypted -- get newer version of gzip\n",
		    progname, ifname);
	    exit_code = ERROR;
	    return -1;
	}
	if ((flags & CONTINUATION) != 0) {
	    fprintf(stderr,
	   "%s: %s is a a multi-part gzip file -- get newer version of gzip\n",
		    progname, ifname);
	    exit_code = ERROR;
	    if (force <= 1) return -1;
	}
	if ((flags & RESERVED) != 0) {
	    fprintf(stderr,
		    "%s: %s has flags 0x%x -- get newer version of gzip\n",
		    progname, ifname, flags);
	    exit_code = ERROR;
	    if (force <= 1) return -1;
	}
	time_stamp  = (ulg)get_byte();
	time_stamp |= ((ulg)get_byte()) << 8;
	time_stamp |= ((ulg)get_byte()) << 16;
	time_stamp |= ((ulg)get_byte()) << 24;

	(void)get_byte();  /* Ignore extra flags for the moment */
	(void)get_byte();  /* Ignore OS type for the moment */

	if ((flags & CONTINUATION) != 0) {
	    unsigned part = (unsigned)get_byte();
	    part |= ((unsigned)get_byte())<<8;
	    if (verbose) {
		fprintf(stderr,"%s: %s: part number %u\n",
			progname, ifname, part);
	    }
	}
	if ((flags & EXTRA_FIELD) != 0) {
	    unsigned len = (unsigned)get_byte();
	    len |= ((unsigned)get_byte())<<8;
	    if (verbose) {
		fprintf(stderr,"%s: %s: extra field of %u bytes ignored\n",
			progname, ifname, len);
	    }
	    while (len--) (void)get_byte();
	}

	/* Get original file name if it was truncated */
	if ((flags & ORIG_NAME) != 0) {
	    if (to_stdout || part_nb > 1) {
		/* Discard the old name */
		while (get_byte() != 0) /* null */ ;
	    } else {
		/* Copy the base name. Keep a directory prefix intact. */
		char *p = basename(ofname);
		for (;;) {
		    *p = (char)get_byte();
		    if (*p++ == '\0') break;
		    if (p >= ofname+sizeof(ofname)) {
			error("corrupted input -- file name too large");
		    }
		}
	    } /* to_stdout */
	} /* orig_name */

	/* Discard file comment if any */
	if ((flags & COMMENT) != 0) {
	    while (get_byte() != 0) /* null */ ;
	}

    } else if (memcmp(magic, PKZIP_MAGIC, 2) == 0 && inptr == 2
	    && memcmp(inbuf, PKZIP_MAGIC, 4) == 0) {
	/* To simplify the code, we support a zip file when alone only.
         * We are thus guaranteed that the entire local header fits in inbuf.
         */
        inptr = 0;
	work = unzip;
	if (check_zipfile(in) == -1) return -1;
	/* check_zipfile may get ofname from the local header */
	last_member = 1;

    } else if (memcmp(magic, PACK_MAGIC, 2) == 0) {
	work = unpack;
	method = PACKED;
    } else if (memcmp(magic, LZW_MAGIC, 2) == 0) {
	work = unlzw;
	method = COMPRESSED;
	last_member = 1;
    }
    if (method >= 0) return method;
    if (part_nb == 1) {
	fprintf(stderr, "%s: %s is not in gzip format\n", progname, ifname);
	exit_code = ERROR;
	return -1;
    } else {
	WARN((stderr, "%s: %s: trailing garbage ignored\n", progname, ifname));
	return -2;
    }
}

/* ========================================================================
 * Return true if the two stat structures correspond to the same file.
 */
local int same_file(stat1, stat2)
    struct stat *stat1;
    struct stat *stat2;
{
    return stat1->st_mode  == stat2->st_mode
	&& stat1->st_ino   == stat2->st_ino
	&& stat1->st_dev   == stat2->st_dev
	&& stat1->st_uid   == stat2->st_uid
	&& stat1->st_gid   == stat2->st_gid
	&& stat1->st_size  == stat2->st_size
	&& stat1->st_atime == stat2->st_atime
	&& stat1->st_mtime == stat2->st_mtime
	&& stat1->st_ctime == stat2->st_ctime;
}

/* ========================================================================
 * Return true if a file name is ambiguous because the operating system
 * truncates file names.
 */
local int name_too_long(name, statb)
    char *name;           /* file name to check */
    struct stat *statb;   /* stat buf for this file name */
{
    int s = strlen(name);
    char c = name[s-1];
    struct stat	tstat; /* stat for truncated name */
    int res;

    tstat = *statb;      /* Just in case OS does not fill all fields */
    name[s-1] = '\0';
    res = stat(name, &tstat) == 0 && same_file(statb, &tstat);
    name[s-1] = c;
    return res;
}

/* ========================================================================
 * If compressing to a file, check if ofname is not ambigous
 * because the operating system truncates names. Otherwise, generate
 * a new ofname and save the original name in the compressed file.
 * If the compressed file already exists, ask for confirmation.
 *    The check for name truncation is made dynamically, because different
 * file systems on the same OS might use different truncation rules (on SVR4
 * s5 truncates to 14 chars and ufs does not truncate).
 *    This function returns -1 if the file must be skipped, and
 * updates save_orig_name if necessary.
 * IN assertions: save_orig_name is already set if ofname has been
 * already truncated because of NO_MULTIPLE_DOTS. The input file has
 * already been open and istat is set.
 */
local int check_ofname()
{
    int s = strlen(ofname);
    struct stat	ostat; /* stat for ofname */

    if (stat(ofname, &ostat) != 0) return 0;

    /* Check for name truncation on existing file: */
#ifdef NO_MULTIPLE_DOTS
    if (!decompress && name_too_long(ofname, &ostat)) {
#else
    if (!decompress && s > 8 && name_too_long(ofname, &ostat)) {
#endif
	save_orig_name = 1;
#ifdef NO_MULTIPLE_DOTS
	strcpy(ofname+s-Z_LEN-1, Z_SUFFIX);  /* f.extz -> f.exz  */
#else
	strcpy(ofname+s-4, ".z"); /* 12345678901234.z -> 123456789012.z */
#endif
	if (stat(ofname, &ostat) != 0) return 0;
    } /* !decompress && name_too_long */

    /* Check that the input and output files are different (could be
     * the same by name truncation or links).
     */
    if (same_file(&istat, &ostat)) {
	fprintf(stderr, "%s: %s and %s are the same file\n",
		progname, ifname, ofname);
	exit_code = ERROR;
	return -1;
    }
    /* Ask permission to overwrite the existing file */
    if (!force) {
	char response[80];
	strcpy(response,"n");
	fprintf(stderr, "%s: %s already exists;", progname, ofname);
	if (foreground && isatty(fileno(stdin))) {
	    fprintf(stderr, " do you wish to overwrite (y or n)? ");
	    fflush(stderr);
	    (void)read(fileno(stdin), response, sizeof(response));
	}
	if (tolow(*response) != 'y') {
	    fprintf(stderr, "\tnot overwritten\n");
	    if (exit_code == OK) exit_code = WARNING;
	    return -1;
	}
    }
    (void) chmod(ofname, 0777);
    if (unlink(ofname)) {
	fprintf(stderr, "%s: ", progname);
	perror(ofname);
	exit_code = ERROR;
	return -1;
    }
    return 0;
}


/* ========================================================================
 * Set the access and modification times from the given stat buffer.
 */
local void reset_times (name, statb)
    char *name;
    struct stat *statb;
{
#ifndef NO_UTIME
    struct utimbuf	timep;

    /* Copy the time stamp */
    timep.actime  = statb->st_atime;
    timep.modtime = statb->st_mtime;

    if (utime(name, &timep)) {
	WARN((stderr, "%s: ", progname));
	if (!quiet) perror(ofname);
    }
#else
    name = name; statb = statb; /* avoid warnings */
#endif
}


/* ========================================================================
 * Copy modes, times, ownership from input file to output file.
 * IN assertion: to_stdout is false.
 */
local void copy_stat(ifstat)
    struct stat *ifstat;
{
#ifndef NO_UTIME
    time_t diff = ifstat->st_mtime - time_stamp;

    if (diff < 0) diff = -diff;
    if (decompress && diff > 60 && time_stamp != 0) {
	ifstat->st_mtime = time_stamp;
	if (verbose) {
	    fprintf(stderr, "%s: time stamp restored\n", ofname);
	}
    }
    reset_times(ofname, ifstat);
#endif
    /* Copy the protection modes */
    if (chmod(ofname, ifstat->st_mode & 07777)) {
	WARN((stderr, "%s: ", progname));
	if (!quiet) perror(ofname);
    }
#ifndef NO_CHOWN
    chown(ofname, ifstat->st_uid, ifstat->st_gid);  /* Copy ownership */
#endif
    remove_ofname = 0;
    /* It's now safe to remove the input file: */
    (void) chmod(ifname, 0777);
    if (unlink(ifname)) {
	WARN((stderr, "%s: ", progname));
	if (!quiet) perror(ifname);
    }
}

#ifndef NO_DIR

/* ========================================================================
 * Recurse through the given directory. This code is taken from ncompress.
 */
local void treat_dir(dir)
    char *dir;
{
    dir_type *dp;
    DIR      *dirp;
    char     nbuf[MAX_PATH_LEN];

    dirp = opendir(dir);
    
    if (dirp == NULL) {
	fprintf(stderr, "%s: %s unreadable\n", progname, dir);
	exit_code = ERROR;
	return ;
    }
    /*
     ** WARNING: the following algorithm could occasionally cause
     ** compress to produce error warnings of the form "<filename>.z
     ** already has .z suffix - ignored". This occurs when the
     ** .z output file is inserted into the directory below
     ** readdir's current pointer.
     ** These warnings are harmless but annoying, so they are suppressed
     ** with option -r (except when -v is on). An alternative
     ** to allowing this would be to store the entire directory
     ** list in memory, then compress the entries in the stored
     ** list. Given the depth-first recursive algorithm used here,
     ** this could use up a tremendous amount of memory. I don't
     ** think it's worth it. -- Dave Mack
     ** (An other alternative might be two passes to avoid depth-first.)
     */
    
    while ((dp = readdir(dirp)) != NULL) {

	if (strequ(dp->d_name,".") || strequ(dp->d_name,"..")) {
	    continue;
	}
	if (((int)strlen(dir) + NLENGTH(dp) + 1) < (MAX_PATH_LEN - 1)) {
	    strcpy(nbuf,dir);
	    if (strlen(dir) != 0) { /* dir = "" means current dir on Amiga */
#ifdef OTHER_PATH_SEP
		if (dir[strlen(dir)-1] != OTHER_PATH_SEP)
#endif
		strcat(nbuf,"/");
	    }
	    strcat(nbuf,dp->d_name);
	    treat_file(nbuf);
	} else {
	    fprintf(stderr,"%s: %s/%s: pathname too long\n",
		    progname, dir, dp->d_name);
	    exit_code = ERROR;
	}
    }
    closedir(dirp);
}
#endif /* ? NO_DIR */

/* ========================================================================
 * Free all dynamically allocated variables and exit with the given code.
 */
local void do_exit(exitcode)
    int exitcode;
{
    if (env != NULL)  free(env),  env  = NULL;
    if (args != NULL) free(args), args = NULL;
    FREE(inbuf);
    FREE(outbuf);
    FREE(d_buf);
    FREE(window);
#ifndef MAXSEG_64K
    FREE(tab_prefix);
#else
    FREE(tab_prefix0);
    FREE(tab_prefix1);
#endif
    exit(exitcode);
}

/* ========================================================================
 * Signal and error handler.
 */
RETSIGTYPE abort_gzip()
{
   if (remove_ofname) {
       close(ofd);
       unlink (ofname);
   }
   do_exit(ERROR);
}
