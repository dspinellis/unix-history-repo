#ifndef lint
static char sccsid[] = "@(#)compress.c	5.2 (Berkeley) %G%";
#endif not lint

#define	min(a,b)	((a>b) ? b : a)

/*
 * machine variants which require cc -Dmachine:  pdp11, z8000, pcxt
 */

/* Set USERMEM to the maximum amount of physical user memory available
 * in bytes.  USERMEM is used to determine the maximum BITS that can be used
 * for compression.  If USERMEM is big enough, use fast compression algorithm.
 *
 * SACREDMEM is the amount of physical memory saved for others; compress
 * will hog the rest.
 */
#ifndef SACREDMEM
#define SACREDMEM	0
#endif

#ifndef USERMEM
# define USERMEM 	750000	/* default user memory */
#endif

#ifdef pdp11
# define BITS 	12	/* max bits/code for 16-bit machine */
# define NO_UCHAR	/* also if "unsigned char" functions as signed char */
# define SHORT_INT	/* ints are short */
# undef USERMEM 
#endif pdp11

#ifdef z8000
# define BITS 	12
# define SHORT_INT
# undef vax		/* weird preprocessor */
# undef USERMEM 
#endif z8000

#ifdef pcxt
# define BITS   12
# define SHORT_INT
# define SMALL_STACK	/* at least for Latice C, sez Lauren */
# undef USERMEM
#endif pcxt

/* 
 * Define FBITS for machines with several MB of physical memory, to use
 * table lookup for (b <= FBITS).  If FBITS is made too large, performance
 * will decrease due to increased swapping/paging.  Since the program minus
 * the fast lookup table is about a half Meg, we can allocate the rest of
 * available physical memory to the fast lookup table.
 * 
 * If FBITS is set to 12, a 2 MB array is allocated, but only 1 MB is
 * addressed for parity-free input (i.e. text).
 *
 * FBITS=10 yields 1/2 meg lookup table + 4K code memory
 * FBITS=11 yields 1 meg lookup table + 8K code memory
 * FBITS=12 yields 2 meg lookup table + 16K code memory
 * FBITS=13 yields 4 meg lookup table + 32K code memory
 *
 */

#ifdef USERMEM
# if USERMEM >= (2621440+SACREDMEM)
#  if USERMEM >= (4718592+SACREDMEM)
#   define FBITS		13
#   define PBITS	16
#  else 2.5M <= USERMEM < 4.5M
#   define FBITS		12
#   define PBITS	16
#  endif USERMEM <=> 4.5M
# else USERMEM < 2.5M
#  if USERMEM >= (1572864+SACREDMEM)
#   define FBITS		11
#   define PBITS	16
#  else USERMEM < 1.5M
#   if USERMEM >= (1048576+SACREDMEM)
#    define FBITS		10
#    define PBITS	16
#   else USERMEM < 1M
#    if USERMEM >= (631808+SACREDMEM)
#     define PBITS	16
#    else
#     if USERMEM >= (329728+SACREDMEM)
#      define PBITS	15
#     else
#      if USERMEM >= (178176+SACREDMEM)
#       define PBITS	14
#      else
#       if USERMEM >= (99328+SACREDMEM)
#        define PBITS	13
#       else
#        define PBITS	12
#       endif
#      endif
#     endif
#    endif
#    undef USERMEM
#   endif USERMEM <=> 1M
#  endif USERMEM <=> 1.5M
# endif USERMEM <=> 2.5M
#endif USERMEM

#ifdef PBITS		/* Preferred BITS for this memory size */
# ifndef BITS
#  define BITS PBITS
# endif BITS
#endif PBITS

#if BITS == 16
# define HSIZE	69001		/* 95% occupancy */
#endif
#if BITS == 15
# define HSIZE	35023		/* 94% occupancy */
#endif
#if BITS == 14
# define HSIZE	18013		/* 91% occupancy */
#endif
#if BITS == 13
# define HSIZE	9001		/* 91% occupancy */
#endif
#if BITS <= 12
# define HSIZE	5003		/* 80% occupancy */
#endif
/* BITS < 9 will cause an error */

/*
 * a code_int must be able to hold 2**BITS values of type int, and also -1
 */
#if BITS > 15
typedef long int	code_int;
#else
typedef int		code_int;
#endif

#ifdef interdata
typedef unsigned long int count_int;
typedef unsigned short int count_short;
#else
typedef long int	  count_int;
#endif

#ifdef NO_UCHAR
 typedef char	char_type;
#else UCHAR
 typedef	unsigned char	char_type;
#endif UCHAR
char_type magic_header[] = { "\037\235" };	/* 1F 9D */

/* Defines for third byte of header */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80
/* Masks 0x40 and 0x20 are free.  I think 0x20 should mean that there is
   a fourth header byte (for expansion).
*/
#define INIT_BITS 9			/* initial number of bits/code */

/*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors:	Spencer W. Thomas	(decvax!harpo!utah-cs!utah-gr!thomas)
 *		Jim McKie		(decvax!mcvax!jim)
 *		Steve Davies		(decvax!vax135!petsd!peora!srd)
 *		Ken Turkowski		(decvax!decwrl!turtlevax!ken)
 *		James A. Woods		(decvax!ihnp4!ames!jaw)
 *		Joe Orost		(decvax!vax135!petsd!joe)
 *
 * $Header: compress.c,v 3.2 85/06/06 21:53:24 jaw Exp $
 * $Log:	compress.c,v $
 * Revision 3.2  85/06/06  21:53:24  jaw
 * Incorporate portability suggestions for Z8000, IBM PC/XT from mailing list.
 * Default to "quiet" output (no compression statistics).
 *
 * Revision 3.1  85/05/12  18:56:13  jaw
 * Integrate decompress() stack speedups (from early pointer mods by McKie).
 * Repair multi-file USERMEM gaffe.  Unify 'force' flags to mimic semantics
 * of SVR2 'pack'.  Streamline block-compress table clear logic.  Increase 
 * output byte count by magic number size.
 * 
 * Revision 3.0   84/11/27  11:50:00  petsd!joe
 * Set HSIZE depending on BITS.  Set BITS depending on USERMEM.  Unrolled
 * loops in clear routines.  Added "-C" flag for 2.0 compatibility.  Used
 * unsigned compares on Perkin-Elmer.  Fixed foreground check.
 *
 * Revision 2.7   84/11/16  19:35:39  ames!jaw
 * Cache common hash codes based on input statistics; this improves
 * performance for low-density raster images.  Pass on #ifdef bundle
 * from Turkowski.
 *
 * Revision 2.6   84/11/05  19:18:21  ames!jaw
 * Vary size of hash tables to reduce time for small files.
 * Tune PDP-11 hash function.
 *
 * Revision 2.5   84/10/30  20:15:14  ames!jaw
 * Junk chaining; replace with the simpler (and, on the VAX, faster)
 * double hashing, discussed within.  Make block compression standard.
 *
 * Revision 2.4   84/10/16  11:11:11  ames!jaw
 * Introduce adaptive reset for block compression, to boost the rate
 * another several percent.  (See mailing list notes.)
 *
 * Revision 2.3   84/09/22  22:00:00  petsd!joe
 * Implemented "-B" block compress.  Implemented REVERSE sorting of tab_next.
 * Bug fix for last bits.  Changed fwrite to putchar loop everywhere.
 *
 * Revision 2.2   84/09/18  14:12:21  ames!jaw
 * Fold in news changes, small machine typedef from thomas,
 * #ifdef interdata from joe.
 *
 * Revision 2.1   84/09/10  12:34:56  ames!jaw
 * Configured fast table lookup for 32-bit machines.
 * This cuts user time in half for b <= FBITS, and is useful for news batching
 * from VAX to PDP sites.  Also sped up decompress() [fwrite->putc] and
 * added signal catcher [plus beef in writeerr()] to delete effluvia.
 *
 * Revision 2.0   84/08/28  22:00:00  petsd!joe
 * Add check for foreground before prompting user.  Insert maxbits into
 * compressed file.  Force file being uncompressed to end with ".Z".
 * Added "-c" flag and "zcat".  Prepared for release.
 *
 * Revision 1.10  84/08/24  18:28:00  turtlevax!ken
 * Will only compress regular files (no directories), added a magic number
 * header (plus an undocumented -n flag to handle old files without headers),
 * added -f flag to force overwriting of possibly existing destination file,
 * otherwise the user is prompted for a response.  Will tack on a .Z to a
 * filename if it doesn't have one when decompressing.  Will only replace
 * file if it was compressed.
 *
 * Revision 1.9  84/08/16  17:28:00  turtlevax!ken
 * Removed scanargs(), getopt(), added .Z extension and unlimited number of
 * filenames to compress.  Flags may be clustered (-Ddvb12) or separated
 * (-D -d -v -b 12), or combination thereof.  Modes and other status is
 * copied with copystat().  -O bug for 4.2 seems to have disappeared with
 * 1.8.
 *
 * Revision 1.8  84/08/09  23:15:00  joe
 * Made it compatible with vax version, installed jim's fixes/enhancements
 *
 * Revision 1.6  84/08/01  22:08:00  joe
 * Sped up algorithm significantly by sorting the compress chain.
 *
 * Revision 1.5  84/07/13  13:11:00  srd
 * Added C version of vax asm routines.  Changed structure to arrays to
 * save much memory.  Do unsigned compares where possible (faster on
 * Perkin-Elmer)
 *
 * Revision 1.4  84/07/05  03:11:11  thomas
 * Clean up the code a little and lint it.  (Lint complains about all
 * the regs used in the asm, but I'm not going to "fix" this.)
 *
 * Revision 1.3  84/07/05  02:06:54  thomas
 * Minor fixes.
 *
 * Revision 1.2  84/07/05  00:27:27  thomas
 * Add variable bit length output.
 *
 */
#ifndef lint
static char rcs_ident[] = "$Header: compress.c,v 3.1 85/05/12 18:56:13 jaw Exp $";
#endif !lint

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

int n_bits;				/* number of bits/code */
int maxbits = BITS;			/* user settable max # bits/code */
code_int maxcode;			/* maximum code, given n_bits */
code_int maxmaxcode = 1 << BITS;	/* should NEVER generate this code */
#ifdef COMPATIBLE		/* But wrong! */
# define MAXCODE(n_bits)	(1 << (n_bits) - 1)
#else COMPATIBLE
# define MAXCODE(n_bits)	((1 << (n_bits)) - 1)
#endif COMPATIBLE

/*
 * One code could conceivably represent (1<<BITS) characters, but
 * to get a code of length N requires an input string of at least
 * N*(N-1)/2 characters.  With 5000 chars in the stack, an input
 * file would have to contain a 25Mb string of a single character.
 * This seems unlikely.
 */
#ifdef SHORT_INT
# define MAXSTACK    5000		/* size of output stack */
#else !SHORT_INT
# define MAXSTACK    8000		/* size of output stack */
#endif !SHORT_INT

count_int htab [HSIZE];
unsigned short codetab [HSIZE];
code_int hsize = HSIZE;			/* for dynamic table sizing */
count_int fsize;

#define tab_prefix	codetab		/* prefix code for this entry */
char_type  	tab_suffix[1<<BITS];	/* last char in this entry */

#ifdef USERMEM
short ftable [(1 << FBITS) * 256];
count_int fcodemem [1 << FBITS];
#endif USERMEM

code_int free_ent = 0;			/* first unused entry */
int exit_stat = 0;

code_int getcode();

Usage() {
#ifdef DEBUG
fprintf(stderr,"Usage: compress [-dDVfc] [-b maxbits] [file ...]\n");
}
int debug = 0;
#else DEBUG
fprintf(stderr,"Usage: compress [-dfvc] [-b maxbits] [file ...]\n");
}
#endif DEBUG
int nomagic = 0;	/* Use a 3-byte magic number header, unless old file */
int zcat_flg = 0;	/* Write output on stdout, suppress messages */
int quiet = 1;		/* don't tell me about compression */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
int block_compress = BLOCK_MASK;
int clear_flg = 0;
double ratio = 0.0;	/* compression ratio for last block */
#define CHECK_GAP 10000	/* ratio check interval */
count_int checkpoint = CHECK_GAP;
/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */ 
#define FIRST	257	/* first free entry */
#define	CLEAR	256	/* table clear output code */

int force = 0;
char ofname [100];
#ifdef DEBUG
int verbose = 0;
#endif DEBUG
int (*bgnd_flag)();

int nfiles = 0;		/* number of files processed */

/*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Usage: compress [-dfvc] [-b bits] [file ...]
 * Inputs:
 *	-d:	    If given, decompression is done instead.
 *
 *      -c:         Write output on stdout, don't remove original.
 *
 *      -b:         Parameter limits the max number of bits/code.
 *
 *	-f:	    Forces output file to be generated, even if one already
 *		    exists, and even if no space is saved by compressing.
 *		    If -f is not used, the user will be prompted if stdin is
 *		    a tty, otherwise, the output file will not be overwritten.
 *
 *      -v:	    Write compression statistics
 *
 * 	file ...:   Files to be compressed.  If none specified, stdin
 *		    is used.
 * Outputs:
 *	file.Z:	    Compressed form of file with same mode, owner, and utimes
 * 	or stdout   (if stdin used as input)
 *
 * Assumptions:
 *	When filenames are given, replaces with the compressed version
 *	(.Z suffix) only if the file decreases in size.
 * Algorithm:
 * 	Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */
main( argc, argv )
register int argc; char **argv;
{
    int do_decomp = 0;
    int overwrite = 0;	/* Do not overwrite unless given -f flag */
    char tempname[100];
    char **filelist, **fileptr;
    char *cp, *rindex(), *malloc();
    struct stat statbuf;
    extern onintr();


    if ( (bgnd_flag = signal ( SIGINT, SIG_IGN )) != SIG_IGN )
	signal ( SIGINT, onintr );

#ifdef COMPATIBLE
    nomagic = 1;	/* Original didn't have a magic number */
#endif COMPATIBLE

    filelist = fileptr = (char **)(malloc(argc * sizeof(*argv)));
    *filelist = NULL;

    if((cp = rindex(argv[0], '/')) != 0) {
	cp++;
    } else {
	cp = argv[0];
    }
    if(strcmp(cp, "uncompress") == 0) {
	do_decomp = 1;
    } else if(strcmp(cp, "zcat") == 0) {
	do_decomp = 1;
	zcat_flg = 1;
    }

#ifdef BSD4_2
    /* 4.2BSD dependent - take it out if not */
    setlinebuf( stderr );
#endif BSD4_2

    /* Argument Processing
     * All flags are optional.
     * -D => debug
     * -V => debug verbose
     * -d => do_decomp
     * -v => unquiet
     * -f => force overwrite of output file
     * -n => no header: useful to uncompress old files
     * -b maxbits => maxbits.  If -b is specified, then maxbits MUST be
     *	    given also.
     * -c => cat all output to stdout
     * -C => generate output compatible with compress 2.0.
     * if a string is left, must be an input filename.
     */
    for (argc--, argv++; argc > 0; argc--, argv++) {
	if (**argv == '-') {	/* A flag argument */
	    while (*++(*argv)) {	/* Process all flags in this arg */
		switch (**argv) {
#ifdef DEBUG
		    case 'D':
			debug = 1;
			break;
		    case 'V':
			verbose = 1;
			break;
#endif DEBUG
		    case 'v':
			quiet = 0;
			break;
		    case 'd':
			do_decomp = 1;
			break;
		    case 'f':
		    case 'F':
			overwrite = 1;
			force = 1;
			break;
		    case 'n':
			nomagic = 1;
			break;
		    case 'C':
			block_compress = 0;
			break;
		    case 'b':
			if (!ARGVAL()) {
			    fprintf(stderr, "Missing maxbits\n");
			    Usage();
			    exit(1);
			}
			maxbits = atoi(*argv);
			goto nextarg;
		    case 'c':
			zcat_flg = 1;
			break;
		    case 'q':
			quiet = 1;
			break;
		    default:
			fprintf(stderr, "Unknown flag: '%c'; ", **argv);
			Usage();
			exit(1);
		}
	    }
	}
	else {		/* Input file name */
	    *fileptr++ = *argv;	/* Build input file list */
	    *fileptr = NULL;
	    /* goto nextarg; */
	}
	nextarg: continue;
    }

    if(maxbits < INIT_BITS) maxbits = INIT_BITS;
    if (maxbits > BITS) maxbits = BITS;
    maxmaxcode = 1 << maxbits;

    if (*filelist != NULL) {
	for (fileptr = filelist; *fileptr; fileptr++) {
	    exit_stat = 0;
	    if (do_decomp != 0) {			/* DECOMPRESSION */
		/* Check for .Z suffix */
		if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") != 0) {
		    /* No .Z: tack one on */
		    strcpy(tempname, *fileptr);
		    strcat(tempname, ".Z");
		    *fileptr = tempname;
		}
		/* Open input file */
		if ((freopen(*fileptr, "r", stdin)) == NULL) {
			perror(*fileptr); continue;
		}
		/* Check the magic number */
		if (nomagic == 0) {
		    if ((getchar() != (magic_header[0] & 0xFF))
		     || (getchar() != (magic_header[1] & 0xFF))) {
			fprintf(stderr, "%s: not in compressed format\n",
			    *fileptr);
		    continue;
		    }
		    maxbits = getchar();	/* set -b from file */
		    block_compress = maxbits & BLOCK_MASK;
		    maxbits &= BIT_MASK;
		    maxmaxcode = 1 << maxbits;
		    if(maxbits > BITS) {
			fprintf(stderr,
			"%s: compressed with %d bits, can only handle %d bits\n",
			*fileptr, maxbits, BITS);
			continue;
		    }
		}
		/* Generate output filename */
		strcpy(ofname, *fileptr);
		ofname[strlen(*fileptr) - 2] = '\0';  /* Strip off .Z */
	    } else {					/* COMPRESSION */
		if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") == 0) {
		    	fprintf(stderr, "%s: already has .Z suffix -- no change\n",
			    *fileptr);
		    continue;
		}
		/* Open input file */
		if ((freopen(*fileptr, "r", stdin)) == NULL) {
		    perror(*fileptr); continue;
		}
		stat ( *fileptr, &statbuf );
		fsize = (long) statbuf.st_size;
		/*
		 * tune hash table size for small files -- ad hoc
		 */
		hsize = HSIZE;
		if ( fsize < (1 << 12) )
		    hsize = min ( 5003, HSIZE );
		else if ( fsize < (1 << 13) )
		    hsize = min ( 9001, HSIZE );
		else if ( fsize < (1 << 14) )
		    hsize = min ( 18013, HSIZE );
		else if ( fsize < (1 << 15) )
		    hsize = min ( 35023, HSIZE );
		else if ( fsize < 47000 )
		    hsize = min ( 50021, HSIZE );

		/* Generate output filename */
		strcpy(ofname, *fileptr);
#ifndef BSD4_2		/* Short filenames */
		if ((cp=rindex(ofname,'/')) != NULL)	cp++;
		else					cp = ofname;
		if (strlen(cp) > 12) {
		    fprintf(stderr,"%s: filename too long to tack on .Z\n",cp);
		    continue;
		}
#endif  BSD4_2		/* Long filenames allowed */
		strcat(ofname, ".Z");
	    }
	    /* Check for overwrite of existing file */
	    if (overwrite == 0 && zcat_flg == 0) {
		if (stat(ofname, &statbuf) == 0) {
		    char response[2];
		    response[0] = 'n';
		    fprintf(stderr, "%s already exists;", ofname);
		    if (foreground()) {
			fprintf(stderr, " do you wish to overwrite %s (y or n)? ",
			ofname);
			fflush(stderr);
			read(2, response, 2);
			while (response[1] != '\n') {
			    if (read(2, response+1, 1) < 0) {	/* Ack! */
				perror("stderr"); break;
			    }
			}
		    }
		    if (response[0] != 'y') {
			fprintf(stderr, "\tnot overwritten\n");
			continue;
		    }
		}
	    }
	    if(zcat_flg == 0) {		/* Open output file */
		if (freopen(ofname, "w", stdout) == NULL) {
		    perror(ofname);
		    continue;
		}
		if(!quiet)
			fprintf(stderr, "%s: ", *fileptr);
	    }

	    /* Actually do the compression/decompression */
	    if (do_decomp == 0)	compress();
#ifndef DEBUG
	    else			decompress();
#else   DEBUG
	    else if (debug == 0)	decompress();
	    else			printcodes();
	    if (verbose)		dump_tab();
#endif DEBUG
	    if(zcat_flg == 0) {
		copystat(*fileptr, ofname);	/* Copy stats */
		if(exit_stat || (!quiet))
			putc('\n', stderr);
	    }
	}
    } else {		/* Standard input */
	if (do_decomp == 0) {
		compress();
		if(!quiet)
			putc('\n', stderr);
	} else {
	    /* Check the magic number */
	    if (nomagic == 0) {
		if ((getchar()!=(magic_header[0] & 0xFF))
		 || (getchar()!=(magic_header[1] & 0xFF))) {
		    fprintf(stderr, "stdin: not in compressed format\n");
		    exit(1);
		}
		maxbits = getchar();	/* set -b from file */
		block_compress = maxbits & BLOCK_MASK;
		maxbits &= BIT_MASK;
		maxmaxcode = 1 << maxbits;
		fsize = 100000;		/* assume stdin large for USERMEM */
		if(maxbits > BITS) {
			fprintf(stderr,
			"stdin: compressed with %d bits, can only handle %d bits\n",
			maxbits, BITS);
			exit(1);
		}
	    }
#ifndef DEBUG
	    decompress();
#else   DEBUG
	    if (debug == 0)	decompress();
	    else		printcodes();
	    if (verbose)	dump_tab();
#endif DEBUG
	}
    }
    exit(exit_stat);
}

static int offset;
long int in_count = 1;			/* length of input */
long int bytes_out;			/* length of compressed output */
long int out_count = 0;			/* # of codes output (for debugging) */

#define HOG_CHECK ((count_int) 2000)	/* Number of chars to read b4 check */
#define MAX_CACHE ((count_int) 1<<BITS) /* Next line is this constant too */
unsigned short hashcache [1<<BITS];	/* common hash short circuit cache */
count_int cfreq [256];			/* character counts */
#ifndef vax
 char chog;				/* most common character from input */
# define CHOG	' '			/* Assume space is most frequent */
#else 
 int chog;				/* char arith slow on VAX */
# define CHOG	(int) ' '		/* Assume space is most frequent */
#endif
int cstat_flg = 0;			/* on after determining char hog */

/*
 * compress stdin to stdout
 *
 * Algorithm:  on large machines, for maxbits <= FBITS, use fast direct table
 * lookup on the prefix code / next character combination.  For smaller code
 * size, use open addressing modular division double hashing (no chaining), ala
 * Knuth vol. 3, sec. 6.4 Algorithm D, along with G. Knott's relatively-prime
 * secondary probe.  Do block compression with an adaptive reset, whereby the
 * code table is cleared when the compression ratio decreases, but after the
 * table fills.  The variable-length output codes are re-sized at this point,
 * and a special CLEAR code is generated for the decompressor.  For the
 * megamemory version, the sparse array is cleared indirectly through a
 * "shadow" output code history.  Late additions: for the hashing code,
 * construct the table according to file size for noticeable speed improvement
 * on small files.  Also detect and cache codes associated with the most
 * common character to bypass hash calculation on these codes (a characteristic
 * of highly-compressable raster images).  Please direct questions about this
 * implementation to ames!jaw.
 */


compress() {
    register long fcode;
    register code_int i = 0;
    register int c;
    register code_int ent;
    register int disp;
    register code_int hsize_reg;

#ifndef COMPATIBLE
    if (nomagic == 0) {
	putchar(magic_header[0]); putchar(magic_header[1]);
	putchar((char)(maxbits | block_compress));
    }
#endif COMPATIBLE

    offset = 0;
    bytes_out = 3;		/* includes 3-byte header mojo */
    out_count = 0;
    clear_flg = 0;
    ratio = 0.0;
    in_count = 1;
    checkpoint = CHECK_GAP;
    maxcode = MAXCODE(n_bits = INIT_BITS);
    free_ent = ((block_compress) ? FIRST : 256 );
    ent = getchar ();

#ifdef USERMEM
if ( maxbits <= FBITS && (fsize >= 30000) ) {	/* use hashing on small files */

    if ( nfiles++ > 0 ) 			/* clear table between files */
	cl_sparse ();

    while ( (c = getchar()) != (unsigned) EOF ) {
	in_count++;
	fcode = (long) (((long) c << maxbits) + ent);
	if ( ftable [fcode] != 0 )		/* test for code in "string" table */
	    ent = ftable [fcode];
	else {
	    output ( (code_int) ent );
	    out_count++;
	    ent = c;
	    if ( free_ent >= maxmaxcode ) {	
	        if ( (count_int)in_count < checkpoint || (!block_compress) ) 
		    continue;
		else {
		    cl_block ();
		    i = 0;
		}
	    } else {				/* put code in table */
		ftable [fcode] = (short) free_ent++;
		fcodemem [i++] = fcode;		/* memorize for block compression */
	    }
	}
    }
    goto fin;
}
#endif USERMEM

    chog = CHOG;		/* assumed character for the hog */
    cstat_flg = 0;
    hsize_reg = hsize;
    cl_hash( (count_int) hsize_reg);		/* clear hash tables */

    while ( (c = getchar()) != (unsigned) EOF ) {
	in_count++;
	if ( cstat_flg == 0 ) {
	    cfreq [c]++; 	/* gather frequencies at start of input */
	    if ( (count_int)in_count >  HOG_CHECK ) {
	    	cstat_flg = 1;
		chog = hogtally();	/* compute char hog */
		if(chog != CHOG) 	/* fixup for wrong assumption */
		    cl_cache( (count_int) free_ent );
	    }
	}
	if ( c == chog )
	    if ( (i = hashcache [ent]) ) {	/* cache -> code */
	    	ent = i;
	    	continue;
	    }
	fcode = (long) (((long) c << maxbits) + ent);
#ifdef SHORT_INT
	i = (((c + 12347) * ent) & 077777) % HSIZE;	/* avoid 'lrem' call */
#else !SHORT_INT
	i = fcode % hsize_reg;			/* division hashing */
#endif SHORT_INT

	if ( htab [i] == fcode ) {
	    ent = codetab [i];
	    continue;
	} else if ( (long)htab [i] < 0 )	/* empty slot */
	    goto nomatch;
	disp = hsize_reg - i;		/* secondary hash (G. Knott) */
	if ( i == 0 )
	    disp = 1;
probe:
	if ( (i -= disp) < 0 )
	    i += hsize_reg;

	if ( htab [i] == fcode ) {
	    ent = codetab [i];
	    continue;
	}
	if ( (long)htab [i] > 0 ) 
	    goto probe;
nomatch:
	output ( (code_int) ent );
	out_count++;
#ifdef interdata
	if ( (unsigned) free_ent < (unsigned) maxmaxcode) {
#else
	if ( free_ent < maxmaxcode ) {
#endif
	    if ( c == chog )		/* code -> cache */
	        hashcache [ent] = free_ent;
	      				/* code -> hashtable */
	    codetab [i] = free_ent++;
	    htab [i] = fcode;
	}
	else if ( (count_int)in_count >= checkpoint && block_compress )
	    cl_block ();
	ent = c;
    }
fin:
    /*
     * Put out the final code.
     */
    output( (code_int)ent );
    out_count++;
    output( (code_int)-1 );

    /*
     * Print out stats on stderr
     */
    if(zcat_flg == 0 && !quiet) {
#ifdef DEBUG
	fprintf( stderr,
	"%ld chars in, %ld codes (%ld bytes) out, compression factor %g\n",
		in_count, out_count, bytes_out,
		(double)in_count / (double)bytes_out );
	fprintf( stderr, "\tCompression as in compact: %5.2f%%\n",
		100.0 * ( in_count - bytes_out ) / (double) in_count );
	fprintf( stderr, "\tLargest code was %d (%d bits)\n", free_ent - 1, n_bits );
#else DEBUG
	fprintf( stderr, "Compression: %5.2f%%",
		100.0 * ( in_count - bytes_out ) / (double) in_count );
#endif DEBUG
    }
    if(bytes_out > in_count)	/* exit(2) if no savings */
	exit_stat = 2;
    return;
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 * 	code:	A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *		that n_bits =< (long)wordsize - 1.
 * Outputs:
 * 	Outputs code to the file.
 * Assumptions:
 *	Chars are 8 bits long.
 * Algorithm:
 * 	Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static char buf[BITS];

#ifndef vax
char_type lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
char_type rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};
#endif !vax

output( code )
code_int  code;
{
#ifdef DEBUG
    static int col = 0;
#endif DEBUG

    /*
     * On the VAX, it is important to have the register declarations
     * in exactly the order given, or the asm will break.
     */
    register int r_off = offset, bits= n_bits;
    register char * bp = buf;

    if ( code >= 0 ) {
#ifdef vax
	/* VAX DEPENDENT!! Implementation on other machines may be difficult.
	 *
	 * Translation: Insert BITS bits from the argument starting at
	 * offset bits from the beginning of buf.
	 */
	0;	/* C compiler bug ?? */
	asm( "insv	4(ap),r11,r10,(r9)" );
#else not a vax
/* 
 * byte/bit numbering on the VAX is simulated by the following code
 */
	/*
	 * Get to the first byte.
	 */
	bp += (r_off >> 3);
	r_off &= 7;
	/*
	 * Since code is always >= 8 bits, only need to mask the first
	 * hunk on the left.
	 */
	*bp = (*bp & rmask[r_off]) | (code << r_off) & lmask[r_off];
	bp++;
	bits -= (8 - r_off);
	code >>= 8 - r_off;
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
	    *bp++ = code;
	    code >>= 8;
	    bits -= 8;
	}
	/* Last bits. */
	if(bits)
	    *bp = code;
#endif vax
	offset += n_bits;
	if ( offset == (n_bits << 3) ) {
	    bp = buf;
	    bits = n_bits;
	    bytes_out += bits;
	    do
		putchar(*bp++);
	    while(--bits);
	    if (ferror(stdout))
		writeerr();
	    offset = 0;
	}
#ifdef DEBUG
	if ( verbose )
	    fprintf( stderr, "%5d%c", code,
		    (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
#endif DEBUG

	/*
	 * If the next entry is going to be too big for the code size,
	 * then increase it, if possible.
	 */
	if ( free_ent > maxcode || (clear_flg > 0))
	{
	    /*
	     * Write the whole buffer, because the input side won't
	     * discover the size increase until after it has read it.
	     */
	    if ( offset > 0 ) {
		if( fwrite( buf, 1, n_bits, stdout ) != n_bits)
			writeerr();
		bytes_out += n_bits;
	    }
	    offset = 0;

	    if ( clear_flg ) {
    	        maxcode = MAXCODE (n_bits = INIT_BITS);
	        clear_flg = 0;
	    }
	    else {
	    	n_bits++;
	    	if ( n_bits == maxbits )
		    maxcode = maxmaxcode;
	    	else
		    maxcode = MAXCODE(n_bits);
	    }
#ifdef DEBUG
	    if ( debug ) {
		fprintf( stderr, "\nChange to %d bits\n", n_bits );
		col = 0;
	    }
#endif DEBUG
	}
    } else {
	/*
	 * At EOF, write the rest of the buffer.
	 */
	if ( offset > 0 )
	    fwrite( buf, 1, (offset + 7) / 8, stdout );
	bytes_out += (offset + 7) / 8;
	offset = 0;
	fflush( stdout );
#ifdef DEBUG
	if ( verbose )
	    fprintf( stderr, "\n" );
#endif DEBUG
	if( ferror( stdout ) )
		writeerr();
    }
}

#ifdef SMALL_STACK
    char_type stack[MAXSTACK];
#endif

decompress() {
    register char_type *stackp;
    register int finchar;
    register code_int code, oldcode, incode;

#ifndef SMALL_STACK
    char_type stack[MAXSTACK];
#endif

    /*
     * As above, initialize the first 256 entries in the table.
     */
    maxcode = MAXCODE(n_bits = INIT_BITS);
    for ( code = 255; code >= 0; code-- ) {
	tab_prefix[code] = 0;
	tab_suffix[code] = (char_type)code;
    }
    free_ent = ((block_compress) ? FIRST : 256 );

    finchar = oldcode = getcode();
    putchar( (char)finchar );		/* first code must be 8 bits = char */
    stackp = &stack[0];

    while ( (code = getcode()) != -1 ) {

	if ( (code == CLEAR) && block_compress ) {
	    for ( code = 255; code >= 0; code-- )
		tab_prefix[code] = 0;
	    clear_flg = 1;
	    free_ent = FIRST - 1;
	    if ( (code = getcode ()) == -1 )	/* O, untimely death! */
		break;
	}
	incode = code;
	/*
	 * Special case for KwKwK string.
	 */
	if ( code >= free_ent ) {
            *stackp++ = finchar;
	    code = oldcode;
	}

	/*
	 * Generate output characters in reverse order
	 */
#ifdef interdata
	while ( ((unsigned long)code) >= ((unsigned long)256) ) {
#else !interdata
	while ( code >= 256 ) {
#endif interdata
	    *stackp++ = tab_suffix[code];
	    code = tab_prefix[code];
	}
	*stackp++ = finchar = tab_suffix[code];

	/*
	 * And put them out in forward order
	 */
	while ( --stackp > &stack[0] )
	    putchar ( *stackp );
	putchar ( *stackp );
	if (ferror(stdout))
		writeerr ( );

	/*
	 * Generate the new entry.
	 */
	if ( (code=free_ent) < maxmaxcode ) {
	    tab_prefix[code] = (unsigned short)oldcode;
	    tab_suffix[code] = finchar;
	    free_ent = code+1;
	} 
	/*
	 * Remember previous code.
	 */
	oldcode = incode;
    }
    fflush( stdout );
    if(ferror(stdout))
	writeerr();
}

/*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the standard input.  If EOF, return -1.
 * Inputs:
 * 	stdin
 * Outputs:
 * 	code or -1 is returned.
 */

code_int
getcode() {
    /*
     * On the VAX, it is important to have the register declarations
     * in exactly the order given, or the asm will break.
     */
    register code_int code;
    static int offset = 0, size = 0;
    static char_type buf[BITS];
    register int r_off, bits;
    register char_type *bp = buf;

    if ( clear_flg > 0 || offset >= size || free_ent > maxcode ) {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( free_ent > maxcode ) {
	    n_bits++;
	    if ( n_bits == maxbits )
		maxcode = maxmaxcode;	/* won't get any bigger now */
	    else
		maxcode = MAXCODE(n_bits);
	}
	if ( clear_flg > 0) {
    	    maxcode = MAXCODE (n_bits = INIT_BITS);
	    clear_flg = 0;
	}
	size = fread( buf, 1, n_bits, stdin );
	if ( size <= 0 )
	    return -1;			/* end of file */
	offset = 0;
	/* Round size down to integral number of codes */
	size = (size << 3) - (n_bits - 1);
    }
    r_off = offset;
    bits = n_bits;
#ifdef vax
    asm( "extzv   r10,r9,(r8),r11" );
#else not a vax
	/*
	 * Get to the first byte.
	 */
	bp += (r_off >> 3);
	r_off &= 7;
	/* Get first part (low order bits) */
#ifdef NO_UCHAR
	code = ((*bp++ >> r_off) & rmask[8 - r_off]) & 0xff;
#else  NO_UCHAR
	code = (*bp++ >> r_off);
#endif NO_UCHAR
	bits -= (8 - r_off);
	r_off = 8 - r_off;		/* now, offset into code word */
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
#ifdef NO_UCHAR
	    code |= (*bp++ & 0xff) << r_off;
#else  NO_UCHAR
	    code |= *bp++ << r_off;
#endif NO_UCHAR
	    r_off += 8;
	    bits -= 8;
	}
	/* high order bits. */
	code |= (*bp & rmask[bits]) << r_off;
#endif vax
    offset += n_bits;

    return code;
}

char *
rindex(s, c)		/* For those who don't have it in libc.a */
register char *s, c;
{
	char *p;
	for (p = NULL; *s; s++)
	    if (*s == c)
		p = s;
	return(p);
}

#ifdef DEBUG
printcodes()
{
    /*
     * Just print out codes from input file.  For debugging.
     */
    code_int code;
    int col = 0, bits;

    bits = n_bits = INIT_BITS;
    maxcode = MAXCODE(n_bits);
    free_ent = ((block_compress) ? FIRST : 256 );
    while ( ( code = getcode() ) >= 0 ) {
	if ( (code == CLEAR) && block_compress ) {
   	    free_ent = FIRST - 1;
   	    clear_flg = 1;
	}
	else if ( free_ent < maxmaxcode )
	    free_ent++;
	if ( bits != n_bits ) {
	    fprintf(stderr, "\nChange to %d bits\n", n_bits );
	    bits = n_bits;
	    col = 0;
	}
	fprintf(stderr, "%5d%c", code, (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
    }
    putc( '\n', stderr );
    exit( 0 );
}

dump_tab()	/* dump string table */
{
    register int i;
    register ent;
    char stack[4 * MAXSTACK];	/* \nnn makes it 4 times bigger */
    int stack_top = 4 * MAXSTACK;

    for ( i = 0; i < free_ent; i++ ) {
	ent = i;
	if ( isascii(tab_suffix[ent]) && isprint(tab_suffix[ent]) )
	    fprintf( stderr, "%5d: %5d/'%c'  \"",
			ent, tab_prefix[ent], tab_suffix[ent] );
	else
	    fprintf( stderr, "%5d: %5d/\\%03o \"",
			ent, tab_prefix[ent], tab_suffix[ent] );
	stack[--stack_top] = '\n';
	stack[--stack_top] = '"';
	for ( ; ent != NULL;
		ent = (ent >= FIRST ? tab_prefix[ent] : NULL) ) {
	    if ( isascii(tab_suffix[ent]) && isprint(tab_suffix[ent]) )
		stack[--stack_top] = tab_suffix[ent];
	    else {
		switch( tab_suffix[ent] ) {
		case '\n': stack[--stack_top] = 'n'; break;
		case '\t': stack[--stack_top] = 't'; break;
		case '\b': stack[--stack_top] = 'b'; break;
		case '\f': stack[--stack_top] = 'f'; break;
		case '\r': stack[--stack_top] = 'r'; break;
		default:
		    stack[--stack_top] = '0' + tab_suffix[ent] % 8;
		    stack[--stack_top] = '0' + (tab_suffix[ent] / 8) % 8;
		    stack[--stack_top] = '0' + tab_suffix[ent] / 64;
		    break;
		}
		stack[--stack_top] = '\\';
	    }
	}
	fwrite( &stack[stack_top], 1, 4 * MAXSTACK - stack_top, stderr );
	stack_top = 4 * MAXSTACK;
    }
}
#endif DEBUG

/*****************************************************************
 * TAG( writeerr )
 *
 * Exits with a message.  We only check for write errors often enough
 * to avoid a lot of "file system full" messages, not on every write.
 * ferror() check after fflush will catch any others (I trust).
 *
 */

writeerr()
{
    perror ( ofname );
    unlink ( ofname );
    exit ( 1 );
}

copystat(ifname, ofname)
char *ifname, *ofname;
{
    struct stat statbuf;
    int mode;
    time_t timep[2];

    fclose(stdout);
    if (stat(ifname, &statbuf)) {		/* Get stat on input file */
	perror(ifname);
	return;
    }
    if ((statbuf.st_mode & S_IFMT/*0170000*/) != S_IFREG/*0100000*/) {
	if(quiet)
	    	fprintf(stderr, "%s: ", ifname);
	fprintf(stderr, " -- not a regular file: unchanged");
	exit_stat = 1;
    } else if (statbuf.st_nlink > 1) {
	if(quiet)
	    	fprintf(stderr, "%s: ", ifname);
	fprintf(stderr, " -- has %d other links: unchanged",
		statbuf.st_nlink - 1);
	exit_stat = 1;
    } else if (exit_stat == 2 && (!force) && !quiet) { /* No compression: remove file.Z */
	fprintf(stderr, " -- file unchanged");
    } else {			/* ***** Successful Compression ***** */
	exit_stat = 0;
	mode = statbuf.st_mode & 07777;
	if (chmod(ofname, mode))		/* Copy modes */
	    perror(ofname);
	chown(ofname, statbuf.st_uid, statbuf.st_gid);	/* Copy ownership */
	timep[0] = statbuf.st_atime;
	timep[1] = statbuf.st_mtime;
	utime(ofname, timep);	/* Update last accessed and modified times */
	if (unlink(ifname))	/* Remove input file */
	    perror(ifname);
	if(!quiet)
		fprintf(stderr, " -- replaced with %s", ofname);
	return;		/* Successful return */
    }

    /* Unsuccessful return -- one of the tests failed */
    if (unlink(ofname))
	perror(ofname);
}
/*
 * This routine returns 1 if we are running in the foreground and stderr
 * is a tty.
 */
foreground()
{
	if(bgnd_flag) {	/* background? */
		return(0);
	} else {			/* foreground */
		if(isatty(2)) {		/* and stderr is a tty */
			return(1);
		} else {
			return(0);
		}
	}
}

onintr ( )
{
    unlink ( ofname );
    exit ( 1 );
}

cl_block ()		/* table clear for block compress */
{
#ifdef DEBUG
	if(debug)
    		fprintf ( stderr, "count: %ld ratio: %f\n", in_count,
     		(double) in_count / (double) bytes_out );
#endif DEBUG

    checkpoint = in_count + CHECK_GAP;
    if ( (double) in_count / (double) bytes_out > ratio )
	ratio = (double) in_count / (double) bytes_out;
    else {
	ratio = 0.0;
#ifdef USERMEM
	if ( maxbits <= FBITS ) 		/* sparse array clear */
	    cl_sparse ();
	else 
#endif USERMEM					/* hash table clear */
	{
	    cl_hash ( (count_int) hsize );
	}
	free_ent = FIRST;
	clear_flg = 1;
	output ( (code_int) CLEAR );
#ifdef DEBUG
	if(debug)
    		fprintf ( stderr, "clear\n" );
#endif DEBUG
    }
}

cl_cache ( n )			/* clear hash cache */
    register count_int n;	/* clear at least this many entries */
{
    register count_int i;
    register unsigned short *hash_p;
    register unsigned short zero = 0;

    if ( nfiles++ == 0 )	/* no clear needed if first time */
	return;
    n = (n+15) & (-16);
    hash_p = hashcache + n;
    for ( i = n; i > 0; i -=16 ) {  /* can do BSD bzero(3) or Sys V memset(3) */
	*(hash_p-16) = zero;
	*(hash_p-15) = zero;
	*(hash_p-14) = zero;
	*(hash_p-13) = zero;
	*(hash_p-12) = zero;
	*(hash_p-11) = zero;
	*(hash_p-10) = zero;
	*(hash_p-9) = zero;
	*(hash_p-8) = zero;
	*(hash_p-7) = zero;
	*(hash_p-6) = zero;
	*(hash_p-5) = zero;
	*(hash_p-4) = zero;
	*(hash_p-3) = zero;
	*(hash_p-2) = zero;
	*(hash_p-1) = zero;
	hash_p -= 16;
    }
}

hogtally ()	/* compute character code hog */
{
    register int i, most;

    for ( i = most = 0; i < 256; i++ )
	if ( cfreq [i] >= cfreq [most] )
	    most = i;
    return ( most );
}

cl_hash(hsize)		/* clear hash cache, re-init code table */
	register count_int hsize;
{
	register count_int *htab_p = htab+hsize;
	register count_int i;
	register long m1 = -1;

	cl_cache( min((count_int)hsize, MAX_CACHE) );

	i = hsize - 16;
	do {
		*(htab_p-16) = m1;
		*(htab_p-15) = m1;
		*(htab_p-14) = m1;
		*(htab_p-13) = m1;
		*(htab_p-12) = m1;
		*(htab_p-11) = m1;
		*(htab_p-10) = m1;
		*(htab_p-9) = m1;
		*(htab_p-8) = m1;
		*(htab_p-7) = m1;
		*(htab_p-6) = m1;
		*(htab_p-5) = m1;
		*(htab_p-4) = m1;
		*(htab_p-3) = m1;
		*(htab_p-2) = m1;
		*(htab_p-1) = m1;
		htab_p -= 16;
	} while ((i -= 16) >= 0);
    	for ( i += 16; i > 0; i-- )
		*--htab_p = m1;
}

#ifdef USERMEM
cl_sparse ( )	/* clear sparse table indirectly thru "shadow" array */
{
	register code_int i;

    	for ( i = (1 << maxbits) - 1; i >= 0; i-- )
	    ftable [fcodemem [i]] = 0;
}
#endif USERMEM
