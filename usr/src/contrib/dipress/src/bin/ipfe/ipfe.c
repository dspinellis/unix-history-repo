/***  file = ipfe.c  ********************************************/

/*** start of specifications ************************************
 *
 * name = ipfe
 * descriptive name = interpress file editor
 * filename = ipfe.c
 * module type = main
 * subsystem name = interpress toolkit
 * copyright = (c) 1985, 1986, xerox corporation
 * author = mark l. rollins
 * date written = 10/oct/85
 *
 * change activity =
 *   07/18/86 r1.05 mlr wrc:	changed so Scale follows, rather than
 *				  precedes, Rotate, BindingOffset, XOffset,
 *				  and YOffset in output processing
 *   06/16/86 r1.04 mlr wrc:	add Scale processing
 *   05/19/86 r1.03 mlr wrc:	fixed bug in dsply of Y-offset values
 *   04/22/86 r1.02 mlr wrc:	fixed bug in pagerange parsing that gave
 *				  error on terminating "]";
 *				add vers display
 *   03/14/86 r1.01 mlr wrc:	major enhancements
 *   10/10/85 r1.00 mlr wrc:	original date
 *
 *   release = r1.05
 *   date = 18/jul/86
 *
 * function = provide the following skeleton- and page-level functions
 *		for editing interpress masters:
 *		.  concatenate masters
 *		.  merge pages from multiple masters into one master
 *		.  overlay selected pages as a single page (may be
 *			combined with merge function)
 *		.  chapterize a master into multiple masters
 *		.  satisfy sequenceInsertFile & sequenceInsertMaster
 *			references
 *		.  resolve alias and binding offset printing instructions
 *		.  return information about the properties of a set of
 *			masters
 *
 * function description =
 *
 *	Usage:  ipfe [ options ] file [ pagerange] ... [ file [pagerange] ...]
 *	Options: [-l logfile] [-dDiLqrRsS] [-a alias:actual [-a alias:actual]
 *		 ...] [-b offset:unit] [-c count:unit] [-o outfile]
 *		 [-p level:propfile] [-S factor] [-X offset:unit]
 *		 [-Y offset:unit]
 *
 *		 -a alias:actual
 *				(alias). If the -s option is specified,
 *				replace any SIF matching the string "alias"
 *				with the string "actual" before attempting
 *				to satisfy the SIF. If "actual" cannot be
 *				opened or there is some error, the SIF
 *				"alias" is preserved, unless the -r option
 *				is also specified.
 *		 -b offset:unit	(binding offset). Shift the image offset units
 *				in the x-direction, where unit may be: none
 *				(default centimeters), c (centimeters), i
 *				(inches), p (points), P (Picas). If the -L
 *				(Landscape) switch is set, the image offset
 *				is in the y-direction.
 *		 -c count:unit	(chapterize) every count units, where unit may
 *				be p (pages), k (kilobytes), or m (megabytes).
 *		 -d		(duplex). For resolving binding offset.
 *		 -D		(Debug). If the -p option is specified, also
 *				write to the properties file the offsets of
 *				each skeleton-level token encountered.
 *		 -i		(insert SIF for overlay). Insert (create) a
 *				SIF for any unresolvable overlays.
 *		 -l logfile	(log). Keep a running log.
 *		 -L		(Landscape). Rotate every page 90 degrees
 *				counterclockwise and preserve the upper left
 *				corner. Intended for printing text in
 *				landscape orientation.
 *		 -o outfile	(output). Where the output goes. If there is
 *				no -o and there is a -p, only the properties
 *				are written, else if there is no -o
 *				"infile.ip" is used if it doesn't already
 *				exist, else "infileN.ip" is used, where N
 *				is the lowest ordinal number such that
 *				"infileN.ip" does not already exist.
 *		 -p level:propfile
 *				(properties). Where the properties are
 *				written. Increasing levels provide increasing
 *				information details.
 *		 -q		(quiet). Don't write info & error msgs (to
 *				STDERR).
 *		 -r		(remove SIFS). If -s is also specified, it
 *				takes precedence and a SIF reference is
 *				removed only if it is unresolved or there is
 *				some other error.
 *		 -R		(Rotate). Rotate every page 90 degrees
 *				clockwise and preserve the center point.
 *				Intended for rotating an image created for
 *				a "landscape printer" to print in portrait
 *				orientation.
 *		 -s		(satisfy SIFS). Replace any SIF references
 *				tokens found in the referenced file. If the
 *				referenced file cannot be opened, the SIF
 *				reference is preserved unless -r is also
 *				specified.
 *		 -S factor	(scale). Scale the image by factor.
 *		 -X offset:unit	(X-imageShift). Shift the image offset units
 *				in the x-direction, where unit may be: none
 *				(default centimeters), c (centimeters), i
 *				(inches), p (points), P (Picas). The shift
 *				is independent of the -L and -R switches
 *				(i.e., the x-direction is the same as the
 *				unrotated image).
 *		 -Y offset:unit	(Y-imageShift). Shift the image offset units
 *				in the y-direction, where unit may be: none
 *				(default centimeters), c (centimeters), i
 *				(inches), p (points), P (Picas). The shift
 *				is independent of the -L and -R switches
 *				(i.e., the y-direction is the same as the
 *				unrotated image).
 *		 infile [pagerange]
 *				See the manual page for syntax details.
 *		  Example:
 *		   [1,4-6,9[pic1],10-11,12[pic2:2(2P,4P)][pic3(-4P,-2P)],15-]
 *				include page 1.
 *				skip pages 2-3.
 *				include pages 4-6.
 *				skip pages 7-8.
 *				include page 9, overlaying it with pic1.
 *				include pages 10-11.
 *				include pages 12, overlaying it with page 2 of
 *				  pic2 at an offset of 2 Picas to the right
 *				  and 4 Picas up, and pic3 at an offset of
 *				  4 Picas to the left and 2 Picas down.
 *				skip pages 13-14.
 *				include pages 15 thru the end of the master.
 *
 * linkage =
 *   entry = exec ipfe
 *   input = see cmd line options above
 *   output = interpress master(s), STDERR, log file, property file
 *   exit = exit(exit_status)
 *
 * publics =
 *
 * dependencies =
 *   src = iptokens.h, ipnames.h
 *   obj =
 *   lnk =
 *
 * environment =
 *   runtime = vax/masscomp unix/4.23bsd
 *   development = vax unix/4.23bsd
 *   processor = cc
 *   code = unix c mnemonics & linkages
 *
 *** end of specifications **************************************/

#ifdef vax11c
# include stdio
# include setjmp
# include ctype
#else
# include <stdio.h>
/***
# include <sys/time.h>
# include <sys/resource.h>
***/
# include <setjmp.h>
# include <ctype.h>
#endif

#include "iptokens.h"
#include "ipnames.h"


#define	IPFE_VERS	"1.05"
#define	IPFE_DATE	"18 JUL 86"

#define	FALSE		0
#define	TRUE		1
#define	FLAG_OFF	0
#define	FLAG_ON		1

#define	CR		13
#define	BEL		07
#define	BS		08
#define	TAB		09
#define	LF		10
#define	FF		12
#define	TOF		12
#define	SPC		32
#define	ESC		27
#define	DEL		127

#define	STDIN		stdin
#define	STDOUT		stdout
#define	STDERR		stderr
#define	STDIN_DEV	0
#define	STDOUT_DEV	1
#define	STDERR_DEV	2
#define	DEF_LOGFILENAME	"ipfe.log"
#define	DEF_OUTFILENAME	"ipfe.ip"
#define	DEF_PROPFILENAME "ipfe.prop"
#define	DEF_WORKFILENAME "ipfe.wrk"
#define	BUFSIZE		2048
#define	OP_Mask		0xe0
#define	OK		0
#define	ERROR		(-1)
#define	DONT_FORMAT	0
#define	FORMAT		1
#define	MX_NO_INFILES	64
#define	MX_NO_OUTFILES	64
#define	MX_NO_OVLY_FILES  64
#define	IP_HDR_CHKLEN	17
#define	IP_VERS_OFFSET	17
#define	IP_HDR_MASTERLEN 21
#define	MX_HDR_BUFLEN	255
#define	MX_NO_ALIASES	128
#define	MX_NO_MPG_ENTRIES 128
#define	MX_BLOCK_DEPTH	8
#define	MX_BODY_DEPTH	8
#define	MX_BOP_DEPTH	16	/* max # of nested body_operators */
#define	MX_SIF_DEPTH	8	/* max sif nesting depth */
#define	INDENT_INCR	3	/* # cols to incr indent each nesting depth */

/*
 *  defines for ipfe fn types
 */
#define	CONCAT		0
#define	CHAPTERIZE	1
#define	SATISFY_SIFS	2
#define	REMOVE_SIFS	3
#define	PROPERTIES	4
#define	INSERT_OVLY	5
#define	DEBUG		6
#define	PROC_LOG	7
#define	FORCE_STDOUT	8
#define	BINDING_OFFSET	9
#define	DUPLEX		10
#define	QUIET		11
#define	LANDSCAPE	12
#define	ROTATE		13
#define	SCALE		14
#define	X_OFFSET	15
#define	Y_OFFSET	16
#define	NUM_FNS		17

/*
 *  defines for chapterization type
 */
#define	CH_PAGES	1
#define	CH_KBYTES	2
#define	CH_MBYTES	3

#ifdef lcv2			/* if Lattice C vers 2.15 or earlier */
#define	void	int
#endif

/*
 *  lattice C defines for fopen file types (binary & text)
 */
#define	BINARY_FILE	0x8000
#define	TEXT_FILE	0

FILE	*input;
FILE	*output;
FILE	*workfile;	/* work file for testing input file opens & storing
			   hdr/preamble for chapterizing */
FILE	*ovly_file;
FILE	*sif_file[MX_SIF_DEPTH];
FILE	*siffile;
FILE	*logfile;
FILE	*propfile;

jmp_buf	next_file;

extern int errno;


int	_fmode = TEXT_FILE;	/* lattice C file_mode */
int	abort_flag, debug_flag;
int	terrno;
int	exit_status;

long	pos, in_offset, out_offset;
long	out_beg_offset[MX_BLOCK_DEPTH],
	out_end_offset[MX_BLOCK_DEPTH];
long	chap_val;
long	boff_num, boff_den;	/* binding offset numerator, denominator */
long	xoff_num, xoff_den;	/* xOffset numerator, denominator */
long	yoff_num, yoff_den;	/* yOffset numerator, denominator */
long	rxoff_num, rxoff_den,	/* rotation xOffset numerator, denominator */
	ryoff_num, ryoff_den;	/*          yOffset numerator, denominator */
long	scale_num, scale_den;	/* scale factor numerator, denominator */
long	blk_num_bytes[MX_BLOCK_DEPTH];
long	pg_num_bytes;

int	num_infiles, out_filnum;
int	input_indx, output_indx, sif_indx;
int	prop_level;

int	cur_indent, off_indent, cur_col;
int	ovly_flag;
int	proc_flag[NUM_FNS];
int	chap_type;
int	ipwrite_flag;
int	rot_deg;		/* rotation degrees */
int	num_aliases;
int	alias_indx;

int	begBody_flag;
int	preamble_expected, endpreamble_expected, frstpg_flag,
	bop_preamble_expected[MX_BOP_DEPTH];
/*
 *  MERGE & OVERLAY ARRAYS
 *    There are 2 second order arrays, one for merge pages and one for
 *    overlay files. The 1st order of both is indexed by input_indx, and
 *    the 2nd order by merge_indx. For overlay files, there are 3
 *    additional arrays, one for an overlay pagenum, one for page offset
 *    numerators, and one for page offset denominators. These 3 arrays
 *    are indexed by ovly_indx (the 1st order indexing for the numerator
 *    and denominator arrays is 0 for the x-offset and 1 for the y-offset).
 *
 *    Each 2nd order vector of the merge_pg[][] array is initialized to
 *    (-1),0,0,... which indicates to include all pages, and each vector
 *    of the ovly_pg[][] array is initialized to (-1),(-1),(-1),... which
 *    indicates no overlay files.
 *    As IPFE scans the command line, an entry is put into the merge_pg[][]
 *    array for each pageNum specified in a pagerange for an infile (the
 *    value put into the entry is pageNum). If there is an overlay file
 *    specified for pageNum, the (-1) in the ovly_pg[][merge_indx] entry
 *    is replaced with the current value of ovly_indx and ovly_indx is
 *    incremented. If there is another overlay file specified for pageNum,
 *    the value pageNum is repeated in the next merge_pg[][] entry and the
 *    incremented ovly_indx is placed in the next ovly_pg[][] entry.
 *      Example:
 *        ipfe infile "[2,4-6,9[ovly1],12[ovly2][ovly3],15-]"
 *      will yield the following merge_pg[input_indx][] & 
 *      ovly_pg[input_indx][] vectors:
 *                    0    1    2    3    4    5    6    7    8    9   10
 *                  ----|----|----|----|----|----|----|----|----|----|----
 *        merge_pg:   2 ,  4 ,(-1),  6 ,  9 , 12 , 12 , 15 ,(-1),  0 ,  0 ,...
 *        ovly_pg:  (-1),(-1),(-1),(-1),  0 ,  1 ,  2 ,(-1),(-1),(-1),(-1),...
 *
 *    The algorithm used to decode the merge_pg vector for each infile is
 *    as follows:
 *      Initialize merge_indx to 0;
 *      Loop:
 *      Current_entry = merge_pg[input_indx][merge_indx];
 *        If Current_entry = 0, we're all done with this infile;
 *	  Else if Current_entry = (-1), look at next merge_indx entry;
 *          Next_entry = merge_pg[input_indx][merge_indx+1];
 *            If Next_entry = 0, include all pages to the end of infile;
 *	      Else include all pages until the value of Next_entry is
 *              reached in infile; when it is, increment merge_indx (making
 *              Current_entry = Next_entry, and goto Loop;
 *        Else exclude all pages until Current_entry is reached in infile
 *          (it may already be reached); when it is, include that page and
 *	      check the ovly_pg[][] entry;
 *          Ovly_entry = ovly_pg[input_indx][merge_indx];
 *            If Ovly_entry != (-1) do overlay processing;
 *          Increment merge_indx, and goto Loop;
 */
int	merge_indx, merge_pg[MX_NO_INFILES][MX_NO_MPG_ENTRIES];
int	ovly_indx, ovly_pg[MX_NO_INFILES][MX_NO_MPG_ENTRIES];
int	ovly_pgnum[MX_NO_OVLY_FILES];
long	ovly_num[2][MX_NO_OVLY_FILES],
	ovly_den[2][MX_NO_OVLY_FILES];

int	block_indx;
int	block_flag;
int	bproc_indx, bop_indx, bop_iindx[MX_BOP_DEPTH];
int	bproc_flag, bop_flag;
int	num_blocks, num_pages, num_sifs,
	tnum_blocks, tnum_pages, tnum_sifs;
int	out_numpages;

int	infile_flag[MX_NO_INFILES];

int	hdr_len;
char	hdr_buf[MX_HDR_BUFLEN+1], hdr_workbuf[MX_HDR_BUFLEN+1];

char	input_name[MX_NO_INFILES][128], input_fnm[128], input_ext[128],
	output_name[128], output_fnm[128], output_ext[128],
	work_fname[128], sif_name[MX_SIF_DEPTH][128], prop_fname[128];
char	ovly_fname[MX_NO_OVLY_FILES][128];
char	alias_ref[MX_NO_ALIASES][128], alias_act[MX_NO_ALIASES][128];
char	*inputname, *outputname, *sifname, *propfname;
char	*logfilename = "ipfe.log";

char	*itostr(), lcase(), *op_name(), *strcat(), *strcpy();

/****************************************************************
 *
 *	main(argc, argv):	ipfe mainline:
 *	main:			  interpress file editor
 *
 ****************************************************************/

main(argc, argv)

	int	argc;
	char	*argv[];
{
	char	msg[128];


	(void)sprintf(msg,
		      "\nIPFE Version %s, %s -- Interpress File Editor\n",
		      IPFE_VERS, IPFE_DATE);
	pip_error(msg);
	pip_error("Copyright (c) 1985,1986 Xerox Corporation\n\n");
	init_ipfe();
	get_cmd_args(argc, argv);
	init1_ipfe();		/* initialization after getting cmd args */


	for (input_indx=0; input_indx < num_infiles; input_indx++)
	{
	    init_proc_file();
	    inputname = input_name[input_indx];
	    (void)sprintf(msg, "-- File:  %s--\n", inputname);
	    pip_error(msg);
	    pip_prop(msg);

	    _fmode = BINARY_FILE;
	    input = fopen(inputname, "r");
	    if  (input == NULL)
	    {
		(void)sprintf(msg, "\nError opening input file: %s\n",
				   inputname);
		pip_error(msg);
		pip_prop(msg);
	    }
	    else
	    {
		proc_file();		/* do the real work */
		(void)fclose(input);
	    }
	}

	if  ((abort_flag == FLAG_OFF) &&
	     (proc_flag[CONCAT] == FLAG_ON))
	{
	    ipwrite_flag = FLAG_ON;	/* in case it was turned off by
					   last file processed */
	    put_op(OP_endBlock);
	}

	cleanup();
	exit(exit_status);
}


/****************************************************************
 *
 *	cleanup:		housecleaning
 *
 ****************************************************************/

cleanup()
{
	char	msg[128];


	if  (proc_flag[CONCAT] == FLAG_ON)
	{
	    (void)sprintf(msg, "\n--Total Number of Blocks:%4d\n", tnum_blocks);
	    pip_prop(msg);
	    (void)sprintf(msg,   "--Total Number of Pages :%4d\n", tnum_pages);
	    pip_prop(msg);
	    (void)sprintf(msg,   "--Total Number of SIFs  :%4d\n", tnum_sifs);
	    pip_prop(msg);
	}
	mputc_prop('\n');

	(void)sprintf(msg, "\n-- Total Number of Input Pages :%4d\n",
			tnum_pages);
	pip_error(msg);
	if  ((output != NULL) || (out_numpages > 0))
	{
	    (void)sprintf(msg, "-- Total Number of Output Pages:%4d\n",
			    out_numpages);
	    pip_error(msg);
	}
	pip_error("\n");

	if  (logfile != NULL)
	    (void)fclose(logfile);

	if  (propfile != NULL)
	    (void)fclose(propfile);

	if  (output != NULL)
	    (void)fclose(output);

}


/****************************************************************
 *
 *	int count_int_bytes(val):
 *	int count_int_bytes:	return the number of bytes in the
 *				specified int val
 *
 ****************************************************************/

int count_int_bytes(val)

	long	val;
{
	long	mask;
	int	i;


	if  ((val == 0) || (val == (-1)))
	    i = 1;		/* avoid infinite looping */
	else
	{
	    if  (val < 0)
		val = ~(val);	/* same # of bytes as one's complement */

	    mask = 0xff800000;
	    for (i=4; ((val & mask) == 0); i--)
		mask = mask >> 8;

	}

return(i);
}


/****************************************************************
 *
 *	create_SIF(fname):
 *	create_SIF:		create a new SIF (for ovly handling)
 *
 ****************************************************************/

create_SIF(fname)

	char	*fname;
{
	int	not_created_flag;
	char	msg[128];


	not_created_flag = FLAG_OFF;
	if  (proc_flag[INSERT_OVLY] == FLAG_ON)
	{
	    if  (ovly_pgnum[ovly_indx] < 2)
	    {
		(void)sprintf(msg,
			"                  --Creating SIF:  %s\n", fname);
		pip_error(msg);
		put_seq_type_len(sequenceInsertFile, (long)strlen(fname));
		ip_puts(fname);
	    }
	    else
	    {
		pip_error(
    "                  --Cannot create SIF for overlay with pageNum > 1\n");
		not_created_flag = FLAG_ON;
	    }
	}
	else
	{
	    pip_error(
    "                    -i switch (Insert SIF for overlay) not specified\n");
	    not_created_flag = FLAG_ON;
	}
	if  (not_created_flag == FLAG_ON)
	{
	    (void)sprintf(msg,
			  "                  --SIF Not Created for:  %s\n",
			  fname);
	    pip_error(msg);
	}

}


/****************************************************************
 *
 *	get_cmd_args(argc, argv):
 *	get_cmd_args:		get cmd_line arguments
 *	gca:			label for editing
 *
 ****************************************************************/

get_cmd_args(argc, argv)

	int	argc;
	char	*argv[];
{
	long	val, val1, d;
	int	arg, opt, opt_len, ch_ctr, neg_flag, sep_flag, retcd;
	int	err_flag = FLAG_OFF;
	char	ch, sep_char, msg[128];

	
	for (arg=1; arg < argc; arg++)	/* look for cmd_arguments */
	{
	    switch
		(argv[arg][0])
	    {
		case ('-'):
		    opt_len = strlen(argv[arg]);  /* because arg can be */
						  /*   modified below   */
		    for (opt=1; opt < opt_len; opt++)
		    {
			switch
			    (argv[arg][opt])
			{
			    case ('-'):
				opt = opt_len;	/* to handle '--' switch */
				break;

			    case ('a'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
				proc_alias(&(argv[arg][opt]));
				opt = opt_len;	/* to get out of loop */
				break;

			    case ('b'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
/*
 *  binding offset:unit
 */
				val = val1 = d = ch_ctr = 0;
				neg_flag = FLAG_OFF;
				while
				    (opt < opt_len)
				{
				    ch = argv[arg][opt++];
				    if  ((ch == '-') && (ch_ctr == 0))
					neg_flag = FLAG_ON;
				    else
				    if ((ch >= '0') && (ch <= '9'))
				    {
					ch = ch - '0';
					if  (d == 0)
					    val = (val * 10) + ch;
					else
					if  (d <= 1000)  /* prec = 10**(-3)
							    truncated */
					{
					    val1 = (val1 * 10) + ch;
					    d = d * 10;
					}
				    }
				    else
				    if (ch == '.')
					d = 1;
				    else
				    {
					if  ((ch == ':') && (opt < opt_len))
					    ch = argv[arg][opt++];
					break;		/* exit loop */
				    }
				    ++ch_ctr;
				}
/*				end while (opt < opt_len) */

				if  (d == 0)
				    d = 1;
				else
				    val = (val * d) + val1;

				if  (val != 0)
				{
				    if  (neg_flag == FLAG_ON)
					val = -(val);
				    switch
					(ch)
				    {
					case ('c'):
					    d = d * 100;
					    break;
					case ('i'):
					    val = val * 254;
					    d = d * 10000;
					    break;
					case ('p'):
					    val = val * 254;
					    d = ((d * 720000) & 0x7fffffff);
					    break;
					case ('P'):
					    val = val * 254;
					    d = ((d * 60000) & 0x7fffffff);
					    break;
					default:
					    d = d * 100;  /* def=cm */
					    break;
				    }
/*				    end switch (ch) */

				    if  (d != 0)  /* last chk for div
						     by 0 just in case
						     of ovflw */
				    {
					boff_num = val;
					boff_den = d;
					proc_flag[BINDING_OFFSET] =
						FLAG_ON;
				    }
				}
				else
				{
				    (void)sprintf(msg,
		"--ipfe:  No binding offset value specified in (%s)\n",
						  argv[arg]);
				    pip_error(msg);
				    err_flag = FLAG_ON;
				}
				--opt;	/* to parse cur ch at cmd line */
				break;

			    case ('c'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
				ch = val = 0;
				while
				    (opt < opt_len)
				{
				    ch = argv[arg][opt];
				    if  ((ch >= '0') && (ch <= '9'))
				    {
					ch = ch - '0';
					val = (val * 10) + ch;
					++opt;
				    }
				    else
					break;
				}
				if  (val == 0)
				{
				    (void)sprintf(msg,
		      "--ipfe:  No length specified for chapterize in %s\n",
					argv[arg]);
				    pip_error(msg);
				    err_flag = FLAG_ON;
				}
				else
				{
				    if  ((ch == ':') && (opt < opt_len))
					ch = argv[arg][++opt];

				    switch
					(ch)
				    {
					case ('p'):
					    proc_flag[CHAPTERIZE] = FLAG_ON;
					    chap_type = CH_PAGES;
					    chap_val = val;
					    break;
					case ('k'):
					    proc_flag[CHAPTERIZE] = FLAG_ON;
					    chap_type = CH_KBYTES;
					    chap_val = val * 1024;
					    break;
					case ('m'):
					    proc_flag[CHAPTERIZE] = FLAG_ON;
					    chap_type = CH_MBYTES;
					    chap_val = ((val * 1048576) &
							0x7fffffff);
					    break;
					default:
					    (void)sprintf(msg,
			"--ipfe:  No unit specified for chapterize in %s\n",
						argv[arg]);
					    pip_error(msg);
					    err_flag = FLAG_ON;
					    --opt;	/* parse opt again */
					    break;
				    }
				}
				break;

			    case ('d'):
				proc_flag[DUPLEX] = FLAG_ON;
				break;

			    case ('D'):
				proc_flag[DEBUG] = FLAG_ON;
				break;

			    case ('i'):
				proc_flag[INSERT_OVLY] = FLAG_ON;
				break;

			    case ('l'):
				proc_flag[PROC_LOG] = FLAG_ON;
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
				ch = argv[arg][opt];
				if  ((ch != ':') && (ch != '-'))
				{
				    logfilename = &(argv[arg][opt]);
				    opt = opt_len;  /* to get out of lp */
				}
				open_logfile(argc, argv);
				break;

			    case ('L'):
				rot_deg = 90;
				rxoff_num = rxoff_den = 0;
				ryoff_num = -(2794);
				ryoff_den = 10000;
				proc_flag[LANDSCAPE] = FLAG_ON;
				break;

			    case ('o'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    outputname = &(argv[arg][opt+1]);
				else
				    outputname = argv[++arg];
				opt = opt_len;	  /* to get out of loop */
				break;

			    case ('p'):
				proc_flag[PROPERTIES] = FLAG_ON;
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
				ch = argv[arg][opt];
				if  ((ch != ':') && (ch != '-'))
				{
				    if  ((ch >= '0') && (ch <= '9'))
				    {
					prop_level = ch - '0';
					++opt;
					if  (argv[arg][opt] == ':')
					    ++opt;
					if  (opt < opt_len)
					    propfname = &(argv[arg][opt]);
					opt = opt_len;	/* to exit loop */
				    }
				    else
				    {
					(void)sprintf(msg,
	    "--ipfe:  *** warning -- property level not specified in %s\n",
						argv[arg]);
					pip_error(msg);
					err_flag = FLAG_ON;
					propfname = &(argv[arg][opt]);
					opt = opt_len;	/* to exit loop */
				    }
				}
				break;

			    case ('q'):
				proc_flag[QUIET] = FLAG_ON;
				break;

			    case ('r'):
				proc_flag[REMOVE_SIFS] = FLAG_ON;
				break;

			    case ('R'):
				rot_deg = -(90);
				rxoff_num = -(2477);	/* xOff = -(9 3/4") */
				ryoff_num = -(318);	/* yOff = -(1 1/4") */
				rxoff_den = ryoff_den = 10000;
				proc_flag[ROTATE] = FLAG_ON;
				break;

			    case ('s'):
				proc_flag[SATISFY_SIFS] = FLAG_ON;
				break;

			    case ('S'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
/*
 *  scale
 */
				val = val1 = d = ch_ctr = 0;
				neg_flag = sep_flag = FLAG_OFF;
				while
				    (opt < opt_len)
				{
				    ch = argv[arg][opt++];
				    if  ((ch == '-') && (ch_ctr == 0))
					neg_flag = FLAG_ON;
				    else
				    if ((ch >= '0') && (ch <= '9'))
				    {
					ch = ch - '0';
					if  (d == 0)
					    val = (val * 10) + ch;
					else
					if  (d <= 1000)  /* prec = 10**(-3)
							    truncated */
					{
					    val1 = (val1 * 10) + ch;
					    d = d * 10;
					}
				    }
				    else
				    if ((ch == '.') || (ch == '/'))
				    {
					if  (sep_flag == FLAG_OFF)
					{
					    d = 1;
					    sep_char = ch;
					    sep_flag = FLAG_ON;
					}
					else
					{
					    (void)sprintf(msg,
	"--ipfe:  Scale factor cannot have BOTH a '.' AND a '/' in (%s)\n",
						  argv[arg]);
					    pip_error(msg);
					    err_flag = FLAG_ON;
					    break;	/* exit loop */
					}
				    }
				    else
				    {
					if  ((ch == ':') && (opt < opt_len))
					    ch = argv[arg][opt++];
					break;		/* exit loop */
				    }
				    ++ch_ctr;
				}
/*				end while (opt < opt_len) */

				if  (d == 0)
				    d = 1;
				else
				if  (sep_char == '/')
				    d = val1;
				else
				    val = (val * d) + val1;

				if  ((val != 0) && (err_flag == FLAG_OFF))
				{
				    if  (neg_flag == FLAG_ON)
					val = -(val);

				    if  (d != 0)  /* last chk for div
						     by 0 just in case
						     of ovflw */
				    {
					scale_num = val;
					scale_den = d;
					proc_flag[SCALE] = FLAG_ON;
				    }
				}
				else
				{
				    (void)sprintf(msg,
		"--ipfe:  No scale factor value specified in (%s)\n",
						  argv[arg]);
				    pip_error(msg);
				    err_flag = FLAG_ON;
				}
				--opt;	/* to parse cur ch at cmd line */
				break;

			    case ('X'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
/*
 *  xOffset:unit
 */
				val = val1 = d = ch_ctr = 0;
				neg_flag = FLAG_OFF;
				while
				    (opt < opt_len)
				{
				    ch = argv[arg][opt++];
				    if  ((ch == '-') && (ch_ctr == 0))
					neg_flag = FLAG_ON;
				    else
				    if ((ch >= '0') && (ch <= '9'))
				    {
					ch = ch - '0';
					if  (d == 0)
					    val = (val * 10) + ch;
					else
					if  (d <= 1000)  /* prec = 10**(-3)
							    truncated */
					{
					    val1 = (val1 * 10) + ch;
					    d = d * 10;
					}
				    }
				    else
				    if (ch == '.')
				    {
					d = 1;
					val1 = 0;
				    }
				    else
				    {
					if  ((ch == ':') && (opt < opt_len))
					    ch = argv[arg][opt++];
					break;		/* exit loop */
				    }
				    ++ch_ctr;
				}
/*				end while (opt < opt_len) */

				if  (d == 0)
				    d = 1;
				else
				    val = (val * d) + val1;

				if  (val != 0)
				{
				    if  (neg_flag == FLAG_ON)
					val = -(val);
				    switch
					(ch)
				    {
					case ('c'):
					    d = d * 100;
					    break;
					case ('i'):
					    val = val * 254;
					    d = d * 10000;
					    break;
					case ('p'):
					    val = val * 254;
					    d = ((d * 720000) & 0x7fffffff);
					    break;
					case ('P'):
					    val = val * 254;
					    d = ((d * 60000) & 0x7fffffff);
					    break;
					default:
					    d = d * 100;  /* def=cm */
					    break;
				    }
/*				    end switch (ch) */

				    if  (d != 0)  /* last chk for div
						     by 0 just in case
						     of ovflw */
				    {
					xoff_num = val;
					xoff_den = d;
					proc_flag[X_OFFSET] = FLAG_ON;
				    }
				}
				else
				{
				    (void)sprintf(msg,
			"--ipfe:  No xOffset value specified in (%s)\n",
						  argv[arg]);
				    pip_error(msg);
				    err_flag = FLAG_ON;
				}
				--opt;	/* to parse cur ch at cmd line */
				break;

			    case ('Y'):
				if  (strlen(&(argv[arg][opt])) > 1)
				    ++opt;
				else
				{
				    ++arg;
				    opt = 0;
				    opt_len = strlen(argv[arg]);
				}
/*
 *  yOffset:unit
 */
				val = val1 = d = ch_ctr = 0;
				neg_flag = FLAG_OFF;
				while
				    (opt < opt_len)
				{
				    ch = argv[arg][opt++];
				    if  ((ch == '-') && (ch_ctr == 0))
					neg_flag = FLAG_ON;
				    else
				    if ((ch >= '0') && (ch <= '9'))
				    {
					ch = ch - '0';
					if  (d == 0)
					    val = (val * 10) + ch;
					else
					if  (d <= 1000)  /* prec = 10**(-3)
							    truncated */
					{
					    val1 = (val1 * 10) + ch;
					    d = d * 10;
					}
				    }
				    else
				    if (ch == '.')
				    {
					d = 1;
					val1 = 0;
				    }
				    else
				    {
					if  ((ch == ':') && (opt < opt_len))
					    ch = argv[arg][opt++];
					break;		/* exit loop */
				    }
				    ++ch_ctr;
				}
/*				end while (opt < opt_len) */

				if  (d == 0)
				    d = 1;
				else
				    val = (val * d) + val1;

				if  (val != 0)
				{
				    if  (neg_flag == FLAG_ON)
					val = -(val);
				    switch
					(ch)
				    {
					case ('c'):
					    d = d * 100;
					    break;
					case ('i'):
					    val = val * 254;
					    d = d * 10000;
					    break;
					case ('p'):
					    val = val * 254;
					    d = ((d * 720000) & 0x7fffffff);
					    break;
					case ('P'):
					    val = val * 254;
					    d = ((d * 60000) & 0x7fffffff);
					    break;
					default:
					    d = d * 100;  /* def=cm */
					    break;
				    }
/*				    end switch (ch) */

				    if  (d != 0)  /* last chk for div
						     by 0 just in case
						     of ovflw */
				    {
					yoff_num = val;
					yoff_den = d;
					proc_flag[Y_OFFSET] = FLAG_ON;
				    }
				}
				else
				{
				    (void)sprintf(msg,
			"--ipfe:  No yOffset value specified in (%s)\n",
						  argv[arg]);
				    pip_error(msg);
				    err_flag = FLAG_ON;
				}
				--opt;	/* to parse cur ch at cmd line */
				break;

			    default:
				(void)sprintf(msg,
				    "--ipfe:  Invalid option in %s (%s)\n",
					argv[arg], &(argv[arg][opt]));
				pip_error(msg);
				err_flag = FLAG_ON;
				break;
			}
		    }
		    break;

		case ('['):
		    break;

		case ('>'):
		    break;

		default:
/*
 *  the current arg is an input file; try to open it
 */
		    if  (input_indx < MX_NO_INFILES)
		    {
			_fmode = BINARY_FILE;
			workfile = fopen(argv[arg], "r");
			if  (workfile == NULL)
			{
			    (void)sprintf(msg, 
				    "--ipfe:  Error opening input file: %s\n",
				    argv[arg]);
			    pip_error(msg);
			    err_flag = FLAG_ON;
			}
			else
			{
/*
 *  check for valid hdr
 */
			    retcd = get_ip_hdr(workfile, argv[arg]);
			    if  (retcd == OK)
			    {
				(void)strcpy(input_name[input_indx],
					     argv[arg]);
/*
 *  check for page range
 */
				if  (((arg + 1) < argc) &&
				     ((argv[arg+1][0] == '[') ||
				      (argv[arg+1][0] == '+') ||
				      (argv[arg+1][0] == '#')))
				{
				    ++arg;
				    get_pagerange(arg, argv);
				}
				else
				    merge_pg[input_indx][0] = (-1);
				infile_flag[input_indx] = FLAG_ON;
				++input_indx;
				++num_infiles;
			    }
			    else
				err_flag = FLAG_ON;
			    (void)fclose(workfile);
			}
		    }
		    else
		    {
			(void)sprintf(msg,
	"--ipfe:  Maximum # of input files = %d; File:  %s not processed\n",
				MX_NO_INFILES, argv[arg]);
			pip_error(msg);
			err_flag = FLAG_ON;
		    }
		    break;
	    }
/*	    end switch (argv[arg][0]) */
	}
/*	end for (arg=1; arg < argc; arg++) */
	if  (err_flag == FLAG_ON)
	    pip_error("\n");

}


/****************************************************************
 *
 *	int get_ip_hdr(file, filename):
 *	int get_ip_hdr:	get the ip_hdr from the specified
 *				file into a work_buf & return a
 *				ptr to the buf
 *
 ****************************************************************/

int get_ip_hdr(file, filename)

	FILE	*file;
	char	*filename;
{
	int	i, retcd, hdr_flag;
	int	c;		/* must be int for stdio EOF compare */
	char	*ptr, msg[128];


	retcd = OK;
/*
 *  get the header
 */
	ptr = hdr_workbuf;
	hdr_flag = FLAG_OFF;
	for (i=0; i < MX_HDR_BUFLEN; i++)
	{
	    c = getc_testeof(file, filename);  /* get a char,          */
	    *ptr++ = c;			/*   & stick it in the buffer  */
	    if  (c == ' ')		/* if c=ip_hdr_terminate_char, */
	    {
		hdr_flag = FLAG_ON;	/*   set hdr_ok flag,          */
		break;			/*   & exit the loop           */
	    }
	}
	*ptr = '\0';
/*
 *  ... & check its validity
 */
	if  (hdr_flag == FLAG_OFF)
	{
	    (void)sprintf(msg, "--ipfe:  Hdr length > %d chars in file: %s\n",
		    MX_HDR_BUFLEN, filename);
	    pip_error(msg);
	    retcd = ERROR;
	}
	if  (mstrncmp(hdr_workbuf, IP_Header, IP_HDR_CHKLEN) != 0)
	{
	    (void)sprintf(msg, "--ipfe:  INVALID HEADER! in file: %s\n",
			  filename);
	    pip_error(msg);
	    retcd = ERROR;
	}

return(retcd);
}


/****************************************************************
 *
 *	get_pagerange(arg, argv):
 *	get_pagerange:	get cmd_line page range argument
 *				for current input_indx from
 *				specified argument
 *
 ****************************************************************/

get_pagerange(arg, argv)

	int	arg;
	char	*argv[];
{
	int	i, opt, opt_len, d, val, val1, lp_ctr, ch_ctr,
		prv_mpg_num, err_flag, neg_flag;
	char	ch, nc;
	char	msg[128], name_buf[128];


	merge_indx = val = 0;
	prv_mpg_num = 0;
	opt_len = strlen(argv[arg]);
	err_flag = FLAG_OFF;
	for (opt=1; ((opt < opt_len) && (err_flag == FLAG_OFF)); opt++)
	{
	    switch
		(argv[arg][opt])
	    {
		case ('-'):
		    merge_pg[input_indx][merge_indx] = (-1);
		    ++merge_indx;
		    break;
/*
 *  all of the following can just be ignored at this level
 */
		case (','):
		case (']'):
		case (')'):
		case (SPC):
		case ('\n'):
		    break;

		case ('['):
		case ('+'):
/*
 *  overlay filename
 */
		    if  (strlen(&(argv[arg][opt])) > 1)
		    {
			ch = '\0';
			i = 0;
			++opt;
			while
			    (opt < opt_len)
			{
			    ch = argv[arg][opt];
			    if  ((ch == ':') || (ch == '(') || (ch == '@') ||
				 (ch == '+'))
			    {
				--opt;	    /* reparse opt at switch level */
				break;
			    }
			    else
			    if  ((ch == ']') || (ch == ','))
				break;	    /* don't bother reparsing */
			    else
			    {
				name_buf[i++] = ch;
				++opt;
			    }
			}
			name_buf[i] = '\0';
			if  (i > 0)
			{
			    if  (merge_indx == 0)
			    {
				merge_pg[input_indx][0] = 1;
				++merge_indx;
			    }
			    if  (merge_pg[input_indx][(merge_indx-1)] > 0)
			    {
				if  (ovly_pg[input_indx][(merge_indx-1)] !=
					(-1))
				{
				    merge_pg[input_indx][merge_indx] =
					merge_pg[input_indx][(merge_indx-1)];
				    ++merge_indx;
				}
				ovly_pg[input_indx][(merge_indx-1)] =
				    ovly_indx;
				(void)strcpy(ovly_fname[ovly_indx], name_buf);
				++ovly_indx;
			    }
			    else
				err_flag = FLAG_ON;
			}
			else
			    err_flag = FLAG_ON;
		    }
		    else
			err_flag = FLAG_ON;
		    break;

		case (':'):
/*
 *  overlay pagenum
 */
		    if  ((ovly_indx == 0) ||
		    	 (ovly_pg[input_indx][merge_indx-1] == (-1)) ||
			 (ovly_pgnum[ovly_indx-1] != 0))
			err_flag = FLAG_ON;
		    else
		    if  (strlen(&(argv[arg][opt])) > 1)
		    {
			val = 0;
			++opt;
			while
			    (opt < opt_len)
			{
			    ch = argv[arg][opt];
			    if  ((ch >= '0') && (ch <= '9'))
			    {
				ch = ch - '0';
				val = (val * 10) + ch;
				++opt;
			    }
			    else
			    {
				--opt;	/* reparse everything else at the
					   switch level */
				break;
			    }
			}
/*
 *  if the pagenum is good, process it
 */
			if  (val > 0)
			    ovly_pgnum[ovly_indx-1] = val;
		    }
		    else
			err_flag = FLAG_ON;
		    break;

		case ('('):
		case ('@'):
/*
 *  overlay page offset
 */
		    if  ((ovly_indx == 0) ||
		    	 (ovly_pg[input_indx][merge_indx-1] == (-1)))
			err_flag = FLAG_ON;
		    else
		    if  (strlen(&(argv[arg][opt])) > 1)
		    {
			val = val1 = d = lp_ctr = ch_ctr = 0;
			neg_flag = FLAG_OFF;
			++opt;
			while
			    ((opt < opt_len) && (lp_ctr < 2))
			{
			    ch = argv[arg][opt++];
			    if  ((ch == '-') && (ch_ctr == 0))
				neg_flag = FLAG_ON;
			    else
			    if ((ch >= '0') && (ch <= '9'))
			    {
				ch = ch - '0';
				if  (d == 0)
				    val = (val * 10) + ch;
				else
				if  (d <= 1000)  /* precision = 10**(-3)
						    truncated */
				{
				    val1 = (val1 * 10) + ch;
				    d = d * 10;
				}
			    }
			    else
			    if (ch == '.')
			    {
				d = 1;
				val1 = 0;
			    }
			    else
			    {
				if  (d != 0)
				    val = (val * d) + val1;
				else
				    d = 1;
				if  (val != 0)
				{
				    if  (neg_flag == FLAG_ON)
					val = -(val);
				    switch
					(ch)
				    {
					case ('c'):
					    d = d * 100;
					    break;
					case ('i'):
					    val = val * 254;
					    d = d * 10000;
					    break;
					case ('p'):
					    val = val * 254;
					    d = ((d * 720000) & 0x7fffffff);
					    break;
					case ('P'):
					    val = val * 254;
					    d = ((d * 60000) & 0x7fffffff);
					    break;
					default:
					    d = d * 100;  /* assume cm */
					    break;
				    }
				    if  (d != 0)  /* last chk for div by 0
						     just in case of ovflw */
				    {
					ovly_num[lp_ctr][ovly_indx-1] = val;
					ovly_den[lp_ctr][ovly_indx-1] = d;
				    }
				    if  (lp_ctr == 0)
				    {
					nc = argv[arg][opt];
					if  (((nc < '0') && (nc != '-')) ||
					     (nc > '9'))
					    ++opt; /* assume only 1 separator
						      after scale char */
				    }
				}
				val = d = 0;
				neg_flag = FLAG_OFF;
				++lp_ctr;
				ch_ctr = (-1);
				if  ((lp_ctr == 2) &&
				     ((ch == '+') || (ch == '[') ||
				      (ch == '-')))
				    --opt;	/* reparse at switch level */
			    }
			    ++ch_ctr;
			}
			--opt;	    /* to undo the automatic incr above */
		    }
		    else
			err_flag = FLAG_ON;
		    break;

		default:
/*
 *  current opt should be a pageNum
 */
		    val = 0;
		    while
			(opt < opt_len)
		    {
			ch = argv[arg][opt];
			if  ((ch >= '0') && (ch <= '9'))
			{
			    ch = ch - '0';
			    val = (val * 10) + ch;
			    ++opt;
			}
			else
			if  ((ch == '-') || (ch == ',') || (ch == '[') ||
			     (ch == ']') || (ch == '+'))
			{
			    --opt;	/* reparse opt at switch level */
			    break;
			}
			else
			if  ((ch == SPC) || (ch == '\n'))
			    break;	/* don't bother to reparse */
			else
			{
			    err_flag = FLAG_ON;
			    break;
			}
		    }
/*
 *  if the val is good, process it
 */
		    if  (val > prv_mpg_num)
		    {
			merge_pg[input_indx][merge_indx] = val;
			prv_mpg_num = val;
			++merge_indx;
		    }
		    else
			err_flag = FLAG_ON;
		    break;
	    }
/*	    end of switch (argv[arg][opt]) */

	    if  (err_flag == FLAG_ON)
	    {
		(void)sprintf(msg,
		    "--ipfe:  Invalid pagerange specification in %s (%s)\n",
				argv[arg], &(argv[arg][opt]));
		pip_error(msg);
		exit_status = 5;
	    }
	}
/*	end of for (opt=1; ((opt < opt_len) && (err_flag == FLAG_OFF));
		    ++ opt) */

}


/****************************************************************
 *
 *	int getc_testeof(file, filename):
 *	int getc_testeof:	get a char from input file;
 *				if we get an EOF here,
 *				    it's an error;
 *				else,
 *				    return the char as an int
 *
 ****************************************************************/

int getc_testeof(file, filename)

	FILE	*file;
	char	*filename;
{
	int	val;
	char	msg[128];


	val = mgetc(file);
	if  (feof(file))
	{
	    (void)sprintf(msg, "\nUnexpected EOF! in file:  %s\n", filename);
	    pip_error(msg);
	    longjmp(next_file, 1);
	}

return(val);
}


/****************************************************************
 *
 *	indent(cnt, file):
 *	indent:		put <cnt> spcs to specified file
 *
 ****************************************************************/

indent(cnt, file)

	FILE	*file;
	int	cnt;
{
	if  (file != NULL)
	{
	    if  ((file == propfile) && (cnt > 0))
		cur_col = cur_col + cnt;

	    while
		(cnt-- > 0)
	    {
		putc(' ', file);
	    }
	}
}


/****************************************************************
 *
 *	init:
 *	init_ipfe:		initialization
 *
 ****************************************************************/

init_ipfe()
{
	int	i, j;


	abort_flag = debug_flag = FLAG_OFF;
	exit_status = 0;
	ovly_flag = FLAG_OFF;
	frstpg_flag = FLAG_OFF;
	num_infiles = out_filnum = 0;
	input_indx = output_indx = sif_indx = 0;
	num_aliases = 0;
	alias_indx = 0;
	prop_level = 0;
	hdr_len = 0;
	pos = in_offset = 0;
	block_indx = 0;
	block_flag = FLAG_OFF;
	out_beg_offset[block_indx] = 0;
	tnum_blocks = tnum_pages = tnum_sifs = 0;
	for (i=0; i < NUM_FNS; i++)
	    proc_flag[i] = FLAG_OFF;
	chap_type = chap_val = 0;
	ipwrite_flag = FLAG_ON;
	merge_indx = ovly_indx = 0;

	for (i=0; i < MX_NO_INFILES; i++)
	{
	    merge_pg[i][0] = (-1);	/* init to all pages */
	    ovly_pg[i][0] = (-1);	/* init to no overlay */
	    for (j=1; j < MX_NO_MPG_ENTRIES; j++)
	    {
		merge_pg[i][j] = 0;
		ovly_pg[i][j] = (-1);
	    }
	}

	for (i=0; i < MX_BLOCK_DEPTH; i++)
	    blk_num_bytes[i] = 0;
	pg_num_bytes = 0;

	for (i=0; i < MX_NO_INFILES; i++)
	    infile_flag[i] = 0;

	outputname = propfname = 0;

/*
 *  make the beginBlock & endBlock names upper case
 */
	op_names[OP_beginBlock] = "BEGIN";
	op_names[OP_endBlock] = "END";

}


/****************************************************************
 *
 *	init_proc_file:	initialize proc_file() loop
 *
 ****************************************************************/

init_proc_file()
{

	sif_indx = 0;
	cur_indent = cur_col = 0;
	in_offset = 0;
	num_blocks = num_pages = num_sifs = 0;
	preamble_expected = FLAG_OFF;
	merge_indx = 0;
	bproc_indx = bop_indx = 0;
	bproc_flag = bop_flag = FLAG_OFF;
	ipwrite_flag = FLAG_ON;

}


/****************************************************************
 *
 *	init1:
 *	init1_ipfe:		initialization after getting cmd args
 *
 ****************************************************************/

init1_ipfe()

{
	int	i, tval, procflag, exit_flag;
	char	msg[128];


	if  (num_infiles == 0)	/* input_name req'd for now */
	    Usage();
	else
	{
/*
 *  split the 1st input_name into input_fnm & input_ext
 */
		i = mrstrichr(input_name[0], '.');
		if  (i != (-1))
		{
		    (void)mstrncpy(input_fnm, input_name[0], i);
		    (void)strcpy(input_ext, (input_name[0] + i));
		}
		else
		    (void)strcpy(input_fnm, input_name[0]);
/*
 *  Open properties file
 */
	    if  (proc_flag[PROPERTIES] == FLAG_ON)
		open_propfile();
/*
 *  Tell about rotate, xOffset, yOffset, binding offset, duplex, and scale
 */
	    if  ((proc_flag[LANDSCAPE] == FLAG_ON) ||
		 (proc_flag[ROTATE] == FLAG_ON))
	    {
		(void)sprintf(msg, "--ipfe:  Rotate %d degrees\n",
				rot_deg);
		pip_error(msg);
	    }
	    if  (proc_flag[X_OFFSET] == FLAG_ON)
	    {
		(void)sprintf(msg, "--ipfe:  xOffset = %ld/%ld meters\n",
				xoff_num, xoff_den);
		pip_error(msg);
	    }
	    if  (proc_flag[Y_OFFSET] == FLAG_ON)
	    {
		(void)sprintf(msg, "--ipfe:  yOffset = %ld/%ld meters\n",
				yoff_num, yoff_den);
		pip_error(msg);
	    }
	    if  (proc_flag[BINDING_OFFSET] == FLAG_ON)
	    {
		(void)sprintf(msg,
			"--ipfe:  Binding offset = %ld/%ld meters\n",
			boff_num, boff_den);
		pip_error(msg);
		if  (proc_flag[DUPLEX] == FLAG_ON)
		    pip_error("--ipfe:  Duplex processing set\n");
	    }
	    if  (proc_flag[SCALE] == FLAG_ON)
	    {
		(void)sprintf(msg, "--ipfe:  Scale = %ld/%ld\n",
				scale_num, scale_den);
		pip_error(msg);
	    }
/*
 *  Tell about chapterizing
 */
	    if  (proc_flag[CHAPTERIZE] == FLAG_ON)
	    {
		(void)sprintf(msg, "--ipfe:  Chapterize every %d ", chap_val);
		pip_error(msg);
		switch
		    (chap_type)
		{
		    case (CH_PAGES):
			pip_error("pages");
			break;
		    case (CH_KBYTES):
			(void)sprintf(msg, "bytes (%d kbytes)",
				(chap_val / 1024));
			pip_error(msg);
			break;
		    case (CH_MBYTES):
			(void)sprintf(msg, "bytes (%d mbytes)",
				(chap_val / 1048576));
			pip_error(msg);
			break;
		}
		pip_error("\n");
	    }
/*
 *  Show any aliases
 */
	    for (i=0; i < num_aliases; i++)
	    {
		if  (i == 0)
		    pip_error("--ipfe:  ");
		else
		    pip_error("--       ");
		(void)sprintf(msg, "Alias: %s=%s\n",
			      alias_ref[i], alias_act[i]);
		pip_error(msg);
	    }

	    if  (num_infiles > 1)
		proc_flag[CONCAT] = FLAG_ON;

	    if  (proc_flag[DEBUG] == FLAG_ON)
		off_indent = 10;
	    else
		off_indent = 0;

/*
 *  For now, IF the properties option flag is SET AND there is no output
 *  file specified,
 *  THEN set the environment for properties only (don't open output)
 */
	    if  ((proc_flag[PROPERTIES] == FLAG_ON) &&
		 (outputname == 0))
		;	/* process properties only; no output */
	    else
	    {
/*
 *  open the output file
 */
		if  (outputname == 0)
		{
/*
 *  construct a default output filename by concatenating ".ip" to the
 *    fnm component of the 1st input filename, making sure the resulting
 *    filename doesn't already exist; if it does exist, append "1" to
 *    the fnm, then "2" to the fnm, etc. until the filename doesn't exist
 */
		    tval = 0;
		    exit_flag = procflag = FLAG_OFF;
		    while
			(exit_flag == FLAG_OFF)
		    {
			(void)strcpy(work_fname, input_fnm);
			if  (procflag == FLAG_ON)
			    (void)strcat(work_fname, itostr(tval));
			(void)strcat(work_fname, ".ip");
			_fmode = BINARY_FILE;
			workfile = fopen(work_fname, "r");
			if  (workfile == NULL)
			{
			    outputname = work_fname;
			    exit_flag = FLAG_ON;
			}
			else
			{
			    (void)fclose(workfile);
			    ++tval;
			    procflag = FLAG_ON;
			}
		    }
		}

		(void)strcpy(output_name, outputname);
		outputname = output_name;
		i = mrstrichr(outputname, '.');
		if  (i != (-1))
		{
		    (void)mstrncpy(output_fnm, outputname, i);
		    (void)strcpy(output_ext, (outputname + i));
		}
		else
		    (void)strcpy(output_fnm, outputname);

		if  (proc_flag[CHAPTERIZE] == FLAG_ON)
		{
		    (void)strcpy(output_name, output_fnm);
		    out_filnum = 1;
		    (void)strcat(output_name, itostr(out_filnum));
		    (void)strcat(output_name, output_ext);
		}

		open_output();
	    }
	}

}


/****************************************************************
 *
 *	iocopyn(length, file, filename):
 *	iocopyn:		copy <length> bytes from specified
 *				file to output, checking for
 *				unexpected EOF
 *
 ****************************************************************/

iocopyn(length, file, filename)

	FILE	*file;
	long	length;
	char	*filename;
{
    
	while
	    (length-- > 0)
	{
	    ip_putc(getc_testeof(file, filename));
	}
}


/****************************************************************
 *
 *	iogetn(length, file, filename, buf):
 *	iogetn:		read <length> bytes from specified
 *				file into specified buf, checking
 *				for unexpected EOF
 *				-- throw the bytes away, for now
 *
 ****************************************************************/

iogetn(length, file, filename, buf)

	FILE	*file;
	long	length;
	char	*filename, *buf;
{
	while
	    (length-- > 0)
	{
	    *buf++ = getc_testeof(file, filename);
	}
	*buf = '\0';
}


/****************************************************************
 *
 *	ioputn(length, buf):
 *	ioputn:		put <length> bytes from specified
 *				buf to output
 *
 ****************************************************************/

ioputn(length, buf)

	long	length;
	char	*buf;
{
	while
	    (length-- > 0)
	{
	    ip_putc(*buf++);
	}
}


/****************************************************************
 *
 *	ioreadn(length, file, filename):
 *	ioreadn:		read <length> bytes from specified
 *				file, checking for unexpected EOF
 *				-- throw the bytes away, for now
 *
 ****************************************************************/

ioreadn(length, file, filename)

	FILE	*file;
	long	length;
	char	*filename;
{
    
	while
	    (length-- > 0)
	{
	    (void)getc_testeof(file, filename);	/* throw it away */
	}
}


/****************************************************************
 *
 *	ip_putc(ch):
 *	ip_putc:		put char to (interpress) output &
 *				bump out_offset
 *
 ****************************************************************/

ip_putc(ch)

	char	ch;
{
	if  ((output != NULL) && (ipwrite_flag == FLAG_ON))
	{
	    putc(ch, output);
	    ++out_offset;
	}
}



/****************************************************************
 *
 *	ip_puts(string):
 *	ip_puts:		put string to (interpress) output
 *				& add strlen(string) to out_offset
 *
 ****************************************************************/

ip_puts(string)

	char	*string;
{
	if  ((output != NULL) && (ipwrite_flag == FLAG_ON))
	{
	    fprintf(output, string);
	    out_offset = out_offset + strlen(string);
	}

}


/****************************************************************
 *
 *	char *itostr(ival):
 *	char *itostr:		cvt int to ascii string; return ptr
 *
 ****************************************************************/

char *itostr(ival)

	int	ival;
{
	int	i, digit, digit_flag;
	char	num_buf[17], *s;


	s = num_buf;
	digit_flag = FLAG_OFF;
	for (i=10000; i > 0; i=i/10)
	{
	    digit = ival / i;
	    if  ((digit == 0) && (digit_flag == FLAG_OFF) && (i > 1))
		;	/* do nothing */
	    else
	    {
		*s = digit + '0';
		++s;
		ival = ival - (digit * i);
		digit_flag = FLAG_ON;
	    }
	}
	*s = '\0';

return(num_buf);
}


/****************************************************************
 *
 *	mgetc(file):
 *	mgetc:		get char from input & bump
 *				appropriate counters
 *
 ****************************************************************/

int mgetc(file)

	FILE	*file;
{
	int	val;


	val = getc(file);
	if  (!(feof(file)))
	{
	    val = val & 0xff;
	    ++in_offset;
	    ++blk_num_bytes[block_indx];
	    ++pg_num_bytes;
	}

return(val);
}


/****************************************************************
 *
 *	mputc_prop(ch):
 *	mputc_prop:		put char to propfile
 *
 ****************************************************************/

mputc_prop(ch)
{
	if  ((propfile != NULL) && (ovly_flag == FLAG_OFF))
	{
	    putc(ch, propfile);
	    if  (ch == '\n')
		cur_col = 0;
	}

}

/****************************************************************
 *
 *	int mrstrichr(str, ch):
 *	int mrstrichr:	reverse srch for char in string;
 *				if fnd,
 *				  return its pos (indexed from 0);
 *				else
 *				  return (-1)
 *
 ****************************************************************/

int mrstrichr(str, ch)

	char	*str, ch;
{
	int	i, indx, retcd;


	retcd = (-1);
	indx = strlen(str) - 1;
	str = str + indx;
	for (i=indx; i >= 0; i--, str--)
	{
	    if  (*str == ch)
	    {
		retcd = i;
		break;
	    }
	}

return(retcd);
}


/****************************************************************
 *
 *	int mstrncmp(s1, s2, len):
 *	int mstrncmp:		cmp s1 to s2 for max of len bytes
 *				(byte-wise compare, left to right);
 *				if *s1 != *s2,
 *				  return (int)(*s1 - *s2)
 *				  (i.e., <0 if s1<s2, >0 if s1>s2)
 *				else
 *				  return 0
 *
 ****************************************************************/

int mstrncmp(s1, s2, len)

	int	len;
	char	*s1, *s2;
{
	int	i, retcd;


	retcd = 0;
	for (i=0; i < len; i++, s1++, s2++)
	{
	    if  (*s1 != *s2)
	    {
		retcd = (int)(*s1 - *s2);
		break;
	    }
	}

return(retcd);
}


/****************************************************************
 *
 *	int mstrncpy(to, from, len):
 *	int mstrncpy:		copy from -> to for len; return
 *				actual # bytes copied (will be
 *				< len if <from> is null-terminated
 *				shorter than len);
 *				guarantees not overwriting allocated
 *				space for <to> if sizeof(to) is at
 *				least len + 1 (i.e., if len =
 *				sizeof(to) - 1;
 *				destination <to> is always null-
 *				terminated
 *
 ****************************************************************/

int mstrncpy(to, from, len)

	int	len;
	char	*to, *from;
{
	int	cnt;


	cnt = 0;
	while
	    ((len-- > 0) && (*from != '\0'))
	{
	    *to++ = *from++;
	    ++cnt;
	}
	*to = '\0';

return(cnt);
}


/****************************************************************
 *
 *	open_logfile(argc, argv):
 *	open_logfile:		open the logfile for processing
 *
 ****************************************************************/

open_logfile(argc, argv)

	int	argc;
	char	*argv[];
{
	int	i;


	_fmode = TEXT_FILE;
	logfile = fopen(logfilename, "w");
	if  (logfile == NULL)
	{
	    fprintf(STDERR,
		    "--ipfe:  Can't open log file: %s\n", logfilename);
	    *logfilename = '\0';
	}
	else
	{
	    fprintf(STDERR, "--ipfe:  Log file: %s is open\n", logfilename);

	    fprintf(logfile, "\n");
	    for (i=0; i < argc; i++)
		fprintf(logfile, "%s ", argv[i]);
	    fprintf(logfile, "\n\n");
	}

}


/****************************************************************
 *
 *	open_output:		open the output for processing
 *
 ****************************************************************/

open_output()
{
	char	msg[128];


	_fmode = BINARY_FILE;
	output = fopen(outputname, "w");
	if  (output == NULL)
	{
	    terrno = errno;		/* save errno */
	    (void)sprintf(msg,
		"--ipfe:  Error opening output file: %s, retcd=%d\n",
		outputname, errno);
	    pip_error(msg);
	    errno = terrno;
	    exit(2);
	}

}


/****************************************************************
 *
 *	open_propfile:	open the properties file for processing
 *
 ****************************************************************/

open_propfile()
{
	char	msg[128];


	_fmode = TEXT_FILE;
	if  (propfname == 0)
	{
	    (void)strcpy(prop_fname, input_fnm);
	    (void)strcat(prop_fname, ".prop");
	    propfname = prop_fname;

	    (void)sprintf(msg,
	    "--ipfe:  No properties filename specified. Using: %s\n",
			propfname);
	    pip_error(msg);
	}

	propfile = fopen(propfname, "w");
	if  (propfile == NULL)
	{
	    (void)sprintf(msg,
		"--ipfe:  Can't open properties file: %s\n", propfname);
	    pip_error(msg);
	    *propfname = '\0';
	}
	else
	{
	    (void)sprintf(msg,
		"--ipfe:  Properties file: %s is open\n", propfname);
	    pip_error(msg);
	}

}


/****************************************************************
 *
 *	char *op_name(op):
 *	char *op_name:	check op_names[] table of op_code
 *				name strings for non-null entry;
 *				if non_null,
 *				  return ptr to the string,
 *				else,
 *				  return ptr to string
 *				  "--Unknown op: <op>"
 *
 ****************************************************************/
    
char *op_name(op)

	int	op;
{
	char	buf[20];

	if  (op_names[op] == NULL)
	{
	    (void)sprintf(buf, "--Unknown op: %d --", op);
	    return(buf);
	}
	else
	    return(op_names[op]);
}


/****************************************************************
 *
 *	pip_error(msg):
 *	pip_error:		put error msg to stdout & logfile
 *
 ****************************************************************/
    
pip_error(msg)

	char	*msg;
{
	if  (proc_flag[QUIET] != FLAG_ON)
	    fprintf(STDERR, msg);
	if  (logfile != NULL)
	    fprintf(logfile, msg);
}



/****************************************************************
 *
 *	pip_prop(msg):
 *	pip_prop:		put msg to propfile
 *
 ****************************************************************/
    
pip_prop(msg)

	char	*msg;
{
	if  ((propfile != NULL) && (ovly_flag == FLAG_OFF))
	{
	    fprintf(propfile, msg);
	    if  (*(msg + strlen(msg) - 1) == '\n')
		cur_col = 0;
	    else
		cur_col = cur_col + strlen(msg);

	}

}


/****************************************************************
 *
 *	preserve_SIF_copy(type_byte, length, file, filename):
 *	preserve_SIF_copy:	preserve SIF reference by copying
 *				from specified input file to output
 *
 ****************************************************************/
    
copy_preserve_SIF(type_byte, length, file, filename)

	FILE	*file;
	long	length;
	int	type_byte;
	char	*filename;
{
	put_seq_type_len(type_byte, length);
	iocopyn(length, file, filename);
}



/****************************************************************
 *
 *	preserve_SIF_put(type_byte, length, buf):
 *	preserve_SIF_put:	preserve SIF reference by copying
 *				from specified buffer to output
 *
 ****************************************************************/
    
put_preserve_SIF(type_byte, length, buf)

	long	length;
	int	type_byte;
	char	*buf;
{
	put_seq_type_len(type_byte, length);
	ioputn(length, buf);
}


/****************************************************************
 *
 *	proc_alias(alias_string):
 *	proc_alias:		process alias argument
 *
 ****************************************************************/

proc_alias(alias_string)

	char	*alias_string;
{
	int	indx, len;
	char	msg[128];


	len = strlen(alias_string);
	indx = mrstrichr(alias_string, ':');
	if  ((indx > 0) && (indx < (len - 1)))  /* past beg & before end */
	{
	    (void)mstrncpy(alias_ref[alias_indx], alias_string, indx);
	    ++indx;
	    (void)mstrncpy(alias_act[alias_indx], (alias_string + indx),
			   (len - indx));
	    ++alias_indx;
	    ++num_aliases;
	}
	else
	{
	    (void)sprintf(msg,
		"--ipfe:  *** warning -- invalid alias specification (%s)\n",
				alias_string);
	    pip_error(msg);
	    exit_status = 4;
	}

}


/****************************************************************
 *
 *	proc_chapterize:	test for & process chapterizing
 *
 ****************************************************************/
    
proc_chapterize()
{
	long	len, temp_end_offset;
	int	i, op, terrno, tblock_indx, proc_op_flag, eof_flag, proc_type;
	int	type_byte;		/* must be int for stdio EOF */
	char	msg[128];


	if  ((proc_flag[CHAPTERIZE] == FLAG_ON) &&
	     (((chap_type == CH_PAGES) && (out_numpages == chap_val)) ||
	      (((chap_type == CH_KBYTES) || (chap_type == CH_MBYTES)) &&
		    (out_offset >= chap_val))))
	{
	    
/*
 *  Eventually, the next two lines should be rewritten to check to see
 *  if we might somehow be nested in a body before putting the
 *  OP_endblock(s)
 */
    
	    for (i=0; i < block_indx; i++)
		put_op(OP_endBlock);
    
	    (void)fclose(output);

/*
 *  Now, check for OP_endBlock(s) appearing immediately in the input
 *  file. If so, skip them: they were already put out above. In looking,
 *  if we get an EOF and there are no more input files, don't open a
 *  new output file.
 */

	    proc_op_flag = eof_flag = FLAG_OFF;
	    tblock_indx = block_indx;	/* because the actual block_indx can
					     be dcr'd in the loop */
	    for (i=0; i <= tblock_indx; i++)
	    {
		type_byte = mgetc(input);
		if  (feof(input))
		{
/*
 *  set eof_flag only if there are no more input files
 */
		    if  (input_indx >= (num_infiles - 1))
			eof_flag = FLAG_ON;
		    break;
		}
		else
		if  ((type_byte & OP_Mask) == (LONG_OP & OP_Mask))
		{
		    op = ((type_byte & 0x1f) << 8) +
			    getc_testeof(input, inputname);
		    if  ((op == OP_endBlock) && (i < block_indx))
			--block_indx;
		    else
		    {
			proc_op_flag = FLAG_ON;	/* set flag for use below */
			proc_type = LONG_OP;
			break;
		    }
		}
		else
		{
		    proc_op_flag = FLAG_ON;	/* set flag for use below */
		    proc_type = ~(LONG_OP);
		    break;
		}
	    }    
	    if  (eof_flag == FLAG_OFF)
	    {
		(void)strcpy(work_fname, outputname);
		workfile = fopen(work_fname, "r");
		if  (workfile == NULL)
		{
		    terrno = errno;		/* save errno */
		    pip_error(
		    "Chapterize error opening previous output file as input\n");
		    (void)sprintf(msg, "  file=%s, retcd=%d\n",
				  outputname, errno);
		    pip_error(msg);
		    pip_error("  Can't complete chapterization --\n\n");
		    pip_error("  *** ABORTING ***\n");
		    errno = terrno;
		    exit(3);
		}
	        else
	        {
		    ++out_filnum;
		    (void)strcpy(output_name, output_fnm);
		    (void)strcat(output_name, itostr(out_filnum));
		    (void)strcat(output_name, output_ext);
		    (void)sprintf(msg,
				  "--ipfe:  Processing output file: %s\n",
				  outputname);
		    pip_error(msg);
    
		    _fmode = BINARY_FILE;
		    output = fopen(outputname, "w");
		    if  (output == NULL)
		    {
			terrno = errno;		/* save errno */
			(void)sprintf(msg,
			  "--ipfe:  Error opening output file=%s, retcd=%d\n",
			  outputname, errno);
			pip_error(msg);
			errno = terrno;
			exit(4);
		    }
		    else
		    {
/*
 *  Then, copy Hdr & BEGIN/Preamble(s) to new output
 */
			out_numpages = 0;
			out_offset = 0;
			for (i=0; i <= block_indx; i++)
			{
			    len = out_end_offset[i] - out_beg_offset[i] + 1;
			    out_beg_offset[i] = out_offset;
			    iocopyn(len, workfile, work_fname);
			    temp_end_offset = out_end_offset[i];
			    out_end_offset[i] = out_offset - 1;
			    if  (i < block_indx)  /* if there is more to do */
			    {
				len = out_beg_offset[i+1] -
						temp_end_offset - 1;
				ioreadn(len, workfile, work_fname);
			    }
			}
		    }
		    (void)fclose(workfile);
/*
 *  Finally, if a token other than OP_endBlock token was read above,
 *  process it
 */
		    if  (proc_op_flag == FLAG_ON)
		    {
			if  (proc_type == LONG_OP)
			    proc_long_op(op);
			else
			    proc_ip_token(type_byte, input, inputname);
		    }
		}
	    }
	}

}


/****************************************************************
 *
 *	proc_file:		process the input file
 *
 ****************************************************************/
    
proc_file()
    
{
	long	offset;
	int	i, op, retcd, exit_flag;
	int	type_byte;		/* must be int for stdio EOF */
	char	msg[128];


	if  (setjmp(next_file) != 0)	/* for error recovery of */
	{				/*   unexpected EOF      */
	    return;
	}
    

	offset = in_offset;
	retcd = get_ip_hdr(input, inputname);	/* get the hdr */
	if  (retcd == OK)
	{
	    (void)sprintf(msg, "-- Hdr :  %s--\n", hdr_workbuf);
	    pip_error(msg);
	    mputc_prop('\n');
	    put_offset_msg_to_prop(offset, hdr_workbuf);
	    pip_prop("--\n");

	    if  (input_indx == 0)		/* if 1st file,   */
	    {
		if  (proc_flag[CONCAT] == FLAG_ON)
		    (void)strcpy((hdr_workbuf+IP_VERS_OFFSET), "2.1 ");
		(void)strcpy(hdr_buf, hdr_workbuf);	/* save hdr */
		if  (output != NULL)
		{
		    (void)sprintf(msg, "\nProcessing output file: %s\n",
			outputname);
		    pip_error(msg);
		    (void)sprintf(msg, "-- Output Hdr:  %s--\n", hdr_workbuf);
		    pip_error(msg);
		    ip_puts(hdr_workbuf);	/* copy hdr & add len to
						   out_offset */
		}

/*
 *  copy everything up to, but NOT including (will include later),
 *    OP_beginBlock
 */
		exit_flag = FLAG_OFF;
		while
		    (exit_flag == FLAG_OFF)
		{
		    type_byte = getc_testeof(input, inputname);
		    if  ((type_byte & OP_Mask) == (LONG_OP & OP_Mask))
		    {
			op = ((type_byte & 0x1f) << 8) +
			      getc_testeof(input, inputname);
			if  (op == OP_beginBlock)
			    exit_flag = FLAG_ON;
			else
			{
			    ip_putc(type_byte);
			    ip_putc(op);
			}
		    }
		    else
			proc_ip_token(type_byte, input, inputname);
		}
		out_end_offset[0] = out_offset - 1;
/*
 *  if concatenating, wrap a BEGIN empty_preamble END after the hdr &
 *    instructionsBody and around the rest of the output
 */
		if  (proc_flag[CONCAT] == FLAG_ON)
		{
		    ++block_indx;
		    out_beg_offset[block_indx] = out_offset;
		    put_op(OP_beginBlock);
		    put_op(OP_beginBody);
		    put_op(OP_endBody);
		    out_end_offset[block_indx] = out_offset - 1;
		}
/*
 *  NOW include the OP_beginBlock, & process the rest of the master
 */
		proc_long_op(OP_beginBlock);
	    }
	    prc_ip_tokens(input, inputname);

	    mputc_prop('\n');
	    indent(off_indent, propfile);
	    (void)sprintf(msg, "--Number of Blocks:%4d\n", num_blocks);
	    pip_prop(msg);
	    indent(off_indent, propfile);
	    (void)sprintf(msg, "--Number of Pages :%4d\n", num_pages);
	    pip_prop(msg);
	    indent(off_indent, propfile);
	    (void)sprintf(msg, "--Number of SIFs  :%4d\n", num_sifs);
	    pip_prop(msg);

	    for (i=0; merge_pg[input_indx][i] != 0; i++)
	    {
		if  (merge_pg[input_indx][i] > num_pages)
		{
		    (void)sprintf(msg,
		  "\n--ipfe:  *** Page %d selected > # of pages in %s (%d)\n",
				  merge_pg[input_indx][i], inputname,
				  num_pages);
		    pip_error(msg);
		    break;
		}
	    }
	}
/***
	else
	{
	    (void)sprintf(msg, "(***  PROCESSING ABORTED  ***)\n");
	    pip_error(msg);
	    input_indx = num_infiles;
	    abort_flag = FLAG_ON;
	}
***/
}


/****************************************************************
 *
 *	prc_IF_bof(ifile, ifilename):
 *	prc_IF_bof:		read specified Interpress file &
 *				discard everything up to, and
 *				including, first OP_beginBlock
 *
 ****************************************************************/

prc_IF_bof(ifile, ifilename)

	FILE	*ifile;
	char	*ifilename;
{
	int	op, exit_flag;
	int	type_byte;		/* must be int for stdio EOF */


	exit_flag = FLAG_OFF;
	while
	    (exit_flag == FLAG_OFF)
	{
	    type_byte = getc_testeof(ifile, ifilename);
	    if  ((type_byte & OP_Mask) == (LONG_OP & OP_Mask))
	    {
		op = ((type_byte & 0x1f) << 8) +
		      getc_testeof(ifile, ifilename);
		if  (op == OP_beginBlock)
		    exit_flag = FLAG_ON;		
	    }
	}
}


/****************************************************************
 *
 *	prc_IF_fragment(ifile, ifilename):
 *	prc_IF_fragment:	process specified Interpress fragment
 *				(i.e., skip everything up to, and
 *				 including the first OP_beginBlock;
 *				 then include everything up to, but
 *				 not including, the next OP_endBlock)
 *
 ****************************************************************/

prc_IF_fragment(ifile, ifilename)

	FILE	*ifile;
	char	*ifilename;
{
	int	op, exit_flag;
	int	type_byte;		/* must be int for stdio EOF */


/*
 *  read & discard everything up to, and including, OP_beginBlock
 */
	prc_IF_bof(ifile, ifilename);

/*
 *  read & copy everything up to, but NOT including OP_endBlock
 */
	exit_flag = FLAG_OFF;
	while
	    (exit_flag == FLAG_OFF)
	{
	    type_byte = getc_testeof(ifile, ifilename);
	    if  ((type_byte & OP_Mask) == (LONG_OP & OP_Mask))
	    {
		op = ((type_byte & 0x1f) << 8) +
		      getc_testeof(ifile, ifilename);
		if  (op == OP_endBlock)
		    exit_flag = FLAG_ON;		
		else
		    put_op(op);
	    }	    
	    else
		proc_ip_token(type_byte, ifile, ifilename);
	}
}


/****************************************************************
 *
 *	prc_IF_master(ifile, ifilename, pg):
 *	prc_IF_master:	process specified page of the
 *				specified Interpress master
 *				(i.e., skip everything up to, and
 *				 including the first OP_beginBlock;
 *				 then include the contents of the
 *				 preamble and the specified page,
 *				 without including the body tokens)
 *
 ****************************************************************/

prc_IF_master(ifile, ifilename, pg)

	FILE	*ifile;
	int	pg;
	char	*ifilename;
{
	int	op, exit_flag, wrt_flag, blk_indx, pg_cnt;
	int	level[MX_BLOCK_DEPTH], ctr[MX_BLOCK_DEPTH];
	int	type_byte;		/* must be int for stdio EOF */


/*
 *  read & discard everything up to, and including, OP_beginBlock
 */
	prc_IF_bof(ifile, ifilename);

/*
 *  read & copy the contents of the preamble; then, copy only the
 *  contents of the specified page
 */
	blk_indx = pg_cnt = 0;
	level[blk_indx] = ctr[blk_indx] = 0;
	wrt_flag = FLAG_OFF;
	exit_flag = FLAG_OFF;
	while
	    (exit_flag == FLAG_OFF)
	{
	    type_byte = getc_testeof(ifile, ifilename);
	    if  ((type_byte & OP_Mask) == (LONG_OP & OP_Mask))
	    {
		op = ((type_byte & 0x1f) << 8) +
		      getc_testeof(ifile, ifilename);
		if  (op == OP_beginBlock)
		{
		    ++blk_indx;
		    level[blk_indx] = ctr[blk_indx] = 0;
		}
		else
		if  (op == OP_endBlock)
		{
		    if  (blk_indx > 0)
			--blk_indx;
		    else
			exit_flag = FLAG_ON;
		}
		else
		if  (op == OP_beginBody)
		{
		    if  (level[blk_indx] == 0)
		    {
			if  (ctr[blk_indx] == 0)
			    wrt_flag = FLAG_ON;	     /* processing preamble */
			else
			{
			    ++pg_cnt;
			    if  (pg == pg_cnt)
				wrt_flag = FLAG_ON;	/* got the right pg */
			}
			++ctr[blk_indx];
		    }
		    else
		    if  (wrt_flag == FLAG_ON)
			put_op(op);
		    ++level[blk_indx];
		}
		else
		if  (op == OP_endBody)
		{
		    --level[blk_indx];
		    if  (level[blk_indx] == 0)
		    {
			wrt_flag = FLAG_OFF;
			if  (pg == pg_cnt)
			    exit_flag = FLAG_ON;
		    }
		    else
		    if  (wrt_flag == FLAG_ON)
			put_op(op);
		}
		else
		if  (wrt_flag == FLAG_ON)
		    put_op(op);
	    }	    
	    else
	    if  (wrt_flag == FLAG_ON)
		proc_ip_token(type_byte, ifile, ifilename);
	}
}


/****************************************************************
 *
 *	proc_ip_token(type_byte, file, filename):
 *	proc_ip_token:	process interpress token of specified
 *				type from specified file
 *
 ****************************************************************/

proc_ip_token(type_byte, file, filename)

	FILE	*file;
	int	type_byte;
	char	*filename;
{
	long	len;
	int	op;


	if  (type_byte != EOF)
	{
	    if ((type_byte & 0x80) == 0)  /* token is a SHORT NUMBER */
	    {
		ip_putc(type_byte);			/* 2-byte token */
		ip_putc(getc_testeof(file, filename));
	    }
	    else			  /* token is something else */
	    {
		switch
		    (type_byte & OP_Mask)
		{
		    case (SHORT_OP & OP_Mask):
			ip_putc(type_byte);		/* 1-byte token */
			break;
	
		    case (LONG_OP & OP_Mask):
			op = ((type_byte & 0x1f) << 8) +
			      getc_testeof(file, filename); /* 2-byte token */
			proc_long_op(op);
			break;
	
		    case (SHORT_SEQUENCE & OP_Mask):
			len = getc_testeof(file, filename);
			proc_sequence(type_byte, len, file, filename);
			break;
	
		    case (LONG_SEQUENCE & OP_Mask):
			len = getc_testeof(file, filename) << 16;
			len = len + (getc_testeof(file, filename) << 8);
			len = len + getc_testeof(file, filename);
			proc_sequence(type_byte, len, file, filename);
			break;
		}
	    }
	}

}


/****************************************************************
 *
 *	prc_ip_tokens(file, filename):
 *	prc_ip_tokens:	process interpress tokens
 *				specified file
 *
 ****************************************************************/

prc_ip_tokens(file, filename)

	FILE	*file;
	char	*filename;
{
	int	type_byte;		/* must be int for stdio EOF */

    
	while
	    ((type_byte = mgetc(file)) != EOF)
	{
	    proc_ip_token(type_byte, file, filename);
	}

}


/****************************************************************
 *
 *	proc_long_op(op):
 *	proc_long_op:		process long_op
 *
 ****************************************************************/

proc_long_op(op)

	int	op;
{
	long	val;
	int	i, nesting_level;


	switch
	    (op)
	{
	    case (OP_beginBlock):
		put_op(op);
		++num_blocks;
		++tnum_blocks;
		if  (block_indx == 0)
		    out_end_offset[block_indx] = out_offset - 3;
		++block_indx;
		out_beg_offset[block_indx] = out_offset - 2;
		blk_num_bytes[block_indx] = 2;
		put_opname_to_prop(op);
		preamble_expected = FLAG_ON;
		break;
	    case (OP_endBlock):
		put_op(op);
		put_opname_to_prop(op);
		--block_indx;
		break;
	    case (OP_beginBody):
		put_opname_to_prop(op);
		if  (preamble_expected == FLAG_ON)
		{
		    put_op(op);
		    preamble_expected = FLAG_OFF;
		    endpreamble_expected = FLAG_ON;
		}
		else
		if  (bop_flag == FLAG_ON)
		{
		    put_op(op);
		    if  (bop_iindx[bop_indx] == 0)
			bop_preamble_expected[bop_indx] = FLAG_ON;
		    else
		    if  ((bop_iindx[bop_indx] == 1) &&
			 (bop_preamble_expected[bop_indx] == FLAG_ON))
			bop_preamble_expected[bop_indx] = FLAG_OFF;

		    ++bop_iindx[bop_indx];
		}
		else		/* we have a righteous pageBody */
		{
		    ++num_pages;
		    ++tnum_pages;
		    frstpg_flag = FLAG_ON;
		    pg_num_bytes = 2;

		    if  ((merge_pg[input_indx][merge_indx] == 0) ||
			 (num_pages < merge_pg[input_indx][merge_indx]))
			ipwrite_flag = FLAG_OFF;
		    else
		    {
			ipwrite_flag = FLAG_ON;	/* it should already be on */
			put_op(op);
			if  ((proc_flag[LANDSCAPE] == FLAG_ON) ||
			     (proc_flag[ROTATE] == FLAG_ON))
			{
			    put_Integer((long)rot_deg);
			    put_op(OP_rotate);
			    put_op(OP_concatt);
			    if  ((rxoff_den == 0) || (rxoff_den == 1))
				put_Integer(rxoff_num);
			    else
				put_seqRational(rxoff_num, rxoff_den);
			    if  ((ryoff_den == 0) || (ryoff_den == 1))
				put_Integer(ryoff_num);
			    else
				put_seqRational(ryoff_num, ryoff_den);
			    put_op(OP_translate);
			    put_op(OP_concatt);
			}
			if  (proc_flag[BINDING_OFFSET] == FLAG_ON)
			{
			    if  ((proc_flag[DUPLEX] == FLAG_ON) &&
				 ((num_pages % 2) == 0))
				val = -(boff_num);
			    else
				val = boff_num;
			    if  (proc_flag[LANDSCAPE] == FLAG_ON)
			    {
				put_Integer(0L);	/* no xShift */
				if  ((boff_den == 0) || (boff_den == 1))
				    put_Integer(-(val)); /* -(yShift) */
				else
				    put_seqRational(-(val), boff_den);
			    }
			    else
			    {
				if  ((boff_den == 0) || (boff_den == 1))
				    put_Integer(val);
				else
				    put_seqRational(val, boff_den);
				put_Integer(0L);	/* no yShift */
			    }
			    put_op(OP_translate);
			    put_op(OP_concatt);
			}
			if  ((proc_flag[X_OFFSET] == FLAG_ON) ||
			     (proc_flag[Y_OFFSET] == FLAG_ON))
			{
			    if  ((xoff_den == 0) || (xoff_den == 1))
				put_Integer(xoff_num);
			    else
				put_seqRational(xoff_num, xoff_den);

			    if  ((yoff_den == 0) || (yoff_den == 1))
				put_Integer(yoff_num);
			    else
				put_seqRational(yoff_num, yoff_den);
			    put_op(OP_translate);
			    put_op(OP_concatt);
			}
			if  (proc_flag[SCALE] == FLAG_ON)
			{
			    if  ((scale_den == 0) | (scale_den == 1))
				put_Integer(scale_num);
			    else
				put_seqRational(scale_num, scale_den);
			    put_op(OP_scale);
			    put_op(OP_concatt);
			}

			if  ((merge_pg[input_indx][merge_indx] == (-1)) &&
			     (num_pages ==
				merge_pg[input_indx][(merge_indx+1)]))
			    ++merge_indx;
			if  ((num_pages ==
				merge_pg[input_indx][merge_indx]) &&
			     (ovly_pg[input_indx][merge_indx] != (-1)))
			{
			    put_op(OP_dosavesimplebody);
			    put_op(OP_beginBody);
			}
		    }
		}
		begBody_flag = FLAG_ON;
		break;
	    case (OP_endBody):
		put_opname_to_prop(op);
		if  (bop_flag == FLAG_ON)
		{
		    put_op(op);
		    --bop_iindx[bop_indx];
		    if  ((bop_iindx[bop_indx] == 0) && (bop_indx > 0))
			--bop_indx;
		    if  ((bop_indx == 0) && (bop_iindx[bop_indx] == 0))
			bop_flag = FLAG_OFF;
		}
		else	/* bop_flag == FLAG_OFF */
		{
		    if  (endpreamble_expected == FLAG_ON)
		    /*	then processing preamble -- can't chapterize */
		    {
			put_op(op);
			endpreamble_expected = FLAG_OFF;
/*
 *  this next code determines the length of each BEGIN/Preamble to copy
 *  for chapterizing as part of handling nested blocks
 */
			out_end_offset[block_indx] = out_offset - 1;
		    }
		    else	/* we have a righteous endBody */
		    {
			if  (ipwrite_flag == FLAG_ON)
			{
			    put_op(op);
			    ++out_numpages;
			    ovly_flag = FLAG_OFF;
			    while
			        (num_pages ==
				    merge_pg[input_indx][merge_indx])
			    {
				if  (ovly_pg[input_indx][merge_indx] != (-1))
				{
				    ovly_flag = FLAG_ON;
				    proc_overlay();
				}
				++merge_indx;
			    }
			    if  (ovly_flag == FLAG_ON)
			    {
				put_op(OP_endBody);
				ovly_flag = FLAG_OFF;
			    }
			    proc_chapterize();
			}
			if  (merge_pg[input_indx][merge_indx] == 0)
			{
			    if  (proc_flag[CONCAT] == FLAG_ON)
				nesting_level = block_indx - 1;
			    else
				nesting_level = block_indx;

			    for (i=0; i < nesting_level; i++)
				put_op(OP_endBlock);
			    ipwrite_flag = FLAG_OFF;
			}
			else
			    ipwrite_flag = FLAG_ON;
		    }
		}
		break;
	    case (OP_makesimpleco):
	    case (OP_dosavesimplebody):
	    case (OP_if):
	    case (OP_ifelse):
	    case (OP_ifcopy):
	    case (OP_correct):
		put_op(op);
		put_opname_to_prop(op);
		if  (bop_flag == FLAG_OFF)
		{
		    bop_indx = 0;
		    bop_flag = FLAG_ON;
		}
		else
		    ++bop_indx;
		bop_iindx[bop_indx] = 0;
		bop_preamble_expected[bop_indx] = FLAG_OFF;
		break;
	    default:
		put_op(op);
		break;
	}
}


/****************************************************************
 *
 *	proc_overlay:		process the current
 *				  ovly_pg[input_indx][merge_indx]
 *
 ****************************************************************/

proc_overlay()
{

	put_op(OP_dosavesimplebody);
	put_op(OP_beginBody);
	ovly_indx = ovly_pg[input_indx][merge_indx];
	if  ((ovly_den[0][ovly_indx] != 0) ||
	     (ovly_den[1][ovly_indx] != 0))
	{
	    if  ((ovly_den[0][ovly_indx] == 0) ||
		 (ovly_den[0][ovly_indx] == 1))
		put_Integer(ovly_num[0][ovly_indx]);
	    else
		put_seqRational(ovly_num[0][ovly_indx],
				ovly_den[0][ovly_indx]);
	    if  ((ovly_den[1][ovly_indx] == 0) ||
		 (ovly_den[1][ovly_indx] == 1))
		put_Integer(ovly_num[1][ovly_indx]);
	    else
		put_seqRational(ovly_num[1][ovly_indx],
				ovly_den[1][ovly_indx]);
	    put_op(OP_translate);
	    put_op(OP_concatt);
	}
	proc_ovly_file(ovly_fname[ovly_indx]);
	put_op(OP_endBody);

}


/****************************************************************
 *
 *	prc_ovly_file(fname):
 *	proc_ovly_file:	try to open specified filename;
 *				if open_err,
 *				    create a SIF reference
 *				else,
 *				    process as a SIF
 *
 ****************************************************************/

proc_ovly_file(fname)

	char	*fname;
{
	int	retcd, pgnum;
	char	msg[128];


	ovly_file = fopen(fname, "r");
	if  (ovly_file == NULL)
	{
	    (void)sprintf(msg,
		"                  --Error opening overlay file:  %s\n",
		fname);
	    pip_error(msg);
	    create_SIF(fname);
	}
	else
	{
	    retcd = get_ip_hdr(ovly_file, fname);
	    if  (retcd == ERROR)
		create_SIF(fname);
	    else
	    {
/***
	The following assumes we're not already nested in a body_operator
	(i.e., that we were at a real endBody when invoked)
***/
		bop_flag = FLAG_ON;
		bop_indx = 0;
		bop_iindx[0] = 0;
		bop_preamble_expected[0] = FLAG_ON;
		if  (strlen(hdr_workbuf) > IP_HDR_MASTERLEN)
		    prc_IF_fragment(ovly_file, fname);
		else
		{
		    if  (ovly_pgnum[ovly_indx] == 0)
			pgnum = 1;
		    else
			pgnum = ovly_pgnum[ovly_indx];
		    prc_IF_master(ovly_file, fname, pgnum);
		}
		bop_flag = FLAG_OFF;
	    }
	    (void)fclose(ovly_file);
	}

}


/****************************************************************
 *
 *	proc_sequence(type_byte, length, file, filename):
 *	proc_sequence:	process Interpress tokens of type
 *				<sequence>
 *
 ****************************************************************/

proc_sequence(type_byte, length, file, filename)

	FILE	*file;
	long	length;
	int	type_byte;
	char	*filename;
{

	if  ((type_byte & 0x1f) == sequenceInsertFile)
	    proc_SIF_token(type_byte, length, file, filename);
	else
	{
	    put_seq_type_len(type_byte, length);
	    iocopyn(length, file, filename);
	}
}


/****************************************************************
 *
 *	prc_SIF_file(sfile, sfilename):
 *	prc_SIF_file:		process sequenceInsertFile
 *
 ****************************************************************/

prc_SIF_file(sfile, sfilename)

	FILE	*sfile;
	char	*sfilename;
{

/*
 *  if the IP_HDR indicates the SIF is a fragment, process as such;
 *  else, for a regular SIF, just process the first page of a master
 */
	if  (strlen(hdr_workbuf) > IP_HDR_MASTERLEN)
	    prc_IF_fragment(sfile, sfilename);
	else
	    prc_IF_master(sfile, sfilename, 1);
}


/****************************************************************
 *
 *	proc_SIF_token(type, length, file, filename):
 *	proc_SIF_token:	process sequenceInsertFile token
 *
 ****************************************************************/

proc_SIF_token(type_byte, length, file, filename)

	FILE	*file;
	long	length;
	int	type_byte;
	char	*filename;
{
	int	i, retcd;
	char	msg[128], buf[MX_HDR_BUFLEN], open_buf[MX_HDR_BUFLEN];
	static  char *preserve_msg =
		     "                  --(Preserving the SIF reference)\n",
		     *remove_msg =
		     "                  --(Removing the SIF reference)\n";


	++num_sifs;
	++tnum_sifs;
	test_newline();
	put_offset_msg_to_prop((in_offset-1), "--SIF:  ");

	if  ((length == 0) || (length > MX_HDR_BUFLEN))
	{
	    (void)sprintf(msg,
		"--Warning:  sequenceInsertFile token encountered with\n");
	    pip_error(msg);
	    (void)sprintf(msg, "            descriptor length = %d\n",
				length);
	    pip_error(msg);
	    (void)sprintf(msg, "(ERROR:  Descriptor length = %d)\n", length);
	    pip_prop(msg);
	    if  (proc_flag[REMOVE_SIFS] == FLAG_ON)
	    {
		pip_error(remove_msg);
		ioreadn(length, file, filename);
	    }
	    else
	    {
	        pip_error(preserve_msg);
/*
 *  preserve the SIF reference
 */
	        copy_preserve_SIF(type_byte, length, file, filename);
	    }
	}
	else
	{
	    iogetn(length, file, filename, buf);
	    (void)strcpy(open_buf, buf);

	    (void)sprintf(msg, "          --SIF:  %s  (nesting depth=%d)\n",
		    buf, sif_indx);
	    pip_error(msg);
	    (void)sprintf(msg, "%s\n", buf);
	    pip_prop(msg);
/*
 *  Check for alias
 */
	    for (i=0; i < num_aliases; i++)
	    {
		if  (strcmp(buf, alias_ref[i]) == 0)
		{
		    (void)strcpy(open_buf, alias_act[i]);
		    (void)sprintf(msg, "                  --Alias: %s=%s\n",
				  buf, open_buf);
		    pip_error(msg);
		    indent((off_indent + cur_indent + 8), propfile);
		    (void)sprintf(msg,"--Alias: %s=%s\n", buf, open_buf);
		    pip_prop(msg);
		    break;
		}
	    }

	    if  (proc_flag[SATISFY_SIFS] == FLAG_OFF)
	    {
		if  (proc_flag[REMOVE_SIFS] == FLAG_ON)
		    pip_error(remove_msg);
		else
		{
		    pip_error(
	"                  -s switch (Satisfy SIFS option) not specified\n");
		    pip_error(preserve_msg);
		    put_preserve_SIF(type_byte, length, buf);
		}
	    }
	    else
	    if  (sif_indx >= MX_SIF_DEPTH)
	    {
		(void)sprintf(msg,
	       "*** warning:  maximum SIF nesting depth allowed in IPFE=%d\n",
			MX_SIF_DEPTH);
		pip_error(msg);
		if  (proc_flag[REMOVE_SIFS] == FLAG_ON)
		    pip_error(remove_msg);
		else
		{
		    pip_error(preserve_msg);
/*
 *  preserve the SIF reference
 */
		    put_preserve_SIF(type_byte, length, buf);
		}
	    }
	    else
	    {
		sif_file[sif_indx] = fopen(open_buf, "r");
		if  (sif_file[sif_indx] == NULL)
		{
		    pip_error("                  ");
		    (void)sprintf(msg, "--Error opening SIF file: %s\n",
				  open_buf);
		    pip_error(msg);
		    indent((off_indent + cur_indent + 8), propfile);
		    pip_prop(msg);
		    if  (proc_flag[REMOVE_SIFS] == FLAG_ON)
			pip_error(remove_msg);
		    else
		    {
			pip_error(preserve_msg);
/*
 *  preserve the SIF reference
 */
			put_preserve_SIF(type_byte, length, buf);
		    }
		}
		else
		{
/*
 *  check the validity of the SIF hdr
 */
		    retcd = get_ip_hdr(sif_file[sif_indx], open_buf);
		    if  (retcd == ERROR)
		    {
			if  (proc_flag[REMOVE_SIFS] == FLAG_ON)
			    pip_error(remove_msg);
			else
			{
			    pip_error(preserve_msg);
/*
 *  preserve the SIF reference
 */
			    put_preserve_SIF(type_byte, length, buf);
			}
		    }
		    else
		    {
			(void)strcpy(sif_name[sif_indx], open_buf);
			sifname = sif_name[sif_indx];
			siffile = sif_file[sif_indx];

			pip_error(
		    "                  -- Processing SIF content --\n");

			++sif_indx;
			prc_SIF_file(siffile, sifname);
			--sif_indx;
		    }
		    (void)fclose(sif_file[sif_indx]);
		}
	    }
	}

}


/****************************************************************
 *
 *	put_Integer(val):
 *	put_Integer:		if  ((val > INTEGER_MAX) ||
 *				     (val < INTEGER_MIN))
 *				    put val out as seqInteger
 *				else
 *				    put val out as Short Number
 *				    token (biased by 4000 (0xfa0))
 *
 ****************************************************************/

put_Integer(val)

	long	val;
{
	int	len;


	if  ((val < INTEGER_MIN) || (val > INTEGER_MAX))
	{
	    len = count_int_bytes(val);
	    put_seq_type_len(sequenceInteger, (long)len);
	    put_intn(val, len);
	}
	else
	{
	    val = val + INTEGER_ZERO;	/* add bias */
	    ip_putc((char)((val >> 8) & 0x7f));
	    ip_putc((char)val);
	}

}


/****************************************************************
 *
 *	put_intn(val, len):
 *	put_intn:		put specified val out as len bytes
 *
 ****************************************************************/

put_intn(val, len)

	long	val;
	int	len;		/* measured in bytes */
{
	int	i;


	if  (len > 0)
	{
	    --len;
	    for (i=len*8; i >= 0; i=i-8)
		ip_putc((char)(val >> i));
	}

}


/****************************************************************
 *
 *	put_offset_msg_to_prop(off, msg):
 *	put_offset_msg_to_prop:  put specified in_offset (formatted)
 *				   + <:  > + <cur_indent> + msg
 *				   to propfile
 *
 ****************************************************************/

put_offset_msg_to_prop(off, msg)

	long	off;
	char	*msg;
{

	if  (cur_col == 0)
	{
	    if  (proc_flag[DEBUG] == FLAG_ON)
		put_offp_to_prop(off);
	    indent(cur_indent, propfile);
	}
	pip_prop(msg);

}


/****************************************************************
 *
 *	put_off_to_prop(off, format_flag):
 *	put_off_to_prop:	put specified in_offset (formatted)
 *				to propfile
 *
 ****************************************************************/

put_off_to_prop(off, format_flag)

	long	off;
	int	format_flag;
{
	int	i, procflag;
	char	digit;


	if  (propfile != NULL)
	{
	    procflag = FLAG_OFF;
	    for (i=20; i >= 0; i=i-4)
	    {
		digit = (off >> i) & 0x0f;
		if  ((digit == 0) && (procflag == FLAG_OFF) && (i > 4))
		{
		    if  (format_flag == FORMAT)
			mputc_prop(SPC);
		}
		else
		{
		    if  (digit > 9)
			digit = digit + 7;
		    mputc_prop(digit + 0x30);
		    procflag = FLAG_ON;
		}
		if  ((i == 16) &&
		     ((format_flag == FORMAT) || (procflag == FLAG_ON)))
		    mputc_prop(SPC);
	    }
	}

}


/****************************************************************
 *
 *	put_offp_to_prop(off):
 *	put_offp_to_prop:	put specified in_offset (formatted
 *				plus "+  " or ":  " to propfile
 *
 ****************************************************************/

put_offp_to_prop(off)

	long	off;
{
	put_off_to_prop(off, FORMAT);
	if  (off > 0xffffff)
	    pip_prop("+  ");
	else
	    pip_prop(":  ");

}


/****************************************************************
 *
 *	put_op(op):
 *	put_op:		put specified op to output
 *
 ****************************************************************/

put_op(op)

	int	op;
{
	if  (op <= SHORT_OP_LIMIT)
	    ip_putc(op | SHORT_OP);
	else
	{
	    ip_putc((op >> 8) | LONG_OP);
	    ip_putc(op);
	}
}


/****************************************************************
 *
 *	put_opname_to_prop(op):
 *	put_opname_to_prop:	put specified op_name to propfile
 *
 ****************************************************************/

put_opname_to_prop(op)

	int	op;
{
	char	msg[128];


	if  (op != OP_endBody)
	    test_begBody();
	begBody_flag = FLAG_OFF;	/* reset because we're ALWAYS putting
					   SOMETHING to propfile here */

	switch
	    (op)
	{
	    case (OP_beginBlock):
		test_newline();
		(void)sprintf(msg, "%s\n", op_name(op));
		put_offset_msg_to_prop((in_offset-2), msg);
		cur_indent = cur_indent + INDENT_INCR;
		break;
	    case (OP_endBlock):
		cur_indent = cur_indent - INDENT_INCR;
		test_newline();
		put_offset_msg_to_prop((in_offset-2), op_name(op));
		if  (proc_flag[DEBUG] == FLAG_ON)
		{
		    indent((INDENT_INCR - 1), propfile);
		    mputc_prop('(');
		    put_off_to_prop((in_offset-1), DONT_FORMAT);
		    mputc_prop(')');
		}
		mputc_prop('\n');
		break;
	    case (OP_beginBody):
		put_offset_msg_to_prop((in_offset-2), op_name(op));
		indent((INDENT_INCR - 1), propfile);
		if  ((proc_flag[PROPERTIES] == FLAG_ON) && (prop_level > 0))
		{
		    if  ((preamble_expected == FLAG_OFF) &&
			 (endpreamble_expected == FLAG_OFF) &&
			 (bop_flag == FLAG_OFF))
		    {		/* we have a righteous pageBody */
			(void)sprintf(msg, "Pg%d", (tnum_pages + 1));
			pip_prop(msg);
			indent((INDENT_INCR - 1), propfile);
		    }
		}
		cur_indent = cur_indent + INDENT_INCR;
		break;
	    case (OP_endBody):
		cur_indent = cur_indent - INDENT_INCR;
		if  (cur_col == 0)
		    indent((cur_indent + off_indent), propfile);
		pip_prop(op_name(op));
		if  ((proc_flag[PROPERTIES] == FLAG_ON) && (prop_level > 0))
		{
		    if  ((endpreamble_expected == FLAG_OFF) &&
			 (bop_flag == FLAG_OFF))
		    {		/* we have a righteous endBody */
			indent((INDENT_INCR - 1), propfile);
			(void)sprintf(msg, "endPg%d", tnum_pages);
			pip_prop(msg);
		    }
		}
		if  (proc_flag[DEBUG] == FLAG_ON)
		{
		    indent((INDENT_INCR - 1), propfile);
		    mputc_prop('(');
		    put_off_to_prop((in_offset-1), DONT_FORMAT);
		    mputc_prop(')');
		}
		if  (debug_flag == FLAG_ON)
		{
		    indent((INDENT_INCR - 1), propfile);
		    (void)sprintf(msg,"[%d %d %d %d]",
				    bop_indx,
				    bop_iindx[bop_indx],
				    bop_iindx[0],
				    endpreamble_expected);
		    pip_prop(msg);
		}
		mputc_prop('\n');
		break;
	    case (OP_makesimpleco):
	    case (OP_dosavesimplebody):
	    case (OP_if):
	    case (OP_ifelse):
	    case (OP_ifcopy):
		put_offset_msg_to_prop((in_offset-2), op_name(op));
		mputc_prop('\n');
		break;
	    case (OP_correct):
/*
 *  don't do all those nasty CORRECT's unless the user forces us to
 */
		if  (prop_level > 1)
		{
		    put_offset_msg_to_prop((in_offset-2), op_name(op));
		    mputc_prop('\n');
		}
		break;
	    default:
		break;
	}
}


/****************************************************************
 *
 *	put_seqRational(num, den):
 *	put_seqRational:	put specified numerator & denominator
 *				out as a sequenceRational
 *
 ****************************************************************/

put_seqRational(num, den)

	long	num, den;
{
	int	len, num_len, den_len;


	num_len = count_int_bytes(num);
	den_len = count_int_bytes(den);
	if  (num_len > den_len)
	    len = num_len;
	else
	    len = den_len;
	put_seq_type_len(sequenceRational, (long)(len * 2));
	put_intn(num, len);
	put_intn(den, len);

}


/****************************************************************
 *
 *	put_seq_type_len(type_byte, length):
 *	put_seq_type_len:	put specified type_byte & length
 *				to output
 *
 ****************************************************************/

put_seq_type_len(type_byte, length)

	long	length;
	int	type_byte;
{
	if  (length > SHORT_SEQUENCE_LIMIT)
	{
	    ip_putc((char)(LONG_SEQUENCE | (type_byte & 0x1f)));
	    ip_putc((char)(length >> 16));
	    ip_putc((char)(length >> 8));
	    ip_putc((char)length);
	}
	else
	{
	    ip_putc((char)(SHORT_SEQUENCE | (type_byte & 0x1f)));
	    ip_putc((char)length);
	}

}


/****************************************************************
 *
 *	test_begBody:		if beBody flag is set,
 *				  put a newline to propfile
 *
 ****************************************************************/

test_begBody()
{
	if  (begBody_flag == FLAG_ON)
	{
	    mputc_prop('\n');
	    begBody_flag = FLAG_OFF;
	}
}



/****************************************************************
 *
 *	test_newline:		if cur_col != 0,
 *				  put a newline to propfile
 *
 ****************************************************************/

test_newline()
{
	if  (cur_col != 0)
	    mputc_prop('\n');

}


/****************************************************************
 *
 *	usage:		dsply the cmd_line interface
 *
 ****************************************************************/

Usage()
{
  fprintf(STDERR,
  "Usage:\n");
  fprintf(STDERR,
  "        ipfe [ options ] file [pagerange ... [file [pagerange]...]\n");
  fprintf(STDERR,
  "Options:\n");
  fprintf(STDERR,
  "         [-l logfile] [-dDiLqrRs] [-a alias:actual [-a alias:actual]\n");
  fprintf(STDERR,
  "         ...] [-b offset:unit] [-c count:unit] [-o outfile]\n");
  fprintf(STDERR,
  "         [-p level:propfile] [-S factor] [-X offset:unit]\n");
  fprintf(STDERR,
  "         [-Y offset:unit]\n\n");

  fprintf(STDERR,
  "          -a alias:actual (alias). If the -s option is specified,\n");
  fprintf(STDERR,
  "                         replace any SIF matching the string \"alias\"\n");
  fprintf(STDERR,
  "                         with the string \"actual\" before attempting\n");
  fprintf(STDERR,
  "                         to satisfy the SIF. If \"actual\" cannot be\n");
  fprintf(STDERR,
  "                         opened or there is some error, the SIF\n");
  fprintf(STDERR,
  "                         \"alias\" is preserved, unless the -r option\n");
  fprintf(STDERR,
  "                         is also specified.\n");
  fprintf(STDERR,
 "          -b offset:unit (binding offset). Shift the image offset units\n");
  fprintf(STDERR,
  "                         in the x-direction, where unit may be: none\n");
  fprintf(STDERR,
  "                         (default centimeters), c (centimeters), i\n");
  fprintf(STDERR,
  "                         (inches), p (points), P (Picas). If the -L\n");
  fprintf(STDERR,
  "                         (Landscape) switch is set, the image shift\n");
  fprintf(STDERR,
  "                         is in the y-direction.\n");
  fprintf(STDERR,
  "          -c count:unit  (chapterize) every count units, where unit\n");
  fprintf(STDERR,
  "                         be: p (pages), k (kilobytes), m (megabytes).\n");
  fprintf(STDERR,
  "          -d             (duplex). For resolving binding offset.\n");
  fprintf(STDERR,
  "          -D             (debug). If the -p option is specified, also\n");
  fprintf(STDERR,
  "                         write to the properties file the offsets of\n");
  fprintf(STDERR,
  "                         each skeleton-level token encountered.\n");
  fprintf(STDERR,
  "          -i             (insert SIF for overlay). Insert (create) a\n");
  fprintf(STDERR,
  "                         SIF for any unresolvable overlays.\n");
  fprintf(STDERR,
  "          -l logfile     (log). Keep a running log.\n");
  fprintf(STDERR,
  "          -L             (Landscape). Rotate every page 90 degrees\n");
  fprintf(STDERR,
  "                         counterclockwise and preserve the upper left\n");
  fprintf(STDERR,
  "                         corner. Intended for printing text in\n");
  fprintf(STDERR,
  "                         landscape orientation.\n");
  fprintf(STDERR,
  "          -o outfile     (output). Where the output goes. If there is\n");
  fprintf(STDERR,
  "                         no -o and there is a -p, only the properties\n");
  fprintf(STDERR,
  "                         are written, else if there is no -o,\n");
  fprintf(STDERR,
  "                         \"infile.ip\" is used if it doesn't already\n");
  fprintf(STDERR,
  "                         exist, else \"infileN.ip\" is used, where N\n");
  fprintf(STDERR,
 "                         is the lowest integer such that \"infileN.ip\"\n");
  fprintf(STDERR,
  "                         does not already exist.\n");
  fprintf(STDERR,
  "          -p level:propfile\n");
  fprintf(STDERR,
  "                         (properties). Where the properties are\n");
  fprintf(STDERR,
  "                         written. Increasing levels provide increasing\n");
  fprintf(STDERR,
  "                         information details.\n");
  fprintf(STDERR,
  "          -q             (quiet). Don't write info and error msgs (to\n");
  fprintf(STDERR,
  "                         STDERR).\n");
  fprintf(STDERR,
  "          -r             (remove SIFS). If -s is also specified, it\n");
  fprintf(STDERR,
  "                         takes precedence and a SIF reference is\n");
  fprintf(STDERR,
  "                         removed only if it is unresolved or there is\n");
  fprintf(STDERR,
  "                         some other error.\n");
  fprintf(STDERR,
  "          -R             (Rotate). Rotate every page 90 degrees\n");
  fprintf(STDERR,
  "                         clockwise and preserve the center point.\n");
  fprintf(STDERR,
  "                         Intended for rotating an image created for a\n");
  fprintf(STDERR,
  "                         \"landscape printer\" to print in portrait\n");
  fprintf(STDERR,
  "                         orientation.\n");
  fprintf(STDERR,
  "          -s             (satisfy SIFS). Replace any SIF reference\n");
  fprintf(STDERR,
  "                         with the tokens found in the referenced file.\n");
  fprintf(STDERR,
  "                         If the referenced file cannot be opened, the\n");
  fprintf(STDERR,
  "                         SIF reference is preserved unless -r is also\n");
  fprintf(STDERR,
  "                         specified.\n");
  fprintf(STDERR,
  "          -S factor      (scale). Scale the image by factor.\n");
  fprintf(STDERR,
  "          -X offset:unit (X-imageShift). Shift the image offset units\n");
  fprintf(STDERR,
  "                         in the x-direction, where unit may be: none\n");
  fprintf(STDERR,
  "                         (default centimeters), c (centimeters), i\n");
  fprintf(STDERR,
  "                         (inches), p (points), P (Picas). The shift\n");
  fprintf(STDERR,
  "                         is independent of the -L and -R switches\n");
  fprintf(STDERR,
  "                         (i.e., the x-direction is the same as the\n");
  fprintf(STDERR,
  "                         unrotated image).\n");
  fprintf(STDERR,
  "          -Y offset:unit (Y-imageShift). Shift the image offset units\n");
  fprintf(STDERR,
  "                         in the y-direction, where unit may be: none\n");
  fprintf(STDERR,
  "                         (default centimeters), c (centimeters), i\n");
  fprintf(STDERR,
  "                         (inches), p (points), P (Picas). The shift\n");
  fprintf(STDERR,
  "                         is independent of the -L and -R switches\n");
  fprintf(STDERR,
  "                         (i.e., the y-direction is the same as the\n");
  fprintf(STDERR,
  "                         unrotated image).\n");
  fprintf(STDERR,
  "          infile [pagerange]\n");
  fprintf(STDERR,
  "                         See the manual page for syntax details.\n");
  fprintf(STDERR,
  "            Example:\n");
  fprintf(STDERR,
  "              [1,4-6,9[pic1],10-11,12[pic2:2(2P,4P)][pic3(-4P,-2P)],15-]\n\n");

  exit(1);
}

/***  end of file = IPFE.C  *************************************/
