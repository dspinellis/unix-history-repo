/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)asmain.c 4.9 %G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#include "as.h"
#include "assyms.h"
#include "asscan.h"
#include "asexpr.h"

#ifdef UNIX
#define	unix_lang_name "VAX/UNIX Assembler V%G% 4.9"
#endif

#ifdef VMS
#define vms_lang_name "VAX/VMS C Assembler V1.00"
#endif VMS

/*
 *	variables to manage reading the assembly source files
 */
char	*dotsname;	/*the current file name; managed by the parser*/
int	lineno;		/*current line number; managed by the parser*/
char	**innames;	/*names of the files being assembled*/
int	ninfiles;	/*how many interesting files there are*/
/*
 *	Flags settable from the argv process argument list
 */
int	silent = 0;	/*don't complain about any errors*/
int	savelabels = 0;	/*write the labels to the a.out file*/
int 	d124 = 4;	/*default allocate 4 bytes for unknown pointers*/
int	anyerrs = 0;	/*no errors yet*/
int	anywarnings=0;	/*no warnings yet*/
int	orgwarn = 0;	/*Bad origins*/
int	passno = 1;	/* current pass*/
int	jxxxJUMP = 0;	/* in jxxxes that branch too far, use jmp instead of brw */
int	readonlydata = 0;	/* initialzed data -> text space */

int	nGHnumbers = 0;		/* GH numbers used */
int	nGHopcodes = 0;		/* GH opcodes used */
int	nnewopcodes = 0;	/* new opcodes used */

#ifdef DEBUG
int 	debug = 0;
int	toktrace = 0;
#endif

int	useVM =		/*put the temp file in virtual memory*/
#ifdef VMS
	1;		/*VMS has virtual memory (duh)*/
#endif VMS
#ifdef UNIX
 	0;
#endif

char	*endcore;	/*where to get more symbol space*/

/*
 *	Managers of the a.out file.
 */
struct	exec	hdr;
#define	MAGIC	0407
u_long	tsize;		/* total text size */
u_long	dsize;		/* total data size */
u_long	datbase;	/* base of the data segment */
u_long	trsize;		/* total text relocation size */
u_long	drsize;		/* total data relocation size */

/*
 *	Information about the current segment is accumulated in
 *	usedot; the most important information stored is the
 *	accumulated size of each of the text and data segments
 *
 *	dotp points to the correct usedot expression for the current segment
 */
struct	exp	usedot[NLOC+NLOC];	/* info about all segments */
struct	exp	*dotp;			/* data/text location pointer */
/*
 *	The inter pass temporary file is opened and closed by stdio, but
 *	is written to using direct read/write, as the temporary file
 *	is composed of buffers exactly BUFSIZ long.
 */
FILE	*tmpfil;			/* interpass communication file */
/*
 *	a.out is created during the second pass.
 *	It is opened by stdio, but is filled with the parallel
 *	block I/O library
 */
char	*outfile = "a.out";
FILE	*a_out_file;			
off_t	a_out_off;			/* cumulative offsets for segments */
/*
 *	The logical files containing the assembled data for each of
 *	the text and data segments are
 *	managed by the parallel block I/O library.
 *	a.out is logically opened in many places at once to
 *	receive the assembled data from the various segments as
 *	it all trickles in, but is physically opened only once
 *	to minimize file overhead.
 */
BFILE	*usefile[NLOC+NLOC];		/* text/data files */
BFILE	*txtfil;			/* current text/data file */
/*
 *	Relocation information is accumulated seperately for each
 *	segment.  This is required by the old loader (from BTL),
 *	but not by the new loader (Bill Joy).  
 *
 *	However, the size of the relocation information can not be computed
 *	during or after the 1st pass because the ''absoluteness' of values
 *	is unknown until all locally declared symbols have been seen.
 *	Thus, the size of the relocation information is only
 *	known after the second pass is finished.
 *	This obviates the use of the block I/O
 *	library, which requires knowing the exact offsets in a.out.
 *
 *	So, we save the relocation information internally (we don't
 *	go to internal files to minimize overhead).
 *
 *	Empirically, we studied 259 files composing the system,
 *	two compilers and a compiler generator: (all of which have
 *	fairly large source files)
 *	
 *	Number of files = 259
 *		Number of non zero text reloc files: 233
 *		Number of non zero data reloc files: 53
 *	Average text relocation = 889
 *	Average data relocation = 346
 *	Number of files > BUFSIZ text relocation = 71
 *	Number of files > BUFSIZ data relocation = 6
 *	
 *	For compiled C code, there is usually one text segment and two
 *	data segments; we see that allocating our own buffers and
 *	doing our internal handling of relocation information will,
 *	on the average, not use more memory than taken up by the buffers
 *	allocated for doing file I/O in parallel to a number of file.
 *	
 *	If we are assembling with the -V option, we
 *	use the left over token buffers from the 2nd pass,
 *	otherwise, we create our own.
 *
 *	When the 2nd pass is complete, closeoutrel flushes the token
 *	buffers out to a BFILE.
 *
 *	The internals to relbufdesc are known only in assyms.c
 *
 *	outrel constructs the relocation information.
 *	closeoutrel flushes the relocation information to relfil.
 */
struct	relbufdesc	*rusefile[NLOC+NLOC];	
struct	relbufdesc 	*relfil;	/* un concatnated relocation info */
BFILE	*relocfile;			/* concatnated relocation info */
/*
 *	Once the relocation information has been written,
 *	we can write out the symbol table using the Block I/O
 *	mechanisms, as we once again know the offsets into
 *	the a.out file.
 *
 *	We use relfil to output the symbol table information.
 */

char	*tmpdirprefix =
#ifdef UNIX
			"/tmp/";
#else VMS
			"/usr/tmp/";
#endif

#define		TMP_SUFFIX	"asXXXXXX"
char		tmpn1[TNAMESIZE];

int delexit();

main(argc, argv)
	int	argc;
	char 	**argv;
{
	char	*sbrk();

	tmpn1[0] = 0;
	endcore = sbrk(0);

	argprocess(argc, argv);		/* process argument lists */
	if (anyerrs) exit(1);

	initialize();
	zeroorigins();			/* set origins to zero */
	zerolocals();			/* fix local label counters */

	i_pass1();			/* open temp files, etc */
	pass1();			/* first pass through .s files */
	testlocals();			/* check for undefined locals */
	if (anyerrs) delexit();

	pass1_5();			/* resolve jxxx */
	if (anyerrs) delexit();

	open_a_out();			/* open a.out */
	roundsegments();		/* round segments to FW */
	build_hdr();			/* build initial header, and output */
	
	i_pass2();			/* reopen temporary file, etc */
	pass2();			/* second pass through the virtual .s */
	if (anyerrs) delexit();

	fillsegments();			/* fill segments with 0 to FW */
	reloc_syms();			/* dump relocation and symbol table */

	delete();			/* remove tmp file */
	bflush();			/* close off block I/O view of a.out */
	fix_a_out();			/* add in text and data reloc counts */

	if (anyerrs == 0 && orgwarn)
		yyerror("Caution: absolute origins.\n");

	if (nGHnumbers)
		yywarning("Caution: G or H format floating point numbers");
	if (nGHopcodes)
		yywarning("Caution: G or H format floating point operators");
	if (nnewopcodes)
		yywarning("Caution: New Opcodes");
	if (nGHnumbers || nGHopcodes || nnewopcodes)
		yywarning("These are not defined for all implementations of the VAX architecture.\n");

	exit(anyerrs != 0);
}	/*end of UNIX main*/

argprocess(argc, argv)
	int	argc;
	char	*argv[];
{
	register	char	*cp;

	ninfiles = 0;
	silent = 0;
#ifdef DEBUG
	debug = 0;
#endif
	innames = (char **)ClearCalloc(argc+1, sizeof (innames[0]));
	dotsname = "<argv error>";
	while (argc > 1) {
		if (argv[1][0] != '-')
			innames[ninfiles++] = argv[1];
		else {
			cp = argv[1] + 1;
			/*
			 *	We can throw away single minus signs, so
			 *	that make scripts for the PDP 11 assembler work
			 *	on this assembler too
			 */
			while (*cp){	
				switch(*cp++){
				 default:
					yyerror("Unknown flag: %c", *--cp);
					cp++;
					break;
				 case 'd':
					d124 = *cp++ - '0';
					if ( (d124 != 1) && (d124 != 2) && 
					     (d124 != 4)){
						yyerror("-d[124] only");
						exit(1);
					}
					break;
				 case 'o':
					if (argc < 3){
						yyerror("-o what???");
						exit(1);
					}
					outfile = argv[2];
				   bumpone:
					argc -= 2;
					argv += 2;
					goto nextarg;

				 case 't':
					if (argc < 3){
						yyerror("-t what???");
						exit(1);
					}
					tmpdirprefix = argv[2];
					goto bumpone;
				 
				 case 'V':
					useVM = 1;
					break;
				 case 'W':
					silent = 1;
					break;
				 case 'L':
					savelabels = 1;
					break;
				 case 'J':
					jxxxJUMP = 1;
					break;
#ifdef DEBUG
				 case 'D':
					debug = 1;
					break;
				 case 'T':
					toktrace = 1;
					break;
#endif
				 case 'R':
					readonlydata = 1;
					break;
				}	/*end of the switch*/
			}	/*end of pulling out all arguments*/
		}	/*end of a flag argument*/
		--argc; ++argv;
	   nextarg:;
	}
	/* innames[ninfiles] = 0; */
}

initialize()
{
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, delexit);
	/*
	 *	Install symbols in the table
	 */
	symtabinit();
	syminstall();
	/*
	 *	Build the expression parser accelerator token sets
	 */
	buildtokensets();		
}

zeroorigins()
{
	register	int	locindex;
	/*
	 *	Mark usedot: the first NLOC slots are for named text segments,
	 *	the next for named data segments.
	 */
	for (locindex = 0; locindex < NLOC; locindex++){
		usedot[locindex].e_xtype = XTEXT;
		usedot[NLOC + locindex].e_xtype = XDATA;
		usedot[locindex].e_xvalue = 0;
		usedot[NLOC + locindex].e_xvalue = 0;
	}
}

zerolocals()
{
	register	int	i;

	for (i = 0; i <= 9; i++) {
		lgensym[i] = 1;
		genref[i] = 0;
	}
}

i_pass1()
{
	if (useVM == 0){
		strcat(tmpn1, tmpdirprefix);
		if (tmpdirprefix[strlen(tmpdirprefix)-1] != '/')
			strcat(tmpn1, "/");
		(void)strcat(tmpn1, TMP_SUFFIX);
		(void)mktemp(tmpn1);
		tmpfil = fopen(tmpn1, "w");
		if (tmpfil==NULL) {
		  yyerror("Bad pass 1 temporary file for writing %s", tmpn1);
		  delexit();
		}
	}

	inittmpfile();
	initijxxx();
}

pass1()
{
	register	int	i;

	passno = 1;
	dotp = &usedot[0];
	txtfil = (BFILE *)0;
	relfil = (struct relbufdesc *)0;

	if (ninfiles == 0){		/*take the input from stdin directly*/
		lineno = 1;
		dotsname = "<stdin>";

		yyparse();
	} else {		/*we have the names tanked*/
		for (i = 0; i < ninfiles; i++){
			new_dot_s(innames[i]);
			if (freopen(innames[i], "r", stdin) == NULL) {
				yyerror( "Can't open source file %s\n",
					innames[i]);
				exit(2);
			}
			/* stdio is NOT used to read the input characters */
			/* we use read directly, into our own buffers */
			yyparse();
		}
	}

	closetmpfile();		/*kick out the last buffered intermediate text*/
}

testlocals()
{
	register	int	i;
	for (i = 0; i <= 9; i++) {
		if (genref[i])
			yyerror("Reference to undefined local label %df", i);
		lgensym[i] = 1;
		genref[i] = 0;
	}
}

pass1_5()
{
	sortsymtab();
#ifdef DEBUG
	if (debug) dumpsymtab();
#endif
	jxxxfix();
#ifdef DEBUG
	if (debug) dumpsymtab();
#endif
}

open_a_out()
{
	/*
	 *	Open up the a.out file now, and get set to build
	 *	up offsets into it for all of the various text,data
	 *	text relocation and data relocation segments.
	 */
	a_out_file = fopen(outfile, "w");
	if (a_out_file == NULL) {
		yyerror("Cannot create %s", outfile);
		delexit();
	}
	biofd = a_out_file->_file;
	a_out_off = 0;
}

roundsegments()
{
	register	int	locindex;
	register	long	v;
	/*
	 *	round and assign text segment origins
	 *	the exec header always goes in usefile[0]
	 */
	tsize = 0;
	for (locindex=0; locindex<NLOC; locindex++) {
		v = round(usedot[locindex].e_xvalue, FW);
		usedot[locindex].e_xvalue = tsize;
		if ((locindex == 0) || (v != 0) ){
			usefile[locindex] = (BFILE *)Calloc(1, sizeof(BFILE));
			bopen(usefile[locindex], a_out_off);
			if (locindex == 0)
				a_out_off = sizeof (struct exec);
		} else {
			usefile[locindex] = (BFILE *)-1;
		}
		tsize += v;
		a_out_off += v;
	}
	/*
	 *		Round and assign data segment origins.
	 */
	datbase = round(tsize, FW);
	for (locindex=0; locindex<NLOC; locindex++) {
		v = round(usedot[NLOC+locindex].e_xvalue, FW);
		usedot[NLOC+locindex].e_xvalue = datbase + dsize;
		if (v != 0){
			usefile[NLOC + locindex] = (BFILE *)Calloc(1,sizeof(BFILE));
			bopen(usefile[NLOC + locindex], a_out_off);
		} else {
			usefile[NLOC + locindex] = (BFILE *)-1;
		}
		dsize += v;
		a_out_off += v;
	}
	/*
	 *	Assign final values to symbols
	 */
	hdr.a_bss = dsize;
	freezesymtab();		/* this touches hdr.a_bss */
	stabfix();
	/*
	 *	Set up the relocation information "files" to
	 *	be zero; outrel takes care of the rest
	 */
	for (locindex = 0; locindex < NLOC + NLOC; locindex++){
		rusefile[locindex] = (struct relbufdesc *)0;
	}
}

build_hdr()
{
	/*
	 *	Except for the text and data relocation sizes,
	 *	calculate the final values for the header
	 *	
	 *	Write out the initial copy; we to come 
	 *	back later and patch up a_trsize and a_drsize,
	 *	and overwrite this first version of the header.
	 */
	hdr.a_magic = MAGIC;
	hdr.a_text = tsize;
	hdr.a_data = dsize;
	hdr.a_bss -= dsize;
	hdr.a_syms = sizesymtab();	/* Does not include string pool length */
	hdr.a_entry = 0;
	hdr.a_trsize = 0;
	hdr.a_drsize = 0;

	bwrite((char *)&hdr, sizeof(hdr), usefile[0]);
}

i_pass2()
{
	if (useVM == 0) {
		fclose(tmpfil);
		tmpfil = fopen(tmpn1, "r");
		if (tmpfil==NULL) {
		   yyerror("Bad pass 2 temporary file for reading %s", tmpn1);
		   delexit();
		}
	}
}

pass2()
{
#ifdef DEBUG
	if (debug)
		printf("\n\n\n\t\tPASS 2\n\n\n\n");
#endif DEBUG
	passno = 2;
	lineno = 1;
	dotp = &usedot[0];
	txtfil = usefile[0];	/* already opened (always!) */
	relfil = 0;		/* outrel takes care of the rest */
	initoutrel();

	inittmpfile();

	yyparse();

	closetmpfile();
}

fillsegments()
{
	int	locindex;
	/*
	 *	Round text and data segments to FW by appending zeros
	 */
	for (locindex = 0; locindex < NLOC + NLOC; locindex++) {
		if (usefile[locindex]) {
			txtfil = usefile[locindex];
			dotp = &usedot[locindex];
			while (usedot[locindex].e_xvalue & FW)
				outb(0);
		}
	}
}

reloc_syms()
{
	u_long	closerelfil();
	/*
	 *	Move the relocation information to a.out
	 *	a_out_off is the offset so far:
	 *	exec + text segments + data segments
	 */
	relocfile = (BFILE *)Calloc(1,sizeof(BFILE));
	bopen(relocfile, a_out_off);
	a_out_off += closeoutrel(relocfile);

	hdr.a_trsize = trsize;
	hdr.a_drsize = drsize;
	if (readonlydata) {
		hdr.a_text += hdr.a_data;
		hdr.a_data = 0;
		hdr.a_trsize += hdr.a_drsize;
		hdr.a_drsize = 0;
	}
	/*
	 *	Output the symbol table
	 *	and if FLEXNAMES is set, the string pool
	 */
	symwrite(relocfile);
}

fix_a_out()
{
	if (lseek(a_out_file->_file, 0L, 0) < 0L)
		yyerror("Reposition for header rewrite fails");
	if (write(a_out_file->_file, (char *)&hdr, sizeof (struct exec)) < 0)
		yyerror("Rewrite of header fails");
}

delexit()
{
	delete();
	if (passno == 2){
		unlink(outfile);
	}
	exit(1);
}

delete()
{
	if (useVM == 0 || tmpn1[0])
		unlink(tmpn1);
}

sawabort()
{
	char	*fillinbuffer();
	while (fillinbuffer() != (char *)0)
		continue;
	delete();
	exit(1);	/*although the previous pass will also exit non zero*/
}

panic(fmt, a1, a2, a3, a4)
	char	*fmt;
	/*VARARGS 1*/
{
	yyerror("Assembler panic: bad internal data structure.");
	yyerror(fmt, a1, a2, a3, a4);
	delete();
	abort();
}
