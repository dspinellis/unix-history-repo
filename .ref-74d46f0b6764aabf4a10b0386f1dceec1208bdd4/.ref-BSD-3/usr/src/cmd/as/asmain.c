/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include <signal.h>

#include "as.h"
#include "assyms.h"
#include "asexpr.h"
#include "asscan.h"

int	curlen;
/*
 *	variables to manage the assembly input
 */
char	*dotsname;	/*the current file name; managed by the parser*/
int	lineno;		/*current line number; managed by the parser*/
int	silent;		/*don't complain about any errors*/
int	savelabels;	/*write the labels to the a.out file*/

#ifdef DEBUG
int 	debug;
int	toktrace;
#endif

long	datbase;

char	*endcore;		/*where to get more symbol space*/

struct	hdr hdr = {
	0410, 0, 0, 0, 0, 0, 0, 0,
};

#ifndef vax
struct {short hiword; short loword;}; /* stupid fp-11 */
#else
#define writel(p,n,f) fwrite( (long) p, sizeof (long), n, f)
#endif

char		*tmpn1;
char		*tmpn2;
char		*tmpn3;

struct	exp	usedot[NLOC+NLOC];

FILE		*usefile[NLOC+NLOC];
FILE		*rusefile[NLOC+NLOC];
char 		sibuf[TOKBUFLG];		/*buffer used for all input*/
char		sobuf[TOKBUFLG];		/*buffer used for all output*/
						/*except stdout and relfil*/
char		stdoutbuf[BUFSIZ];		/*stdout buffer*/

extern int 	njxxx;		/*number of jumpxxx  instructs*/
extern int 	d124;		/*allocate 1,2 or 4 bytes for unknowns*/

int delexit();

char		*innames[32];	/*names of the files being assembled*/
int		ninfiles;	/*how many interesting files there are*/

main(argc, argv)
	int	argc;
	char 	**argv;
{
	int 		locindex;
	long 		v;
	char 		*outfile = "a.out";
	int		filestep;
	char		*cp;

	setbuf(stdout, stdoutbuf);
	ninfiles = 0;
	silent = 0;
	useVM = 0;
#ifdef DEBUG
	debug = 0;
#endif
	/*
	 *	Give the error processor something to complain about
	 *	if there is an error processing an argument
	 */
	dotsname = "<argv error>";
	while (argc > 1) {
		if (argv[1][0] == '-'){
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
					argc -= 2;
					argv += 2;
					goto nextarg;

				 case 'V':
					useVM = 1;
					break;
#ifdef fooiearg
				 case 'M':
					if (argc < 3){
						yyerror("Mode what?");
						exit(1);
					}
					hdr.magic = 0;
					cp = argv[2];
					while (*cp && ('0' <= *cp) && (*cp <= '7'))
						hdr.magic = hdr.magic<<3 + *cp++ - '0';
					argc -= 2;
					argv += 2;
					goto nextarg;
				 case 'W':	silent = 1;
					break;
#endif

#ifdef DEBUG
				 case 'D':	debug = 1;
					break;
				 case 'T':	toktrace = 1;
					break;
#endif
#ifdef METRIC
				 case 'C':	outcounters = 1;
					break;
#endif
				 case 'L':	savelabels = 1;
					break;
				}	/*end of the switch*/
			}	/*end of pulling out all arguments*/
		}	/*end of a flag argument*/
		else {	/*file name*/
			if (ninfiles > 32){
				yyerror("More than 32 file names");
				exit(3);
			}
			innames[ninfiles++] = argv[1];
		}
		--argc; ++argv;
	   nextarg:;
	}	/*end of looking at all of the arguments*/
				 
	if (anyerrs)
		exit(1);

	endcore = (char *)sbrk(0);

	/*
	 *	Install symbols in the table
	 */
	symtabinit();
	syminstall();
	/*
	 *	mark usedot: first NLOC slots for named text segments,
	 *	the next for named data segments.
	 */
	for (locindex=0; locindex<NLOC; locindex++) {
		usedot[locindex].xtype = XTEXT;
		usedot[locindex+NLOC].xtype = XDATA;
	}

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, delexit);

	tmpn1 = (char *)mktemp("/tmp/asXXXXX");
	tmpfil = fopen(tmpn1, "w");
	if (tmpfil==NULL) {
		yyerror("Bad pass 1 temporary file for writing %s", tmpn1);
		delexit();
	}
	setbuf(tmpfil,sobuf);

	inittmpfile();
	buildtokensets();		/*sets to implement expression lookahead*/

	if (ninfiles == 0){		/*take the input from stdin directly*/
		setbuf(stdin, sibuf);
		lineno = 1;
		dotsname = "<stdin>";

		yyparse();
	} else {		/*we have the names tanked*/
		for (filestep = 0; filestep < ninfiles; filestep++){
			new_dot_s(innames[filestep]);
			if (freopen(innames[filestep], "r", stdin) == NULL) {
				yyerror( "Can't open source file %s\n",
					innames[filestep]);
				exit(2);
			}
			setbuf(stdin,sibuf);

			yyparse();
		}
	}

	closetmpfile();		/*kick out the last buffered intermediate text*/

	if (anyerrs)
		delexit();

	/*
	 *	Pass 1.5
	 */
	sortsymtab();

#ifdef DEBUG
	if (debug)
		dumpsymtab();
#endif

	jxxxfix();

#ifdef DEBUG
	if (debug)
		dumpsymtab();
#endif

#ifdef METRIC
	lgtmpfile = ftell(tmpfil);
#endif

	fclose(tmpfil);
	tmpfil = fopen(tmpn1, "r");
	if (tmpfil==NULL) {
		yyerror("Bad pass 2 temporary file for reading %s", tmpn1);
		delexit();
	}
	setbuf(tmpfil,sibuf);

	/*
	 *	round and assign text segment origins 
	 */
	tsize = 0;
	for (locindex=0; locindex<NLOC; locindex++) {
		v = round(usedot[locindex].xvalue, FW);
		usedot[locindex].xvalue = tsize;
		tsize += v;
	}
	/*
	 *	round and assign data segment origins 
	 */
	datbase = round(tsize, PAGRND);
	for (locindex=0; locindex<NLOC; locindex++) {
		v = round(usedot[NLOC+locindex].xvalue, FW);
		usedot[NLOC+locindex].xvalue = datbase+dsize;
		dsize += v;
	}

	hdr.bsize = dsize;

	/*
	 *	Assign final values to symbols
	 */
	freezesymtab();
	stabfix();

	hdr.bsize -= dsize;

	txtfil = fopen(outfile, "w");
	if (txtfil==NULL) {
		yyerror("Cannot create %s", outfile);
		delexit();
	}
	setbuf(txtfil,sobuf);

	usefile[0] = txtfil;

	tmpn2 = (char *)mktemp("/tmp/aaatXXXXX");
	tmpn3 = (char *)mktemp("/tmp/abatXXXXX");

	relfil = fopen(tmpn3, "w");
	if (relfil==NULL) {
		yyerror("Bad temp file for writing extra text segments %s", tmpn3);
		delexit();
	}
	rusefile[0] = relfil;

	hdr.tsize = tsize;
	hdr.dsize = dsize;
	hdr.ssize = sizesymtab();
	/*
	 *	hdr.trsize, hdr.drsize set by outrel 
	 */

	/* *************** PASS 2 **************** */

	writel(&hdr,8,txtfil);
	tsize = 0;
	dsize = 0;
	lineno = 1;
	dotp = &usedot[0];
#ifdef DEBUG
	if (debug)
		printf("\n\n\n\t\tPASS 2\n\n\n\n");
#endif
	passno = 2;
	inittmpfile();

	yyparse();

	closetmpfile();

	/*
	 *	round csects to FW 
	 */
	for (locindex=0; locindex<NLOC; locindex++) {
		if (usefile[locindex]) {
			txtfil=usefile[locindex];
			dotp= &usedot[locindex];
			while (usedot[locindex].xvalue & FW)
				outb(0);
			if (locindex>0)
				fclose(usefile[locindex]);
			fclose(rusefile[locindex]);
		}
		if (usefile[NLOC+locindex]) {
			txtfil = usefile[NLOC+locindex]; 
			dotp= &usedot[locindex+NLOC];
			relfil = rusefile[NLOC+locindex];
			while (usedot[locindex+NLOC].xvalue & FW)
				outb(0);
			fclose(txtfil);
			fclose(relfil);
		}
	}

	txtfil = usefile[0];
	/*
	 *	append csect text onto text for csect 0 
	 */
	for (locindex=1; locindex<NLOC+NLOC; locindex++) {
		char	buffer[BUFSIZ];
		if (usefile[locindex]) {
			tmpn2[TMPC] = locindex+'a';
			relfil = fopen(tmpn2, "r");
			if (relfil==NULL) {
				yyerror("cannot reopen temp");
				continue;
			}
			while (!feof(relfil))
				fwrite(buffer, 1,
					fread(buffer, 1, BUFSIZ, relfil),
					txtfil);
			fclose(relfil);
		}
	}
	/*
	 *	append relocation info onto text 
	 */
	for (locindex=0; locindex<NLOC+NLOC; locindex++) {
		char	buffer[BUFSIZ];
		if (rusefile[locindex]) {
			tmpn3[TMPC] = locindex+'a';
			relfil = fopen(tmpn3, "r");
			if (relfil==NULL) {
				yyerror("cannot reopen temp");
				continue;
			}
			while (!feof(relfil))
				fwrite(buffer, 1, 
				       fread(buffer, 1, BUFSIZ, relfil),
				       txtfil);
			fclose(relfil);
		}
	}

	symwrite(txtfil);
	/*
	 *	Go back and patch up rsize
	 */
	fseek(txtfil,0L,0);
	writel(&hdr,8,txtfil);

	delete();

	if (anyerrs==0 && orgwarn)
		yyerror("Caution: absolute origins.\n");
#ifdef METRIC
	pcounters();
#endif
	exit(anyerrs!=0);
}	/*end of main*/


delexit()
{
	delete();
#ifdef METRIC
	pcounters();
#endif
	exit(1);
}

sawabort()
{
	char	buffer[BUFSIZ];
#ifdef METRIC
	pcounters();
#endif
	while (!feof(stdin))
		fread(buffer, 1, BUFSIZ, stdin);
	delete();
	exit(1);	/*although the previous pass will also exit non zero*/
}

delete()
{
	register locindex;

	if (tmpn1)
		unlink(tmpn1);
	for (locindex=0; locindex<NLOC+NLOC; locindex++) {
		if (tmpn2) {
			tmpn2[TMPC] = locindex+'a';
			unlink(tmpn2);
		}
		if (tmpn3) {
			tmpn3[TMPC] = locindex+'a';
			unlink(tmpn3);
		}
	}
}
#ifdef METRIC
pcounters()
{
	int	i;
	struct {
		long	p_user;
		long	p_sys;
		long	c_user;
		long	c_sys;
	} tbuffer;

	if (!outcounters) return;
	printf("Assembly of files: ");
	if (innames[0] == 0)
		printf("<Standard Input.>\n");
	else {
		for (i = 0; i<ninfiles; i++)
			printf("%s ", innames[i]);
		printf("\n");
	}
	if (useVM)
		printf("Using ");
	else
		printf("NOT using ");
	printf("Virtual Memory for the interpass temporary file.\n");
	printf("%d hashing collsions\n", nhcollisions);
	printf("%d hash table accesses\n", nhashed);
	printf("%d values entered in the hash table\n", nentered);
	printf("%d byte length of the temporary file\n", lgtmpfile);
	printf("%d symbols in the symbol table\n", nsyms);
	printf("%d labels in the symbol table\n", nlabels);
	printf("%d forgotten symbols\n", nforgotten);
	printf("%d iterations through all symbols to remove jxxxes\n",
			jxxxiterate);
	printf("%d jumps resolved via tunnelling\n", jxxxtunnel);
	if (jxdeadlock)
		printf("%d DEADLOCKED JXXXentries: Resolved by %s jumping\n",
			jxdeadlock, nbadjxsegs == 0 ? "short" : "long");
	if (nbadjxsegs)
		printf("%d Segments with jxxx over aligns.\n",
			nbadjxsegs);
	times(&tbuffer);
	printf("%1.2fu 1.2fs\n",
		((float)tbuffer.p_user)/60.0,
		((float)tbuffer.p_sys)/60.0);
}
#endif
