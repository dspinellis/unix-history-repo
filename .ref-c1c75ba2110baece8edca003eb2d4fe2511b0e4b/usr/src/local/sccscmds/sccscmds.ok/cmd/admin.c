# include "../hdr/defines.h"
# include "../hdr/had.h"

static char Sccsid[] = "@(#)admin.c	4.3	%G%";

/*
	Program to create new SCCS files and change parameters
	of existing ones. Arguments to the program may appear in
	any order and consist of keyletters, which begin with '-',
	and named files. Named files which do not exist are created
	and their parameters are initialized according to the given
	keyletter arguments, or are given default values if the
	corresponding keyletters were not supplied. Named files which
	do exist have those parameters corresponding to given key-letter
	arguments changed and other parameters are left as is.

	If a directory is given as an argument, each SCCS file within
	the directory is processed as if it had been specifically named.
	If a name of '-' is given, the standard input is read for a list
	of names of SCCS files to be processed.
	Non-SCCS files are ignored.

	Files created are given mode 444.
*/

# define MINR 1		/* minimum release number */
# define MAXR 9999	/* maximum release number */
# define MAXNAMES 9
# define COPY 0
# define NOCOPY 1

char *ifile, *tfile;
char *z;	/* for validation program name */
char had[26], had_flag[26], rm_flag[26];
char	*Comments, *Mrs;
char Valpgm[] = "/usr/local/val";
int irel, fexists, num_files;
int	VFLAG = 0;
int	Domrs;
char *Sflags[];
char *anames[MAXNAMES], *enames[MAXNAMES];
char *flag_p[26];
int asub, esub;
int check_id;
int Did_id;

main(argc,argv)
int argc;
char *argv[];
{
	register int j;
	register char *p;
	char c, f;
	int i, testklt;
	extern admin();
	extern int Fcnt;
	struct sid sid;

	/*
	Set flags for 'fatal' to issue message, call clean-up
	routine and terminate processing.
	*/
	Fflags = FTLMSG | FTLCLN | FTLEXIT;

	testklt = 1;

	/*
	The following loop processes keyletters and arguments.
	Note that these are processed only once for each
	invocation of 'main'.
	*/
	for(j=1; j<argc; j++)
		if(argv[j][0] == '-' && (c = argv[j][1])) {
			p = &argv[j][2];
			switch (c) {

			case 'i':	/* name of file of body */
				ifile = p;
				break;

			case 't':	/* name of file of descriptive text */
				tfile = p;
				break;
			case 'm':	/* mr flag */
				Mrs = p;
				break;
			case 'y':	/* comments flag for entry */
				Comments = p;
				break;

			case 'd':	/* flags to be deleted */
				testklt = 0;
				if (!(f = *p))
					fatal("d has no argument (ad1)");
				p = &argv[j][3];

				switch (f) {

				case IDFLAG:	/* see 'f' keyletter */
				case BRCHFLAG:	/* for meanings of flags */
				case VALFLAG:
				case TYPEFLAG:
				case MODFLAG:
				case NULLFLAG:
				case FLORFLAG:
				case CEILFLAG:
				case DEFTFLAG:
					if (*p) {
						sprintf(Error, "value after %c flag (ad12)",f);
						fatal(Error);
					}
					break;

				default:
					fatal("unknown flag (ad3)");
				}

				if (rm_flag[f - 'a']++)
					fatal("flag twice (ad4)");
				break;

			case 'f':	/* flags to be added */
				testklt = 0;
				if (!(f = *p))
					fatal("f has no argument (ad5)");
				p = &argv[j][3];

				switch (f) {

				case IDFLAG:	/* id-kwd message (err/warn) */
				case BRCHFLAG:	/* branch */
				case NULLFLAG:	/* null deltas */
					if (*p) {
						sprintf(Error, "value after %c flag (ad13)",f);
						fatal(Error);
					}
					break;

				case VALFLAG:	/* mr validation */
					VFLAG++;
					if (*p)
						z = p;
					break;

				case FLORFLAG:	/* floor */
					if ((i = patoi(p)) == -1)
						fatal("floor not numeric (ad22)");
					if ((size(p) > 5) || (i < MINR) ||
							(i > MAXR))
						fatal("floor out of range (ad23)");
					break;

				case CEILFLAG:	/* ceiling */
					if ((i = patoi(p)) == -1)
						fatal("ceiling not numeric (ad24)");
					if ((size(p) > 5) || (i < MINR) ||
							(i > MAXR))
						fatal("ceiling out of range (ad25)");
					break;

				case DEFTFLAG:	/* default sid */
					if (!(*p))
						fatal("no default sid (ad14)");
					chksid(sid_ab(p,&sid),&sid);
					break;

				case TYPEFLAG:	/* type */
				case MODFLAG:	/* module name */
					if (!(*p)) {
						sprintf(Error, "flag %c has no value (ad2)",f);
						fatal(Error);
					}
					break;

				default:
					fatal("unknown flag (ad3)");
				}

				if (had_flag[f - 'a']++)
					fatal("flag twice (ad4)");
				flag_p[f - 'a'] = p;
				break;

			case 'r':	/* initial release number supplied */
				if ((irel = patoi(p)) == -1)
					fatal("r arg not numeric (ad6)");
				if ((size(p) > 5) || (irel < MINR) ||
						(irel > MAXR))
					fatal("r out of range (ad7)");
				break;

			case 'n':	/* creating new SCCS file */
			case 'h':	/* only check hash of file */
			case 'z':	/* zero the input hash */
				break;

			case 'a':	/* user-name allowed to make deltas */
				testklt = 0;
				if (!(*p))
					fatal("bad a argument (ad8)");
				if (asub > MAXNAMES)
					fatal("too many 'a' keyletters (ad9)");
				anames[asub++] = p;
				break;

			case 'e':	/* user-name to be removed */
				testklt = 0;
				if (!(*p))
					fatal("bad e argument (ad10)");
				if (esub > MAXNAMES)
					fatal("too many 'e' keyletters (ad11)");
				enames[esub++] = p;
				break;

			default:
				fatal("unknown key letter (cm1)");
			}

			if (had[c - 'a']++ && testklt++)
				fatal("key letter twice (cm2)");
			argv[j] = 0;
		}
		else
			num_files++;

	if (num_files == 0)
		fatal("missing file arg (cm3)");

	if (HADI && num_files > 1) /* only one file allowed with `i' */
		fatal("more than one file (ad15)");

	setsig();

	/*
	Change flags for 'fatal' so that it will return to this
	routine (main) instead of terminating processing.
	*/
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;

	/*
	Call 'admin' routine for each file argument.
	*/
	for (j=1; j<argc; j++)
		if (p = argv[j])
			do_file(p,admin);

	exit(Fcnt ? 1 : 0);
}


/*
	Routine that actually does admin's work on SCCS files.
	Existing s-files are copied, with changes being made, to a
	temporary file (x-file). The name of the x-file is the same as the
	name of the s-file, with the 's.' replaced by 'x.'.
	s-files which are to be created are processed in a similar
	manner, except that a dummy s-file is first created with
	mode 444.
	At end of processing, the x-file is renamed with the name of s-file
	and the old s-file is removed.
*/

struct packet gpkt;	/* see file defines.h */
char	Zhold[BUFSIZ];	/* temporary z-file name */

USXALLOC();		/* defines alloc() and free() */

admin(afile)
char *afile;
{
	struct deltab dt;	/* see file defines.h */
	struct stats stats;	/* see file defines.h */
	FILE *iptr;
	register int k;
	register char *cp, *q;
	char command[80];
	char line[512];
	int i;			/* used in forking procedure */
	int status;
	extern nfiles;
	extern had_dir;

	if (setjmp(Fjmp))	/* set up to return here from 'fatal' */
		return;		/* and return to caller of admin */

	if (HADI && had_dir) /* directory not allowed with `i' keyletter */
		fatal("directory named with `i' keyletter (ad26)");

	fexists = exists(afile);

	if (HADI)
		HADN = 1;
	if (HADI || HADN) {
		if (HADM && !VFLAG)
			fatal("MRs not allowed (de8)");

		if (VFLAG && !HADM)
			fatal("MRs required (de10)");

	}

	if (!HADI && HADR)
		fatal("r only allowed with i (ad16)");

	if (HADN && HADT && !(*tfile))
		fatal("t has no argument (ad17)");

	if (HADN && HADD)
		fatal("d not allowed with n (ad18)");

	if (HADN && fexists) {
		sprintf(Error,"file %s exists (ad19)",afile);
		fatal(Error);
	}

	if (!HADN && !fexists) {
		sprintf(Error,"file %s does not exist (ad20)",afile);
		fatal(Error);
	}
	/*
	   Check for '-h' flag.  If set, create child process and
	   invoke 'get' to examine format of SCCS file.
	*/

	if (HADH) {
		/*
		   fork here so 'admin' can execute 'val' to
		   check for a corrupted file.
		*/
		if ((i = fork()) < 0)
			fatal("cannot fork, try again");
		if (i == 0) {		/* child */
			/*
			   perform 'val' with appropriate keyletters
			*/
			sprintf(command, "/usr/local/val -s %s", afile);
			execl("/bin/sh","/bin/sh","-c", command, 0);
			sprintf(Error,"cannot execute '%s'",Valpgm);
			fatal(Error);
		}
		else {
			wait(&status);	   /* wait on status from 'execl' */
			if (status)
				fatal("corrupted file (co6)");
			return;		/* return to caller of 'admin' */
		}
	}

	/*
	Lock out any other user who may be trying to process
	the same file.
	*/
	if (!HADH && lockit(copy(auxf(afile,'z'),Zhold),2,getpid()))
		fatal("cannot create lock file (cm4)");

	if (fexists)
		sinit(&gpkt,afile,1);	/* init pkt & open s-file */
	else {
		xfcreat(afile,0444);	/* create dummy s-file */
		sinit(&gpkt,afile,0);	/* and init pkt */
	}

	if (!HADH)
		/*
		   set the flag for 'putline' routine to open
		   the 'x-file' and allow writing on it.
		*/
		gpkt.p_upd = 1;

	if (HADZ) {
		gpkt.do_chksum = 0;	/* ignore checksum processing */
		gpkt.p_ihash = 0;
	}

	/*
	Get statistics of latest delta in old file.
	*/
	if (!HADN) {
		stats_ab(&gpkt,&stats);
		gpkt.p_wrttn++;
		newstats(&gpkt,line,"0");
	}

	if (HADN) {		/*   N E W   F I L E   */

		/*
		Beginning of SCCS file.
		*/
		sprintf(line,"%c%c%s\n",CTLCHAR,HEAD,"00000");
		putline(&gpkt,line);

		/*
		Statistics.
		*/
		newstats(&gpkt,line,"0");

		dt.d_type = 'D';	/* type of delta */

		/*
		Set initial release, level, branch and
		sequence values.
		*/
		if (HADR)
			dt.d_sid.s_rel = irel;
		else
			dt.d_sid.s_rel = 1;
		dt.d_sid.s_lev = 1;
		dt.d_sid.s_br = dt.d_sid.s_seq = 0;

		time(&dt.d_datetime);		/* get time and date */

		copy(logname(),dt.d_pgmr);	/* get user's name */

		dt.d_serial = 1;
		dt.d_pred = 0;

		del_ba(&dt,line);	/* form and write */
		putline(&gpkt,line);	/* delta-table entry */

		/*
		If -m flag, enter MR numbers
		*/

		if (Mrs) {
			mrfixup();
			if (z && valmrs(&gpkt,z))
				fatal("invalid MRs (de9)");
			putmrs(&gpkt);
		}

		/*
		Enter comment line for `chghist'
		*/

		if (HADY) {
			sprintf(line,"%c%c ",CTLCHAR,COMMENTS);
			putline(&gpkt,line);
			putline(&gpkt,Comments);
			putline(&gpkt,"\n");
		}
		else {
			/*
			insert date/time and pgmr into comment line
			*/
			cmt_ba(&dt,line);
			putline(&gpkt,line);
		}
		/*
		End of delta-table.
		*/
		sprintf(line,CTLSTR,CTLCHAR,EDELTAB);
		putline(&gpkt,line);

		/*
		Beginning of user-name section.
		*/
		sprintf(line,CTLSTR,CTLCHAR,BUSERNAM);
		putline(&gpkt,line);
	}
	else
		/*
		For old file, copy to x-file until user-name section
		is found.
		*/
		flushto(&gpkt,BUSERNAM,COPY);

	/*
	Write user-names to be added to list of those
	allowed to make deltas.
	*/
	if (HADA)
		for (k = 0; k < asub; k++) {
			sprintf(line,"%s\n",anames[k]);
			putline(&gpkt,line);
		}

	/*
	Do not copy those user-names which are to be erased.
	*/
	if (HADE && !HADN)
		while ((cp = getline(&gpkt)) &&
				!(*cp++ == CTLCHAR && *cp == EUSERNAM)) {
			for (k = 0; k < esub; k++) {
				cp = &gpkt.p_line;
				while (*cp)	/* find and */
					cp++;	/* zero newline */
				*--cp = '\0';	/* character */

				if (equal(enames[k],&gpkt.p_line)) {
					/*
					Tell getline not to output
					previously read line.
					*/
					gpkt.p_wrttn = 1;
					break;
				}
				else
					*cp = '\n';	/* restore newline */
			}
		}

	if (HADN) {		/*   N E W  F I L E   */

		/*
		End of user-name section.
		*/
		sprintf(line,CTLSTR,CTLCHAR,EUSERNAM);
		putline(&gpkt,line);
	}
	else
		/*
		For old file, copy to x-file until end of
		user-names section is found.
		*/
		if (!HADE)
			flushto(&gpkt,EUSERNAM,COPY);

	/*
	For old file, read flags and their values (if any), and
	store them. Check to see if the flag read is one that
	should be deleted.
	*/
	if (!HADN)
		while ((cp = getline(&gpkt)) &&
				(*cp++ == CTLCHAR && *cp == FLAG)) {

			gpkt.p_wrttn = 1;	/* don't write previous line */

			cp += 2;	/* point to flag character */
			k = *cp - 'a';

			if (!had_flag[k] && !rm_flag[k]) {
				had_flag[k] = 2;	/* indicate flag is */
							/* from file, not */
							/* from arg list */

				if (*++cp != '\n') {	/* get flag value */
					q = alloc(size(gpkt.p_line)-5);
					copy(++cp,q);
					flag_p[k] = q;
					while (*q)	/* find and */
						q++;	/* zero newline */
					*--q = '\0';	/* character */
				}
			}
			else
				if (rm_flag[k])
					had_flag[k] = 0;
		}


	/*
	Write out flags.
	*/
	for (k = 0; k < 26; k++)
		if (had_flag[k]) {
			if (flag_p[k])
				sprintf(line,"%c%c %c %s\n",
					CTLCHAR,FLAG,'a'+k,flag_p[k]);
			else
				sprintf(line,"%c%c %c\n",
					CTLCHAR,FLAG,'a'+k);

			putline(&gpkt,line);

			if (had_flag[k] == 2) {	/* flag was taken from file */
				had_flag[k] = 0;
				if (flag_p[k]) {
					free(flag_p[k]);
					flag_p[k] = 0;
				}
			}
		}

	if (HADN) {
		/*
		Beginning of descriptive (user) text.
		*/
		sprintf(line,CTLSTR,CTLCHAR,BUSERTXT);
		putline(&gpkt,line);
	}
	else
		/*
		Write out BUSERTXT record which was read in
		above loop that processes flags.
		*/
		gpkt.p_wrttn = 0;
		putline(&gpkt,0);

	/*
	Get user description, copy to x-file.
	*/
	if (HADT) {
		if (*tfile) {
			iptr = xfopen(tfile,0);
			fgetchk(line,512,iptr,tfile,&gpkt);
			fclose(iptr);
		}

		/*
		If old file, ignore any previously supplied
		commentary. (i.e., don't copy it to x-file.)
		*/
		if (!HADN)
			flushto(&gpkt,EUSERTXT,NOCOPY);
	}

	if (HADN) {		/*   N E W  F I L E   */

		/*
		End of user description.
		*/
		sprintf(line,CTLSTR,CTLCHAR,EUSERTXT);
		putline(&gpkt,line);

		/*
		Beginning of body (text) of first delta.
		*/
		sprintf(line,"%c%c %u\n",CTLCHAR,INS,1);
		putline(&gpkt,line);

		if (HADI) {		/* get body */

			/*
			Set indicator to check lines of body of file for
			keyword definitions.
			If no keywords are found, a warning
			will be produced.
			*/
			check_id = 1;
			/*
			Set indicator that tells whether there
			were any keywords to 'no'.
			*/
			Did_id = 0;
			if (*ifile)
				iptr = xfopen(ifile,0);	/* from a file */
			else
				iptr = stdin;	/* from standard input */

			/*
			Read and copy to x-file, while checking
			first character of each line to see that it
			is not the control character (octal 1).
			Also, count lines read, and set statistics'
			structure appropriately.
			The 'fgetchk' routine will check for keywords.
			*/
			stats.s_ins = fgetchk(line,512,iptr,ifile,&gpkt);
			stats.s_del = stats.s_unc = 0;

			/*
			If no keywords were found, issue warning.
			*/
			if (!Did_id) {
				if (had_flag[IDFLAG - 'a'])
					fatal("no id keywords (cm6)");
				else
					fprintf(stderr,"%s\n","No id keywords (cm7)");
			}

			check_id = 0;
			Did_id = 0;
		}

		/*
		End of body of first delta.
		*/
		sprintf(line,"%c%c %u\n",CTLCHAR,END,1);
		putline(&gpkt,line);
	}
	else {
		/*
		Indicate that EOF at this point is ok, and
		flush rest of (old) s-file to x-file.
		*/
		gpkt.p_chkeof = 1;
		while (getline(&gpkt)) ;
	}

	/*
	Flush the buffer, take care of rewinding to insert
	checksum and statistics in file, and close.
	*/
	flushline(&gpkt,&stats);

	/*
	Change x-file name to s-file, and delete old file.
	Unlock file before returning.
	*/
	if (!HADH) {
		rename(auxf(&gpkt,'x'),&gpkt);
		xrm(&gpkt);
		unlockit(auxf(afile,'z'),getpid());
	}
}


fgetchk(strp,len,inptr,file,pkt)
register char *strp;
register int len;
FILE *inptr;
register char *file;
register struct packet *pkt;
{
	register int k;

	for (k = 1; fgets(strp,len,inptr); k++) {
		if (*strp == CTLCHAR) {
			sprintf(Error,"%s illegal data on line %d (ad21)",
				file,k);
			fatal(Error);
		}

		if (check_id)
			chkid(strp);

		putline(pkt,strp);
	}
	return(k - 1);
}


clean_up()
{
	xrm(&gpkt);
	if (!HADH)
		unlockit(Zhold,getpid());
	if (HADN)
		unlink(&gpkt);
}


cmt_ba(dt,str)
register struct deltab *dt;
char *str;
{
	register char *p;

	p = str;
	*p++ = CTLCHAR;
	*p++ = COMMENTS;
	*p++ = ' ';
	copy("date and time created",p);
	while (*p++)
		;
	--p;
	*p++ = ' ';
	date_ba(&dt->d_datetime,p);
	while (*p++)
		;
	--p;
	*p++ = ' ';
	copy("by",p);
	while (*p++)
		;
	--p;
	*p++ = ' ';
	copy(dt->d_pgmr,p);
	while (*p++)
		;
	--p;
	*p++ = '\n';
	*p = 0;
	return(str);
}


putmrs(pkt)
struct packet *pkt;
{
	register char **argv;
	char str[64];
	extern char *Varg[];

	for (argv = &Varg[VSTART]; *argv; argv++)
		sprintf(str,"%c%c %s\n",CTLCHAR,MRNUM,*argv);
		putline(pkt,str);
}
