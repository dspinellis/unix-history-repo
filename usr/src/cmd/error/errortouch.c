static	char *sccsid = "@(#)errortouch.c	1.2 (Berkeley) 10/16/80";
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "error.h"

findfiles(nerrors, errors, r_nfiles, r_files)
	int	nerrors;
	struct	error_desc	**errors;
	int	*r_nfiles;
	struct	error_desc	****r_files;
{
			int	nfiles;
	struct	error_desc	***files;

			char	*currentfilename;
	register	int	errorindex;
			int	fileindex;
	register	struct	error_desc	*errorp;
	/*
	 *	First, go through and count all of the filenames
	 */
	for (errorp = errors[errorindex = 0],nfiles = 0, currentfilename = "\1";
	     errorindex < nerrors;
	     errorp = errors[++errorindex]){
		if (SORTABLE(errorp->error_e_class)){
			if (strcmp(errorp->error_text[0],currentfilename) != 0){
				nfiles++;
				currentfilename = errorp->error_text[0];
			}
		}
	}
	files = (struct error_desc ***)Calloc(nfiles + 3,
		sizeof (struct error_desc**));
	touchedfiles = (boolean	*)Calloc(nfiles+3, sizeof(boolean));
	/*
	 *	Now, go through and partition off the error messages
	 *	into those that are synchronization, discarded or
	 *	not specific to any file, and those that were
	 *	nulled or true errors.
	 */
	files[0] = &errors[0];
	for (errorp = errors[errorindex = 0], fileindex = 0;
	     (errorindex < nerrors) &&
		(NOTSORTABLE(errorp->error_e_class));
	     errorp = errors[++errorindex]){
		continue;
	}
	/*
	 *	Now, go through and partition off all error messages
	 *	for a given file.
	 */
	files[1] = &errors[errorindex];
	touchedfiles[0] = touchedfiles[1] = FALSE;
	for (errorp = errors[errorindex], currentfilename = "\1", fileindex = 1;
	     errorindex < nerrors; errorp = errors[++errorindex]){
		if ( (errorp->error_e_class == C_NULLED) || (errorp->error_e_class == C_TRUE) ){
			if (strcmp(errorp->error_text[0],currentfilename) != 0){
				currentfilename = errorp->error_text[0];
				touchedfiles[fileindex] = FALSE;
				files[fileindex++] = &errors[errorindex];
			}
		}
	}
	files[fileindex] = &errors[nerrors];
	*r_nfiles = nfiles;
	*r_files = files;
}

char	*class_table[] = {
	/*C_UNKNOWN	0	*/	"Unknown",
	/*C_IGNORE	1	*/	"ignore",
	/*C_SYNC	2	*/	"synchronization",
	/*C_DISCARD	3	*/	"discarded",
	/*C_NONSPEC	4	*/	"non specific",
	/*C_THISFILE	5	*/	"specific to this file",
	/*C_NULLED	6	*/	"nulled",
	/*C_TRUE	7	*/	"true",
	/*C_DUPL	8	*/	"duplicated"
};

int	class_count[C_LAST - C_FIRST] = {0};

filenames(nfiles, files)
	int	nfiles;
	struct	error_desc	***files;
{
	register	int	fileindex;
	register	struct	error_desc	*errorp;
	register	struct	error_desc	**erpp;
			char	*sep = " ";
	register	int	errortype;
	extern		char	*class_table[];
			int	someerrors = 0;

	/*
	 *	first, go through and simply dump out errors that
	 *	don't pertain to any file
	 */
	if (files[1] - files[0] > 0){
	    for(errortype = C_UNKNOWN; NOTSORTABLE(errortype); errortype++){
		if (class_count[errortype] > 0){
			if (errortype > C_SYNC)
				someerrors++;
			fprintf(stdout, "\n\t%d %s errors follow:\n",
				class_count[errortype], class_table[errortype]);
			for (errorp = *(erpp = files[0]);
			     erpp < files[1];
			     errorp = (*++erpp)){
				if (errorp->error_e_class == errortype)
					errorprint(stdout, errorp, TRUE);
			}
		}
	    }
	}
	if (nfiles){
		someerrors++;
		fprintf(stdout, "%d files contain errors:", nfiles);
		for (fileindex = 1; fileindex <= nfiles; fileindex++){
			fprintf(stdout, "%s\"%s\" (%d)",
				sep, (*files[fileindex])->error_text[0],
				files[fileindex+1] - files[fileindex]);
			sep = ", ";
		}
		fprintf(stdout, "\n");
	}
	if (!someerrors)
		fprintf(stdout, "No errors.\n");
}

extern	boolean	notouch;

boolean touchfiles(nfiles, files, r_edargc, r_edargv)
	int	nfiles;
	struct	error_desc	***files;
	int	*r_edargc;
	char	***r_edargv;
{
			char	*currentfilename;
	register	struct	error_desc	*errorp;
	register	int	fileindex;
	register	struct	error_desc	**erpp;
			int		ntrueerrors;
			int		errordest;	/* where errors go*/
			char		*sep;
			boolean		scribbled;
			int		n_pissed_on;	/* # of file touched*/
			int		previewed;

	for (fileindex = 1; fileindex <= nfiles; fileindex++){
		fprintf(stdout, "\nFile \"%s\" has %d total error messages.\n",
			currentfilename = (*files[fileindex])->error_text[0],
			files[fileindex+1] - files[fileindex]);
		/*
		 *	First, iterate through all error messages in this file
		 *	to see how many of the error messages really will
		 *	get inserted into the file.
		 */
		for (erpp = files[fileindex], ntrueerrors = 0;
		     erpp < files[fileindex+1];
		     erpp++){
			errorp = *erpp;
			if (errorp->error_e_class == C_TRUE)
				ntrueerrors++;
		}
		fprintf(stdout,"\t%d of these errors can be inserted into the file.\n",
			ntrueerrors);

		/*
		 *	What does the operator want?
		 */
		previewed = 0;
		errordest = TOSTDOUT;
		if (oktotouch(currentfilename) && (ntrueerrors > 0) ){
			if (query && inquire("Do you want to preview the errors first?")){
				previewed = 1;
				for (erpp = files[fileindex];
				     erpp < files[fileindex + 1];
				     erpp++){
					errorprint(stdout, *erpp, TRUE);
				}
				fprintf(stdout, "\n");
			}
			if (   !query
			    || inquire("Do you want to touch file \"%s\"? ",
					currentfilename)
			){
				errordest = TOTHEFILE;
				if (!probethisfile(currentfilename)){
					errordest = TOSTDOUT;
					fprintf(stdout,
					 "Can't find file \"%s\" to insert error messages into.\n",
						currentfilename);
				} else {
					if (edit(currentfilename))
						errordest = TOSTDOUT;
					else
						touchedfiles[fileindex] = TRUE;
				}
			}
		}
		if (previewed && (errordest == TOSTDOUT))
			continue;		/* with the next file */
		/*
		 *	go through and print each error message,
		 *	diverting to the right place
		 */
		if ( (files[fileindex+1] - files[fileindex]) != ntrueerrors)
			if (!previewed) fprintf(stdout,
			    ">>Uninserted error messages for file \"%s\" follow.\n",
			    currentfilename);
		for (erpp = files[fileindex];erpp < files[fileindex+1];erpp++){
			errorp = *erpp;
			if (errorp->error_e_class == C_TRUE){
				switch (errordest){
				  case TOSTDOUT:
					if (!previewed)
						errorprint(stdout,errorp, TRUE);
					break;
				  case TOTHEFILE:
					insert(errorp->error_line);
					text(errorp, FALSE);
					break;
				}	/* switch */
			} else {
				if (!previewed)
					errorprint(stdout, errorp, TRUE);
			}
		}	/* end of walking through all errors*/
		if (errordest == TOTHEFILE){
			writetouched();
		}
	}	/* end of walking through all files*/
	scribbled = FALSE;
	for (n_pissed_on = 0, fileindex = 1; fileindex <= nfiles; fileindex++){
		scribbled |= touchedfiles[fileindex];
		n_pissed_on++;
	}
	if (scribbled){
		/*
		 *	Construct an execv argument
		 *	We need 1 argument for the editor's name
		 *	We need 1 argument for the initial search string
		 *	We need n_pissed_on arguments for the file names
		 *	We need 1 argument that is a null for execv.
		 *	The caller fills in the editor's name.
		 *	We fill in the initial search string.
		 *	We fill in the arguments, and the null.
		 */
		(*r_edargv) = (char **)Calloc(n_pissed_on + 3, sizeof(char *));
		(*r_edargc) =  n_pissed_on + 2;
		(*r_edargv)[1] = "+/###/";
		n_pissed_on = 2;
		fprintf(stdout, "You touched file(s):");
		sep = " ";
		for (fileindex = 1; fileindex <= nfiles; fileindex++){
			if (!touchedfiles[fileindex])
				continue;
			errorp = *(files[fileindex]);
			fprintf(stdout,"%s\"%s\"", sep, errorp->error_text[0]);
			sep = ", ";
			(*r_edargv)[n_pissed_on++] = errorp->error_text[0];
		}
		fprintf(stdout, "\n");
		(*r_edargv)[n_pissed_on] = 0;
		return(TRUE);
	} else {
		fprintf(stdout, "You didn't touch any files.\n");
		return(FALSE);
	}

}	/* end of touchfiles*/
int	oktotouch(filename)
	char	*filename;
{
	extern		char	*suffixlist;
	register	char	*src;
	register	char	*pat;
			char	*osrc;

	pat = suffixlist;
	if (pat == 0)
		return(0);
	if (*pat == '*')
		return(1);
	while (*pat++ != '.')
		continue;
	--pat;		/* point to the period */

	for (src = &filename[strlen(filename)], --src;
	     (src > filename) && (*src != '.'); --src)
		continue;
	if (*src != '.')
		return(0);

	for (src++, pat++, osrc = src; *src && *pat; src = osrc, pat++){
		for (;   *src			/* not at end of the source */
		      && *pat			/* not off end of pattern */
		      && *pat != '.'		/* not off end of sub pattern */
		      && *pat != '*'		/* not wild card */
		      && *src == *pat;		/* and equal... */
		      src++, pat++)
			continue;
		if (*src == 0 && (*pat == 0 || *pat == '.' || *pat == '*'))
			return(1);
		if (*src != 0 && *pat == '*')
			return(1);
		while (*pat && *pat != '.')
			pat++;
		if (! *pat)
			return(0);
	}
	return(0);
}

FILE	*o_touchedfile;	/* the old file */
FILE	*n_touchedfile;	/* the new file */
char	*o_name;
char	n_name[32];
char	*canon_name = "ErrorXXXXXX";
int	o_lineno;
int	n_lineno;
boolean	tempfileopen = FALSE;
/*
 *	open the file; guaranteed to be both readable and writable
 *	Well, if it isn't, then return TRUE if something failed
 */
boolean edit(name)
	char	*name;
{
	o_name = name;
	if ( (o_touchedfile = fopen(name, "r")) == NULL){
		fprintf(stderr, "%s: Can't open file \"%s\" to touch (read).\n",
			processname, name);
		return(TRUE);
	}
	strcpy(n_name, canon_name);
	mktemp(n_name);
	if ( (n_touchedfile = fopen(n_name, "w")) == NULL){
		fprintf(stderr,"%s: Can't open file \"%s\" to touch (write).\n",
			processname, name);
		return(TRUE);
	}
	tempfileopen = TRUE;
	n_lineno = 0;
	o_lineno = 0;
	return(FALSE);
}
/*
 *	Position to the line (before, after) the line given by place
 */
char	edbuffer[BUFSIZ];
insert(place)
	int	place;
{
	--place;	/* always insert messages before the offending line*/
	for(; o_lineno < place; o_lineno++, n_lineno++){
		if(fgets(edbuffer, BUFSIZ, o_touchedfile) == NULL)
			return;
		fputs(edbuffer, n_touchedfile);
	}
}

text(errorp, use_all)
	register	struct	error_desc	*errorp;
	boolean	use_all;
{
	int	offset = use_all ? 0 : 2;
	fputs(lang_table[errorp->error_language].lang_incomment, n_touchedfile);
	fprintf(n_touchedfile, "%d [%s] ",
		errorp->error_line,
		lang_table[errorp->error_language].lang_name);
	wordvprint(n_touchedfile,
		errorp->error_lgtext-offset, errorp->error_text+offset);
	fputs(lang_table[errorp->error_language].lang_outcomment,n_touchedfile);
	n_lineno++;
}

writetouched()
{
	int	bytes_read;
	for(; (bytes_read = fread(edbuffer, 1, sizeof(edbuffer), o_touchedfile))!= NULL; ){
		fwrite(edbuffer, 1, bytes_read, n_touchedfile);
	}
	fclose(n_touchedfile);
	fclose(o_touchedfile);
	unlink(o_name);
	link(n_name, o_name);	
	unlink(n_name);
	tempfileopen = FALSE;
}
onintr()
{
	if (inquire("\nInterrupt: Do you want to continue?")){
		signal(SIGINT, onintr);
		return;
	}
	if (tempfileopen)
		writetouched();
	exit(1);
}
errorprint(place, errorp, print_all)
	FILE	*place;
	struct	error_desc	*errorp;
	boolean	print_all;
{
	int	offset = print_all ? 0 : 2;

	if (errorp->error_e_class == C_IGNORE)
		return;
	fprintf(place, "[%s] ", lang_table[errorp->error_language].lang_name);
	wordvprint(place,errorp->error_lgtext-offset,errorp->error_text+offset);
	putc('\n', place);
}

boolean inquire(fmt, a1, a2)
	char	*fmt;
	/*VARARGS1*/
{
	char	buffer[128];
	char	ch;
	for(;;){
		do{
			fflush(stdout);
			fprintf(stderr, fmt, a1, a2);
			fflush(stderr);
		} while (fgets(buffer, 127, queryfile) == NULL);
		ch = buffer[0];
		if (ch == 'Y' || ch == 'y')
			return(TRUE);
		if (ch == 'N' || ch == 'n')
			return(FALSE);
		fprintf(stderr, "Yes or No only!\n");
	}
}

boolean probethisfile(currentfilename)
	char	*currentfilename;
{
	struct stat statbuf;
	if (stat(currentfilename, &statbuf) != 0)
		return(FALSE);
	if ( (statbuf.st_mode&S_IREAD) && (statbuf.st_mode&S_IWRITE))
		return(TRUE);
	return(FALSE);
}
