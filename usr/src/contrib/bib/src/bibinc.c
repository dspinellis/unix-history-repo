#include "comments.h"

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/malloc.h>
#include <string.h>
#define NODEBUG /* */
#include "adsdebug.h"

FILE   *outfull, *outshort, *inall;
char    inbuf[512];
char    outbuf[512];
int     state, lineno;
bool	tibOption, bibOption, troffOption, texOption;
bool usingTibMacros;
bool usingTexProcessor;
bool emittingForTib;
bool emittingForTex;
bool outputRestricted;

#define commentChar  '#'
#define restrictChar '?'
#define escapeChar   '+'
#define macroChar    '|'
#define backwhack    '\\'

#define skipBlanks(p) \
		while (isspace(*p)) p++; \
		if (*p == commentChar) while (*++p != '\0')

#define maxNofLetters 128
#define maxNofFilesPerLetter 8

char letter[maxNofLetters];		int nofLetters;
FILE *letterFiles[maxNofLetters][maxNofFilesPerLetter];
char letterState[maxNofLetters];

#	define donothing	' '
#	define outfile		'O'
#	define deffile		'D'

char *filename[maxNofLetters];		int nofFilenames;

int outputIdx = 0;

void
  flushOutbuf()
{
  int j;
  if (*outbuf != '\0') {
      if (outbuf[j=strlen(outbuf)] != '\n') outbuf[j] = '\n';
      outbuf[j+1] = '\0';
      for (j=0; letterFiles[outputIdx][j] != NULL; j++) {
	  fputs(outbuf, letterFiles[outputIdx][j]);
	  }
      *outbuf = '\0';
      }
  }

closeAllFiles()
{
    int lidx,j;
    flushOutbuf();
    fclose(inall);
    for (lidx = 1; lidx < nofLetters; lidx++) {
	for (j = 0; letterFiles[lidx][j] != NULL; j++) {
	    fclose(letterFiles[lidx][j]);
	    }
	}
}

error(s)
    char   *s;
{
    fprintf(stderr, "%s\n", s);
    fprintf(stderr, "Error near line %d:\n", lineno);
    fprintf(stderr, "%s\n", inbuf);
    closeAllFiles();
    exit(1);
}

ready2out(fileIdx,buf)
    int	   fileIdx;
    char   *buf;
{
    register char *ip;
    register char *op;
    register char *tp;
    bool seenOneBar;
    int j;
    /* first write outbuf, if it is there (had to wait to write it until
     * the continuation slash was appended, if necessary)
     */
    flushOutbuf();
    outputIdx = fileIdx;
    ip = buf;
    op = outbuf;
    seenOneBar = false;
    while (*ip != '\0') {
	if (*ip == backwhack) {
	    /*  \&	if usingTexProcessor, remove
			if usingTibMacros, remove
			if usingTroffProcessor, keep
			if usingBibMacros, keep
		\\	if usingTexProcessor, reduce to \
			else keep
		\<NULL>	remove
	    /* */
	    if (*(ip+1) == '\0') ip += 2; /* cont'n slash; ignore it */
	    else {
	       if (*(ip+1) == backwhack) {
		  if (usingTexProcessor) {
		     *op++ = *ip++; ip++; /* reduce to \ */
		     }
		  else { *op++ = *ip++; *op++ = *ip++; } /* keep */
		  }
	       else if (*(ip+1) == '&') {
		  if (usingTexProcessor || usingTibMacros) {
		     ip += 2; /* skip it */
		     }
		  else { *op++ = *ip++; *op++ = *ip++; } /* keep */
		  }
	       else { *op++ = *ip++; *op++ = *ip++; }    /* keep */
	       }
	    }
	else if (*ip == macroChar) {
	    if (seenOneBar) {
		if (!usingTibMacros) {
		    /* check the following character: 
		     * if alphabetic, turn into \& */
		    if (isalpha(*(ip+1))) {
			*op++ = backwhack; 
			*op++ = '&';
			}
		    }
		else *op++ = macroChar;
		seenOneBar = false;
		}
	    else {
		if (!usingTibMacros) {
		    /* check the preceding char: if alphabetic, turn into \& */
		    if (op != outbuf && isalpha(*(op-1))) {
			*op++ = backwhack; 
			*op++ = '&';
			}
		    }
		else *op++ = macroChar;
		seenOneBar = true;
		}
	    ip++;
	    }
	else if (*ip == commentChar) {
	    while (op != outbuf && isspace(*--op));
	    op++;
	    break;
	    }
	else {
	    *op++ = *ip++;
	    }
        }
    if (seenOneBar) {
	fprintf(stderr,"Oops: misbalanced |bars| detected\n%s",buf);
	}
    *op = '\0';
}

main(argc, argv) 
    int argc;
    char **argv;
{
    int opt;
    register char *inp;
    int lidx;
    int i,j;

    tibOption = bibOption = troffOption = texOption = false;
    for (i=1; i<argc; i++) {
	char *cp;
	dbg(loop) dbgd(i) dbgs(argv[i]) 
	    dbgb(tibOption) dbgb(bibOption) dbgb(troffOption) dbgb(texOption)
	     eor;
	j = 0;
	if (argv[i][0] == '-') j = 1;
	cp = argv[i]; 
	while (*cp != '\0') { 
	   if (isupper(*cp)) *cp = tolower(*cp); 
	   cp++; 
	   }
	if      (strcmp(&argv[i][j], "tib") == 0) tibOption = true;
	else if (strcmp(&argv[i][j], "troff") == 0) troffOption = true;
	else if (strcmp(&argv[i][j], "tex") == 0) texOption = true;
	else if (strcmp(&argv[i][j], "bib") == 0) bibOption = true;
	else error("Unknown option on invocation line");
	}
    dbg(opts) dbgb(tibOption) dbgb(bibOption) dbgb(troffOption) dbgb(texOption)
    eor;
    if (!texOption && !troffOption) troffOption = true;
    if (!bibOption && !tibOption) bibOption = true;
    if (bibOption) troffOption = true;
    if (texOption) tibOption = true;
    if ((bibOption && tibOption) 
	|| (troffOption && texOption) 
	|| (bibOption && texOption)
	|| (troffOption && !bibOption && !tibOption)
	|| (tibOption && !troffOption && !texOption)) {
	error("Cannot specify this combination of invocation line parameters");
    }

    usingTibMacros = tibOption;
    usingTexProcessor = texOption;
    emittingForTib = usingTibMacros;
    emittingForTex = usingTexProcessor;

    inall = stdin;
    state = donothing;
    lineno = 0;
    fgets(inbuf, sizeof(inbuf), inall);
    { register int i; if (inbuf[i=strlen(inbuf)-1] == '\n') inbuf[i] = '\0'; }
    nofLetters = 0;
    while (!feof(inall)) {
	/* note that state is set across line boundaries and maintained
	 * across iterations of this loop! */
	lineno++;
	inp = inbuf;
	if (*inp == commentChar)
	    state = donothing;
	else if (*inp == restrictChar) {
	    inp++;
	    skipBlanks(inp);
	    if (*inp == '\0') {
		emittingForTib = usingTibMacros;
		emittingForTex = usingTexProcessor;
		}
	    else if (strncmp(inp, "bib", 3) == 0) emittingForTib = false;
	    else if (strncmp(inp, "tib", 3) == 0) emittingForTib = true;
	    else if (strncmp(inp, "troff", 5) == 0) emittingForTex = false;
	    else if (strncmp(inp, "tex", 3) == 0) emittingForTex = true;
	    else error("Unknown restriction word");
	    while (!isspace(*inp) && *inp != '\0') inp++;
	    skipBlanks(inp);
	    if (*inp != '\0')
	       error("Illegal restriction syntax");

	    outputRestricted = 
		(usingTibMacros && !emittingForTib)
	    ||	(!usingTibMacros && emittingForTib)
	    ||	(usingTexProcessor && !emittingForTex)
	    ||	(!usingTexProcessor && emittingForTex);
	    state = donothing;
	    }
	else if (outputRestricted) {
	    state = donothing;
	    }
	else if (*inp == escapeChar) {
	    int cidx;
	    char flagChar = *++inp;
	    if (!(flagChar > ' ' && flagChar <= '~')) {
	       error("Illegal character follows escape char ");
	       }
	    state = deffile;
	    for (lidx=0; lidx < nofLetters; lidx++) {
		if (letter[lidx] == flagChar) {
		    state = letterState[lidx];
		    break;
		    }
		}
	    if (state == outfile) {
	        if (isspace(*(inp+1))) {
		    *inp = 'D'; /* turn into D <word> <stuff> */
		    }
		else {
		    inp++; /* arbitrary stuff */
		    }
		ready2out(lidx, inp);
		}
	    else if (state == deffile) {
		/* syntax: +<flagChar> <filename>
		    or:    +<flagChar> +<flagChar2> ...
			    where <flagChar2> has already been defined
		/* */
		int j;
		if (!isspace(*++inp)) {
		    fprintf(stderr,"%s\n", inp);
		    error("Single character expected in char definition");
		    }
		/* has this letter already been defined? */
		for (j=0; j < nofLetters; j++)
		    if (letter[j] == flagChar) error("letter already defined");
		letter[nofLetters] 
			= flagChar; /* but don't bump nofLetters yet */
		/* do we have a <filename> or +<letter2> ? */
		skipBlanks(inp);
		if (*inp == escapeChar) {
		    int k;
		    /* use this letter's files;
		     * notice that this letter has not yet been added to the
		     *	letter array; and it won't be until we check to see 
		     *	if letter2 exists */
		    k = 0; /* current letter's file index */
		    while (*inp == escapeChar) {
			bool found;
			int i;
			found = false;
			inp++;
			for (i=0; i<nofLetters; i++) {
			    if (*inp == letter[i]) {
				found = true;
				for (j=0; letterFiles[i][j] != NULL; j++) {
				    if (k > maxNofFilesPerLetter)
					error(
					"Too many files defined for letter");
				    letterFiles[nofLetters][k++] =
					letterFiles[i][j];
				    }
				break;
				}
			    }
			if (!found) error("undefined letter in letter list");
			inp++;
			while (isspace(*inp)) inp++;
			}
		    letterState[nofLetters] = outfile;
		    letterFiles[nofLetters++][k] = NULL;
		    /* now the letter is defined */
		    }
		else { /* then we have a file name */
		    int k;
		    char tbuf[255];
		    char *tp;
		    /* this better define a writable file */
		    if (nofFilenames == maxNofLetters)
			error("too many files defined");
		    tp = tbuf;
		    while (isgraph(*inp) && *inp != commentChar) {
			*tp++ = *inp++;
			if (tp == &tbuf[255]) error("filename too long(?)");
			}
		    *tp = '\0';
		    filename[nofFilenames] = (char *)malloc(strlen(tbuf)+1);
		    strcpy(filename[nofFilenames],tbuf);
		    for (k=0; k < nofFilenames; k++)
			if (strcmp(filename[k],tbuf) == 0)
				error("File defined twice");
		    for (k=0; letterFiles[nofLetters][k] != NULL; k++);
		    if (k > maxNofFilesPerLetter) {
			error("Too many files defined for letter");
			}
		    if ((letterFiles[nofLetters][k] = 
			fopen(filename[nofFilenames],"w")) == NULL)
			    error("cannot open file");
		    nofFilenames++;
		    letterState[nofLetters++] = outfile;
		    }
		while (isspace(*inp)) inp++;
		if (*inp != commentChar && *inp != '\0') {
		    error("Illegal input on file definition line");
		    }
		}
	    }
	else if (state == outfile) {
	    /* continuation line skip white space */
	    int j;
	    if (*outbuf != '\0') strcat(outbuf, "\\");
	    while (isspace(*inp)) inp++;
	    ready2out(lidx, inp);
	    }
	fgets(inbuf, sizeof(inbuf), inall);
	{ register int i; 
	    if (inbuf[i=strlen(inbuf)-1] == '\n') inbuf[i] = '\0'; 
	    }
        }

    closeAllFiles();
    exit(0);
}
