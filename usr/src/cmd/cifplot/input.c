/*******************************************************************
*                                                                  *
*    File: CIFPLOT/input.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/


#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "scanner.h"

IMPORT string Concat();
IMPORT alloc();
IMPORT PrintError();
IMPORT expand();

extern struct ErrorType *ErrorList;	/* Need to print line if there
					 * is an error		*/
int bad,LineLevel;

InitInput(b,n,v)
int b,n;
char **v;
{
   CharNo = 0;
   LineNo = 1;

   /* Allocate space for input buffer */
   in_buf_size = InBufSize;
   in_buf = (char *) alloc(in_buf_size);
   in_store = (char *) alloc(in_buf_size);
   
   argv = v;
   argb = b;
   argc = n;

   if (argb<argc) {
	/* Open first cif file */
	if (NULL == (in_file[0] = fopen(argv[argb],"r"))) {
		perror(argv[argb]);
		exit(1);
		}
	CurrentFile = FileName[0] = Concat(argv[argb],0);
	return(1);
        }
     else {
	/* Error if no file to read */
	fprintf(stderr,"Usage: %s options file1.cif file2.cif ...\n",argv[0]);
	exit(0);
	}
   }

Next()
/* Next gets the next file to be read,
 * and returns 0. It returns 1 when there
 * are no files left.
 */
{
    fclose(in_file[FStackPtr]);
    Free(FileName[FStackPtr]);
    /* Check to see that no files are stacked up. If there are
     * some open them before reading next file */
    if (FStackPtr == 0) {
    	if (++argb < argc) {
		if (NULL == (in_file[0] = fopen(argv[argb],"r"))) {
			fprintf(stderr,"Can't read %s\n",argv[argb]);
			perror("");
			exit();
			}
		if(debug>1) fprintf(stderr,"Begin next file: %s\n",argv[argb]);
		CurrentFile = FileName[0] = Concat(argv[argb],0);
		return(0);
		}
    	    else {
		return(1);
		}
	}
      else {
	FStackPtr--;
	CurrentFile = FileName[FStackPtr];
	if(debug>1) fprintf(stderr,"Finnished with file %s, resume file %s\n",FileName[FStackPtr+1],CurrentFile);
	return(0);
	}
   }

Include(s)
char *s;
/* Open file 's', stack current file */
{
   if (debug>1) fprintf(stderr,"File switch from %s to %s\n",CurrentFile,s);
   FStackPtr++;
   if (FStackPtr < FStackSize) {
	if (NULL == (in_file[FStackPtr] = fopen(s,"r"))) {
		FStackPtr--;
		fprintf(stderr,"Unable to read %s which was called from %s",s,CurrentFile);
		fprintf(stderr,"---Include command ignored\n");
		perror("");
		}
	   else {
		CurrentFile = FileName[FStackPtr] = Concat(s,0);
		}
	}
      else {
	 fprintf(stderr,"Can't open %s---Too many files already open\n",s);
	 FStackPtr--;
	 }
   }

input()
/* Return next input character */
{
   char ch;
   int i;
   
   if(EOF == (ch = getc(in_file[FStackPtr]))) ch=0;
   if (0 <= CharNo && CharNo < in_buf_size) in_buf[CharNo] = ch;
	else 
	   if (CharNo >= in_buf_size) {
	      
	      /* Buffer is too small - Double its size */
	       if(debug>1) fprintf(stderr,"Increase input buffer size to %d\n",2*in_buf_size);
	       in_buf_size = in_buf_size*2;
	       in_buf = (char *) expand(in_buf,in_buf_size);
	       in_store = (char *) expand(in_store,in_buf_size);
	       in_buf[CharNo] = ch;
	       }
   CharNo++;
   if (ch == '\n') {
	if ((list || (ErrorList != NIL)) && (! bad)) {
		/* Print the line if we are in listing mode or
		 * there is an error on that line 	*/
		fprintf(stderr,"%6d    ",LineNo);
		for(i=0; i<CharNo; i++) if (i < in_buf_size) fprintf(stderr,"%c",in_buf[i]);
		OldLength = CharNo;
		maxlines = LineNo;
		bad++;
		}
	if(!bad)  Store();
	LineNo++;
	CharNo = 0;
	PrintError();
	if(--bad < 0) bad = 0;
	}
   return(ch);
   }

unput(c)
char c;
/* Put 'c' back into the input stream */
{
   CharNo--;
   if (c == '\n') {
       LineNo--;
       /* bad indicates how many lines have been put back */
       bad++;
       }
   ungetc(c,in_file[FStackPtr]);
   return;
   }

Store()
/* This function saves the last input line */
{
    char *q;
    LineLevel = LineNo;
    OldLength = CharNo;
    q = in_store;
    in_store = in_buf;
    in_buf = q;
    return;
    }


PrintLine(n)
int n;
/* If line n is currently available 'PrintLine' will print it */
{
   int i;
   if((LineLevel == n) && (0 < n)) {
	fprintf(stderr,"%6d    ",n);
	for(i=0; i<OldLength;i++) fprintf(stderr,"%c",in_store[i]);
	bad++;
	maxlines = n;
	LineLevel = -1;
	}
   }
