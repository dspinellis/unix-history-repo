/*
 * Linker main program that controls the linking process.
 */

#include "ilink.h"

#define MAXNAME  256		/* maximum length of file name */

FILE *infile;			/* input file (.u1 or .u2) */
FILE *outfile;			/* interpreter code output file */
FILE *dbgfile;			/* debug file */
char inbuf[BUFSIZ];		/* buffer for input file */
char inname[MAXNAME];		/* input file name */
char outname[MAXNAME];		/* output file name */
char icnname[MAXNAME];		/* icon source file name */
char dbgname[MAXNAME];		/* debug file name */
char ixhdr[50];			/* header for directly executable .u files */
char *iconx = "/bin/echo iconx path not in";		/* pathname of iconx */
int  hdrloc;			/* location to place hdr at */
struct lfile *lfiles;		/* List of files to link */

int line = 0;			/* current source program line number */
char *file = NULL;		/* current source program file */
int fatalerrs = 0;		/* number of errors encountered */
int Dflag = 0;			/* debug flag */

char *pname;			/* name of program that is running */
char **filep;			/* name of current input file */

main(argc, argv)
int argc;
char **argv;
   {
   register int i;
   extern char *maknam(), *maknam2();
   char *p, *getenv();
   struct lfile *lf,*lfls;

   pname = argv[0];
   meminit(argc, argv);	/* Note that meminit also processes arguments. */

   /*
    * Phase I: load global information contained in .u2 files into
    *  data structures.
    *
    * The list of files to link is maintained as a queue with lfiles
    *  as the base.  lf moves along the list.  Each file is processed
    *  in turn by forming .u2 and .icn names from each file name, each
    *  of which ends in .u1.  The .u2 file is opened and globals is called
    *  to process it.  When the end of the list is reached, lf becomes
    *  NULL and the loop is terminated, completing phase I.  Note that
    *  link instructions in the .u2 file cause files to be added to list
    *  of files to link.
    */
   if (!(lf = lfiles))
	exit(0);	
   while (lf) {
      filep = &(lf->lf_name);
      maknam2(inname, *filep, ".u2");
      maknam(icnname, *filep, ".icn");
      infile = fopen(inname, "r");
      if (infile == NULL) {
         fprintf(stderr, "%s: cannot open %s\n", pname, inname);
         exit(1);
         }
      setbuf(infile, inbuf);
      globals(i);
      fclose(infile);
      lf = lf->lf_link;
      }

   /* Phase II: resolve undeclared variables and generate code. */

   /*
    * Open the output file.  If no file was named with -o, form the
    *  name from that of the first input file named.
    */
   if (!outname[0])
      maknam(outname, lfiles->lf_name, "");
   outfile = fopen(outname, "w");
   if (outfile == NULL) {
      fprintf(stderr, "%s: cannot create %s\n", pname, outname);
      exit(1);
      }
   setbuf(outfile, NULL);

   /*
    * Form the #! line.
    */
#ifdef DIREX
   sprintf(ixhdr,"#!%s\n",iconx);
   hdrloc = strlen(ixhdr);	/* point past end of #! line */
   if (hdrloc > 32)
       syserr("interpreter pathname too long--see section 3.1 of installation document");
#else
   {
   /*
    * Direct execution of icode files is not available.  Open HDRFILE,
    *  which contains the start-up program and copy it to the output
    *  file.  Then, set up for an fseek by setting hdrloc to the byte
    *  past the end of the start-up program and the #! line.
    */
   int hfile, hsize;
   char hdrdat[MAXHDR];
   
   sprintf(ixhdr,"#!iconx\n");
   hfile = open(HDRFILE,0);
   if (hfile == -1) {
      fprintf(stderr,"Can't open linker header file %s\n",HDRFILE);
      exit(1);
      }
   hsize = read(hfile,hdrdat,MAXHDR);
   fwrite(hdrdat,sizeof(char),hsize,outfile);
   fseek(outfile,(long)MAXHDR,0);
   hdrloc = MAXHDR + strlen(ixhdr);
   }
#endif DIREX
   /*
    * Put the #! line in the file and seek past it and possibly the
    *  start-up program, and leave space for the icode file header.
    */
   genheader();
   fseek(outfile, (long)(hdrloc + sizeof(struct header)), 0);

   /*
    * Open the .ux file if debugging is on.
    */
   if (Dflag) {
      maknam(dbgname, lfiles->lf_name, ".ux");
      dbgfile = fopen(dbgname, "w");
      if (dbgfile == NULL) {
         fprintf(stderr, "%s: cannot create %s\n", pname, dbgname);
         exit(1);
         }
      setbuf(dbgfile, NULL);
      }

   /*
    * Loop through input files and generate code for each.
    */
   lfls = lfiles;
   while (lf = getlfile(&lfls)) {
      filep = &(lf->lf_name);
      maknam2(inname, *filep, ".u1");
      maknam(icnname, *filep, ".icn");
      infile = fopen(inname, "r");
      if (infile == NULL) {
         fprintf(stderr, "%s: cannot open %s\n", pname, inname);
         exit(1);
         }
      setbuf(infile, inbuf);
      gencode();
      fclose(infile);
      }
   gentables();	/* Generate record, field, global, global names,
		     static, and identifier tables. */
   if (fatalerrs > 0)
      exit(1);
   exit(0);
   }

/*
 * maknam - makes a file name from prefix and suffix.
 *
 * Uses only the last file specification if name is a path,
 * replaces suffix of name with suffix argument.
 */
char *maknam(dest, name, suffix)
char *dest, *name, *suffix;
   {
   register char *d, *pre, *suf;
   char *mark;

   d = dest;
   pre = name;
   suf = suffix;
   mark = pre;
   while (*pre) 		/* find last slash */
      if (*pre++ == '/')
         mark = pre;
   pre = mark;
   mark = 0;
   while (*d = *pre++)		/* copy from last slash into dest */
      if (*d++ == '.')		/*   look for last dot, too */
         mark = d - 1;
   if (mark)			/* if no dot, just append suffix */
      d = mark;
   while (*d++ = *suf++) ;	/* copy suffix into dest */
   return (dest);
   }

/*
 * maknam2 - makes a file name from prefix and suffix.
 *
 * Like maknam, but leaves initial pathname component intact.
 */
char *maknam2(dest, name, suffix)
char *dest, *name, *suffix;
   {
   register char *d, *pre, *suf;
   char *mark;

   d = dest;
   pre = name;
   suf = suffix;
   mark = 0;
   while (*d = *pre++) {
      if (*d == '/')
         mark = 0;
      if (*d++ == '.')		/*   look for last dot, too */
         mark = d - 1;
      }
   if (mark)			/* if no dot, just append suffix */
      d = mark;
   while (*d++ = *suf++) ;	/* copy suffix into dest */
   return (dest);
   }

/*
 * syserr - issue error message and die.
 */
syserr(s)
char *s;
   {
   fprintf(stderr, "%s\n", s);
   exit(1);
   }

/*
 * warn - issue a warning message.
 */
warn(s1, s2, s3)
char *s1, *s2, *s3;
   {
   fprintf(stderr, "%s: ", icnname);
   if (line)
      fprintf(stderr, "%d: ", line);
   if (s1)
      fprintf(stderr, "\"%s\": ", s1);
   if (s2)
      fprintf(stderr, "%s", s2);
   if (s3)
      fprintf(stderr, "%s", s3);
   fprintf(stderr, "\n");
   }

/*
 * err - issue an error message.
 */

err(s1, s2, s3)
char *s1, *s2, *s3;
   {
   fprintf(stderr, "%s: ", icnname);
   if (line)
      fprintf(stderr, "%d: ", line);
   if (s1)
      fprintf(stderr, "\"%s\": ", s1);
   if (s2)
      fprintf(stderr, "%s", s2);
   if (s3)
      fprintf(stderr, "%s", s3);
   fprintf(stderr, "\n");
   fatalerrs++;
   }
