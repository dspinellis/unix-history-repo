#ifndef lint
static char sccsid[] = "@(#)invert.c	2.6	%G%";
#endif not lint
#
/*  input:  records of lines, separated by blank lines
    output: key:file1 start/length ... start/length:file2 start/length ...
*/

# include "stdio.h"
# include "streams.h"
# include "bib.h"
# define isnull(x)  (*(x) == NULL)
# define makelow(c) ('A'<=(c) && (c)<='Z' ? (c)-'A'+'a' : c)

int     max_kcnt = 100;     /*  max number of keys                      */
int     max_klen =   6;     /*  max length of keys                      */
char    *ignore =           /*  string of line starts to ignore         */
            "CNOPVX";
char    *common =           /*  name of file of common words            */
            COMFILE;
char    *INDEX=             /*  name of output file                     */
            INDXFILE;

char    *bibtmpfile =          /*  name of temporary file                  */
            INVTEMPFILE;

int	silent = 0;	    /*  0 => statistics printed			*/
			    /*  1 => no statisitics printed		*/

char *sort_it =
        "sort -u +0 -1 +1 -2 +2n -3 +3n %s -o %s";
char sortcmd[maxstr];

int     argc;
char    **argv;

main(argcount,arglist)
int argcount;
char **arglist;
{   char            *filename;
    FILE            *input, *output;
    long int        start,length;
    char            word[maxstr];
    int             kcnt;
    char            tag_line[maxstr];

    long int	    records = 0;  /*  number of records read           */
    long int	    keys    = 0;  /*  number of keys read (occurences) */
    long int	    distinct;     /*  number of distinct keys          */
    long int	    shorten();

    strcpy(COMFILE, N_COMFILE);
    strcpy(BMACLIB, N_BMACLIB);

    argc= argcount-1;
    argv= arglist+1;
    mktemp(bibtmpfile);
    output= fopen(bibtmpfile,"w");

    for ( flags() ; argc>0 ; argc--, argv++ ,flags() )
    {   /* open input file              */
            filename=   *argv;
            input=      fopen(filename,"r");
            if (input==NULL)
            {   fprintf(stderr, "invert: error in open of %s\n", filename);
                continue;
            }
            start=      0L;
            length=     0L;

        for(;;) /* each record  */
        {   /* find start of next record (exit if none)     */
                start= nextrecord(input,start+length);
                if (start==EOF)   break;
            records++;
	    kcnt= 0;
            length= recsize(input,start);
            sprintf(tag_line, " %s %d %d\n", filename, start, length);

            while (ftell(input) < start+length && kcnt < max_kcnt)
            {   getword(input,word,ignore);
                makekey(word,max_klen,common);
                if (!isnull(word))
                {   fputs(word,output); fputs(tag_line,output);
                    kcnt++; keys++;
                }
            }
        }
        fclose(input);
    }
    fclose(output);

    sprintf(sortcmd, sort_it, bibtmpfile, bibtmpfile);
    system(sortcmd);

    distinct = shorten(bibtmpfile,INDEX);
    if( silent == 0 )
	fprintf(stderr,
	    "%d documents   %d distinct keys  %d key occurrences\n",
	    records, distinct, keys);
    exit(0);
}



/*  Flag    Meaning                             Default
    -ki     Keys per record                     100
    -li     max Length of keys                  6
    -%str   ignore lines that begin with %x     CNOPVX
            where x is in str
            str is a seq of chars
    -cfile  file contains Common words          /usr/new/lib/bib/common
            do not use common words as keys
    -pfile  name of output file                 INDEX
    -s	    do not print statistics		statistics printed
*/

# define    operand     (strlen(*argv+2)==0 ? (argv++,argc--,*argv) : *argv+2)

flags()
{
    char *p;
    for (; argc>0 && *argv[0]=='-';  argc--,argv++)
    {   switch ((*argv)[1])
        {   case 'k':   max_kcnt= atoi(operand);
                        break;
            case 'l':   max_klen= atoi(operand);
                        break;
            case 'c':   common=  operand;
                        break;
            case '%':   ignore=  *argv+2;
                        break;
            case 'p':   INDEX=  operand;
                        break;
	    case 's':	silent= 1;
			break;
	    case 'd':
		p = &argv[0][2];
		if (!p) { 
			argv++;
			p = &argv[0][0];
		}
		strreplace(COMFILE, BMACLIB, p);
		strcpy(BMACLIB, p);
		break;
            default:    fprintf(stderr, "unknown flag '%s'\n", *argv);
        }
    }
}


/*  shorten(inf,outf): file "inf" consists of lines of the form:
        key file start length
    sorted by key and file.  replace lines with the same key
    with one line of the form:
        key:file1 start/length ... start/length:file2 start/length ...
    rename as file "outf"
    returns number of lines in output
*/
long shorten(inf,outf)
char *inf, *outf;
{   FILE *in, *out;
    char line[maxstr];
    char key[maxstr],  newkey[maxstr],
         file[maxstr], newfile[maxstr];
    long int start, length;
    long int lines = 0;

    in=  fopen(inf, "r");
    out= fopen(outf, "w");
    if (in==NULL || out==NULL)
    {   fprintf(stderr, "invert: error in opening file for compression\n");
        return(0);
    }

    getline(in,line);
    sscanf(line,"%s%s%d%d", key, file, &start, &length);
    fprintf(out, "%s :%s %d/%d", key, file, start, length);
    for ( getline(in, line) ; !feof(in);  getline(in, line))
    {   sscanf(line,"%s%s%d%d", newkey, newfile, &start, &length);
        if (strcmp(key,newkey)!=0)
        {   strcpy(key, newkey);
            strcpy(file, newfile);
            fprintf(out, "\n%s :%s %d/%d",  key, file, start, length);
	    lines++;
        }
        else if (strcmp(file,newfile)!=0)
        {   strcpy(file,newfile);
            fprintf(out, ":%s %d/%d", file, start, length);
        }
        else
            fprintf(out, " %d/%d", start, length);
    }
    fprintf(out, "\n");
    lines++;

    fclose(in); fclose(out);
    unlink(inf);
    return (lines);
}
