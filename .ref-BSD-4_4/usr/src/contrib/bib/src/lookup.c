#ifndef lint
static char sccsid[] = "@(#)lookup.c	2.6	5/27/93";
#endif not lint
# include "stdio.h"
# include "streams.h"
# include "bib.h"

char *locate();

int     fflag =   0;        /*  print out file names                    */
int     Aflag =   0;        /*  print hits from All indexes 		*/
int     max_klen =   6;     /*  max length of keys                      */
char    INDEX[maxstr] =     /*  name of index file                      */
            INDXFILE;

int     argc;
char    **argv;

main(argcount,arglist)
int argcount;
char **arglist;
{   char *refs;
    char keys[maxstr];
    char *p,*q;
    char one_index[maxstr];
    int found;

    InitDirectory(BMACLIB,N_BMACLIB);
    InitDirectory(COMFILE,N_COMFILE);

    argc= argcount-1;
    argv= arglist+1;
    flags();

    /*  add SYSINDEX to search path.  all names are comma terminated */
	strcat(INDEX, ",");
	strcat(INDEX, SYSINDEX);
	strcat(INDEX, ",");

    while (fgets(keys,maxstr,stdin)!=NULL)
    {	found = 0;
	for (p = one_index, q = INDEX; *q != 0 ; q++)
	    if (*q == ',' )
	    {   *p = 0;
	        refs = locate(keys, one_index, max_klen, COMFILE);
		if( refs==NULL )
		{   fprintf(stderr,"%s removed from index list.\n", one_index);
		    /* delete this file name (shift remainder on top) */
			strcpy(q-strlen(one_index),q+1);
			q = q-strlen(one_index)-1;
		}
                if (refs!=NULL && *refs!=NULL)
		{
		    printf("%s", refs);
		    free(refs);
		    found = 1;
		    if (!Aflag) break;
		}
	        p = one_index;
	    }
	    else *p++ = *q;

        if (!found)  printf("No references found.\n");
    }
    exit(0);
}

# define    operand     (strlen(*argv+2)==0 ? (argv++,argc--,*argv) : *argv+2)

flags()
{
    char *p;
    for (; argc>0 && *argv[0]=='-';  argc--,argv++)
    {   switch ((*argv)[1])
        {   case 'l':   max_klen= atoi(operand);
                        break;
	    case 'f':	fflag++;
			break;
            case 'c':   strcpy(COMFILE, operand);
                        break;
	    case 'A':	Aflag++;
			break;
            case 'p':   strcpy(INDEX,operand);
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
