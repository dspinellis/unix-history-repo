#ifndef lint
static char sccsid[] = "@(#)streams.c	2.4	%G%";
#endif not lint
#

# include "stdio.h"
# include "streams.h"
# include "ctype.h"

/*  getword(stream,p,ignore):
        read next sequence of nonspaces on current line into *p.
    null if no more words on current line.
    %x (x in ignore) terminates line and any following non-blank lines that
       don't begin with '%'
    all words of the form %a are returned as null.
    *p is a null terminated string (char p[maxstr]).
*/
getword(stream,p,ignore)
FILE *stream;
char *p, *ignore;
{   char c;
    char *oldp, *stop;
    long save;

    oldp= p;
    stop= p+maxstr-1;
    do{ c= getc(stream);
    }   while (isspace(c) && c!='\n');

    while (!isspace(c))
    {   *p= c;
        if (p < stop)  p++;
        c= getc(stream);
    }
    *p= NULL;

    if (oldp[0]=='%')
    {   oldp[0]= NULL;
        if (index(ignore, oldp[1]) != NULL)
	{   do{ while (c!='\n') c=getc(stream);
                save= ftell(stream);
                c= getc(stream);
	    }   while (c!= EOF && !isspace(c) && c!='%');
            pos(save);
	}
    }
}



/*  recsize(stream,start):
    returns length of record beginning at start
    (record ends at blank line or eof)
    assumes and retains stream positioned at start
*/
long int recsize(stream,start)
FILE *stream;
long int start;
{   char c;                 /*  length = # of chars from start to beginning */
    long int length;        /*  of current line.  c in current line.        */
    int nonspaces;          /*  nonspaces = # of nonspaces in current line. */

    nonspaces= 0;
    c= getc(stream);
    length= 0L;

    while ( (c!='\n' || nonspaces!=0) && c!=EOF)
    {   if      (c=='\n')
        {   length= ftell(stream)-start;
            nonspaces= 0;
        }
        else if (!isspace(c))    nonspaces++;

        c= getc(stream);
    }

    pos(start);
    return(length);
}


/*  nextrecord(stream,x): seeks in stream for first non-blank line
        at or after char x in stream. seeks to eof if x is past last record.
        x is the index of a character in the file (not eof).
    returns position in stream.  (returns EOF, if seeks to EOF)
*/
long int nextrecord(stream,x)
FILE *stream;
long int x;
{   long int start;         /*  position of the beginning of the line  */
    char c;                 /*      containing c                       */

    pos(x);
    start= x;
    /*  find start of first non-blank record        */
        c= getc(stream);
        for(;;)
        {   if      (c=='\n')          {start= ftell(stream); c= getc(stream);}
            else if (c=='#')           while (c!='\n') c=getc(stream);
            else if (!isspace(c))      break;
            else                       c= getc(stream);
        }

    if (feof(stream))   { pos(start);  start= EOF;  }
    else                pos(start);
    return(start);
}

/*  nextline(stream,x): seeks in stream after first newline at or after
        char x in stream. seeks to eof if x is in last line.
        x is the index of a character in the file (not eof).
    returns position in stream
*/
long int nextline(stream,x)
FILE *stream;
long int x;
{   pos(x);
    while (getc(stream)!='\n') ;
    return(ftell(stream));
}


/*  printline(stream): copies stream up to a newline
*/
printline(stream)
FILE *stream;
{   char c;
    while ((c=getc(stream)) != '\n' && c!=EOF)  putchar(c);
    putchar('\n');
}

/*  getline(stream,p):  store in *p next chars in stream up to \n
        advance stream past \n.
    limit of  maxstr-1 chars may be stored at p.
*/
getline(stream,p)
FILE *stream;
char *p;
{   char *stop;
    stop= p+maxstr-1;
    while ( (*p= getc(stream)) != '\n' && *p!=EOF)
        if (p<stop)    p++;
    *p= NULL;
}

/* replace string old at the head of subj by new */
strreplace(subj, old, new)
	char *subj, *old, *new;
{
	char buf[128];
	int lg;
	strcpy(buf, &subj[strlen(old)]);
	strcpy(subj, new);
	strcat(subj, buf);
}
