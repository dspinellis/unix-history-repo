#ifndef lint
static char sccsid[] = "@(#)alpha.seek.c	2.3	5/27/93";
#endif not lint
#

# include "stdio.h"
# include "ctype.h"
# include "streams.h"
# define  nexttry           ((high+low)/2)

/*  alpha_seek(stream, word, s_size, fold)
        seeks the first line in stream that is at least word.
    assumes that stream is a sorted file of lines.  (last char must be \n)
    if fold, assumes that word is lowercase and folds stream to lowercase.
    s_size = size of stream
    returns 1 if word = line, 0 o.w.
*/
int alpha_seek(stream, word, s_size, fold)
FILE *stream;
char *word;
long int s_size;
int  fold;
{   long int high, low, mid;    /*  point to beginning of a line in stream  */
    int      ans;               /*  line(low) < word <= line(high)          */
    char     line[maxstr];


    /*  initialize low (return if first line >= word)       */
        low= 0L;
        pos(low); getline(stream, line);
        if (fold) foldline(line);
        ans= strcmp(line,word);

        if ( ans >= 0)
        {   pos(low);   return(ans==0); }

    /*  initialize high to "line" after last line           */
        high= s_size;

    mid= nextline(stream, nexttry );
    while (mid < high )
    {   getline(stream,line);
        if (fold) foldline(line);
        if (strcmp(line,word) < 0)    low=  mid;
        else                          high= mid;
        mid= nextline(stream, nexttry );
    }

    /* linear search from low to high   */
        low= nextline(stream,low);
        for(;;)
        {   if (low>=high)      break;

            getline(stream,line);
            if (fold) foldline(line);
            ans=strcmp(line,word);

            if (ans>=0)         break;
            low= ftell(stream);
        }

    pos(low);
    if (low==high)  return(0);
    else            return(ans==0);
}


/*  foldline(p):    change all uppercase to lowercase in string p
*/
foldline(p)
char *p;
{   for (; *p!=NULL;  p++)
    {   if (isupper(*p))    *p = tolower(*p);
    }
}
