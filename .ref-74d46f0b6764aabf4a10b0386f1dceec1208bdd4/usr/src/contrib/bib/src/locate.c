#ifndef lint
static char sccsid[] = "@(#)locate.c	2.6	%G%";
#endif not lint
#

# include   "stdio.h"
# include   "streams.h"
# include   "ctype.h"
# define    maxrefs      1000

struct reftype{
    char reffile[maxstr];
    long int start, length;
    };

char *calloc();
char *rindex();
char *stripkeys();
int   fetchref();

/*  locate(keys, name, max_klen, common):
        Returns a string containing all references pointed to by name
        that contain all keys in keys.  Common is name of common word file.
    Pointer returned comes from calloc.  Use free to return storage.
    NB A zero length string returned if nothing is found.
       A NULL pointer indicates an error accessing the file "name".
*/
int fflag;	/* set if want the reference string to have the file name*/
char *locate(keys,name,max_klen,common)
char *keys, *name, *common;
int  max_klen;          /* max key length */
{   static  char oldname[maxstr] = "";  /* oldname is name of stream index */
    static  FILE *index = NULL;
    static  long int i_size;            /* size of index                   */
    static  char oldtext[maxstr];       /* oldtext is the path to stream   */
    static  FILE *text = NULL;		/*  text.  if it is a relative     */
    static  int  pathlen;		/*  path, it is relative to index  */
					/*  directory.                     */
					/* oldname[0..pathlen-1] is index  */
					/*  directory                      */
    int  len;
    char key[maxstr];                   /* refs[i] is a line of index for  */
    struct reftype  refs[maxrefs];      /* all keys up to key              */

    int  refcnt, copied, comp;          /* refcnt = # of refs               */
                                        /* copied = # of refs copied        */
                                        /* comp   = # of refs compared      */
    struct reftype ref;
    char   str[maxstr];
    int    more;

    long int ans;
    int i,j;
    unsigned total;
    char *allrefs, *next;               /* all refs (separated by null line)*/
    char *p;

    /*  open index */
        if  (strcmp(oldname,name)!=0)
        {   if (index) fclose(index);
            if (text) fclose(text);
            strcpy(oldname,name);
            strcpy(oldtext,"");
            /*  determine pathlen   */
                p= rindex(oldname, '/');
                if      (p!=NULL)           pathlen= p-oldname+1;
                else                        pathlen= 0;

            index= fopen(oldname,"r");
            if (index==NULL)
            {   fprintf(stderr, "locate: cannot open %s\n", oldname);
		strcpy(oldname, "");
                return(NULL);
            }
            else
            {   fseek(index,0L,2);     /*  seeks last newline      */
                i_size= ftell(index);
            }

        }

    /*  load references to first key  */
        keys= stripkeys(keys,key, max_klen, common);
	if (*key==NULL)
	{ fprintf(stderr,"locate: no keys for citation\n");
	  allrefs = (char *) calloc(1, sizeof (char));
	  if (allrefs==NULL)
	  {  fprintf(stderr, 
	       "locate: insufficient space for references\n");
	     exit(1);
	  }
	  *allrefs= NULL;
	  return(allrefs);
	}
        len= strlen(key);
        strcat(key," ");
        alpha_seek(index, key, i_size, 0);
        key[len]= NULL;                     /*  strip blank off */

        refcnt= 0;
        fscanf(index,"%s ", str);
        if (strcmp(str,key) == 0)
        {   str[0]= NULL;
            while (refcnt < maxrefs && fetchref(index, str, &ref) )
            {   refs[refcnt]= ref;
                refcnt++;
            }
        }

        if (refcnt==maxrefs)
            fprintf(stderr,
		"locate: first key (%s) matched too many refs\n", key);

    /*  intersect the reference sets for remaining keys with first set */
        while (*keys!=NULL)
        {   keys= stripkeys(keys, key, max_klen, common);
            if (*key==NULL) continue;

            len= strlen(key);
            strcat(key," ");
            alpha_seek(index, key, i_size, 0);
            key[len]= NULL;

            fscanf(index,"%s ", str);
            if (strcmp(str,key) != 0)  refcnt= 0;   /*  no matching refs */

            copied= 0; comp= 0; more= fetchref(index, str, &ref);
            while (comp < refcnt && more)
            {   /*  ans= ref-refs[comp]    */
                    ans= strcmp(ref.reffile, refs[comp].reffile);
                    if (ans==0)     ans= ref.start-refs[comp].start;
                    if (ans==0)     ans= ref.length-refs[comp].length;
                if (ans<0)  more= fetchref(index, str, &ref);
                if (ans==0) { refs[copied]= refs[comp]; comp++; copied++;
                              more= fetchref(index, str, &ref);}
                if (ans>0)  comp++;
            }

            refcnt= copied;
        }

    total= 0;
    for (i=0; i<refcnt; i++) {
        total += refs[i].length+1;
        if (fflag){
	    total += strlen(refs[i].reffile) + 1;
        }
    }

    allrefs= (char *) calloc(total+1, sizeof (char));
    if (allrefs==NULL)
    {   fprintf(stderr, "locate: insufficient space for references\n");
	exit(1);
    }

    /* copy refs into allrefs */
        next= allrefs;
        for (i=0; i<refcnt; i++)
        {   /*  open text */
                if (strcmp(oldtext,refs[i].reffile) != 0)
                {   strcpy(oldtext,refs[i].reffile);
		    if (oldtext[0]=='/')
		    {   /* absolute path */
			strcpy(str,oldtext);
		    } else
		    {   /* relative name */
			strncpy(str, oldname, pathlen);  str[pathlen]= NULL;
			strcat(str, oldtext);
		    }
                    if (text) fclose(text);
                    text= fopen(str, "r");
                    if (text==NULL)
                    {   fprintf(stderr, "locate: cannot open %s\n", str);
			strcpy(oldtext, "");
                        return(NULL);
                    }
                }
            fseek(text, refs[i].start, 0);
	    if (fflag){
		strcat(next, refs[i].reffile);
		next += strlen(next);
		*next++ = '\n';
		*next = 0;
	    }
            for (j=0; j<refs[i].length; j++)    *next++ = getc(text);
            *next++ = '\n';
        }
        *next = NULL;
    return(allrefs);
}



/*  stripkeys(line,key,max_klen, common):
        assigns to key the first key in line
        and returns a pointer to the position following the key
*/
char *stripkeys(line,key,max_klen,common)
char *line, *key;
int  max_klen;
char *common;
{   char *p;

    do
    {   while (isspace(*line))   line++;

        p= key;
        while (*line!=NULL && !isspace(*line))
        {   *p++ = *line++;
        }
        *p= NULL;

        makekey(key, max_klen, common);
    }   while (*key==NULL && *line!=NULL);
    return(line);
}

/*  read a reference pair from stream into *ref.  if file not given,
    use oldfile. return 1 if pair found, 0 ow.
*/
int fetchref(stream, oldfile, ref)
FILE *stream;
char *oldfile;
struct reftype *ref;
{   char cntl;

    fscanf(stream, "%c", &cntl);
    if (cntl=='\n') {return (0);}
    if (cntl==':')  fscanf(stream, "%s", oldfile);
    strcpy(ref->reffile, oldfile);
    fscanf(stream, "%D/%D", &ref->start, &ref->length);
    return(1);
}
