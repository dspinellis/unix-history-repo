/*
 *  Interpress utility - count the number of pages in a interpress file
 *
 *  Written for Xerox Corporation by Lee Moore & William LeFebvre
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * History:
 *	 2-sep-85 lee moore	created out of iptotext.c
 */

#ifdef vax11c
# include stdio
# include setjmp
# include ctype
# include "iptokens.h"
#else
# include <stdio.h>
# include <setjmp.h>
# include <ctype.h>
# include "iptokens.h"
#endif

jmp_buf next_file;

extern int errno;

main(argc, argv)

int  argc;
char *argv[];

{
    FILE *acctFile;
    int c,
	pageCount;
    char *login,
	 *host;
    extern int optind;
    extern char *optarg;

    login = "name";
    host = "host";

    while ((c = getopt(argc, argv, "cw:l:i:n:h:")) != EOF)
	switch (c) {
		case 'c':
		case 'w':
		case 'l':
		case 'i':
			break;

		case 'n':
			login = optarg;
			break;

		case 'h':
			host = optarg;
			break;

		default:
			printf("option '%c' not allowed\n");
	}

	pageCount = do_file(stdin);

	if( pageCount < 0 )
	    exit(2);


	if( login )
	    if( host )
		fprintf(stdout, "%d\t%s:%s\n", pageCount, host, login);
	    else
		fprintf(stdout, "%d\t%s\n", pageCount, login);

	exit(0);

}


/*
 * process one file
 */

do_file(file)
FILE *file;
{
# define  Buffsize	256
    char buff[Buffsize];
    char *ptr;
    int len;
    int bodyDepth;	/* how many bodies down we are */
    register int bodyCount,	/* how many bodies we have seen so far */
    		 val,
    		 byte;		/* has to be "int" for stdio EOF detection */
				/* stdio is a pile! */
    int hlen;

    hlen = strlen(IP_Header);

    /* for error recovery */
    if (setjmp(next_file) != 0)
    {
	return -1;
    }

    /* get the header */
    for (hlen = 0, ptr = buff; hlen < Buffsize; hlen++)
    {
	if ((*ptr++ = getnoeofc(file)) == ' ')
	    break;
    }
    *ptr = '\0';

    /* check the validity of the header */
    if (strcmp(buff, IP_Header) != 0)
    {
	fprintf(stderr, " (INVALID HEADER!)");
    }

    bodyDepth = 0;
    bodyCount = 0;

    /* main loop */
    while ((byte = getc(file)) != EOF)
    {
	if ((byte & 0200) == 0)
	{
	    /* a short number */
	    val = (byte << 8) + getnoeofc(file) - INTEGER_ZERO;
	}
	else
	{
	    /* something else */
	    switch(byte >> 5)
	    {
		case (SHORT_OP >> 5):
		    break;

		case (LONG_OP >> 5):
		    val = ((byte & 037) << 8) + getnoeofc(file);
		    if( val == OP_beginBody )
		    {
			bodyDepth++;
		    } 
		    else if( val == OP_endBody )
		    {
			bodyDepth--;

			/* is this a top level body? */
			if( bodyDepth == 0 )
			    bodyCount++;
		    }
		    break;

		case (SHORT_SEQUENCE >> 5):
		    len = getnoeofc(file);
		    eatBytes(file, len);
		    break;

		case (LONG_SEQUENCE >> 5):
		    len  =  getnoeofc(file) << 16;
		    len += (getnoeofc(file) << 8);
		    len += getnoeofc(file);
		    eatBytes(file, len);
		    break;
	    }
	}
    }

    return bodyCount - 1;	/* the preamble is an extra body */
}


/*
 * get a character
 */

getnoeofc(file)

FILE *file;

{
    register int val;

#ifdef vax11c
    val= getc(file);
    if ( feof(file) )
#else
    if ((val = getc(file)) == EOF)
#endif
    {
	fprintf(stderr, "Unexpected EOF!");
	longjmp(next_file, 1);
    }
    return(val);
}


/*
 * read some bytes from the input stream
 */

eatBytes(file, length)
FILE *file;
int length;

{
    register int count;

    count = length;

    while(count-- > 0)
    {
	(void) getnoeofc(file);
    }
}
