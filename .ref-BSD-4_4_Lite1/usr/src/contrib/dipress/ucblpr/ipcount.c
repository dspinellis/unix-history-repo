From moore.WBST@Xerox.ARPA Tue Sep 10 15:14:08 1985
Received: from ucbkim.ARPA by ucbdali.ARPA (5.5/4.48)
	id AA10859; Tue, 10 Sep 85 15:13:58 PDT
Received: from UCB-VAX.ARPA (ucbvax.ARPA) by ucbkim.ARPA (5.5/5.3)
	id AA00229; Tue, 10 Sep 85 14:28:49 PDT
Received: from Xerox.ARPA (xerox.arpa.ARPA) by UCB-VAX.ARPA (4.24/5.3)
	id AA01317; Tue, 10 Sep 85 14:23:25 pdt
Received: from Aurora.ms by ArpaGateway.ms ; 10 SEP 85 14:25:54 PDT
Date: 10 Sep 85 17:25:44 EDT (Tuesday)
From: moore.WBST@Xerox.ARPA
Subject: counting pages in a press file
To: sklower@BERKELEY
Cc: parmelee@CU-ARPA.CS.CORNELL.EDU
Message-Id: <850910-142554-4998@Xerox>
Status: R

Hi,

Here is the hack on iptotext that will count the pages and write them to
an accounting file.  I have tried it with both troff generated
interpress files and Star generated ones.  I don't really understand the
Berkeley spooling system (even after glancing at the manual) so I don't
know if this is what you really want.  What it DOES however, is to take
an ip master on the standard input and count the number of pages it has.
This information is written to the accounting file which might be named
on the command line.  What it doesn't do is copy the IP master to the
standard output or send the file to a printer.  I hope that this isn't a
necessary function...

lee

#!/bin/sh
# This is a shell archive, meaning:
# 1. Remove everything above the #!/bin/sh line.
# 2. Save the resulting text in a file.
# 3. Execute the file with /bin/sh (not csh) to create the files:
#	Makefile
#	getopt.c
#	ipf.c
# This archive created: Tue Sep 10 17:12:41 1985
export PATH; PATH=/bin:$PATH
if test -f 'Makefile'
then
	echo shar: over-writing existing file "'Makefile'"
fi
cat << \SHAR_EOF > 'Makefile'
#
# Copyright (c) 1984, 1985 Xerox Corp.
#
DESTINCLUDE=/usr/new/include
DESTBIN=../../../bin

ipf: ipf.o getopt.o
	cc -o ipf ipf.o getopt.o

ipf.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/ipnames.h
	cc -c -I$(DESTINCLUDE) ipf.c

lint: 
	lint -I$(DESTINCLUDE) ipf.c getopt.c
clean:
	rm -f ipf *.o

install: $(DESTBIN)/ipf

$(DESTBIN)/ipf: ipf
	install -c -s ipf $(DESTBIN)
SHAR_EOF
if test -f 'getopt.c'
then
	echo shar: over-writing existing file "'getopt.c'"
fi
cat << \SHAR_EOF > 'getopt.c'
/*
 * getopt - get option letter from argv
 *	This software is in the public domain
 *	Originally written by Henry Spenser at the U. of Toronto
 */

#include <stdio.h>

char	*optarg;	/* Global argument pointer. */
int	optind = 0;	/* Global argv index. */

static char	*scan = NULL;	/* Private scan pointer. */

extern char	*index();

int
getopt(argc, argv, optstring)
int argc;
char *argv[];
char *optstring;
{
	register char c;
	register char *place;

	optarg = NULL;

	if (scan == NULL || *scan == '\0') {
		if (optind == 0)
			optind++;
	
		if (optind >= argc || argv[optind][0] != '-' || argv[optind][1] ==
'\0')
			return(EOF);
		if (strcmp(argv[optind], "--")==0) {
			optind++;
			return(EOF);
		}
	
		scan = argv[optind]+1;
		optind++;
	}

	c = *scan++;
	place = index(optstring, c);

	if (place == NULL || c == ':') {
		fprintf(stderr, "%s: unknown option -%c\n", argv[0], c);
		return('?');
	}

	place++;
	if (*place == ':') {
		if (*scan != '\0') {
			optarg = scan;
			scan = NULL;
		} else {
			optarg = argv[optind];
			optind++;
		}
	}

	return(c);
}
SHAR_EOF
if test -f 'ipf.c'
then
	echo shar: over-writing existing file "'ipf.c'"
fi
cat << \SHAR_EOF > 'ipf.c'
/*
 *  Interpress utility - count the number of pages in a interpress file
 *
 *  Written for Xerox Corporation by Lee Moore & William LeFebvre
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * History:
 *	 2-sep-85 lee moore	created out of iptotext.c
 */

#ifdef vax11c
# include stdio
# include setjmp
# include ctype
# include "iptokens.h"
# include "ipnames.h"
#else
# include <stdio.h>
# include <setjmp.h>
# include <ctype.h>
# include "iptokens.h"
# include "ipnames.h"
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

    login = NULL;
    host = NULL;

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

    if (argc - optind == 1)
    {
	pageCount = do_file(stdin);

	if( pageCount < 0 )
	    exit(2);

	if( (acctFile = fopen(argv[optind], "a")) == NULL ) {
	    fprintf(stderr, "ipf: can't open acct file: %s\n", argv[optind]);
	    exit(2);
	}

	if( login )
	    if( host )
		fprintf(acctFile, "%d\t%s:%s\n", pageCount, host, login);
	    else
		fprintf(acctFile, "%d\t%s\n", pageCount, login);

	exit(0);
    }

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
SHAR_EOF
#	End of shell archive
exit 0


