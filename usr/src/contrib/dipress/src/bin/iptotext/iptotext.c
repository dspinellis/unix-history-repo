/*
 *  Interpress utility
 *
 *  Written for Xerox Corporation by William LeFebvre
 *
 *  7-June-1984
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * HISTORY
 * 20-Mar-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Changed the output comment for sequencePackedPixel to print out the
 *	number of bits per sample and the number of samples per scan line.
 *
 * 13-Jan-86  lee at Xerox, WRC
 *	Changed a call to strmpn to strncmp.
 *
 * 01-Dec-85  lee at Xerox, WRC
 *	Linted
 *
 * 11-oct-85 ed flint
 *	don't worry about version # in Interpress header
 *	look only for 'Interpress/Xerox/'
 *
 * 23-may-85 ed flint
 *	fixed bug in print_string_from_file & print_string
 *	that would strip off high byte of ch in isprint
 *
 * 8-apr-85  ed flint
 *	add -d option to dump pixel arrays
 *	conditional compilation for vax-11c (vms)
 */

/*
 *  iptotext - convert an encoded interpress file into readable text
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

int hlen;
jmp_buf next_file;
FILE *file;
FILE *output;

int errno;		/* imported value */

char *op_name();
int dump_pixel= 0;

main(argc, argv)

int  argc;
char *argv[];

{
    int arg;
    int temp;
    char *outputname;

    hlen = strlen(IP_Header);

    outputname= 0;

    /* look for options */
    for ( arg= 1; arg < argc; arg++ )
    {
	if ( argv[arg][0] == '-' )
	{
	    switch(argv[arg][1])
	    {
	        case('o'):
	        case('O'):
		    if ( strlen(argv[arg]) > 2 )
		       outputname= &(argv[arg][2]);
		    else
		       outputname= argv[++arg];
		    break;

	        case('d'):
	        case('D'):
		    dump_pixel= 1;
		    break;

		default:
		    fprintf(stderr,"invalid option %s\n",argv[arg]);
		    break;
	    }
	}
	else
	{
	    break;
	}
    }

	/* open the output file */
    if ( outputname != 0 )
    {
	if ((output = fopen(outputname, "w")) == NULL)
    	{
	    /* save errno value */
	    temp = errno;
	    fputs("iptotext: ", stderr);
	    errno = temp;
	    perror(outputname);
	    exit(1);
	}
    }
    else
    {
	/* default output -- use stdout */
	output = stdout;
    }

    if (argc == 1)
    {
	file = stdin;
	do_file();
	(void) putc('\n', output);
    }

    /* make the begin- and end-block names be upper case */
    op_names[OP_beginBlock] = "BEGIN";
    op_names[OP_endBlock]   = "END";

    for ( ; arg < argc; arg++)		/* assume arg is index of first file name */
    {
	if ((file = fopen(argv[arg], "r")) == NULL)
	{
	    perror(argv[arg]);
	    continue;		/* on to the next file */
	}

	/* print pretty banner */
	fprintf(output, "(File: \"%s\")\n", argv[arg]);

	do_file();
	(void) fclose(file);
	(void) putc('\n', output);
    }
}

do_file()

{
# define  Buffsize	256
    unsigned char buff[Buffsize];
    unsigned char *ptr;
    int val;
    int len;
    int byte;		/* has to be "int" for stdio EOF detection */
			/* stdio is a pile! */

    /* for error recovery */
    if (setjmp(next_file) != 0)
    {
	return;
    }

    /* get the header */
    for (hlen = 0, ptr = buff; hlen < Buffsize; hlen++)
    {
	if ((*ptr++ = getnoeofc(file)) == ' ')
	    break;
    }
    *ptr = '\0';

    /* display the header */
    fputs("Header: ", output);
    print_string(buff);

    /* check the validity of the header */
    if (strncmp((char *)buff, IP_Header, 17) != 0)
    {
	fprintf(output, " (INVALID HEADER!)");
    }
    (void) putc('\n', output);

    /* main loop */
    while ((byte = getc(file)) != EOF)
    {
	if ((byte & 0200) == 0)
	{
	    /* a short number */
	    val = (byte << 8) + getnoeofc(file) - INTEGER_ZERO;
	    fprintf(output, "%d\n", val);
	}
	else
	{
	    /* something else */
	    switch(byte >> 5)
	    {
		case (SHORT_OP >> 5):
		    fprintf(output, "%s\n", op_name(byte & 037));
		    break;

		case (LONG_OP >> 5):
		    val = ((byte & 037) << 8) + getnoeofc(file);
		    fprintf(output, "%s%s\n", op_name(val),
			val == OP_beginBlock || val == OP_endBlock ?
			    " (block)" : "");
		    break;

		case (SHORT_SEQUENCE >> 5):
		    len = getnoeofc(file);
		    fputs("> ", output);
		    do_sequence(byte & 037, len);
		    break;

		case (LONG_SEQUENCE >> 5):
		    len  =  getnoeofc(file) << 16;
		    len += (getnoeofc(file) << 8);
		    len += getnoeofc(file);
		    fputs(">>", output);
		    do_sequence(byte & 037, len);
		    break;
	    }
	}
    }
}

do_sequence(type, length)

int type;
int length;

{
    int val;
    int val2;

    switch(type)
    {
	case sequenceAdaptivePixelVector:
	    fprintf(output, "Adaptive Pixel Vector: (%d words) [\n", length/2);
	    print_words_from_file(file, length);
	    fputs("]\n", file);
	    break;

	case sequenceComment:
	    fputs("Comment: ", output);
	    print_string_from_file(file, length);
	    (void) putc('\n', output);
	    break;

	case sequenceCompressedPixelVector:
	    fprintf(output, "Compressed Pixel Vector: (%d words) [\n", length/2);
	    print_words_from_file(file, length);
	    fputs("]\n", file);
	    break;

	case sequenceContinued:
	    fprintf(output, "Continuing last sequence: ");
	    break;

	case sequenceIdentifier:
	    fprintf(output, "Identifier: ");
	    iocopy(length);
	    fputs("\n", output);
	    break;

	case sequenceInsertFile:
	    fputs("Insert file: ", output);
	    print_string_from_file(file, length);
	    (void) putc('\n', output);
	    break;

	case sequenceInteger:
	    val = getint(length);
	    fprintf(output, "Integer: %d\n", val);
	    (void) putc('\n', output);
	    break;

	case sequenceLargeVector:
#ifdef notdef
	    val = getnoeofc(file);
	    fprintf(output, "Large Pixel Vector: (%d words of %d bytes) [\n"
		(length - 1) / val, val);
#endif
	    break;

	case sequencePackedPixelVector:
	    val = getint(2);
	    val2 = getint(2);
	    fprintf(output, "Packed Pixel Vector:  (%d + 2 = %d words, %d bit(s) per sample, %d sample(s) per scanline) [\n",
			length/2 -2, length/2, val, val2);
	    print_words_from_file(file, length - 4);
	    fputs("]\n", output);
	    break;

	case sequenceRational:
	    length >>= 1;
	    val = getint(length);
	    val2 = getint(length);
	    fprintf(output, "Rational: %d/%d ", val, val2);
	    if (val2 != 0)
	    {
		fprintf(output, "(%f)\n", (float)val / (float)val2);
	    }
	    else
	    {
		fputs("(???)\n", output);
	    }
	    break;

	case sequenceString:
	    fputs("String: ", output);
	    print_string_from_file(file, length);
	    (void) putc('\n', output);
	    break;
    }
}

iocopy(length)

int length;

{
    int byte;

    while(length-- > 0)
    {
	byte = getnoeofc(file);
	(void) putc(byte, output);
    }
}

getint(length)

int length;

{
    int val;

    val = getnoeofc(file);

    if ((val & 0x80) != 0) {
	/* this is a negative number -- extend the sign */
	val |= (-1 & ~(0xFF));
    }

    while (--length > 0) {
	val <<= 8;
	val |= getnoeofc(file);
    }

    return(val);
}

char *op_name(op_code)

int op_code;

{
    static char nbuff[10];

    if (op_names[op_code] == NULL)
    {
	(void) sprintf(nbuff, "--Unknown op: %d--", op_code);
	return(nbuff);
    }
    else
    {
	return(op_names[op_code]);
    }
}

getnoeofc(file)

FILE *file;

{
    int val;

#ifdef vax11c
    val= getc(file);
    if ( feof(file) )
#else
    if ((val = getc(file)) == EOF)
#endif
    {
	fprintf(output, "Unexpected EOF!");
	longjmp(next_file, 1);
    }
    return(val);
}

print_string_from_file(file, length)

FILE *file;
int length;

{
    register int ch;
    register int val;

    (void) putc('"', output);
    for (val = 0; val < length; val++)
    {
	ch = getnoeofc(file);
	if ( ((ch & 0x80) == 0) && (isprint(ch)) )
	{
	    if (ch == '"' || ch == '\\')
	    {
		(void) putc('\\', output);
	    }
	    (void) putc(ch, output);
	}
	else
	{
	    fprintf(output, "\\%03o", ch);
	}
    }
    (void) putc('"', output);
}

print_string(string)

unsigned char *string;

{
    register unsigned char *ptr;
    register unsigned char ch;

    (void) putc('"', output);
    ptr = string;
    while ((ch = *ptr++) != '\0')
    {
	if ( ((ch & 0x80) == 0) && (isprint(ch)) )
	{
	    if (ch == '"')
	    {
		(void) putc('\\', output);
	    }
	    (void) putc(ch, output);
	}
	else
	{
	    fprintf(output, "\\%03o", ch);
	}
    }
    (void) putc('"', output);
}

print_words_from_file(file, length)

FILE *file;
int length;

{
    int val;
    int cnt = 0;

    if ( dump_pixel == 1 )
    {
        while (length > 0)
    	{
	    val = getnoeofc(file) << 8;
	    val += getnoeofc(file);
    
	    fprintf(output, "%04x  ", val);
	    if (++cnt > 12)
	    {
		(void) fputc('\n',output);
	        cnt = 0;
	    }

	    length -= 2;
    	}
    }
    else
    {
	while ( length > 0 )
	{
	    val= getnoeofc(file);
	    length--;
	}	
    }
}
