/*
 *  maharani - a czarina-like program that generates interpress files
 *
 *  Written for Xerox Corporation by William LeFebvre
 *
 *  Copyright (c)  1984, 1985, 1986 Xerox Corporation
 *
 *  HISTORY
 * 23-Sep-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added Jim Mayer's fix to the space allocator.
 *
 * 13-apr-86  Ed Flint (ed) at Xerox Webster Research Center
 *	do_file will reset to left margin when encountering carriage return
 *
 * 11-Feb-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added various suggestions from Larry Parmalee at Cornell U.
 *	1)  Margin adjustments.  Margins vary according to portrait or
 *	    landscape mode.  If headings are suppressed, then that area
 *	    is used for text.  Room left to punch holes at left.
 *	2)  Baseline spacing is tightened up for smaller font sizes
 *	3)  options can now reset each other
 *	4)  added "-P" option to specifiy output device (for compatibility
 *	    with lpr system stuff.)
 *	5)  Improved maha environment variable handling (pair up and eliminate
 *	    double and single quotes).
 *	6)  Interpress file now has mode 600 (for security reasons)
 *	7)  Maximum line length increased.
 *
 * 13-Jan-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Changed a call to strcpyn to strncpy.
 *
 *  8-apr-85  ed flint
 *	conditional compilation for vax11-c (vms)
 *
 *  26-mar-85 ed flint @ Xerox, WRC
 *	add fclose after do_file to prevent running out of open file descriptors
 */

#ifdef vax11c
# include stdio
# include ssdef
# include ctype
# include descrip
#else
# include <stdio.h>
# include <pwd.h>
# include <strings.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/time.h>
#endif

# include "iptokens.h"
# include "literal.h"
# include "operator.h"


/*
 * the following defines a program that will queue an Interpress master
 * for printing
 */
# define  QIP		"qip"

# define  Break_size	1024
# define  Default_universal_prefix	"Xerox/XC1-1-1/"
# define  Line_size	192		/* upped from 132 */

/* All page boundaries are computed in the 1/10 point co-ordinate system */
/*
 *  Orig_y is an offset from the top of the page.  It must be converted
 *  to a measurement from the bottom of the page (a calculation that is
 *  rotation-dependent).
 */

# define  INCH		720
# define  Half_INCH	360
# define  Sixth_INCH	120			/* one line at 6 lpi */
# define  Page_width	(8 * INCH + Half_INCH)
# define  Page_length	(11 * INCH)
			/* rflg  ?  Landscape mode : Portrait mode */
# define  Orig_x	(rflg ? (1.5 * INCH/10) : (9 * INCH/10))
# define  Orig_y	(rflg ? (Sixth_INCH * 8) : (Sixth_INCH * 5))
# define  Header_to_orig_x	0
# define  Header_to_orig_y	(2 * Sixth_INCH)

/* Frame variable defines */
# define  F_transform	0
# define  F_headfont	1
# define  F_bodyfont	2
# define  F_italicfont	3

# define  No	0
# define  Yes	1

extern int errno;

/* enum, perhaps? */
typedef char boolean;

/* routines that return something other than int */
char *strecpy();
char *allocate();
char *next_arg();
char *sbrk();
char *getenv();
char *itoa();
char *rindex();

/* option flags */
boolean lflg = No;	/* line printer mode */
boolean rflg = No;	/* rotation - landscape mode */
boolean tflg = No;	/* omit title */

/* valued options */
int  columns   = 1;
char *bodyfont_name = "Vintage-Printwheel/10";
char *headfont_name = "Modern-Bold/12";
char *italicfont_name = "Modern-Bold-Italic/12";
char *banner   = NULL;
char *copies   = "1";
char *printer  = NULL;			/* destination printer */
char *header   = "%f            %t            Page %p, line %l";
char *name     = NULL;
char *output   = NULL;
char *pages    = NULL;

#ifdef vax11c
char *template = "IPPXXXXXX";
#endif

/*
 *  page characteristics:  these variables define the extremes for the
 *  current page or column.  'column_separation' is the distance between
 *  the left sides of each column on the page.
 */
#ifndef notdef
int left_margin = (9 * INCH / 10);	/* x origin in portrait mode */
#else
int left_margin = Orig_x;
#endif
int right_margin;
int top_margin;
int bottom_margin;
int column_separation;

/* sundries */
boolean send_to_printer = Yes;
char real_header[256];		/* header built here */
char *myname;			/* name invoked with */
char *filename;
int  *page_select = NULL;	/* array of page selections */
int  *curr_page_select;
int  page_low;
int  page_high;
int  ipress_file;		/* interpress file descriptor */
int  null_file;			/* fd for /dev/null */
int  line_number;
int  page_number;
int  pages_printed = 0;		/* total pages for this interpress file */
int  special_font = 0;		/* fonts that require special handling */
int  line_spacing;
int  tab_amount = 8;
#ifndef vax11c
struct passwd *pwd;		/* passwd entry for this user */
struct stat   file_stat;	/* stat of current file */
#endif

# define  Font_Terminal  1

/* current arguments */
int  argc;
char **argv;

/* font structure definition */
struct font
{
    char *ft_universal_name;
    char *ft_leaf_name;
    int  ft_size;
};

/* fonts used */
struct font headfont;
struct font bodyfont;
struct font italicfont;

main(_argc, _argv)

int  _argc;
char **_argv;

{
    char *ptr;		/* temporary pointers used for loops and such */
    char *src;
    char *dest;
    int length;
    int i;
    FILE *file;		/* file currently processing */
#ifdef vax11c
    int error;
    int retlen;
    char command[256];
    $DESCRIPTOR(mahadesc,"MAHAENV");
    $DESCRIPTOR(cmddesc,command);
#endif

    /* get our name */
    if (_argc < 1)
    {
	exit(1);
    }

#ifdef vax11c
    myname= _argv[0];
#else
    if ((myname = rindex(_argv[0], '/')) == NULL)
    {
	myname = _argv[0];
    }
    else
    {
	myname++;
    }
#endif

    /* get the options specified in the environment (defaults) */

#ifdef vax11c
    if ( (error= lib$get_symbol(&mahadesc,&cmddesc,&retlen)) == SS$_NORMAL )
    {
	if ( retlen != 0 )
	{
	    command[retlen & 0xff]= '\0';		/* null terminate string */
	    src= command;
	/* break the string up into null terminated arguments */
	/* half the length is a good upper bound on number of arguments */
		argv = (char **)allocate(strlen(src) / 2);
		for (argc = 1, ptr = src; *ptr != '\0'; argc++)
		{
		    while (*ptr == ' ')
			ptr++;
		    argv[argc] = ptr;
		    while (*ptr != ' ' && *ptr != '\0')
		    {
			if (*ptr == '"')
			{
			    while (*++ptr != '"' && *ptr != '\0');
			}
			else if (*ptr == '\'')
			{
			    while (*++ptr != '\'' && *ptr != '\0');
			}
			ptr++;
		    }
		    *ptr++ = '\0';
		}

	/* terminate the argument list */
		argv[argc] = NULL;

	/* process the options found in the environment */
		get_options();
	}
    }

#else
    if ((src = getenv("MAHA")) != NULL)
    {
	/* break the string up into null terminated arguments */
	/* half the length is a good upper bound on number of arguments */
	argv = (char **)allocate(strlen(src) / 2);
	for (argc = 1, ptr = src; *ptr != '\0'; argc++)
	{
	    register char *dst;
	    char quote;

	    while (*ptr == ' ')
		ptr++;
	    argv[argc] = dst = ptr;
	    while (*ptr != ' ' && *ptr != '\0')
	    {
		if (*ptr == '"' || *ptr == '\'')
		{
		    quote = *ptr++;	/* Save + skip quote */

		    while( *ptr != quote && *ptr != '\0')
			*dst++ = *ptr++;

		    if (*ptr != '\0')
			ptr++;		/* Skip closing quote */
		}
		else
		    *dst++ = *ptr++;
	    }
	    if (*ptr != '\0')
		ptr++;		/* skip 1st trailing space */

	    *dst = '\0';	/* end arg. */
	}

	/* terminate the argument list */
	argv[argc] = NULL;

	/* process the options found in the environment */
	get_options();
    }
#endif

    /* use the real arguments */
    argc = _argc;
    argv = _argv;

    /* process (real) arguments */
    get_options();

    /* establish and verify the requested fonts */
    establish_font(headfont_name, &headfont);
    establish_font(bodyfont_name, &bodyfont);
    establish_font(italicfont_name, &italicfont);

#ifndef vax11c
    /* get passwd entry for future reference */
    pwd = getpwuid(geteuid());
#endif

    /* setup output file */
    if (output == NULL)
    {

	/* build a temporary file name */

#ifdef vax11c
	output= mktemp(template);
#else
	output = allocate(1 + 5 + 1 + 2 + 1);
	(void) sprintf(output, "/tmp/@%d.ip", getpid());
#endif
    }

#ifdef vax11c
    if ((ipress_file = creat(output, 0, "rfm=udf")) == -1)
#else
    if ((ipress_file = creat(output, 0600)) == -1)
#endif
    {
	system_error(output);
	exit(1);
    }
    ip_select(ipress_file);

#ifndef vax11c
    /* open the null device for throwing away output */
    null_file = open("/dev/null", 1);

    /* set null strings to default values */
    if (name == NULL)
    {
	/* banner name defaults to full name from gecos field */
	name = pwd->pw_gecos;

	/* perform expansion and stripping */
	if ((ptr = index(name, ',')) != NULL)
	{
	    *ptr = '\0';	/* this affects pwd->pw_gecos, too! */
	}
	if (index(name, '&') != NULL)
	{
	    name = allocate(strlen(name) + strlen(pwd->pw_name) + 1);
	    for (src = pwd->pw_gecos, dest = name; *src != '\0'; src++, dest++)
	    {
		if (*src == '&')
		{
		    for (ptr = pwd->pw_name; *ptr != '\0'; ptr++)
		    {
			*dest++ = *ptr;
		    }
		}
		else
		{
		    *dest = *src;
		}
	    }
	}
    }
#endif

    if (banner == NULL)
    {
	/* banner defaults to file name(s) */
	if (argc == 0)
	{
	    banner = "out of the blue";
	}
	else
	{
	    for (length = 0, i = 0; i < argc; i++)
	    {
		length += strlen(argv[i]) + 2;
	    }
	    banner = allocate(length + 1);
	    for (ptr = banner, i = 0; i < argc; i++)
	    {
		ptr = strecpy(ptr, argv[i]);
		ptr = strecpy(ptr, ", ");
	    }
	    ptr -= 2;
	    *ptr = '\0';
	}
    }

    /* unravel the page specifiation */
    /* we will never need more than strlen(pages) ints to hold the info */
    if (pages != NULL)
    {
	page_select = (int *)allocate(strlen(pages) * sizeof(int));
	unravel_pages(pages, page_select);
    }

    /* write the preamble for the interpress file */
    AppendOp(OP_beginBlock);
    AppendOp(OP_beginBody);	/* preamble start */

    /* setup font definitions in frame */
    SetupFont(headfont.ft_universal_name,
	      headfont.ft_size * 10.,
	      F_headfont);
    SetupFont(bodyfont.ft_universal_name,
	      bodyfont.ft_size * 10.,
	      F_bodyfont);
    SetupFont(italicfont.ft_universal_name,
	      headfont.ft_size * 10.,		/* use headfont's size */
	      F_italicfont);

    /* remember special fonts */
    if (strcmp(bodyfont.ft_leaf_name, "Terminal") == 0)
    {
	special_font = Font_Terminal;
    }

    /* save scaling transform that uses 1/10 point co-ordinate system */
    top_margin = (rflg ? Page_width : Page_length) - Orig_y;

    if (tflg)		/* the user may not want headings... */
	top_margin += Header_to_orig_y; /* use heading area */

    bottom_margin = 2 * Sixth_INCH;
    right_margin = (rflg ? Page_length : Page_width) - Orig_x;
    column_separation = (right_margin - Orig_x) / columns;
    line_spacing = (bodyfont.ft_size +
		  ((bodyfont.ft_size > 8) ? 2 : 0)) * 10;
    if (rflg)
    {
	/* we need a rotation transform, too */
	Rotate(90.);
	Translate((double)Page_width, (double)0);
    }
    AppendRational(353L,10000000L);
    AppendOp(OP_scale);
    if (rflg)
    {
	AppendOp(OP_concat);
	AppendOp(OP_concat);
    }
    AppendInteger((long) F_transform);
    AppendOp(OP_fset);

    AppendOp(OP_endBody);	/* end preamble */

    if (argc == 0)
    {
	/* no filenames -- do standard input */
	filename = NULL;
	do_file(stdin);
    }

    for (; argc > 0; argc--, argv++)
    {
	filename = argv[0];
	if (strcmp(filename, "-") == 0)
	{
	    /* this is really standard input */
	    filename = NULL;
	    do_file(stdin);
	}
	else
	{
	    /* open the file */
	    if ((file = fopen(filename, "r")) == NULL)
	    {
		system_error(filename);
	    }
	    else
	    {
		do_file(file);
		(void) fclose(file);
	    }
	}
    }

    /* wrap up the output */
    ip_select(ipress_file);
    AppendOp(OP_endBlock);
    ip_close();

    /* send to the printer */
    if (send_to_printer)
    {
	if (pages_printed == 0)
	{
	    /* don't print anything but remove temporary */
#ifdef vax11c
	    delete(output);
#else
	    (void) unlink(output);
#endif
	}
	else
	{
#ifdef vax11c
	    char buff[256];
	    int wait= 0;
	    $DESCRIPTOR(buffdesc,buff);

	    (void) strcpy(buff,"xpress/noformat ");
	    (void) strcat(buff,output);
    	    buffdesc.dsc$w_length= strlen(buff);
	    if ( (error= lib$spawn(&buffdesc,0,0,&wait)) != SS$_NORMAL )
	    {
	    	fprintf(stderr,"\nFile %s contains interpress master\n",output);
		exit(error);
	    }
    	    delete(output);
#else
	    char *buff;
   
	    /* exec a "qip" to queue the file */
	    buff = allocate(strlen(name) + 1 + 1);
	    (void) strcpy(buff, "F");
	    (void) strcat(buff, name);

	    if (printer)
		execlp(QIP, "qip", "-q", printer, "-c", copies, "-nc", "-nk",
		    "-t", banner, "-x", buff, output, 0);
	    else
		execlp(QIP, "qip", "-c", copies, "-nc", "-nk",
		    "-t", banner, "-x", buff, output, 0);

	    fprintf(stderr, "Can't execl the queing program: %s\n", QIP);
	    perror(QIP);
	    fprintf(stderr, "File %s contains interpress master.\n", output);
	    exit(1);
#endif
	}
    }
}

get_options()

{
    while (--argc > 0)
    {
	argv++;
	if (!process_arg())
	{
	    break;
	}
    }
}

/*
 *  unravel_pages(str, spec) - unravel the page range specification in "str"
 *			       into integer pairs in "spec".  The first two
 *			       ints in "spec" bound the first range of pages,
 *			       the next two bound the second range, and so on.
 *			       The array is terminated with the pair 0, 0.
 */

unravel_pages(str, spec)

char *str;
int  *spec;

{
    int last_num = 0;
    int this_num = 0;
    register char ch;
    boolean is_range = No;
    boolean bad_spec = No;
    boolean done = No;

# define    Start_new_num	(last_num = this_num, this_num = 0)

    while (!done)
    {
	if ((ch = *str++) == '\0')
	{
	    /* set "done" flag and pretend it's the end of a number */
	    done = Yes;
	    ch = ',';
	}
	if (ch >= '0' && ch <= '9')
	{
	    this_num *= 10;
	    this_num += ch - '0';
	}
	else if (ch == '-')
	{
	    if (this_num < last_num && *str != '\0')
	    {
		bad_spec = Yes;
	    }
	    *spec++ = this_num;
	    Start_new_num;
	    is_range = Yes;
	}
	else if (ch == ',')
	{
	    if (this_num < last_num)
	    {
		bad_spec = Yes;
	    }
	    *spec++ = this_num;
	    if (is_range)
	    {
		is_range = No;
	    }
	    else
	    {
		*spec++ = this_num;
	    }
	    Start_new_num;
	}
	else
	{
	    fprintf(stderr, "%s: bad character in page specification\n",
		myname);
	    exit(1);
	}
    }
    if (*--spec == 0)
    {
	*spec = 1 << 15;	/* infinity */
    }
    if (bad_spec)
    {
	fprintf(stderr,
	    "%s: pages should be given in non-descending order.\n",
	    myname);
    }
}

process_arg()

{
    register char ch;
    register int  temp;
    register char *p1;
    register char *p2;

    if (argv[0][0] == '-')
    {
	if ((ch = argv[0][1]) > '0' && ch <= '9')
	{
	    /* this is a column count specifier */
	    columns = ch - '0';
	}
	else switch(ch)
	{
	    case '\0':		/* not an option */
		return(No);

	    case 'b':
		banner = next_arg();
		break;

	    case 'c':
		temp = atoi(copies = next_arg());
		if (temp < 1)
		{
		    fprintf(stderr,
			"%s: bogus number of copies; you only get one!\n",
			myname);
			copies = "1";
		}
		break;

	    case 'f':
		bodyfont_name = next_arg();
		break;

	    case 'F':
		headfont_name = next_arg();
		break;

	    case 'H':			/* replace header */
		tflg = No;
		header = next_arg();
		break;

	    case 'h':			/* append to header */
		tflg = No;
		p1 = next_arg();
		p2 = allocate(strlen(header) + strlen(p1) + 1);
		(void) strcpy(p2, header);
		(void) strcat(p2, "      ");
		(void) strcat(p2, p1);
		header = p2;
		break;

	    case 'l':
		tflg = lflg = Yes;
		break;

	    case 'n':
		name = next_arg();
		break;

	    case 'o':
		output = next_arg();
		send_to_printer = No;
		break;

	    case 'P':
		printer = next_arg();
		break;

	    case 'r':
		rflg = Yes;
		break;

	    case 'R':
		rflg = No;
		break;

	    case 's':
		pages = next_arg();
		break;

	    case 't':
		tflg = Yes;
		break;

	    default:
		fprintf(stderr, "%s: unknown option '%c'\n", myname, ch);
	}
	return(Yes);
    }
    else
    {
	return(No);
    }
}

char *next_arg()

{
    if (argv[0][2] == '\0')
    {
	if (--argc > 0)
	{
	    return((++argv)[0]);
	}
	else
	{
	    argv++;
	    return(NULL);
	}
    }
    else
    {
	return(&(argv[0][2]));
    }
}

/*
 *  establish_font(name, font) - break apart the parts of the string "name"
 *				 and fill in the structure pointed to by
 *				 "font".  Also, verify that the font requested
 *				 actually exists.  This routine also
 *				 understands universal font names.
 */

establish_font(name, font)

char *name;
struct font *font;

{
    register char *unamep;
    register char *ptr;
    char *slashp;
    register int size;

    if (name[0] != '/')
    {
	/* not a universal name -- put the default on the front */
	font->ft_universal_name = unamep =
		allocate(strlen(Default_universal_prefix) +
			 strlen(name) + 1);
	(void) strcpy(unamep, Default_universal_prefix);
	(void) strcat(unamep, name);
    }
    else
    {
	/* already is a universal name -- just allocate space for it */
	font->ft_universal_name = unamep = allocate(strlen(name));

	/* copy in the whole name, without the leading slash */
	(void) strcpy(unamep, name + 1);
    }

    /* strip size off the end, if it is there */
    if ((slashp = ptr = rindex(unamep, '/')) != NULL)
    {
	register char ch;

	size = 0;
	while ((ch = *++ptr) != '\0')
	{
	    if (ch < '0' || ch > '9')
	    {
		/* last element is not a number -- no point size */
		size = 0;
		break;
	    }

	    /* shift this digit in */
	    size *= 10;
	    size += (ch - '0');
	}

	/* if no point size, use default */
	if (size == 0)
	{
	    font->ft_size = 10;
	}
	else
	{
	    font->ft_size = size;
	    *slashp = '\0';
	}
    }

    /* set pointer to last element */
    if ((ptr = rindex(unamep, '/')) != NULL)
    {
	font->ft_leaf_name = ptr + 1;
    }
    else
    {
	font->ft_leaf_name = font->ft_universal_name;
    }
}

do_file(file)

FILE *file;

{
    char *src;
    char *dest;
    char input_line[Line_size];
    char line_buffer[Line_size];
    char ch;
    int current_line;
    int lines_on_page;
    int length;
    int column;
    
#ifndef vax11c
    /* fstat it to get information displayed in the header */
    if (fstat(fileno(file), &file_stat) == -1)
    {
	system_error("fstat botched");
	return;
    }
#endif

    /* reset essentials */
    page_number = 0;
    line_number = 1;
    lines_on_page = 0;
    curr_page_select = page_select;
    if (pages != NULL)
    {
    	page_low  = page_select[0];
    	page_high = page_select[1];
    }
    current_line = top_margin;

    /*
     *  Strangeness:  page_number is incremented by page_start and
     *  line_number is incremented in the "while(fgets..." loop.
     */

    /* start the first page */
    page_start();

    /*
     *  More strangeness:  we had to set line_number to 1 to trick
     *  page_start into reporting the right line count in the header.  Now
     *  we reset it to 0 before entering the read/print loop.
     */
    line_number = 0;

    while (fgets(input_line, Line_size, file) != NULL)
    {
	/* new line */
	line_number++;

	/* remember the length */
	length = strlen(input_line);

	/* nuke any trailing newline */
	if (input_line[length - 1] == '\n')
	{
	    input_line[--length] = '\0';
	}

	if (lflg ? lines_on_page >= 66 : current_line < bottom_margin)
	{
	    /* start a new page */
	    page_end(No);
	    page_start();
	    lines_on_page = 0;

	    /* remember, y goes backwards */
	    current_line = top_margin;
	}

	/* make sure that the line actually contains something */
	if (input_line[0] != '\0')
	{
	    /* set x and y for the beginning of the line */
	    Setxy((double)left_margin, (double)current_line);

	    /* copy from input_line to line_buffer making any necessary
	       changes along the way */
	    column = 0;
	    src = input_line;
	    dest = line_buffer;
	    while ((ch = *src) != '\0')
	    {
		switch(ch)
		{
		    case '\r':		/* carriage return */
			*dest = '\0';
			if (line_buffer[0] != '\0')
			{
			    ShowString(line_buffer);
			}
			Setxy((double)left_margin, (double)current_line);
			dest= line_buffer;
			break;

		    case '\f':		/* new page after this line */
			current_line = bottom_margin;
			lines_on_page = 66;
			break;

		    case '\t':		/* tab expansion */
			do
			{
			    *dest++ = ' ';
			    column++;
			} while (column % tab_amount != 0);
			break;

		    case '$':
			*dest++ = '\244';
			column++;
			break;

		    case '-':
			if (special_font == Font_Terminal)
			{
			    /* heavy hackery here */
			    *dest = '\0';
			    ShowString(line_buffer);
			    Setyrel(-20.);
			    ShowString("\305");
			    Setyrel(20.);
			    dest = line_buffer;
			    column++;
			    break;
			}
			/* else fall thru ... */

		    default:
			*dest++ = ch;
			column++;
		}
		src++;
	    }
	    *dest = '\0';
	    if (line_buffer[0] != '\0')
	    {
		ShowString(line_buffer);
	    }
	}

	/* advance the line counters */
	current_line -= line_spacing;
	lines_on_page++;
    }

    /* wrap up the file */
    page_end(Yes);
}

/*
 *  page handling:  a distinction is made between virtual pages and actual
 *  pages.  A virtual page is one series of lines from the file that appears
 *  vertically on the printed page.  The actual page is the page as the
 *  printer prints it (a printed page, if you will).  There may be several
 *  virtual pages on one actual page.  The page_start and page_end routines
 *  that follow start and terminate virtual pages.  The mapping between
 *  virtual and actual pages is a function of the options specified by the
 *  user.  If the user requests two column output then there will be two
 *  virtual pages for every actual page.  These pages will sit side-by-side on
 *  the actual page.  The mapping is accomplished by changing the variables
 *  left_margin and right_margin.  "page_start" also handles printing of the
 *  page header, since there is only one of these on every actual page.
 */

static int current_column;

page_start()

{
    boolean in_set;

#ifdef vax11c
    long bintim;
#endif

    /* reset the column count if starting a new file */
    if (line_number == 1)
    {
	current_column = 0;
    }

    /* either move the left margin or put out a new page */
    if (current_column != 0)
    {
	left_margin += column_separation;
    }
    else
    {
	/* increment page count and reset margin */
	page_number++;
	left_margin = Orig_x;

	/* is it in the page specification set? */
	if (page_select == NULL)
	{
	    /* every page is in the set if there is no specification */
	    in_set = Yes;
	}
	else
	{
	    if (page_low <= page_number && page_number <= page_high)
	    {
		in_set = Yes;
		ip_select(ipress_file);
		if (page_number == page_high)
		{
		    /* at the top of the current range -- time to move up */
		    curr_page_select += 2;
		    page_low  = curr_page_select[0];
		    page_high = curr_page_select[1];
		}
	    }
	    else
	    {
		/* not in set -- redirect output to null device */
		in_set = No;
		ip_select(null_file);
	    }
	}

	if (in_set)
	{
	    register char *src;
	    register char *dst;
	    register char ch;

	    /* increment total page count */
	    pages_printed++;

	    /* output stuff for new ip page */
	    AppendOp(OP_beginBody);
	
	    /* set the transformation */
	    Fget(F_transform);
	    AppendOp(OP_concatt);

	    /* build the header if we need to print it */
	    if (!tflg)
	    {
		/* move characters from header to real_header */
		/* and expand format items along the way.     */
		src = header;
		dst = real_header;
		while ((ch = *src) != '\0')
		{
		    if (ch == '%')
		    {
			switch(ch = *++src)
			{
			    case 'f':		/* file name */
				dst = strecpy(dst,
					    filename == NULL ? 
						"Standard input" :
						filename);
				break;
    
			    case 't':		/* mtime */
#ifdef vax11c

				time(&bintim);
				strncpy(dst,ctime(&bintim), 24);
				dst += 24;
#else
				/*
				 *  ctime returns a 26 character string that
				 *  has a newline and null at the end.
				 *  26 - 2 == 24.
				 */
				if (file_stat.st_mtime != 0)
				{
				    (void) strncpy(dst,ctime(&file_stat.st_mtime),24);
				    dst += 24;
				}
#endif
				break;
    
			    case 'p':		/* page number */
				dst = itoa(dst, page_number);
				break;
    
			    case 'l':		/* line number */
				dst = itoa(dst, line_number);
				break;
    
			    case '\0':		/* end of the string */
				src--;		/* maintain loop invariant */
				break;
    
			    default:		/* copy the character */
				*dst++ = ch;
				/* break; */
			}
		    }
		    else
		    {
			*dst++ = ch;
		    }
		    src++;
		}
    
		/* terminate the real header */
		*dst = '\0';
	    
		/* display the header */
		Setxy((double)(left_margin - Header_to_orig_x),
		      (double)(top_margin + Header_to_orig_y));
		Setfont(F_headfont);
		ShowString(real_header);
	    }
	}
    }

    /* select the body font */
    Setfont(F_bodyfont);
}

page_end(eof)

int eof;

{
    if ((current_column = ++current_column % columns) == 0 || eof)
    {
	AppendOp(OP_endBody);
    }
}

char *strecpy(dest, src)

register char *src;
register char *dest;

{
    while (*dest++ = *src++)
	;
    return(--dest);
}

char *itoa(buff, val)

char *buff;
int  val;

{
    char tbuff[12];	/* will build number here -- max of 10 digits */
    register char *ptr = tbuff + 11;

    *ptr-- = '\0';
    while (val != 0)
    {
	*ptr-- = (val % 10) + '0';
	val /= 10;
    }
    return(strecpy(buff, ++ptr));
}

/*
 *  allocate(space) - allocate "space" bytes with sbrk.  This routine uses a
 *		      fairly naive algorithm.  It sbrk-s space in Break_size
 *		      chunks and allocates space from a chunk until a request
 *		      for more space than is left in the chunk is made.  Then,
 *		      it allocates a new chunk.  The unused space at the end
 *		      of the old chunk remains unused.  This does NOT depend
 *		      on sbrk returning contiguous chunks of memory during the
 *		      life of the program.  If the request is greater than
 *		      Break_size, the next multiple of Break_size greater
 *		      than the request size is chosen.
 */

char *allocate(space)

int space;

{
    static char *hi_water = NULL;
    static char *max_alloc = NULL;
    register char *ptr;

    if (hi_water == NULL || max_alloc + space > hi_water)
    {
	int alloc_size = (space <= Break_size
			  ? Break_size
			  : ((space + Break_size-1) / Break_size) * Break_size);

	hi_water = sbrk(alloc_size);

	if ((int)hi_water == -1)
	{
	    system_error("out of space");
	    exit(1);
	}

	max_alloc = hi_water + alloc_size - 1;
    }

    ptr = hi_water;
    hi_water += space;
    return(ptr);
}

system_error(message)

char *message;

{
    int saved_errno;

    /* value of errno not preserved by fprintf */
    saved_errno = errno;
    fprintf(stderr, "%s: ", myname);
    errno = saved_errno;
    perror(message);
}

#ifdef vax11c
char *rindex(string, c)
char *string, c;
{
	register char *pos;

	pos = 0;
	do {
		if (*string == c)
			pos = string;
	} while (*string++);
	return(pos);
}

#endif

