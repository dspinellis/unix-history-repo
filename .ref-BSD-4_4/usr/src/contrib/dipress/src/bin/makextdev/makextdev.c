/*
 *  makextdev - make an extended character code table for the ditroff
 *		interpress converter.
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  History:
 *	24-oct-85  ed flint	close fontfiles, ran out of open file
 *				descriptors with large number of fonts
 *
 *  Some of the characters in the interpress fonts have character codes
 *  greater than 255.  Since the ditroff character code table is an array of
 *  char, the codes are too large.  This attempts to remedy that situation.
 *  If the character code specified in the normal code table is 0377 (decimal
 *  255), then the real code can be found in the extended table (which is an
 *  array of short).  This program reads the same files that makedev reads and
 *  build the extended code table.
 *
 *  To specify an extended code in a font table, use a line of the following
 *  form:
 *
 *	    c       www     kk      0377    xxxxx
 *
 *  where:  c is the one or two letter character code,
 *	    www is the character's width,
 *	    kk is the kerning information, and
 *	    xxxxx is the actual interpress code for the character.
 *
 *  Basically, this is just like any other line in the font table file, except
 *  that the standard code is set to 0377 and the actual code is placed on the
 *  line as the fifth item.  Note that a file with lines like this can still
 *  be run thru makedev successfully -- makedev will just ignore the fifth
 *  item on the line.
 *
 *  One can make the extended tables for all the loaded fonts by saying
 *  "makextdev DESC" (just as with makedev), but makedev must be run before
 *  doing this, since it will require looking at DESC.out.
 */

# include "deviceinfo.h"
# include <stdio.h>
# include <sys/types.h>
# include <strings.h>

# define	Line_length	256

# define	Max_chars	256	/* maximum # of funny chars */
/* Tab_size includes all ascii characters except control characters */
# define	Table_size	(Max_chars + 128 - 32)

# define	white(ch)	((ch) == ' ' || (ch) == '\t' || (ch) == '\n')

/* values returned by strcmp */
# define	Equal	0

char	char_name[5 * Max_chars];
short	char_index_table[Max_chars];

unsigned char	font_index_table[Table_size];
unsigned short	extend_table[Table_size];
struct device_entry device_entry;

main(argc, argv)

int  argc;
char *argv[];

{
    int fntcnt;
    int descfd;
    register char *ptr;
    char line[Line_length];
    char keyword[80];
    char *fname;
    FILE *descfile;

    if (argc < 2)
    {
	fprintf(stderr, "usage: makextdev font\n");
	exit(1);
    }

    /* First off, get the DESC.out file -- we need the information */
    if ((descfd = open("DESC.out", 0)) == -1)
    {
	perror("DESC.out");
	fprintf(stderr, "makextdev: please run \"makedev DESC\" first!\n");
	exit(1);
    }

    /* read struct device_entry off the front */
    (void) read(descfd, (char *)&device_entry, sizeof(device_entry));

    /* skip over the point size table */
    (void) lseek(descfd, (device_entry.num_sizes + 1) * sizeof(short), 1);

    /* read char_index_table and char_name arrays */
    (void) read(descfd, (char *)char_index_table, device_entry.spec_char_num * sizeof(short));
    (void) read(descfd, char_name, device_entry.spec_name_len);
#ifdef DEBUG
    for (i=0; i<device_entry.spec_char_num; i++)
    {
	fprintf(stderr, "%d  ", char_index_table[i]);
    }
    fputc('\n', stderr);
    for (i=0; i<device_entry.spec_char_num; i++)
    {
	if (char_name[i] == '\0')
	{
	    fputc(' ', stderr);
	}
	else
	{
	    fputc(char_name[i], stderr);
	}
    }
    fputc('\n', stderr);
#endif

    /* step thru the arguments */
    while (--argc > 0)
    {
	fname = *++argv;

	if (strcmp(fname, "DESC") == Equal)
	{
	    /* do all the preloaded fonts */
	    if ((descfile = fopen(fname, "r")) == NULL)
	    {
		perror(fname);
		exit(1);
	    }

	    /* scan thru DESC looking for the "fonts" keyword */
	    do
	    {
		if (fgets(line, Line_length, descfile) == NULL)
		{
		    fprintf(stderr, "makextdev: no fonts listed in DESC\n");
		    exit(1);
		}
		(void) sscanf(line, "%s", keyword);
	    } while (strcmp(keyword, "fonts") != Equal);

	    /* found the line -- do each font listed on the line */
	    /* line looks like this:  fonts n X X X X ... 	 */
	    /* where n is number of fonts and X is a font name   */
	    (void) fclose(descfile);
	    ptr = line + 5;	/* 5 == strlen("fonts") */
	    while (white(*ptr))
	    {
		ptr++;		/* never trust a macro */
	    }

	    /* get font count */
	    fntcnt = atoi(ptr);
	    while (!white(*ptr))
	    {
		ptr++;
	    }
	    while (white(*ptr))
	    {
		ptr++;
	    }

	    /* process each font on the line */
	    while (fntcnt-- > 0)
	    {
		register char *fontname;

		fontname = ptr;
		while (!white(*ptr))
		{
		    ptr++;
		}
		*ptr++ = '\0';
		dofont(fontname);
		while (white(*ptr))
		{
		    ptr++;
		}
	    }
	}
	else
	{
	    dofont(fname);
	}
    }
}

dofont(fontname)

char *fontname;

{
    int outfile;
    int i;
    int ccode;
    unsigned int xcode;
    char outname[20];
    char ch[10];
    char swid[10];
    char skern[10];
    char scode[10];
    char sauxcode[10];
    char line[Line_length];
    char keyword[20];
    FILE *fontfile;
    struct font_entry font_entry;

    /* open the file that describes the font */
    if ((fontfile = fopen(fontname, "r")) == NULL)
    {
	perror(fontname);
	exit(1);
    }

    /* find the keyword "charset" */
    do
    {
	if (fgets(line, Line_length, fontfile) == NULL)
	{
	    fprintf(stderr, "makextdev: font %s has no charset\n", fontname);
	    exit(1);
	}
	(void) sscanf(line, "%s", keyword);
    } while (strcmp(keyword, "charset") != Equal);

    /* get font info from *.out file */
    /* first, open it		     */
    (void) strcpy(outname, fontname);
    (void) strcat(outname, ".out");
    if ((outfile = open(outname, 0)) == -1)
    {
	perror(outname);
	fprintf(stderr, "makextdev: please run \"makedev %s\" first!\n",
		fontname);
	exit(1);
    }

    /* zero extend_table */
    bzero(extend_table, sizeof(extend_table));

    /* now read the header and font_index_table */
    (void) read(outfile, (char *)&font_entry, sizeof(font_entry));
    (void) lseek(outfile, (off_t)((unsigned char)(font_entry.num_char_wid) * 3), 1);	/* skip width, kern, and code */
    (void) read(outfile, (char *)font_index_table, device_entry.spec_char_num + 128 - 32);
    (void) close(outfile);

    /* unscramble each line */
    while (fgets(line, Line_length, fontfile) != NULL)
    {
	i = sscanf(line, "%s %s %s %s %s", ch, swid, skern, scode, sauxcode);

	if (swid[0] != '"')
	{
	    /* translate scode and auxcode into actual numerical values */
	    ccode = convcode(scode);
	    xcode = convcode(sauxcode);
	}
	else
	{
	    /* it's a repeat of the last character */
	    i = 5;
	}

	/* check for extended code */
	if (ccode == 0377)
	{
	    if (i < 5)
	    {
		fprintf(stderr, "%s: character %s has no extended code\n",
			fontname, ch);
	    }
	    else
	    {
		/* find the findex and store the extended code in extend_table */
		if (strlen(ch) == 1)
		{
/*		    fprintf(stderr, "font_index_table[%d] = %d\n", 
			ch[0] - 32, font_index_table[ch[0] - 32]);
*/		    extend_table[font_index_table[ch[0] - 32]] = xcode;
		}
		else
		{
		    /* it's a funny name */
		    for (i = 0; i < device_entry.spec_char_num; i++)
		    {
			if (strcmp(&char_name[char_index_table[i]], ch) == Equal)
			{
/*			    fprintf(stderr, "i = %d, font_index_table[%d] = %d\n",
				i, i+128-32, font_index_table[i+128-32]);
*/
			    extend_table[font_index_table[i + 128-32]] = xcode;
			    break;
			}
		    }
		}
	    }
	}
    }

    /* Now, write extend_table in an appropriate file */
    /* remember: outname currently looks like "XX.out" */
    (void) strcat(outname, ".ext");
    if ((outfile = creat(outname, 0666)) == -1)
    {
	perror(outname);
	exit(1);
    }
    (void) write(outfile, (char *)extend_table, (device_entry.spec_char_num + 128-32) * sizeof(short));
    (void) close(outfile);
    (void) fclose(fontfile);
}

convcode(str)

char *str;

{
    int val;

    if (str[0] == '0')
    {
	(void) sscanf(str, "%o", &val);
    }
    else
    {
	val = atoi(str);
    }

    return(val);
}
