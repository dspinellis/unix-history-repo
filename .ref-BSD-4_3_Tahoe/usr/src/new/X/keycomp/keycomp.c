#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

#ifndef lint
static char *rcsid_keycomp_c = "$Header: keycomp.c,v 10.5 86/05/29 15:55:47 newman Rel $";
#endif

#include <stdio.h>
#include <X/X.h>
#include "Xkeymap.h"

#define isnum(c) (((c) >= '0') && ((c) <= '9'))
#define isoctal(c) (((c) >= '0') && ((c) <= '7'))
#define whitespace(c) (((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\0'))

#define bool int
#define TRUE 1
#define FALSE 0

#define MAXLENGTH 80

typedef struct _EscMapEntry {
    char from, to;} EscMapEntry;

typedef enum _ParseError {
    e_NoError,
    e_NoKeycode,
    e_KeycodeTooBig,
    e_Not1Or16Items,
    e_NotNumber,
    e_NumberTooBig,
    e_SingleQuoteNotClosed,
    e_StringTooLong,
    e_DoubleQuoteNotClosed,
    e_TooManyCharsBetweenQuotes,
    e_Unrecognized
    } ParseError;

#define CT_ESC_ENTRIES 5
static EscMapEntry EscMap [CT_ESC_ENTRIES] = {
    {'n', '\n'},
    {'t', '\t'},
    {'b', '\b'},
    {'r', '\r'},
    {'f', '\f'}} ;
    
static KeyMapElt keymap [256];    

static int column_map[16] = 
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

/*  the following variables are global to facilitate error-handling */
static int line_no = 0, item_no = 0;

/* the following variables are global to simplify initialization */
char string_array [16][BUFSIZ];
char *strings[16];

main ()
    {
    char s[BUFSIZ];
    int i, j;
    char magic = X_KEYMAP_MAGIC;
    /* seek past end of keymap (to beginning of extension) */
    if (isatty(fileno(stdout)) 
      || (fseek (stdout, sizeof (keymap)+1, 0) == -1)) {
	/* "+1" above is because magic number is first byte in file */
	fprintf (stderr, "Couldn't fseek output file\n");
	exit (-1);
	}
    for (i=0;i<256;i++)
    	for (j=0;j<16;j++)
	    keymap[i][j] = UNBOUND;
    for (i=0;i<16;i++)
    	strings[i] = string_array[i];
    while (gets(s)) {
    	ProcessLine(s);
	line_no++;
	}
    fseek (stdout, 0, 0);
    if (!fwrite (&magic, 1, 1, stdout)
    	|| !fwrite (keymap, sizeof(keymap), 1, stdout)) {
	fprintf (stderr, "Error writing beginning of output file\n");
	exit (-1);
	}
    exit(0);
    }
    
ProcessLine (line)
    char *line;
    {
    int lengths [MAXLENGTH];
    int i=0, items, keycode;
    char c;
    if (line[0] == '#' || line[0] == '\0')
    	/* ignore comment lines (starting with '#') and blank lines */
    	return;
    if (!isnum(line[0]))
    	Error(e_NoKeycode);  /* line must start with key code */
    i++;
    while (isnum(line[i]))
    	i++;
    c = line[i];
    line[i] = '\0';
    sscanf (line, (line[0] == '0') ? "%o" : "%d", &keycode);
    if (keycode > 255)
    	Error(e_KeycodeTooBig);
    line[i] = c;
    items = ScanForItems (&line[i], strings, lengths);
    if (items == 1) {
	unsigned char value;
	int j;
	if (lengths[0] == 0)
	    value = UNBOUND;
    	else if (lengths[0] > 1 || !SingleCharBound (strings[0][0])) {
	    value = EXTENSION_BOUND;
	    AddToExtension (keycode, DontCareMetaBits, strings[0], lengths[0]);
	    }
	else
	    value = strings[0][0];
	for (j=0;j<16;j++)
	    keymap[keycode][j] = value;
	}
    else if (items == 16) {
	int j;
	for (j=0;j<16;j++) {
	    unsigned char value;
	    if (lengths[j] == 0)
	    	value = UNBOUND;
	    else if (lengths[j] > 1 || !SingleCharBound (strings[j][0])) {
		value = EXTENSION_BOUND;
		AddToExtension (keycode, column_map[j], strings[j], lengths[j]);
		}
	    else 
	    	value = strings[j][0];
	    keymap [keycode] [column_map[j]] = value;
	    }
	}
    else Error(e_Not1Or16Items);
    }

AddToExtension (keycode, metabits, string, length)
    unsigned int keycode, metabits;
    char *string;
    int length;
    {
    ExtensionHeader header;
    header.keycode = keycode;
    header.metabits = metabits;
    header.length = length;
    if (!fwrite (&header, ExtensionHeaderSize, 1, stdout) ||
    	!fwrite (string, length, 1, stdout)) {
	    fprintf (stderr, "Error writing extension to output file\n");
	    exit (-3);
	    }
    }

int ScanForItems (line, items, lengths)
    char *line;
    char *items[16];
    int lengths[16];
    {
    int i = 0;
    item_no = 0;
    while (1) {

	/* skip over leading white space */
	while (whitespace(line[i])) {
	    if (line[i] == '\0')
	    	return (item_no);
	    i++;
	    }
	
	if (isnum(line[i])) {
	    char *begin_num = &line[i];
	    char c;

	    /* find end of number string */
	    while (c = line[++i], isnum (c))
	    	/* this CANNOT be written isnum(line[++i]) because of side
	    	 * effect in expression passed to macro */
	    	;

	    /* temporarily put null character at end of number string */
	    c = line[i];
	    line[i] = '\0';
	    lengths [item_no] = TranslateNumber (begin_num, items[item_no]);
	    line[i] = c;
	    }

	else switch (line[i]) {
	    case '#':
	    	return(item_no);  /* rest of line is comment -- ignore it */

	    case 'U':	/* "U" means "unbound" */
	    	lengths [item_no] = 0;
		i++;  /* increment past the "U" character */
		break;

	    case '\'':
	    case '"': /* something between quotes */ {
	    	char c;
		char *begin_quote = &line[i++];
		bool backslash = FALSE;
		while (1) 
		    switch (c = line[i++]) {
			case '\0':
			    Error ((*begin_quote == '\'')
			    	? e_SingleQuoteNotClosed
				: e_DoubleQuoteNotClosed);
			    break;
			case '\\':
			    backslash = !backslash;
			    break;
			default:
			    if (c == *begin_quote && !backslash)
			    	goto out1;
			    backslash = FALSE;
			    break;
		      	}
	      out1:
		c = line [i];
		line[i] = '\0';
		lengths[item_no] = TranslateQuote (begin_quote, items[item_no]);
		if ((lengths[item_no] > 1) && (*begin_quote == '\''))
		    Error (e_TooManyCharsBetweenQuotes);
		line[i] = c;
		break;
		}

	    default:
	    	Error(e_Unrecognized);
		break;

	    }
    	
	if (line[i] == ',')
	    i++;  /* ignore terminating comma */
	if (!whitespace (line[i]))
	    Error(e_Unrecognized);
	item_no++;
	if (item_no == 16)
	    return (item_no);	/* ignore anything on line after 16th char */
	}
    
    }

int TranslateNumber (from, to)
    char *from, *to;
    {
    int value;
    sscanf (from, (from[0] == '0') ? "%o" : "%d", &value);
    if (value > 255)
    	Error(e_NumberTooBig);
    to[0] = value;
    return (1);  /* length */
    }


int TranslateQuote (from, to)
    char *from, *to;
    {
    int from_length = strlen (from);
    int i, to_length = 0;
    for (i=1;i<from_length-1;i++) {
	if (to_length >= MAXLENGTH)
	    Error(e_StringTooLong);
	if (from[i] == '\\') {
	    if (isoctal (from[i+1])) {
		/* backslash followed by octal digits */
		int digits = 1; /* how many successive digits (max 3) */
		int value;
		if (isoctal (from[i+2]))
		    digits += (1 + isoctal (from[i+3]));
		sscanf (from+i+1, "%3o", &value);
		if (value > 255)
		    Error(e_NumberTooBig);
		to[to_length++] = value;
		i += digits;
		}
	    else {
		/* backslash followed by non-number */
		int j;
		for (j=0;j<CT_ESC_ENTRIES;j++)
		    if (EscMap[j].from == from[i+1]) {
			to[to_length++] = EscMap[j].to;
			goto out;
			}
		to[to_length++] = from[i+1];
	      out:
		i++;
		}
	    }
	else
	    /* not a backslash, just an ordinary character */
	    to[to_length++] = from[i];
	}
    return (to_length);
    }

Error (type)
    ParseError type;
    {
    char *s;
    switch (type) {
	case e_NoKeycode:
	    s = "Line doesn't begin with keycode"; break;
	case e_KeycodeTooBig:
	    s = "Keycode is too big"; break;
	case e_Not1Or16Items:
	    s = "Line doesn't have 1 or 16 entries"; break;
	case e_NotNumber:
	    s = "Non-number found after backslash in char constant"; break;
	case e_NumberTooBig:
	    s = "Number after backslash is too big for a char constant"; break;
	case e_SingleQuoteNotClosed:
	    s = "Closing single quote not found"; break;
	case e_StringTooLong:
	    s = "String is too long"; break;
	case e_DoubleQuoteNotClosed:
	    s = "Closing double quote not found"; break;
	case e_TooManyCharsBetweenQuotes:
	    s = "Too many characters for single character constant"; break;
	case e_Unrecognized:
	    s = "Not a U, number, single- or double-quoted string"; break;
	default:
	    s = "Unknown error";  break;
	}
    fprintf (stderr, "Parse error at item %d on line %d:\n\t %s\n",
    	item_no+1, line_no+1, s);
    exit (type);
    }
