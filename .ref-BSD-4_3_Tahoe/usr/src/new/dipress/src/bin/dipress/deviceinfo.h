/*
	deviceinfo.h:
		 data describing the Interpress device and fonts
*/

struct device_entry {
	short	font_size;	/* total size of font_entries in file (bytes), */
	short	resolution;	/* resolution of device */
	short	hor_units;	/* horizontal units */
	short	ver_units;	/* vertical units */
	short	width_units;	/* units used to descibe character widths */
	short	num_fonts;	/* number of valid fonts for device */
	short	num_sizes;	/* number of valid sizes for device  */
	short	scaling;	/* scale factor used to represent non-integer point sizes */
	short	output_wid;	/* maximum width of output (in units) */
	short	output_len;	/* maximum length of output (in units) */
	short	spec_char_num;	/* number of 'special' characters recognized by device */
				/* must exist in specCharTab */
	short	spec_name_len;	/* length of string containing all 'special' */
				/* character names */
	short	num_stiptypes;  /* number of different stipple families */
	short	unused[1];	/* for expansion */
};

struct font_entry {
	char	num_char_wid;		/* number of width entries for this font */
	char	special_flag;		/* indicates if this font is a special font (=1)  */
	char	ligature_flag;		/* indicates if ligatures exist on this font (=1) */
	char	unused;
	char	font_name[10];		/* troff internal font name  */
	char	font_number[10];	/* troff internal font number (ascii equivalent) */
};

