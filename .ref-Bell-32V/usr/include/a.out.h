/*
 * Format of an a.out header
 */
 
struct	exec {	/* a.out header */
	int		a_magic;	/* magic number */
	unsigned	a_text;		/* size of text segment */
	unsigned	a_data;		/* size of initialized data */
	unsigned	a_bss;		/* size of uninitialized data */
	unsigned	a_syms;		/* size of symbol table */
	unsigned	a_entry;	/* entry point */
	unsigned	a_trsize;	/* size of text relocation */
	unsigned	a_drsize;	/* size of data relocation */
};

#define	A_MAGIC1	0407		/* normal */
#define	A_MAGIC2	0410		/* read-only text */
#define	A_MAGIC3	0411		/* separated I&D */
#define	A_MAGIC4	0405		/* overlay */

struct	nlist { /* symbol table entry */
	char	n_name[8];	/* symbol name */
	char	n_type;		/* type flag */
	char	n_other;
	short	n_desc;
	unsigned n_value;	/* value */
};

	/* values for type flag */
#define	N_UNDF	0		/* undefined */
#define	N_ABS	02		/* absolute */
#define	N_TEXT	04		/* text */
#define	N_DATA	06		/* data */
#define	N_BSS	08
#define	N_TYPE	037
#define	N_FN	037		/* file name symbol */

#define	N_GSYM	0040		/* global sym: name,,type,0 */
#define	N_FUN	0044		/* function: name,,linenumber,address */
#define	N_STSYM 0046		/* static symbol: name,,type,address */
#define	N_RSYM	0100		/* register sym: name,,register,offset */
#define	N_SLINE	0104		/* src line: ,,linenumber,address */
#define	N_SSYM	0140		/* structure elt: name,,type,struct_offset */
#define	N_SO	0144		/* source file name: name,,,address */
#define	N_LSYM	0200		/* local sym: name,,type,offset */
#define	N_SOL	0204		/* #line source filename: name,,,address */
#define	N_PSYM	0240		/* parameter: name,,type,offset */
#define	N_LBRAC	0300		/* left bracket: ,,nesting level,address */
#define	N_RBRAC	0340		/* right bracket: ,,nesting level,address */
#define	N_LENG	0376		/* second stab entry with length information */

#define	N_EXT	01		/* external bit, or'ed in */

#define	FORMAT	"%08x"

#define	STABTYPES	0340
