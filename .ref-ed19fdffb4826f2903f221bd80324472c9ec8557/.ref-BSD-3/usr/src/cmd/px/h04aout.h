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
