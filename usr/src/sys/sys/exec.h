/*	exec.h	1.1	84/07/21	*/

/*
 * Header prepended to each a.out file.
 */
struct exec {
	long	a_magic;	/* magic number */
unsigned long	a_text;		/* size of text segment */
unsigned long	a_data;		/* size of initialized data */
unsigned long	a_bss;		/* size of uninitialized data */
unsigned long	a_syms;		/* size of symbol table */
unsigned long	a_entry;	/* entry point */
unsigned long	a_trsize;	/* size of text relocation */
unsigned long	a_drsize;	/* size of data relocation */
};

#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */
