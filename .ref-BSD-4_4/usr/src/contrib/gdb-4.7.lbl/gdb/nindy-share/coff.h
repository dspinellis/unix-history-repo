/*  Copyright 1990, 1992 Free Software Foundation, Inc.
 *
 *   This code was donated by Intel Corp.
 *
 * This is a coff version of a.out.h to support 80960 debugging from
 * a Unix (possibly BSD) host.  It's used by:
 *	o gdb960 to symbols in code generated with Intel (non-GNU) tools.
 *	o comm960 to convert a b.out file to a coff file for download.
 */


/********************** FILE HEADER **********************/

struct filehdr {
	unsigned short	f_magic;	/* magic number			*/
	unsigned short	f_nscns;	/* number of sections		*/
	long		f_timdat;	/* time & date stamp		*/
	long		f_symptr;	/* file pointer to symtab	*/
	long		f_nsyms;	/* number of symtab entries	*/
	unsigned short	f_opthdr;	/* sizeof(optional hdr)		*/
	unsigned short	f_flags;	/* flags			*/
};


/* Bits for f_flags:
 *	F_RELFLG	relocation info stripped from file
 *	F_EXEC		file is executable (no unresolved externel references)
 *	F_LNNO		line nunbers stripped from file
 *	F_LSYMS		local symbols stripped from file
 *	F_AR32WR	file has byte ordering of an AR32WR machine (e.g. vax)
 */
#define F_RELFLG	0000001
#define F_EXEC		0000002
#define F_LNNO		0000004
#define F_LSYMS	0000010
#define F_AR32WR	0000400


/*
 *	Intel 80960 (I960) processor flags.
 *	F_I960TYPE == mask for processor type field. 
 */
#define	F_I960TYPE		0170000
#define	F_I960CA		0010000
#define F_I960FLOAT		0020000
#define F_I960BA		0030000
#define F_I960XA		0040000

/*
 * i80960 Magic Numbers
 */
#define I960ROMAGIC	0540	/* read-only text segments	*/
#define I960RWMAGIC	0541	/* read-write text segments	*/

#define I960BADMAG(x) (((x).f_magic!=I960ROMAGIC) && ((x).f_magic!=I960RWMAGIC))

#define	FILHDR	struct filehdr
#define	FILHSZ	sizeof(FILHDR)


/********************** AOUT "OPTIONAL HEADER" **********************/

typedef struct {
	unsigned long	phys_addr;
	unsigned long	bitarray;
} TAGBITS;

typedef	struct aouthdr {
	short		magic;	/* type of file				*/
	short		vstamp;	/* version stamp			*/
	unsigned long	tsize;	/* text size in bytes, padded to FW bdry*/
	unsigned long	dsize;	/* initialized data "  "		*/
	unsigned long	bsize;	/* uninitialized data "   "		*/
#if U3B
	unsigned long	dum1;
	unsigned long	dum2;	/* pad to entry point	*/
#endif
	unsigned long	entry;	/* entry pt.				*/
	unsigned long	text_start;	/* base of text used for this file */
	unsigned long	data_start;	/* base of data used for this file */
	unsigned long	tagentries;	/* number of tag entries to follow */
} AOUTHDR;

/* return a pointer to the tag bits array */

#define TAGPTR(aout) ((TAGBITS *) (&(aout.tagentries)+1))

/* compute size of a header */

#define AOUTSZ(aout) (sizeof(AOUTHDR)+(aout.tagentries*sizeof(TAGBITS)))

/********************** STORAGE CLASSES **********************/

#define C_EFCN		-1	/* physical end of function	*/
#define C_NULL		0
#define C_AUTO		1	/* automatic variable		*/
#define C_EXT		2	/* external symbol		*/
#define C_STAT		3	/* static			*/
#define C_REG		4	/* register variable		*/
#define C_EXTDEF	5	/* external definition		*/
#define C_LABEL		6	/* label			*/
#define C_ULABEL	7	/* undefined label		*/
#define C_MOS		8	/* member of structure		*/
#define C_ARG		9	/* function argument		*/
#define C_STRTAG	10	/* structure tag		*/
#define C_MOU		11	/* member of union		*/
#define C_UNTAG		12	/* union tag			*/
#define C_TPDEF		13	/* type definition		*/
#define C_USTATIC	14	/* undefined static		*/
#define C_ENTAG		15	/* enumeration tag		*/
#define C_MOE		16	/* member of enumeration	*/
#define C_REGPARM	17	/* register parameter		*/
#define C_FIELD		18	/* bit field			*/
#define C_BLOCK		100	/* ".bb" or ".eb"		*/
#define C_FCN		101	/* ".bf" or ".ef"		*/
#define C_EOS		102	/* end of structure		*/
#define C_FILE		103	/* file name			*/
#define C_LINE		104	/* line # reformatted as symbol table entry */
#define C_ALIAS	 	105	/* duplicate tag		*/
#define C_HIDDEN	106	/* ext symbol in dmert public lib */

	/* New storage classes for 80960 */
 
#define C_SCALL		107	/* Procedure reachable via system call	*/
#define C_LEAFPROC	108	/* Leaf procedure, "call" via BAL	*/


/********************** SECTION HEADER **********************/

struct scnhdr {
	char		s_name[8];	/* section name			*/
	long		s_paddr;	/* physical address, aliased s_nlib */
	long		s_vaddr;	/* virtual address		*/
	long		s_size;		/* section size			*/
	long		s_scnptr;	/* file ptr to raw data for section */
	long		s_relptr;	/* file ptr to relocation	*/
	long		s_lnnoptr;	/* file ptr to line numbers	*/
	unsigned short	s_nreloc;	/* number of relocation entries	*/
	unsigned short	s_nlnno;	/* number of line number entries*/
	long		s_flags;	/* flags			*/
	unsigned long	s_align;	/* section alignment		*/
};

/*
 * names of "special" sections
 */
#define _TEXT	".text"
#define _DATA	".data"
#define _BSS	".bss"

/*
 * s_flags "type"
 */
#define	STYP_TEXT	0x20		/* section contains text only	*/
#define STYP_DATA	0x40		/* section contains data only	*/
#define STYP_BSS	0x80		/* section contains bss only	*/

#define	SCNHDR	struct scnhdr
#define	SCNHSZ	sizeof(SCNHDR)


/********************** LINE NUMBERS **********************/

/* 1 line number entry for every "breakpointable" source line in a section.
 * Line numbers are grouped on a per function basis; first entry in a function
 * grouping will have l_lnno = 0 and in place of physical address will be the
 * symbol table index of the function name.
 */
struct lineno{
	union {
		long l_symndx;	/* function name symbol index, iff l_lnno == 0*/
		long l_paddr;	/* (physical) address of line number	*/
	} l_addr;
	unsigned short	l_lnno;	/* line number		*/
	char padding[2];	/* force alignment	*/
};

#define	LINENO	struct lineno
#define	LINESZ	sizeof(LINENO) 


/********************** SYMBOLS **********************/

#define SYMNMLEN	8	/* # characters in a symbol name	*/
#define FILNMLEN	14	/* # characters in a file name		*/
#define DIMNUM		4	/* # array dimensions in auxiliary entry */


struct syment {
	union {
		char	_n_name[SYMNMLEN];	/* old COFF version	*/
		struct {
			long	_n_zeroes;	/* new == 0		*/
			long	_n_offset;	/* offset into string table */
		} _n_n;
		char	*_n_nptr[2];	/* allows for overlaying	*/
	} _n;
	long		n_value;	/* value of symbol		*/
	short		n_scnum;	/* section number		*/
	char		pad1[2];	/* force alignment		*/
	unsigned long	n_type;		/* type and derived type	*/
	char		n_sclass;	/* storage class		*/
	char		n_numaux;	/* number of aux. entries	*/
	char		pad2[2];	/* force alignment		*/
};

#define n_name		_n._n_name
#define n_zeroes	_n._n_n._n_zeroes
#define n_offset	_n._n_n._n_offset

/*
 * Relocatable symbols have number of the section in which they are defined,
 * or one of the following:
 */
#define N_UNDEF	0	/* undefined symbol				*/
#define N_ABS	-1	/* value of symbol is absolute			*/
#define N_DEBUG	-2	/* debugging symbol -- symbol value is meaningless */

/*
 * Type of a symbol, in low 4 bits of the word
 */
#define T_NULL		0
#define T_VOID		1	/* function argument (only used by compiler) */
#define T_CHAR		2	/* character		*/
#define T_SHORT		3	/* short integer	*/
#define T_INT		4	/* integer		*/
#define T_LONG		5	/* long integer		*/
#define T_FLOAT		6	/* floating point	*/
#define T_DOUBLE	7	/* double word		*/
#define T_STRUCT	8	/* structure 		*/
#define T_UNION		9	/* union 		*/
#define T_ENUM		10	/* enumeration 		*/
#define T_MOE		11	/* member of enumeration*/
#define T_UCHAR		12	/* unsigned character	*/
#define T_USHORT	13	/* unsigned short	*/
#define T_UINT		14	/* unsigned integer	*/
#define T_ULONG		15	/* unsigned long	*/
#define T_LNGDBL	16	/* long double		*/


/*
 * derived types
 */
#define DT_PTR		1	/* pointer	*/
#define DT_FCN		2	/* function	*/
#define DT_ARY		3	/* array	*/

#define N_BTMASK	037
#define N_TMASK		0140
#define N_BTSHFT	5
#define N_TSHIFT	2

#define BTYPE(x)	((x) & N_BTMASK)


#define ISPTR(x)	(((x) & N_TMASK) == (DT_PTR << N_BTSHFT))
#define ISFCN(x)	(((x) & N_TMASK) == (DT_FCN << N_BTSHFT))
#define ISARY(x)	(((x) & N_TMASK) == (DT_ARY << N_BTSHFT))

#define DECREF(x) ((((x)>>N_TSHIFT)&~N_BTMASK)|((x)&N_BTMASK))

union auxent {
	struct {
		long x_tagndx;	/* str, un, or enum tag indx */
		union {
			struct {
			    unsigned short x_lnno; /* declaration line number */
			    unsigned short x_size; /* str/union/array size */
			} x_lnsz;
			long x_fsize;	/* size of function */
		} x_misc;
		union {
			struct {		/* if ISFCN, tag, or .bb */
			    long x_lnnoptr;	/* ptr to fcn line # */
			    long x_endndx;	/* entry ndx past block end */
			} x_fcn;
			struct {		/* if ISARY, up to 4 dimen. */
			    unsigned short x_dimen[DIMNUM];
			} x_ary;
		} x_fcnary;
		unsigned short x_tvndx;		/* tv index */
	} x_sym;

	union {
		char x_fname[FILNMLEN];
		struct {
			long x_zeroes;
			long x_offset;
		} x_n;
	} x_file;

	struct {
		long x_scnlen;			/* section length */
		unsigned short x_nreloc;	/* # relocation entries */
		unsigned short x_nlinno;	/* # line numbers */
	} x_scn;

	struct {
		long x_stdindx;
	} x_sc;

	struct {
		unsigned long x_balntry;
	} x_bal;

	char a[sizeof(struct syment)];	/* force auxent/syment sizes to match */
};

#define	SYMENT	struct syment
#define	SYMESZ	sizeof(SYMENT)	
#define	AUXENT	union auxent
#define	AUXESZ	sizeof(AUXENT)

#if VAX || I960
#	define _ETEXT	"_etext"
#else
#	define _ETEXT	"etext"
#endif

/********************** RELOCATION DIRECTIVES **********************/

struct reloc {
	long r_vaddr;		/* Virtual address of reference */
	long r_symndx;		/* Index into symbol table	*/
	unsigned short r_type;	/* Relocation type		*/
	char pad[2];		/* Unused			*/
};

/* Only values of r_type GNU/960 cares about */
#define R_RELLONG	17	/* Direct 32-bit relocation		*/
#define R_IPRMED	25	/* 24-bit ip-relative relocation	*/
#define R_OPTCALL	27	/* 32-bit optimizable call (leafproc/sysproc) */


#define RELOC struct reloc
#define RELSZ sizeof(RELOC)
