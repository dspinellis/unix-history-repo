/*** coff information for M68K */


/********************** FILE HEADER **********************/

struct external_filehdr {
	char f_magic[2];	/* magic number			*/
	char f_nscns[2];	/* number of sections		*/
	char f_timdat[4];	/* time & date stamp		*/
	char f_symptr[4];	/* file pointer to symtab	*/
	char f_nsyms[4];	/* number of symtab entries	*/
	char f_opthdr[2];	/* sizeof(optional hdr)		*/
	char f_flags[2];	/* flags			*/
};


/* Bits for f_flags:
 *	F_RELFLG	relocation info stripped from file
 *	F_EXEC		file is executable (no unresolved external references)
 *	F_LNNO		line numbers stripped from file
 *	F_LSYMS		local symbols stripped from file
 *	F_AR32WR	file has byte ordering of an AR32WR machine (e.g. vax)
 */

#define F_RELFLG	(0x0001)
#define F_EXEC		(0x0002)
#define F_LNNO		(0x0004)
#define F_LSYMS		(0x0008)



#define	I386MAGIC	0x14c
#define I386BADMAG(x) (((x).f_magic!=I386MAGIC))


#define	FILHDR	struct external_filehdr
#define	FILHSZ	sizeof(FILHDR)


/********************** AOUT "OPTIONAL HEADER" **********************/


typedef struct 
{
  char 	magic[2];		/* type of file				*/
  char	vstamp[2];		/* version stamp			*/
  char	tsize[4];		/* text size in bytes, padded to FW bdry*/
  char	dsize[4];		/* initialized data "  "		*/
  char	bsize[4];		/* uninitialized data "   "		*/
  char	entry[4];		/* entry pt.				*/
  char 	text_start[4];		/* base of text used for this file */
  char 	data_start[4];		/* base of data used for this file */
}
AOUTHDR;


#define AOUTSZ (sizeof(AOUTHDR))

#define OMAGIC          0404    /* object files, eg as output */
#define ZMAGIC          0413    /* demand load format, eg normal ld output */
#define STMAGIC		0401	/* target shlib */
#define SHMAGIC		0443	/* host   shlib */


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
#define C_AUTOARG	19	/* auto argument		*/
#define C_LASTENT	20	/* dummy entry (end of block)	*/
#define C_BLOCK		100	/* ".bb" or ".eb"		*/
#define C_FCN		101	/* ".bf" or ".ef"		*/
#define C_EOS		102	/* end of structure		*/
#define C_FILE		103	/* file name			*/
#define C_LINE		104	/* line # reformatted as symbol table entry */
#define C_ALIAS	 	105	/* duplicate tag		*/
#define C_HIDDEN	106	/* ext symbol in dmert public lib */

/********************** SECTION HEADER **********************/


struct external_scnhdr {
	char		s_name[8];	/* section name			*/
	char		s_paddr[4];	/* physical address, aliased s_nlib */
	char		s_vaddr[4];	/* virtual address		*/
	char		s_size[4];	/* section size			*/
	char		s_scnptr[4];	/* file ptr to raw data for section */
	char		s_relptr[4];	/* file ptr to relocation	*/
	char		s_lnnoptr[4];	/* file ptr to line numbers	*/
	char		s_nreloc[2];	/* number of relocation entries	*/
	char		s_nlnno[2];	/* number of line number entries*/
	char		s_flags[4];	/* flags			*/
};

/*
 * names of "special" sections
 */
#define _TEXT	".text"
#define _DATA	".data"
#define _BSS	".bss"
#define _COMMENT ".comment"

/*
 * s_flags "type"
 */
#define STYP_REG	 (0x0000) /* "regular": allocated, relocated, loaded */
#define STYP_DSECT	 (0x0001) /* "dummy":  relocated only*/
#define STYP_NOLOAD	 (0x0002) /* "noload": allocated, relocated, not loaded */
#define STYP_GROUP	 (0x0004) /* "grouped": formed of input sections */
#define STYP_PAD	 (0x0008) /* "padding": not allocated, not relocated, loaded */
#define STYP_COPY	 (0x0010) /* "copy": for decision function used by field update;  not allocated, not relocated,
				     loaded; reloc & lineno entries processed normally */
#define STYP_TEXT	 (0x0020) /* section contains text only */
#define S_SHRSEG	 (0x0020) /* In 3b Update files (output of ogen), sections which appear in SHARED segments of the Pfile
				     will have the S_SHRSEG flag set by ogen, to inform dufr that updating 1 copy of the proc. will
				     update all process invocations. */
#define STYP_DATA	 (0x0040) /* section contains data only */
#define STYP_BSS	 (0x0080) /* section contains bss only */
#define S_NEWFCN	 (0x0100) /* In a minimal file or an update file, a new function (as compared with a replaced function) */
#define STYP_INFO	 (0x0200) /* comment: not allocated not relocated, not loaded */
#define STYP_OVER	 (0x0400) /* overlay: relocated not allocated or loaded */
#define STYP_LIB	 (0x0800) /* for .lib: same as INFO */
#define STYP_MERGE	 (0x2000) /* merge section -- combines with text, data or bss sections only */
#define STYP_REVERSE_PAD (0x4000) /* section will be padded with no-op instructions wherever padding is necessary and there is a
				     word of contiguous bytes beginning on a word boundary. */

#define	SCNHDR	struct external_scnhdr
#define	SCNHSZ	sizeof(SCNHDR)


/********************** LINE NUMBERS **********************/

/* 1 line number entry for every "breakpointable" source line in a section.
 * Line numbers are grouped on a per function basis; first entry in a function
 * grouping will have l_lnno = 0 and in place of physical address will be the
 * symbol table index of the function name.
 */
struct external_lineno {
	union {
		char l_symndx[4];	/* function name symbol index, iff l_lnno == 0*/
		char l_paddr[4];	/* (physical) address of line number	*/
	} l_addr;
	char l_lnno[2];	/* line number		*/
};


#define	LINENO	struct external_lineno
#define	LINESZ	6


/********************** SYMBOLS **********************/

#define E_SYMNMLEN	8	/* # characters in a symbol name	*/
#define E_FILNMLEN	14	/* # characters in a file name		*/
#define E_DIMNUM	4	/* # array dimensions in auxiliary entry */

struct external_syment 
{
  union {
    char e_name[E_SYMNMLEN];
    struct {
      char e_zeroes[4];
      char e_offset[4];
    } e;
  } e;
  char e_value[4];
  char e_scnum[2];
  char e_type[2];
  char e_sclass[1];
  char e_numaux[1];
};


/*#define n_name		_n._n_name
#define n_ptr		_n._n_nptr[1]
#define n_zeroes	_n._n_n._n_zeroes
#define n_offset	_n._n_n._n_offset
*/

/*
 * Relocatable symbols have number of the section in which they are defined,
 * or one of the following:
 */
#define N_UNDEF	((short)0)  /* undefined symbol */
#define N_ABS	((short)-1) /* value of symbol is absolute */
#define N_DEBUG	((short)-2) /* debugging symbol -- value is meaningless */
#define N_TV	((short)-3) /* indicates symbol needs preload transfer vector */
#define P_TV	((short)-4) /* indicates symbol needs postload transfer vector*/

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
 * derived types, in n_type
*/
#define DT_NON		(0)	/* no derived type */
#define DT_PTR		(1)	/* pointer */
#define DT_FCN		(2)	/* function */
#define DT_ARY		(3)	/* array */

#define N_BTMASK	(0xf)
#define N_TMASK		(0x30)
#define N_BTSHFT	(4)
#define N_TSHIFT	(2)
  
#define BTYPE(x)	((x) & N_BTMASK)

#define ISPTR(x)	(((x) & N_TMASK) == (DT_PTR << N_BTSHFT))
#define ISFCN(x)	(((x) & N_TMASK) == (DT_FCN << N_BTSHFT))
#define ISARY(x)	(((x) & N_TMASK) == (DT_ARY << N_BTSHFT))

#define DECREF(x) ((((x)>>N_TSHIFT)&~N_BTMASK)|((x)&N_BTMASK))

union external_auxent {
	struct {
		char x_tagndx[4];	/* str, un, or enum tag indx */
		union {
			struct {
			    char  x_lnno[2]; /* declaration line number */
			    char  x_size[2]; /* str/union/array size */
			} x_lnsz;
			char x_fsize[4];	/* size of function */
		} x_misc;
		union {
			struct {		/* if ISFCN, tag, or .bb */
			    char x_lnnoptr[4];	/* ptr to fcn line # */
			    char x_endndx[4];	/* entry ndx past block end */
			} x_fcn;
			struct {		/* if ISARY, up to 4 dimen. */
			    char x_dimen[E_DIMNUM][2];
			} x_ary;
		} x_fcnary;
		char x_tvndx[2];		/* tv index */
	} x_sym;

	union {
		char x_fname[E_FILNMLEN];
		struct {
			char x_zeroes[4];
			char x_offset[4];
		} x_n;
	} x_file;

	struct {
		char x_scnlen[4];			/* section length */
		char x_nreloc[2];	/* # relocation entries */
		char x_nlinno[2];	/* # line numbers */
	} x_scn;

        struct {
		char x_tvfill[4];	/* tv fill value */
		char x_tvlen[2];	/* length of .tv */
		char x_tvran[2][2];	/* tv range */
	} x_tv;		/* info about .tv section (in auxent of symbol .tv)) */


};

#define	SYMENT	struct external_syment
#define	SYMESZ	18	
#define	AUXENT	union external_auxent
#define	AUXESZ	18


#	define _ETEXT	"etext"


/********************** RELOCATION DIRECTIVES **********************/



struct external_reloc {
  char r_vaddr[4];
  char r_symndx[4];
  char r_type[2];
};


#define RELOC struct external_reloc
#define RELSZ 10

#define DEFAULT_DATA_SECTION_ALIGNMENT 4
#define DEFAULT_BSS_SECTION_ALIGNMENT 4
#define DEFAULT_TEXT_SECTION_ALIGNMENT 4
/* For new sections we havn't heard of before */
#define DEFAULT_SECTION_ALIGNMENT 4
