/* Rudimentary ECOFF support on MIPS machines. 
   This lacks symbol information, normally provided on MIPS Unix systems
   in the files <sym.h> and <symconst.h>.  */

/********************** FILE HEADER **********************/

struct external_filehdr {
  unsigned char f_magic[2];	/* magic number			*/
  unsigned char f_nscns[2];	/* number of sections		*/
  unsigned char f_timdat[4];	/* time & date stamp		*/
  unsigned char f_symptr[4];	/* file pointer to symtab	*/
  unsigned char f_nsyms[4];	/* number of symtab entries	*/
  unsigned char f_opthdr[2];	/* sizeof(optional hdr)		*/
  unsigned char f_flags[2];	/* flags			*/
};


/* Mips magics */
#define MIPS_MAGIC_1 0x0180
#define MIPS_MAGIC_2 0x0162
#define MIPS_MAGIC_3 0x0160

#define ECOFFBADMAG(x) (((x).f_magic!=MIPS_MAGIC_1) && \
			((x).f_magic!=MIPS_MAGIC_2) &&\
			((x).f_magic!=MIPS_MAGIC_3))


#define	FILHDR	struct external_filehdr
#define	FILHSZ	20

/********************** AOUT "OPTIONAL HEADER" **********************/


typedef struct external_aouthdr
{
  unsigned char magic[2];	/* type of file				*/
  unsigned char	vstamp[2];	/* version stamp			*/
  unsigned char	tsize[4];	/* text size in bytes, padded to FW bdry*/
  unsigned char	dsize[4];	/* initialized data "  "		*/
  unsigned char	bsize[4];	/* uninitialized data "   "		*/
  unsigned char	entry[4];	/* entry pt.				*/
  unsigned char text_start[4];	/* base of text used for this file */
  unsigned char data_start[4];	/* base of data used for this file */
} AOUTHDR;

/* compute size of a header */

#define AOUTSZ (sizeof(AOUTHDR))


/********************** SECTION HEADER **********************/

struct external_scnhdr {
  unsigned char	s_name[8];	/* section name			*/
  unsigned char	s_paddr[4];	/* physical address, aliased s_nlib */
  unsigned char	s_vaddr[4];	/* virtual address		*/
  unsigned char	s_size[4];	/* section size			*/
  unsigned char	s_scnptr[4];	/* file ptr to raw data for section */
  unsigned char	s_relptr[4];	/* file ptr to relocation	*/
  unsigned char	s_lnnoptr[4];	/* file ptr to line numbers	*/
  unsigned char	s_nreloc[2];	/* number of relocation entries	*/
  unsigned char	s_nlnno[2];	/* number of line number entries*/
  unsigned char	s_flags[4];	/* flags			*/
};

#define	SCNHDR	struct external_scnhdr
#define	SCNHSZ	sizeof(SCNHDR)

/*
 * names of "special" sections
 */
#define _TEXT   ".text"
#define _DATA   ".data"
#define _BSS    ".bss"

#define DEFAULT_DATA_SECTION_ALIGNMENT 4
#define DEFAULT_BSS_SECTION_ALIGNMENT 4
#define DEFAULT_TEXT_SECTION_ALIGNMENT 16
/* For new sections we havn't heard of before */
#define DEFAULT_SECTION_ALIGNMENT 4

/********************** RELOCATION DIRECTIVES **********************/

struct external_reloc {
  unsigned char r_vaddr[4];
  unsigned char r_symndx[4];
  unsigned char r_type[2];
  unsigned char pad[2];
};


/* Relevent values for r_type and ecoff.  Would someone please document them */

#define RELOC struct external_reloc
#define RELSZ 12
