/* ELF executable support for BFD.
   Copyright 1991, 1992 Free Software Foundation, Inc.

   Written by Fred Fish @ Cygnus Support, from information published
   in "UNIX System V Release 4, Programmers Guide: ANSI C and
   Programming Support Tools".  Sufficient support for gdb.

   Rewritten by Mark Eichin @ Cygnus Support, from information
   published in "System V Application Binary Interface", chapters 4
   and 5, as well as the various "Processor Supplement" documents
   derived from it. Added support for assembler and other object file
   utilities.
   
This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


	/****************************************

	  		WARNING

	This is only a partial ELF implementation,
	incorporating only those parts that are
	required to get gdb up and running.  It is
	expected that it will be expanded to a full
	ELF implementation at some future date.

	Unimplemented stubs call abort() to ensure
	that they get proper attention if they are
	ever called.  The stubs are here since
	this version was hacked from the COFF
	version, and thus they will probably
	go away or get expanded appropriately in a
	future version.

	fnf@cygnus.com

	*****************************************/


/* Problems and other issues to resolve.

   (1)	BFD expects there to be some fixed number of "sections" in
        the object file.  I.E. there is a "section_count" variable in the
	bfd structure which contains the number of sections.  However, ELF
	supports multiple "views" of a file.  In particular, with current
	implementations, executable files typically have two tables, a
	program header table and a section header table, both of which
	partition the executable.

	In ELF-speak, the "linking view" of the file uses the section header
	table to access "sections" within the file, and the "execution view"
	uses the program header table to access "segments" within the file.
	"Segments" typically may contain all the data from one or more
	"sections".

	Note that the section header table is optional in ELF executables,
	but it is this information that is most useful to gdb.  If the
	section header table is missing, then gdb should probably try
	to make do with the program header table.  (FIXME)

*/

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "obstack.h"
#include "elf/common.h"
#include "elf/internal.h"
#include "elf/external.h"

#ifdef HAVE_PROCFS	/* Some core file support requires host /proc files */
#include <sys/procfs.h>
#else
#define bfd_prstatus(abfd, descdata, descsz, filepos)	/* Define away */
#define bfd_fpregset(abfd, descdata, descsz, filepos)	/* Define away */
#define bfd_prpsinfo(abfd, descdata, descsz, filepos)	/* Define away */
#endif

/* Forward data declarations */

extern bfd_target elf_little_vec, elf_big_vec;

/* Currently the elf_symbol_type struct just contains the generic bfd
   symbol structure. */

typedef struct
{
  asymbol symbol;
} elf_symbol_type;

/* Some private data is stashed away for future use using the tdata pointer
   in the bfd structure.  */

struct elf_obj_tdata
{
  Elf_Internal_Ehdr elf_header[1];	/* Actual data, but ref like ptr */
  Elf_Internal_Shdr *elf_sect_ptr;
  struct strtab     *strtab_ptr;
  int		    symtab_section;
  void 		    *prstatus;		/* The raw /proc prstatus structure */
  void 		    *prpsinfo;		/* The raw /proc prpsinfo structure */
};

#define elf_tdata(bfd)		((bfd) -> tdata.elf_obj_data)
#define elf_elfheader(bfd)	(elf_tdata(bfd) -> elf_header)
#define elf_elfsections(bfd)	(elf_tdata(bfd) -> elf_sect_ptr)
#define elf_shstrtab(bfd)	(elf_tdata(bfd) -> strtab_ptr)
#define elf_onesymtab(bfd)	(elf_tdata(bfd) -> symtab_section)
#define core_prpsinfo(bfd)	(elf_tdata(bfd) -> prpsinfo)
#define core_prstatus(bfd)	(elf_tdata(bfd) -> prstatus)

/* Translate an ELF symbol in external format into an ELF symbol in internal
   format. */

static void
DEFUN(elf_swap_symbol_in,(abfd, src, dst),
      bfd               *abfd AND
      Elf_External_Sym *src AND
      Elf_Internal_Sym *dst)
{
  dst -> st_name = bfd_h_get_32 (abfd, (bfd_byte *) src -> st_name);
  dst -> st_value = bfd_h_get_32 (abfd, (bfd_byte *) src -> st_value);
  dst -> st_size = bfd_h_get_32 (abfd, (bfd_byte *) src -> st_size);
  dst -> st_info = bfd_h_get_8 (abfd, (bfd_byte *) src -> st_info);
  dst -> st_other = bfd_h_get_8 (abfd, (bfd_byte *) src -> st_other);
  dst -> st_shndx = bfd_h_get_16 (abfd, (bfd_byte *) src -> st_shndx);
}

/* Translate an ELF symbol in internal format into an ELF symbol in external
   format. */

static void
DEFUN(elf_swap_symbol_out,(abfd, src, dst),
      bfd               *abfd AND
      Elf_Internal_Sym *src AND
      Elf_External_Sym *dst)
{
  bfd_h_put_32 (abfd, src->st_name, dst->st_name);
  bfd_h_put_32 (abfd, src->st_value, dst->st_value);
  bfd_h_put_32 (abfd, src->st_size, dst->st_size);
  bfd_h_put_8  (abfd, src->st_info, dst->st_info);
  bfd_h_put_8  (abfd, src->st_other, dst->st_other);
  bfd_h_put_16 (abfd, src->st_shndx, dst->st_shndx);
}


/* Translate an ELF file header in external format into an ELF file header in
   internal format. */

static void
DEFUN(elf_swap_ehdr_in,(abfd, src, dst),
      bfd               *abfd AND
      Elf_External_Ehdr *src AND
      Elf_Internal_Ehdr *dst)
{
  memcpy (dst -> e_ident, src -> e_ident, EI_NIDENT);
  dst -> e_type = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_type);
  dst -> e_machine = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_machine);
  dst -> e_version = bfd_h_get_32 (abfd, (bfd_byte *) src -> e_version);
  dst -> e_entry = bfd_h_get_32 (abfd, (bfd_byte *) src -> e_entry);
  dst -> e_phoff = bfd_h_get_32 (abfd, (bfd_byte *) src -> e_phoff);
  dst -> e_shoff = bfd_h_get_32 (abfd, (bfd_byte *) src -> e_shoff);
  dst -> e_flags = bfd_h_get_32 (abfd, (bfd_byte *) src -> e_flags);
  dst -> e_ehsize = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_ehsize);
  dst -> e_phentsize = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_phentsize);
  dst -> e_phnum = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_phnum);
  dst -> e_shentsize = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_shentsize);
  dst -> e_shnum = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_shnum);
  dst -> e_shstrndx = bfd_h_get_16 (abfd, (bfd_byte *) src -> e_shstrndx);
}

/* Translate an ELF file header in internal format into an ELF file header in
   external format. */

static void
DEFUN(elf_swap_ehdr_out,(abfd, src, dst),
      bfd               *abfd AND
      Elf_Internal_Ehdr *src AND
      Elf_External_Ehdr *dst)
{
  memcpy (dst -> e_ident, src -> e_ident, EI_NIDENT);
  /* note that all elements of dst are *arrays of unsigned char* already... */
  bfd_h_put_16 (abfd, src->e_type, dst->e_type);
  bfd_h_put_16 (abfd, src->e_machine, dst->e_machine);
  bfd_h_put_32 (abfd, src->e_version, dst->e_version);
  bfd_h_put_32 (abfd, src->e_entry, dst->e_entry);
  bfd_h_put_32 (abfd, src->e_phoff, dst->e_phoff);
  bfd_h_put_32 (abfd, src->e_shoff, dst->e_shoff);
  bfd_h_put_32 (abfd, src->e_flags, dst->e_flags);
  bfd_h_put_16 (abfd, src->e_ehsize, dst->e_ehsize);
  bfd_h_put_16 (abfd, src->e_phentsize, dst->e_phentsize);
  bfd_h_put_16 (abfd, src->e_phnum, dst->e_phnum);
  bfd_h_put_16 (abfd, src->e_shentsize, dst->e_shentsize);
  bfd_h_put_16 (abfd, src->e_shnum, dst->e_shnum);
  bfd_h_put_16 (abfd, src->e_shstrndx, dst->e_shstrndx);
}


/* Translate an ELF section header table entry in external format into an
   ELF section header table entry in internal format. */

static void
DEFUN(elf_swap_shdr_in,(abfd, src, dst),
      bfd               *abfd AND
      Elf_External_Shdr *src AND
      Elf_Internal_Shdr *dst)
{
  dst->sh_name = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_name);
  dst->sh_type = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_type);
  dst->sh_flags = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_flags);
  dst->sh_addr = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_addr);
  dst->sh_offset = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_offset);
  dst->sh_size = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_size);
  dst->sh_link = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_link);
  dst->sh_info = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_info);
  dst->sh_addralign = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_addralign);
  dst->sh_entsize = bfd_h_get_32 (abfd, (bfd_byte *) src->sh_entsize);
  /* we haven't done any processing on it yet, so... */
  dst->rawdata = (void*)0;
}

/* Translate an ELF section header table entry in internal format into an
   ELF section header table entry in external format. */

static void
DEFUN(elf_swap_shdr_out,(abfd, src, dst),
      bfd               *abfd AND
      Elf_Internal_Shdr *src AND
      Elf_External_Shdr *dst)
{
  /* note that all elements of dst are *arrays of unsigned char* already... */
  bfd_h_put_32 (abfd, src->sh_name, dst->sh_name);
  bfd_h_put_32 (abfd, src->sh_type, dst->sh_type);
  bfd_h_put_32 (abfd, src->sh_flags, dst->sh_flags);
  bfd_h_put_32 (abfd, src->sh_addr, dst->sh_addr);
  bfd_h_put_32 (abfd, src->sh_offset, dst->sh_offset);
  bfd_h_put_32 (abfd, src->sh_size, dst->sh_size);
  bfd_h_put_32 (abfd, src->sh_link, dst->sh_link);
  bfd_h_put_32 (abfd, src->sh_info, dst->sh_info);
  bfd_h_put_32 (abfd, src->sh_addralign, dst->sh_addralign);
  bfd_h_put_32 (abfd, src->sh_entsize, dst->sh_entsize);
}


/* Translate an ELF program header table entry in external format into an
   ELF program header table entry in internal format. */

static void
DEFUN(elf_swap_phdr_in,(abfd, src, dst),
      bfd               *abfd AND
      Elf_External_Phdr *src AND
      Elf_Internal_Phdr *dst)
{
  dst->p_type = bfd_h_get_32 (abfd, (bfd_byte *) src->p_type);
  dst->p_offset = bfd_h_get_32 (abfd, (bfd_byte *) src->p_offset);
  dst->p_vaddr = bfd_h_get_32 (abfd, (bfd_byte *) src->p_vaddr);
  dst->p_paddr = bfd_h_get_32 (abfd, (bfd_byte *) src->p_paddr);
  dst->p_filesz = bfd_h_get_32 (abfd, (bfd_byte *) src->p_filesz);
  dst->p_memsz = bfd_h_get_32 (abfd, (bfd_byte *) src->p_memsz);
  dst->p_flags = bfd_h_get_32 (abfd, (bfd_byte *) src->p_flags);
  dst->p_align = bfd_h_get_32 (abfd, (bfd_byte *) src->p_align);
}


/* Translate an ELF reloc from external format to internal format. */
static void
DEFUN(elf_swap_reloc_in,(abfd, src, dst),
      bfd            *abfd AND
      Elf_External_Rel *src AND
      Elf_Internal_Rel *dst)
{
  dst->r_offset = bfd_h_get_32 (abfd, (bfd_byte *) src->r_offset);
  dst->r_info = bfd_h_get_32 (abfd, (bfd_byte *) src->r_info);
}

static void
DEFUN(elf_swap_reloca_in,(abfd, src, dst),
      bfd            *abfd AND
      Elf_External_Rela *src AND
      Elf_Internal_Rela *dst)
{
  dst->r_offset = bfd_h_get_32 (abfd, (bfd_byte *) src->r_offset);
  dst->r_info = bfd_h_get_32 (abfd, (bfd_byte *) src->r_info);
  dst->r_addend = bfd_h_get_32 (abfd, (bfd_byte *) src->r_addend);
}

/* Translate an ELF reloc from internal format to external format. */
static void
DEFUN(elf_swap_reloc_out,(abfd, src, dst),
      bfd            *abfd AND
      Elf_Internal_Rel *src AND
      Elf_External_Rel *dst)
{
  bfd_h_put_32 (abfd, src->r_offset, dst->r_offset);
  bfd_h_put_32 (abfd, src->r_info, dst->r_info);
}

static void
DEFUN(elf_swap_reloca_out,(abfd, src, dst),
      bfd            *abfd AND
      Elf_Internal_Rela *src AND
      Elf_External_Rela *dst)
{
  bfd_h_put_32 (abfd, src->r_offset, dst->r_offset);
  bfd_h_put_32 (abfd, src->r_info, dst->r_info);
  bfd_h_put_32 (abfd, src->r_addend, dst->r_addend);
}

static char *EXFUN(elf_read, (bfd *, long, int));
static struct sec * EXFUN(section_from_elf_index, (bfd *, int));
static int EXFUN(elf_section_from_bfd_section, (bfd *, struct sec *));
static boolean EXFUN(elf_slurp_symbol_table, (bfd *, asymbol **));
static void EXFUN(elf_info_to_howto, (bfd *, arelent *, Elf_Internal_Rela *));
static char *EXFUN(elf_get_str_section, (bfd *, unsigned int));
     
/* 
INTERNAL_FUNCTION
	bfd_elf_find_section

SYNOPSIS
	struct elf_internal_shdr *bfd_elf_find_section (bfd *abfd, char *name);

DESCRIPTION
	Helper functions for GDB to locate the string tables.
	Since BFD hides string tables from callers, GDB needs to use an
	internal hook to find them.  Sun's .stabstr, in particular,
	isn't even pointed to by the .stab section, so ordinary
	mechanisms wouldn't work to find it, even if we had some.
*/

struct elf_internal_shdr *
DEFUN(bfd_elf_find_section, (abfd, name),
      bfd		*abfd AND
      char		*name)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  char *shstrtab = elf_get_str_section (abfd, elf_elfheader (abfd)->e_shstrndx);
  unsigned int max = elf_elfheader (abfd)->e_shnum;
  unsigned int i;

  for (i = 1; i < max; i++)
    if (!strcmp (&shstrtab[i_shdrp[i].sh_name], name))
      return &i_shdrp[i];
  return 0;
}

/* End of GDB support.  */

static char *
DEFUN(elf_get_str_section, (abfd, shindex),
      bfd		*abfd AND
      unsigned int	shindex)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  unsigned int shstrtabsize = i_shdrp[shindex].sh_size;
  unsigned int offset = i_shdrp[shindex].sh_offset;
  char *shstrtab = i_shdrp[shindex].rawdata;

  if (shstrtab)
    return shstrtab;

  if ((shstrtab = elf_read (abfd, offset, shstrtabsize)) == NULL)
    {
      return (NULL);
    }
  i_shdrp[shindex].rawdata = (void*)shstrtab;
  return shstrtab;
}

static char *
DEFUN(elf_string_from_elf_section, (abfd, shindex, strindex),
      bfd		*abfd AND
      unsigned int	shindex AND
      unsigned int	strindex)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  Elf_Internal_Shdr *hdr = i_shdrp + shindex;

  if (! hdr->rawdata)
    {
      if (elf_get_str_section (abfd, shindex) == NULL)
	{
	  return NULL;
	}
    }
  return ((char*)hdr->rawdata)+strindex;
}

#define elf_string_from_elf_strtab(abfd, strindex) \
  elf_string_from_elf_section (abfd, elf_elfheader(abfd)->e_shstrndx, strindex)

/* Create a new bfd section from an ELF section header. */

static boolean
DEFUN(bfd_section_from_shdr, (abfd, shindex),
      bfd               *abfd AND
      unsigned int	shindex)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  Elf_Internal_Shdr *hdr = i_shdrp + shindex;
  asection *newsect;
  char *name;

  name = hdr->sh_name ?
    elf_string_from_elf_strtab (abfd, hdr->sh_name) : "unnamed";

  switch(hdr->sh_type) {

  case SHT_NULL:
    /* inactive section. Throw it away. */
    return true;

  case SHT_PROGBITS:
  case SHT_NOBITS:
    /* Bits that get saved. This one is real. */
    if (! hdr->rawdata ) 
      {
	newsect = bfd_make_section (abfd, name);
	newsect->vma = hdr->sh_addr;
	newsect->_raw_size = hdr->sh_size;
	newsect->filepos = hdr->sh_offset; /* so we can read back the bits */
	newsect->flags |= SEC_HAS_CONTENTS;
	
	if (hdr->sh_flags & SHF_ALLOC)
	  {
	    newsect->flags |= SEC_ALLOC;
	    if (hdr->sh_type != SHT_NOBITS)
	      newsect->flags |= SEC_LOAD;
	  }
	
	if (!(hdr->sh_flags & SHF_WRITE))
	  newsect->flags |= SEC_READONLY;
	
	if (hdr->sh_flags & SHF_EXECINSTR)
	  newsect->flags |= SEC_CODE;	/* FIXME: may only contain SOME code */
	else
	  newsect->flags |= SEC_DATA;
	
	hdr->rawdata = (void*)newsect;
      }
    return true;
    break;

  case SHT_SYMTAB:			/* A symbol table */
    BFD_ASSERT (hdr->sh_entsize == sizeof (Elf_External_Sym));
    elf_onesymtab (abfd) = shindex;
    abfd->flags |= HAS_SYMS;
    return true;

  case SHT_STRTAB:			/* A string table */
    return true;

  case SHT_REL:
  case SHT_RELA:
    /* *these* do a lot of work -- but build no sections! */
    /* the spec says there can be multiple strtabs, but only one symtab */
    /* but there can be lots of REL* sections. */
    /* FIXME:  The above statement is wrong!  There are typically at least
       two symbol tables in a dynamically linked executable, ".dynsym"
       which is the dynamic linkage symbol table and ".symtab", which is
       the "traditional" symbol table.  -fnf */
       
    {
      asection		*target_sect;
      
      bfd_section_from_shdr (abfd, hdr->sh_link); /* symbol table */
      bfd_section_from_shdr (abfd, hdr->sh_info); /* target */
      target_sect = section_from_elf_index (abfd, hdr->sh_info);
      if (target_sect == NULL)
	  return false;

#if 0
      /* FIXME:  We are only prepared to read one symbol table, so
	 do NOT read the dynamic symbol table since it is only a
	 subset of the full symbol table.  Also see comment above. -fnf */
      if (!elf_slurp_symbol_table(abfd, i_shdrp + hdr->sh_link))
	return false;
#endif

      target_sect->reloc_count = hdr->sh_size / hdr->sh_entsize;
      target_sect->flags |= SEC_RELOC;
      target_sect->relocation = 0;
      target_sect->rel_filepos = hdr->sh_offset;
      return true;
    }
    break;

  case SHT_HASH:
  case SHT_DYNAMIC:
  case SHT_DYNSYM:		/* could treat this like symtab... */
#if 0
    fprintf(stderr, "Dynamic Linking sections not yet supported.\n");
    abort ();
#endif
    break;

  case SHT_NOTE:
#if 0
    fprintf(stderr, "Note Sections not yet supported.\n");
    abort ();
#endif
    break;

  case SHT_SHLIB:
#if 0
    fprintf(stderr, "SHLIB Sections not supported (and non conforming.)\n");
#endif
    return true;

  default:
    break;
  }
  
  return (true);
}




struct strtab {
  char *tab;
  int nentries;
  int length;
};


static struct strtab *
DEFUN(bfd_new_strtab, (abfd),
      bfd		*abfd)
{
  struct strtab *ss;

  ss = (struct strtab *)malloc(sizeof(struct strtab));
  ss->tab = malloc(1);
  BFD_ASSERT(ss->tab != 0);
  *ss->tab = 0;
  ss->nentries = 0;
  ss->length = 1;

  return ss;
}

static int
DEFUN(bfd_add_to_strtab, (abfd, ss, str),
      bfd		*abfd AND
      struct strtab	*ss AND
      CONST char	*str)
{
  /* should search first, but for now: */
  /* include the trailing NUL */
  int ln = strlen(str)+1;
  
  /* should this be using obstacks? */
  ss->tab = realloc(ss->tab, ss->length + ln);

  BFD_ASSERT(ss->tab != 0);
  strcpy(ss->tab + ss->length, str);
  ss->nentries++;
  ss->length += ln;

  return ss->length - ln;
}

static int
DEFUN(bfd_add_2_to_strtab, (abfd, ss, str, str2),
      bfd		*abfd AND
      struct strtab	*ss AND
      char		*str AND
      CONST char	*str2)
{
  /* should search first, but for now: */
  /* include the trailing NUL */
  int ln = strlen(str)+strlen(str2)+1;
  
  /* should this be using obstacks? */
  if (ss->length)
    ss->tab = realloc(ss->tab, ss->length + ln);
  else 
    ss->tab = malloc(ln);

  BFD_ASSERT(ss->tab != 0);
  strcpy(ss->tab + ss->length, str);
  strcpy(ss->tab + ss->length + strlen(str), str2);
  ss->nentries++;
  ss->length += ln;

  return ss->length - ln;
}

/* Create a new ELF section from a bfd section. */

static boolean
DEFUN(bfd_shdr_from_section, (abfd, hdr, shstrtab, indx),
      bfd               *abfd AND
      Elf_Internal_Shdr *hdr AND
      struct strtab	*shstrtab AND
      int		indx)
{
  asection *sect;
  int ndx;
  
  /* figure out out to write the section name from the bfd section name. MWE */
  	      
  sect = abfd->sections;
  for (ndx = indx; --ndx; )
    {
      sect = sect->next;
    }
  hdr[indx].sh_name = bfd_add_to_strtab(abfd, shstrtab,
					bfd_section_name(abfd, sect));
  hdr[indx].sh_addr = sect->vma;
  hdr[indx].sh_size = sect->_raw_size;
  hdr[indx].sh_flags = 0;
  /* these need to be preserved on */
  hdr[indx].sh_link = 0;
  hdr[indx].sh_info = 0;
  hdr[indx].sh_addralign = 0;
  hdr[indx].sh_entsize = 0;

  hdr[indx].sh_type = 0;
  if (sect->flags & SEC_RELOC) {
    hdr[indx].sh_type = SHT_RELA; /* FIXME -- sparc specific */
  }
  
  if (sect->flags & SEC_HAS_CONTENTS)
    {
      hdr[indx].sh_offset = sect->filepos;
      hdr[indx].sh_size = sect->_raw_size;
    }
  if (sect->flags & SEC_ALLOC)
    {
      hdr[indx].sh_flags |= SHF_ALLOC;
      if (sect->flags & SEC_LOAD)
	{
	  /* do something with sh_type ? */
	}
    }
  if (!(sect->flags & SEC_READONLY)) 
    hdr[indx].sh_flags |= SHF_WRITE;

  if (sect->flags & SEC_CODE)
    hdr[indx].sh_flags |= SHF_EXECINSTR;

  return (true);
}

/* Create a new bfd section from an ELF program header.

   Since program segments have no names, we generate a synthetic name
   of the form segment<NUM>, where NUM is generally the index in the
   program header table.  For segments that are split (see below) we
   generate the names segment<NUM>a and segment<NUM>b.

   Note that some program segments may have a file size that is different than
   (less than) the memory size.  All this means is that at execution the
   system must allocate the amount of memory specified by the memory size,
   but only initialize it with the first "file size" bytes read from the
   file.  This would occur for example, with program segments consisting
   of combined data+bss.

   To handle the above situation, this routine generates TWO bfd sections
   for the single program segment.  The first has the length specified by
   the file size of the segment, and the second has the length specified
   by the difference between the two sizes.  In effect, the segment is split
   into it's initialized and uninitialized parts.

 */

static boolean
DEFUN(bfd_section_from_phdr, (abfd, hdr, index),
      bfd               *abfd AND
      Elf_Internal_Phdr *hdr AND
      int		 index)
{
  asection *newsect;
  char *name;
  char namebuf[64];
  int split;

  split = ((hdr -> p_memsz > 0) &&
	   (hdr -> p_filesz > 0) &&
	   (hdr -> p_memsz > hdr -> p_filesz));
  sprintf (namebuf, split ? "segment%da" : "segment%d", index);
  name = bfd_alloc (abfd, strlen (namebuf) + 1);
  strcpy (name, namebuf);
  newsect = bfd_make_section (abfd, name);
  newsect -> vma = hdr -> p_vaddr;
  newsect -> _raw_size = hdr -> p_filesz;
  newsect -> filepos = hdr -> p_offset;
  newsect -> flags |= SEC_HAS_CONTENTS;
  if (hdr -> p_type == PT_LOAD)
    {
      newsect -> flags |= SEC_ALLOC;
      newsect -> flags |= SEC_LOAD;
      if (hdr -> p_flags & PF_X)
	{
	  /* FIXME: all we known is that it has execute PERMISSION,
	     may be data. */
	  newsect -> flags |= SEC_CODE;
	}
    }
  if (!(hdr -> p_flags & PF_W))
    {
      newsect -> flags |= SEC_READONLY;
    }

  if (split)
    {
      sprintf (namebuf, "segment%db", index);
      name = bfd_alloc (abfd, strlen (namebuf) + 1);
      strcpy (name, namebuf);
      newsect = bfd_make_section (abfd, name);
      newsect -> vma = hdr -> p_vaddr + hdr -> p_filesz;
      newsect -> _raw_size = hdr -> p_memsz - hdr -> p_filesz;
      if (hdr -> p_type == PT_LOAD)
	{
	  newsect -> flags |= SEC_ALLOC;
	  if (hdr -> p_flags & PF_X)
	    newsect -> flags |= SEC_CODE;
	}
      if (!(hdr -> p_flags & PF_W))
	newsect -> flags |= SEC_READONLY;
    }

  return (true);
}

#ifdef HAVE_PROCFS

static void
DEFUN(bfd_prstatus,(abfd, descdata, descsz, filepos),
      bfd	*abfd AND
      char	*descdata AND
      int	 descsz AND
      long	 filepos)
{
  asection *newsect;
  prstatus_t *status = (prstatus_t *)0;

  if (descsz == sizeof (prstatus_t))
    {
      newsect = bfd_make_section (abfd, ".reg");
      newsect -> _raw_size = sizeof (status->pr_reg);
      newsect -> filepos = filepos + (long) &status->pr_reg;
      newsect -> flags = SEC_ALLOC | SEC_HAS_CONTENTS;
      newsect -> alignment_power = 2;
      if ((core_prstatus (abfd) = bfd_alloc (abfd, descsz)) != NULL)
	{
	  memcpy (core_prstatus (abfd), descdata, descsz);
	}
    }
}

/* Stash a copy of the prpsinfo structure away for future use. */

static void
DEFUN(bfd_prpsinfo,(abfd, descdata, descsz, filepos),
      bfd	*abfd AND
      char	*descdata AND
      int	 descsz AND
      long	 filepos)
{
  asection *newsect;

  if (descsz == sizeof (prpsinfo_t))
    {
      if ((core_prpsinfo (abfd) = bfd_alloc (abfd, descsz)) != NULL)
	{
	  memcpy (core_prpsinfo (abfd), descdata, descsz);
	}
    }
}

static void
DEFUN(bfd_fpregset,(abfd, descdata, descsz, filepos),
      bfd	*abfd AND
      char	*descdata AND
      int	 descsz AND
      long	 filepos)
{
  asection *newsect;

  newsect = bfd_make_section (abfd, ".reg2");
  newsect -> _raw_size = descsz;
  newsect -> filepos = filepos;
  newsect -> flags = SEC_ALLOC | SEC_HAS_CONTENTS;
  newsect -> alignment_power = 2;
}

#endif	/* HAVE_PROCFS */

/* Return a pointer to the args (including the command name) that were
   seen by the program that generated the core dump.  Note that for
   some reason, a spurious space is tacked onto the end of the args
   in some (at least one anyway) implementations, so strip it off if
   it exists. */

char *
DEFUN(elf_core_file_failing_command, (abfd),
     bfd *abfd)
{
#ifdef HAVE_PROCFS
  if (core_prpsinfo (abfd))
    {
      prpsinfo_t *p = core_prpsinfo (abfd);
      char *scan = p -> pr_psargs;
      while (*scan++) {;}
      scan -= 2;
      if ((scan > p -> pr_psargs) && (*scan == ' '))
	{
	  *scan = '\000';
	}
      return (p -> pr_psargs);
    }
#endif
  return (NULL);
}

/* Return the number of the signal that caused the core dump.  Presumably,
   since we have a core file, we got a signal of some kind, so don't bother
   checking the other process status fields, just return the signal number.
   */

static int
DEFUN(elf_core_file_failing_signal, (abfd),
      bfd *abfd)
{
#ifdef HAVE_PROCFS
  if (core_prstatus (abfd))
    {
      return (((prstatus_t *)(core_prstatus (abfd))) -> pr_cursig);
    }
#endif
  return (-1);
}

/* Check to see if the core file could reasonably be expected to have
   come for the current executable file.  Note that by default we return
   true unless we find something that indicates that there might be a
   problem.
   */

static boolean
DEFUN(elf_core_file_matches_executable_p, (core_bfd, exec_bfd),
      bfd *core_bfd AND
      bfd *exec_bfd)
{
#ifdef HAVE_PROCFS
  char *corename;
  char *execname;
#endif

  /* First, xvecs must match since both are ELF files for the same target. */

  if (core_bfd->xvec != exec_bfd->xvec)
    {
      bfd_error = system_call_error;
      return (false);
    }

#ifdef HAVE_PROCFS

  /* If no prpsinfo, just return true.  Otherwise, grab the last component
     of the exec'd pathname from the prpsinfo. */

  if (core_prpsinfo (core_bfd))
    {
      corename = (((struct prpsinfo *) core_prpsinfo (core_bfd)) -> pr_fname);
    }  
  else
    {
      return (true);
    }

  /* Find the last component of the executable pathname. */

  if ((execname = strrchr (exec_bfd -> filename, '/')) != NULL)
    {
      execname++;
    }
  else
    {
      execname = (char *) exec_bfd -> filename;
    }

  /* See if they match */

  return (strcmp (execname, corename) ? false : true);

#else

  return (true);

#endif	/* HAVE_PROCFS */
}

/* ELF core files contain a segment of type PT_NOTE, that holds much of
   the information that would normally be available from the /proc interface
   for the process, at the time the process dumped core.  Currently this
   includes copies of the prstatus, prpsinfo, and fpregset structures.

   Since these structures are potentially machine dependent in size and
   ordering, bfd provides two levels of support for them.  The first level,
   available on all machines since it does not require that the host
   have /proc support or the relevant include files, is to create a bfd
   section for each of the prstatus, prpsinfo, and fpregset structures,
   without any interpretation of their contents.  With just this support,
   the bfd client will have to interpret the structures itself.  Even with
   /proc support, it might want these full structures for it's own reasons.

   In the second level of support, where HAVE_PROCFS is defined, bfd will
   pick apart the structures to gather some additional information that
   clients may want, such as the general register set, the name of the
   exec'ed file and its arguments, the signal (if any) that caused the
   core dump, etc.

   */

static boolean
DEFUN(elf_corefile_note, (abfd, hdr),
      bfd               *abfd AND
      Elf_Internal_Phdr *hdr)
{
  Elf_External_Note *x_note_p;	/* Elf note, external form */
  Elf_Internal_Note i_note;	/* Elf note, internal form */
  char *buf = NULL;		/* Entire note segment contents */
  char *namedata;		/* Name portion of the note */
  char *descdata;		/* Descriptor portion of the note */
  char *sectname;		/* Name to use for new section */
  long filepos;			/* File offset to descriptor data */
  asection *newsect;

  if (hdr -> p_filesz > 0
      && (buf = (char *) bfd_xmalloc (hdr -> p_filesz)) != NULL
      && bfd_seek (abfd, hdr -> p_offset, SEEK_SET) != -1
      && bfd_read ((PTR) buf, hdr -> p_filesz, 1, abfd) == hdr -> p_filesz)
    {
      x_note_p = (Elf_External_Note *) buf;
      while ((char *) x_note_p < (buf + hdr -> p_filesz))
	{
	  i_note.namesz = bfd_h_get_32 (abfd, (bfd_byte *) x_note_p -> namesz);
	  i_note.descsz = bfd_h_get_32 (abfd, (bfd_byte *) x_note_p -> descsz);
	  i_note.type = bfd_h_get_32 (abfd, (bfd_byte *) x_note_p -> type);
	  namedata = x_note_p -> name;
	  descdata = namedata + BFD_ALIGN (i_note.namesz, 4);
	  filepos = hdr -> p_offset + (descdata - buf);
	  switch (i_note.type) {
	    case NT_PRSTATUS:
	      /* process descdata as prstatus info */
	      bfd_prstatus (abfd, descdata, i_note.descsz, filepos);
	      sectname = ".prstatus";
	      break;
	    case NT_FPREGSET:
	      /* process descdata as fpregset info */
	      bfd_fpregset (abfd, descdata, i_note.descsz, filepos);
	      sectname = ".fpregset";
	      break;
	    case NT_PRPSINFO:
	      /* process descdata as prpsinfo */
	      bfd_prpsinfo (abfd, descdata, i_note.descsz, filepos);
	      sectname = ".prpsinfo";
	      break;
	    default:
	      /* Unknown descriptor, just ignore it. */
	      sectname = NULL;
	      break;
	  }
	  if (sectname != NULL)
	    {
	      newsect = bfd_make_section (abfd, sectname);
	      newsect -> _raw_size = i_note.descsz;
	      newsect -> filepos = filepos;
	      newsect -> flags = SEC_ALLOC | SEC_HAS_CONTENTS;
	      newsect -> alignment_power = 2;
	    }
	  x_note_p = (Elf_External_Note *)
			(descdata + BFD_ALIGN (i_note.descsz, 4));
	}
    }
  if (buf != NULL)
    {
      free (buf);
    }
  return true;
  
}


/* Read a specified number of bytes at a specified offset in an ELF
   file, into a newly allocated buffer, and return a pointer to the 
   buffer. */

static char *
DEFUN(elf_read, (abfd, offset, size),
      bfd	*abfd AND
      long	offset AND
      int	size)
{
  char *buf;

  if ((buf = bfd_alloc (abfd, size)) == NULL)
    {
      bfd_error = no_memory;
      return (NULL);
    }
  if (bfd_seek (abfd, offset, SEEK_SET) == -1)
    {
      bfd_error = system_call_error;
      return (NULL);
    }
  if (bfd_read ((PTR) buf, size, 1, abfd) != size)
    {
      bfd_error = system_call_error;
      return (NULL);
    }
  return (buf);
}

/* Begin processing a given object.

   First we validate the file by reading in the ELF header and checking
   the magic number.

   */

static bfd_target *
DEFUN (elf_object_p, (abfd), bfd *abfd)
{
  Elf_External_Ehdr x_ehdr;	/* Elf file header, external form */
  Elf_Internal_Ehdr *i_ehdrp;	/* Elf file header, internal form */
  Elf_External_Shdr x_shdr;	/* Section header table entry, external form */
  Elf_Internal_Shdr *i_shdrp;	/* Section header table, internal form */
  int shindex;
  char *shstrtab;		/* Internal copy of section header stringtab */
  
  /* Read in the ELF header in external format.  */

  if (bfd_read ((PTR) &x_ehdr, sizeof (x_ehdr), 1, abfd) != sizeof (x_ehdr))
    {
      bfd_error = system_call_error;
      return (NULL);
    }

  /* Now check to see if we have a valid ELF file, and one that BFD can
     make use of.  The magic number must match, the address size ('class')
     and byte-swapping must match our XVEC entry, and it must have a
     section header table (FIXME: See comments re sections at top of this
     file). */

  if (x_ehdr.e_ident[EI_MAG0] != ELFMAG0 ||
      x_ehdr.e_ident[EI_MAG1] != ELFMAG1 ||
      x_ehdr.e_ident[EI_MAG2] != ELFMAG2 ||
      x_ehdr.e_ident[EI_MAG3] != ELFMAG3)
    {
wrong:
      bfd_error = wrong_format;
      return (NULL);
    }

  /* FIXME, Check EI_VERSION here !  */

  switch (x_ehdr.e_ident[EI_CLASS])
    {
    case ELFCLASSNONE:		/* address size not specified */
      goto wrong;		/* No support if can't tell address size */
    case ELFCLASS32:		/* 32-bit addresses */
      break;
    case ELFCLASS64:		/* 64-bit addresses */
      goto wrong;		/* FIXME: 64 bits not yet supported */
    default:
      goto wrong;		/* No support if unknown address class */
    }

  /* Switch xvec to match the specified byte order.  */
  switch (x_ehdr.e_ident[EI_DATA])
    {
    case ELFDATA2MSB:		/* Big-endian */ 
      if (!abfd->xvec->header_byteorder_big_p)
	goto wrong;
      break;
    case ELFDATA2LSB:		/* Little-endian */
      if (abfd->xvec->header_byteorder_big_p)
	goto wrong;
      break;
    case ELFDATANONE:		/* No data encoding specified */
    default:			/* Unknown data encoding specified */
      goto wrong;
    }
  
  /* Allocate an instance of the elf_obj_tdata structure and hook it up to
     the tdata pointer in the bfd. */

  if (NULL == (elf_tdata (abfd) = (struct elf_obj_tdata *)
               bfd_zalloc (abfd, sizeof (struct elf_obj_tdata))))
    {
      bfd_error = no_memory;
      return (NULL);
    }

  /* FIXME:  Any `wrong' exits below here will leak memory (tdata).  */

  /* Now that we know the byte order, swap in the rest of the header */
  i_ehdrp = elf_elfheader (abfd);
  elf_swap_ehdr_in (abfd, &x_ehdr, i_ehdrp);
  
  /* If there is no section header table, we're hosed. */
  if (i_ehdrp->e_shoff == 0)
    goto wrong;

  if (i_ehdrp->e_type == ET_EXEC || i_ehdrp->e_type == ET_DYN)
    abfd -> flags |= EXEC_P;

  switch (i_ehdrp->e_machine)
    {
    case EM_NONE:
    case EM_M32:		/* or should this be bfd_arch_obscure? */
      bfd_default_set_arch_mach(abfd, bfd_arch_unknown, 0);
      break;
    case EM_SPARC:
      bfd_default_set_arch_mach(abfd, bfd_arch_sparc, 0);
      break;
    case EM_386:
      bfd_default_set_arch_mach(abfd, bfd_arch_i386, 0);
      break;
    case EM_68K:
      bfd_default_set_arch_mach(abfd, bfd_arch_m68k, 0);
      break;
    case EM_88K:
      bfd_default_set_arch_mach(abfd, bfd_arch_m88k, 0);
      break;
    case EM_860:
      bfd_default_set_arch_mach(abfd, bfd_arch_i860, 0);
      break;
    case EM_MIPS:
      bfd_default_set_arch_mach(abfd, bfd_arch_mips, 0);
      break;
    default:
      goto wrong;
    }
  
  /* Allocate space for a copy of the section header table in 
     internal form, seek to the section header table in the file,
     read it in, and convert it to internal form.  As a simple sanity
     check, verify that the what BFD thinks is the size of each section
     header table entry actually matches the size recorded in the file. */

  if (i_ehdrp->e_shentsize != sizeof (x_shdr))
    goto wrong;
  i_shdrp = (Elf_Internal_Shdr *)
    bfd_alloc (abfd, sizeof (*i_shdrp) * i_ehdrp->e_shnum);
  if (! i_shdrp)
    {
      bfd_error = no_memory;
      return (NULL);
    }
  if (bfd_seek (abfd, i_ehdrp->e_shoff, SEEK_SET) == -1)
    {
      bfd_error = system_call_error;
      return (NULL);
    }
  for (shindex = 0; shindex < i_ehdrp->e_shnum; shindex++)
    {
      if (bfd_read ((PTR) &x_shdr, sizeof x_shdr, 1, abfd)
	  != sizeof (x_shdr))
	{
	  bfd_error = system_call_error;
	  return (NULL);
	}
      elf_swap_shdr_in (abfd, &x_shdr, i_shdrp + shindex);
    }

  elf_elfsections (abfd) = i_shdrp;
  
  /* Read in the string table containing the names of the sections.  We
     will need the base pointer to this table later. */
  /* We read this inline now, so that we don't have to go through
     bfd_section_from_shdr with it (since this particular strtab is
     used to find all of the ELF section names.) */

  shstrtab = elf_get_str_section (abfd, i_ehdrp->e_shstrndx);
  if (! shstrtab)
    return (NULL);
  
  /* Once all of the section headers have been read and converted, we
     can start processing them.  Note that the first section header is
     a dummy placeholder entry, so we ignore it.

     We also watch for the symbol table section and remember the file
     offset and section size for both the symbol table section and the
     associated string table section. */

  for (shindex = 1; shindex < i_ehdrp->e_shnum; shindex++)
    {
      bfd_section_from_shdr (abfd, shindex);
    }

  /* Remember the entry point specified in the ELF file header. */

  bfd_get_start_address (abfd) = i_ehdrp->e_entry;

  return (abfd->xvec);
}

/*  Core files are simply standard ELF formatted files that partition
    the file using the execution view of the file (program header table)
    rather than the linking view.  In fact, there is no section header
    table in a core file.

    The process status information (including the contents of the general
    register set) and the floating point register set are stored in a
    segment of type PT_NOTE.  We handcraft a couple of extra bfd sections
    that allow standard bfd access to the general registers (.reg) and the
    floating point registers (.reg2).

 */

static bfd_target *
DEFUN (elf_core_file_p, (abfd), bfd *abfd)
{
  Elf_External_Ehdr x_ehdr;	/* Elf file header, external form */
  Elf_Internal_Ehdr *i_ehdrp;	/* Elf file header, internal form */
  Elf_External_Phdr x_phdr;	/* Program header table entry, external form */
  Elf_Internal_Phdr *i_phdrp;	/* Program header table, internal form */
  unsigned int phindex;
  
  /* Read in the ELF header in external format.  */

  if (bfd_read ((PTR) &x_ehdr, sizeof (x_ehdr), 1, abfd) != sizeof (x_ehdr))
    {
      bfd_error = system_call_error;
      return (NULL);
    }

  /* Now check to see if we have a valid ELF file, and one that BFD can
     make use of.  The magic number must match, the address size ('class')
     and byte-swapping must match our XVEC entry, and it must have a
     program header table (FIXME: See comments re segments at top of this
     file). */

  if (x_ehdr.e_ident[EI_MAG0] != ELFMAG0 ||
      x_ehdr.e_ident[EI_MAG1] != ELFMAG1 ||
      x_ehdr.e_ident[EI_MAG2] != ELFMAG2 ||
      x_ehdr.e_ident[EI_MAG3] != ELFMAG3)
    {
wrong:
      bfd_error = wrong_format;
      return (NULL);
    }

  /* FIXME, Check EI_VERSION here !  */

  switch (x_ehdr.e_ident[EI_CLASS])
    {
    case ELFCLASSNONE:		/* address size not specified */
      goto wrong;		/* No support if can't tell address size */
    case ELFCLASS32:		/* 32-bit addresses */
      break;
    case ELFCLASS64:		/* 64-bit addresses */
      goto wrong;		/* FIXME: 64 bits not yet supported */
    default:
      goto wrong;		/* No support if unknown address class */
    }

  /* Switch xvec to match the specified byte order.  */
  switch (x_ehdr.e_ident[EI_DATA])
    {
    case ELFDATA2MSB:		/* Big-endian */ 
      abfd->xvec = &elf_big_vec;
      break;
    case ELFDATA2LSB:		/* Little-endian */
      abfd->xvec = &elf_little_vec;
      break;
    case ELFDATANONE:		/* No data encoding specified */
    default:			/* Unknown data encoding specified */
      goto wrong;
    }
  
  /* Allocate an instance of the elf_obj_tdata structure and hook it up to
     the tdata pointer in the bfd. */

  elf_tdata (abfd) =
    (struct elf_obj_tdata *) bfd_zalloc (abfd, sizeof (struct elf_obj_tdata));
  if (elf_tdata (abfd) == NULL)
    {
      bfd_error = no_memory;
      return (NULL);
    }

  /* FIXME, `wrong' returns from this point onward, leak memory.  */

  /* Now that we know the byte order, swap in the rest of the header */
  i_ehdrp = elf_elfheader (abfd);
  elf_swap_ehdr_in (abfd, &x_ehdr, i_ehdrp);

  /* If there is no program header, or the type is not a core file, then
     we are hosed. */
  if (i_ehdrp->e_phoff == 0 || i_ehdrp->e_type != ET_CORE)
    goto wrong;

  /* Allocate space for a copy of the program header table in 
     internal form, seek to the program header table in the file,
     read it in, and convert it to internal form.  As a simple sanity
     check, verify that the what BFD thinks is the size of each program
     header table entry actually matches the size recorded in the file. */

  if (i_ehdrp->e_phentsize != sizeof (x_phdr))
    goto wrong;
  i_phdrp = (Elf_Internal_Phdr *)
    bfd_alloc (abfd, sizeof (*i_phdrp) * i_ehdrp->e_phnum);
  if (! i_phdrp)
    {
      bfd_error = no_memory;
      return (NULL);
    }
  if (bfd_seek (abfd, i_ehdrp->e_phoff, SEEK_SET) == -1)
    {
      bfd_error = system_call_error;
      return (NULL);
    }
  for (phindex = 0; phindex < i_ehdrp->e_phnum; phindex++)
    {
      if (bfd_read ((PTR) &x_phdr, sizeof (x_phdr), 1, abfd)
	  != sizeof (x_phdr))
	{
	  bfd_error = system_call_error;
	  return (NULL);
	}
      elf_swap_phdr_in (abfd, &x_phdr, i_phdrp + phindex);
    }

  /* Once all of the program headers have been read and converted, we
     can start processing them. */

  for (phindex = 0; phindex < i_ehdrp->e_phnum; phindex++)
    {
      bfd_section_from_phdr (abfd, i_phdrp + phindex, phindex);
      if ((i_phdrp + phindex) -> p_type == PT_NOTE)
	{
	  elf_corefile_note (abfd, i_phdrp + phindex);
	}
    }

  /* Remember the entry point specified in the ELF file header. */

  bfd_get_start_address (abfd) = i_ehdrp->e_entry;

  return (abfd->xvec);
}

static boolean
DEFUN (elf_mkobject, (abfd), bfd *abfd)
{
  /* this just does initialization */
  /* coff_mkobject zalloc's space for tdata.coff_obj_data ... */
  elf_tdata(abfd) = (struct elf_obj_tdata *)
    bfd_zalloc (abfd, sizeof(struct elf_obj_tdata));
  if (elf_tdata(abfd) == 0) {
    bfd_error = no_memory;
    return false;
  }
  /* since everything is done at close time, do we need any
     initialization? */
  
  return (true);
}

/*
  Create ELF output from BFD sections.
  
  Essentially, just create the section header and forget about the program
  header for now.   
  
*/

/* lacking nested functions and nested types, set up for mapping over
   BFD sections to produce ELF sections */

typedef struct {
  Elf_Internal_Ehdr	*i_ehdr;
  Elf_Internal_Shdr	*i_shdrp;
  struct strtab		*shstrtab;
  int			symtab_section;
} elf_sect_thunk;



static void
DEFUN (elf_make_sections, (abfd, asect, obj),
       bfd		*abfd AND
       asection		*asect AND
       PTR		obj)
{
  elf_sect_thunk *thunk = (elf_sect_thunk*)obj;
  /* most of what is in bfd_shdr_from_section goes in here... */
  /* and all of these sections generate at *least* one ELF section. */
  int this_section;
  int idx;
  
  /* check if we're making a PROGBITS section... */
  /* if ((asect->flags & SEC_ALLOC) && (asect->flags & SEC_LOAD)) */
  /* this was too strict... what *do* we want to check here? */
  if(1)
    {
      Elf_Internal_Shdr *this_hdr;
      this_section = elf_section_from_bfd_section (abfd, asect);
      this_hdr = &thunk->i_shdrp[this_section];

      this_hdr->sh_addr = asect->vma;
      this_hdr->sh_size = asect->_raw_size;
      /* contents already set by elf_set_section_contents */

      if (asect->flags & SEC_RELOC)
	{
	  /* emit a reloc section, and thus strtab and symtab... */
	  Elf_Internal_Shdr *rela_hdr;
	  Elf_Internal_Shdr *symtab_hdr;
	  Elf_Internal_Shdr *symstrtab_hdr;
	  Elf_External_Rela *outbound_relocs;
	  Elf_External_Sym *outbound_syms;
	  int rela_section;
	  int symstrtab_section;
	  
	  symtab_hdr = &thunk->i_shdrp[thunk->symtab_section];

	  if (thunk->symtab_section == this_section + 1)
	    rela_section = thunk->symtab_section + 2; /* symtab + symstrtab */
	  else
	    rela_section = this_section + 1;
	  rela_hdr = &thunk->i_shdrp[rela_section];
	  rela_hdr->sh_type = SHT_RELA;
	  rela_hdr->sh_link = thunk->symtab_section;
	  rela_hdr->sh_info = this_section;
	  rela_hdr->sh_entsize = sizeof (Elf_External_Rela);
	  /* orelocation has the data, reloc_count has the count... */
	  rela_hdr->sh_size = rela_hdr->sh_entsize * asect->reloc_count;
	  outbound_relocs = (Elf_External_Rela *)
	    bfd_alloc(abfd, asect->reloc_count * sizeof(Elf_External_Rela));
	  for (idx = 0; idx < asect->reloc_count; idx++)
	    {
	      Elf_Internal_Rela dst;
	      arelent        *ptr;
	      Elf_External_Rela  *src;
	      
	      ptr = asect->orelocation[idx];
	      src = outbound_relocs + idx;
	      if (asect->flags & SEC_RELOC)
		dst.r_offset = ptr->address - asect->vma;
	      else
		dst.r_offset = ptr->address;

	      dst.r_info = ELF_R_INFO(1 /*ptr->sym_ptr_ptr*/, /* needs index into symtab (FIXME) */
				      ptr->howto->type);

	      dst.r_addend = ptr->addend;
	      elf_swap_reloca_out(abfd, &dst, src);
	    }
	  rela_hdr->contents = (void*)outbound_relocs;
	}
    }
}

static void
DEFUN (elf_fake_sections, (abfd, asect, obj),
       bfd		*abfd AND
       asection		*asect AND
       PTR		obj)
{
  elf_sect_thunk *thunk = (elf_sect_thunk*)obj;
  /* most of what is in bfd_shdr_from_section goes in here... */
  /* and all of these sections generate at *least* one ELF section. */
  int this_section;
  int idx;
  
  /* check if we're making a PROGBITS section... */
  /* if ((asect->flags & SEC_ALLOC) && (asect->flags & SEC_LOAD)) */
  /* this was too strict... what *do* we want to check here? */
  if(1)
    {
      Elf_Internal_Shdr *this_hdr;
      this_section = thunk->i_ehdr->e_shnum++;
      this_hdr = &thunk->i_shdrp[this_section];
      this_hdr->sh_name = 
	bfd_add_to_strtab (abfd, thunk->shstrtab, asect->name);
      /* we need to log the type *now* so that elf_section_from_bfd_section
	 can find us... have to set rawdata too. */
      this_hdr->rawdata = (void*)asect;
      if ((asect->flags & SEC_ALLOC) && (asect->flags & SEC_LOAD))
	this_hdr->sh_type = SHT_PROGBITS;
      else
	/* what *do* we put here? */
	this_hdr->sh_type = SHT_PROGBITS;


      if (asect->flags & SEC_RELOC)
	{
	  /* emit a reloc section, and thus strtab and symtab... */
	  Elf_Internal_Shdr *rela_hdr;
	  Elf_Internal_Shdr *symtab_hdr;
	  Elf_Internal_Shdr *symstrtab_hdr;
	  Elf_External_Rela *outbound_relocs;
	  Elf_External_Sym *outbound_syms;
	  int rela_section;
	  int symstrtab_section;
	  
	  /* note that only one symtab is used, so just remember it
	     for now */
	  if (! thunk->symtab_section)
	    {
	      thunk->symtab_section = thunk->i_ehdr->e_shnum++;
	      symtab_hdr = &thunk->i_shdrp[thunk->symtab_section];
	      symtab_hdr->sh_name =
		bfd_add_to_strtab (abfd, thunk->shstrtab, ".symtab");
	      symtab_hdr->sh_type = SHT_SYMTAB;
	      symtab_hdr->sh_entsize = sizeof (Elf_External_Sym);

	      symstrtab_section = thunk->i_ehdr->e_shnum++;
	      BFD_ASSERT(symstrtab_section == thunk->symtab_section+1);
	      symstrtab_hdr = &thunk->i_shdrp[symstrtab_section];
	      symtab_hdr->sh_link = symstrtab_section;
	      symstrtab_hdr->sh_name =
		bfd_add_to_strtab (abfd, thunk->shstrtab, ".strtab");
	      symstrtab_hdr->sh_type = SHT_STRTAB;

	      symtab_hdr->contents = 0;
	      symstrtab_hdr->contents = 0;
	      symstrtab_hdr->sh_size = 0;
	    } 
	  else
	    symtab_hdr = &thunk->i_shdrp[thunk->symtab_section];

	  rela_section = thunk->i_ehdr->e_shnum++;
	  rela_hdr = &thunk->i_shdrp[rela_section];
	  rela_hdr->sh_name =
	    bfd_add_2_to_strtab (abfd, thunk->shstrtab, ".rela", asect->name);
	  rela_hdr->sh_type = SHT_RELA;
	  rela_hdr->sh_link = thunk->symtab_section;
	  rela_hdr->sh_info = this_section;
	  rela_hdr->sh_entsize = sizeof (Elf_External_Rela);
	}
    }
}


static boolean
DEFUN (elf_compute_section_file_positions, (abfd), bfd *abfd)
{
  Elf_Internal_Ehdr *i_ehdrp;	/* Elf file header, internal form */
  Elf_Internal_Shdr *i_shdrp;	/* Section header table, internal form */
  struct strtab *shstrtab;
  int count, maxsections;
  int outbase;
  elf_sect_thunk est;

  if (! elf_shstrtab (abfd)) {
    i_ehdrp = elf_elfheader (abfd);	/* build new header in tdata memory */
    shstrtab = bfd_new_strtab(abfd);
    
    i_ehdrp->e_ident[EI_MAG0] = ELFMAG0;
    i_ehdrp->e_ident[EI_MAG1] = ELFMAG1;
    i_ehdrp->e_ident[EI_MAG2] = ELFMAG2;
    i_ehdrp->e_ident[EI_MAG3] = ELFMAG3;

    i_ehdrp->e_ident[EI_CLASS] = ELFCLASS32; /* FIXME: find out from bfd */
    i_ehdrp->e_ident[EI_DATA] =
      abfd->xvec->byteorder_big_p ? ELFDATA2MSB : ELFDATA2LSB;
    i_ehdrp->e_ident[EI_VERSION] = EV_CURRENT;

    for(count = EI_PAD; count < EI_NIDENT; count ++)
      i_ehdrp->e_ident[count] = 0;
      
    i_ehdrp->e_type = (abfd->flags & EXEC_P)? ET_EXEC : ET_REL;
    switch(bfd_get_arch(abfd))
      {
      case bfd_arch_unknown:
	i_ehdrp->e_machine = EM_NONE;
	break;
      case bfd_arch_sparc:
	i_ehdrp->e_machine = EM_SPARC;
	break;
      case bfd_arch_i386:
	i_ehdrp->e_machine = EM_386;
	break;
      case bfd_arch_m68k:
	i_ehdrp->e_machine = EM_68K;
	break;
      case bfd_arch_m88k:
	i_ehdrp->e_machine = EM_88K;
	break;
      case bfd_arch_i860:
	i_ehdrp->e_machine = EM_860;
	break;
      case bfd_arch_mips:		  /* MIPS Rxxxx */
	i_ehdrp->e_machine = EM_MIPS; /* only MIPS R3000 */
	break;
	/* also note that EM_M32, AT&T WE32100 is unknown to bfd */
      default:
	i_ehdrp->e_machine = EM_NONE;
      }
    i_ehdrp->e_version = EV_CURRENT;
    i_ehdrp->e_ehsize = sizeof(Elf_External_Ehdr);
    
    /* no program header, for now. */
    i_ehdrp->e_phoff = 0;
    i_ehdrp->e_phentsize = 0;
    i_ehdrp->e_phnum = 0;

    /* each bfd section is section header entry */
    i_ehdrp->e_entry = bfd_get_start_address (abfd);
    i_ehdrp->e_shentsize = sizeof (Elf_External_Shdr);

    /* figure at most each section can have a rel, strtab, symtab */
    maxsections = 4*bfd_count_sections(abfd)+2;

    i_ehdrp->e_shoff = i_ehdrp->e_ehsize;

    /* and we'll just have to fix up the offsets later. */
    /* outbase += i_ehdr.e_shentsize * i_ehdr.e_shnum; */
    
    i_shdrp = (Elf_Internal_Shdr *)
      bfd_alloc (abfd, sizeof (*i_shdrp) * maxsections);
    if (! i_shdrp)
      {
	bfd_error = no_memory;
	return (false);
      }
    for (count=0; count < maxsections; count++) 
      {
	i_shdrp[count].rawdata = 0;
	i_shdrp[count].contents = 0;
      }
    
    
    i_shdrp[0].sh_name = 0;
    i_shdrp[0].sh_type = SHT_NULL;
    i_shdrp[0].sh_flags = 0;
    i_shdrp[0].sh_addr = 0;
    i_shdrp[0].sh_offset = 0;
    i_shdrp[0].sh_size = 0;
    i_shdrp[0].sh_link = SHN_UNDEF;
    i_shdrp[0].sh_info = 0;
    i_shdrp[0].sh_addralign = 0;
    i_shdrp[0].sh_entsize = 0;

    i_ehdrp->e_shnum = 1;

    elf_elfsections (abfd) = i_shdrp;
    elf_shstrtab (abfd) = shstrtab;
  }
  est.i_ehdr = elf_elfheader(abfd);
  est.i_shdrp = elf_elfsections(abfd);
  est.shstrtab = elf_shstrtab(abfd);
  est.symtab_section = 0;	/* elf_fake_sections fils it in */

  bfd_map_over_sections(abfd, elf_fake_sections, &est);
  elf_onesymtab (abfd) = est.symtab_section;
  return (true);
}

static boolean
DEFUN (elf_write_object_contents, (abfd), bfd *abfd)
{
  Elf_External_Ehdr x_ehdr;	/* Elf file header, external form */
  Elf_Internal_Ehdr *i_ehdrp;	/* Elf file header, internal form */
  Elf_External_Phdr *x_phdrp;	/* Program header table, external form */
  Elf_Internal_Phdr *i_phdrp;	/* Program header table, internal form */
  Elf_External_Shdr *x_shdrp;	/* Section header table, external form */
  Elf_Internal_Shdr *i_shdrp;	/* Section header table, internal form */
  asection *nsect;
  int maxsections;
  elf_sect_thunk est;
  
  int outbase = 0;
  int count;
  struct strtab *shstrtab;
  
  if(abfd->output_has_begun == false) 
    elf_compute_section_file_positions(abfd);

  i_ehdrp = elf_elfheader (abfd);
  i_shdrp = elf_elfsections (abfd);
  shstrtab = elf_shstrtab (abfd);

  est.i_ehdr = i_ehdrp;
  est.i_shdrp = i_shdrp;
  est.shstrtab = shstrtab;
  est.symtab_section = elf_onesymtab (abfd); /* filled in by elf_fake */

  bfd_map_over_sections(abfd, elf_make_sections, &est);

  /* dump out the one symtab */
  {
    int symcount = bfd_get_symcount (abfd);
    asymbol ** syms = bfd_get_outsymbols (abfd);
    struct strtab * stt = bfd_new_strtab (abfd);
    Elf_Internal_Shdr *symtab_hdr;
    Elf_Internal_Shdr *symstrtab_hdr;
    int symstrtab_section;
    Elf_External_Sym *outbound_syms;
    int idx;

    symtab_hdr = &i_shdrp[est.symtab_section];
    symtab_hdr->sh_type = SHT_SYMTAB;
    symtab_hdr->sh_entsize = sizeof (Elf_External_Sym);
    symtab_hdr->sh_size = symtab_hdr->sh_entsize * symcount;

    /* see assert in elf_fake_sections that supports this: */
    symstrtab_section = est.symtab_section+1;
    symstrtab_hdr = &i_shdrp[symstrtab_section];
    symtab_hdr->sh_link = symstrtab_section;
    symstrtab_hdr->sh_type = SHT_STRTAB;

    outbound_syms = (Elf_External_Sym*)
      bfd_alloc(abfd, (1+symcount) * sizeof(Elf_External_Sym));
    /* now generate the data (for "contents") */
    for (idx = 0; idx < symcount; idx++)
      {
	Elf_Internal_Sym sym;
	sym.st_name = bfd_add_to_strtab (abfd, stt, syms[idx]->name);
	sym.st_value = syms[idx]->value;
	sym.st_size =  0; /* we should recover this (FIXME) */
	if (syms[idx]->flags & BSF_WEAK)
	  sym.st_info = ELF_ST_INFO(STB_WEAK, STT_OBJECT);
	else if (syms[idx]->flags & BSF_LOCAL)
	  sym.st_info = ELF_ST_INFO(STB_LOCAL, STT_OBJECT);
	else if (syms[idx]->flags & BSF_GLOBAL)
	  sym.st_info = ELF_ST_INFO(STB_GLOBAL, STT_OBJECT);
	else if (syms[idx]->flags & BSF_SECTION_SYM)
	  sym.st_info = ELF_ST_INFO(STB_LOCAL, STT_SECTION);
	else if (syms[idx]->flags & BSF_FILE)
	  sym.st_info = ELF_ST_INFO(STB_LOCAL, STT_FILE);

	sym.st_other = 0;
	if (syms[idx]->section) 
	  sym.st_shndx =
	    elf_section_from_bfd_section(abfd,
					 syms[idx]->section->output_section);
	else
	  sym.st_shndx = SHN_UNDEF;

	elf_swap_symbol_out (abfd, &sym, outbound_syms+idx+1);
      }
    {
      /* fill in 0th symbol */
      Elf_Internal_Sym sym;
      sym.st_name = 0;
      sym.st_value = 0;
      sym.st_size = 0;
      sym.st_info = 0;
      sym.st_other = 0;
      sym.st_shndx = SHN_UNDEF;
      elf_swap_symbol_out (abfd, &sym, outbound_syms);
    }
    symtab_hdr->contents = (void*)outbound_syms;
    symstrtab_hdr->contents = (void*)stt->tab;
    symstrtab_hdr->sh_size = stt->length;
  }

  /* put the strtab out too... */
  {
    Elf_Internal_Shdr *this_hdr;
    int this_section;

    this_section = i_ehdrp->e_shnum++;
    i_ehdrp->e_shstrndx = this_section;
    this_hdr = &i_shdrp[this_section];
    this_hdr->sh_name = bfd_add_to_strtab (abfd, shstrtab, ".shstrtab");
    this_hdr->sh_size = shstrtab->length;
    this_hdr->contents = (void*)shstrtab->tab;
  }

  outbase = i_ehdrp->e_ehsize;

  /* swap the header before spitting it out... */
  elf_swap_ehdr_out (abfd, i_ehdrp, &x_ehdr);
  bfd_seek (abfd, (file_ptr) 0, SEEK_SET);
  bfd_write ((PTR) &x_ehdr, sizeof(x_ehdr), 1, abfd);

  outbase += i_ehdrp->e_shentsize * i_ehdrp->e_shnum;

  /* now we fix up the offsets... */
  for (count = 0; count < i_ehdrp->e_shnum; count ++)
    {
      i_shdrp[count].sh_offset = outbase;
      outbase += i_shdrp[count].sh_size;
    }

  /* at this point we've concocted all the ELF sections... */
  x_shdrp = (Elf_External_Shdr *)
    bfd_alloc (abfd, sizeof (*x_shdrp) * (i_ehdrp->e_shnum));
  if (! x_shdrp)
    {
      bfd_error = no_memory;
      return (false);
    }

  for (count = 0; count < i_ehdrp->e_shnum; count ++)
    {
      elf_swap_shdr_out (abfd, i_shdrp+count, x_shdrp+count);
    }
  bfd_write ((PTR) x_shdrp, sizeof(*x_shdrp), i_ehdrp->e_shnum, abfd);
  /* need to dump the string table too... */
  
  /* after writing the headers, we need to write the sections too... */
  nsect = abfd->sections;
  for (count = 0; count < i_ehdrp->e_shnum; count ++)
    {
      if(i_shdrp[count].contents)
	{
	  bfd_seek (abfd, i_shdrp[count].sh_offset, SEEK_SET);
	  bfd_write (i_shdrp[count].contents, i_shdrp[count].sh_size, 1, abfd);
	}
    }
  
  /* sample use of bfd:
   * bfd_seek (abfd, (file_ptr) 0, SEEK_SET);
   * bfd_write ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd);
   * if (bfd_seek(abfd, scn_base, SEEK_SET) != 0)
   * return false;
   * old = bfd_tell(abfd);
   */

  return true;
  
}

/* Given an index of a section, retrieve a pointer to it.  Note
   that for our purposes, sections are indexed by {1, 2, ...} with
   0 being an illegal index. */

/* In the original, each ELF section went into exactly one BFD
   section. This doesn't really make sense, so we need a real mapping.
   The mapping has to hide in the Elf_Internal_Shdr since asection
   doesn't have anything like a tdata field... */
   
static struct sec *
DEFUN (section_from_elf_index, (abfd, index),
       bfd            *abfd AND
       int             index)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  Elf_Internal_Shdr *hdr = i_shdrp + index;

  switch (hdr->sh_type)
    {
      /* ELF sections that map to BFD sections */
    case SHT_PROGBITS:
    case SHT_NOBITS:
      if (! hdr->rawdata)
	bfd_section_from_shdr (abfd, index);
      return (struct sec *)hdr->rawdata;
      break;
    default:
      return (struct sec *)&bfd_abs_section;
    }
}

/* given a section, search the header to find them... */
static int
DEFUN (elf_section_from_bfd_section, (abfd, asect),
       bfd		*abfd AND
       struct sec	*asect)
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  int index;
  Elf_Internal_Shdr *hdr;
  int maxindex = elf_elfheader (abfd)->e_shnum;
  
  for(index = 0; index < maxindex; index++) {
    hdr = &i_shdrp[index];
    switch (hdr->sh_type)
      {
	/* ELF sections that map to BFD sections */
      case SHT_PROGBITS:
      case SHT_NOBITS:
	if (hdr->rawdata) 
	  {
	    if (((struct sec *)(hdr->rawdata)) == asect)
	      return index;
	  }
	break;
      default:
	break;
      }
  }
  return 0;
}

static boolean
DEFUN (elf_slurp_symbol_table, (abfd, symptrs),
       bfd		*abfd AND
       asymbol		**symptrs)	/* Buffer for generated bfd symbols */
{
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  Elf_Internal_Shdr *hdr = i_shdrp + elf_onesymtab (abfd);
  int symcount;		/* Number of external ELF symbols */
  int i;
  asymbol *sym;		/* Pointer to current bfd symbol */
  asymbol *symbase;	/* Buffer for generated bfd symbols */
  Elf_Internal_Sym i_sym;
  Elf_External_Sym *x_symp;

  /* this is only valid because there is only one symtab... */
  /* FIXME:  This is incorrect, there may also be a dynamic symbol
     table which is a subset of the full symbol table.  We either need
     to be prepared to read both (and merge them) or ensure that we
     only read the full symbol table.  Currently we only get called to
     read the full symbol table.  -fnf */
  if (bfd_get_outsymbols (abfd) != NULL)
    {
      return (true);
    }

  /* Read each raw ELF symbol, converting from external ELF form to
     internal ELF form, and then using the information to create a
     canonical bfd symbol table entry.

     Note that we allocate the initial bfd canonical symbol buffer
     based on a one-to-one mapping of the ELF symbols to canonical
     symbols.  We actually use all the ELF symbols, so there will be no
     space left over at the end.  When we have all the symbols, we
     build the caller's pointer vector. */

  if (bfd_seek (abfd, hdr->sh_offset, SEEK_SET) == -1)
    {
      bfd_error = system_call_error;
      return (false);
    }

  symcount = hdr->sh_size / sizeof (Elf_External_Sym);
  symbase = (asymbol *) bfd_zalloc (abfd, symcount * sizeof (asymbol));
  sym = symbase;

  /* Temporarily allocate room for the raw ELF symbols.  */
  x_symp = (Elf_External_Sym *) malloc (symcount * sizeof (Elf_External_Sym));

  if (bfd_read ((PTR) x_symp, sizeof (Elf_External_Sym), symcount, abfd) 
      != symcount * sizeof (Elf_External_Sym))
    {
      free ((PTR)x_symp);
      bfd_error = system_call_error;
      return (false);
    }
  /* Skip first symbol, which is a null dummy.  */
  for (i = 1; i < symcount; i++)
    {
      elf_swap_symbol_in (abfd, x_symp + i, &i_sym);
      sym -> the_bfd = abfd;
      if (i_sym.st_name > 0)
	sym -> name = elf_string_from_elf_section(abfd, hdr->sh_link,
						i_sym.st_name);
      else
	sym -> name = "unnamed"; /* perhaps should include the number? */
      sym -> value = i_sym.st_value;
/* FIXME -- this is almost certainly bogus.  It's from Pace Willisson's
hasty Solaris support, to pass the sizes of object files or functions
down into GDB via the back door, to circumvent some other kludge in
how Sun hacked stabs.   -- gnu@cygnus.com  */
      sym -> udata = (PTR)i_sym.st_size;
/* FIXME -- end of bogosity.  */
      if (i_sym.st_shndx > 0 && i_sym.st_shndx < SHN_LORESERV)
	{
	  sym -> section = section_from_elf_index (abfd, i_sym.st_shndx);
	}
      else if (i_sym.st_shndx == SHN_ABS)
	{
	  sym -> section = &bfd_abs_section;
	}
      else if (i_sym.st_shndx == SHN_COMMON)
	{
	  sym -> section = &bfd_com_section;
	}
      else if (i_sym.st_shndx == SHN_UNDEF)
	{
	  sym -> section = &bfd_und_section;
	}
      else
	sym -> section = &bfd_abs_section;
      
      switch (ELF_ST_BIND (i_sym.st_info))
	{
	  case STB_LOCAL:
	    sym -> flags |= BSF_LOCAL;
	    break;
	  case STB_GLOBAL:
	    sym -> flags |= (BSF_GLOBAL | BSF_EXPORT);
	    break;
	  case STB_WEAK:
	    sym -> flags |= BSF_WEAK;
	    break;
	}

      switch (ELF_ST_TYPE (i_sym.st_info))
	{
	  case STT_SECTION:
	    sym->flags |= BSF_SECTION_SYM | BSF_DEBUGGING;
	    break;
	  case STT_FILE:
	    sym->flags |= BSF_FILE | BSF_DEBUGGING;
	    break;
	}
      sym++;
    }

  /* We rely on the zalloc to clear out the final symbol entry.  */

  /* We're now done with the raw symbols.  */
  free ((PTR)x_symp);

  bfd_get_symcount(abfd) = symcount = sym - symbase;
  
  /* Fill in the user's symbol pointer vector if needed.  */
  if (symptrs)
    {
      sym = symbase;
      while (symcount-- > 0)
	{
	  *symptrs++ = sym++;
	}
      *symptrs = 0;			/* Final null pointer */
    }

  return (true);
}

/* Return the number of bytes required to hold the symtab vector.

   Note that we base it on the count plus 1, since we will null terminate
   the vector allocated based on this size.  However, the ELF symbol table
   always has a dummy entry as symbol #0, so it ends up even.  */

static unsigned int
DEFUN (elf_get_symtab_upper_bound, (abfd), bfd *abfd)
{
  unsigned int symcount;
  unsigned int symtab_size;
  Elf_Internal_Shdr *i_shdrp = elf_elfsections (abfd);
  Elf_Internal_Shdr *hdr = i_shdrp + elf_onesymtab (abfd);

  symcount = hdr->sh_size / sizeof (Elf_External_Sym);
  symtab_size = (symcount - 1 + 1) * (sizeof (asymbol));
  return (symtab_size);
}

/*
	This function return the number of bytes required to store the
	relocation information associated with section <<sect>>
	attached to bfd <<abfd>>

*/
static unsigned int
elf_get_reloc_upper_bound (abfd, asect)
bfd            *abfd;
sec_ptr         asect;
{
  if (asect->flags & SEC_RELOC)
    {
      /* either rel or rela */
      return asect->_raw_size;
    }
  else
    return (0);
}

/* FIXME!!! sparc howto should go into elf-32-sparc.c */
#ifdef sparc
enum reloc_type
  {
    R_SPARC_NONE = 0,
    R_SPARC_8,		R_SPARC_16,		R_SPARC_32, 
    R_SPARC_DISP8,	R_SPARC_DISP16,		R_SPARC_DISP32, 
    R_SPARC_WDISP30,	R_SPARC_WDISP22,
    R_SPARC_HI22,	R_SPARC_22,
    R_SPARC_13,		R_SPARC_LO10,
    R_SPARC_GOT10,	R_SPARC_GOT13,		R_SPARC_GOT22,
    R_SPARC_PC10,	R_SPARC_PC22,
    R_SPARC_WPLT30,
    R_SPARC_COPY,
    R_SPARC_GLOB_DAT,	R_SPARC_JMP_SLOT,
    R_SPARC_RELATIVE,
    R_SPARC_UA32,
    };

#define	RELOC_TYPE_NAMES	\
    "R_SPARC_NONE",		\
    "R_SPARC_8",	"R_SPARC_16",		"R_SPARC_32",		\
    "R_SPARC_DISP8",	"R_SPARC_DISP16",	"R_SPARC_DISP32",	\
    "R_SPARC_WDISP30",	"R_SPARC_WDISP22",	\
    "R_SPARC_HI22",	"R_SPARC_22",		\
    "R_SPARC_13",	"R_SPARC_LO10",		\
    "R_SPARC_GOT10",	"R_SPARC_GOT13",	"R_SPARC_GOT22",	\
    "R_SPARC_PC10",	"R_SPARC_PC22",		\
    "R_SPARC_WPLT30",		\
    "R_SPARC_COPY",		\
    "R_SPARC_GLOB_DAT",	"R_SPARC_JMP_SLOT",	\
    "R_SPARC_RELATIVE",		\
    "R_SPARC_UA32"

static reloc_howto_type elf_howto_table[] = 
{
  HOWTO(R_SPARC_NONE,   0,0, 0,false,0,false,false, 0,"R_SPARC_NONE",   false,0,0x00000000,false),
  HOWTO(R_SPARC_8,      0,0, 8,false,0,true,  true, 0,"R_SPARC_8",      false,0,0x000000ff,false),
  HOWTO(R_SPARC_16,     0,1,16,false,0,true,  true, 0,"R_SPARC_16",     false,0,0x0000ffff,false),
  HOWTO(R_SPARC_32,     0,2,32,false,0,true,  true, 0,"R_SPARC_32",     false,0,0xffffffff,false),
  HOWTO(R_SPARC_DISP8,  0,0, 8,true, 0,false, true, 0,"R_SPARC_DISP8",  false,0,0x000000ff,false),
  HOWTO(R_SPARC_DISP16, 0,1,16,true, 0,false, true, 0,"R_SPARC_DISP16", false,0,0x0000ffff,false),
  HOWTO(R_SPARC_DISP32, 0,2,32,true, 0,false, true, 0,"R_SPARC_DISP32", false,0,0x00ffffff,false),
  HOWTO(R_SPARC_WDISP30,2,2,30,true, 0,false, true, 0,"R_SPARC_WDISP30",false,0,0x3fffffff,false),
  HOWTO(R_SPARC_WDISP22,2,2,22,true, 0,false, true, 0,"R_SPARC_WDISP22",false,0,0x003fffff,false),
  HOWTO(R_SPARC_HI22,  10,2,22,false,0,true, false, 0,"R_SPARC_HI22",   false,0,0x003fffff,false),
  HOWTO(R_SPARC_22,     0,2,22,false,0,true,  true, 0,"R_SPARC_22",     false,0,0x003fffff,false),
  HOWTO(R_SPARC_13,     0,1,13,false,0,true,  true, 0,"R_SPARC_13",     false,0,0x00001fff,false),
  HOWTO(R_SPARC_LO10,   0,1,10,false,0,true, false, 0,"R_SPARC_LO10",   false,0,0x000003ff,false),
  HOWTO(R_SPARC_GOT10,  0,1,10,false,0,false, true, 0,"R_SPARC_GOT10",  false,0,0x000003ff,false),
  HOWTO(R_SPARC_GOT13,  0,1,13,false,0,false, true, 0,"R_SPARC_GOT13",  false,0,0x00001fff,false),
  HOWTO(R_SPARC_GOT22, 10,2,22,false,0,false, true, 0,"R_SPARC_GOT22",  false,0,0x003fffff,false),
  HOWTO(R_SPARC_PC10,   0,1,10,false,0,true,  true, 0,"R_SPARC_PC10",   false,0,0x000003ff,false),
  HOWTO(R_SPARC_PC22,   0,2,22,false,0,true,  true, 0,"R_SPARC_PC22",   false,0,0x003fffff,false),
  HOWTO(R_SPARC_WPLT30, 0,0,00,false,0,false,false, 0,"R_SPARC_WPLT30", false,0,0x00000000,false),
  HOWTO(R_SPARC_COPY,   0,0,00,false,0,false,false, 0,"R_SPARC_COPY",   false,0,0x00000000,false),
  HOWTO(R_SPARC_GLOB_DAT,0,0,00,false,0,false,false,0,"R_SPARC_GLOB_DAT",false,0,0x00000000,false),
  HOWTO(R_SPARC_JMP_SLOT,0,0,00,false,0,false,false,0,"R_SPARC_JMP_SLOT",false,0,0x00000000,false),
  HOWTO(R_SPARC_RELATIVE,0,0,00,false,0,false,false,0,"R_SPARC_RELATIVE",false,0,0x00000000,false),
  HOWTO(R_SPARC_UA32,    0,0,00,false,0,false,false,0,"R_SPARC_UA32",    false,0,0x00000000,false),
};
#endif

static void
DEFUN(elf_info_to_howto, (abfd, cache_ptr, dst),
      bfd		*abfd AND
      arelent		*cache_ptr AND
      Elf_Internal_Rela	*dst)
{
  /* FIXME!!! just doing sparc for now... */
#ifdef sparc
  BFD_ASSERT (ELF_R_TYPE(dst->r_info) < 24);

  cache_ptr->howto = &elf_howto_table[ELF_R_TYPE(dst->r_info)];
#else
  fprintf (stderr, "elf_info_to_howto not implemented\n");
  abort ();
#endif
}
      
static boolean
DEFUN(elf_slurp_reloca_table,(abfd, asect, symbols),
      bfd            *abfd AND
      sec_ptr         asect AND
      asymbol       **symbols)
{
  Elf_External_Rela   *native_relocs;
  arelent        *reloc_cache;
  arelent        *cache_ptr;

  unsigned int idx;
  
  if (asect->relocation)
    return true;
  if (asect->reloc_count == 0)
    return true;
  if (asect->flags & SEC_CONSTRUCTOR)
    return true;

  bfd_seek (abfd, asect->rel_filepos, SEEK_SET);
  native_relocs = (Elf_External_Rela *)
    bfd_alloc(abfd, asect->reloc_count * sizeof(Elf_External_Rela));
  bfd_read ((PTR) native_relocs,
	    sizeof(Elf_External_Rela), asect->reloc_count, abfd);
  
  reloc_cache = (arelent *)
    bfd_alloc(abfd, (size_t) (asect->reloc_count * sizeof(arelent)));

  if (! reloc_cache) {
    bfd_error = no_memory;
    return false;
  } 
  
  for (idx = 0; idx < asect->reloc_count; idx ++) 
    {
#ifdef RELOC_PROCESSING
      /* sparc, 68k, 88k, 860 use rela only. */
      /* 386 and we32000 use rel only... fix it for them later. */
      Elf_Internal_Rela dst;
      Elf_External_Rela  *src;

      cache_ptr = reloc_cache + idx;
      src = native_relocs + idx;
      elf_swap_reloca_in(abfd, src, &dst);

      RELOC_PROCESSING(cache_ptr, &dst, symbols, abfd, asect);
#else
      Elf_Internal_Rela dst;
      Elf_External_Rela  *src;

      cache_ptr = reloc_cache + idx;
      src = native_relocs + idx;

      elf_swap_reloca_in(abfd, src, &dst);

      if(asect->flags & SEC_RELOC)
	{
	  /* relocatable, so the offset is off of the section */
	  cache_ptr->address = dst.r_offset + asect->vma;
	}
      else
	{
	  /* non-relocatable, so the offset a virtual address */
	  cache_ptr->address = dst.r_offset;
	}
      /* ELF_R_SYM(dst.r_info) is the symbol table offset... */
      cache_ptr->sym_ptr_ptr = symbols + ELF_R_SYM(dst.r_info);
      cache_ptr->addend = dst.r_addend;

      /* Fill in the cache_ptr->howto field from dst.r_type */
      elf_info_to_howto(abfd, cache_ptr, &dst);
#endif
    }

  asect->relocation = reloc_cache;
  return true;
}


static unsigned int
elf_canonicalize_reloc (abfd, section, relptr, symbols)
bfd            *abfd;
sec_ptr         section;
arelent       **relptr;
asymbol       **symbols;
{
  arelent        *tblptr = section->relocation;
  unsigned int    count = 0;

  /* snarfed from coffcode.h */
  /* FIXME: this could be reloc... */
  elf_slurp_reloca_table(abfd, section, symbols);

  tblptr = section->relocation;
  if (!tblptr)
    return 0;

  for (; count++ < section->reloc_count;)
    *relptr++ = tblptr++;

  *relptr = 0;
  return section->reloc_count;
}

static unsigned int
DEFUN (elf_get_symtab, (abfd, alocation),
       bfd            *abfd AND
       asymbol       **alocation)
{

  if (!elf_slurp_symbol_table (abfd, alocation))
    return (0);
  else
    return (bfd_get_symcount (abfd));
}

static asymbol *
DEFUN (elf_make_empty_symbol, (abfd),
       bfd *abfd)
{
  elf_symbol_type *newsym;

  newsym = (elf_symbol_type *) bfd_zalloc (abfd, sizeof (elf_symbol_type));
  if (! newsym)
    {
      bfd_error = no_memory;
      return (NULL);
    }
  else
    {
      newsym -> symbol.the_bfd = abfd;
      return (&newsym -> symbol);
    }
}

static void 
DEFUN (elf_print_symbol,(ignore_abfd, filep, symbol, how),
      bfd            *ignore_abfd AND
      PTR           filep AND
      asymbol        *symbol AND
      bfd_print_symbol_type how)
{
  FILE *file = (FILE *)filep;
  switch (how)
    {
    case bfd_print_symbol_name:
      fprintf(file, "%s", symbol->name);
      break;
    case bfd_print_symbol_more:
      fprintf(file, "elf %lx %lx",
	      symbol->value,
	      symbol->flags);
      break;
    case bfd_print_symbol_nm:
    case bfd_print_symbol_all:
      {
	CONST char *section_name;
	section_name = symbol->section? symbol->section->name : "(*none*)";
	bfd_print_symbol_vandf((PTR) file, symbol);
	fprintf(file, " %s\t%s",
		section_name,
		symbol->name);
      }
      break;
    }

}

static alent *
DEFUN (elf_get_lineno,(ignore_abfd, symbol),
      bfd            *ignore_abfd AND
      asymbol        *symbol)
{
  fprintf (stderr, "elf_get_lineno unimplemented\n");
  fflush (stderr);
  abort ();
  return (NULL);
}

static boolean
DEFUN (elf_set_arch_mach,(abfd, arch, machine),
      bfd            *abfd AND
      enum bfd_architecture arch AND
      unsigned long   machine)
{
  /* Allow any architecture to be supported by the elf backend */
  switch(arch)
    {
    case bfd_arch_unknown:	/* EM_NONE */
    case bfd_arch_sparc:	/* EM_SPARC */
    case bfd_arch_i386:		/* EM_386 */
    case bfd_arch_m68k:		/* EM_68K */
    case bfd_arch_m88k:		/* EM_88K */
    case bfd_arch_i860:		/* EM_860 */
    case bfd_arch_mips:		/* EM_MIPS (MIPS R3000) */
      return  bfd_default_set_arch_mach(abfd, arch, machine);
    default:
      return false;
    }
}

static boolean
DEFUN (elf_find_nearest_line,(abfd,
			      section,
			      symbols,
			      offset,
			      filename_ptr,
			      functionname_ptr,
			      line_ptr),
      bfd            *abfd AND
      asection       *section AND
      asymbol       **symbols AND
      bfd_vma         offset AND
      CONST char      **filename_ptr AND
      CONST char       **functionname_ptr AND
      unsigned int   *line_ptr)
{
  fprintf (stderr, "elf_find_nearest_line unimplemented\n");
  fflush (stderr);
  abort ();
  return (false);
}

static int 
DEFUN (elf_sizeof_headers, (abfd, reloc),
      bfd *abfd AND
      boolean reloc)
{
  fprintf (stderr, "elf_sizeof_headers unimplemented\n");
  fflush (stderr);
  abort ();
  return (0);
}

boolean
DEFUN(elf_set_section_contents, (abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
  int dest_sect;
  void *contents;
  if (abfd->output_has_begun == false) /* set by bfd.c handler? */
    {
      /* do setup calculations (FIXME) */
      elf_compute_section_file_positions(abfd);
    }
#if 0
  if(bfd_seek (abfd, (file_ptr)section->filepos + offset, SEEK_SET) == -1)
    return false;
  if(bfd_write (location, (bfd_size_type)1, count, abfd) != count)
    return false;
#endif
  /* we really just need to save the contents away... */
  dest_sect = elf_section_from_bfd_section(abfd, section);
  if(!dest_sect)
    return false;

  /* FIXME: allocate in set_section_size, then copy in here... */
  contents = (void*)bfd_alloc(abfd, count);
  BFD_ASSERT(contents);
  memcpy(contents, location, count);
  elf_elfsections (abfd)[dest_sect].contents = contents;

  return true;
}


/* This structure contains everything that BFD knows about a target.
   It includes things like its byte order, name, what routines to call
   to do various operations, etc.  Every BFD points to a target structure
   with its "xvec" member.

   There are two such structures here:  one for big-endian machines and
   one for little-endian machines.   */

/* Archives are generic or unimplemented.  */
#define elf_slurp_armap			bfd_false
#define elf_slurp_extended_name_table	_bfd_slurp_extended_name_table
#define elf_truncate_arname		bfd_dont_truncate_arname
#define elf_openr_next_archived_file	bfd_generic_openr_next_archived_file
#define elf_generic_stat_arch_elt	bfd_generic_stat_arch_elt
#define	elf_write_armap			(PROTO (boolean, (*),		\
     (bfd *arch, unsigned int elength, struct orl *map, unsigned int orl_count,	\
      int stridx))) bfd_false

/* Ordinary section reading and writing */
#define elf_new_section_hook		_bfd_dummy_new_section_hook
#define elf_get_section_contents	bfd_generic_get_section_contents
/* #define elf_set_section_contents	bfd_generic_set_section_contents */
#define	elf_close_and_cleanup		bfd_generic_close_and_cleanup

#define elf_bfd_debug_info_start	bfd_void
#define elf_bfd_debug_info_end		bfd_void
#define elf_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void
#define elf_bfd_get_relocated_section_contents \
 bfd_generic_get_relocated_section_contents
#define elf_bfd_relax_section bfd_generic_relax_section
bfd_target elf_big_vec =
{
  /* name: identify kind of target */
  "elf-big",

  /* flavour: general indication about file */
  bfd_target_elf_flavour,

  /* byteorder_big_p: data is big endian */
  true,

  /* header_byteorder_big_p: header is also big endian */
  true,

  /* object_flags: mask of all file flags */
  (HAS_RELOC | EXEC_P | HAS_LINENO | HAS_DEBUG | HAS_SYMS | HAS_LOCALS |
   DYNAMIC | WP_TEXT),
  
  /* section_flags: mask of all section flags */
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_READONLY |
   SEC_CODE | SEC_DATA), 


   /* leading_symbol_char: is the first char of a user symbol
      predictable, and if so what is it */
   0,

  /* ar_pad_char: pad character for filenames within an archive header
     FIXME:  this really has nothing to do with ELF, this is a characteristic
     of the archiver and/or os and should be independently tunable */
  '/',

  /* ar_max_namelen: maximum number of characters in an archive header
     FIXME:  this really has nothing to do with ELF, this is a characteristic
     of the archiver and should be independently tunable.  This value is
     a WAG (wild a** guess) */
  15,

  /* align_power_min: minimum alignment restriction for any section
     FIXME:  this value may be target machine dependent */
  3,

  /* Routines to byte-swap various sized integers from the data sections */
  _do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16,

  /* Routines to byte-swap various sized integers from the file headers */
  _do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16,

  /* bfd_check_format: check the format of a file being read */
  { _bfd_dummy_target,		/* unknown format */
    elf_object_p,		/* assembler/linker output (object file) */
    bfd_generic_archive_p,	/* an archive */
    elf_core_file_p		/* a core file */
  },

  /* bfd_set_format: set the format of a file being written */
  { bfd_false,
    elf_mkobject,
    _bfd_generic_mkarchive,
    bfd_false
  },

  /* bfd_write_contents: write cached information into a file being written */
  { bfd_false,
    elf_write_object_contents,
    _bfd_write_archive_contents,
    bfd_false
  },

  /* Initialize a jump table with the standard macro.  All names start
     with "elf" */
  JUMP_TABLE(elf),

  /* SWAP_TABLE */
  NULL, NULL, NULL
};

bfd_target elf_little_vec =
{
  /* name: identify kind of target */
  "elf-little",

  /* flavour: general indication about file */
  bfd_target_elf_flavour,

  /* byteorder_big_p: data is big endian */
  false,		/* Nope -- this one's little endian */

  /* header_byteorder_big_p: header is also big endian */
  false,		/* Nope -- this one's little endian */

  /* object_flags: mask of all file flags */
  (HAS_RELOC | EXEC_P | HAS_LINENO | HAS_DEBUG | HAS_SYMS | HAS_LOCALS |
   DYNAMIC | WP_TEXT),
  
  /* section_flags: mask of all section flags */
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_READONLY |
   SEC_DATA), 

   /* leading_symbol_char: is the first char of a user symbol
      predictable, and if so what is it */
   0,

  /* ar_pad_char: pad character for filenames within an archive header
     FIXME:  this really has nothing to do with ELF, this is a characteristic
     of the archiver and/or os and should be independently tunable */
  '/',

  /* ar_max_namelen: maximum number of characters in an archive header
     FIXME:  this really has nothing to do with ELF, this is a characteristic
     of the archiver and should be independently tunable.  This value is
     a WAG (wild a** guess) */
  15,

  /* align_power_min: minimum alignment restriction for any section
     FIXME:  this value may be target machine dependent */
  3,

  /* Routines to byte-swap various sized integers from the data sections */
  _do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16,

  /* Routines to byte-swap various sized integers from the file headers */
  _do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16,

  /* bfd_check_format: check the format of a file being read */
  { _bfd_dummy_target,		/* unknown format */
    elf_object_p,		/* assembler/linker output (object file) */
    bfd_generic_archive_p,	/* an archive */
    elf_core_file_p		/* a core file */
  },

  /* bfd_set_format: set the format of a file being written */
  { bfd_false,
    elf_mkobject,
    _bfd_generic_mkarchive,
    bfd_false
  },

  /* bfd_write_contents: write cached information into a file being written */
  { bfd_false,
    elf_write_object_contents,
    _bfd_write_archive_contents,
    bfd_false
  },

  /* Initialize a jump table with the standard macro.  All names start
     with "elf" */
  JUMP_TABLE(elf),

  /* SWAP_TABLE */
  NULL, NULL, NULL
};
