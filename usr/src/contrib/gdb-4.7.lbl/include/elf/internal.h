/* ELF support for BFD.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

   Written by Fred Fish @ Cygnus Support, from information published
   in "UNIX System V Release 4, Programmers Guide: ANSI C and
   Programming Support Tools".

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


/* This file is part of ELF support for BFD, and contains the portions
   that describe how ELF is represented internally in the BFD library.
   I.E. it describes the in-memory representation of ELF.  It requires
   the elf-common.h file which contains the portions that are common to
   both the internal and external representations. */
   
/* Types used by various structures, functions, etc. */

typedef unsigned long	Elf_Addr;	/* Unsigned program address */
typedef unsigned long	Elf_Off;	/* Unsigned file offset */
typedef 	 long	Elf_Sword;	/* Signed large integer */
typedef unsigned long	Elf_Word;	/* Unsigned large integer */
typedef unsigned short	Elf_Half;	/* Unsigned medium integer */
typedef unsigned char	Elf_Char;	/* Unsigned tiny integer */

/* ELF Header */

#define EI_NIDENT	16		/* Size of e_ident[] */

typedef struct elf_internal_ehdr {
  unsigned char	e_ident[EI_NIDENT];	/* ELF "magic number" */
  Elf_Half	e_type;			/* Identifies object file type */
  Elf_Half	e_machine;		/* Specifies required architecture */
  Elf_Word	e_version;		/* Identifies object file version */
  Elf_Addr	e_entry;		/* Entry point virtual address */
  Elf_Off	e_phoff;		/* Program header table file offset */
  Elf_Off	e_shoff;		/* Section header table file offset */
  Elf_Word	e_flags;		/* Processor-specific flags */
  Elf_Half	e_ehsize;		/* ELF header size in bytes */
  Elf_Half	e_phentsize;		/* Program header table entry size */
  Elf_Half	e_phnum;		/* Program header table entry count */
  Elf_Half	e_shentsize;		/* Section header table entry size */
  Elf_Half	e_shnum;		/* Section header table entry count */
  Elf_Half	e_shstrndx;		/* Section header string table index */
} Elf_Internal_Ehdr;

/* Program header */

typedef struct elf_internal_phdr {
  Elf_Word	p_type;			/* Identifies program segment type */
  Elf_Off	p_offset;		/* Segment file offset */
  Elf_Addr	p_vaddr;		/* Segment virtual address */
  Elf_Addr	p_paddr;		/* Segment physical address */
  Elf_Word	p_filesz;		/* Segment size in file */
  Elf_Word	p_memsz;		/* Segment size in memory */
  Elf_Word	p_flags;		/* Segment flags */
  Elf_Word	p_align;		/* Segment alignment, file & memory */
} Elf_Internal_Phdr;

/* Section header */

typedef struct elf_internal_shdr {
  Elf_Word	sh_name;		/* Section name, index in string tbl */
  Elf_Word	sh_type;		/* Type of section */
  Elf_Word	sh_flags;		/* Miscellaneous section attributes */
  Elf_Addr	sh_addr;		/* Section virtual addr at execution */
  Elf_Off	sh_offset;		/* Section file offset */
  Elf_Word	sh_size;		/* Size of section in bytes */
  Elf_Word	sh_link;		/* Index of another section */
  Elf_Word	sh_info;		/* Additional section information */
  Elf_Word	sh_addralign;		/* Section alignment */
  Elf_Word	sh_entsize;		/* Entry size if section holds table */

  /* The internal rep also has some cached info associated with it. */
  void		*rawdata;		/* null if unused... */
  void		*contents;		/* null if unused... */
  
} Elf_Internal_Shdr;

/* Symbol table entry */

typedef struct elf_internal_sym {
  Elf_Word	st_name;		/* Symbol name, index in string tbl */
  Elf_Addr	st_value;		/* Value of the symbol */
  Elf_Word	st_size;		/* Associated symbol size */
  Elf_Char	st_info;		/* Type and binding attributes */
  Elf_Char	st_other;		/* No defined meaning, 0 */
  Elf_Half	st_shndx;		/* Associated section index */
} Elf_Internal_Sym;

/* Note segments */

typedef struct elf_internal_note {
  Elf_Word	namesz;			/* Size of entry's owner string */
  Elf_Word	descsz;			/* Size of the note descriptor */
  Elf_Word	type;			/* Interpretation of the descriptor */
  char		name[1];		/* Start of the name+desc data */
} Elf_Internal_Note;

/* Relocation Entries */

typedef struct elf_internal_rel {
  Elf_Addr	r_offset;	/* Location at which to apply the action */
  Elf_Word	r_info;		/* index and type of relocation */
} Elf_Internal_Rel;

typedef struct elf_internal_rela {
  Elf_Addr	r_offset;	/* Location at which to apply the action */
  Elf_Word	r_info;		/* Index and Type of relocation */
  Elf_Sword	r_addend;	/* Constant addend used to compute value */
} Elf_Internal_Rela;
