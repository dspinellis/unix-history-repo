/* Execute AIXcoff files, for GDB.
   Copyright 1988, 1989, 1991, 1992 Free Software Foundation, Inc.
   Derived from exec.c.  Modified by IBM Corporation.
   Donated by IBM Corporation and Cygnus Support.

This file is part of GDB.

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

/* xcoff-exec -	deal with executing XCOFF files.  */
  
#include "defs.h"

#include <sys/types.h>
#include <sys/param.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/ldr.h>

#include "frame.h"
#include "inferior.h"
#include "target.h"
#include "gdbcmd.h"
#include "gdbcore.h"
#include "symfile.h"
#include "objfiles.h"

#include "libbfd.h"		/* BFD internals (sigh!)  FIXME */
#include "xcoffsolib.h"

/* Prototypes for local functions */

static void
file_command PARAMS ((char *, int));

static void
exec_close PARAMS ((int));

struct section_table *exec_sections, *exec_sections_end;

#define eq(s0, s1)	!strcmp(s0, s1)

/* Whether to open exec and core files read-only or read-write.  */

int write_files = 0;

extern int info_verbose;

bfd *exec_bfd;			/* needed by core.c	*/

extern char *getenv();
extern void add_syms_addr_command ();
extern void symbol_file_command ();
static void exec_files_info();
extern struct objfile *lookup_objfile_bfd ();

#if 0
/*
 * the vmap struct is used to describe the virtual address space of
 * the target we are manipulating.  The first entry is always the "exec"
 * file.  Subsequent entries correspond to other objects that are
 * mapped into the address space of a process created from the "exec" file.
 * These are either in response to exec()ing the file, in which case all
 * shared libraries are loaded, or a "load" system call, followed by the
 * user's issuance of a "load" command.
 */
struct vmap {
	struct vmap *nxt;	/* ^ to next in chain			*/
	bfd *bfd;		/* BFD for mappable object library	*/
	char *name;		/* ^ to object file name		*/
	char *member;		/* ^ to member name			*/
	CORE_ADDR tstart;	/* virtual addr where member is mapped	*/
	CORE_ADDR tend;		/* virtual upper bound of member	*/
	CORE_ADDR tadj;		/* heuristically derived adjustment	*/
	CORE_ADDR dstart;	/* virtual address of data start	*/
	CORE_ADDR dend;		/* vitrual address of data end		*/
};


struct vmap_and_bfd {
  bfd *pbfd;
  struct vmap *pvmap;
};

static struct vmap *vmap;	/* current vmap				*/
#endif /* 0 */

struct vmap *vmap;		/* current vmap */

extern struct target_ops exec_ops;


/* exec_close -	done with exec file, clean up all resources. */

static void
exec_close(quitting)
{
  register struct vmap *vp, *nxt;
  struct objfile *obj;
  
  for (nxt = vmap; vp = nxt; )
    {
      nxt = vp->nxt;

      /* if there is an objfile associated with this bfd,
	 free_objfile() will do proper cleanup of objfile *and* bfd. */
		   
      if (obj = lookup_objfile_bfd (vp->bfd))
	free_objfile (obj);
      else
	bfd_close(vp->bfd);
      
      free_named_symtabs(vp->name);
      free(vp);
    }
  
  vmap = 0;

  if (exec_bfd) {
    bfd_close (exec_bfd);
    exec_bfd = NULL;
  }
  if (exec_ops.to_sections) {
    free (exec_ops.to_sections);
    exec_ops.to_sections = NULL;
    exec_ops.to_sections_end = NULL;
  }
}

/*
 * exec_file_command -	handle the "exec" command, &c.
 */
void
exec_file_command(filename, from_tty)
char *filename;
{
  target_preopen(from_tty);

  /* Remove any previous exec file.  */
  unpush_target(&exec_ops);

  /* Now open and digest the file the user requested, if any. */

  if (filename) {
  	char *scratch_pathname;
  	int scratch_chan;
      
  	filename = tilde_expand(filename);
  	make_cleanup (free, filename);
      
  	scratch_chan = openp(getenv("PATH"), 1, filename,
			     write_files? O_RDWR: O_RDONLY, 0,
  			     &scratch_pathname);
  	if (scratch_chan < 0)
	  perror_with_name(filename);

  	exec_bfd = bfd_fdopenr(scratch_pathname, NULL, scratch_chan);
  	if (!exec_bfd)
	  error("Could not open `%s' as an executable file: %s"
  		      , scratch_pathname, bfd_errmsg(bfd_error));

  	/* make sure we have an object file */

  	if (!bfd_check_format(exec_bfd, bfd_object))
  		error("\"%s\": not in executable format: %s.",
  		      scratch_pathname, bfd_errmsg(bfd_error));


  	/* setup initial vmap */

  	map_vmap (exec_bfd, 0);
  	if (!vmap)
  		error("Can't find the file sections in `%s': %s",
  		      exec_bfd->filename, bfd_errmsg(bfd_error));

	if (build_section_table (exec_bfd, &exec_ops.to_sections,
				&exec_ops.to_sections_end))
	  error ("Can't find the file sections in `%s': %s", 
  		exec_bfd->filename, bfd_errmsg (bfd_error));

  	/* make sure core, if present, matches */
  	validate_files();

  	push_target(&exec_ops);

  	/* Tell display code(if any) about the changed file name. */

  	if (exec_file_display_hook)
  		(*exec_file_display_hook)(filename);
  } 
  else {
  	exec_close(0);	/* just in case	*/
  	if (from_tty)
	  printf("No exec file now.\n");
  }
}

/* Set both the exec file and the symbol file, in one command.  What a
 * novelty.  Why did GDB go through four major releases before this
 * command was added?
 */
static void
file_command(arg, from_tty)
char *arg; {

	exec_file_command(arg, from_tty);
	symbol_file_command(arg, from_tty);
}

/* Locate all mappable sections of a BFD file. 
   table_pp_char is a char * to get it through bfd_map_over_sections;
   we cast it back to its proper type.  */

static void
add_to_section_table (abfd, asect, table_pp_char)
     bfd *abfd;
     sec_ptr asect;
     char *table_pp_char;
{
  struct section_table **table_pp = (struct section_table **)table_pp_char;
  flagword aflag;

  aflag = bfd_get_section_flags (abfd, asect);
  /* FIXME, we need to handle BSS segment here...it alloc's but doesn't load */
  if (!(aflag & SEC_LOAD))
    return;
  if (0 == bfd_section_size (abfd, asect))
    return;
  (*table_pp)->bfd = abfd;
  (*table_pp)->sec_ptr = asect;
  (*table_pp)->addr = bfd_section_vma (abfd, asect);
  (*table_pp)->endaddr = (*table_pp)->addr + bfd_section_size (abfd, asect);
  (*table_pp)++;
}

int
build_section_table (some_bfd, start, end)
     bfd *some_bfd;
     struct section_table **start, **end;
{
  unsigned count;

  count = bfd_count_sections (some_bfd);
  if (count == 0)
    abort();	/* return 1? */
  if (*start)
    free (*start);
  *start = (struct section_table *) xmalloc (count * sizeof (**start));
  *end = *start;
  bfd_map_over_sections (some_bfd, add_to_section_table, (char *)end);
  if (*end > *start + count)
    abort();
  /* We could realloc the table, but it probably loses for most files.  */
  return 0;
}

/*
 * lookup_symtab_bfd -	find if we currently have any symbol tables from bfd
 */
struct objfile *
lookup_objfile_bfd(bfd *bfd) {
	register struct objfile *s;

	for (s = object_files; s; s = s->next)
		if (s->obfd == bfd)
			return s;
	return 0;
}


void
sex_to_vmap(bfd *bf, sec_ptr sex, struct vmap_and_bfd *vmap_bfd) 
{
  register struct vmap *vp, **vpp;
  register struct symtab *syms;
  bfd *arch = vmap_bfd->pbfd;
  vp = vmap_bfd->pvmap;

  if ((bfd_get_section_flags(bf, sex) & SEC_LOAD) == 0)
    return;

  if (!strcmp(bfd_section_name(bf, sex), ".text")) {
    vp->tstart = 0;
    vp->tend   = vp->tstart + bfd_section_size(bf, sex);

    /* When it comes to this adjustment value, in contrast to our previous
       belief shared objects should behave the same as the main load segment.
       This is the offset from the beginning of text section to the first
       real instruction. */

    vp->tadj = sex->filepos - bfd_section_vma(bf, sex);
  }

  else if (!strcmp(bfd_section_name(bf, sex), ".data")) {
    vp->dstart = 0;
    vp->dend   = vp->dstart + bfd_section_size(bf, sex);
  }

  else if (!strcmp(bfd_section_name(bf, sex), ".bss"))	/* FIXMEmgo */
    printf ("bss section in exec! Don't know what the heck to do!\n");
}

/* Make a vmap for the BFD "bf", which might be a member of the archive
   BFD "arch".  If we have not yet read in symbols for this file, do so.  */

map_vmap (bfd *bf, bfd *arch)
{
  struct vmap_and_bfd vmap_bfd;
  struct vmap *vp, **vpp;
  struct objfile *obj;

  vp = (void*) xmalloc (sizeof (*vp));
  bzero (vp, sizeof (*vp));
  vp->nxt = 0;
  vp->bfd = bf;
  vp->name = bfd_get_filename(arch ? arch : bf);
  vp->member = arch ? bfd_get_filename(bf) : "";
  
  vmap_bfd.pbfd = arch;
  vmap_bfd.pvmap = vp;
  bfd_map_over_sections (bf, sex_to_vmap, &vmap_bfd);

  obj = lookup_objfile_bfd (bf);
  if (exec_bfd && !obj) {
    obj = allocate_objfile (bf, 0);

#if 0
    /* This is only needed if we want to load shared libraries no matter what.
       Since we provide the choice of incremental loading of shared objects
       now, we do not have to load them as default anymore. */
    
    syms_from_objfile (obj, 0, 0, 0);
    new_symfile_objfile (obj, 0, 0);
#endif
  }

  /* find the end of the list, and append. */
  for (vpp = &vmap; *vpp; vpp = &(*vpp)->nxt)
  ;
  *vpp = vp;
}


/* true, if symbol table and minimal symbol table are relocated. */

int symtab_relocated = 0;


/*  vmap_symtab -	handle symbol translation on vmapping */

vmap_symtab(vp, old_start, vip)
register struct vmap *vp;
CORE_ADDR old_start;
struct stat *vip; 
{
  register struct symtab *s;
  register struct objfile *objfile;
  register struct minimal_symbol *msymbol;
  
  /*
   * for each symbol table generated from the vp->bfd
   */
  ALL_OBJFILES (objfile)
    {
      for (s = objfile -> symtabs; s != NULL; s = s -> next) {
	
	/* skip over if this is not relocatable and doesn't have a line table */
	if (s->nonreloc && !LINETABLE (s))
	  continue;
	
	/* matching the symbol table's BFD and the *vp's BFD is hairy.
	   exec_file creates a seperate BFD for possibly the
	   same file as symbol_file.FIXME ALL THIS MUST BE RECTIFIED. */
	
	if (objfile->obfd == vp->bfd) {
	  /* if they match, we luck out. */
	  ;
	} else if (vp->member[0]) {
	  /* no match, and member present, not this one. */
	  continue;
	} else {
	  struct stat si;
	  FILE *io;
	  
	  /*
	   * no match, and no member. need to be sure.
	   */
	  io = bfd_cache_lookup(objfile->obfd);
	  if (!io)
	    fatal("cannot find BFD's iostream for sym");
	  /*
	   * see if we are referring to the same file
	   */
	  if (fstat(fileno(io), &si) < 0)
	    fatal("cannot fstat BFD for sym");
	  
	  if (vip && (si.st_dev != vip->st_dev
	      || si.st_ino != vip->st_ino))
	    continue;
	}
	
	if (vp->tstart != old_start) {

	  /* Once we find a relocation base address for one of the symtabs
	     in this objfile, it will be the same for all symtabs in this
	     objfile. Clean this algorithm. FIXME. */

	  for (; s; s = s->next)
	    if (!s->nonreloc || LINETABLE(s))
		vmap_symtab_1(s, vp, old_start);

#if 0 
	  Himm.., recently we nullified trampoline entry names in order not
	  to confuse them with real symbols.  Appearently this turned into a
	  problem, and msymbol vector did not get relocated properly.  If
	  msymbols have to have non-null names, then we should name
	  trampoline entries with empty strings. 

	  ALL_MSYMBOLS (objfile, msymbol)
#else
	  for (msymbol = objfile->msymbols;
		msymbol->name || msymbol->address; (msymbol)++)
#endif
	      if (msymbol->address < TEXT_SEGMENT_BASE)
		msymbol -> address += vp->tstart - old_start;

	   break;
	}
      }
    }

  if (vp->tstart != old_start) {
    /* breakpoints need to be relocated as well. */
    fixup_breakpoints (0, TEXT_SEGMENT_BASE, vp->tstart - old_start);
  }
  
  symtab_relocated = 1;
}


vmap_symtab_1(s, vp, old_start)
register struct symtab *s;
register struct vmap *vp;
CORE_ADDR old_start; 
{
    register int i, j;
    int len, blen;
    register struct linetable *l;
    struct blockvector *bv;
    register struct block *b;
    int depth;
    register ulong reloc, dreloc;
    
    if ((reloc = vp->tstart - old_start) == 0)
	return;

    dreloc = vp->dstart;			/* data relocation */

    /*
     * The line table must be relocated.  This is only present for
     * .text sections, so only vp->text type maps need be considered.
     */
    l = LINETABLE (s);
    if (l) {
      len = l->nitems;
      for (i = 0; i < len; i++)
	l->item[i].pc += reloc;
    }

    /* if this symbol table is not relocatable, only line table should
       be relocated and the rest ignored. */
    if (s->nonreloc)
      return;
    
    bv  = BLOCKVECTOR(s);
    len = BLOCKVECTOR_NBLOCKS(bv);
    
    for (i = 0; i < len; i++) {
	b = BLOCKVECTOR_BLOCK(bv, i);
	
	BLOCK_START(b) += reloc;
	BLOCK_END(b)   += reloc;
	
	blen = BLOCK_NSYMS(b);
	for (j = 0; j < blen; j++) {
	    register struct symbol *sym;
	    
	    sym = BLOCK_SYM(b, j);
	    switch (SYMBOL_NAMESPACE(sym)) {
	      case STRUCT_NAMESPACE:
	      case UNDEF_NAMESPACE:
		continue;
		
	      case LABEL_NAMESPACE:
	      case VAR_NAMESPACE:
		break;
	    }
	    
	    switch (SYMBOL_CLASS(sym)) {
	      case LOC_CONST:
	      case LOC_CONST_BYTES:
	      case LOC_LOCAL:
	      case LOC_REGISTER:
	      case LOC_ARG:
	      case LOC_LOCAL_ARG:
	      case LOC_REF_ARG:
	      case LOC_REGPARM:
	      case LOC_TYPEDEF:
		continue;
		
#ifdef FIXME
	      case LOC_EXTERNAL:
#endif
	      case LOC_LABEL:
		SYMBOL_VALUE_ADDRESS(sym) += reloc;
		break;

	      case LOC_STATIC:
		SYMBOL_VALUE_ADDRESS(sym) += dreloc;
		break;

	      case LOC_BLOCK:
		break;
		
	      default:
		fatal("botched symbol class %x"
		      , SYMBOL_CLASS(sym));
		break;
	    }
	}
    }
}

/*
 * add_vmap -	add a new vmap entry based on ldinfo() information
 */
add_vmap(ldi)
register struct ld_info *ldi; {
	bfd *bfd, *last;
	register char *mem, *objname;

	/* This ldi structure was allocated using alloca() in 
	   xcoff_relocate_symtab(). Now we need to have persistent object 
	   and member names, so we should save them. */

	mem = ldi->ldinfo_filename + strlen(ldi->ldinfo_filename) + 1;
	mem = savestring (mem, strlen (mem));
	objname = savestring (ldi->ldinfo_filename, strlen (ldi->ldinfo_filename));

	bfd = bfd_fdopenr(objname, NULL, ldi->ldinfo_fd);
	if (!bfd)
	  error("Could not open `%s' as an executable file: %s",
					objname, bfd_errmsg(bfd_error));


	/* make sure we have an object file */

	if (bfd_check_format(bfd, bfd_object))
	  map_vmap (bfd, 0);

	else if (bfd_check_format(bfd, bfd_archive)) {
		last = 0;
		/*
		 * FIXME??? am I tossing BFDs?  bfd?
		 */
		while (last = bfd_openr_next_archived_file(bfd, last))
			if (eq(mem, last->filename))
				break;

		if (!last) {
		  bfd_close(bfd);
		  /* FIXME -- should be error */
		  warning("\"%s\": member \"%s\" missing.", bfd->filename, mem);
		  return;
		}

		if (!bfd_check_format(last, bfd_object)) {
			bfd_close(last);	/* XXX???	*/
			goto obj_err;
		}

		map_vmap (last, bfd);
	}
	else {
	    obj_err:
		bfd_close(bfd);
/* FIXME -- should be error */
		warning("\"%s\": not in executable format: %s."
		      , objname, bfd_errmsg(bfd_error));
		return;
	}
}


/* As well as symbol tables, exec_sections need relocation. After
   the inferior process' termination, there will be a relocated symbol
   table exist with no corresponding inferior process. At that time, we
   need to use `exec' bfd, rather than the inferior process's memory space
   to look up symbols.

   `exec_sections' need to be relocated only once, as long as the exec
   file remains unchanged.
*/
vmap_exec ()
{
  static bfd *execbfd;
  int i;

  if (execbfd == exec_bfd)
    return;

  execbfd = exec_bfd;

  if (!vmap || !exec_ops.to_sections)
    error ("vmap_exec: vmap or exec_ops.to_sections == 0\n");

  for (i=0; &exec_ops.to_sections[i] < exec_ops.to_sections_end; i++)
    {
      if (strcmp(".text", exec_ops.to_sections[i].sec_ptr->name) == 0)
	{
	  exec_ops.to_sections[i].addr += vmap->tstart;
	  exec_ops.to_sections[i].endaddr += vmap->tstart;
	}
      else if (strcmp(".data", exec_ops.to_sections[i].sec_ptr->name) == 0)
	{
	  exec_ops.to_sections[i].addr += vmap->dstart;
	  exec_ops.to_sections[i].endaddr += vmap->dstart;
	}
    }
}


int
text_adjustment (abfd)
bfd *abfd;
{
  static bfd *execbfd;
  static int adjustment;
  sec_ptr  sect;

  if (exec_bfd == execbfd)
    return adjustment;

  sect = bfd_get_section_by_name (abfd, ".text");
  if (sect)
    adjustment = sect->filepos - sect->vma;
  else
    adjustment = 0x200;				/* just a wild assumption */

  return adjustment;
}


/*
 * vmap_ldinfo -	update VMAP info with ldinfo() information
 *
 * Input:
 *	ldi	-	^ to ldinfo() results.
 */
vmap_ldinfo(ldi)
register struct ld_info *ldi;
{
  struct stat ii, vi;
  register struct vmap *vp;
  register got_one, retried;
  CORE_ADDR ostart;

  /*
   * for each *ldi, see if we have a corresponding *vp
   *	if so, update the mapping, and symbol table.
   *	if not, add an entry and symbol table.
   */
  do {
	char *name = ldi->ldinfo_filename;
	char *memb = name + strlen(name) + 1;

	retried = 0;

	if (fstat(ldi->ldinfo_fd, &ii) < 0)
		fatal("cannot fstat(%d) on %s"
		      , ldi->ldinfo_fd
		      , name);
retry:
	for (got_one = 0, vp = vmap; vp; vp = vp->nxt) {
	  FILE *io;

	  /* First try to find a `vp', which is the same as in ldinfo.
	     If not the same, just continue and grep the next `vp'. If same,
	     relocate its tstart, tend, dstart, dend values. If no such `vp'
	     found, get out of this for loop, add this ldi entry as a new vmap
	     (add_vmap) and come back, fins its `vp' and so on... */

	  /* The filenames are not always sufficient to match on. */

	  if ((name[0] == "/" && !eq(name, vp->name))
	      	|| (memb[0] && !eq(memb, vp->member)))
	    continue;

	  io = bfd_cache_lookup(vp->bfd);		/* totally opaque! */
	  if (!io)
	    fatal("cannot find BFD's iostream for %s", vp->name);

	  /* see if we are referring to the same file */

	  if (fstat(fileno(io), &vi) < 0)
	    fatal("cannot fstat BFD for %s", vp->name);

	  if (ii.st_dev != vi.st_dev || ii.st_ino != vi.st_ino)
	    continue;

	  if (!retried)
	    close(ldi->ldinfo_fd);

	  ++got_one;

	  /* found a corresponding VMAP. remap! */
	  ostart = vp->tstart;

	  vp->tstart = ldi->ldinfo_textorg;
	  vp->tend   = vp->tstart + ldi->ldinfo_textsize;
	  vp->dstart = ldi->ldinfo_dataorg;
	  vp->dend   = vp->dstart + ldi->ldinfo_datasize;

	  if (vp->tadj) {
	    vp->tstart += vp->tadj;
	    vp->tend   += vp->tadj;
	  }

	  /* relocate symbol table(s). */
	  vmap_symtab(vp, ostart, &vi);

	  /* there may be more, so we don't break out of the loop. */
	}

	/* if there was no matching *vp, we must perforce create the sucker(s) */
  	if (!got_one && !retried) {
	  add_vmap(ldi);
	  ++retried;
	  goto retry;
	}
  } while (ldi->ldinfo_next
	 && (ldi = (void *) (ldi->ldinfo_next + (char *) ldi)));

}

/*
 * vmap_inferior -	print VMAP info for inferior
 */
vmap_inferior() {

	if (inferior_pid == 0)
	  return 0;				/* normal processing	*/

	exec_files_info();
	return 1;
}

/* Read or write the exec file.

   Args are address within exec file, address within gdb address-space,
   length, and a flag indicating whether to read or write.

   Result is a length:

	0:    We cannot handle this address and length.
	> 0:  We have handled N bytes starting at this address.
	      (If N == length, we did it all.)  We might be able
	      to handle more bytes beyond this length, but no
	      promises.
	< 0:  We cannot handle this address, but if somebody
	      else handles (-N) bytes, we can start from there.

    The same routine is used to handle both core and exec files;
    we just tail-call it with more arguments to select between them.  */

int
xfer_memory (memaddr, myaddr, len, write, target)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
     int write;
     struct target_ops *target;
{
  boolean res;
  struct section_table *p;
  CORE_ADDR nextsectaddr, memend;
  boolean (*xfer_fn) PARAMS ((bfd *, sec_ptr, PTR, file_ptr, bfd_size_type));

  if (len <= 0)
    abort();

  memend = memaddr + len;
  xfer_fn = write? bfd_set_section_contents: bfd_get_section_contents;
  nextsectaddr = memend;

  for (p = target->to_sections; p < target->to_sections_end; p++)
    {
      if (p->addr <= memaddr)
	if (p->endaddr >= memend)
	  {
	    /* Entire transfer is within this section.  */
	    res = xfer_fn (p->bfd, p->sec_ptr, myaddr, memaddr - p->addr, len);
	    return (res != false)? len: 0;
	  }
	else if (p->endaddr <= memaddr)
	  {
	    /* This section ends before the transfer starts.  */
	    continue;
	  }
	else 
	  {
	    /* This section overlaps the transfer.  Just do half.  */
	    len = p->endaddr - memaddr;
	    res = xfer_fn (p->bfd, p->sec_ptr, myaddr, memaddr - p->addr, len);
	    return (res != false)? len: 0;
	  }
      else if (p->addr < nextsectaddr)
	nextsectaddr = p->addr;
    }

  if (nextsectaddr >= memend)
    return 0;				/* We can't help */
  else
    return - (nextsectaddr - memaddr);	/* Next boundary where we can help */
}

void
print_section_info (t, abfd)
  struct target_ops *t;
  bfd *abfd;
{
  struct section_table *p;

  printf_filtered ("\t`%s', ", bfd_get_filename(abfd));
  wrap_here ("        ");
  printf_filtered ("file type %s.\n", bfd_get_target(abfd));

  for (p = t->to_sections; p < t->to_sections_end; p++) {
    printf_filtered ("\t%s", local_hex_string_custom (p->addr, "08"));
    printf_filtered (" - %s", local_hex_string_custom (p->endaddr, "08"));
    if (info_verbose)
      printf_filtered (" @ %s",
		       local_hex_string_custom (p->sec_ptr->filepos, "08"));
    printf_filtered (" is %s", bfd_section_name (p->bfd, p->sec_ptr));
    if (p->bfd != abfd) {
      printf_filtered (" in %s", bfd_get_filename (p->bfd));
    }
    printf_filtered ("\n");
  }
}


static void
exec_files_info (t)
  struct target_ops *t;
{
  register struct vmap *vp = vmap;

  print_section_info (t, exec_bfd);

  if (!vp)
    return;

  printf("\tMapping info for file `%s'.\n", vp->name);

  printf("\t  %8.8s   %8.8s   %8.8s   %8.8s %8.8s %s\n",
    "tstart", "tend", "dstart", "dend", "section", "file(member)");

  for (; vp; vp = vp->nxt)
     printf("\t0x%8.8x 0x%8.8x 0x%8.8x 0x%8.8x %s%s%s%s\n",
	vp->tstart,
	vp->tend,
	vp->dstart,
	vp->dend,
	vp->name,
	*vp->member ? "(" : "",
	vp->member,
	*vp->member ? ")" : "");
}

#ifdef DAMON
/*  Damon's implementation of set_section_command! It is based on the sex member
  (which is a section pointer from vmap) of vmap.
  We will not have multiple vmap entries (one for each section), rather transmit
  text and data base offsets and fix them at the same time. Elimination of sex
  entry in vmap make this function obsolute, use the one from exec.c. 
  Need further testing!!	FIXMEmgo.  */

static void
set_section_command(args, from_tty)
char *args; 
{
	register struct vmap *vp = vmap;
	char *secname;
	unsigned seclen;
	unsigned long secaddr;
	char secprint[100];
	long offset;

	if (args == 0)
		error("Must specify section name and its virtual address");

	/* Parse out section name */
	for (secname = args; !isspace(*args); args++)
		;
	seclen = args - secname;

	/* Parse out new virtual address */
	secaddr = parse_and_eval_address(args);

	for (vp = vmap; vp; vp = vp->nxt) {
		if (!strncmp(secname
			     , bfd_section_name(vp->bfd, vp->sex), seclen)
		    && bfd_section_name(vp->bfd, vp->sex)[seclen] == '\0') {
			offset = secaddr - vp->tstart;
			vp->tstart += offset;
			vp->tend   += offset;
			exec_files_info();
			return;
		}
	} 

	if (seclen >= sizeof(secprint))
		seclen = sizeof(secprint) - 1;
	strncpy(secprint, secname, seclen);
	secprint[seclen] = '\0';
	error("Section %s not found", secprint);
}
#else
static void
set_section_command (args, from_tty)
     char *args;
     int from_tty;
{
  struct section_table *p;
  char *secname;
  unsigned seclen;
  unsigned long secaddr;
  char secprint[100];
  long offset;

  if (args == 0)
    error ("Must specify section name and its virtual address");

  /* Parse out section name */
  for (secname = args; !isspace(*args); args++) ;
  seclen = args - secname;

  /* Parse out new virtual address */
  secaddr = parse_and_eval_address (args);

  for (p = exec_ops.to_sections; p < exec_ops.to_sections_end; p++) {
    if (!strncmp (secname, bfd_section_name (exec_bfd, p->sec_ptr), seclen)
	&& bfd_section_name (exec_bfd, p->sec_ptr)[seclen] == '\0') {
      offset = secaddr - p->addr;
      p->addr += offset;
      p->endaddr += offset;
      if (from_tty)
        exec_files_info(&exec_ops);
      return;
    }
  } 
  if (seclen >= sizeof (secprint))
    seclen = sizeof (secprint) - 1;
  strncpy (secprint, secname, seclen);
  secprint[seclen] = '\0';
  error ("Section %s not found", secprint);
}

#endif /* !DAMON */

struct target_ops exec_ops = {
	"exec", "Local exec file",
	"Use an executable file as a target.\n\
Specify the filename of the executable file.",
	exec_file_command, exec_close, /* open, close */
	find_default_attach, 0, 0, 0, /* attach, detach, resume, wait, */
	0, 0, /* fetch_registers, store_registers, */
	0, /* prepare_to_store */
	xfer_memory, exec_files_info,
	0, 0, /* insert_breakpoint, remove_breakpoint, */
	0, 0, 0, 0, 0, /* terminal stuff */
	0, 0, /* kill, load */
	0, /* lookup sym */
	find_default_create_inferior,
	0, /* mourn_inferior */
	0, /* can_run */
	0, /* notice_signals */
	file_stratum, 0, /* next */
	0, 1, 0, 0, 0,	/* all mem, mem, stack, regs, exec */
	0, 0,			/* section pointers */
	OPS_MAGIC,		/* Always the last thing */
};


void
_initialize_exec()
{

  add_com("file", class_files, file_command,
	   "Use FILE as program to be debugged.\n\
It is read for its symbols, for getting the contents of pure memory,\n\
and it is the program executed when you use the `run' command.\n\
If FILE cannot be found as specified, your execution directory path\n\
($PATH) is searched for a command of that name.\n\
No arg means to have no executable file and no symbols.");

  add_com("exec-file", class_files, exec_file_command,
	   "Use FILE as program for getting contents of pure memory.\n\
If FILE cannot be found as specified, your execution directory path\n\
is searched for a command of that name.\n\
No arg means have no executable file.");

  add_com("section", class_files, set_section_command,
   "Change the base address of section SECTION of the exec file to ADDR.\n\
This can be used if the exec file does not contain section addresses,\n\
(such as in the a.out format), or when the addresses specified in the\n\
file itself are wrong.  Each section must be changed separately.  The\n\
``info files'' command lists all the sections and their addresses.");

  add_target(&exec_ops);
}
