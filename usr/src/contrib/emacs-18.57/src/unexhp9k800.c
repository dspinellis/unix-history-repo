/* Unexec for HP 9000 Series 800 machines.
   Bob Desinger <hpsemc!bd@hplabs.hp.com>

   Note that the GNU project considers support for HP operation a
   peripheral activity which should not be allowed to divert effort
   from development of the GNU system.  Changes in this code will be
   installed when users send them in, but aside from that we don't
   plan to think about it, or about whether other Emacs maintenance
   might break it.


  Unexec creates a copy of the old a.out file, and replaces the old data
  area with the current data area.  When the new file is executed, the
  process will see the same data structures and data values that the
  original process had when unexec was called.
  
  Unlike other versions of unexec, this one copies symbol table and
  debug information to the new a.out file.  Thus, the new a.out file
  may be debugged with symbolic debuggers.
  
  If you fix any bugs in this, I'd like to incorporate your fixes.
  Send them to uunet!hpda!hpsemc!jmorris or jmorris%hpsemc@hplabs.HP.COM.
  
  CAVEATS:
  This routine saves the current value of all static and external
  variables.  This means that any data structure that needs to be
  initialized must be explicitly reset.  Variables will not have their
  expected default values.
  
  Unfortunately, the HP-UX signal handler has internal initialization
  flags which are not explicitly reset.  Thus, for signals to work in
  conjunction with this routine, the following code must executed when
  the new process starts up.
  
  void _sigreturn();
  ...
  sigsetreturn(_sigreturn);
*/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include <a.out.h>

#define NBPG 2048
#define roundup(x,n) ( ( (x)+(n-1) ) & ~(n-1) )  /* n is power of 2 */
#define min(x,y)  ( ((x)<(y))?(x):(y) )


/* Create a new a.out file, same as old but with current data space */

unexec(new_name, old_name, new_end_of_text, dummy1, dummy2)
     char new_name[];		/* name of the new a.out file to be created */
     char old_name[];		/* name of the old a.out file */
     char *new_end_of_text;	/* ptr to new edata/etext; NOT USED YET */
     int dummy1, dummy2;	/* not used by emacs */
{
  int old, new;
  int old_size, new_size;
  struct header hdr;
  struct som_exec_auxhdr auxhdr;
  
  /* For the greatest flexibility, should create a temporary file in
     the same directory as the new file.  When everything is complete,
     rename the temp file to the new name.
     This way, a program could update its own a.out file even while
     it is still executing.  If problems occur, everything is still
     intact.  NOT implemented.  */
  
  /* Open the input and output a.out files */
  old = open(old_name, O_RDONLY);
  if (old < 0)
    { perror(old_name); exit(1); }
  new = open(new_name, O_CREAT|O_RDWR|O_TRUNC, 0777);
  if (new < 0)
    { perror(new_name); exit(1); }
  
  /* Read the old headers */
  read_header(old, &hdr, &auxhdr);
  
  /* Decide how large the new and old data areas are */
  old_size = auxhdr.exec_dsize;
  new_size = sbrk(0) - auxhdr.exec_dmem;
  
  /* Copy the old file to the new, up to the data space */
  lseek(old, 0, 0);
  copy_file(old, new, auxhdr.exec_dfile);
  
  /* Skip the old data segment and write a new one */
  lseek(old, old_size, 1);
  save_data_space(new, &hdr, &auxhdr, new_size);
  
  /* Copy the rest of the file */
  copy_rest(old, new);
  
  /* Update file pointers since we probably changed size of data area */
  update_file_ptrs(new, &hdr, &auxhdr, auxhdr.exec_dfile, new_size-old_size);
  
  /* Save the modified header */
  write_header(new, &hdr, &auxhdr);
  
  /* Close the binary file */
  close(old);
  close(new);
  exit(0);
}

/* Save current data space in the file, update header.  */

save_data_space(file, hdr, auxhdr, size)
     int file;
     struct header *hdr;
     struct som_exec_auxhdr *auxhdr;
     int size;
{
  /* Write the entire data space out to the file */
  if (write(file, auxhdr->exec_dmem, size) != size)
    { perror("Can't save new data space"); exit(1); }
  
  /* Update the header to reflect the new data size */
  auxhdr->exec_dsize = size;
  auxhdr->exec_bsize = 0;
}

/* Update the values of file pointers when something is inserted.  */

update_file_ptrs(file, hdr, auxhdr, location, offset)
     int file;
     struct header *hdr;
     struct som_exec_auxhdr *auxhdr;
     unsigned int location;
     int offset;
{
  struct subspace_dictionary_record subspace;
  int i;
  
  /* Increase the overall size of the module */
  hdr->som_length += offset;
  
  /* Update the various file pointers in the header */
#define update(ptr) if (ptr > location) ptr = ptr + offset
  update(hdr->aux_header_location);
  update(hdr->space_strings_location);
  update(hdr->init_array_location);
  update(hdr->compiler_location);
  update(hdr->symbol_location);
  update(hdr->fixup_request_location);
  update(hdr->symbol_strings_location);
  update(hdr->unloadable_sp_location);
  update(auxhdr->exec_tfile);
  update(auxhdr->exec_dfile);
  
  /* Do for each subspace dictionary entry */
  lseek(file, hdr->subspace_location, 0);
  for (i = 0; i < hdr->subspace_total; i++)
    {
      if (read(file, &subspace, sizeof(subspace)) != sizeof(subspace))
	{ perror("Can't read subspace record"); exit(1); }
      
      /* If subspace has a file location, update it */
      if (subspace.initialization_length > 0 
	  && subspace.file_loc_init_value > location)
	{
	  subspace.file_loc_init_value += offset;
	  lseek(file, -sizeof(subspace), 1);
	  if (write(file, &subspace, sizeof(subspace)) != sizeof(subspace))
	    { perror("Can't update subspace record"); exit(1); }
	}
    } 
  
  /* Do for each initialization pointer record */
  /* (I don't think it applies to executable files, only relocatables) */
#undef update
}

/* Read in the header records from an a.out file.  */

read_header(file, hdr, auxhdr)
     int file;
     struct header *hdr;
     struct som_exec_auxhdr *auxhdr;
{
  
  /* Read the header in */
  lseek(file, 0, 0);
  if (read(file, hdr, sizeof(*hdr)) != sizeof(*hdr))
    { perror("Couldn't read header from a.out file"); exit(1); }
  
  if (hdr->a_magic != EXEC_MAGIC && hdr->a_magic != SHARE_MAGIC
      &&  hdr->a_magic != DEMAND_MAGIC)
    {
      fprintf(stderr, "a.out file doesn't have legal magic number\n"); 
      exit(1);  
    }
  
  lseek(file, hdr->aux_header_location, 0);
  if (read(file, auxhdr, sizeof(*auxhdr)) != sizeof(*auxhdr))
    {
      perror("Couldn't read auxiliary header from a.out file");
      exit(1);
    }  
}

/* Write out the header records into an a.out file.  */

write_header(file, hdr, auxhdr)
     int file;
     struct header *hdr;
     struct som_exec_auxhdr *auxhdr;
{
  /* Update the checksum */
  hdr->checksum = calculate_checksum(hdr);
  
  /* Write the header back into the a.out file */
  lseek(file, 0, 0);
  if (write(file, hdr, sizeof(*hdr)) != sizeof(*hdr))
    { perror("Couldn't write header to a.out file"); exit(1); }
  lseek(file, hdr->aux_header_location, 0);
  if (write(file, auxhdr, sizeof(*auxhdr)) != sizeof(*auxhdr))
    { perror("Couldn't write auxiliary header to a.out file"); exit(1); }
}

/* Calculate the checksum of a SOM header record. */

calculate_checksum(hdr)
     struct header *hdr;
{
  int checksum, i, *ptr;
  
  checksum = 0;  ptr = (int *) hdr;
  
  for (i=0; i<sizeof(*hdr)/sizeof(int)-1; i++)
    checksum ^= ptr[i];
  
  return(checksum);
}

/* Copy size bytes from the old file to the new one.  */

copy_file(old, new, size)
     int new, old;
     int size;
{
  int len;
  int buffer[8196];  /* word aligned will be faster */
  
  for (; size > 0; size -= len)
    {
      len = min(size, sizeof(buffer));
      if (read(old, buffer, len) != len)
	{ perror("Read failure on a.out file"); exit(1); }
      if (write(new, buffer, len) != len)
	{ perror("Write failure in a.out file"); exit(1); }
    }
}

/* Copy the rest of the file, up to EOF.  */

copy_rest(old, new)
     int new, old;
{
  int buffer[4096];
  int len;
  
  /* Copy bytes until end of file or error */
  while ( (len = read(old, buffer, sizeof(buffer))) > 0)
    if (write(new, buffer, len) != len) break;
  
  if (len != 0)
    { perror("Unable to copy the rest of the file"); exit(1); }
}

#ifdef	DEBUG
display_header(hdr, auxhdr)
     struct header *hdr;
     struct som_exec_auxhdr *auxhdr;
{
  /* Display the header information (debug) */
  printf("\n\nFILE HEADER\n");
  printf("magic number %d \n", hdr->a_magic); 
  printf("text loc %.8x   size %d \n", auxhdr->exec_tmem, auxhdr->exec_tsize);
  printf("data loc %.8x   size %d \n", auxhdr->exec_dmem, auxhdr->exec_dsize);
  printf("entry     %x \n",   auxhdr->exec_entry);
  printf("Bss  segment size %u\n", auxhdr->exec_bsize);
  printf("\n");
  printf("data file loc %d    size %d\n",
	 auxhdr->exec_dfile, auxhdr->exec_dsize);
  printf("som_length %d\n", hdr->som_length);
  printf("unloadable sploc %d    size %d\n",
	 hdr->unloadable_sp_location, hdr->unloadable_sp_size);
}
#endif /* DEBUG */
