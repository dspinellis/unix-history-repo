/* Three-way file comparison program (diff3) for Project GNU
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Written by Randy Smith */

#ifdef __STDC__
#define VOID void
#else
#define VOID char
#endif

/* 
 * Include files.
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef USG
#include <fcntl.h>

/* Define needed BSD functions in terms of sysV library.  */

#define bcmp(s1,s2,n)	memcmp((s1),(s2),(n))
#define bzero(s,n)	memset((s),0,(n))

#ifndef XENIX
#define dup2(f,t)	(close(t),fcntl((f),F_DUPFD,(t)))
#endif

#define vfork	fork

#else /* not USG */
#include <sys/wait.h>
#endif /* not USG */

#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#undef WIFEXITED /* Avoid 4.3BSD incompatibility with Posix.  */
#endif
#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifdef sparc
/* vfork clobbers registers on the Sparc, so don't use it.  */
#define vfork fork
#endif

/*
 * Internal data structures and macros for the diff3 program; includes
 * data structures for both diff3 diffs and normal diffs.
 */

/*
 * Different files within a diff
 */
#define	FILE0	0
#define	FILE1	1
#define	FILE2	2

/*
 * Three way diffs are build out of two two-way diffs; the file which
 * the two two-way diffs share is:
 */
#define	FILEC	FILE0

/* The ranges are indexed by */
#define	START	0
#define	END	1

enum diff_type {
  ERROR,			/* Should not be used */
  ADD,				/* Two way diff add */
  CHANGE,			/* Two way diff change */
  DELETE,			/* Two way diff delete */
  DIFF_ALL,			/* All three are different */
  DIFF_1ST,			/* Only the first is different */
  DIFF_2ND,			/* Only the second */
  DIFF_3RD			/* Only the third */
};

/* Two-way diff */
struct diff_block {
  int ranges[2][2];	    	/* Ranges are inclusive */
  char **lines[2];		/* The actual lines (may contain nulls) */
  int *lengths[2];		/* Line lengths (including newlines, if any) */
  struct diff_block *next;
};

/* Three-way diff */

struct diff3_block {
  enum diff_type correspond;	/* Type of diff */
  int ranges[3][2];	    	/* Ranges are inclusive */
  char **lines[3];		/* The actual lines (may contain nulls) */
  int *lengths[3];		/* Line lengths (including newlines, if any) */
  struct diff3_block *next;
};

/*
 * Access the ranges on a diff block.
 */
#define	D_LOWLINE(diff, filenum)	\
  ((diff)->ranges[filenum][START])
#define	D_HIGHLINE(diff, filenum)	\
  ((diff)->ranges[filenum][END])
#define	D_NUMLINES(diff, filenum)	\
  (D_HIGHLINE((diff), (filenum)) - D_LOWLINE((diff), (filenum)) + 1)

/*
 * Access the line numbers in a file in a diff by relative line
 * numbers (i.e. line number within the diff itself).  Note that these
 * are lvalues and can be used for assignment.
 */
#define	D_RELNUM(diff, filenum, linenum)	\
  (*((diff)->lines[filenum] + linenum))
#define	D_RELLEN(diff, filenum, linenum)	\
  (*((diff)->lengths[filenum] + linenum))

/*
 * And get at them directly, when that should be necessary.
 */
#define	D_LINEARRAY(diff, filenum)	\
  ((diff)->lines[filenum])
#define	D_LENARRAY(diff, filenum)	\
  ((diff)->lengths[filenum])

/*
 * Next block.
 */
#define	D_NEXT(diff)	((diff)->next)

/*
 * Access the type of a diff3 block.
 */
#define	D3_TYPE(diff)	((diff)->correspond)

/*
 * Line mappings based on diffs.  The first maps off the top of the
 * diff, the second off of the bottom.
 */
#define	D_HIGH_MAPLINE(diff, fromfile, tofile, lineno)	\
  ((lineno)						\
   - D_HIGHLINE ((diff), (fromfile))			\
   + D_HIGHLINE ((diff), (tofile)))

#define	D_LOW_MAPLINE(diff, fromfile, tofile, lineno)	\
  ((lineno)						\
   - D_LOWLINE ((diff), (fromfile))			\
   + D_LOWLINE ((diff), (tofile)))

/*
 * General memory allocation function.
 */
#define	ALLOCATE(number, type)	\
  (type *) xmalloc ((number) * sizeof (type))

/*
 * Options variables for flags set on command line.
 *
 * ALWAYS_TEXT: Treat all files as text files; never treat as binary.
 *
 * EDSCRIPT: Write out an ed script instead of the standard diff3 format.
 *
 * FLAGGING: Indicates that in the case of overlapping diffs (type
 * DIFF_ALL), the lines which would normally be deleted from file 1
 * should be preserved with a special flagging mechanism.
 *
 * DONT_WRITE_OVERLAP: 1 if information for overlapping diffs should
 * not be output.
 *
 * DONT_WRITE_SIMPLE: 1 if information for non-overlapping diffs
 * should not be output. 
 *
 * FINALWRITE: 1 if a :wq should be included at the end of the script
 * to write out the file being edited.
 *
 * MERGE: output a merged file.
 */
int always_text;
int edscript;
int flagging;
int dont_write_overlap;
int dont_write_simple;
int finalwrite;
int merge;

extern int optind;

char *argv0;

/*
 * Forward function declarations.
 */
struct diff_block *process_diff ();
struct diff3_block *make_3way_diff ();
void output_diff3 ();
int output_diff3_edscript ();
int output_diff3_merge ();
void usage ();

struct diff3_block *using_to_diff3_block ();
int copy_stringlist ();
struct diff3_block *create_diff3_block ();
int compare_line_list ();

char *read_diff ();
enum diff_type process_diff_control ();
char *scan_diff_line ();

struct diff3_block *reverse_diff3_blocklist ();

VOID *xmalloc ();
VOID *xrealloc ();

char diff_program[] = DIFF_PROGRAM;

/*
 * Main program.  Calls diff twice on two pairs of input files,
 * combines the two diffs, and outputs them.
 */
main (argc, argv)
     int argc;
     char **argv;
{
  int c, i;
  int mapping[3];
  int rev_mapping[3];
  int incompat;
  int overlaps_found;
  struct diff_block *thread1, *thread2;
  struct diff3_block *diff;
  int tag_count = 0;
  /* Element 0 is for file 0, element 1 is for file 2.  */
  char *tag_strings[2];
  extern char *optarg;
  char *commonname;
  struct stat statb;

  incompat = 0;
  tag_strings[0] = tag_strings[1] = 0;

  argv0 = argv[0];
  
  while ((c = getopt (argc, argv, "aeimx3EXL:")) != EOF)
    {
      switch (c)
	{
	case 'a':
	  always_text = 1;
	  break;
	case 'x':
	  dont_write_simple = 1;
	  incompat++;
	  break;
	case '3':
	  dont_write_overlap = 1;
	  incompat++;
	  break;
	case 'i':
	  finalwrite = 1;
	  break;
	case 'm':
	  merge = 1;
	  break;
	case 'X':
	  dont_write_simple = 1;
	  /* Falls through */
	case 'E':
	  flagging = 1;
	  /* Falls through */
	case 'e':
	  incompat++;
	  break;
	case 'L':
	  /* Handle one or two -L arguments.  */
	  if (tag_count < 2)
	    {
	      tag_strings[tag_count++] = optarg;
	      break;
	    }
	  /* Falls through */
	case '?':
	default:
	  usage ();
	  /* NOTREACHED */
	}
    }

  edscript = incompat & ~merge;  /* -eExX3 without -m implies ed script.  */
  flagging |= ~incompat & merge;  /* -m without -eExX3 implies -E.  */

  if (incompat > 1  /* Ensure at most one of -eExX3.  */
      || finalwrite & (~incompat | merge)
	    /* -i needs one of -eExX3; -i -m would rewrite input file.  */
      || tag_count && ! flagging /* -L requires one of -EX.  */
      || argc - optind != 3)
    usage ();

  if (tag_strings[0] == 0)
    tag_strings[0] = argv[optind];
  if (tag_strings[1] == 0)
    tag_strings[1] = argv[optind + 2];

  if (*argv[optind] == '-' && *(argv[optind] + 1) == '\0')
    {
      /* Sigh.  We've got standard input as the first arg. We can't */
      /* call diff twice on stdin */
      if (! strcmp (argv[optind + 1], "-") || ! strcmp (argv[optind + 2], "-"))
	fatal ("`-' specified for more than one input file");
      mapping[0] = 1;
      mapping[1] = 2;
      mapping[2] = 0;
      rev_mapping[1] = 0;
      rev_mapping[2] = 1;
      rev_mapping[0] = 2;
    }
  else
    {
      /* Normal, what you'd expect */
      mapping[0] = 0;
      mapping[1] = 1;
      mapping[2] = 2;
      rev_mapping[0] = 0;
      rev_mapping[1] = 1;
      rev_mapping[2] = 2;
    }

  for (i = 0; i < 3; i++)
    if (argv[optind + i][0] != '-' || argv[optind + i][1] != '\0')
      if (stat (argv[optind + i], &statb) < 0)
	perror_with_exit (argv[optind + i]);
      else if ((statb.st_mode & S_IFMT) == S_IFDIR)
	{
	  fprintf (stderr, "%s: %s: Is a directory\n", argv0,
		   argv[optind + i]);
	  exit (2);
	}
  

  commonname = argv[optind + rev_mapping[0]];
  thread1 = process_diff (commonname, argv[optind + rev_mapping[1]]);
  thread2 = process_diff (commonname, argv[optind + rev_mapping[2]]);
  diff = make_3way_diff (thread1, thread2);
  if (edscript)
    overlaps_found
      = output_diff3_edscript (stdout, diff, mapping, rev_mapping,
			       tag_strings[0], argv[optind+1], tag_strings[1]);
  else if (merge)
    {
      if (! freopen (commonname, "r", stdin))
	perror_with_exit (commonname);
      overlaps_found
	= output_diff3_merge (stdin, stdout, diff, mapping, rev_mapping,
			      tag_strings[0], argv[optind+1], tag_strings[1]);
      if (ferror (stdin))
	fatal ("read error");
    }
  else
    {
      output_diff3 (stdout, diff, mapping, rev_mapping);
      overlaps_found = 0;
    }

  if (ferror (stdout) || fflush (stdout) != 0)
    fatal ("write error");
  exit (overlaps_found);
}
      
/*
 * Explain, patiently and kindly, how to use this program.  Then exit.
 */
void
usage ()
{
  fprintf (stderr, "Usage:\t%s [-exEX3 [-i | -m] [-L label1 -L label3]] file1 file2 file3\n",
	   argv0);
  fprintf (stderr, "\tOnly one of [exEX3] allowed\n");
  exit (2);
}

/*
 * Routines that combine the two diffs together into one.  The
 * algorithm used follows:
 *
 *   File0 is shared in common between the two diffs.
 *   Diff01 is the diff between 0 and 1.
 *   Diff02 is the diff between 0 and 2.
 *
 * 	1) Find the range for the first block in File0.
 *  	    a) Take the lowest of the two ranges (in File0) in the two
 *  	       current blocks (one from each diff) as being the low
 *  	       water mark.  Assign the upper end of this block as
 *  	       being the high water mark and move the current block up
 *  	       one.  Mark the block just moved over as to be used.
 *	    b) Check the next block in the diff that the high water
 *  	       mark is *not* from.  
 *	       
 *	       *If* the high water mark is above
 *  	       the low end of the range in that block, 
 * 
 *	           mark that block as to be used and move the current
 *  	           block up.  Set the high water mark to the max of
 *  	           the high end of this block and the current.  Repeat b.
 * 
 *  	 2) Find the corresponding ranges in Files1 (from the blocks
 *  	    in diff01; line per line outside of diffs) and in File2.
 *  	    Create a diff3_block, reserving space as indicated by the ranges.
 *	    
 *	 3) Copy all of the pointers for file0 in.  At least for now,
 *  	    do bcmp's between corresponding strings in the two diffs.
 *	    
 *	 4) Copy all of the pointers for file1 and 2 in.  Get what you
 *  	    need from file0 (when there isn't a diff block, it's
 *  	    identical to file0 within the range between diff blocks).
 *	    
 *	 5) If the diff blocks you used came from only one of the two
 * 	    strings of diffs, then that file (i.e. the one other than
 * 	    file 0 in that diff) is the odd person out.  If you used
 * 	    diff blocks from both sets, check to see if files 1 and 2 match:
 *	    
 *	        Same number of lines?  If so, do a set of bcmp's (if a
 *  	    bcmp matches; copy the pointer over; it'll be easier later
 *  	    if you have to do any compares).  If they match, 1 & 2 are
 *  	    the same.  If not, all three different.
 * 
 *   Then you do it again, until you run out of blocks. 
 * 
 */

/* 
 * This routine makes a three way diff (chain of diff3_block's) from two
 * two way diffs (chains of diff_block's).  It is assumed that each of
 * the two diffs passed are off of the same file (i.e. that each of the
 * diffs were made "from" the same file).  The three way diff pointer
 * returned will have numbering 0--the common file, 1--the other file
 * in diff1, and 2--the other file in diff2.
 */
struct diff3_block *
make_3way_diff (thread1, thread2)
     struct diff_block *thread1, *thread2;
{
/*
 * This routine works on the two diffs passed to it as threads.
 * Thread number 0 is diff1, thread number 1 is diff2.  The USING
 * array is set to the base of the list of blocks to be used to
 * construct each block of the three way diff; if no blocks from a
 * particular thread are to be used, that element of the using array
 * is set to 0.  The elements LAST_USING array are set to the last
 * elements on each of the using lists.
 *
 * The HIGH_WATER_MARK is set to the highest line number in File 0
 * described in any of the diffs in either of the USING lists.  The
 * HIGH_WATER_THREAD names the thread.  Similarly the BASE_WATER_MARK
 * and BASE_WATER_THREAD describe the lowest line number in File 0
 * described in any of the diffs in either of the USING lists.  The
 * HIGH_WATER_DIFF is the diff from which the HIGH_WATER_MARK was
 * taken. 
 *
 * The HIGH_WATER_DIFF should always be equal to LAST_USING
 * [HIGH_WATER_THREAD].  The OTHER_DIFF is the next diff to check for
 * higher water, and should always be equal to
 * CURRENT[HIGH_WATER_THREAD ^ 0x1].  The OTHER_THREAD is the thread
 * in which the OTHER_DIFF is, and hence should always be equal to
 * HIGH_WATER_THREAD ^ 0x1.
 *
 * The variable LAST_DIFF is kept set to the last diff block produced
 * by this routine, for line correspondence purposes between that diff
 * and the one currently being worked on.  It is initialized to
 * ZERO_DIFF before any blocks have been created.
 */

  struct diff_block
    *using[2],
    *last_using[2],
    *current[2];

  int
    high_water_mark;

  int
    high_water_thread,
    base_water_thread,
    other_thread;

  struct diff_block
    *high_water_diff,
    *other_diff;

  struct diff3_block
    *result,
    *tmpblock,
    *result_last,
    *last_diff;

  static struct diff3_block zero_diff = {
      ERROR,
      { {0, 0}, {0, 0}, {0, 0} },
      { (char **) 0, (char **) 0, (char **) 0 },
      { (int *) 0, (int *) 0, (int *) 0 },
      (struct diff3_block *) 0
      };

  /* Initialization */
  result = result_last = (struct diff3_block *) 0;
  current[0] = thread1; current[1] = thread2;
  last_diff = &zero_diff;

  /* Sniff up the threads until we reach the end */

  while (current[0] || current[1])
    {
      using[0] = using[1] = last_using[0] = last_using[1] =
	(struct diff_block *) 0;

      /* Setup low and high water threads, diffs, and marks.  */
      if (!current[0])
	base_water_thread = 1;
      else if (!current[1])
	base_water_thread = 0;
      else
	base_water_thread =
	  (D_LOWLINE (current[0], FILE0) > D_LOWLINE (current[1], FILE0));

      high_water_thread = base_water_thread;
      
      high_water_diff = current[high_water_thread];
	
#if 0
      /* low and high waters start off same diff */
      base_water_mark = D_LOWLINE (high_water_diff, FILE0);
#endif

      high_water_mark = D_HIGHLINE (high_water_diff, FILE0);

      /* Make the diff you just got info from into the using class */
      using[high_water_thread]
	= last_using[high_water_thread]
	= high_water_diff;
      current[high_water_thread] = high_water_diff->next;
      last_using[high_water_thread]->next
	= (struct diff_block *) 0;

      /* And mark the other diff */
      other_thread = high_water_thread ^ 0x1;
      other_diff = current[other_thread];

      /* Shuffle up the ladder, checking the other diff to see if it
         needs to be incorporated */
      while (other_diff
	     && D_LOWLINE (other_diff, FILE0) <= high_water_mark + 1)
	{

	  /* Incorporate this diff into the using list.  Note that
	     this doesn't take it off the current list */
	  if (using[other_thread])
	    last_using[other_thread]->next = other_diff;
	  else
	    using[other_thread] = other_diff;
	  last_using[other_thread] = other_diff;

	  /* Take it off the current list.  Note that this following
	     code assumes that other_diff enters it equal to
	     current[high_water_thread ^ 0x1] */
	  current[other_thread]
	    = current[other_thread]->next;
	  other_diff->next
	    = (struct diff_block *) 0;

	  /* Set the high_water stuff
	     If this comparison is equal, then this is the last pass
	     through this loop; since diff blocks within a given
	     thread cannot overlap, the high_water_mark will be
	     *below* the range_start of either of the next diffs. */

	  if (high_water_mark < D_HIGHLINE (other_diff, FILE0))
	    {
	      high_water_thread ^= 1;
	      high_water_diff = other_diff;
	      high_water_mark = D_HIGHLINE (other_diff, FILE0);
	    }

	  /* Set the other diff */
	  other_thread = high_water_thread ^ 0x1;
	  other_diff = current[other_thread];
	}

      /* The using lists contain a list of all of the blocks to be
         included in this diff3_block.  Create it.  */

      tmpblock = using_to_diff3_block (using, last_using,
				       base_water_thread, high_water_thread,
				       last_diff);

      if (!tmpblock)
	fatal ("internal: screwup in format of diff blocks");

      /* Put it on the list */
      if (result)
	result_last->next = tmpblock;
      else
	result = tmpblock;
      result_last = tmpblock;

      /* Setup corresponding lines correctly */
      last_diff = tmpblock;
    }
  return result;
}

/*
 * using_to_diff3_block:
 *   This routine takes two lists of blocks (from two separate diff
 * threads) and puts them together into one diff3 block.
 * It then returns a pointer to this diff3 block or 0 for failure.
 *
 * All arguments besides using are for the convenience of the routine;
 * they could be derived from the using array.
 * LAST_USING is a pair of pointers to the last blocks in the using
 * structure.
 * LOW_THREAD and HIGH_THREAD tell which threads contain the lowest
 * and highest line numbers for File0.
 * last_diff contains the last diff produced in the calling routine.
 * This is used for lines mappings which would still be identical to
 * the state that diff ended in.
 *
 * A distinction should be made in this routine between the two diffs
 * that are part of a normal two diff block, and the three diffs that
 * are part of a diff3_block.
 */
struct diff3_block *
using_to_diff3_block (using, last_using, low_thread, high_thread, last_diff)
     struct diff_block
       *using[2],
       *last_using[2];
     int low_thread, high_thread;
     struct diff3_block *last_diff;
{
  int lowc, highc, low1, high1, low2, high2;
  struct diff3_block *result;
  struct diff_block *ptr;
  int i;
  int current0line;
  
  /* Find the range in file0 */
  lowc = using[low_thread]->ranges[0][START];
  highc = last_using[high_thread]->ranges[0][END];

  /* Find the ranges in the other files.
     If using[x] is null, that means that the file to which that diff
     refers is equivalent to file 0 over this range */
  
  if (using[0])
    {
      low1 = D_LOW_MAPLINE (using[0], FILE0, FILE1, lowc);
      high1 = D_HIGH_MAPLINE (last_using[0], FILE0, FILE1, highc); 
    }
  else
    {
      low1 = D_HIGH_MAPLINE (last_diff, FILEC, FILE1, lowc);
      high1 = D_HIGH_MAPLINE (last_diff, FILEC, FILE1, highc);
    }

  /*
   * Note that in the following, we use file 1 relative to the diff,
   * and file 2 relative to the corresponding lines struct.
   */
  if (using[1])
    {
      low2 = D_LOW_MAPLINE (using[1], FILE0, FILE1, lowc);
      high2 = D_HIGH_MAPLINE (last_using[1], FILE0, FILE1, highc); 
    }
  else
    {
      low2 = D_HIGH_MAPLINE (last_diff, FILEC, FILE2, lowc);
      high2 = D_HIGH_MAPLINE (last_diff, FILEC, FILE2, highc);
    }

  /* Create a block with the appropriate sizes */
  result = create_diff3_block (lowc, highc, low1, high1, low2, high2);

  /* Copy over all of the information for File 0.  Return with a zero
     if any of the compares failed. */
  for (ptr = using[0]; ptr; ptr = D_NEXT (ptr))
    {
      int result_offset = D_LOWLINE (ptr, FILE0) - lowc;
      int copy_size
	= D_HIGHLINE (ptr, FILE0) - D_LOWLINE (ptr, FILE0) + 1;
      
      if (!copy_stringlist (D_LINEARRAY (ptr, FILE0),
			    D_LENARRAY (ptr, FILE0),
			    D_LINEARRAY (result, FILEC) + result_offset,
			    D_LENARRAY (result, FILEC) + result_offset,
			    copy_size))
	return 0;
    }

  for (ptr = using[1]; ptr; ptr = D_NEXT (ptr))
    {
      int result_offset = D_LOWLINE (ptr, FILEC) - lowc;
      int copy_size
	= D_HIGHLINE (ptr, FILEC) - D_LOWLINE (ptr, FILEC) + 1;
      
      if (!copy_stringlist (D_LINEARRAY (ptr, FILE0),
			    D_LENARRAY (ptr, FILE0),
			    D_LINEARRAY (result, FILEC) + result_offset,
			    D_LENARRAY (result, FILEC) + result_offset,
			    copy_size))
	return 0;
    }

  /* Copy stuff for file 1.  First deal with anything that might be
     before the first diff. */

  for (i = 0;
       i + low1 < (using[0] ? D_LOWLINE (using[0], FILE1) : high1 + 1);
       i++)
    {
      D_RELNUM (result, FILE1, i) = D_RELNUM (result, FILEC, i);
      D_RELLEN (result, FILE1, i) = D_RELLEN (result, FILEC, i);
    }
  
  for (ptr = using[0]; ptr; ptr = D_NEXT (ptr))
    {
      int result_offset = D_LOWLINE (ptr, FILE1) - low1;
      int copy_size
	= D_HIGHLINE (ptr, FILE1) - D_LOWLINE (ptr, FILE1) + 1;

      if (!copy_stringlist (D_LINEARRAY (ptr, FILE1),
			    D_LENARRAY (ptr, FILE1),
			    D_LINEARRAY (result, FILE1) + result_offset,
			    D_LENARRAY (result, FILE1) + result_offset,
			    copy_size))
	return 0;

      /* Catch the lines between here and the next diff */
      current0line = D_HIGHLINE (ptr, FILE0) + 1 - lowc;
      for (i = D_HIGHLINE (ptr, FILE1) + 1 - low1;
	   i < (D_NEXT (ptr) ?
		D_LOWLINE (D_NEXT (ptr), FILE1) :
		high1 + 1) - low1;
	   i++)
	{
	  D_RELNUM (result, FILE1, i)
	    = D_RELNUM (result, FILEC, current0line);
	  D_RELLEN (result, FILE1, i)
	    = D_RELLEN (result, FILEC, current0line++);
	}
    }

  /* Copy stuff for file 2.  First deal with anything that might be
     before the first diff. */

  for (i = 0;
       i + low2 < (using[1] ? D_LOWLINE (using[1], FILE1) : high2 + 1);
       i++)
    {
      D_RELNUM (result, FILE2, i) = D_RELNUM (result, FILEC, i);
      D_RELLEN (result, FILE2, i) = D_RELLEN (result, FILEC, i);
    }
  
  for (ptr = using[1]; ptr; ptr = D_NEXT (ptr))
    {
      int result_offset = D_LOWLINE (ptr, FILE1) - low2;
      int copy_size
	= D_HIGHLINE (ptr, FILE1) - D_LOWLINE (ptr, FILE1) + 1;

      if (!copy_stringlist (D_LINEARRAY (ptr, FILE1),
			    D_LENARRAY (ptr, FILE1),
			    D_LINEARRAY (result, FILE2) + result_offset,
			    D_LENARRAY (result, FILE2) + result_offset,
			    copy_size))
	return 0;

      /* Catch the lines between here and the next diff */
      current0line = D_HIGHLINE (ptr, FILE0) + 1 - lowc;
      for (i = D_HIGHLINE (ptr, FILE1) + 1 - low2;
	   i < (D_NEXT (ptr) ?
		D_LOWLINE (D_NEXT (ptr), FILE1) :
		high2 + 1) - low2;
	   i++)
	{
	  D_RELNUM (result, FILE2, i)
	    = D_RELNUM (result, FILEC, current0line);
	  D_RELLEN (result, FILE2, i)
	    = D_RELLEN (result, FILEC, current0line++);
	}
    }

  /* Set correspond */
  if (!using[0])
    D3_TYPE (result) = DIFF_3RD;
  else if (!using[1])
    D3_TYPE (result) = DIFF_2ND;
  else
    {
      int nl1
	= D_HIGHLINE (result, FILE1) - D_LOWLINE (result, FILE1) + 1;
      int nl2
	= D_HIGHLINE (result, FILE2) - D_LOWLINE (result, FILE2) + 1;

      if (nl1 != nl2
	  || !compare_line_list (D_LINEARRAY (result, FILE1),
				 D_LENARRAY (result, FILE1),
				 D_LINEARRAY (result, FILE2),
				 D_LENARRAY (result, FILE2),
				 nl1))
	D3_TYPE (result) = DIFF_ALL;
      else
	D3_TYPE (result) = DIFF_1ST;
    }
  
  return result;
}

/*
 * This routine copies pointers from a list of strings to a different list
 * of strings.  If a spot in the second list is already filled, it
 * makes sure that it is filled with the same string; if not it
 * returns 0, the copy incomplete.
 * Upon successful completion of the copy, it returns 1.
 */
int
copy_stringlist (fromptrs, fromlengths, toptrs, tolengths, copynum)
     char *fromptrs[], *toptrs[];
     int *fromlengths, *tolengths;
     int copynum;
{
  register char
    **f = fromptrs,
    **t = toptrs;
  register int
    *fl = fromlengths,
    *tl = tolengths;
  
  while (copynum--)
    {
      if (*t)
	{ if (*fl != *tl || bcmp (*f, *t, *fl)) return 0; }
      else
	{ *t = *f ; *tl = *fl; }

      t++; f++; tl++; fl++;
    }
  return 1;
}

/*
 * Create a diff3_block, with ranges as specified in the arguments.
 * Allocate the arrays for the various pointers (and zero them) based
 * on the arguments passed.  Return the block as a result.
 */
struct diff3_block *
create_diff3_block (low0, high0, low1, high1, low2, high2)
     register int low0, high0, low1, high1, low2, high2;
{
  struct diff3_block *result = ALLOCATE (1, struct diff3_block);
  int numlines;

  D3_TYPE (result) = ERROR;
  D_NEXT (result) = 0;

  /* Assign ranges */
  D_LOWLINE (result, FILE0) = low0;
  D_HIGHLINE (result, FILE0) = high0;
  D_LOWLINE (result, FILE1) = low1;
  D_HIGHLINE (result, FILE1) = high1;
  D_LOWLINE (result, FILE2) = low2;
  D_HIGHLINE (result, FILE2) = high2;

  /* Allocate and zero space */
  numlines = D_NUMLINES (result, FILE0);
  if (numlines)
    {
      D_LINEARRAY (result, FILE0) = ALLOCATE (numlines, char *);
      D_LENARRAY (result, FILE0) = ALLOCATE (numlines, int);
      bzero (D_LINEARRAY (result, FILE0), (numlines * sizeof (char *)));
      bzero (D_LENARRAY (result, FILE0), (numlines * sizeof (int)));
    }
  else
    {
      D_LINEARRAY (result, FILE0) = (char **) 0;
      D_LENARRAY (result, FILE0) = (int *) 0;
    }

  numlines = D_NUMLINES (result, FILE1);
  if (numlines)
    {
      D_LINEARRAY (result, FILE1) = ALLOCATE (numlines, char *);
      D_LENARRAY (result, FILE1) = ALLOCATE (numlines, int);
      bzero (D_LINEARRAY (result, FILE1), (numlines * sizeof (char *)));
      bzero (D_LENARRAY (result, FILE1), (numlines * sizeof (int)));
    }
  else
    {
      D_LINEARRAY (result, FILE1) = (char **) 0;
      D_LENARRAY (result, FILE1) = (int *) 0;
    }

  numlines = D_NUMLINES (result, FILE2);
  if (numlines)
    {
      D_LINEARRAY (result, FILE2) = ALLOCATE (numlines, char *);
      D_LENARRAY (result, FILE2) = ALLOCATE (numlines, int);
      bzero (D_LINEARRAY (result, FILE2), (numlines * sizeof (char *)));
      bzero (D_LENARRAY (result, FILE2), (numlines * sizeof (int)));
    }
  else
    {
      D_LINEARRAY (result, FILE2) = (char **) 0;
      D_LENARRAY (result, FILE2) = (int *) 0;
    }

  /* Return */
  return result;
}

/*
 * Compare two lists of lines of text.
 * Return 1 if they are equivalent, 0 if not.
 */
int
compare_line_list (list1, lengths1, list2, lengths2, nl)
     char *list1[], *list2[];
     int *lengths1, *lengths2;
     int nl;
{
  char
    **l1 = list1,
    **l2 = list2;
  int
    *lgths1 = lengths1,
    *lgths2 = lengths2;
  
  while (nl--)
    if (!*l1 || !*l2 || *lgths1 != *lgths2++
	|| bcmp (*l1++, *l2++, *lgths1++))
      return 0;
  return 1;
}

/* 
 * Routines to input and parse two way diffs.
 */

extern char **environ;

#define	DIFF_CHUNK_SIZE	10000

struct diff_block *
process_diff (filea, fileb)
     char *filea, *fileb;
{
  char *diff_contents;
  char *diff_limit;
  char *scan_diff;
  enum diff_type dt;
  int i;
  struct diff_block *block_list, *block_list_end, *bptr;

  diff_limit = read_diff (filea, fileb, &diff_contents);
  scan_diff = diff_contents;
  bptr = block_list_end = block_list = (struct diff_block *) 0;

  while (scan_diff < diff_limit)
    {
      bptr = ALLOCATE (1, struct diff_block);
      bptr->next = 0;
      bptr->lines[0] = bptr->lines[1] = (char **) 0;
      bptr->lengths[0] = bptr->lengths[1] = (int *) 0;
      
      dt = process_diff_control (&scan_diff, bptr);
      if (dt == ERROR || *scan_diff != '\n')
	{
	  fprintf (stderr, "%s: diff error: ", argv0);
	  do
	    {
	      putc (*scan_diff, stderr);
	    }
	  while (*scan_diff++ != '\n');
	  exit (2);
	}
      scan_diff++;
      
      /* Force appropriate ranges to be null, if necessary */
      switch (dt)
	{
	case ADD:
	  bptr->ranges[0][0]++;
	  break;
	case DELETE:
	  bptr->ranges[1][0]++;
	  break;
	case CHANGE:
	  break;
	default:
	  fatal ("internal: Bad diff type in process_diff");
	  break;
	}
      
      /* Allocate space for the pointers for the lines from filea, and
	 parcel them out among these pointers */
      if (dt != ADD)
	{
	  bptr->lines[0] = ALLOCATE ((bptr->ranges[0][END]
				      - bptr->ranges[0][START] + 1),
				     char *);
	  bptr->lengths[0] = ALLOCATE ((bptr->ranges[0][END]
					- bptr->ranges[0][START] + 1),
				       int);
	  for (i = 0; i <= (bptr->ranges[0][END]
			    - bptr->ranges[0][START]); i++)
	    scan_diff = scan_diff_line (scan_diff,
					&(bptr->lines[0][i]),
					&(bptr->lengths[0][i]),
					diff_limit,
					'<');
	}
      
      /* Get past the separator for changes */
      if (dt == CHANGE)
	{
	  if (strncmp (scan_diff, "---\n", 4))
	    fatal ("Bad diff format: bad change separator");
	  scan_diff += 4;
	}
      
      /* Allocate space for the pointers for the lines from fileb, and
	 parcel them out among these pointers */
      if (dt != DELETE)
	{
	  bptr->lines[1] = ALLOCATE ((bptr->ranges[1][END]
				      - bptr->ranges[1][START] + 1),
				     char *);
	  bptr->lengths[1] = ALLOCATE ((bptr->ranges[1][END]
					- bptr->ranges[1][START] + 1),
				       int);
	  for (i = 0; i <= (bptr->ranges[1][END]
			    - bptr->ranges[1][START]); i++)
	    scan_diff = scan_diff_line (scan_diff,
					&(bptr->lines[1][i]),
					&(bptr->lengths[1][i]),
					diff_limit,
					'>');
	}
      
      /* Place this block on the blocklist */
      if (block_list_end)
	block_list_end->next = bptr;
      else
	block_list = bptr;
      
      block_list_end = bptr;
      
    }

  return block_list;
}

/*
 * This routine will parse a normal format diff control string.  It
 * returns the type of the diff (ERROR if the format is bad).  All of
 * the other important information is filled into to the structure
 * pointed to by db, and the string pointer (whose location is passed
 * to this routine) is updated to point beyond the end of the string
 * parsed.  Note that only the ranges in the diff_block will be set by
 * this routine.
 *
 * If some specific pair of numbers has been reduced to a single
 * number, then both corresponding numbers in the diff block are set
 * to that number.  In general these numbers are interpetted as ranges
 * inclusive, unless being used by the ADD or DELETE commands.  It is
 * assumed that these will be special cased in a superior routine.  
 */

enum diff_type
process_diff_control (string, db)
     char **string;
     struct diff_block *db;
{
  char *s = *string;
  int holdnum;
  enum diff_type type;

/* These macros are defined here because they can use variables
   defined in this function.  Don't try this at home kids, we're
   trained professionals!

   Also note that SKIPWHITE only recognizes tabs and spaces, and
   that READNUM can only read positive, integral numbers */

#define	SKIPWHITE(s)	{ while (*s == ' ' || *s == '\t') s++; }
#define	READNUM(s, num)	\
	{ if (!isdigit (*s)) return ERROR; holdnum = 0;	\
	  do { holdnum = (*s++ - '0' + holdnum * 10); }	\
	  while (isdigit (*s)); (num) = holdnum; }

  /* Read first set of digits */
  SKIPWHITE (s);
  READNUM (s, db->ranges[0][START]);

  /* Was that the only digit? */
  SKIPWHITE(s);
  if (*s == ',')
    {
      /* Get the next digit */
      s++;
      READNUM (s, db->ranges[0][END]);
    }
  else
    db->ranges[0][END] = db->ranges[0][START];

  /* Get the letter */
  SKIPWHITE (s);
  switch (*s)
    {
    case 'a':
      type = ADD;
      break;
    case 'c':
      type = CHANGE;
      break;
    case 'd':
      type = DELETE;
      break;
    default:
      return ERROR;			/* Bad format */
    }
  s++;				/* Past letter */
  
  /* Read second set of digits */
  SKIPWHITE (s);
  READNUM (s, db->ranges[1][START]);

  /* Was that the only digit? */
  SKIPWHITE(s);
  if (*s == ',')
    {
      /* Get the next digit */
      s++;
      READNUM (s, db->ranges[1][END]);
      SKIPWHITE (s);		/* To move to end */
    }
  else
    db->ranges[1][END] = db->ranges[1][START];

  *string = s;
  return type;
}

char *
read_diff (filea, fileb, output_placement)
     char *filea, *fileb;
     char **output_placement;
{
  char *argv[6];
  char **ap;
  int fds[2];
  char *diff_result;
  int current_chunk_size;
  int bytes;
  int total;
  int pid, w;
  int wstatus;

  ap = argv;
  *ap++ = diff_program;
  if (always_text)
    *ap++ = "-a";
  *ap++ = "--";
  *ap++ = filea;
  *ap++ = fileb;
  *ap = (char *) 0;

  if (pipe (fds) < 0)
    perror_with_exit ("Pipe failed");

  pid = vfork ();
  if (pid == 0)
    {
      /* Child */
      close (fds[0]);
      if (fds[1] != fileno (stdout))
	{
	  dup2 (fds[1], fileno (stdout));
	  close (fds[1]);
	}
      execve (diff_program, argv, environ);
      /* Avoid stdio, because the parent process's buffers are inherited. */
      write (fileno (stderr), diff_program, strlen (diff_program));
      write (fileno (stderr), ": not found\n", 12);
      _exit (2);
    }

  if (pid == -1)
    perror_with_exit ("Fork failed");

  close (fds[1]);		/* Prevent erroneous lack of EOF */
  current_chunk_size = DIFF_CHUNK_SIZE;
  diff_result = (char *) xmalloc (current_chunk_size);
  total = 0;
  do {
    bytes = myread (fds[0],
		    diff_result + total,
		    current_chunk_size - total);
    total += bytes;
    if (total == current_chunk_size)
      diff_result = (char *) xrealloc (diff_result, (current_chunk_size *= 2));
  } while (bytes);

  if (total != 0 && diff_result[total-1] != '\n')
    fatal ("bad diff format; incomplete last line");

  *output_placement = diff_result;

  do
    if ((w = wait (&wstatus)) == -1)
      perror_with_exit ("Wait failed");
  while (w != pid);

  if (! (WIFEXITED (wstatus) && WEXITSTATUS (wstatus) < 2))
    fatal ("Subsidiary diff failed");

  return diff_result + total;
}


/*
 * Scan a regular diff line (consisting of > or <, followed by a
 * space, followed by text (including nulls) up to a newline.
 *
 * This next routine began life as a macro and many parameters in it
 * are used as call-by-reference values.
 */
char *
scan_diff_line (scan_ptr, set_start, set_length, limit, firstchar)
     char *scan_ptr, **set_start;
     int *set_length;
     char *limit;
     char firstchar;
{
  char *line_ptr;

  if (!(scan_ptr[0] == (firstchar)
	&& scan_ptr[1] == ' '))
    fatal ("Bad diff format; incorrect leading line chars");

  *set_start = line_ptr = scan_ptr + 2;
  while (*line_ptr++ != '\n')
    ;

  /* Include newline if the original line ended in a newline,
     or if an edit script is being generated.
     Copy any missing newline message to stderr if an edit script is being
     generated, because edit scripts cannot handle missing newlines.
     Return the beginning of the next line.  */
  *set_length = line_ptr - *set_start;
  if (line_ptr < limit && *line_ptr == '\\')
    {
      if (edscript)
	fprintf (stderr, "%s:", argv0);
      else
	--*set_length;
      line_ptr++;
      do
	{
	  if (edscript)
	    putc (*line_ptr, stderr);
	}
      while (*line_ptr++ != '\n');
    }

  return line_ptr;
}

/*
 * This routine outputs a three way diff passed as a list of
 * diff3_block's.
 * The argument MAPPING is indexed by external file number (in the
 * argument list) and contains the internal file number (from the
 * diff passed).  This is important because the user expects his
 * outputs in terms of the argument list number, and the diff passed
 * may have been done slightly differently (if the first argument in
 * the argument list was the standard input, for example).
 * REV_MAPPING is the inverse of MAPPING.
 */
void
output_diff3 (outputfile, diff, mapping, rev_mapping)
     FILE *outputfile;
     struct diff3_block *diff;
     int mapping[3], rev_mapping[3];
{
  int i;
  int oddoneout;
  char *cp;
  struct diff3_block *ptr;
  int line;
  int length;
  int dontprint;
  static int skew_increment[3] = { 2, 3, 1 }; /* 0==>2==>1==>3 */

  for (ptr = diff; ptr; ptr = D_NEXT (ptr))
    {
      char x[2];

      switch (ptr->correspond)
	{
	case DIFF_ALL:
	  x[0] = '\0';
	  dontprint = 3;	/* Print them all */
	  oddoneout = 3;	/* Nobody's odder than anyone else */
	  break;
	case DIFF_1ST:
	case DIFF_2ND:
	case DIFF_3RD:
	  oddoneout = rev_mapping[(int) ptr->correspond - (int) DIFF_1ST];
	    
	  x[0] = oddoneout + '1';
	  x[1] = '\0';
	  dontprint = oddoneout==0;
	  break;
	default:
	  fatal ("internal: Bad diff type passed to output");
	}
      fprintf (outputfile, "====%s\n", x);

      /* Go 0, 2, 1 if the first and third outputs are equivalent. */
      for (i = 0; i < 3;
	   i = (oddoneout == 1 ? skew_increment[i] : i + 1))
	{
	  int realfile = mapping[i];
	  int
	    lowt = D_LOWLINE (ptr, realfile),
	    hight = D_HIGHLINE (ptr, realfile);
	  
	  fprintf (outputfile, "%d:", i + 1);
	  switch (lowt - hight)
	    {
	    case 1:
	      fprintf (outputfile, "%da\n", lowt - 1);
	      break;
	    case 0:
	      fprintf (outputfile, "%dc\n", lowt);
	      break;
	    default:
	      fprintf (outputfile, "%d,%dc\n", lowt, hight);
	      break;
	    }

	  if (i == dontprint) continue;

	  for (line = 0; line < hight - lowt + 1; line++)
	    {
	      fprintf (outputfile, "  ");
	      cp = D_RELNUM (ptr, realfile, line);
	      length = D_RELLEN (ptr, realfile, line);
	      fwrite (cp, sizeof (char), length, outputfile);
	    }
	  if (line != 0 && cp[length - 1] != '\n')
	    fprintf (outputfile, "\n\\ No newline at end of file\n");
	}
    }
}

/*
 * This routine outputs a diff3 set of blocks as an ed script.  This
 * script applies the changes between file's 2 & 3 to file 1.  It
 * takes the precise format of the ed script to be output from global
 * variables set during options processing.  Note that it does
 * destructive things to the set of diff3 blocks it is passed; it
 * reverses their order (this gets around the problems involved with
 * changing line numbers in an ed script).
 *
 * Note that this routine has the same problem of mapping as the last
 * one did; the variable MAPPING maps from file number according to
 * the argument list to file number according to the diff passed.  All
 * files listed below are in terms of the argument list.
 * REV_MAPPING is the inverse of MAPPING.
 *
 * The arguments FILE0, FILE1 and FILE2 are the strings to print
 * as the names of the three files.  These may be the actual names,
 * or may be the arguments specified with -L.
 *
 * Returns 1 if overlaps were found.
 */

int
output_diff3_edscript (outputfile, diff, mapping, rev_mapping,
		       file0, file1, file2)
     FILE *outputfile;
     struct diff3_block *diff;
     int mapping[3], rev_mapping[3];
     char *file0, *file1, *file2;
{
  int i;
  int leading_dot;
  int overlaps_found = 0;
  struct diff3_block *newblock, *thisblock;

  leading_dot = 0;

  newblock = reverse_diff3_blocklist (diff);

  for (thisblock = newblock; thisblock; thisblock = thisblock->next)
    {
      /* Must do mapping correctly.  */
      enum diff_type type
	= ((thisblock->correspond == DIFF_ALL) ?
	   DIFF_ALL :
	   ((enum diff_type)
	    (((int) DIFF_1ST)
	     + rev_mapping[(int) thisblock->correspond - (int) DIFF_1ST])));

      /* If we aren't supposed to do this output block, skip it */
      if (type == DIFF_2ND || type == DIFF_1ST
	  || (type == DIFF_3RD && dont_write_simple)
	  || (type == DIFF_ALL && dont_write_overlap))
	continue;

      if (flagging && type == DIFF_ALL)
	/* Do special flagging */
	{

	  /* Put in lines from FILE2 with bracket */
	  fprintf (outputfile, "%da\n",
		   D_HIGHLINE (thisblock, mapping[FILE0]));
	  fprintf (outputfile, "=======\n");
	  for (i = 0;
	       i < D_NUMLINES (thisblock, mapping[FILE2]);
	       i++)
	    {
	      if (D_RELNUM (thisblock, mapping[FILE2], i)[0] == '.')
		{ leading_dot = 1; fprintf(outputfile, "."); }
	      fwrite (D_RELNUM (thisblock, mapping[FILE2], i), sizeof (char),
		      D_RELLEN (thisblock, mapping[FILE2], i), outputfile);
	    }
	  fprintf (outputfile, ">>>>>>> %s\n.\n", file2);
	  overlaps_found = 1;

	  /* Add in code to take care of leading dots, if necessary. */
	  if (leading_dot)
	    {
	      fprintf (outputfile, "%d,%ds/^\\.\\./\\./\n",
		       D_HIGHLINE (thisblock, mapping[FILE0]) + 1,
		       (D_HIGHLINE (thisblock, mapping[FILE0])
			+ D_NUMLINES (thisblock, mapping[FILE2])));
	      leading_dot = 0;
	    }

	  /* Put in code to do initial bracket of lines from FILE0  */
	  fprintf (outputfile, "%da\n<<<<<<< %s\n.\n",
		   D_LOWLINE (thisblock, mapping[FILE0]) - 1,
		   file0);
	}
      else if (D_NUMLINES (thisblock, mapping[FILE2]) == 0)
	/* Write out a delete */
	{
	  if (D_NUMLINES (thisblock, mapping[FILE0]) == 1)
	    fprintf (outputfile, "%dd\n",
		     D_LOWLINE (thisblock, mapping[FILE0]));
	  else
	    fprintf (outputfile, "%d,%dd\n",
		     D_LOWLINE (thisblock, mapping[FILE0]),
		     D_HIGHLINE (thisblock, mapping[FILE0]));
	}
      else
	/* Write out an add or change */
	{
	  switch (D_NUMLINES (thisblock, mapping[FILE0]))
	    {
	    case 0:
	      fprintf (outputfile, "%da\n",
		       D_HIGHLINE (thisblock, mapping[FILE0]));
	      break;
	    case 1:
	      fprintf (outputfile, "%dc\n",
		       D_HIGHLINE (thisblock, mapping[FILE0]));
	      break;
	    default:
	      fprintf (outputfile, "%d,%dc\n",
		       D_LOWLINE (thisblock, mapping[FILE0]),
		       D_HIGHLINE (thisblock, mapping[FILE0]));
	      break;
	    }
	  for (i = 0;
	       i < D_NUMLINES (thisblock, mapping[FILE2]);
	       i++)
	    {
	      if (D_RELNUM (thisblock, mapping[FILE2], i)[0] == '.')
		{ leading_dot = 1; fprintf (outputfile, "."); }
	      fwrite (D_RELNUM (thisblock, mapping[FILE2], i), sizeof (char),
		      D_RELLEN (thisblock, mapping[FILE2], i), outputfile);
	    }
	  fprintf (outputfile, ".\n");
	  
	  /* Add in code to take care of leading dots, if necessary. */
	  if (leading_dot)
	    {
	      fprintf (outputfile, "%d,%ds/^\\.\\./\\./\n",
		       D_HIGHLINE (thisblock, mapping[FILE0]) + 1,
		       (D_HIGHLINE (thisblock, mapping[FILE0])
			+ D_NUMLINES (thisblock, mapping[FILE2])));
	      leading_dot = 0;
	    }
	}
    }
  if (finalwrite) fprintf (outputfile, "w\nq\n");
  return overlaps_found;
}

/*
 * Read from COMMONFILE and output to OUTPUTFILE a set of diff3_ blocks DIFF
 * as a merged file.  This acts like 'ed file0 <[output_diff3_edscript]',
 * except that it works even for binary data or incomplete lines.
 *
 * As before, MAPPING maps from arg list file number to diff file number,
 * REV_MAPPING is its inverse,
 * and FILE0, FILE1, and FILE2 are the names of the files.
 *
 * Returns 1 if overlaps were found.
 */

int
output_diff3_merge (commonfile, outputfile, diff, mapping, rev_mapping,
		    file0, file1, file2)
     FILE *commonfile, *outputfile;
     struct diff3_block *diff;
     int mapping[3], rev_mapping[3];
     char *file0, *file1, *file2;
{
  int c, i;
  int overlaps_found = 0;
  struct diff3_block *b;
  int linesread = 0;

  for (b = diff; b; b = b->next)
    {
      /* Must do mapping correctly */
      enum diff_type type
	= ((b->correspond == DIFF_ALL) ?
	   DIFF_ALL :
	   ((enum diff_type)
	    (((int) DIFF_1ST)
	     + rev_mapping[(int) b->correspond - (int) DIFF_1ST])));

      /* If we aren't supposed to do this output block, skip it.  */
      if (type == DIFF_2ND || type == DIFF_1ST
	  || (type == DIFF_3RD && dont_write_simple)
	  || (type == DIFF_ALL && dont_write_overlap))
	continue;

      /* Copy I lines from common file.  */
      i = D_LOWLINE (b, FILE0) - linesread - 1;
      linesread += i;
      while (0 <= --i)
	{
	  while ((c = getc(commonfile)) != '\n')
	    {
	      if (c == EOF)
		fatal ("input file shrank");
	      putc (c, outputfile);
	    }
	  putc (c, outputfile);
	}

      if (flagging && type == DIFF_ALL)
	/* Do special flagging.  */
	{
	  /* Put in lines from FILE0 with bracket.  */
	  fprintf (outputfile, "<<<<<<< %s\n", file0);
	  for (i = 0;
	       i < D_NUMLINES (b, mapping[FILE0]);
	       i++)
	    fwrite (D_RELNUM (b, mapping[FILE0], i), sizeof (char),
		    D_RELLEN (b, mapping[FILE0], i), outputfile);
	  fprintf (outputfile, "=======\n");
	  overlaps_found = 1;
	}

      /* Put in lines from FILE2.  */
      for (i = 0;
	   i < D_NUMLINES (b, mapping[FILE2]);
	   i++)
	fwrite (D_RELNUM (b, mapping[FILE2], i), sizeof (char),
		D_RELLEN (b, mapping[FILE2], i), outputfile);

      if (flagging && type == DIFF_ALL)
	fprintf (outputfile, ">>>>>>> %s\n", file2);

      /* Skip I lines in common file.  */
      i = D_NUMLINES (b, FILE0);
      linesread += i;
      while (0 <= --i)
	while ((c = getc(commonfile)) != '\n')
	  if (c == EOF)
	    {
	      if (i || b->next)
		fatal ("input file shrank");
	      return overlaps_found;
	    }
    }
  /* Copy rest of common file.  */
  while ((c = getc (commonfile)) != EOF)
    putc (c, outputfile);
  return overlaps_found;
}

/*
 * Reverse the order of the list of diff3 blocks.
 */
struct diff3_block *
reverse_diff3_blocklist (diff)
     struct diff3_block *diff;
{
  register struct diff3_block *tmp, *next, *prev;

  for (tmp = diff, prev = (struct diff3_block *) 0;
       tmp; tmp = next)
    {
      next = tmp->next;
      tmp->next = prev;
      prev = tmp;
    }
  
  return prev;
}

int
myread (fd, ptr, size)
     int fd, size;
     char *ptr;
{
  int result = read (fd, ptr, size);
  if (result < 0)
    perror_with_exit ("Read failed");
  return result;
}

VOID *
xmalloc (size)
     int size;
{
  VOID *result = (VOID *) malloc (size ? size : 1);
  if (!result)
    fatal ("Malloc failed");
  return result;
}

VOID *
xrealloc (ptr, size)
     VOID *ptr;
     int size;
{
  VOID *result = (VOID *) realloc (ptr, size ? size : 1);
  if (!result)
    fatal ("Malloc failed");
  return result;
}

fatal (string)
     char *string;
{
  fprintf (stderr, "%s: %s\n", argv0, string);
  exit (2);
}

perror_with_exit (string)
     char *string;
{
  perror (string);
  exit (2);
}
