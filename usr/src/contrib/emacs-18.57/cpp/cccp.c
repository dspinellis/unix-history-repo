/* C Compatible Compiler Preprocessor (CCCP)
Copyright (C) 1986, Free Software Foundation, Inc.
                    Written by Paul Rubin, June 1986

		       NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

		GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously
and appropriately publish on each copy a valid copyright notice
"Copyright (C) 1986 Free Software Foundation"; and include
following the copyright notice a verbatim copy of the above disclaimer
of warranty and of this License.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more extensive
    warranty protection to some or all third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

Mere aggregation of another unrelated program with this program (or its
derivative) on a volume of a storage or distribution medium does not bring
the other program under the scope of these terms.

  3. You may copy and distribute this program (or a portion or derivative
of it, under Paragraph 2) in object code or executable form under the terms
of Paragraphs 1 and 2 above provided that you also do one of the following:

    a) accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    b) accompany it with a written offer, valid for at least three
    years, to give any third party free (except for a nominal
    shipping charge) a complete machine-readable copy of the
    corresponding source code, to be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    c) accompany it with the information you received as to where the
    corresponding source code may be obtained.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form alone.)

For an executable file, complete source code means all the source code for
all modules it contains; but, as a special exception, it need not include
source code for modules which are standard libraries that accompany the
operating system on which the executable file runs.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

typedef unsigned char U_CHAR;

#ifdef EMACS
#define NO_SHORTNAMES
#include "../src/config.h"
#ifdef static
#undef static
#endif
#ifdef open
#undef open
#undef close
#undef read
#undef write
#endif /* open */
#endif /* EMACS */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <ctype.h>
#include <stdio.h>
#ifndef USG
#include <sys/time.h>		/* for __DATE__ and __TIME__ */
#else
#define index strchr
#define rindex strrchr
#include <time.h>
#include <fcntl.h>
#endif /* USG */

void bcopy (), bzero ();
int bcmp ();

char *xmalloc (), *xrealloc (), *xcalloc ();
void fatal (), pfatal_with_name (), perror_with_name ();

char *progname;

#define FATAL_EXIT_CODE 33	/* gnu cc command understands this */

struct directory_stack
  {
    struct directory_stack *next;
    char *fname;
  };

/* #include "file" starts with the first entry in the stack */
/* #include <file> starts with the second. */
/* -I directories are added after the first */
struct directory_stack default_includes[2] =
  {
    { &default_includes[1], "." },
    { 0, "/usr/include" }
  };
struct directory_stack *include = &default_includes[0];

int max_include_len = 14;	/* strlen (default_include) + 2
							(for / and null) */

char STDIN_FILE[] = "";		/* Empty, like real cpp */
int put_out_comments = 0;	/* JF non-zero means leave comments in the
				   output file.  Used by lint */

/* table to tell if char can be part of a C identifier. */
U_CHAR is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
U_CHAR is_idstart[256];
/* table to tell if c is horizontal space.  isspace() thinks that
   newline is space; this is not a good idea for this program. */
U_CHAR is_hor_space[256];

/* I/O buffer structure.  Ought to be used for the output file too.
   These are also used when there is no file present, for example,
   when rescanning a definition.  Then, the fname field is null. */
#define INPUT_STACK_MAX 100
struct file_buf {
  struct infile *next;	/* for making stacks of file ptrs */
  char *fname;
  int lineno;
  int length;
  U_CHAR *buf;
  U_CHAR *bufp;
} instack[INPUT_STACK_MAX];
int indepth = 0;

typedef struct file_buf FILE_BUF;

/* The output buffer.  Its LENGTH field is the amount of room allocated
   for the buffer, not the number of chars actually present.  To get
   that, subtract outbuf.buf from outbuf.bufp. */

#define OUTBUF_SIZE 10	/* initial size of output buffer */
FILE_BUF outbuf;

/* Structure allocated for every #define.  For a simple replacement
   such as
   	#define foo bar ,
   nargs = -1, the `pattern' list is null, and the expansion is just
   the replacement text.  Nargs = 0 means a real macro with no args,
   e.g.,
       #define getchar() getc(stdin) .
   When there are args, the expansion is the replacement text with the
   args squashed out, and the reflist is a list describing how to
   build the output from the input: e.g., "3 chars, then the 1st arg,
   then 9 chars, then the 3rd arg, then 0 chars, then the 2nd arg".
   The chars here come from the expansion.  Thus, for any definition
   d , strlen(d->expansion) should equal the sum of all the
   d->pattern->nchars.  Note that the list can be arbitrarily long---
   its length depends on the number of times the arguements appear in
   the replacement text, not how many args there are.  Example:
   #define f(x) x+x+x+x+x+x+x would have replacement text "++++++" and
   pattern list
     { (0, 1), (1, 1), (1, 1), ..., (1, 1), NULL }
   where (x, y) means (nchars, argno). */

typedef struct definition DEFINITION;
struct definition {
  int nargs;
  int length;			/* length of expansion string */
  U_CHAR *expansion;
  struct reflist {
    struct reflist *next;
    int nchars;
    int argno;
  } *pattern;
};

/* different kinds of things that can appear in the value field
   of a hash node.  Actually, this may be useless now. */
union hashval {
  int ival;
  char *cpval;
  DEFINITION *defn;
};


/* The structure of a node in the hash table.  The hash table
   has entries for all tokens defined by #define commands (type T_MACRO),
   plus some special tokens like __LINE__ (these each have their own
   type, and the appropriate code is run when that type of node is seen.
   It does not contain control words like "#define", which are recognized
   by a separate piece of code. */
typedef struct hashnode HASHNODE;
struct hashnode {
  HASHNODE *next;		/* double links for easy deletion */
  HASHNODE *prev;
  HASHNODE **bucket_hdr;	/* also, a back pointer to this node's hash
				   chain is kept, in case the node is the head
				   of the chain and gets deleted. */
  int type;			/* type of special token */
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  union hashval value;		/* pointer to expansion, or whatever */
};


HASHNODE *install();
/* different flavors of hash nodes --- also used in keyword table */
#define T_DEFINE	1	/* the "#define" keyword */
#define T_INCLUDE	2	/* the "#include" keyword */
#define T_IFDEF		3	/* the "#ifdef" keyword */
#define T_IF		4	/* the "#if" keyword */
#define T_EXPAND	5	/* argument to be expanded (now unused) */
#define T_MACRO		6	/* macro defined by "#define" */
#define T_ELSE		7	/* "#else" */
#define T_PRAGMA	8	/* "#pragma" */
#define T_ELIF		9	/* "#else" */
#define T_UNDEF		10	/* "#undef" */
#define T_LINE		11	/* "#line" */
#define T_ERROR		12	/* "#error" */
#define T_IFNDEF	13	/* "#ifndef"; forgot this earlier */
#define T_ENDIF		14	/* "#endif" */
#define T_SPECLINE	15	/* special symbol "__LINE__" */
#define T_DATE		16	/* "__DATE__" */
#define T_FILE		17	/* "__FILE__" */
#define T_TIME		18	/* "__TIME__" */

#define T_SPEC_DEFINED	19	/* special macro for use in #if statements */



/* some more different types will be needed --- no longer bloody likely */


int do_define(), do_line(), do_include(), do_undef(), do_error(),
  do_pragma(), do_if(), do_xifdef(), do_else(),
  do_elif(), do_endif();


/* table of control words, along with code to execute when the keyword
   is seen.  For now, it is searched linearly, so put the most frequently
   found keywords at the beginning of the list. */

struct keyword_table {
  int length;
  int (*func)();
  char *name;
  int type;
} keyword_table[] = {
  {  6, do_define, "define", T_DEFINE},
  {  4, do_line, "line", T_LINE},
  {  7, do_include, "include", T_INCLUDE},
  {  5, do_undef, "undef", T_UNDEF},
  {  5, do_error, "error", T_ERROR},
  {  2, do_if, "if", T_IF},
  {  5, do_xifdef, "ifdef", T_IFDEF},
  {  6, do_xifdef, "ifndef", T_IFNDEF},
  {  4, do_else, "else", T_ELSE},
  {  4, do_elif, "elif", T_ELIF},
  {  5, do_endif, "endif", T_ENDIF},
  {  6, do_pragma, "pragma", T_PRAGMA},
  {  -1, 0, "", -1},
};

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf() below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf() function.  Hashf() only exists for the sake of
   politeness, for use when speed isn't so important. */

#define HASHSIZE 1009
HASHNODE *hashtab[HASHSIZE];
#define HASHSTEP(old, c) ((old << 1) + c)
#define MAKE_POS(v) (v & ~0x80000000) /* make number positive */

#define SKIP_WHITE_SPACE(p) { while (is_hor_space[*p]) p++; }



main (argc, argv)
     int argc;
     char **argv;
{
  struct stat sbuf;
  char *in_fname, *out_fname;
  int out_fd = 1;	/* default to stdout */
  int f, i;
  FILE_BUF *fp;

  progname = argv[0];
  in_fname = NULL;
  out_fname = NULL;
  initialize_random_junk ();

  fp = &instack[indepth++];

/*  if (argc < 2)		JF no args means work as filter
    return FATAL_EXIT_CODE; */

  for (i = 1; i < argc; i++) {
    if (argv[i][0] != '-') {
      if (out_fname != NULL)
	fatal ("Usage: %s [switches] input output\n", argv[0]);
      else if (in_fname != NULL) {
	out_fname = argv[i];
	if ((out_fd = creat (out_fname, 0666)) < 0)
	  pfatal_with_name (out_fname);
      } else
	in_fname = argv[i];
    } else {
      switch (argv[i][1]) {
	U_CHAR *p;
	struct directory_stack *dirtmp;
      case 'D':
	if ((p = (U_CHAR *) index(argv[i]+2, '=')) != NULL)
	  *p = ' ';
	make_definition (argv[i] + 2);
	break;
      case 'U':		/* JF #undef something */
	make_undef(argv[i] + 2);
	break;
      case 'C':		/* JF do later -C means leave comments alone! */
	put_out_comments++;
	break;
      case 'E':			/* -E comes from cc -E; ignore it.  */
	break;
      case 'M':			/* Makefile dependencies or something like
				   that.  Not implimented yet */
	break;
      case 'I':			/* JF handle directory path right */
        dirtmp = (struct directory_stack *)
			xmalloc (sizeof (struct directory_stack));
	dirtmp->next = include->next;
	include->next = dirtmp;
	dirtmp->fname = argv[i]+2;
	include = dirtmp;
	if (strlen (argv[i]) > max_include_len)
	  max_include_len = strlen (argv[i]);
	break;

      case '\0': /* JF handle '-' as file name meaning stdin or stdout */
	if (in_fname == NULL) {
	  in_fname = STDIN_FILE;
	  break;
	} else if (out_fname == NULL) {
	  out_fname = "stdout";
	  break;
	}	/* else fall through into error */

      default:
	fatal ("Illegal option %s\n", argv[i]);
      }
    }
  }

  /* JF check for stdin */
  if (in_fname == STDIN_FILE || in_fname == NULL)
    f = 0;
  else if ((f = open (in_fname, O_RDONLY)) < 0)
    goto perror;

  fstat (f, &sbuf);
  fp->fname = in_fname;
  fp->lineno = 1;
  /* JF all this is mine about reading pipes and ttys */
  if ((sbuf.st_mode & S_IFMT) != S_IFREG) {
    int size;
    int bsize;
    int cnt;
    U_CHAR *bufp;

    bsize = 2000;
    size = 0;
    fp->buf = (U_CHAR *) xmalloc (bsize + 1);
    bufp = fp->buf;
    for (;;) {
      cnt = read (f, bufp, bsize - size);
      if (cnt < 0) goto perror;	/* error! */
      if (cnt == 0) break;	/* End of file */
      size += cnt;
      bufp += cnt;
      if (bsize-size == 0) {	/* Buffer is full! */
        bsize *= 2;
        fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 1);
	bufp = fp->buf + size;	/* May have moved */
      }
    }
    fp->buf[size] = '\0';
    fp->length = size;
  } else {
    fp->length = sbuf.st_size;
    fp->buf = (U_CHAR *) alloca (sbuf.st_size + 1);

    if (read (f, fp->buf, sbuf.st_size) != sbuf.st_size)
      goto perror;

    fp->buf[sbuf.st_size] = '\0';
  }

  /* initialize output buffer */
  outbuf.buf = (U_CHAR *) xmalloc (OUTBUF_SIZE);
  outbuf.bufp = outbuf.buf;
  outbuf.length = OUTBUF_SIZE;

  output_line_command (fp, &outbuf);
  rescan (fp, &outbuf);

  /* do something different than this later */
  fflush (stdout);
  write (out_fd, outbuf.buf, outbuf.bufp - outbuf.buf);
  exit (0);

 perror:
  pfatal_with_name (argv[1]);
}

/*
 * The main loop of the program.  Try to examine and move past most
 * ordinary input chars as fast as possible.  Call appropriate routines
 * when something special (such as a macro expansion) has to happen.

IP is the source of input to scan.
OP is the place to put input. */

rescan (ip, op)
     FILE_BUF *ip, *op;
{
  register int c;
  register int ident_length = 0, hash = 0;
  register U_CHAR *limit;
  U_CHAR *check_expand ();
  struct keyword_table *handle_directive ();
  int excess_newlines = 0;
  int escaped = 0;
  
  U_CHAR *bp;
  
  check_expand(op, ip->length);
  
  ip->bufp = ip->buf;
  limit = ip->buf + ip->length;
  while (1) {
    if (ip->bufp < limit) {
      c = *ip->bufp++;
      *op->bufp++ = c;
    } else {
      c = -1;
    }

    --escaped;
    /* Now ESCAPED is 0 if and only if this character is escaped.  */

    switch (c) {
    case '\\':
      if (escaped == 0)
	goto randomchar;
      if (*ip->bufp != '\n')
	{
	  escaped = 1;
	  goto randomchar;
	}
      /* always merge lines ending with backslash-newline */
      ++ip->bufp;
      ++ip->lineno;
      ++excess_newlines;
      --op->bufp;		/* remove backslash from obuf */
      continue;			/* back to top of while loop */

    case '#':
      /* # keyword: a # must be first nonblank char on the line */
      for (bp = ip->bufp - 1; bp >= ip->buf; bp--)
	if (*bp == '\n')
	  break;
      bp++;			/* skip nl or move back into buffer */
      SKIP_WHITE_SPACE (bp);
      if (*bp != '#')
	goto randomchar;
      ident_length = hash = 0;
      --op->bufp;		/* don't copy the '#' */

      if (handle_directive (ip, op, &excess_newlines) == NULL) {
	++op->bufp;		/* copy the '#' after all */
	goto randomchar;
      }
      break;

    case '\"':			/* skip quoted string */
    case '\'':
      /* a single quoted string is treated like a double -- some
	 programs (e.g., troff) are perverse this way */

      if (escaped == 0)
	goto randomchar;	/* false alarm-- it's escaped. */

      /* skip ahead to a matching quote.  */

      bp = ip->bufp;
      while (bp < limit) {
	*op->bufp++ = *bp;
	switch (*bp++) {
	case '\n':
	  ++ip->lineno;
	  break;
	case '\\':
	  if (bp >= limit)
	    break;
	  if (*bp == '\n')
	    {
	      /* backslash newline is replaced by nothing at all,
		 but remember that the source line count is out of synch.  */
	      --op->bufp;
	      ++bp;
	      ++excess_newlines;
	      ++ip->lineno;
	    }
	  else
	    *op->bufp++ = *bp++;
	  break;
	case '\"':
	case '\'':
	  if (bp[-1] == c)
	    goto while2end;
	  break;
	}
      }
    while2end:
      ip->bufp = bp;
      break;

    case '/':			/* possible comment */
      if (*ip->bufp != '*')
	goto randomchar;
      if (put_out_comments) {
        bp = ip->bufp;
	*op->bufp++ = *bp++;
      } else {
	bp = ip->bufp + 1;
	--op->bufp;		/* don't leave initial slash in buffer */
      }

      for (;;) {
	while (bp < limit) {
	  if (put_out_comments)
	    *op->bufp++ = *bp;
	  switch (*bp++) {
	  case '*':
	    goto whileend;
	  case '\n':
	    /* copy the newline into the output buffer, in order to
	       avoid the pain of a #line every time a multiline comment
	       is seen.  This means keywords with embedded comments
	       that contain newlines (blucch!) will lose.  By making
	       sure that excess_newlines is not just a flag, but really
	       an accurate count, it might be possible to get around this. */
	    if (!put_out_comments)
	      *op->bufp++ = '\n';
	    ++ip->lineno;
	  }
	}
      whileend:
	if (bp >= limit) {
	  error ("unterminated comment");
	  break;		/* causes eof condition */
	}
	if (*bp == '/')
	  break;
      }
      if (put_out_comments)
        *op->bufp++ = '/';
      ip->bufp = bp + 1;
      break;
      
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      /* if digit is not part of identifier, it is random */
      if (ident_length == 0)
	goto randomchar;
      /* fall through */
      
    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': 
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z': 
      ident_length++;
      /* compute step of hash function, to avoid a proc call on every token */
      hash = HASHSTEP(hash, c);
      break;

    default:
randomchar:
      if (ident_length > 0) {
	register HASHNODE *hp;
	for (hp = hashtab[MAKE_POS(hash) % HASHSIZE]; hp != NULL;
	     hp = hp->next) {
          U_CHAR *save_ibufp;	/* kludge, see below */
	      
          if (hp->length == ident_length) {
	    register int i = ident_length;
	    register U_CHAR *p = hp->name;
	    register U_CHAR *q = op->bufp - i;

	    if (c != (U_CHAR) -1)
	      q--;

	    do {		/* all this to avoid a strncmp() */
	      if (*p++ != *q++)
		goto hashcollision;
	    } while (--i);
	 
	    save_ibufp = ip->bufp;
	    /* back up over identifier, then expand token */
	    op->bufp -= ident_length;
	    if (c != (U_CHAR) -1) op->bufp--;
	    macroexpand (hp, ip, op, &excess_newlines);

	    check_expand(op, ip->length - (ip->bufp - ip->buf));
	    
	    /* If we just processed an identifier at end of input,
	       return right away.  */
	    if (c == (U_CHAR) -1)
	      return;

	    /* if the expansion routine has not moved the input
	       pointer, put back the char that ended the token.
	       This is a kludge because there might be a different
	       reason to put it back or not put it back. */
	    if (ip->bufp == save_ibufp)
	      *op->bufp++ = c;
	    
	    break;		/* out of for loop */
	  }
hashcollision:
	       ;
	}			/* end for loop */
	ident_length = hash = 0; /* stop collecting identifier */
      }
	    
      /* If we just processed an identifier at end of input,
	 return right away.  */
      if (c == -1)
	return;

      /* count the newline, if it was one.  The reason this is
	 done down here instead of as a case in the switch is
	 that some expansions might want to look at the line
	 number, and if they happen right before the newline,
	 we don't want them to get the wrong one.  So the newline
	 must be counted AFTER any expansions happen. */
      if (c == '\n') {
	++ip->lineno;
	if (excess_newlines > 0) {
	  output_line_command (ip, op);
	  check_expand(op, ip->length - (ip->bufp - ip->buf));

	  excess_newlines = 0;
	}
      }
      break;			/* from switch */
    }
  }
}

/*
 * Process a # directive.  Expects ip->bufp to point to the '#', as in
 * "#define foo bar".  Bumps *excess_newlines counter as necessary if
 * the command is several lines long (and also updates ip->lineno).
 * The main reason for this is that the comments could contain
 * newlines, which would be confusing.  Passes to the command handler
 * (do_define, do_include, etc.): the addresses of the 1st and
 * last chars of the command (starting immediately after the #
 * keyword), plus op and the keyword table pointer.  If the line
 * contains comments the command is copied into a temporary buffer
 * (sans comments) and the temporary buffer is passed to the command
 * handler instead.
 */

struct keyword_table *
handle_directive (ip, op, excess_newlines)
     FILE_BUF *ip, *op;
     int *excess_newlines;
{
  register U_CHAR *bp, *cp;
  register struct keyword_table *kt;
  register int ident_length;

  /* Nonzero means we must copy the entire command
     to get rid of comments or backslash-newlines.  */
  int copy_command = 0;

  bp = ip->bufp;
  SKIP_WHITE_SPACE(bp);
  cp = bp;
  while (is_idchar[*cp])
    cp++;
  ident_length = cp - bp;
  
  /*
   * Decode the keyword and call the appropriate expansion
   * routine, after moving the input pointer up to the next line.
   * If the keyword is not a legitimate control word, return NULL.
   * Otherwise, return ptr to the keyword structure matched.
   */
  for (kt = keyword_table; kt->length > 0; kt++) {
    if (kt->length == ident_length && !strncmp(kt->name, bp, ident_length)) {
      register U_CHAR *buf;
      register U_CHAR *limit = ip->buf + ip->length;
      U_CHAR *skip_to_end_of_comment();
      
      buf = bp = bp + ident_length;
      while (bp < limit) {
	if (*bp == '\'' || *bp == '\"') {		/* JF handle quotes right  */
	  U_CHAR quotec;

	  for (quotec = *bp++; bp < limit && *bp != quotec; bp++) {
	    if (*bp == '\\') bp++;
	    if (*bp == '\n') {
	      if (bp[-1] == '\\')
		copy_command++;
	      else {
		/* --bp; */
		break;	/* JF ugly, but might work */
	      }
	    }
	  }
	  continue;
	}
	if (*bp == '/' && bp[1] == '*') {
	  copy_command++;
	  ip->bufp = bp + 2;
	  skip_to_end_of_comment (ip, NULL);
	  bp = ip->bufp;
	  continue;
	}

	if (*bp++ == '\n') {
	  if (*(bp-2) == '\\')
	    copy_command++;
	  else {
	    --bp;		/* point to the newline */
	    break;
	  }
	}
      }
      if (copy_command) {
	/* need to copy entire command into temp buffer before dispatching */

	cp = (U_CHAR *) alloca (bp - buf + 5); /* room for cmd plus
						  some slop */
	bp = buf;
	buf = cp;
	
	while (bp < limit) {
	  if (*bp == '\'' || *bp == '\"') {	/* JF handle quotes right  */
	    U_CHAR quotec;

	    *cp++ = *bp;
	    for (quotec = *bp++; bp < limit && *bp != quotec; *cp++ = *bp++) {
	      if (*bp == '\\')
		*cp++ = *bp++;
	      if (*bp == '\n') {
		if (bp[-1] == '\\') {
		  ++ip->lineno;
		  ++*excess_newlines;
		} else break;	/* JF ugly, but might work */
	      }
	    }
	    continue;
	  }
	  if (*bp == '/' && bp[1] == '*') {
	    int newlines_found = 0;
	    ip->bufp = bp + 2;
	    skip_to_end_of_comment (ip, &newlines_found);
	    *excess_newlines += newlines_found;
	    ip->lineno += newlines_found;
	    bp = ip->bufp;
	    continue;
	  }

	  if (*bp == '\n') {
	    if (bp[-1] == '\\') {
	      ++ip->lineno;
	      ++*excess_newlines;
	    } else
	      break;
	  }
	  *cp++ = *bp++;
	}
      }
      else
	cp = bp;

      ip->bufp = bp;		/* skip to the end of the command */

      /* call the appropriate command handler.  Buf now points to
	 either the appropriate place in the input buffer, or to
	 the temp buffer if it was necessary to make one.  Cp
	 points to the first char after the contents of the (possibly
	 copied) command, in either case. */
      (*kt->func) (buf, cp, op, kt);
      check_expand (op, ip->length - (ip->bufp - ip->buf));

      break;
    }
  }
  if (kt->length <= 0)
    kt = NULL;

  return kt;
}

static char *monthnames[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
			     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
			    };

/*
 * expand things like __FILE__.  Place the expansion into the output
 * buffer *without* rescanning.
 */
expand_special_symbol (hp, ip, op)
     HASHNODE *hp;
     FILE_BUF *ip, *op;
{
  char *buf;
  int i, len;
  FILE_BUF *last_ip = NULL;
  static struct tm *timebuf = NULL;
  struct tm *localtime();

  int paren = 0;		/* for special `defined' keyword */
  HASHNODE *lookup();

  for (i = indepth - 1; i >= 0; i--)
    if (instack[i].fname != NULL) {
      last_ip = &instack[i];
      break;
    }
  if (last_ip == NULL) {
    error("CCCP error: not in any file?!");
    return;			/* the show must go on */
  }

  switch (hp->type) {
  case T_FILE:
    buf = (char *) alloca (3 + strlen(last_ip->fname));
    sprintf (buf, "\"%s\"", last_ip->fname);
    break;
  case T_SPECLINE:
    buf = (char *) alloca (10);
    sprintf (buf, "%d", last_ip->lineno);
    break;
  case T_DATE:
  case T_TIME:
    if (timebuf == NULL) {
      i = time(0);
      timebuf = localtime(&i);
    }
    buf = (char *) alloca (20);
    if (hp->type == T_DATE)
      sprintf(buf, "\"%s %2d %4d\"", monthnames[timebuf->tm_mon - 1],
	      timebuf->tm_mday, timebuf->tm_year + 1900);
    else
      sprintf(buf, "\"%02d:%02d:%02d\"", timebuf->tm_hour, timebuf->tm_min,
	      timebuf->tm_sec);
    break;
  case T_SPEC_DEFINED:
    buf = " 0 ";		/* assume symbol is not defined */
    if (is_hor_space[*(ip->bufp-1)]) {
      SKIP_WHITE_SPACE(ip->bufp);
      if (*ip->bufp == '(') {
	paren++;
	ip->bufp++;			/* skip over the paren */
      }
    } else if (*(ip->bufp-1) == '(')
      paren++;

    if (!is_idstart[*ip->bufp])
      goto oops;
    if (lookup(ip->bufp))
      buf = " 1 ";
    while (is_idchar[*ip->bufp])
      ++ip->bufp;
    SKIP_WHITE_SPACE (ip->bufp);
    if (paren) {
      if (*ip->bufp != ')')
	goto oops;
      ++ip->bufp;
    }
    break;
    
oops:
    
    error ("`defined' must be followed by IDENT or (IDENT)");
    break;
      
  default:
    error("CCCP error: illegal special hash type"); /* time for gdb */
    abort ();
  }
  len = strlen(buf);
  check_expand(op, len);
  bcopy (buf, op->bufp, len);
  op->bufp += len;
  
  return;
}


/* routines to handle #directives */

/*
 * process include file by reading it in and calling rescan.
 * expects to see "fname" or <fname> on the input.
 * add error checking and -I option later.
 */

do_include (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  char *fname;		/* dynamically allocated fname buffer */
  U_CHAR *fbeg, *fend;		/* beginning and end of fname */
  U_CHAR term;			/* terminator for fname */
  int err = 0;			/* some error has happened */
  struct stat sbuf;		/* to stat the include file */
  FILE_BUF *fp;	/* for input stack frame */
  struct directory_stack *stackp;
  int flen;

  int save_indepth = indepth;
				/* in case of errors */

  int f;			/* file number */
  char *other_dir;		/* JF */

  f= -1;	/* JF we iz PARANOID! */
  fbeg = buf;
  SKIP_WHITE_SPACE(fbeg);

  switch (*fbeg++) {
  case '\"':
    term = '\"';
    stackp = include;
    break;
  case '<':
    term = '>';
    stackp = include->next;
    break;
  default:
    error ("#include expects \"fname\" or <fname>");
    fbeg--;			/* so person can see whole fname */
    err++;
    term = '\n';
    break;
  }
  for (fend = fbeg; *fend != term; fend++)
    {
      if (fend >= limit)
	{
	  error ("illegal or unterminated include file name");
	  goto nope;
	}
    }

  flen = fend - fbeg;
  if (err)
    goto nope;

  other_dir = NULL;
  if (stackp == include)
    {
      fp = &instack[indepth];
      while(--fp >= &instack[0])
	{
	  int n;
	  char *ep,*nam;
	  extern char *rindex ();

	  if ((nam = fp->fname) != NULL)
	    {
	      if ((ep = rindex (nam, '/')) != NULL)
		{
		  n = ep - nam;
		  other_dir = (char *) alloca (n + 1);
		  strncpy (other_dir, nam, n);
		  other_dir[n] = '\0';
		}
	      break;
	    }
	}
    }
  			/* JF search directory path */
  fname = (char *) alloca (max_include_len + flen);
  for (; stackp; stackp = stackp->next)
    {
      if (other_dir)
	{
	  strcpy (fname, other_dir);
	  other_dir = 0;
	}
      else
	strcpy (fname, stackp->fname);
      strcat (fname, "/");
      strncat (fname, fbeg, flen);
      if ((f = open (fname, O_RDONLY)) >= 0)
	break;
    }
  if (f < 0)
    {
      err++;
      goto nope;
    }

  if (fstat(f, &sbuf) < 0)
    {
      perror_with_name (fname);
      goto nope;		/* impossible? */
    }

  fp = &instack[indepth++];
  fp->buf = (U_CHAR *) alloca (sbuf.st_size + 1);
  fp->fname = fname;
  fp->length = sbuf.st_size;
  fp->lineno = 1;

  if (read(f, fp->buf, sbuf.st_size) != sbuf.st_size)
    goto nope;

  fp->buf[sbuf.st_size] = '\0';

  output_line_command (fp, op);
  rescan(fp, op);

nope:

  if (f > 0)
    close (f);
  indepth = save_indepth;
  output_line_command (&instack[indepth-1], op);
  if (err) {
    strncpy (fname, fbeg, flen);
    fname[flen] = '\0';
    perror_with_name (fname);
  }
  return err;
}

/* the arglist structure is built by do_define to tell
   collect_definition where the argument names begin.  That
   is, for a define like "#define f(x,y,z) foo+x-bar*y", the arglist
   would contain pointers to the strings x, y, and z.
   Collect_definition would then build a DEFINITION node,
   with reflist nodes pointing to the places x, y, and z had
   appeared.  So the arglist is just convenience data passed
   between these two routines.  It is not kept around after
   the current #define has been processed and entered into the
   hash table. */

struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

/* Process a #define command.
BUF points to the contents of the #define command, as a continguous string.
LIMIT points to the first character past the end of the definition.
KEYWORD is the keyword-table entry for #define.  */

do_define (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  U_CHAR *bp;			/* temp ptr into input buffer */
  U_CHAR *symname;		/* remember where symbol name starts */
  int sym_length;		/* and how long it is */
  U_CHAR *def;			/* beginning of expansion */

  DEFINITION *defn, *collect_expansion();

  bp = buf;

  while (is_hor_space[*bp])
    bp++;
  if (!is_idstart[*bp]) {
    error("illegal macro name: must start with an alphabetic or '_'");
    goto nope;
  }
  symname = bp;			/* remember where it starts */
  while (is_idchar[*bp] && bp < limit)
    bp++;
  sym_length = bp - symname;

  /* lossage will occur if identifiers or control keywords are broken
     across lines using backslash.  This is not the right place to take
     care of that. */

  if (is_hor_space[*bp] || *bp == '\n' || bp >= limit) {
    /* simple expansion or empty definition; gobble it */
    if (is_hor_space[*bp])
      def = ++bp;		/* skip exactly one blank/tab char */
    else
      def = bp;			/* empty definition */

    defn = (DEFINITION *) xmalloc (sizeof (DEFINITION) + limit - def);
    defn->nargs = -1;
    defn->pattern = NULL;
    defn->expansion = ((U_CHAR *) defn) + sizeof (DEFINITION);
    defn->length = limit - def;
    if (defn->length > 0)
      bcopy (def, defn->expansion, defn->length);
  }
  else if (*bp == '(') {
    struct arglist *arg_ptrs = NULL;
    int argno = 0;

    bp++;			/* skip '(' */
    SKIP_WHITE_SPACE(bp);

    while (*bp != ')') {
      struct arglist *temp;

      temp = (struct arglist *) alloca (sizeof (struct arglist));
      temp->name = bp;
      temp->next = arg_ptrs;
      temp->argno = ++argno;
      arg_ptrs = temp;
      while (is_idchar[*bp])
	bp++;
      temp->length = bp - temp->name;
      SKIP_WHITE_SPACE (bp);	/* there should not be spaces here,
				   but let it slide if there are. */
      if (temp->length == 0 || (*bp != ',' && *bp != ')')) {
	error ("illegal parameter to macro");
	goto nope;
      }
      if (*bp == ',') {
	bp++;
	SKIP_WHITE_SPACE(bp);
      }
      if (bp >= limit) {
	error ("unterminated format parameter list in #define");
	goto nope;
      }
    }

    ++bp;			/* skip paren */
    /* Skip exactly one space or tab if any.  */
    if (bp < limit && (*bp == ' ' || *bp == '\t')) ++bp;
      
    /* now everything from bp before limit is the definition. */
    defn = collect_expansion(bp, limit - bp, arg_ptrs);
  } else {
    error("#define symbol name not followed by SPC, TAB, or '('");
    goto nope;
  }

  {
    HASHNODE *hp, *lookup();
    DEFINITION *old_def;
    if ((hp = lookup(symname)) != NULL) {
      old_def = hp->value.defn;
      if (compare_defs(defn, old_def)) {
	U_CHAR *msg;			/* what pain... */
	msg = (U_CHAR *) alloca (sym_length + 20);
	bcopy (symname, msg, sym_length);
	strcpy (msg + sym_length, " redefined");
	error (msg);
	/* flush the most recent old definition */
	delete (hp);
      }
    }
  }
  
  install (symname, T_MACRO, defn);
  return 0;
  
nope:

  return 1;
}

/*
 * return zero if two DEFINITIONs are isomorphic
 */
static
compare_defs(d1, d2)
     DEFINITION *d1, *d2;
{
  struct reflist *a1, *a2;

  if (d1->nargs != d2->nargs || d1->length != d2->length)
    return 1;
  if (strncmp(d1->expansion, d2->expansion, d1->length) != 0)
    return 1;
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next)
    if (a1->nchars != a2->nchars || a1->argno != a2->argno)
      return 1;
   return 0;
}

/* Read a macro definition for a macro with parameters.
   Build the DEFINITION structure.
   Reads SIZE characters of text starting at BUF.
   ARGLIST specifies the formal parameters to look for
   in the text of the definition.  */

static DEFINITION *
collect_expansion(buf, size, arglist)
     U_CHAR *buf;
     int size;
     struct arglist *arglist;
{
  DEFINITION *defn;
  U_CHAR *p, *lastp, *exp_p;
  int id_len;
  struct arglist *arg;
  struct reflist *endpat = NULL;

  /* scan thru the macro definition, ignoring comments and quoted
   strings, picking up on the macro calls.  It does a linear search
   thru the arg list on every potential symbol.  Profiling might say
   that something smarter should happen. */


  if (size < 0)
    abort ();

  defn = (DEFINITION *) xcalloc (1, sizeof (DEFINITION));

  /* watch out!  the arg count here depends on the order in which
     arglist was built.  you might have to count the args if
     you change something. */
  if (arglist != NULL)
    defn->nargs = arglist->argno;
  else
    defn->nargs = 0;
  exp_p = defn->expansion = (U_CHAR *) xmalloc (size + 1);

  /* write comment and quote handling
     and speed this loop up later; this is a stripped version */

  /* On the other hand, is it really worth doing that here?
     comments will get taken care of on rescan.  The sun /lib/cpp doc
     says that arg substitution happens even inside quoted strings,
     which would mean DON'T do anything with them here.  Check the
     standard on this. */

  lastp = p = buf;
  while (p < buf+size) {
    int skipped_arg = 0;

    if (is_idstart[*p] && (p==buf || !is_idchar[*(p-1)])) {

      for (id_len = 0; is_idchar[p[id_len]]; id_len++)
	;
      for (arg = arglist; arg != NULL; arg = arg->next) {
	struct reflist *tpat;

	if (arg->length == id_len && strncmp(arg->name, p, id_len) == 0) {
	  /* make a pat node for this arg and append it to the end of
	     the pat list */
	  tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	  tpat->next = NULL;
	  if (endpat == NULL)
	    defn->pattern = tpat;
	  else
	    endpat->next = tpat;
	  endpat = tpat;

	  tpat->argno = arg->argno;
	  tpat->nchars = p - lastp;
	  p += id_len;
	  lastp = p;		/* place to start copying from next time */
	  skipped_arg++;
	  break;
	}
      }
    }

    if (skipped_arg == 0)
      *exp_p++ = *p++;
  }

  *exp_p++ = '\0';

  defn->length = exp_p - defn->expansion - 1;
  
  /* give back excess storage */
  defn->expansion = (U_CHAR *) xrealloc (defn->expansion, defn->length + 1);

  return defn;
}

#ifdef DEBUG
/*
 * debugging routine ---- return a ptr to a string containing
 *   first n chars of s.  Returns a ptr to a static object
 *   since I happen to know it will fit.
 */
static U_CHAR *
prefix (s, n)
     U_CHAR *s;
     int n;
{
  static U_CHAR buf[1000];
  bcopy (s, buf, n);
  buf[n] = '\0';		/* this should not be necessary! */
  return buf;
}
#endif

/*
 * interpret #line command.  Remembers previously seen fnames
 * in its very own hash table.
 */
#define FNAME_HASHSIZE 37

do_line(buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  register U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth - 1];

  bp = buf;
  ip->lineno = atoi(bp);
  /* this time, skip to the end of the line WITHOUT
     bumping lineno.  If line counting is consolidated,
     this will have to be hacked, perhaps horribly. */

  /* skip over blanks, optional sign, digits, blanks. */
  SKIP_WHITE_SPACE (bp);
  if (*bp == '-' || *bp == '+')
    bp++;
  while (isdigit(*bp))
    bp++;
  SKIP_WHITE_SPACE (bp);

  if (*bp != '\n') {		/* if eol, then don't hack fname */
    static HASHNODE *fname_table[FNAME_HASHSIZE];
    HASHNODE *hp, **hash_bucket;
    U_CHAR *fname;
    int fname_length;

    if (*bp != '"') {
      error ("#line directive must be #line NNN [\"fname\"]");
      goto done;
    }
    fname = ++bp;

    while (*bp != '"' && bp < limit)
      bp++;
    if (*bp != '"') {
      error ("Unterminated fname in #line command");
      goto done;
    }
    fname_length = bp - fname;
    hash_bucket =
      &fname_table[hashf(fname, fname_length, FNAME_HASHSIZE)];
    for (hp = *hash_bucket; hp != NULL; hp = hp->next)
      if (hp->length == fname_length &&
	  strncmp(hp->value.cpval, fname, fname_length) == 0) {
	ip->fname = hp->value.cpval;
	goto done;
      }
    /* didn't find it, cons up a new one */
    hp = (HASHNODE *) xcalloc (1, sizeof (HASHNODE) + fname_length + 1);
    hp->next = *hash_bucket;
    *hash_bucket = hp;

    hp->length = fname_length;
    ip->fname = hp->value.cpval = ((char *) hp) + sizeof (HASHNODE);
    bcopy (fname, hp->value.cpval, fname_length);
  }

done:

  output_line_command (ip, op);
  check_expand (op, ip->length - (ip->bufp - ip->buf));
}

/*
 * remove all definitions of symbol from symbol table.
 * according to un*x /lib/cpp, it is not an error to undef
 * something that has no definitions, so it isn't one here either.
 */
do_undef(buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  register U_CHAR *bp;
  HASHNODE *hp, *lookup();

  SKIP_WHITE_SPACE (buf);

  while ((hp = lookup(buf)) != NULL)
    delete (hp);
}

/* handle #error command later */ 
do_error()
{
}

/*
 * the behavior of the #pragma directive is implementation defined.
 * this implementation defines it as follows.
 */
do_pragma()
{
  close (0);
  if (open ("/dev/tty", O_RDONLY) != 0)
    goto nope;
  close (1);
  if (open("/dev/tty", O_WRONLY) != 1)
    goto nope;
  execl("/usr/games/rogue", "#pragma", 0);
  execl("/usr/games/hack", "#pragma", 0);
  execl("/usr/new/emacs -f hanoi 9 -kill", "#pragma", 0);
nope:
  fatal ("You are in a maze of twisty compiler features, all different");
}

typedef struct if_stack {
  struct if_stack *next;	/* for chaining to the next stack frame */
  char *fname;		/* copied from input when frame is made */
  int lineno;			/* similarly */
  int if_succeeded;		/* true if a leg of this if-group
				    has been passed through rescan */
  int type;			/* type of last directive seen in this group */
};
typedef struct if_stack IF_STACK_FRAME ;
IF_STACK_FRAME *if_stack = NULL;

/*
 * handle #if command by
 *   1) inserting special `defined' keyword into the hash table
 *	that gets turned into 0 or 1 by expand_special_symbol (thus,
 *	if the luser has a symbol called `defined' already, it won't
 *      work inside the #if command)
 *   2) rescan the input into a temporary output buffer
 *   3) pass the output buffer to the yacc parser and collect a value
 *   4) clean up the mess left from steps 1 and 2.
 *   5) call conditional_skip to skip til the next #endif (etc.),
 *      or not, depending on the value from step 3.
 */
do_if (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  int value;
  FILE_BUF *ip = &instack[indepth - 1];

  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_IF);
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */

do_elif (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  int value;
  FILE_BUF *ip = &instack[indepth - 1];

  if (if_stack == NULL)
    error ("if-less #elif");
  else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#elif after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (if_stack->fname != NULL && ip->fname != NULL &&
	  strcmp(if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf(stderr, ")\n");
    }
    if_stack->type = T_ELIF;
  }
  
  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_ELIF);
}

/*
 * evaluate a #if expression in BUF, of length LENGTH,
 * making careful arrangements to handle `defined' and
 * prepare for calling the yacc parser.
 */
static int
eval_if_expression (buf, length)
     U_CHAR *buf;
     int length;
{
  FILE_BUF temp_ibuf, temp_obuf;
  HASHNODE *save_defined;
  int value;

  bzero (&temp_ibuf, sizeof temp_ibuf);	/* paranoia */
  temp_ibuf.length = length;
  temp_ibuf.buf = temp_ibuf.bufp = buf;

  temp_obuf.length = length;
  temp_obuf.bufp = temp_obuf.buf = (U_CHAR *) xmalloc (length);

  save_defined = install("defined", T_SPEC_DEFINED, 0);
  rescan (&temp_ibuf, &temp_obuf);
  *temp_obuf.bufp = '\0';
  value = parse_c_expression(temp_obuf.buf);

  delete (save_defined);	/* clean up special symbol */
  free (temp_obuf.buf);
  
  return value;
}

/*
 * routine to handle ifdef/ifndef.  Try to look up the symbol,
 * then do or don't skip to the #endif/#else/#elif depending
 * on what directive is actually being processed.
 */
do_xifdef (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  HASHNODE *lookup();
  int skip;
  FILE_BUF *ip = &instack[indepth - 1];

  SKIP_WHITE_SPACE (buf);
  skip = (lookup(buf) == NULL) ^ (keyword->type == T_IFNDEF);
  conditional_skip (ip, skip, T_IF);
}

/*
 * push TYPE on stack; then, if SKIP is nonzero, skip ahead.
 */
static
conditional_skip (ip, skip, type)
     FILE_BUF *ip;
     int skip, type;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = ip->fname;
  temp->lineno = ip->lineno;
  temp->next = if_stack;
  if_stack = temp;

  if_stack->type = type;
  
  if (skip != 0) {
    skip_if_group(ip);
    return;
  } else {
    ++if_stack->if_succeeded;
    output_line_command(ip, &outbuf);	/* JF */
  }
}

/*
 * skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 */
static
skip_if_group(ip)
     FILE_BUF *ip;
{
  register U_CHAR *bp = ip->bufp, *cp;
  register U_CHAR *endb = ip->buf + ip->length;
  struct keyword_table *kt;
  U_CHAR *save_sharp, *skip_to_end_of_comment (), *skip_quoted_string ();
  IF_STACK_FRAME *save_if_stack = if_stack; /* don't pop past here */

  while (bp <= endb) {
    switch (*bp++) {
    case '/':			/* possible comment */
      if (*bp == '*') {
	ip->bufp = ++bp;
	bp = skip_to_end_of_comment (ip, &ip->lineno);
      }
      break;
    case '\"':
    case '\'':
      ip->bufp = bp - 1;
      bp = skip_quoted_string (ip, NULL);	/* JF was (ip) */
      break;
    case '\n':
      ++ip->lineno;
      break;
    case '#':
      /* # keyword: the # must be first nonblank char on the line */
      for (cp = bp - 1; cp >= ip->buf; cp--)
	if (*cp == '\n')
	  break;
      cp++;			/* skip nl or move back into buffer */
      SKIP_WHITE_SPACE (cp);
      if (cp != bp - 1)	/* ????? */
	break;

      save_sharp = cp;		/* point at '#' */
      SKIP_WHITE_SPACE (bp);
      for (kt = keyword_table; kt->length >= 0; kt++) {
	IF_STACK_FRAME *temp;
	if (strncmp(bp, kt->name, kt->length) == 0
	    && !is_idchar[bp[kt->length]]) {
	  switch (kt->type) {
	  case T_IF:
	  case T_IFDEF:
	  case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
	    temp->next = if_stack;
	    if_stack = temp;
	    temp->lineno = ip->lineno;
	    temp->fname = ip->fname;
	    temp->type = kt->type;
	    break;
	  case T_ELSE:
	  case T_ELIF:
	  case T_ENDIF:
	    ip->bufp = save_sharp;
	    if (if_stack == NULL) {
	      U_CHAR msg[50];
	      sprintf (msg, "if-less #%s", kt->name);
	      error (msg);
	      break;
	    }
	    else if (if_stack == save_if_stack)
	      return;		/* found what we came for */

	    if (kt->type != T_ENDIF) {
	      if (if_stack->type == T_ELSE)
		error ("#else or #elif after #else");
	      if_stack->type = kt->type;
	      break;
	    }

	    temp = if_stack;
	    if_stack = if_stack->next;
	    free (temp);
	    break;
	  }
	}
      }
    }
  }
  ip->bufp = bp;
  ip->lineno = instack->lineno; /* bufp won't be right, though */
  error ("unterminated #if/#ifdef/#ifndef conditional");
  /* after this returns, the main loop will exit because ip->bufp
     now points to the end of the buffer.  I am not sure whether
     this is dirty or not. */
  return;
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */ 
do_else(buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  register U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth - 1];

  if (if_stack == NULL) {
    error ("if-less #else");
    return;
  } else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#else after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (strcmp(if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf(stderr, ")\n");
    }
    if_stack->type = T_ELSE;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip);
  else {
    ++if_stack->if_succeeded;	/* continue processing input */
    output_line_command(ip, op);	/* JF try to keep line #s right? */
  }
}

/*
 * unstack after #endif command
 */
do_endif(buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct keyword_table *keyword;
{
  register U_CHAR *bp;

  if (if_stack == NULL)
    error ("if-less #endif");
  else {
    IF_STACK_FRAME *temp = if_stack;
    if_stack = if_stack->next;
    free (temp);
    /* JF try to keep line #s right? */
    output_line_command (&instack[indepth - 1], op);
  }
}

/*
 * Skip a comment, assuming the input ptr immediately follows the
 * initial slash-star.  Bump line counter as necessary.
 * (The canonical line counter is &ip->lineno).
 * Don't use this routine (or the next one) if bumping the line
 * counter is not sufficient to deal with newlines in the string.
 */
U_CHAR *
skip_to_end_of_comment (ip, line_counter)
     register FILE_BUF *ip;
     int *line_counter;		/* place to remember newlines, or NULL */
{
  register U_CHAR *limit = ip->buf + ip->length;
  register U_CHAR *bp = ip->bufp;
  FILE_BUF *op = &outbuf;	/* JF */

	/* JF this line_counter stuff is a crock to make sure the
	   comment is only put out once, no matter how many times
	   the comment is skipped.  It almost works */
  if (put_out_comments && !line_counter) {
    *op->bufp++ = '/';
    *op->bufp++ = '*';
  }
  while (bp < limit) {
    if (put_out_comments && !line_counter)
      *op->bufp++ = *bp;
    switch (*bp++) {
    case '\n':
      if (line_counter != NULL)
	++*line_counter;
      break;
    case '*':
      if (*bp == '/') {
        if (put_out_comments && !line_counter)
	  *op->bufp++ = '/';
	ip->bufp = ++bp;
	return bp;
      }
      break;
    }
  }
  ip->bufp = bp;
  return bp;
}
/*
 * skip over a quoted string.  Unlike skip_to_end_of_comment, this
 * wants ip->bufp at the beginning quote, not after it.  this is so we
 * can tell what kind of quote to match.  return if unescaped eol is
 * encountered --- it is probably some sort of error in the input.
 */
U_CHAR *
skip_quoted_string (ip, count_newlines)
     register FILE_BUF *ip;
     int count_newlines;
{
  register U_CHAR *limit = ip->buf + ip->length;
  register U_CHAR *bp = ip->bufp;
  register U_CHAR c, match;

  match = *bp++;
  while (bp < limit) {
    c = *bp++;
    if (c == '\\') {
      if (*bp++ == '\n' && count_newlines)
	++ip->lineno;
    } else if (c == '\n') {
      bp -= 2;			/* whoa!  back up to eol and punt. */
      break;
    } else if (c == match)
      break;
  }
  ip->bufp = bp;
  return bp;
}

/*
 * write out a #line command, for instance, after an #include file.
 */
static
output_line_command (ip, op)
     FILE_BUF *ip, *op;
{
  int len, line_cmd_buf[500];

  if (ip->fname == NULL)
    return;

#ifdef OUTPUT_LINE_COMMANDS
  sprintf(line_cmd_buf, "#line %d \"%s\"\n", ip->lineno, ip->fname);
#else
  sprintf(line_cmd_buf, "# %d \"%s\"\n", ip->lineno, ip->fname);
#endif
  len = strlen(line_cmd_buf);
  check_expand (op, len);
  if (op->bufp > op->buf && op->bufp[-1] != '\n')	/* JF make sure */
  	*op->bufp++ = '\n';
  bcopy (line_cmd_buf, op->bufp, len);
  op->bufp += len;
}


/* Expand a macro call.
   HP points to the symbol that is the macro being called.
   IP is the input source for reading the arguments of the macro.
   Send the result of the expansion to OP.
   EXCESS_NEWLINES_PTR points to an integer;
   we increment that integer once for each newline swallowed
   in the process of reading this macro call.  */

macroexpand (hp, ip, op, excess_newlines_ptr)
     HASHNODE *hp;
     FILE_BUF *ip, *op;
     int *excess_newlines_ptr;
{
  FILE_BUF *ip2;
  int nargs;
  DEFINITION *defn = hp->value.defn;
  int newlines_found = 0;

  /* it might not actually be a macro.  */
  if (hp->type != T_MACRO)
    return expand_special_symbol (hp, ip, op);

  ip2 = &instack[indepth++];
  bzero (ip2, sizeof (FILE_BUF)); /* paranoia */

  nargs = defn->nargs;

  if (nargs >= 0)
    {
      register U_CHAR *bp, *xbuf;
      U_CHAR *skip_macro_argument ();
      register int i;
      int xbuf_len;
      int offset;		/* offset in expansion,
				   copied a piece at a time */
      int totlen;		/* total amount of exp buffer filled so far */

      register struct reflist *ap;
      struct argptrs { 
		       U_CHAR *argstart;
		       int length;
		     } *args;

      args = (struct argptrs *) alloca ((nargs + 1) * sizeof (struct argptrs));
      if (ip->bufp >= ip->buf+ip->length)
	{			/* JF evil magic to make things work! */
	  ip = &instack[indepth-3];
	}
      bp = ip->bufp;

      /* make sure it really was a macro call. */
      if (isspace(bp[-1])) {
	while (isspace (*bp)) {
	  if (*bp == '\n')
	    ++newlines_found;
	  bp++;
	}
	if (*bp != '(')
	  goto nope;
	bp++;			/* skip over the paren */
      }
      else if (*(bp-1) != '(')
	goto nope;

      for (i = 0; i < nargs; i++) {
	args[i].argstart = bp;
	bp = skip_macro_argument(bp, ip, &newlines_found);
	args[i].length = bp - args[i].argstart;
	if (*bp == ',')
	  bp++;
      }
      args[nargs].argstart = bp;
      if (*bp++ != ')')
	goto nope;

      /* make a rescan buffer with enough room for the pattern plus
	 all the arg strings. */
      xbuf_len = defn->length + 1;
      for (ap = defn->pattern; ap != NULL; ap = ap->next)
	xbuf_len += args[ap->argno - 1].length;
      xbuf = (U_CHAR *) alloca (xbuf_len);

      offset = totlen = 0;
      for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	bcopy (defn->expansion + offset, xbuf + totlen, ap->nchars);
	totlen += ap->nchars;
	offset += ap->nchars;

	if (ap->argno > 0) {
	  bcopy (args[ap->argno - 1].argstart, xbuf + totlen,
		 args[ap->argno - 1].length);
	  totlen += args[ap->argno - 1].length;
	}

	if (totlen > xbuf_len)
	  {
	    /* impossible */
	    error ("cpp impossible internal error: expansion too large");
	    goto nope;		/* this can't happen??? */
	  }
      }

      /* if there is anything left after handling the arg list,
	 copy that in too. */
      if (offset < defn->length) {
	bcopy (defn->expansion + offset, xbuf + totlen,
	       defn->length - offset);
	totlen += defn->length - offset;
      }

      ip2->buf = xbuf;
      ip2->length = totlen;

      /* skip the input over the whole macro call. */
      ip->bufp = bp;

    }
  else
    {
      ip2->buf = ip2->bufp = defn->expansion;
      ip2->length = defn->length;
    }
  
  rescan (ip2, op);
  --indepth;
  *excess_newlines_ptr += newlines_found;
  ip->lineno += newlines_found;

  return 0;

 nope:
  error ("argument mismatch");
  --indepth;
  return 1;
}

/*
 * skip a balanced paren string up to the next comma.
 */
U_CHAR *
skip_macro_argument(bp, ip, newlines)
     U_CHAR *bp;
     FILE_BUF *ip;
     int *newlines;
{
  int paren = 0;
  int quotec = 0;
  
  while (bp < ip->buf + ip->length) {
    switch (*bp) {
    case '(':
      paren++;
      break;
    case ')':
      if (--paren < 0)
	return bp;
      break;
    case '\n':
      ++*newlines;
      break;
    case '/':
      if (bp[1] != '*' || bp + 1 >= ip->buf + ip->length)
	break;
      bp += 2;
      while ((bp[0] != '*' || bp[1] != '/')
	     && bp + 1 < ip->buf + ip->length)
	{
	  if (*bp == '\n') ++*newlines;
	  bp++;
	}
      break;
    case '\'':		/* JF handle quotes right  */
    case '\"':
      for (quotec = *bp++; bp < ip->buf + ip->length && *bp != quotec; bp++)
	{
	  if (*bp == '\\') bp++;
	  if (*bp == '\n')
	    ++*newlines;
	}
      break;
    case ',':
      if (paren == 0)
	return bp;
      break;
    }
    bp++;
  }
  return bp;
}

/*
 * error - print out message.  also make print on stderr.  Uses stdout
 * now for debugging convenience.
 */
error (msg)
     U_CHAR *msg;
{
  int i;
  FILE_BUF *ip = NULL;

  for (i = indepth - 1; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL)
    fprintf(stdout, "file %s, offset %d (line %d): ",
	    ip->fname, ip->bufp - ip->buf, ip->lineno);
  fprintf(stdout, "%s\n", msg);
  return 0;
}

/*
 * if OBUF doesn't have NEEDED bytes after OPTR, make it bigger
 *    this should be a macro, for speed.
 * The "expand" in the name of this routine means buffer expansion,
 * not macro expansion.  It may become necessary to have some hacky
 * mechanism for flushing out the output buffer if it gets too big.
 *
 * As things stand, nothing is ever placed in the output buffer to be
 * removed again except when it's KNOWN to be part of an identifier,
 * so flushing and moving down everything left, instead of expanding,
 * should work ok.
 */
U_CHAR *
check_expand(obuf, needed)
     register FILE_BUF *obuf;
     register int needed;
{
  register int i;
  register U_CHAR *p;
  
  if (obuf->length - (obuf->bufp - obuf->buf) > needed)
    return obuf->buf;

  i = 2 * obuf->length;
  if (needed >= i)
    i += (3 * needed) / 2;

  if ((p = (U_CHAR *) xrealloc (obuf->buf, i)) == NULL)
    return NULL;
  obuf->bufp = p + (obuf->bufp - obuf->buf);
  obuf->buf = p;
  obuf->length = i;

  return p;
}
  
/*
 * install a name in the main hash table, even if it is already there.
 *   name stops with first non alphanumeric, except leading '#'.
 * caller must check against redefinition if that is desired.
 * delete() removes things installed by install() in fifo order.
 * this is important because of the `defined' special symbol used
 * in #if, and also if pushdef/popdef directives are ever implemented.
 */
HASHNODE *
install (name, type, value)
     U_CHAR *name;
     int type;
     int value;
        /* watch out here if sizeof(U_CHAR *) != sizeof (int) */
{
  HASHNODE *hp;
  int i, len = 0, bucket;
  register U_CHAR *p;

  p = name;
  while (is_idchar[*p])
    p++;
  len = p - name;

  i = sizeof (HASHNODE) + len + 1;
  hp = (HASHNODE *) xmalloc (i);
  bucket = hashf(name, len, HASHSIZE);
  hp->bucket_hdr = &hashtab[bucket];
  hp->next = hashtab[bucket];
  hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->type = type;
  hp->length = len;
  hp->value.ival = value;
  hp->name = ((U_CHAR *) hp) + sizeof (HASHNODE);
  bcopy (name, hp->name, len);
  return hp;
}
/*
 * find the most recent hash node for name name (ending with first
 * non-identifier char) installed by install
 */
HASHNODE *
lookup (name)
     U_CHAR *name;
{
  register U_CHAR *bp;
  register HASHNODE *bucket;
  int len;

  for (bp = name; is_idchar[*bp]; bp++)
    ;
  len = bp - name;
  bucket = hashtab[hashf(name, len, HASHSIZE)];
  while (bucket) {
    if (bucket->length == len && strncmp(bucket->name, name, len) == 0)
      return bucket;
    bucket = bucket->next;
  }
  return NULL;
}

/*
 * Delete a hash node.  Some weirdness to free junk from macros.
 * More such weirdness will have to be added if you define more hash
 * types that need it.
 */
delete(hp)
     HASHNODE *hp;
{
  
  if (hp->prev != NULL)
    hp->prev->next = hp->next;
  if (hp->next != NULL)
    hp->next->prev = hp->prev;

  /* make sure that the bucket chain header that
     the deleted guy was on points to the right thing afterwards. */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

  if (hp->type == T_MACRO) {
    DEFINITION *d = hp->value.defn;
    struct reflist *ap, *nextap;

    for (ap = d->pattern; ap != NULL; ap = nextap) {
      nextap = ap->next;
      free (ap);
    }
    free (d);
  }
}

/*
 * return hash function on name.  must be compatible with the one
 * computed a step at a time, elsewhere
 */
int
hashf(name, len, hashsize)
     register U_CHAR *name;
     register int len;
     int hashsize;
{
  register int r = 0;
  
  while (len--)
    r = HASHSTEP(r, *name++);
  
  return MAKE_POS(r) % hashsize;
}


/*
 * initialize random junk in the hash table and maybe other places
 */
initialize_random_junk()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha(c) || c == '_'), etc.
   * Must do set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    ++is_idchar[i - 'a' + 'A'];
    ++is_idchar[i];
    ++is_idstart[i - 'a' + 'A'];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];

  /* horizontal space table */
  ++is_hor_space[' '];
  ++is_hor_space['\t'];

  install("__LINE__", T_SPECLINE, 0);
  install("__DATE__", T_DATE, 0);
  install("__FILE__", T_FILE, 0);
  install("__TIME__", T_TIME, 0);

#ifdef vax
  make_definition("vax 1");
#endif

#ifdef unix
  make_definition("unix 1");
#endif

  /* is there more? */
  
}

/*
 * process a given definition string, for initialization
 */
make_definition(str)
     U_CHAR *str;
{
  FILE_BUF *ip;
  struct keyword_table *kt;

  ip = &instack[indepth++];
  ip->fname = "*Initialization*";

  ip->buf = ip->bufp = str;
  ip->length = strlen(str);
  ip->lineno = 1;

  for (kt = keyword_table; kt->type != T_DEFINE; kt++)
    ;

  /* pass NULL as output ptr to do_define since we KNOW it never
     does any output.... */
  do_define (str, str + strlen(str) /* - 1 JF */ , NULL, kt);
  --indepth;
}

/* JF, this does the work for the -U option */
make_undef(str)
     U_CHAR *str;
{
  FILE_BUF *ip;
  struct keyword_table *kt;

  ip = &instack[indepth++];
  ip->fname = "*undef*";

  ip->buf = ip->bufp = str;
  ip->length = strlen(str);
  ip->lineno = 1;

  for(kt = keyword_table; kt->type != T_UNDEF; kt++)
    ;

  do_undef(str,str + strlen(str) - 1, NULL, kt);
  --indepth;
}


#ifndef BSD
#ifndef BSTRING

void
bzero (b, length)
     register char *b;
     register int length;
{
#ifdef VMS
  short zero = 0;
  long max_str = 65535;

  while (length > max_str)
    {
      (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
      length -= max_str;
      b += max_str;
    }
  (void) LIB$MOVC5 (&zero, &zero, &zero, &length, b);
#else
  while (length-- > 0)
    *b++ = 0;
#endif /* not VMS */
}

void 
bcopy (b1, b2, length)
     register char *b1;
     register char *b2;
     register int length;
{
#ifdef VMS
  long max_str = 65535;

  while (length > max_str)
    {
      (void) LIB$MOVC3 (&max_str, b1, b2);
      length -= max_str;
      b1 += max_str;
      b2 += max_str;
    }
  (void) LIB$MOVC3 (&length, b1, b2);
#else
  while (length-- > 0)
    *b2++ = *b1++;
#endif /* not VMS */
}
 
int
bcmp (b1, b2, length)	/* This could be a macro! */
     register char *b1;
     register char *b2;
      register int length;
 {
#ifdef VMS
   struct dsc$descriptor_s src1 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b1};
   struct dsc$descriptor_s src2 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b2};

   return STR$COMPARE (&src1, &src2);
#else
   while (length-- > 0)
     if (*b1++ != *b2++)
       return 1;

   return 0;
#endif /* not VMS */
}
#endif /* not BSTRING */
#endif /* not BSD */


void
fatal (str, arg)
     char *str, *arg;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, str, arg);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

void
perror_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];

  fprintf (stderr, "%s: ", progname);
  if (errno < sys_nerr)
    fprintf (stderr, "%s for %s\n", sys_errlist[errno], name);
  else
    fprintf (stderr, "cannot open %s\n", sys_errlist[errno], name);
}

void
pfatal_with_name (name)
     char *name;
{
  perror_with_name (name);
  exit (FATAL_EXIT_CODE);
}


static void
memory_full ()
{
  fatal ("Memory exhausted.");
}


char *
xmalloc (size)
     int size;
{
  extern char *malloc ();
  register char *ptr = malloc (size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xrealloc (old, size)
     char *old;
     int size;
{
  extern char *realloc ();
  register char *ptr = realloc (old, size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xcalloc (number, size)
     int number, size;
{
  extern char *malloc ();
  register int total = number * size;
  register char *ptr = malloc (total);
  if (ptr != 0)
    {
      bzero (ptr, total);
      return (ptr);
    }
  memory_full ();
  /*NOTREACHED*/
}
