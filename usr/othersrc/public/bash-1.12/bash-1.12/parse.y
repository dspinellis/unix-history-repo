/* Yacc grammar for bash. */

/* Copyright (C) 1989 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 1, or (at your option) any later
   version.

   Bash is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License along
   with Bash; see the file LICENSE.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

%{
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include "shell.h"
#include "flags.h"
#include "input.h"

#if defined (READLINE)
#include <readline/readline.h>
#endif /* READLINE */

#include <readline/history.h>

#if defined (JOB_CONTROL)
#  include "jobs.h"
#endif /* JOB_CONTROL */

#define YYDEBUG 1
extern int eof_encountered;
extern int no_line_editing;
extern int interactive, interactive_shell;

/* **************************************************************** */
/*								    */
/*		    "Forward" declarations			    */
/*								    */
/* **************************************************************** */

/* This is kind of sickening.  In order to let these variables be seen by
   all the functions that need them, I am forced to place their declarations
   far away from the place where they should logically be found. */

static int reserved_word_acceptable ();

/* PROMPT_STRING_POINTER points to one of these, never to an actual string. */
char *ps1_prompt, *ps2_prompt;

/* Handle on the current prompt string.  Indirectly points through
   ps1_ or ps2_prompt. */
char **prompt_string_pointer = (char **)NULL;
char *current_prompt_string;

/* The number of lines read from input while creating the current command. */
int current_command_line_count = 0;

/* Variables to manage the task of reading here documents, because we need to
   defer the reading until after a complete command has been collected. */
REDIRECT *redirection_needing_here_doc = (REDIRECT *)NULL;
int need_here_doc = 0;
%}

%union {
  WORD_DESC *word;		/* the word that we read. */
  int number;			/* the number that we read. */
  WORD_LIST *word_list;
  COMMAND *command;
  REDIRECT *redirect;
  ELEMENT element;
  PATTERN_LIST *pattern;
}

/* Reserved words.  Members of the first group are only recognized
   in the case that they are preceded by a list_terminator.  Members
   of the second group are recognized only under special circumstances. */
%token IF THEN ELSE ELIF FI CASE ESAC FOR WHILE UNTIL DO DONE FUNCTION
%token IN BANG

/* More general tokens. yylex () knows how to make these. */
%token <word> WORD
%token <number> NUMBER
%token AND_AND OR_OR GREATER_GREATER LESS_LESS LESS_AND
%token GREATER_AND SEMI_SEMI LESS_LESS_MINUS AND_GREATER LESS_GREATER
%token GREATER_BAR

/* The types that the various syntactical units return. */

%type <command> inputunit command pipeline
%type <command> list list0 list1 simple_list simple_list1
%type <command> simple_command shell_command_1 shell_command
%type <command>  group_command if_command elif_clause
%type <redirect> redirection redirections
%type <element> simple_command_element
%type <word_list> words pattern 
%type <pattern> pattern_list case_clause_sequence case_clause_1 pattern_list_1

%start inputunit

%left '&' ';' '\n' yacc_EOF
%left AND_AND OR_OR
%right '|'
%%

inputunit:	simple_list '\n'
			{
			  /* Case of regular command.  Discard the error
			     safety net,and return the command just parsed. */
			  global_command = $1;
			  eof_encountered = 0;
			  discard_parser_constructs (0);
			  YYACCEPT;
			}
	|	'\n'
			{
			  /* Case of regular command, but not a very
			     interesting one.  Return a NULL command. */
			  global_command = (COMMAND *)NULL;
			  YYACCEPT;
			}
	|
		error '\n'
			{
			  /* Error during parsing.  Return NULL command. */
			  global_command = (COMMAND *)NULL;
			  eof_encountered = 0;
			  discard_parser_constructs (1);
			  if (interactive)
			    {
			      YYACCEPT;
			    }
			  else
			    {
			      YYABORT;
			    }
			}
	|	yacc_EOF
			{
			  /* Case of EOF seen by itself.  Do ignoreeof or 
			     not. */
			  global_command = (COMMAND *)NULL;
			  handle_eof_input_unit ();
			  YYACCEPT;
			}
	;

words:	
			{ $$ = (WORD_LIST *)NULL; }
	|	words WORD
			{ $$ = make_word_list ($2, $1); }
	;

redirection:	'>' WORD
			{ $$ = make_redirection ( 1, r_output_direction, $2); }
	|	'<' WORD
			{ $$ = make_redirection ( 0, r_input_direction, $2); }
	|	NUMBER '>' WORD
			{ $$ = make_redirection ($1, r_output_direction, $3); }
	|	NUMBER '<' WORD
			{ $$ = make_redirection ($1, r_input_direction, $3); }
	|	GREATER_GREATER WORD
			{ $$ = make_redirection ( 1, r_appending_to, $2); }
	|	NUMBER GREATER_GREATER WORD
			{ $$ = make_redirection ($1, r_appending_to, $3); }
	|	LESS_LESS WORD
			{
			  $$ = make_redirection ( 0, r_reading_until, $2);
			  redirection_needing_here_doc = $$;
			  need_here_doc = 1;
			}
	|	NUMBER LESS_LESS WORD
			{
			  $$ = make_redirection ($1, r_reading_until, $3);
			  redirection_needing_here_doc = $$;
			  need_here_doc = 1;
			}
	|	LESS_AND NUMBER
			{
			  $$ = make_redirection ( 0, r_duplicating_input, $2);
			}
	|	NUMBER LESS_AND NUMBER
			{
			  $$ = make_redirection ($1, r_duplicating_input, $3);
			}
	|	GREATER_AND NUMBER
			{
			  $$ = make_redirection ( 1, r_duplicating_output, $2);
			}
	|	NUMBER GREATER_AND NUMBER
			{
			  $$ = make_redirection ($1, r_duplicating_output, $3);
			}
	|	LESS_AND WORD
			{
			  $$ = make_redirection
			    (0, r_duplicating_input_word, $2);
			}
	|	NUMBER LESS_AND WORD
			{
			  $$ = make_redirection
			    ($1, r_duplicating_input_word, $3);
			}
	|	GREATER_AND WORD
			{
			  $$ = make_redirection
			    (1, r_duplicating_output_word, $2);
			}
	|	NUMBER GREATER_AND WORD
			{
			  $$ = make_redirection
			    ($1, r_duplicating_output_word, $3);
			}
	|	LESS_LESS_MINUS WORD
			{
			  $$ = make_redirection
			    (0, r_deblank_reading_until, $2);
			  redirection_needing_here_doc = $$;
			  need_here_doc = 1;
			}
	|	NUMBER LESS_LESS_MINUS WORD
			{
			  $$ = make_redirection
			    ($1, r_deblank_reading_until, $3);
			  redirection_needing_here_doc = $$;
			  need_here_doc = 1;
			}
	|	GREATER_AND '-'
			{ $$ = make_redirection ( 1, r_close_this, 0); }
	|	NUMBER GREATER_AND '-'
			{ $$ = make_redirection ($1, r_close_this, 0); }
	|	LESS_AND '-'
			{ $$ = make_redirection ( 0, r_close_this, 0); }
	|	NUMBER LESS_AND '-'
			{ $$ = make_redirection ($1, r_close_this, 0); }
	|	AND_GREATER WORD
			{ $$ = make_redirection ( 1, r_err_and_out, $2); }
	|	NUMBER LESS_GREATER WORD
			{ $$ = make_redirection ( $1, r_input_output, $3); }
	|	LESS_GREATER WORD
			{
			  REDIRECT *t1, *t2;
			  extern WORD_DESC *copy_word ();

			  t1 = make_redirection ( 0, r_input_direction, $2);
			  t2 = make_redirection ( 1, r_output_direction, copy_word ($2));
			  t1->next = t2;
			  $$ = t1;
			}			  
	|	GREATER_BAR WORD
			{ $$ = make_redirection ( 1, r_output_force, $2); }
	|	NUMBER GREATER_BAR WORD
			{ $$ = make_redirection ( $1, r_output_force, $3); }
	;

simple_command_element: WORD
			{ $$.word = $1; $$.redirect = 0; }
	|	redirection
			{ $$.redirect = $1; $$.word = 0; }
	;

redirections:	redirection
			{
			  $$ = $1;
			}
	|	redirections redirection
			{ 
			  register REDIRECT *t = $1;

			  while (t->next)
			    t = t->next;
			  t->next = $2; 
			  $$ = $1;
			}
	;

simple_command:	simple_command_element
			{ $$ = make_simple_command ($1, (COMMAND *)NULL); }
	|	simple_command simple_command_element
			{ $$ = make_simple_command ($2, $1); }
	;

command:	simple_command
			{ $$ = clean_simple_command ($1); }
	|	shell_command
			{ $$ = $1; }
	;

shell_command:	shell_command_1
			{ $$ = $1; }
	|	shell_command_1 redirections
			{ $1->redirects = $2; $$ = $1; }

	|	redirections shell_command_1
			{ $2->redirects = $1; $$ = $2; }
	;

shell_command_1: FOR WORD newlines DO list DONE
			{ $$ = make_for_command ($2, (WORD_LIST *)add_string_to_list ("\"$@\"", (WORD_LIST *)NULL), $5); }
	|	FOR WORD newlines '{' list '}'
			{ $$ = make_for_command ($2, (WORD_LIST *)add_string_to_list ("$@", (WORD_LIST *)NULL), $5); }
	|	FOR WORD ';' newlines DO list DONE
			{ $$ = make_for_command ($2, (WORD_LIST *)add_string_to_list ("\"$@\"", (WORD_LIST *)NULL), $6); }
	|	FOR WORD ';' newlines '{' list '}'
			{ $$ = make_for_command ($2, (WORD_LIST *)add_string_to_list ("\"$@\"", (WORD_LIST *)NULL), $6); }

	|	FOR WORD newlines IN words list_terminator newlines DO list DONE
			{ $$ = make_for_command ($2, (WORD_LIST *)reverse_list ($5), $9); }
	|	FOR WORD newlines IN words list_terminator newlines '{' list '}'
			{ $$ = make_for_command ($2, (WORD_LIST *)reverse_list ($5), $9); }

	|	CASE WORD newlines IN newlines ESAC
			{ $$ = make_case_command ($2, (PATTERN_LIST *)NULL); }
	|	CASE WORD newlines IN case_clause_sequence newlines ESAC
			{ $$ = make_case_command ($2, $5); }
	|	CASE WORD newlines IN case_clause_1 ESAC
			{ /* Nobody likes this...
			     report_syntax_error ("Inserted `;;'"); */
			  $$ = make_case_command ($2, $5); }
 
	|	if_command
			{ $$ = $1; }
	|	WHILE list DO list DONE
			{ $$ = make_while_command ($2, $4); }
	|	UNTIL list DO list DONE
			{ $$ = make_until_command ($2, $4); }

	|	'(' list ')'
			{ $2->flags |= CMD_WANT_SUBSHELL; $$ = $2; }

	|	group_command
			{ $$ = $1; }

	|	WORD '(' ')' newlines group_command
			{ $$ = make_function_def ($1, $5); }

	|	FUNCTION WORD '(' ')' newlines group_command
			{ $$ = make_function_def ($2, $6); }

	|	FUNCTION WORD newlines group_command
			{ $$ = make_function_def ($2, $4); }
	;

if_command:	IF list THEN list FI
			{ $$ = make_if_command ($2, $4, (COMMAND *)NULL); }
	|	IF list THEN list ELSE list FI
			{ $$ = make_if_command ($2, $4, $6); }
	|	IF list THEN list elif_clause FI
			{ $$ = make_if_command ($2, $4, $5); }
	;


group_command:	'{' list '}'
			{ $$ = make_group_command ($2); }
	;

elif_clause:	ELIF list THEN list
			{ $$ = make_if_command ($2, $4, (COMMAND *)NULL); }
	|	ELIF list THEN list ELSE list
			{ $$ = make_if_command ($2, $4, $6); }
	|	ELIF list THEN list elif_clause
			{ $$ = make_if_command ($2, $4, $5); }
	;

case_clause_1:	pattern_list_1
	|	case_clause_sequence pattern_list_1
			{ $2->next = $1; $$ = $2; }
	;

pattern_list_1:	newlines pattern ')' list
			{ $$ = make_pattern_list ($2, $4); }
	|	newlines pattern ')' newlines
			{ $$ = make_pattern_list ($2, (COMMAND *)NULL); }
	|	newlines '(' pattern ')' list
			{ $$ = make_pattern_list ($3, $5); }
	|	newlines '(' pattern ')' newlines
			{ $$ = make_pattern_list ($3, (COMMAND *)NULL); }
	;

case_clause_sequence:  pattern_list

	|	case_clause_sequence pattern_list
			{ $2->next = $1; $$ = $2; }
	;

pattern_list:	newlines pattern ')' list SEMI_SEMI
			{ $$ = make_pattern_list ($2, $4); }
	|	newlines pattern ')' newlines SEMI_SEMI
			{ $$ = make_pattern_list ($2, (COMMAND *)NULL); }
	|	newlines '(' pattern ')' list SEMI_SEMI
			{ $$ = make_pattern_list ($3, $5); }
	|	newlines '(' pattern ')' newlines SEMI_SEMI
			{ $$ = make_pattern_list ($3, (COMMAND *)NULL); }
	;

pattern:	WORD
			{ $$ = make_word_list ($1, (WORD_LIST *)NULL); }
	|	pattern '|' WORD
			{ $$ = make_word_list ($3, $1); }
	;

/* A list allows leading or trailing newlines and
   newlines as operators (equivalent to semicolons).
   It must end with a newline or semicolon.
   Lists are used within commands such as if, for, while.  */

list:		newlines list0
			{
			  $$ = $2;
			  if (need_here_doc)
			    make_here_document (redirection_needing_here_doc);
			  need_here_doc = 0;
			 }
	;

list0:		list1
	|	list1 '\n' newlines
	|	list1 '&' newlines
			{ $$ = command_connect ($1, 0, '&'); }
	|	list1 ';' newlines

	;

list1:		list1 AND_AND newlines list1
			{ $$ = command_connect ($1, $4, AND_AND); }
	|	list1 OR_OR newlines list1
			{ $$ = command_connect ($1, $4, OR_OR); }
	|	list1 '&' newlines list1
			{ $$ = command_connect ($1, $4, '&'); }
	|	list1 ';' newlines list1
			{ $$ = command_connect ($1, $4, ';'); }
	|	list1 '\n' newlines list1
			{ $$ = command_connect ($1, $4, ';'); }
	|	pipeline
			{ $$ = $1; }
	|	BANG pipeline
			{
			  $2->flags |= CMD_INVERT_RETURN;
			  $$ = $2;
			}
	;

list_terminator:'\n'
	|	';'
	|	yacc_EOF
	;

newlines:
	|	newlines '\n'
	;

/* A simple_list is a list that contains no significant newlines
   and no leading or trailing newlines.  Newlines are allowed
   only following operators, where they are not significant.

   This is what an inputunit consists of.  */

simple_list:	simple_list1
			{
			  $$ = $1;
			  if (need_here_doc)
			    make_here_document (redirection_needing_here_doc);
			  need_here_doc = 0;
			}
	|	simple_list1 '&'
			{
			  $$ = command_connect ($1, (COMMAND *)NULL, '&');
			  if (need_here_doc)
			    make_here_document (redirection_needing_here_doc);
			  need_here_doc = 0;
			}
	|	simple_list1 ';'
			{
			  $$ = $1;
			  if (need_here_doc)
			    make_here_document (redirection_needing_here_doc);
			  need_here_doc = 0;
			}
	;

simple_list1:	simple_list1 AND_AND newlines simple_list1
			{ $$ = command_connect ($1, $4, AND_AND); }
	|	simple_list1 OR_OR newlines simple_list1
			{ $$ = command_connect ($1, $4, OR_OR); }
	|	simple_list1 '&' simple_list1
			{ $$ = command_connect ($1, $3, '&'); }
	|	simple_list1 ';' simple_list1
			{ $$ = command_connect ($1, $3, ';'); }
	|	pipeline
			{ $$ = $1; }
	|	BANG pipeline
			{
			  $2->flags |= CMD_INVERT_RETURN;
			  $$ = $2;
			}
	;

pipeline:
		pipeline '|' newlines pipeline
			{ $$ = command_connect ($1, $4, '|'); }
	|	command
			{ $$ = $1; }
	;
%%

/* Initial size to allocate for tokens, and the
   amount to grow them by. */
#define TOKEN_DEFAULT_GROW_SIZE 512

/* The token currently being read. */
int current_token = 0;

/* The last read token, or NULL.  read_token () uses this for context
   checking. */
int last_read_token = 0;

/* The token read prior to last_read_token. */
int token_before_that = 0;

/* Global var is non-zero when end of file has been reached. */
int EOF_Reached = 0;

/* yy_getc () returns the next available character from input or EOF.
   yy_ungetc (c) makes `c' the next character to read.
   init_yy_io (get, unget, type, location) makes the function GET the
   installed function for getting the next character, makes UNGET the
   installed function for un-getting a character, set the type of stream
   (either string or file) from TYPE, and makes LOCATION point to where
   the input is coming from. */

/* Unconditionally returns end-of-file. */
return_EOF ()
{
  return (EOF);
}

/* Variable containing the current get and unget functions.
   See ./input.h for a clearer description. */
BASH_INPUT bash_input;

/* Set all of the fields in BASH_INPUT to NULL. */
void
initialize_bash_input ()
{
  bash_input.type = 0;
  bash_input.name = (char *)NULL;
  bash_input.location.file = (FILE *)NULL;
  bash_input.location.string = (char *)NULL;
  bash_input.getter = (Function *)NULL;
  bash_input.ungetter = (Function *)NULL;
}

/* Set the contents of the current bash input stream from
   GET, UNGET, TYPE, NAME, and LOCATION. */
init_yy_io (get, unget, type, name, location)
     Function *get, *unget;
     int type;
     char *name;
     INPUT_STREAM location;
{
  bash_input.type = type;
  if (bash_input.name)
    free (bash_input.name);

  if (name != (char *)NULL)
    bash_input.name = savestring (name);
  else
    bash_input.name = (char *)NULL;

  bash_input.location = location;
  bash_input.getter = get;
  bash_input.ungetter = unget;
}

/* Call this to get the next character of input. */
yy_getc ()
{
  return (*(bash_input.getter)) ();
}

/* Call this to unget C.  That is, to make C the next character
   to be read. */
yy_ungetc (c)
{
  return (*(bash_input.ungetter)) (c);
}

/* **************************************************************** */
/*								    */
/*		  Let input be read from readline ().		    */
/*								    */
/* **************************************************************** */

#if defined (READLINE)
char *current_readline_prompt = (char *)NULL;
char *current_readline_line = (char *)NULL;
int current_readline_line_index = 0;

static int readline_initialized_yet = 0;
int
yy_readline_get ()
{
  if (!current_readline_line)
    {
      extern sighandler sigint_sighandler ();
      extern int interrupt_immediately;
      extern char *readline ();
      SigHandler *old_sigint;
#if defined (JOB_CONTROL)
      extern pid_t shell_pgrp;
      extern int job_control;
#endif /* JOB_CONTROL */

      if (!readline_initialized_yet)
	{
	  initialize_readline ();
	  readline_initialized_yet = 1;
	}

#if defined (JOB_CONTROL)
      if (job_control)
	give_terminal_to (shell_pgrp);
#endif /* JOB_CONTROL */

      old_sigint = (SigHandler *)signal (SIGINT, sigint_sighandler);
      interrupt_immediately++;

      if (!current_readline_prompt)
	current_readline_line = readline ("");
      else
	current_readline_line = readline (current_readline_prompt);

      interrupt_immediately--;
      signal (SIGINT, old_sigint);

      /* Reset the prompt to whatever is in the decoded value of
	 prompt_string_pointer. */
      reset_readline_prompt ();

      current_readline_line_index = 0;

      if (!current_readline_line)
	{
	  current_readline_line_index = 0;
	  return (EOF);
	}

      current_readline_line =
	(char *)xrealloc (current_readline_line,
			  2 + strlen (current_readline_line));
      strcat (current_readline_line, "\n");
    }

  if (!current_readline_line[current_readline_line_index])
    {
      free (current_readline_line);
      current_readline_line = (char *)NULL;
      return (yy_readline_get ());
    }
  else
    {
      int c = current_readline_line[current_readline_line_index++];
      return (c);
    }
}

int
yy_readline_unget (c)
{
  if (current_readline_line_index && current_readline_line)
    current_readline_line[--current_readline_line_index] = c;
  return (c);
}
  
with_input_from_stdin ()
{
  INPUT_STREAM location;

  location.string = current_readline_line;
  init_yy_io (yy_readline_get, yy_readline_unget,
	      st_string, "readline stdin", location);
}

#else  /* !READLINE */

with_input_from_stdin ()
{
  with_input_from_stream (stdin, "stdin");
}
#endif	/* !READLINE */

/* **************************************************************** */
/*								    */
/*   Let input come from STRING.  STRING is zero terminated.	    */
/*								    */
/* **************************************************************** */

int
yy_string_get ()
{
  register char *string;
  register int c;

  string = bash_input.location.string;
  c = EOF;

  /* If the string doesn't exist, or is empty, EOF found. */
  if (string && *string)
    {
      c = *string++;
      bash_input.location.string = string;
    }
  return (c);
}

int
yy_string_unget (c)
     int c;
{
  *(--bash_input.location.string) = c;
  return (c);
}

void
with_input_from_string (string, name)
     char *string;
     char *name;
{
  INPUT_STREAM location;

  location.string = string;

  init_yy_io (yy_string_get, yy_string_unget, st_string, name, location);
}

/* **************************************************************** */
/*								    */
/*		     Let input come from STREAM.		    */
/*								    */
/* **************************************************************** */

int
yy_stream_get ()
{
  if (bash_input.location.file)
#if defined (USG) || (defined (_POSIX_VERSION) && defined (Ultrix))
    return (sysv_getc (bash_input.location.file));
#else
    return (getc (bash_input.location.file));
#endif	/* !USG && !(_POSIX_VERSION && Ultrix) */
  else
    return (EOF);
}

int
yy_stream_unget (c)
     int c;
{
  return (ungetc (c, bash_input.location.file));
}

with_input_from_stream (stream, name)
     FILE *stream;
     char *name;
{
  INPUT_STREAM location;

  location.file = stream;
  init_yy_io (yy_stream_get, yy_stream_unget, st_stream, name, location);
}

typedef struct stream_saver {
  struct stream_saver *next;
  BASH_INPUT bash_input;
  int line;
} STREAM_SAVER;

/* The globally known line number. */
int line_number = 0;

STREAM_SAVER *stream_list = (STREAM_SAVER *)NULL;

push_stream ()
{
  STREAM_SAVER *saver = (STREAM_SAVER *)xmalloc (sizeof (STREAM_SAVER));

  bcopy (&bash_input, &(saver->bash_input), sizeof (BASH_INPUT));
  saver->line = line_number;
  bash_input.name = (char *)NULL;
  saver->next = stream_list;
  stream_list = saver;
  EOF_Reached = line_number = 0;
}

pop_stream ()
{
  if (!stream_list)
    {
      EOF_Reached = 1;
    }
  else
    {
      STREAM_SAVER *saver = stream_list;

      EOF_Reached = 0;
      stream_list = stream_list->next;

      init_yy_io (saver->bash_input.getter,
		  saver->bash_input.ungetter,
		  saver->bash_input.type,
		  saver->bash_input.name,
		  saver->bash_input.location);

      line_number = saver->line;

      if (saver->bash_input.name)
	free (saver->bash_input.name);

      free (saver);
    }
}

/*
 * This is used to inhibit alias expansion and reserved word recognition
 * inside case statement pattern lists.  A `case statement pattern list'
 * is:
 *	everything between the `in' in a `case word in' and the next ')'
 *	or `esac'
 *	everything between a `;;' and the next `)' or `esac'
 */
static int in_case_pattern_list = 0;

#if defined (ALIAS)
/*
 * Pseudo-global variables used in implementing token-wise alias expansion.
 */

static int expand_next_token = 0;
static char *current_token_being_expanded = (char *)NULL;
static char *pending_token_being_expanded = (char *)NULL;

/*
 * Pushing and popping strings.  This works together with shell_getc to 
 * implement alias expansion on a per-token basis.
 */

typedef struct string_saver {
  struct string_saver *next;
  int expand_alias;  /* Value to set expand_alias to when string is popped. */
  char *saved_line;
  int saved_line_size, saved_line_index, saved_line_terminator;
  char *saved_token_being_expanded;
} STRING_SAVER;

STRING_SAVER *pushed_string_list = (STRING_SAVER *)NULL;

static void save_expansion ();

/*
 * Push the current shell_input_line onto a stack of such lines and make S
 * the current input.  Used when expanding aliases.  EXPAND is used to set
 * the value of expand_next_token when the string is popped, so that the
 * word after the alias in the original line is handled correctly when the
 * alias expands to multiple words.  TOKEN is the token that was expanded
 * into S; it is saved and used to prevent infinite recursive expansion.
 */
static void
push_string (s, expand, token)
     char *s;
     int expand;
     char *token;
{
  extern char *shell_input_line;
  extern int shell_input_line_size, shell_input_line_index,
	     shell_input_line_terminator;
  STRING_SAVER *temp = (STRING_SAVER *) xmalloc (sizeof (STRING_SAVER));

  temp->expand_alias = expand;
  temp->saved_line = shell_input_line;
  temp->saved_line_size = shell_input_line_size;
  temp->saved_line_index = shell_input_line_index;
  temp->saved_line_terminator = shell_input_line_terminator;
  temp->saved_token_being_expanded = current_token_being_expanded;
  temp->next = pushed_string_list;
  pushed_string_list = temp;

  save_expansion (token);

  current_token_being_expanded = token;
  shell_input_line = s;
  shell_input_line_size = strlen (s);
  shell_input_line_index = 0;
  shell_input_line_terminator = '\0';
  expand_next_token = 0;
}

/*
 * Make the top of the pushed_string stack be the current shell input.
 * Only called when there is something on the stack.  Called from shell_getc
 * when it thinks it has consumed the string generated by an alias expansion
 * and needs to return to the original input line.
 */
static void
pop_string ()
{
  extern char *shell_input_line;
  extern int shell_input_line_size, shell_input_line_index,
	     shell_input_line_terminator;
  STRING_SAVER *t;

  if (shell_input_line)
    free (shell_input_line);
  shell_input_line = pushed_string_list->saved_line;
  shell_input_line_index = pushed_string_list->saved_line_index;
  shell_input_line_size = pushed_string_list->saved_line_size;
  shell_input_line_terminator = pushed_string_list->saved_line_terminator;
  expand_next_token = pushed_string_list->expand_alias;
  pending_token_being_expanded = pushed_string_list->saved_token_being_expanded;
  t = pushed_string_list;
  pushed_string_list = pushed_string_list->next;
  free((char *)t);
}

static void
free_string_list ()
{
  register STRING_SAVER *t = pushed_string_list, *t1;

  while (t)
    {
      t1 = t->next;
      if (t->saved_line)
	free (t->saved_line);
      if (t->saved_token_being_expanded)
	free (t->saved_token_being_expanded);
      free ((char *)t);
      t = t1;
    }
  pushed_string_list = (STRING_SAVER *)NULL;
}

/* This is a stack to save the values of all tokens for which alias
   expansion has been performed during the current call to read_token ().
   It is used to prevent alias expansion loops:

      alias foo=bar
      alias bar=baz
      alias baz=foo

   Ideally this would be taken care of by push and pop string, but because
   of when strings are popped the stack will not contain the correct
   strings to test against.  (The popping is done in shell_getc, so that when
   the current string is exhausted, shell_getc can simply pop that string off
   the stack, restore the previous string, and continue with the character
   following the token whose expansion was originally pushed on the stack.)

   What we really want is a record of all tokens that have been expanded for
   aliases during the `current' call to read_token().  This does that, at the
   cost of being somewhat special-purpose (OK, OK vile and unclean).  Brian,
   you had better rewrite this whole piece of garbage before the next version
   is released.
*/

typedef struct _exp_saver {
      struct _exp_saver *next;
      char *saved_token;
} EXPANSION_SAVER;

EXPANSION_SAVER *expanded_token_stack = (EXPANSION_SAVER *)NULL;

static void
save_expansion (s)
     char *s;
{
  EXPANSION_SAVER *t;

  t = (EXPANSION_SAVER *) xmalloc (sizeof (EXPANSION_SAVER));
  t->saved_token = savestring (s);
  t->next = expanded_token_stack;
  expanded_token_stack = t;
}

/*
 * Return 1 if TOKEN has already been expanded in the current `stack' of
 * expansions.  If it has been expanded already, it will appear as the value
 * of saved_token for some entry in the stack of expansions created for the
 * current token being expanded.
 */
static int
token_has_been_expanded (token)
     char *token;
{
  register EXPANSION_SAVER *t = expanded_token_stack;

  while (t)
    {
      if (STREQ (token, t->saved_token))
	return (1);
      t = t->next;
    }
  return (0);
}

static void
free_expansion_stack ()
{
  register EXPANSION_SAVER *t = expanded_token_stack, *t1;

  while (t)
    {
      t1 = t->next;
      free (t->saved_token);
      free (t);
      t = t1;
    }
  expanded_token_stack = (EXPANSION_SAVER *)NULL;
}

#endif /* ALIAS */

/* Return a line of text, taken from wherever yylex () reads input.
   If there is no more input, then we return NULL. */
char *
read_a_line ()
{
  char *line_buffer = (char *)NULL;
  int indx = 0, buffer_size = 0;
  int c;

  while (1)
    {
      c = yy_getc ();

      if (c == 0)
	continue;

      /* If there is no more input, then we return NULL. */
      if (c == EOF)
	{
	  c = '\n';
	  if (!line_buffer)
	    return ((char *)NULL);
	}

      /* `+2' in case the final (200'th) character in the buffer is a newline;
	 otherwise the code below that NULL-terminates it will write over the
	 201st slot and kill the range checking in free(). */
      if (indx + 2 > buffer_size)
	if (!buffer_size)
	  line_buffer = (char *)xmalloc (buffer_size = 200);
	else
	  line_buffer = (char *)xrealloc (line_buffer, buffer_size += 200);

      line_buffer[indx++] = c;
      if (c == '\n')
	{
	  line_buffer[indx] = '\0';
	  return (line_buffer);
	}
    }
}

/* Return a line as in read_a_line (), but insure that the prompt is
   the secondary prompt. */
char *
read_secondary_line ()
{
  prompt_string_pointer = &ps2_prompt;
  prompt_again ();
  return (read_a_line ());
}


/* **************************************************************** */
/*								    */
/*				YYLEX ()			    */
/*								    */
/* **************************************************************** */

/* Reserved words.  These are only recognized as the first word of a
   command.  TOKEN_WORD_ALIST. */
STRING_INT_ALIST word_token_alist[] = {
  { "if", IF },
  { "then", THEN },
  { "else", ELSE },
  { "elif", ELIF },
  { "fi", FI },
  { "case", CASE },
  { "esac", ESAC },
  { "for", FOR },
  { "while", WHILE },
  { "until", UNTIL },
  { "do", DO },
  { "done", DONE },
  { "in", IN },
  { "function", FUNCTION },
  { "{", '{' },
  { "}", '}' },
  { "!", BANG },
  { (char *)NULL,  0}
};

/* Where shell input comes from.  History expansion is performed on each
   line when the shell is interactive. */
char *shell_input_line = (char *)NULL;
int shell_input_line_index = 0;
int shell_input_line_size = 0;	/* Amount allocated for shell_input_line. */
int shell_input_line_len = 0;	/* strlen (shell_input_line) */

/* Either zero, or EOF. */
int shell_input_line_terminator = 0;

/* Return the next shell input character.  This always reads characters
   from shell_input_line; when that line is exhausted, it is time to
   read the next line. */
int
shell_getc (remove_quoted_newline)
     int remove_quoted_newline;
{
  int c;

  QUIT;

#if defined (ALIAS)
  /* If shell_input_line[shell_input_line_index] == 0, but there is
     something on the pushed list of strings, then we don't want to go
     off and get another line.  We let the code down below handle it. */

  if (!shell_input_line || ((!shell_input_line[shell_input_line_index]) &&
			    (pushed_string_list == (STRING_SAVER *)NULL)))
#else /* !ALIAS */
  if (!shell_input_line || !shell_input_line[shell_input_line_index])
#endif /* !ALIAS */
    {
      register int i, l;
      char *pre_process_line (), *expansions;

      restart_read_next_line:

      line_number++;

    restart_read:

      QUIT;	/* XXX experimental */

      i = 0;
      shell_input_line_terminator = 0;

#if defined (JOB_CONTROL)
      notify_and_cleanup ();
#endif

      clearerr (stdin);
      while (c = yy_getc ())
	{
	  if (i + 2 > shell_input_line_size)
	    shell_input_line = (char *)
	      xrealloc (shell_input_line, shell_input_line_size += 256);

	  if (c == EOF)
	    {
	      clearerr (stdin);

	      if (!i)
		shell_input_line_terminator = EOF;

	      shell_input_line[i] = '\0';
	      break;
	    }

	  shell_input_line[i++] = c;

	  if (c == '\n')
	    {
	      shell_input_line[--i] = '\0';
	      current_command_line_count++;
	      break;
	    }
	}
      shell_input_line_index = 0;
      shell_input_line_len = i;		/* == strlen (shell_input_line) */

      if (!shell_input_line || !shell_input_line[0])
	goto after_pre_process;

      if (interactive)
	{
	  expansions = pre_process_line (shell_input_line, 1, 1);

	  free (shell_input_line);
	  shell_input_line = expansions;
	  shell_input_line_len = shell_input_line ?
				 strlen (shell_input_line) :
				 0;
	  /* We have to force the xrealloc below because we don't know the
	     true allocated size of shell_input_line anymore. */
	  shell_input_line_size = shell_input_line_len;
	}

  after_pre_process:
      if (shell_input_line)
	{
	  if (echo_input_at_read)
	    fprintf (stderr, "%s\n", shell_input_line);
	}
      else
	{
	  shell_input_line_size = 0;
	  prompt_string_pointer = &current_prompt_string;
	  prompt_again ();
	  goto restart_read;
	}

      /* Add the newline to the end of this string, iff the string does
	 not already end in an EOF character.  */
      if (shell_input_line_terminator != EOF)
	{
	  l = shell_input_line_len;	/* was a call to strlen */

	  if (l + 3 > shell_input_line_size)
	    shell_input_line = (char *)xrealloc (shell_input_line,
					1 + (shell_input_line_size += 2));

	  strcpy (shell_input_line + l, "\n");
	}
    }
  
  c = shell_input_line[shell_input_line_index];

  if (c)
    shell_input_line_index++;

  if (c == '\\' && remove_quoted_newline &&
      shell_input_line[shell_input_line_index] == '\n')
    {
	prompt_again ();
	goto restart_read_next_line;
    }

#if defined (ALIAS)
  /*
   * If c is NULL, we have reached the end of the current input string.  If
   * pushed_string_list is non-empty, it's time to pop to the previous string
   * because we have fully consumed the result of the last alias expansion.
   * Do it transparently; just return the next character of the string popped
   * to.  We need to hang onto current_token_being_expanded until the token
   * currently being read has been recognized; we can't restore it in
   * pop_string () because the token currently being read would be
   * inappropriately compared with it.  We defer restoration until the next
   * call to read_token ().
   */

  if (!c && (pushed_string_list != (STRING_SAVER *)NULL))
    {
      pop_string ();
      c = shell_input_line[shell_input_line_index];
      if (c)
	shell_input_line_index++;
    }
#endif /* ALIAS */

  if (!c && shell_input_line_terminator == EOF)
    {
      if (shell_input_line_index != 0)
	return ('\n');
      else
	return (EOF);
    }

  return (c);
}

/* Put C back into the input for the shell. */
shell_ungetc (c)
     int c;
{
  if (shell_input_line && shell_input_line_index)
    shell_input_line[--shell_input_line_index] = c;
}

/* Discard input until CHARACTER is seen. */
discard_until (character)
     int character;
{
  int c;

  while ((c = shell_getc (0)) != EOF && c != character)
    ;

  if (c != EOF)
    shell_ungetc (c);
}

#if defined (HISTORY_REEDITING)
/* Tell readline () that we have some text for it to edit. */
re_edit (text)
     char *text;
{
#if defined (READLINE)
  if (strcmp (bash_input.name, "readline stdin") == 0)
    bash_re_edit (text);
#endif /* READLINE */
}
#endif /* HISTORY_REEDITING */

/* Non-zero means do no history expansion on this line, regardless
   of what history_expansion says. */
int history_expansion_inhibited = 0;

/* Do pre-processing on LINE.  If PRINT_CHANGES is non-zero, then
   print the results of expanding the line if there were any changes.
   If there is an error, return NULL, otherwise the expanded line is
   returned.  If ADDIT is non-zero the line is added to the history
   list after history expansion.  ADDIT is just a suggestion;
   REMEMBER_ON_HISTORY can veto, and does.
   Right now this does history expansion. */
char *
pre_process_line (line, print_changes, addit)
     char *line;
     int print_changes, addit;
{
  extern int remember_on_history;
  extern int history_expansion;
  extern int history_expand ();
  char *history_value;
  char *return_value;
  int expanded = 0;

  return_value = line;

  /* History expand the line.  If this results in no errors, then
     add that line to the history if ADDIT is non-zero. */
  if (!history_expansion_inhibited && history_expansion)
    {
      expanded = history_expand (line, &history_value);

      if (expanded)
	{
	  if (print_changes)
	    fprintf (stderr, "%s\n", history_value);

	  /* If there was an error, return NULL. */
	  if (expanded < 0)
	    {
	      free (history_value);

#if defined (HISTORY_REEDITING)
	      /* New hack.  We can allow the user to edit the
		 failed history expansion. */
	      re_edit (line);
#endif /* HISTORY_REEDITING */
	      return ((char *)NULL);
	    }
	}

      /* Let other expansions know that return_value can be free'ed,
	 and that a line has been added to the history list.  Note
	 that we only add lines that have something in them. */
      expanded = 1;
      return_value = history_value;
    }

  if (addit && remember_on_history && *return_value)
    {
      extern int history_control;

      switch (history_control)
	{
	  case 0:
	    bash_add_history (return_value);
	    break;
	  case 1:
	    if (*return_value != ' ')
	      bash_add_history (return_value);
	    break;
	  case 2:
	    {
	      HIST_ENTRY *temp;

	      using_history ();
	      temp = previous_history ();

	      if (!temp || (strcmp (temp->line, return_value) != 0))
		bash_add_history (return_value);

	      using_history ();
	    }
	    break;
	}
    }

  if (!expanded)
    return_value = savestring (line);

  return (return_value);
}


/* Place to remember the token.  We try to keep the buffer
   at a reasonable size, but it can grow. */
char *token = (char *)NULL;

/* Current size of the token buffer. */
int token_buffer_size = 0;

/* Command to read_token () explaining what we want it to do. */
#define READ 0
#define RESET 1
#define prompt_is_ps1 \
      (!prompt_string_pointer || prompt_string_pointer == &ps1_prompt)

/* Function for yyparse to call.  yylex keeps track of
   the last two tokens read, and calls read_token.  */

yylex ()
{
  if (interactive && (!current_token || current_token == '\n'))
    {
      /* Before we print a prompt, we might have to check mailboxes.
	 We do this only if it is time to do so. Notice that only here
	 is the mail alarm reset; nothing takes place in check_mail ()
	 except the checking of mail.  Please don't change this. */
      if (prompt_is_ps1 && time_to_check_mail ())
	{
	  check_mail ();
	  reset_mail_timer ();
	}

      /* Allow the execution of a random command just before the printing
	 of each primary prompt.  If the shell variable PROMPT_COMMAND
	 is set then the value of it is the command to execute. */
      if (prompt_is_ps1)
	{
	  char *command_to_execute;

	  command_to_execute = get_string_value ("PROMPT_COMMAND");

	  if (command_to_execute)
	    {
	      extern Function *last_shell_builtin, *this_shell_builtin;
	      extern int last_command_exit_value;
	      Function *temp_last, *temp_this;
	      int temp_exit_value, temp_eof_encountered;

	      temp_last = last_shell_builtin;
	      temp_this = this_shell_builtin;
	      temp_exit_value = last_command_exit_value;
	      temp_eof_encountered = eof_encountered;

	      parse_and_execute
		(savestring (command_to_execute), "PROMPT_COMMAND");

	      last_shell_builtin = temp_last;
	      this_shell_builtin = temp_this;
	      last_command_exit_value = temp_exit_value;
	      eof_encountered = temp_eof_encountered;
	    }
	}
      prompt_again ();
    }

  token_before_that = last_read_token;
  last_read_token = current_token;
  current_token = read_token (READ);
  return (current_token);
}

/* Called from shell.c when Control-C is typed at top level.  Or
   by the error rule at top level. */
reset_parser ()
{
  read_token (RESET);
}
  
/* When non-zero, we have read the required tokens
   which allow ESAC to be the next one read. */
static int allow_esac_as_next = 0;

/* When non-zero, accept single '{' as a token itself. */
static int allow_open_brace = 0;

/* DELIMITER is the value of the delimiter that is currently
   enclosing, or zero for none. */
static int delimiter = 0;
static int old_delimiter = 0;

/* When non-zero, an open-brace used to create a group is awaiting a close
   brace partner. */
static int open_brace_awaiting_satisfaction = 0;

/* If non-zero, it is the token that we want read_token to return regardless
   of what text is (or isn't) present to be read.  read_token resets this. */
int token_to_read = 0;

/* Read the next token.  Command can be READ (normal operation) or 
   RESET (to normalize state). */
read_token (command)
     int command;
{
  extern int interactive_shell;	/* Whether the current shell is interactive. */
  int character;		/* Current character. */
  int peek_char;		/* Temporary look-ahead character. */
  int result;			/* The thing to return. */
  WORD_DESC *the_word;		/* The value for YYLVAL when a WORD is read. */

  if (token_buffer_size < TOKEN_DEFAULT_GROW_SIZE)
    {
      if (token)
	free (token);
      token = (char *)xmalloc (token_buffer_size = TOKEN_DEFAULT_GROW_SIZE);
    }

  if (command == RESET)
    {
      delimiter = old_delimiter = 0;
      open_brace_awaiting_satisfaction = 0;
      in_case_pattern_list = 0;

#if defined (ALIAS)
      if (pushed_string_list)
	{
	  free_string_list ();
	  pushed_string_list = (STRING_SAVER *)NULL;
	}

      if (pending_token_being_expanded)
	{
	  free (pending_token_being_expanded);
	  pending_token_being_expanded = (char *)NULL;
	}

      if (current_token_being_expanded)
	{
	  free (current_token_being_expanded);
	  current_token_being_expanded = (char *)NULL;
	}

      if (expanded_token_stack)
	{
	  free_expansion_stack ();
	  expanded_token_stack = (EXPANSION_SAVER *)NULL;
	}

      expand_next_token = 0;
#endif /* ALIAS */

      if (shell_input_line)
	{
	  free (shell_input_line);
	  shell_input_line = (char *)NULL;
	  shell_input_line_size = shell_input_line_index = 0;
	}
      last_read_token = '\n';
      token_to_read = '\n';
      return ('\n');
    }

  if (token_to_read)
    {
      int rt = token_to_read;
      token_to_read = 0;
      return (rt);
    }

#if defined (ALIAS)
  /*
   * Now we can replace current_token_being_expanded with 
   * pending_token_being_expanded, since the token that would be 
   * inappropriately compared has already been returned.
   *
   * To see why restoring current_token_being_expanded in pop_string ()
   * could be a problem, consider "alias foo=foo".  Then try to
   * expand `foo'.  The initial value of current_token_being_expanded is
   * NULL, so that is what is pushed onto pushed_string_list as the
   * value of saved_token_being_expanded.  "foo" then becomes shell_input_line.
   * read_token calls shell_getc for `f', `o', `o', and then shell_getc
   * hits the end of shell_input_line.  pushed_string_list is not empty
   * so it gets popped.  If we were to blindly restore
   * current_token_being_expanded at this point, `foo' would be compared
   * with a NULL string in the check for recursive expansion, and would
   * infinitely recurse.
   */
  if (pending_token_being_expanded)
    {
      if (current_token_being_expanded)
	free (current_token_being_expanded);
      current_token_being_expanded = pending_token_being_expanded;
      pending_token_being_expanded = (char *)NULL;
    }

  /* If we hit read_token () and there are no saved strings on the
     pushed_string_list, then we are no longer currently expanding a
     token.  This can't be done in pop_stream, because pop_stream
     may pop the stream before the current token has finished being
     completely expanded (consider what happens when we alias foo to foo,
     and then try to expand it). */
  if (!pushed_string_list && current_token_being_expanded)
    {
      free (current_token_being_expanded);
      current_token_being_expanded = (char *)NULL;

      if (expanded_token_stack)
	{
	  free_expansion_stack ();
	  expanded_token_stack = (EXPANSION_SAVER *)NULL;
	}
    }

  /* This is a place to jump back to once we have successfully expanded a
     token with an alias and pushed the string with push_string () */
re_read_token:

#endif /* ALIAS */

  /* Read a single word from input.  Start by skipping blanks. */
  while ((character = shell_getc (1)) != EOF && whitespace (character));

  if (character == EOF)
    return (yacc_EOF);

  if (character == '#' && !interactive)
    {
      /* A comment.  Discard until EOL or EOF, and then return a newline. */
      discard_until ('\n');
      shell_getc (0);

      /* If we're about to return an unquoted newline, we can go and collect
	 the text of any pending here document. */
      if (need_here_doc)
	make_here_document (redirection_needing_here_doc);
      need_here_doc = 0;

#if defined (ALIAS)
      expand_next_token = 0;
#endif /* ALIAS */

      return ('\n');
    }

  if (character == '\n')
    {
      /* If we're about to return an unquoted newline, we can go and collect
	 the text of any pending here document. */
      if (need_here_doc)
	make_here_document (redirection_needing_here_doc);
      need_here_doc = 0;

#if defined (ALIAS)
      expand_next_token = 0;
#endif /* ALIAS */

      return (character);
    }

  if (member (character, "()<>;&|"))
    {
#if defined (ALIAS)
      /* Turn off alias tokenization iff this character sequence would
	 not leave us ready to read a command. */
      if (character == '<' || character == '>')
	expand_next_token = 0;
#endif /* ALIAS */

      /* Please note that the shell does not allow whitespace to
	 appear in between tokens which are character pairs, such as
	 "<<" or ">>".  I believe this is the correct behaviour. */
      if (character == (peek_char = shell_getc (1)))
	{
	  switch (character)
	    {
	      /* If '<' then we could be at "<<" or at "<<-".  We have to
		 look ahead one more character. */
	    case '<':
	      peek_char = shell_getc (1);
	      if (peek_char == '-')
		return (LESS_LESS_MINUS);
	      else
		{
		  shell_ungetc (peek_char);
		  return (LESS_LESS);
		}

	    case '>':
	      return (GREATER_GREATER);

	    case ';':
	      in_case_pattern_list = 1;
#if defined (ALIAS)
	      expand_next_token = 0;
#endif /* ALIAS */
	      return (SEMI_SEMI);

	    case '&':
	      return (AND_AND);

	    case '|':
	      return (OR_OR);
	    }
	}
      else
	{
	  if (peek_char == '&')
	    {
	      switch (character)
		{
		case '<': return (LESS_AND);
		case '>': return (GREATER_AND);
		}
	    }
	  if (character == '<' && peek_char == '>')
	    return (LESS_GREATER);
	  if (character == '>' && peek_char == '|')
	    return (GREATER_BAR);
	  if (peek_char == '>' && character == '&')
	    return (AND_GREATER);
	}
      shell_ungetc (peek_char);

      /* If we look like we are reading the start of a function
	 definition, then let the reader know about it so that
	 we will do the right thing with `{'. */
      if (character == ')' &&
	  last_read_token == '(' && token_before_that == WORD)
	{
	  allow_open_brace = 1;
#if defined (ALIAS)
	  expand_next_token = 0;
#endif /* ALIAS */
	}

      if (in_case_pattern_list && (character == ')'))
	in_case_pattern_list = 0;

      return (character);
    }

  /* Hack <&- (close stdin) case. */
  if (character == '-')
    {
      switch (last_read_token)
	{
	case LESS_AND:
	case GREATER_AND:
	  return (character);
	}
    }
  
  /* Okay, if we got this far, we have to read a word.  Read one,
     and then check it against the known ones. */
  {
    /* Index into the token that we are building. */
    int token_index = 0;

    /* ALL_DIGITS becomes zero when we see a non-digit. */
    int all_digits = digit (character);

    /* DOLLAR_PRESENT becomes non-zero if we see a `$'. */
    int dollar_present = 0;

    /* QUOTED becomes non-zero if we see one of ("), ('), (`), or (\). */
    int quoted = 0;

    /* Non-zero means to ignore the value of the next character, and just
       to add it no matter what. */
    int pass_next_character = 0;

    /* Non-zero means parsing a dollar-paren construct.  It is the count of
       un-quoted closes we need to see. */
    int dollar_paren_level = 0;

    /* Non-zero means parsing a dollar-bracket construct ($[...]).  It is
       the count of un-quoted `]' characters we need to see. */
    int dollar_bracket_level = 0;

    /* Another level variable.  This one is for dollar_parens inside of
       double-quotes. */
    int delimited_paren_level = 0;

    for (;;)
      {
	if (character == EOF)
	  goto got_token;

	if (pass_next_character)
	  {
	    pass_next_character = 0;
	    goto got_character;
	  }

      if (delimiter && character == '\\' && delimiter != '\'')
	{
	  peek_char = shell_getc (0);
	  if (peek_char != '\\')
	    shell_ungetc (peek_char);
	  else
	    {
	      token[token_index++] = character;
	      goto got_character;
	    }
	}

	/* Handle backslashes.  Quote lots of things when not inside of
	   double-quotes, quote some things inside of double-quotes. */
	   
	if (character == '\\' && delimiter != '\'')
	  {
	    peek_char = shell_getc (0);

	    /* Backslash-newline is ignored in all cases excepting
	       when quoted with single quotes. */
	    if (peek_char == '\n')
	      {
		character = '\n';
		goto next_character;
	      }
	    else
	      {
		shell_ungetc (peek_char);

		/* If the next character is to be quoted, do it now. */
		if (!delimiter || delimiter == '`' ||
		    ((delimiter == '"' ) &&
		     (member (peek_char, slashify_in_quotes))))
		  {
		    pass_next_character++;
		    quoted = 1;
		    goto got_character;
		  }
	      }
	  }

	/* This is a hack, in its present form.  If a backquote substitution
	   appears within double quotes, everything within the backquotes
	   should be read as part of a single word.  Jesus.  Now I see why
	   Korn introduced the $() form. */
	if (delimiter && delimiter == '"' && character == '`')
	  {
	    old_delimiter = delimiter;
	    delimiter = character;
	    goto got_character;
	  }

	if (delimiter)
	  {
	    if (character == delimiter)
	      {
		if (delimited_paren_level)
		  {
#if defined (NOTDEF)
		    report_error ("Expected ')' before %c", character);
		    return ('\n');
#else
		    goto got_character;
#endif /* NOTDEF */
		  }

		delimiter = 0;

		if (old_delimiter == '"' && character == '`')
		  {
		    delimiter = old_delimiter;
		    old_delimiter = 0;
		  }

		goto got_character;
	      }
	  }

	if (!delimiter || delimiter == '`' || delimiter == '"')
	  {
	    if (character == '$')
	      {
		peek_char = shell_getc (1);
		shell_ungetc (peek_char);
		if (peek_char == '(')
		  {
		    if (!delimiter)
		      dollar_paren_level++;
		    else
		      delimited_paren_level++;

		    pass_next_character++;
		    goto got_character;
		  }
		else if (peek_char == '[')
		  {
		    if (!delimiter)
		      dollar_bracket_level++;

		    pass_next_character++;
		    goto got_character;
		  }
	      }

	    /* If we are parsing a $() or $[] construct, we need to balance
	       parens and brackets inside the construct.  This whole function
	       could use a rewrite. */
	    if (character == '(')
	      {
		if (delimiter && delimited_paren_level)
		  delimited_paren_level++;

		if (!delimiter && dollar_paren_level)
		  dollar_paren_level++;
	      }

	    if (character == '[')
	      {
		if (!delimiter && dollar_bracket_level)
		  dollar_bracket_level++;
	      }

	    /* This code needs to take into account whether we are inside a
	       case statement pattern list, and whether this paren is supposed
	       to terminate it (hey, it could happen).  It's not as simple
	       as just using in_case_pattern_list, because we're not parsing
	       anything while we're reading a $( ) construct.  Maybe we
	       should move that whole mess into the yacc parser. */
	    if (character == ')')
	      {
		if (delimiter && delimited_paren_level)
		  delimited_paren_level--;

		if (!delimiter && dollar_paren_level)
		  {
		    dollar_paren_level--;
		    goto got_character;
		  }
	      }

	    if (character == ']')
	      {
		if (!delimiter && dollar_bracket_level)
		  {
		    dollar_bracket_level--;
		    goto got_character;
		  }
	      }
	  }

	if (!dollar_paren_level && !dollar_bracket_level && !delimiter &&
	    member (character, " \t\n;&()|<>"))
	  {
	    shell_ungetc (character);
	    goto got_token;
	  }
    
	if (!delimiter)
	  {
	    if (character == '"' || character == '`' || character == '\'')
	      {
		quoted = 1;
		delimiter = character;
		goto got_character;
	      }
	  }

	if (all_digits) all_digits = digit (character);
	if (character == '$') dollar_present = 1;

      got_character:

	token[token_index++] = character;

	if (token_index == (token_buffer_size - 1))
	  token = (char *)xrealloc (token, (token_buffer_size
					    += TOKEN_DEFAULT_GROW_SIZE));
	{
	  char *decode_prompt_string ();

	next_character:
	  if (character == '\n' && interactive && bash_input.type != st_string)
	    prompt_again ();
	}
	/* We want to remove quoted newlines (that is, a \<newline> pair)
	   unless we are within single quotes or pass_next_character is
	   set (the shell equivalent of literal-next). */
	character = shell_getc ((delimiter != '\'') && (!pass_next_character));
      }

  got_token:

    token[token_index] = '\0';
	
    if ((delimiter || dollar_paren_level || dollar_bracket_level) &&
	character == EOF)
      {
	if (dollar_paren_level && !delimiter)
	  delimiter = ')';
	else if (dollar_bracket_level && !delimiter)
	  delimiter = ']';

	report_error ("Unexpected EOF.  Looking for `%c'.", delimiter);
	return (-1);
      }

    if (all_digits)
      {
	/* Check to see what thing we should return.  If the last_read_token
	   is a `<', or a `&', or the character which ended this token is
	   a '>' or '<', then, and ONLY then, is this input token a NUMBER.
	   Otherwise, it is just a word, and should be returned as such. */

	if ((character == '<' || character == '>') ||
	    (last_read_token == LESS_AND ||
	     last_read_token == GREATER_AND))
	  {
	    yylval.number = atoi (token); /* was sscanf (token, "%d", &(yylval.number)); */
	    return (NUMBER);
	  }
      }

    /* Handle special case.  IN is recognized if the last token
       was WORD and the token before that was FOR or CASE. */
    if ((last_read_token == WORD) &&
	((token_before_that == FOR) || (token_before_that == CASE)) &&
	(STREQ (token, "in")))
      {
	if (token_before_that == CASE)
	  {
	    in_case_pattern_list = 1;
	    allow_esac_as_next++;
	  }
	return (IN);
      }

    /* Ditto for DO in the FOR case. */
    if ((last_read_token == WORD) && (token_before_that == FOR) &&
	(STREQ (token, "do")))
      return (DO);

    /* Ditto for ESAC in the CASE case. 
       Specifically, this handles "case word in esac", which is a legal
       construct, certainly because someone will pass an empty arg to the
       case construct, and we don't want it to barf.  Of course, we should
       insist that the case construct has at least one pattern in it, but
       the designers disagree. */
    if (allow_esac_as_next)
      {
	allow_esac_as_next--;
	if (STREQ (token, "esac"))
	  {
	    in_case_pattern_list = 0;
	    return (ESAC);
	  }
      }

    /* Ditto for `{' in the FUNCTION case. */
    if (allow_open_brace)
      {
	allow_open_brace = 0;
	if (STREQ (token, "{"))
	  {
	    open_brace_awaiting_satisfaction++;
	    return ('{');
	  }
      }

#if defined (ALIAS)

#define command_token_position(token) \
    ((token) != SEMI_SEMI && reserved_word_acceptable (token))

    /* OK, we have a token.  Let's try to alias expand it, if (and only if)
       it's eligible. 

       It is eligible for expansion if the shell is in interactive mode, and
       the token is unquoted and the last token read was a command
       separator (or expand_next_token is set), and we are currently
       processing an alias (pushed_string_list is non-empty) and this
       token is not the same as the current or any previously
       processed alias.

       Special cases that disqualify:
	 In a pattern list in a case statement (in_case_pattern_list). */
    if (interactive_shell && !quoted && !in_case_pattern_list &&
	(command_token_position (last_read_token) || expand_next_token))
      {
	char *alias_expand_word (), *expanded;
	if (current_token_being_expanded &&
	     ((STREQ (token, current_token_being_expanded)) ||
	      (token_has_been_expanded (token))))
	  goto no_expansion;

	expanded = alias_expand_word (token);
	if (expanded)
	  {
	    int len = strlen (expanded), expand_next;
	    char *temp;

	    /* Erase the current token. */
	    token_index = 0;

	    expand_next = (expanded[len - 1] == ' ') ||
			  (expanded[len - 1] == '\t');

	    temp = savestring (token);
	    push_string (expanded, expand_next, temp);
	    goto re_read_token;
	  }
	else
	  /* This is an eligible token that does not have an expansion. */
no_expansion:
	  expand_next_token = 0;
      }
    else
      {
	expand_next_token = 0;
      }
#endif /* ALIAS */

    /* Check to see if it is a reserved word.  */
    if (!dollar_present && !quoted &&
	reserved_word_acceptable (last_read_token))
      {
	int i;
	for (i = 0; word_token_alist[i].word != (char *)NULL; i++)
	  if (STREQ (token, word_token_alist[i].word))
	    {
	      if (in_case_pattern_list && (word_token_alist[i].token != ESAC))
		break;

	      if (word_token_alist[i].token == ESAC)
		in_case_pattern_list = 0;

	      if (word_token_alist[i].token == '{')
		open_brace_awaiting_satisfaction++;

	      return (word_token_alist[i].token);
	    }
      }

    /* What if we are attempting to satisfy an open-brace grouper? */
    if (open_brace_awaiting_satisfaction && strcmp (token, "}") == 0)
      {
	open_brace_awaiting_satisfaction--;
	return ('}');
      }

    the_word = (WORD_DESC *)xmalloc (sizeof (WORD_DESC));
    the_word->word = (char *)xmalloc (1 + strlen (token));
    strcpy (the_word->word, token);
    the_word->dollar_present = dollar_present;
    the_word->quoted = quoted;
    the_word->assignment = assignment (token);

    yylval.word = the_word;
    result = WORD;
    if (last_read_token == FUNCTION)
      allow_open_brace = 1;
  }
  return (result);
}

#if defined (NOTDEF)		/* Obsoleted function no longer used. */
/* Return 1 if this token is a legal shell `identifier'; that is, it consists
   solely of letters, digits, and underscores, and does not begin with a 
   digit. */
legal_identifier (name)
     char *name;
{
  register char *s;

  if (!name || !*name)
    return (0);

  if (digit (*name))
    return (0);

  for (s = name; s && *s; s++)
    {
      if (!isletter (*s) && !digit (*s) && (*s != '_'))
	return (0);
    }
  return (1);
}
#endif /* NOTDEF */

/* Return 1 if TOKEN is a token that after being read would allow
   a reserved word to be seen, else 0. */
static int
reserved_word_acceptable (token)
     int token;
{
  if (member (token, "\n;()|&{") ||
      token == AND_AND ||
      token == BANG ||
      token == DO ||
      token == ELIF ||
      token == ELSE ||
      token == IF ||
      token == OR_OR ||
      token == SEMI_SEMI ||
      token == THEN ||
      token == UNTIL ||
      token == WHILE ||
      token == 0)
    return (1);
  else
    return (0);
}

#if defined (READLINE)
/* Called after each time readline is called.  This insures that whatever
   the new prompt string is gets propagated to readline's local prompt
   variable. */
reset_readline_prompt ()
{
  if (prompt_string_pointer && *prompt_string_pointer)
    {
      char *temp_prompt, *decode_prompt_string ();

      temp_prompt = decode_prompt_string (*prompt_string_pointer);

      if (!temp_prompt)
	temp_prompt = savestring ("");

      if (current_readline_prompt)
	free (current_readline_prompt);

      current_readline_prompt = temp_prompt;
    }
}
#endif

/* A list of tokens which can be followed by newlines, but not by
   semi-colons.  When concatenating multiple lines of history, the
   newline separator for such tokens is replaced with a space. */
int no_semi_successors[] = {
  '\n', '{', '(', ')', ';',
  CASE, DO, ELSE, IF, IN, SEMI_SEMI, THEN, UNTIL, WHILE,
  0
};

/* Add a line to the history list.
   The variable COMMAND_ORIENTED_HISTORY controls the style of history
   remembering;  when non-zero, and LINE is not the first line of a
   complete parser construct, append LINE to the last history line instead
   of adding it as a new line. */
bash_add_history (line)
     char *line;
{
  extern int command_oriented_history;
  int add_it = 1;

  if (command_oriented_history && current_command_line_count > 1)
    {
      register int offset;
      register HIST_ENTRY *current, *old;
      char *chars_to_add, *new_line;

      /* If we are not within a delimited expression, try to be smart
	 about which separators can be semi-colons and which must be
	 newlines. */
      if (!delimiter)
	{
	  register int i;

	  chars_to_add = (char *)NULL;

	  for (i = 0; no_semi_successors[i]; i++)
	    {
	      if (token_before_that == no_semi_successors[i])
		{
		  chars_to_add = " ";
		  break;
		}
	    }
	  if (!chars_to_add)
	    chars_to_add = "; ";
	}
      else
	chars_to_add = "\n";

      using_history ();

      current = previous_history ();

      if (current)
	{
	  offset = where_history ();
	  new_line = (char *) xmalloc (1
				       + strlen (current->line)
				       + strlen (line)
				       + strlen (chars_to_add));
	  sprintf (new_line, "%s%s%s", current->line, chars_to_add, line);
	  old = replace_history_entry (offset, new_line, current->data);
	  free (new_line);

	  if (old)
	    {
	      /* Note that the old data is not freed, since it was simply
		 copied to the new history entry. */
	      if (old->line)
		free (old->line);

	      free (old);
	    }
	  add_it = 0;
	}
    }

  if (add_it)
    {
      extern int history_lines_this_session;

      add_history (line);
      history_lines_this_session++;
    }
  using_history ();
}

/* Issue a prompt, or prepare to issue a prompt when the next character
   is read. */
prompt_again ()
{
  char *temp_prompt, *decode_prompt_string ();

  ps1_prompt = get_string_value ("PS1");
  ps2_prompt = get_string_value ("PS2");

  if (!prompt_string_pointer)
    prompt_string_pointer = &ps1_prompt;

  if (*prompt_string_pointer)
    temp_prompt = decode_prompt_string (*prompt_string_pointer);
  else
    temp_prompt = savestring ("");

  current_prompt_string = *prompt_string_pointer;
  prompt_string_pointer = &ps2_prompt;

#if defined (READLINE)
  if (!no_line_editing)
    {
      if (current_readline_prompt)
	free (current_readline_prompt);
      
      current_readline_prompt = temp_prompt;
    }
  else
#endif	/* READLINE */
    {
      if (interactive)
	{
	  fprintf (stderr, "%s", temp_prompt);
	  fflush (stderr);
	}
      free (temp_prompt);
    }
}

#include "maxpath.h"

/* Return a string which will be printed as a prompt.  The string
   may contain special characters which are decoded as follows:
   
	\t	the time
	\d	the date
	\n	CRLF
	\s	the name of the shell
	\w	the current working directory
	\W	the last element of PWD
	\u	your username
	\h	the hostname
	\#	the command number of this command
	\!	the history number of this command
	\$	a $ or a # if you are root
	\<octal> character code in octal
	\\	a backslash
*/
#include <sys/param.h>
#include <time.h>

#define PROMPT_GROWTH 50
char *
decode_prompt_string (string)
     char *string;
{
  int result_size = PROMPT_GROWTH;
  int result_index = 0;
  char *result = (char *)xmalloc (PROMPT_GROWTH);
  int c;
  char *temp = (char *)NULL;

  result[0] = 0;
  while (c = *string++)
    {
      if (c == '\\')
	{
	  c = *string;

	  switch (c)
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      {
		char octal_string[4];
		int n;

		strncpy (octal_string, string, 3);
		octal_string[3] = '\0';

		n = read_octal (octal_string);

		temp = savestring ("\\");
		if (n != -1)
		  {
		    string += 3;
		    temp[0] = n;
		  }

		c = 0;
		goto add_string;
	      }
	  
	    case 't':
	    case 'd':
	      /* Make the current time/date into a string. */
	      {
		long the_time = time (0);
		char *ttemp = ctime (&the_time);
		temp = savestring (ttemp);

		if (c == 't')
		  {
		    strcpy (temp, temp + 11);
		    temp[8] = '\0';
		  }
		else
		  temp[10] = '\0';

		goto add_string;
	      }

	    case 'n':
	      if (!no_line_editing)
		temp = savestring ("\r\n");
	      else
		temp = savestring ("\n");
	      goto add_string;

	    case 's':
	      {
		extern char *shell_name;
		extern char *base_pathname ();

		temp = base_pathname (shell_name);
		temp = savestring (temp);
		goto add_string;
	      }
	
	    case 'w':
	    case 'W':
	      {
		/* Use the value of PWD because it is much more effecient. */
#define EFFICIENT
#ifdef EFFICIENT
		char *polite_directory_format (), t_string[MAXPATHLEN];

		temp = get_string_value ("PWD");

		if (!temp)
		  getwd (t_string);
		else
		  strcpy (t_string, temp);
#else
		getwd (t_string);
#endif	/* EFFICIENT */

		if (c == 'W')
		  {
		    char *dir = (char *)rindex (t_string, '/');
		    if (dir && dir != t_string)
		      strcpy (t_string, dir + 1);
		    temp = savestring (t_string);
		  }
		else
		  temp = savestring (polite_directory_format (t_string));
		goto add_string;
	      }
      
	    case 'u':
	      {
		extern char *current_user_name;
		temp = savestring (current_user_name);

		goto add_string;
	      }

	    case 'h':
	      {
		extern char *current_host_name;
		char *t_string;

		temp = savestring (current_host_name);
		if (t_string = (char *)index (temp, '.'))
		  *t_string = '\0';
		
		goto add_string;
	      }

	    case '#':
	      {
		extern int current_command_number;
		char number_buffer[20];
		sprintf (number_buffer, "%d", current_command_number);
		temp = savestring (number_buffer);
		goto add_string;
	      }

	    case '!':
	      {
		extern int history_base, where_history ();
		char number_buffer[20];

		using_history ();
		if (get_string_value ("HISTSIZE"))
		  sprintf (number_buffer, "%d",
			   history_base + where_history ());
		else
		  strcpy (number_buffer, "!");
		temp = savestring (number_buffer);
		goto add_string;
	      }

	    case '$':
	      temp = savestring (geteuid () == 0 ? "#" : "$");
	      goto add_string;

	    case '\\':
	      temp = savestring ("\\");
	      goto add_string;

	    default:
	      temp = savestring ("\\ ");
	      temp[1] = c;

	    add_string:
	      if (c)
		string++;
	      result =
		(char *)sub_append_string (temp, result,
					   &result_index, &result_size);
	      temp = (char *)NULL; /* Free ()'ed in sub_append_string (). */
	      result[result_index] = '\0';
	      break;
	    }
	}
      else
	{
	  while (3 + result_index > result_size)
	    result = (char *)xrealloc (result, result_size += PROMPT_GROWTH);

	  result[result_index++] = c;
	  result[result_index] = '\0';
	}
    }

  /* I don't really think that this is a good idea.  Do you? */
  if (!find_variable ("NO_PROMPT_VARS"))
    {
      WORD_LIST *expand_string (), *list;
      char *string_list ();

      list = expand_string (result, 1);
      free (result);
      result = string_list (list);
      dispose_words (list);
    }

  return (result);
}

/* Report a syntax error, and restart the parser.  Call here for fatal
   errors. */
yyerror ()
{
  report_syntax_error ((char *)NULL);
  reset_parser ();
}

/* Report a syntax error with line numbers, etc.
   Call here for recoverable errors.  If you have a message to print,
   then place it in MESSAGE, otherwise pass NULL and this will figure
   out an appropriate message for you. */
report_syntax_error (message)
     char *message;
{
  if (message)
    {
      if (!interactive)
	{
	  char *name = bash_input.name ? bash_input.name : "stdin";
	  report_error ("%s:%d: `%s'", name, line_number, message);
	}
      else
	report_error ("%s", message);

      return;
    }

  if (shell_input_line && *shell_input_line)
    {
      char *error_token, *t = shell_input_line;
      register int i = shell_input_line_index;
      int token_end = 0;

      if (!t[i] && i)
	i--;

      while (i && (t[i] == ' ' || t[i] == '\t' || t[i] == '\n'))
	i--;

      if (i)
	token_end = i + 1;

      while (i && !member (t[i], " \n\t;|&"))
	i--;

      while (i != token_end && member (t[i], " \n\t"))
	i++;

      if (token_end)
	{
	  error_token = (char *)alloca (1 + (token_end - i));
	  strncpy (error_token, t + i, token_end - i);
	  error_token[token_end - i] = '\0';

	  report_error ("syntax error near `%s'", error_token);
	}
      else if ((i == 0) && (token_end == 0))	/* a 1-character token */
	{
	  error_token = (char *) alloca (2);
	  strncpy(error_token, t + i, 1);
	  error_token[1] = '\0';

	  report_error ("syntax error near `%s'", error_token);
	}

      if (!interactive)
	{
	  char *temp = savestring (shell_input_line);
	  char *name = bash_input.name ? bash_input.name : "stdin";
	  int l = strlen (temp);

	  while (l && temp[l - 1] == '\n')
	    temp[--l] = '\0';

	  report_error ("%s:%d: `%s'", name, line_number, temp);
	  free (temp);
	}
    }
  else
    report_error ("Syntax error");
}

/* ??? Needed function. ??? We have to be able to discard the constructs
   created during parsing.  In the case of error, we want to return
   allocated objects to the memory pool.  In the case of no error, we want
   to throw away the information about where the allocated objects live.
   (dispose_command () will actually free the command. */
discard_parser_constructs (error_p)
     int error_p;
{
/*   if (error_p) {
     fprintf (stderr, "*");
  } */
}
   
/* Do that silly `type "bye" to exit' stuff.  You know, "ignoreeof". */

/* The number of times that we have encountered an EOF character without
   another character intervening.  When this gets above the limit, the
   shell terminates. */
int eof_encountered = 0;

/* The limit for eof_encountered. */
int eof_encountered_limit = 10;

/* If we have EOF as the only input unit, this user wants to leave
   the shell.  If the shell is not interactive, then just leave.
   Otherwise, if ignoreeof is set, and we haven't done this the
   required number of times in a row, print a message. */
handle_eof_input_unit ()
{
  extern int login_shell, EOF_Reached;

  if (interactive)
    {
      /* If the user wants to "ignore" eof, then let her do so, kind of. */
      if (find_variable ("ignoreeof") || find_variable ("IGNOREEOF"))
	{
	  if (eof_encountered < eof_encountered_limit)
	    {
	      fprintf (stderr, "Use \"%s\" to leave the shell.\n",
		       login_shell ? "logout" : "exit");
	      eof_encountered++;
	      /* Reset the prompt string to be $PS1. */
	      prompt_string_pointer = (char **)NULL;
	      prompt_again ();
	      last_read_token = current_token = '\n';
	      return;
	    } 
	}

      /* In this case EOF should exit the shell.  Do it now. */
      reset_parser ();
      exit_builtin ((WORD_LIST *)NULL);
    }
  else
    {
      /* We don't write history files, etc., for non-interactive shells. */
      EOF_Reached = 1;
    }
}
