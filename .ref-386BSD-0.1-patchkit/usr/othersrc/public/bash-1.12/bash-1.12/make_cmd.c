/* make_cmd.c --
   Functions for making instances of the various parser constructs. */

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
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include "config.h"
#include "general.h"
#include "error.h"
#include "command.h"
#include "flags.h"
#include "filecntl.h"

#if defined (JOB_CONTROL)
#include "jobs.h"
#endif

WORD_DESC *
make_word (string)
     char *string;
{
  WORD_DESC *temp;

  temp = (WORD_DESC *)xmalloc (sizeof (WORD_DESC));
  temp->word = savestring (string);
  temp->quoted = temp->dollar_present = temp->assignment = 0;

  while (*string)
    {
      if (*string == '$') temp->dollar_present = 1;

      if (member (*string, "'`\\\""))
	{
	  temp->quoted = 1;
	  if (*string == '\\')
	    string++;
	}

      if (*string)
	(string++);
    }
  return (temp);
}

WORD_DESC *
make_word_from_token (token)
     int token;
{
  char tokenizer[2];

  tokenizer[0] = token;
  tokenizer[1] = '\0';

  return (make_word (tokenizer));
}

WORD_LIST *
make_word_list (word, link)
     WORD_DESC *word;
     WORD_LIST *link;
{
  WORD_LIST *temp;

  temp = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
  temp->word = word;
  temp->next = link;
  return (temp);
}

WORD_LIST *
add_string_to_list (string, list)
     char *string;
     WORD_LIST *list;
{
  WORD_LIST *temp = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
  temp->word = make_word (string);
  temp->next = list;
  return (temp);
}

WORD_DESC *
coerce_to_word (number)
     int number;
{
  char *string;

  string = (char *)alloca (24);
  sprintf (string, "%d", number);
  return (make_word (string));
}

COMMAND *
make_command (type, pointer)
     enum command_type type;
     SIMPLE_COM *pointer;
{
  COMMAND *temp;

  temp = (COMMAND *)xmalloc (sizeof (COMMAND));
  temp->type = type;
  temp->value.Simple = pointer;
  temp->value.Simple->flags = 0;
  temp->flags = 0;
  temp->redirects = (REDIRECT *)NULL;
  return (temp);
}

COMMAND *
command_connect (com1, com2, connector)
     COMMAND *com1, *com2;
     int connector;
{
  CONNECTION *temp;

  temp = (CONNECTION *)xmalloc (sizeof (CONNECTION));
  temp->connector = connector;
  temp->first = com1;
  temp->second = com2;
  return (make_command (cm_connection, (SIMPLE_COM *)temp));
}

COMMAND *
make_for_command (name, map_list, action)
     WORD_DESC *name;
     WORD_LIST *map_list;
     COMMAND *action;
{
  FOR_COM *temp = (FOR_COM *)xmalloc (sizeof (FOR_COM));

  temp->flags = 0;
  temp->name = name;
  temp->map_list = map_list;
  temp->action = action;
  return (make_command (cm_for, (SIMPLE_COM *)temp));
}

COMMAND *
make_group_command (command)
     COMMAND *command;
{
  GROUP_COM *temp = (GROUP_COM *)xmalloc (sizeof (GROUP_COM));

  temp->command = command;
  return (make_command (cm_group, (SIMPLE_COM *)temp));
}

COMMAND *
make_case_command (word, clauses)
     WORD_DESC *word;
     PATTERN_LIST *clauses;
{
  extern GENERIC_LIST *reverse_list ();
  CASE_COM *temp;

  temp = (CASE_COM *)xmalloc (sizeof (CASE_COM));
  temp->flags = 0;
  temp->word = word;
  temp->clauses = (PATTERN_LIST *)reverse_list (clauses);
  return (make_command (cm_case, (SIMPLE_COM *)temp));
}

PATTERN_LIST *
make_pattern_list (patterns, action)
     WORD_LIST *patterns;
     COMMAND *action;
{
  PATTERN_LIST *temp;

  temp = (PATTERN_LIST *)xmalloc (sizeof (PATTERN_LIST));
  temp->patterns = patterns;
  temp->action = action;
  temp->next = NULL;
  return (temp);
}

COMMAND *
make_if_command (test, true_case, false_case)
     COMMAND *test, *true_case, *false_case;
{
  IF_COM *temp;

  temp = (IF_COM *)xmalloc (sizeof (IF_COM));
  temp->flags = 0;
  temp->test = test;
  temp->true_case = true_case;
  temp->false_case = false_case;
  return (make_command (cm_if, (SIMPLE_COM *)temp));
}

COMMAND *
make_until_or_while (test, action, which)
     COMMAND *test, *action;
     enum command_type which;
{
  WHILE_COM *temp;

  temp = (WHILE_COM *)xmalloc (sizeof (WHILE_COM));
  temp->flags = 0;
  temp->test = test;
  temp->action = action;
  return (make_command (which, (SIMPLE_COM *)temp));
}

COMMAND *
make_while_command (test, action)
     COMMAND *test, *action;
{
  return (make_until_or_while (test, action, cm_while));
}

COMMAND *
make_until_command (test, action)
     COMMAND *test, *action;
{
  return (make_until_or_while (test, action, cm_until));
}

COMMAND *
make_bare_simple_command ()
{
  COMMAND *command;
  SIMPLE_COM *temp = (SIMPLE_COM *)xmalloc (sizeof (SIMPLE_COM));

  temp->flags = 0;
  temp->words = (WORD_LIST *)NULL;
  temp->redirects = (REDIRECT *)NULL;
  command = (COMMAND *)xmalloc (sizeof (COMMAND));
  command->type = cm_simple;
  command->redirects = (REDIRECT *)NULL;
  command->flags = 0;
  command->value.Simple = temp;
  return (command);
}

/* Return a command which is the connection of the word or redirection
   in ELEMENT, and the command * or NULL in COMMAND. */
COMMAND *
make_simple_command (element, command)
     ELEMENT element;
     COMMAND *command;
{
  /* If we are starting from scratch, then make the initial command
     structure.  Also note that we have to fill in all the slots, since
     malloc doesn't return zeroed space. */
  if (!command)
    command = make_bare_simple_command ();
 
  if (element.word)
    {
      WORD_LIST *tw = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
      tw->word = element.word;
      tw->next = command->value.Simple->words;
      command->value.Simple->words = tw;
    }
  else
    {
      REDIRECT *r = element.redirect;
      /* Due to the way <> is implemented, there may be more than a single
	 redirection in element.redirect.  We just follow the chain as far
	 as it goes, and hook onto the end. */
      while (r->next)
	r = r->next;
      r->next = command->value.Simple->redirects;
      command->value.Simple->redirects = element.redirect;
    }
  return (command);
}

#define POSIX_HERE_DOCUMENTS
make_here_document (temp)
     REDIRECT *temp;
{
  int kill_leading = 0;

  switch (temp->instruction)
    {
      /* Because we are Bourne compatible, we read the input for this
	 << or <<- redirection now, from wherever input is coming from.
	 We store the input read into a WORD_DESC.  Replace the text of
	 the redirectee.word with the new input text.  If <<- is on,
	 then remove leading TABS from each line. */

      case r_deblank_reading_until:	/* <<-foo */
	kill_leading++;
	/* ... */
      case r_reading_until:		/* <<foo */
	{
	  extern char *redirection_expand ();
	  extern char *string_quote_removal ();
	  char *redirectee_word;
	  int len;

	  char *document = (char *)NULL;
	  int document_index = 0, document_size = 0;

#if !defined (POSIX_HERE_DOCUMENTS)
	  /* Because of Bourne shell semantics, we turn off globbing, but
	     only for this style of redirection.  I feel a little ill.  */
	  {
	    extern int disallow_filename_globbing;
	    int old_value = disallow_filename_globbing;
	    disallow_filename_globbing = 1;

	    redirectee_word = redirection_expand (temp->redirectee.filename);

	    disallow_filename_globbing = old_value;
	  }
#else /* POSIX_HERE_DOCUMENTS */
	  /* Quote removal is the only expansion performed on the delimiter
	     for here documents, making it an extremely special case.  I
	     still feel ill. */
	  redirectee_word =
	    string_quote_removal (temp->redirectee.filename->word, 0);
#endif /* POSIX_HERE_DOCUMENTS */

	  /* redirection_expand will return NULL if the expansion results in
	     multiple words or no words.  Check for that here, and just abort
	     this here document if it does. */
	  if (redirectee_word)
	    len = strlen (redirectee_word);
	  else
	    {
	      temp->here_doc_eof = savestring ("");
	      goto document_done;
	    }

	  free (temp->redirectee.filename->word);
	  temp->here_doc_eof = redirectee_word;

	  /* Read lines from wherever lines are coming from.
	     For each line read, if kill_leading, then kill the
	     leading tab characters.
	     If the line matches redirectee_word exactly, then we have
	     manufactured the document.  Otherwise, add the line to the
	     list of lines in the document. */
	  {
	    extern char *read_secondary_line ();
	    char *line;
	    int l;

	    while (line = read_secondary_line ())
	      {
		if (!line)
		  goto document_done;

		if (kill_leading)
		  {
		    register int i;

		    /* Hack:  To be compatible with some Bourne shells, we 
		       check the word before stripping the whitespace.  This
		       is a hack, though. */
		    if ((strncmp (line, redirectee_word, len) == 0) &&
			line[len] == '\n')
		      goto document_done;

		    for (i = 0; line[i] == '\t'; i++)
		      ;

		    if (i)
		      strcpy (&line[0], &line[i]);
		  }

		if ((strncmp (line, redirectee_word, len) == 0) &&
		    line[len] == '\n')
		  goto document_done;

		l = strlen (line);
		if (l + document_index >= document_size)
		  {
		    document = (char *)
		      xrealloc (document, (document_size += (10 * l)));
		  }

		if (l != 0)
		  {
		    strcpy (&document[document_index], line);
		    free (line);
		    document_index += l;
		  }
	      }
  document_done:
	    if (!document)
	      document = savestring ("");
	    temp->redirectee.filename->word = document;
	  }
	}
    }
}
   
/* Generate a REDIRECT from SOURCE, DEST, and INSTRUCTION. 
   INSTRUCTION is the instruction type, SOURCE is an INT,
   and DEST is an INT or a WORD_DESC *. */
REDIRECT *
make_redirection (source, instruction, dest)
     enum r_instruction instruction;
{
  REDIRECT *temp = (REDIRECT *)xmalloc (sizeof (REDIRECT));

  /* First do the common cases. */
  temp->redirector = source;
  temp->redirectee.dest = dest;
  temp->instruction = instruction;
  temp->next = (REDIRECT *)NULL;

  switch (instruction)
    {

    case r_output_direction:	/* >foo */
    case r_output_force:	/* >| foo */
      temp->flags = O_TRUNC | O_WRONLY | O_CREAT;
      break;

    case r_input_direction:	/* <foo */
    case r_inputa_direction:	/* foo & makes this. */
      temp->flags = O_RDONLY;
      break;

    case r_appending_to:	/* >>foo */
      temp->flags = O_APPEND | O_WRONLY | O_CREAT;
      break;

    case r_deblank_reading_until: /* <<-foo */
    case r_reading_until:	/* << foo */
      break;

    case r_duplicating_input:		/* 1<&2 */
    case r_duplicating_output:		/* 1>&2 */
    case r_close_this:			/* <&- */
    case r_duplicating_input_word:	/* 1<&$foo */
    case r_duplicating_output_word:	/* 1>&$foo */
      break;
    
    case r_err_and_out:		/* command &>filename */
      temp->flags = O_TRUNC | O_WRONLY | O_CREAT;
      break;

    case r_input_output:
      temp->flags = O_RDWR;
      break;

    default:
      programming_error ("Redirection instruction from yyparse () '%d' is\n\
out of range in make_redirection ().", instruction);
      abort ();
      break;
    }
  return (temp);
}

COMMAND *
make_function_def (name, command)
     WORD_DESC *name;
     COMMAND *command;
{
  FUNCTION_DEF *temp;

  temp = (FUNCTION_DEF *)xmalloc (sizeof (FUNCTION_DEF));
  temp->command = command;
  temp->name = name;
  return (make_command (cm_function_def, (SIMPLE_COM *)temp));
}

/* Reverse the word list and redirection list in the simple command
   has just been parsed.  It seems simpler to do this here the one
   time then by any other method that I can think of. */
COMMAND *
clean_simple_command (command)
     COMMAND *command;
{
  extern GENERIC_LIST *reverse_list ();

  if (command->type != cm_simple)
    {
      programming_error
	("clean_simple_command () got a command with type %d.", command->type);
    }
  else
    {
      command->value.Simple->words =
	(WORD_LIST *)reverse_list (command->value.Simple->words);
      command->value.Simple->redirects = 
	(REDIRECT *)reverse_list (command->value.Simple->redirects);
    }

  return (command);
}

/* Cons up a new array of words.  The words are taken from LIST,
   which is a WORD_LIST *.  Absolutely everything is malloc'ed,
   so you should free everything in this array when you are done.
   The array is NULL terminated. */
char **
make_word_array (list)
     WORD_LIST *list;
{
  int count = list_length (list);
  char **array = (char **)xmalloc ((1 + count) * sizeof (char *));

  for (count = 0; list; count++)
    {
      array[count] = (char *)xmalloc (1 + strlen (list->word->word));
      strcpy (array[count], list->word->word);
      list = list->next;
    }
  array[count] = (char *)NULL;
  return (array);
}
