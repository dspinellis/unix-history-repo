/* copy_command.c -- copy a COMMAND structure.  This is needed
   primarily for making function definitions, but I'm not sure
   that anyone else will need it.  */

/* Copyright (C) 1987,1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include "shell.h"

/* Forward declaration. */
extern COMMAND *copy_command ();

WORD_DESC *
copy_word (word)
     WORD_DESC *word;
{
  WORD_DESC *new_word = (WORD_DESC *)xmalloc (sizeof (WORD_DESC));
  bcopy (word, new_word, sizeof (WORD_DESC));
  new_word->word = savestring (word->word);
  return (new_word);
}

/* Copy the chain of words in LIST.  Return a pointer to 
   the new chain. */
WORD_LIST *
copy_word_list (list)
     WORD_LIST *list;
{
  WORD_LIST *new_list = NULL;

  while (list)
    {
      WORD_LIST *temp = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
      temp->next = new_list;
      new_list = temp;
      new_list->word = copy_word (list->word);
      list = list->next;
    }
  return ((WORD_LIST *)reverse_list (new_list));
}

PATTERN_LIST *
copy_case_clause (clause)
     PATTERN_LIST *clause;
{
  PATTERN_LIST *new_clause = (PATTERN_LIST *)xmalloc (sizeof (PATTERN_LIST));
  new_clause->patterns = copy_word_list (clause->patterns);
  new_clause->action = copy_command (clause->action);
  return (new_clause);
}

PATTERN_LIST *
copy_case_clauses (clauses)
     PATTERN_LIST *clauses;
{
  PATTERN_LIST *new_list = (PATTERN_LIST *)NULL;

  while (clauses)
    {
      PATTERN_LIST *new_clause = copy_case_clause (clauses);
      new_clause->next = new_list;
      new_list = new_clause;
      clauses = clauses->next;
    }
  return ((PATTERN_LIST *)reverse_list (new_list));
}

/* Copy a single redirect. */
REDIRECT *
copy_redirect (redirect)
     REDIRECT *redirect;
{
  REDIRECT *new_redirect = (REDIRECT *)xmalloc (sizeof (REDIRECT));
  bcopy (redirect, new_redirect, (sizeof (REDIRECT)));
  switch (redirect->instruction)
    {
    case r_reading_until:
    case r_deblank_reading_until:
      new_redirect->here_doc_eof = savestring (redirect->here_doc_eof);
      /* There is NO BREAK HERE ON PURPOSE!!!! */
    case r_appending_to:
    case r_output_direction:
    case r_input_direction:
    case r_inputa_direction:
    case r_err_and_out:
    case r_input_output:
    case r_output_force:
    case r_duplicating_input_word:
    case r_duplicating_output_word:
      new_redirect->redirectee.filename =
	copy_word (redirect->redirectee.filename);
      break;
    }
  return (new_redirect);
}
  
REDIRECT *
copy_redirects (list)
     REDIRECT *list;
{
  REDIRECT *new_list = NULL;

  while (list)
    {
      REDIRECT *temp = copy_redirect (list);
      temp->next = new_list;
      new_list = temp;
      list = list->next;
    }
  return ((REDIRECT *)reverse_list (new_list));
}
  
FOR_COM *
copy_for_command (com)
     FOR_COM *com;
{
  FOR_COM *new_for = (FOR_COM *)xmalloc (sizeof (FOR_COM));
  new_for->flags = com->flags;
  new_for->name = copy_word (com->name);
  new_for->map_list = copy_word_list (com->map_list);
  new_for->action = copy_command (com->action);
  return (new_for);
}

GROUP_COM *
copy_group_command (com)
     GROUP_COM *com;
{
  GROUP_COM *new_group = (GROUP_COM *)xmalloc (sizeof (GROUP_COM));

  new_group->command = copy_command (com->command);
  return (new_group);
}

CASE_COM *
copy_case_command (com)
     CASE_COM *com;
{
  CASE_COM *new_case = (CASE_COM *)xmalloc (sizeof (CASE_COM));

  new_case->flags = com->flags;
  new_case->word = copy_word (com->word);
  new_case->clauses = copy_case_clauses (com->clauses);
  return (new_case);
}

WHILE_COM *
copy_while_command (com)
     WHILE_COM *com;
{
  WHILE_COM *new_while = (WHILE_COM *)xmalloc (sizeof (WHILE_COM));

  new_while->flags = com->flags;
  new_while->test = copy_command (com->test);
  new_while->action = copy_command (com->action);
  return (new_while);
}

IF_COM *
copy_if_command (com)
     IF_COM *com;
{
  IF_COM *new_if = (IF_COM *)xmalloc (sizeof (IF_COM));

  new_if->flags = com->flags;
  new_if->test = copy_command (com->test);
  new_if->true_case = copy_command (com->true_case);
  new_if->false_case = copy_command (com->false_case);
  return (new_if);
}

SIMPLE_COM *
copy_simple_command (com)
     SIMPLE_COM *com;
{
  SIMPLE_COM *new_simple = (SIMPLE_COM *)xmalloc (sizeof (SIMPLE_COM));

  new_simple->flags = com->flags;
  new_simple->words = copy_word_list (com->words);
  new_simple->redirects = copy_redirects (com->redirects);
  return (new_simple);
}
  
FUNCTION_DEF *
copy_function_def (com)
     FUNCTION_DEF *com;
{
  FUNCTION_DEF *new_def = (FUNCTION_DEF *)xmalloc (sizeof (FUNCTION_DEF));

  new_def->name = copy_word (com->name);
  new_def->command = copy_command (com->command);
  return (new_def);
}

/* Copy the command structure in COMMAND.  Return a pointer to the
   copy.  Don't you forget to dispose_command () on this pointer
   later! */
COMMAND *
copy_command (command)
     COMMAND *command;
{
  COMMAND *new_command = (COMMAND *)NULL;

  if (command)
    {
      new_command = (COMMAND *)xmalloc (sizeof (COMMAND));
      bcopy (command, new_command, sizeof (COMMAND));
      new_command->flags = command->flags;

      if (command->redirects)
	new_command->redirects = copy_redirects (command->redirects);

      switch (command->type)
	{
	case cm_for:
	  new_command->value.For = copy_for_command (command->value.For);
	  break;

	case cm_group:
	  new_command->value.Group = copy_group_command (command->value.Group);
	  break;

	case cm_case:
	  new_command->value.Case = copy_case_command (command->value.Case);
	  break;
      
	case cm_until:
	case cm_while:
	  new_command->value.While = copy_while_command (command->value.While);
	  break;
      
	case cm_if:
	  new_command->value.If = copy_if_command (command->value.If);
	  break;
      
	case cm_simple:
	  new_command->value.Simple = copy_simple_command (command->value.Simple);
	  break;
      
	case cm_connection:
	  {
	    CONNECTION *new_connection;

	    new_connection = (CONNECTION *)xmalloc (sizeof (CONNECTION));
	    new_connection->connector = command->value.Connection->connector;
	    new_connection->first =
	      copy_command (command->value.Connection->first);
	    new_connection->second =
	      copy_command (command->value.Connection->second);
	    new_command->value.Connection = new_connection;
	    break;
	  }
      
	  /* Pathological case.  I'm not even sure that you can have a
	     function definition as part of a function definition. */
	case cm_function_def:
	  new_command->value.Function_def =
	    copy_function_def (command->value.Function_def);
	  break;
	}
    }
  return (new_command);
}
