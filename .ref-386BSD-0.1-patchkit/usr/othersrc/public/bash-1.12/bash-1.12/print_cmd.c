/* print_command -- A way to make readable commands from a command tree. */
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

#if defined (HAVE_VFPRINTF)
#include <varargs.h>
#endif

#include "shell.h"
#include "y.tab.h"

static int indentation = 0;
static int indentation_amount = 4;

#define PRINTED_COMMAND_GROW_SIZE 1024
char *the_printed_command = (char *)NULL;
int the_printed_command_size = 0;
int command_string_index = 0;

/* Non-zero means the stuff being printed is inside of a function def. */
static int inside_function_def = 0;
static int skip_this_indent = 0;

/* Print COMMAND (a command tree) on standard output. */
print_command (command)
     COMMAND *command;
{
  char *make_command_string ();
  command_string_index = 0;
  printf ("%s", make_command_string (command));
}

/* Make a string which is the printed representation of the command
   tree in COMMAND.  We return this string.  However, the string is
   not consed, so you have to do that yourself if you want it to
   remain around. */
char *
make_command_string (command)
     COMMAND *command;
{
  command_string_index = 0;
  make_command_string_internal (command);
  return (the_printed_command);
}

/* The internal function.  This is the real workhorse. */
make_command_string_internal (command)
     COMMAND *command;
{
  if (!command)
    {
      cprintf ("");
    }
  else
    {
      if (skip_this_indent)
	skip_this_indent--;
      else
	indent (indentation);

      if (command->flags & CMD_WANT_SUBSHELL)
	cprintf ("( ");

      if (command->flags & CMD_INVERT_RETURN)
	cprintf (" ! ");

      switch (command->type)
	{
	case cm_for:
	  print_for_command (command->value.For);
	  break;

	case cm_case:
	  print_case_command (command->value.Case);
	  break;

	case cm_while:
	  print_while_command (command->value.While);
	  break;

	case cm_until:
	  print_until_command (command->value.While);
	  break;

	case cm_if:
	  print_if_command (command->value.If);
	  break;

	case cm_simple:
	  print_simple_command (command->value.Simple);
	  break;

	case cm_connection: 

	  skip_this_indent++;
	  make_command_string_internal (command->value.Connection->first);

	  switch (command->value.Connection->connector)
	    {
	    case '&':
	    case '|':
	      {
		char c = command->value.Connection->connector;
		cprintf (" %c", c);
		if (c != '&' || command->value.Connection->second)
		  {
		    cprintf (" ");
		    skip_this_indent++;
		  }
	      }
	      break;

	    case AND_AND:
	      cprintf (" && ");
	      if (command->value.Connection->second)
		skip_this_indent++;
	      break;

	    case OR_OR:
	      cprintf (" || ");
	      if (command->value.Connection->second)
		skip_this_indent++;
	      break;
	
	    case ';':
	      cprintf (";");

	      if (inside_function_def)
		cprintf ("\n");
	      else
		{
		  cprintf (" ");
		  if (command->value.Connection->second)
		    skip_this_indent++;
		}
	      break;

	    default:
	      cprintf ("OOPS!  Bad connector `%d'!",
		       command->value.Connection->connector);
	      break;
	    }

	  make_command_string_internal (command->value.Connection->second);
	  break;
      
	case cm_function_def:
	  print_function_def (command->value.Function_def);
	  break;

	case cm_group:
	  print_group_command (command->value.Group);
	  break;

	default:
	  programming_error ("OOPS!  Bad command type `%d'!", command->type);
	  break;
	}

      if (command->flags & CMD_WANT_SUBSHELL)
	cprintf (" )");

      if (command->redirects)
	print_redirection_list (command->redirects);
    }
}

print_word_list (list, separator)
     WORD_LIST *list;
     char *separator;
{
  while (list)
    {
      printf ("%s", list->word->word);
      list = list->next;
      if (list)
	printf ("%s", separator);
    }
}

command_print_word_list (list, separator)
     WORD_LIST *list;
     char *separator;
{
  while (list)
    {
      cprintf ("%s", list->word->word);
      list = list->next;
      if (list)
	cprintf ("%s", separator);
    }
}

print_for_command (for_command)
     FOR_COM *for_command;
{
  cprintf ("for %s in ", for_command->name->word);
  command_print_word_list (for_command->map_list, " ");
  cprintf (";");
  newline ("do\n");
  indentation += indentation_amount;
  make_command_string_internal (for_command->action);
  cprintf (";");
  indentation -= indentation_amount;
  newline ("done");
}

print_group_command (group_command)
     GROUP_COM *group_command;
{
  cprintf ("{ ");

  if (!inside_function_def)
    skip_this_indent++;
  else
    cprintf ("\n");

  make_command_string_internal (group_command->command);

  if (inside_function_def)
#if 0
    cprintf ("; \n}");
#else
    cprintf ("\n}");
#endif
  else
    cprintf (" }");
}

print_case_command (case_command)
     CASE_COM *case_command;
{
  cprintf ("case %s in ", case_command->word->word);
  if (case_command->clauses)
    print_case_clauses (case_command->clauses);
  newline ("esac");
}

print_case_clauses (clauses)
     PATTERN_LIST *clauses;
{
  indentation += indentation_amount;
  while (clauses)
    {
      newline ("");
      command_print_word_list (clauses->patterns, " | ");
      cprintf (")\n");
      indentation += indentation_amount;
      make_command_string_internal (clauses->action);
      indentation -= indentation_amount;
      newline (";;");
      clauses = clauses->next;
    }
  indentation -= indentation_amount;
}

print_while_command (while_command)
     WHILE_COM *while_command;
{
  print_until_or_while (while_command, "while");
}

print_until_command (while_command)
     WHILE_COM *while_command;
{
  print_until_or_while (while_command, "until");
}

print_until_or_while (while_command, which)
     WHILE_COM *while_command;
     char *which;
{
  cprintf ("%s ", which);
  skip_this_indent++;
  make_command_string_internal (while_command->test);
  cprintf (";");
  newline ("do\n");
  indentation += indentation_amount;
  make_command_string_internal (while_command->action);
  indentation -= indentation_amount;
  cprintf (";");
  newline ("done");
}

print_if_command (if_command)
     IF_COM *if_command;
{
  cprintf ("if ");
  skip_this_indent++;
  make_command_string_internal (if_command->test);
  cprintf (" ; then\n");
  indentation += indentation_amount;
  make_command_string_internal (if_command->true_case);
  indentation -= indentation_amount;

  if (if_command->false_case)
    {
      cprintf (";");
      newline ("else\n");
      indentation += indentation_amount;
      make_command_string_internal (if_command->false_case);
      indentation -= indentation_amount;
    }
  cprintf (" ;");
  newline ("fi");
}

print_simple_command (simple_command)
     SIMPLE_COM *simple_command;
{
  command_print_word_list (simple_command->words, " ");

  if (simple_command->redirects)
    {
      cprintf (" ");
      print_redirection_list (simple_command->redirects);
    }
}

print_redirection_list (redirects)
     REDIRECT *redirects;
{
  while (redirects)
    {
      print_redirection (redirects);
      cprintf (" ");
      redirects = redirects->next;
    }
}

print_redirection (redirect)
     REDIRECT *redirect;
{
  int kill_leading = 0;
  int redirector = redirect->redirector;
  WORD_DESC *redirectee = redirect->redirectee.filename;

  switch (redirect->instruction)
    {
    case r_output_direction:
      if (redirector != 1)
	cprintf ("%d", redirector);
      cprintf (">%s", redirectee->word);
      break;

    case r_input_direction:
      if (redirector != 0)
	cprintf ("%d", redirector);
      cprintf ("<%s", redirectee->word);
      break;

    case r_inputa_direction:	/* Redirection created by the sh. */
      cprintf ("&");
      break;

    case r_appending_to:
      if (redirector != 1)
	cprintf ("%d", redirector);
      cprintf (">>%s", redirectee->word);
      break;

    case r_deblank_reading_until:
      kill_leading++;
      /* ... */
    case r_reading_until:
      if (redirector != 0)
	cprintf ("%d", redirector);
      cprintf ("<<%s%s", kill_leading? "-" : "", redirect->here_doc_eof);
      /* This is only important if we are printing a function definition
	 for export.  If not, we could overrun the buffer that `cprintf'
	 writes into, quite unexpectedly -- when unpacking shar files, for
	 instance. */
      if (inside_function_def)
	{
	  cprintf ("\n");
	  cprintf ("%s%s", redirect->redirectee.filename->word,
			   redirect->here_doc_eof);
	}
      break;

    case r_duplicating_input:
      cprintf ("%d<&%d", redirector, (int)redirectee);
      break;

    case r_duplicating_output:
      cprintf ("%d>&%d", redirector, (int)redirectee);
      break;

    case r_duplicating_input_word:
      cprintf ("%d<&%s", redirector, redirectee->word);
      break;

    case r_duplicating_output_word:
      cprintf ("%d>&%s", redirector, redirectee->word);
      break;

    case r_close_this:
      cprintf ("%d>&-", redirector);
      break;

    case r_err_and_out:
      cprintf (">&%s", redirectee->word);
      break;

    case r_input_output:
      if (redirector != 1)
	cprintf ("%d", redirector);
      cprintf ("<>%s", redirectee->word);
      break;

    case r_output_force:
      if (redirector != 1)
	cprintf ("%d", redirector);
      cprintf (">|%s", redirectee->word);
      break;
    }
}

static void
reset_locals ()
{
  inside_function_def = 0;
  indentation = 0;
}

print_function_def (func)
     FUNCTION_DEF *func;
{
  cprintf ("function %s () \n", func->name->word);
  add_unwind_protect (reset_locals, 0);
  inside_function_def++;
  skip_this_indent++;
  indentation += indentation_amount;
  make_command_string_internal (func->command);
  remove_unwind_protect ();
  indentation -= indentation_amount;
  inside_function_def--;
}

/* Return the string representation of the named function.
   NAME is the name of the function.
   COMMAND is the function body.  It should be a GROUP_COM.
   MULTI_LINE is non-zero to pretty-print, or zero for all on one line.
  */
char *
named_function_string (name, command, multi_line)
     char *name;
     COMMAND *command;
     int multi_line;
{
  char *result;
  int old_indent = indentation, old_amount = indentation_amount;

  command_string_index = 0;

  if (name && *name)
    cprintf ("%s ", name);

  cprintf ("() ");

  if (!multi_line)
    {
      indentation = 1;
      indentation_amount = 0;
    }
  else
    {
      cprintf ("\n");
      indentation += indentation_amount;
    }

  inside_function_def++;
  skip_this_indent++;
  make_command_string_internal (command);

  indentation = old_indent;
  indentation_amount = old_amount;
  inside_function_def--;

  result = the_printed_command;

  if (!multi_line)
    {
#if 0
      register int i;
      for (i = 0; result[i]; i++)
	if (result[i] == '\n')
	  {
	    strcpy (&result[i], &result[i + 1]);
	    --i;
	  }
#else
      if (result[2] == '\n')	/* XXX -- experimental */
        strcpy (&result[2], &result[3]);
#endif
    }

  return (result);
}

newline (string)
     char *string;
{
  cprintf ("\n");
  indent (indentation);
  cprintf ("%s", string);
}

indent (amount)
     int amount;
{
  while (amount-- > 0)
    cprintf (" ");
}

#if !defined (HAVE_VFPRINTF)
/* How to make the string. */
cprintf (control, arg1, arg2, arg3, arg4, arg5)
     char *control;
{
  char temp_buffer[5000];
  int l;

  sprintf (temp_buffer, control, arg1, arg2, arg3, arg4, arg5);
  l = strlen (temp_buffer);

  if (!the_printed_command)
    the_printed_command =
      (char *)xmalloc (the_printed_command_size = PRINTED_COMMAND_GROW_SIZE);

  while (the_printed_command_size <= command_string_index + l)
    the_printed_command = (char *)
      xrealloc (the_printed_command,
		the_printed_command_size += PRINTED_COMMAND_GROW_SIZE);

  strcpy (the_printed_command + command_string_index, temp_buffer);
  command_string_index += l;
}

#else /* We have support for varargs. */

/* How to make the string. */
cprintf (va_alist)
     va_dcl
{
  char temp_buffer[5000];
  int l;
  char *control;
  va_list args;

  va_start (args);
  control = va_arg (args, char *);
  vsprintf (temp_buffer, control, args);
  va_end (args);
  l = strlen (temp_buffer);

  if (!the_printed_command)
    the_printed_command =
      (char *)xmalloc (the_printed_command_size = PRINTED_COMMAND_GROW_SIZE);

  while (the_printed_command_size <= command_string_index + l)
    the_printed_command = (char *)
      xrealloc (the_printed_command,
		the_printed_command_size += PRINTED_COMMAND_GROW_SIZE);

  strcpy (the_printed_command + command_string_index, temp_buffer);
  command_string_index += l;
}
#endif /* HAVE_VFPRINTF */
