/*
 * Prompting and other messages.
 * There are three flavors of prompts, SHORT, MEDIUM and LONG,
 * selected by the -m/-M options.
 * A prompt is either a colon or a message composed of various
 * pieces, such as the name of the file being viewed, the percentage
 * into the file, etc.
 */

#include "less.h"
#include "position.h"

extern int pr_type;
extern int ispipe;
extern int hit_eof;
extern int new_file;
extern int sc_width;
extern char current_file[];
extern int ac;
extern char **av;
extern int curr_ac;

static char message[500];

/*
 * Append the name of the current file (to the message buffer).
 */
	static void
ap_filename()
{
	if (!ispipe)
		sprintf(message + strlen(message), 
			"%s", current_file);
}

/*
 * Append the "file N of M" message.
 */
	static void
ap_of()
{
	if (ac > 1)
		sprintf(message + strlen(message), 
			" (file %d of %d)", curr_ac+1, ac);
}

/*
 * Append the byte offset into the current file.
 */
	static void
ap_byte()
{
	POSITION pos, len;

	pos = position(BOTTOM_PLUS_ONE);
	if (pos != NULL_POSITION)
	{
		sprintf(message + strlen(message), 
			" byte %ld", pos);
		len = ch_length();
		if (len > 0)
			sprintf(message + strlen(message), 
				"/%ld", len);
	}
}

/*
 * Append the percentage into the current file.
 * If we cannot find the percentage and must_print is true,
 * the use the byte offset.
 */
	static void
ap_percent(must_print)
{
	POSITION pos,len;

	pos = position(BOTTOM_PLUS_ONE);
	len = ch_length();
	if (len > 0 && pos != NULL_POSITION)
		sprintf(message + strlen(message),
			" (%ld%%)", (100 * pos) / len);
	else if (must_print)
		ap_byte();
}

/*
 * Append the end-of-file message.
 */
	static void
ap_eof()
{
	strcat(message, " END");
	if (curr_ac + 1 < ac)
		sprintf(message + strlen(message),
			" - Next: %s", av[curr_ac+1]);
}

/*
 * Return a message suitable for printing by the "=" command.
 */
	public char *
eq_message()
{
	message[0] = '\0';
	ap_filename();
	ap_of();
	ap_byte();
	ap_percent(0);
	/*
	 * Truncate to the screen width.
	 * {{ This isn't very nice. }}
	 */
	message[error_width()] = '\0';
	return (message);
}

/*
 * Return a prompt.
 * This depends on the prompt type (SHORT, MEDIUM, LONG), etc.
 * If we can't come up with an appropriate prompt, return NULL
 * and the caller will prompt with a colon.
 */
	public char *
pr_string()
{
	message[0] = '\0';
	switch (pr_type)
	{
	case PR_SHORT:
		if (new_file)
		{
			ap_filename();
			ap_of();
		}
		if (hit_eof)
			ap_eof();
		break;
	case PR_MEDIUM:
		if (new_file)
		{
			ap_filename();
			ap_of();
		}
		if (hit_eof)
			ap_eof();
		else
			ap_percent(1);
		break;
	case PR_LONG:
		ap_filename();
		if (new_file)
			ap_of();
		ap_byte();
		if (hit_eof)
			ap_eof();
		else
			ap_percent(0);
		break;
	}
	new_file = 0;
	if (message[0] == '\0')
		return (NULL);
	/*
	 * Truncate to the screen width.
	 * {{ This isn't very nice. }}
	 */
	message[sc_width-2] = '\0';
	return (message);
}
