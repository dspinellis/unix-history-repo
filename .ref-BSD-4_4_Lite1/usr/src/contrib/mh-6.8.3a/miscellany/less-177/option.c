/*
 * Process command line options.
 *
 * Each option is a single letter which controls a program variable.
 * The options have defaults which may be changed via
 * the command line option, toggled via the "-" command, 
 * or queried via the "_" command.
 */

#include "less.h"
#include "option.h"

static struct option *pendopt;
public int plusoption;

static char *propt();
static char *optstring();
static int flip_triple();

extern int screen_trashed;
extern char *every_first_cmd;

/* 
 * Scan an argument (either from the command line or from the 
 * LESS environment variable) and process it.
 */
	public void
scan_option(s)
	char *s;
{
	register struct option *o;
	register int c;
	char *str;
	int set_default;
	PARG parg;

	if (s == NULL)
		return;

	/*
	 * If we have a pending string-valued option, handle it now.
	 * This happens if the previous option was, for example, "-P"
	 * without a following string.  In that case, the current
	 * option is simply the string for the previous option.
	 */
	if (pendopt != NULL)
	{
		(*pendopt->ofunc)(INIT, s);
		pendopt = NULL;
		return;
	}

	set_default = 0;

	while (*s != '\0')
	{
		/*
		 * Check some special cases first.
		 */
		switch (c = *s++)
		{
		case ' ':
		case '\t':
		case END_OPTION_STRING:
			continue;
		case '-':
			/*
			 * "-+" means set these options back to their defaults.
			 * (They may have been set otherwise by previous 
			 * options.)
			 */
			if (set_default = (*s == '+'))
				s++;
			continue;
		case '+':
			/*
			 * An option prefixed by a "+" is ungotten, so 
			 * that it is interpreted as less commands 
			 * processed at the start of the first input file.
			 * "++" means process the commands at the start of
			 * EVERY input file.
			 */
			plusoption = 1;
			if (*s == '+')
				every_first_cmd = save(++s);
			else
				ungetsc(s);
			s = optstring(s, c);
			continue;
		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
			/*
			 * Special "more" compatibility form "-<number>"
			 * instead of -z<number> to set the scrolling 
			 * window size.
			 */
			s--;
			c = 'z';
			break;
		}

		/*
		 * Not a special case.
		 * Look up the option letter in the option table.
		 */
		o = findopt(c);
		if (o == NULL)
		{
			parg.p_string = propt(c);
			error("There is no %s flag (\"less -\\?\" for help)",
				&parg);
			quit(1);
		}

		switch (o->otype & OTYPE)
		{
		case BOOL:
			if (set_default)
				*(o->ovar) = o->odefault;
			else
				*(o->ovar) = ! o->odefault;
			break;
		case TRIPLE:
			if (set_default)
				*(o->ovar) = o->odefault;
			else
				*(o->ovar) = flip_triple(o->odefault,
						(o->oletter == c));
			break;
		case STRING:
			if (*s == '\0')
			{
				/*
				 * Set pendopt and return.
				 * We will get the string next time
				 * scan_option is called.
				 */
				pendopt = o;
				return;
			}
			/*
			 * Don't do anything here.
			 * All processing of STRING options is done by 
			 * the handling function.
			 */
			str = s;
			s = optstring(s, c);
			break;
		case NUMBER:
			*(o->ovar) = getnum(&s, c, (int*)NULL);
			break;
		}
		/*
		 * If the option has a handling function, call it.
		 */
		if (o->ofunc != NULL)
			(*o->ofunc)(INIT, str);
	}
}

/*
 * Toggle command line flags from within the program.
 * Used by the "-" and "_" commands.
 * how_toggle may be:
 *	OPT_NO_TOGGLE	just report the current setting, without changing it.
 *	OPT_TOGGLE	invert the current setting
 *	OPT_UNSET	set to the default value
 *	OPT_SET		set to the inverse of the default value
 */
	public void
toggle_option(c, s, how_toggle)
	int c;
	char *s;
	int how_toggle;
{
	register struct option *o;
	register int num;
	int err;
	PARG parg;

	/*
	 * Look up the option letter in the option table.
	 */
	o = findopt(c);
	if (o == NULL)
	{
		parg.p_string = propt(c);
		error("There is no %s flag", &parg);
		return;
	}

	if (how_toggle == OPT_TOGGLE && (o->otype & NO_TOGGLE))
	{
		parg.p_string = propt(c);
		error("Cannot change the %s flag", &parg);
		return;
	} 

	/*
	 * Check for something which appears to be a do_toggle
	 * (because the "-" command was used), but really is not.
	 * This could be a string option with no string, or
	 * a number option with no number.
	 */
	switch (o->otype & OTYPE)
	{
	case STRING:
	case NUMBER:
		if (how_toggle == OPT_TOGGLE && *s == '\0')
			how_toggle = OPT_NO_TOGGLE;
		break;
	}

	/*
	 * Now actually toggle (change) the variable.
	 */
	if (how_toggle != OPT_NO_TOGGLE)
	{
		switch (o->otype & OTYPE)
		{
		case BOOL:
			/*
			 * Boolean.
			 */
			switch (how_toggle)
			{
			case OPT_TOGGLE:
				*(o->ovar) = ! *(o->ovar);
				break;
			case OPT_UNSET:
				*(o->ovar) = o->odefault;
				break;
			case OPT_SET:
				*(o->ovar) = ! o->odefault;
				break;
			}
			break;
		case TRIPLE:
			/*
			 * Triple:
			 *	If user gave the lower case letter, then switch 
			 *	to 1 unless already 1, in which case make it 0.
			 *	If user gave the upper case letter, then switch
			 *	to 2 unless already 2, in which case make it 0.
			 */
			switch (how_toggle)
			{
			case OPT_TOGGLE:
				*(o->ovar) = flip_triple(*(o->ovar), 
						o->oletter == c);
				break;
			case OPT_UNSET:
				*(o->ovar) = o->odefault;
				break;
			case OPT_SET:
				*(o->ovar) = flip_triple(o->odefault,
						o->oletter == c);
				break;
			}
			break;
		case STRING:
			/*
			 * String: don't do anything here.
			 *	The handling function will do everything.
			 */
			switch (how_toggle)
			{
			case OPT_SET:
			case OPT_UNSET:
				error("Can't use \"-+\" or \"--\" for a string flag",
					NULL_PARG);
				return;
			}
			break;
		case NUMBER:
			/*
			 * Number: set the variable to the given number.
			 */
			switch (how_toggle)
			{
			case OPT_TOGGLE:
				num = getnum(&s, '\0', &err);
				if (!err)
					*(o->ovar) = num;
				break;
			case OPT_UNSET:
				*(o->ovar) = o->odefault;
				break;
			case OPT_SET:
				error("Can't use \"--\" for a numeric flag",
					NULL_PARG);
				return;
			}
			break;
		}
	}

	/*
	 * Call the handling function for any special action 
	 * specific to this option.
	 */
	if (o->ofunc != NULL)
		(*o->ofunc)((how_toggle==OPT_NO_TOGGLE) ? QUERY : TOGGLE, s);

	/*
	 * Print a message describing the new setting.
	 */
	switch (o->otype & OTYPE)
	{
	case BOOL:
	case TRIPLE:
		/*
		 * Print the odesc message.
		 */
		error(o->odesc[*(o->ovar)], NULL_PARG);
		break;
	case NUMBER:
		/*
		 * The message is in odesc[1] and has a %d for 
		 * the value of the variable.
		 */
		parg.p_int = *(o->ovar);
		error(o->odesc[1], &parg);
		break;
	case STRING:
		/*
		 * Message was already printed by the handling function.
		 */
		break;
	}

	if (how_toggle != OPT_NO_TOGGLE && (o->otype & REPAINT))
		screen_trashed = 1;
}

/*
 * "Toggle" a triple-valued option.
 */
	static int
flip_triple(val, lc)
	int val;
	int lc;
{
	if (lc)
		return ((val == 1) ? 0 : 1);
	else
		return ((val == 2) ? 0 : 2);
}

/*
 * Return a string suitable for printing as the "name" of an option.
 * For example, if the option letter is 'x', just return "-x".
 */
	static char *
propt(c)
	int c;
{
	static char buf[8];

	sprintf(buf, "-%s", prchar(c));
	return (buf);
}

/*
 * Determine if an option is a single character option (BOOL or TRIPLE),
 * or if it a multi-character option (NUMBER).
 */
	public int
single_char_option(c)
	int c;
{
	register struct option *o;

	o = findopt(c);
	if (o == NULL)
		return (1);
	return (o->otype & (BOOL|TRIPLE|NOVAR|NO_TOGGLE));
}

/*
 * Return the prompt to be used for a given option letter.
 * Only string and number valued options have prompts.
 */
	public char *
opt_prompt(c)
	int c;
{
	register struct option *o;

	o = findopt(c);
	if (o == NULL || (o->otype & (STRING|NUMBER)) == 0)
		return (NULL);
	return (o->odesc[0]);
}

/*
 * Return whether or not there is a string option pending;
 * that is, if the previous option was a string-valued option letter 
 * (like -P) without a following string.
 * In that case, the current option is taken to be the string for
 * the previous option.
 */
	public int
isoptpending()
{
	return (pendopt != NULL);
}

/*
 * Print error message about missing string.
 */
	static void
nostring(c)
	int c;
{
	PARG parg;
	parg.p_string = propt(c);
	error("String is required after %s", &parg);
}

/*
 * Print error message if a STRING type option is not followed by a string.
 */
	public void
nopendopt()
{
	nostring(pendopt->oletter);
}

/*
 * Scan to end of string or to an END_OPTION_STRING character.
 * In the latter case, replace the char with a null char.
 * Return a pointer to the remainder of the string, if any.
 */
	static char *
optstring(s, c)
	char *s;
	int c;
{
	register char *p;

	if (*s == '\0')
	{
		nostring(c);
		quit(1);
	}
	for (p = s;  *p != '\0';  p++)
		if (*p == END_OPTION_STRING)
		{
			*p = '\0';
			return (p+1);
		}
	return (p);
}

/*
 * Translate a string into a number.
 * Like atoi(), but takes a pointer to a char *, and updates
 * the char * to point after the translated number.
 */
	public int
getnum(sp, c, errp)
	char **sp;
	int c;
	int *errp;
{
	register char *s;
	register int n;
	register int neg;
	PARG parg;

	s = skipsp(*sp);
	neg = 0;
	if (*s == '-')
	{
		neg = 1;
		s++;
	}
	if (*s < '0' || *s > '9')
	{
		if (errp != NULL)
		{
			*errp = 1;
			return (-1);
		}
		parg.p_string = propt(c);
		error("Number is required after %s", &parg);
		quit(1);
	}

	n = 0;
	while (*s >= '0' && *s <= '9')
		n = 10 * n + *s++ - '0';
	*sp = s;
	if (errp != NULL)
		*errp = 0;
	if (neg)
		n = -n;
	return (n);
}
