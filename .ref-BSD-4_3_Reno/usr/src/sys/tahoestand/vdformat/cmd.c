/*	cmd.c	1.2	86/11/04	*/

#include	"vdfmt.h"
#include	"cmd.h"

#define TRUE	1
#define FALSE	0

#define	HELP	1
#define	STATUS	2
#define	KILL	3
#define	KQUIT	4

static cmd_text_element	primary[] = {
	{ STATUS,	"!",		"" },
	{ HELP,		"HElp",		"" },
	{ KILL,		"KILL",		"" },
	{ KQUIT,	"QUIT",		"" },
	{ STATUS,	"STATus",	"" },
	{ HELP,		"?",		"" },
	{ 0,		"",		"" }
};


/*
*/

boolean confirm(token)
int	token;
{
	char	*action;
	char	query[50];

	if(token == KILL)
		action = "kill";
	else
		action = "quit";
	sprintf(query, "Confirm %s operations", action);
	return get_yes_no(query);
}


/*
**
*/

get_text_cmd(table, tokens)
cmd_text_element	*table;
int			*tokens;
{
	extern boolean	get_yes_no();
	int		*t_ptr;
	char		line[133];

	agets(line);
	/* Check for help, status, or kill */
	cmd_parse(primary, line, tokens);
	t_ptr = tokens;
	while(*t_ptr) {
		switch (*t_ptr) {
			case STATUS :
				cmd_status();
				break;
			case KQUIT :
			case KILL :
				if(confirm(*t_ptr) == true) {
					kill_processes = true;
					return 0;
				}
				break;
			default:
				help_text(table);
				break;
		}
		t_ptr++;
	}
	/* Now parse all the operator's commands */
	cmd_parse(table, line, tokens);
	return strlen(line);
}


/*
**
*/

cmd_intcmp(a, b)
int	*a, *b;
{
	if(*a==*b)
		return 0;
	if(*a<*b)
		return -1;
	return 1;
}


/*
**
*/

condition_list(tokens, sentinal)
int	*tokens, sentinal;
{
	register int	*t_ptr = tokens;
	register int	num_tok;

	for(num_tok=0; *t_ptr++ != sentinal; num_tok++)
		;	
	qsort(tokens, num_tok, sizeof(int), cmd_intcmp);
	/* compress out dups */
	while(*tokens != sentinal) {
		if(*tokens == *(tokens+1)) {
			for(t_ptr=tokens+1; *t_ptr != sentinal; t_ptr++) {
				*t_ptr = *(t_ptr+1);
			}
			continue;
		}
		tokens++;
	}
}


/*
**
*/

cmd_parse(table, line, tokens) 
cmd_text_element	*table;
char			*line;
int			*tokens;
{
	char		*seperators = "\t ,.;:-~+/\\";
	register char	*tok_start;
	register int	*tok = tokens;
	char		save_buf[133];

	strcpy(save_buf, line);
	tok_start = (char *)strtok((char *)save_buf, seperators);
	while(tok_start != NULL) {
		if(strlen(tok_start)) {
			if(*tok = cmd_search(table, tok_start)) {
				tok++;
			}
		}
		tok_start = (char *)strtok((char *)NULL, seperators);
	}
	*tok = 0;
	condition_list(tokens, 0);
}


/*
**
*/

cmd_search(table, command)
cmd_text_element	*table;
char			*command;
{
	register char	*tbl_ptr;
	register char	*cmd_ptr;

	while(table->cmd_token != 0) {
		cmd_ptr = command;
		tbl_ptr = table->cmd_text;
		while(ismustmatch(*tbl_ptr)) {
			if(toupper(*cmd_ptr) != *tbl_ptr)
				break;
			cmd_ptr++;
			tbl_ptr++;
		}
		if((*tbl_ptr == 0) || !ismustmatch(*tbl_ptr))
			return table->cmd_token;
		table++;
	}
	return 0;
}


/*
**
*/

is_in_digit_table(table, token)
int	*table, token;
{
	while(*table != -1) {
		if(token == *table)
			return TRUE;
		table++;
	}
	return FALSE;
}


/*
**
*/

int *fill_in(tokens, table, start, end)
int	*tokens, *table, start, end;
{

	if(start > end) {
		register int temp = end;

		end = start;
		start = temp;
	}
	while((*table != -1) && (*table < start))
		table++;
	while((*table != -1) && (*table <= end)) {
		*tokens++ = *table++;
	}
	return tokens;
}

/*
**
*/

get_digit_list(tokens, table, help)
int	*tokens, *table, (*help)();
{
	int		*tok_ptr;
	char		*ptr, line[133];

	condition_list(table, -1);
	agets(line);
	if(!line[0]) {
		*tokens = -1;
		return;
	}
	/* Check for help, status, or kill */
	cmd_parse(primary, line, tokens);
	tok_ptr = tokens;
	while(*tok_ptr) {
		switch (*tok_ptr) {
			case STATUS :
				cmd_status();
				break;
			case KQUIT :
			case KILL :
				if(confirm(*tok_ptr)) {
					kill_processes = true;
					return;
				}
				break;
			default:
				(help)();
				break;
		}
		tok_ptr++;
	}
	tok_ptr = tokens;
	ptr = line;
	while(*ptr) {
		finddigit(ptr);
		if(sscanf(ptr, "%d", tok_ptr) > 0) {
			skipdigits(ptr);
			skip_junk(ptr);
			if((*ptr == '~') || (*ptr == '-')) {
				register int	start = *tok_ptr;

				finddigit(ptr);
				if(sscanf(ptr, "%d", tok_ptr) > 0) {
					skipdigits(ptr);
					tok_ptr = fill_in(tok_ptr,
					    table, start, *tok_ptr);
					continue;
				}
				else
					*tok_ptr = start;
			}
			if(is_in_digit_table(table, *tok_ptr))
				tok_ptr++;
		}
	}
	*tok_ptr = -1;
	condition_list(tokens, -1);
}



/*
**
*/

get_digit_cmd(help)
int	(*help)();
{
	int	tokens[20], *t_ptr;
	char	line[80];
	int	results;

	agets(line);
	if(!*line)
		return -1;
	/* Check for help, status, or kill */
	cmd_parse(primary, line, tokens);
	t_ptr = tokens;
	while(*t_ptr) {
		switch (*t_ptr) {
			case STATUS :
				cmd_status();
				break;
			case KQUIT :
			case KILL :
				if(confirm(*t_ptr)) {
					kill_processes = true;
					return -1;
				}
				break;
			default:
				(*help)();
				break;
		}
		t_ptr++;
	}
	if(sscanf(line, "%d", &results) > 0)
		return results;
	return -1;
}


/*
**
*/

get_string_cmd(line, help)
char	*line;
int	(*help)();
{
	int	tokens[20], *t_ptr;

	agets(line);
	if(!*line)
		return;
	/* Check for help, status, or kill */
	cmd_parse(primary, line, tokens);
	t_ptr = tokens;
	while(*t_ptr) {
		switch (*t_ptr) {
			case STATUS :
				cmd_status();
				break;
			case KQUIT :
			case KILL :
				if(confirm(*t_ptr)) {
					kill_processes = true;
					return;
				}
				break;
			default:
				(*help)();
				break;
		}
		t_ptr++;
	}
	while(*line) {
		*line = tolower(*line);
		line++;
	}
	return;
}


/*
**
*/

cmd_status()
{
	indent();
	switch (cur.state) {
		case cmd :
			print("Waiting for operator input.\n\n");
			break;
		default :
			status();
			break;
	}
	exdent(1);
}


/*
** 	Vdget_yes_no is used to ask simple yes or no questions.  The question
** prompt is supplied by the caller,  The question mark, possible responses,
** and the default response is printed at the end of the prompt.  The routine
** then reads the answer and returns a 1 if a 'y' is typed or no response was
** given, otherwise, a zero is returned.
*/

boolean get_yes_no(str)
register char	*str;
{
	extern int	wait_for_char;
	char		answer[80];
	boolean		retval;
	int		old_wait_status = wait_for_char;

	wait_for_char = 1;
	for(;;) {
		if(*str)
			print("%s", str);
		printf("? [Yes/No] ");
		agets(answer);
		if((answer[0] == 'Y') || (answer[0] == 'y')) {
			retval = true;
			break;
		}
		if((answer[0] == 'N') || (answer[0] == 'n')) {
			retval = false;
			break;
		}
		print("\n");
		print("A 'Yes' or 'No' must be entered!\n\n");
	}
	wait_for_char = old_wait_status;
	return retval;
}


/*
**
*/

get_next_digit(ptr)
char	*ptr;
{
	int	results;

	finddigit(ptr);
	if(sscanf(ptr, "%d", &results) <= 0)
		return -1;
	return results;
}
