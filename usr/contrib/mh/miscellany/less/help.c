#include  "less.h"

/*
 * Display some help.
 * Help is in two pages.
 */
	static void
help0()
{
	puts("f, SPACE       Forward one screen.\n");
	puts("b              Backward one screen.\n");
	puts("e, j, CR    *  Forward N lines, default 1.\n");
	puts("y, k        *  Backward N lines, default 1.\n");
	puts("d           *  Forward N lines, default 10 or last N to d or u command.\n");
	puts("u           *  Backward N lines, default 10 or last N to d or u command.\n");
	puts("r              Repaint screen.\n");
	puts("g           *  Go to line N, default 1.\n");
	puts("G           *  Like g, but default is last line in file.\n");
	puts("=              Print current file name\n");
	puts("/pattern    *  Search forward for N-th occurence of pattern.\n");
	puts("?pattern    *  Search backward for N-th occurence of pattern.\n");
	puts("n           *  Repeat previous search (for N-th occurence).\n");
	puts("q              Exit.\n");
	error("More help...");
}

	static void
help1()
{
	char message[100];
	extern char all_options[];

	puts("R              Repaint screen, discarding buffered input.\n");
	puts("p, %        *  Position to N percent into the file.\n");
	puts("m<letter>      Mark the current position with <letter>.\n");
	puts("'<letter>      Return to a previously marked position.\n");
	sprintf(message, 
	     "-X             Toggle a flag (X may be one of \"%s\").\n", 
				all_options);
	puts(message);
	puts("E [file]       Examine a new file.\n");
	puts("N              Examine the next file (from the command line).\n");
	puts("P              Examine the previous file (from the command line).\n");
	puts("V              Print version number.\n");
#if SHELL_ESCAPE
	puts("!command       Passes the command to a shell to be executed.\n");
#endif
#if EDITOR
	sprintf(message,
	     "v              Edit the current file with $EDITOR (default %s).\n",
				EDIT_PGM);
	puts(message);
#endif
	error("");
}

	public void
help()
{
	register int i;

	for (i = 0;  i < 2;  i++)
	{
		clear();
		puts("Commands marked with * may be preceeded by a number, N.\n\n");

		switch (i)
		{
		case 0:		help0();	break;
		case 1:		help1();	break;
		}
	}
}
