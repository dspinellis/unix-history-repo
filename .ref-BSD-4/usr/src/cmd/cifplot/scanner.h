
int CharNo;	/* in_buf[CharNo] is the current char being read */
int LineNo;			/* Current line number */
int in_buf_size;		/* Current allocation for in_buf */
char *in_buf;			/* Buffer holding current input line */
char *in_store;			/* Buffer to store previous line */

char *CurrentFile;		/* Name of current input file */
char *file1;

int maxlines;
int OldLength;		/* Length of previous line of input */


int FStackPtr;
char *FileName[FStackSize];
FILE *in_file[FStackSize];

int argb,argc;
char **argv;

