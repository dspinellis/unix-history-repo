/* structure declarations used by dipress */

struct ifont
{
    char name[10];		/* troff name */
    char *uname;		/* interpress universal name */
    char *frames;		/* frame number array -- indexed by size */
				/* frame_num == NULL if font never used */
    unsigned short *extab;	/* table of extended character codes */
				/* == NULL if extab not loaded */
    struct ifont *next;		/* next font on inactive list */
};


struct state
{
	int	ssize;
	int	sfont;
	int	shpos;
	int	svpos;
	int	shorig;
	int	svorig;
};
