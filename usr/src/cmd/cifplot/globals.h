char *programName;	/* Name of program; appears on plot */
int debug;		/* If zero no debugging information is output */
int outline;		/* One if outlines around symbols */
int circle;		/* Zero if circle, otherwise number of edges in
			 * polygon approximation */
int standard;		/* One if only standard CIF is accepted */
int list;		/* One if a listing of the input is to be produced */
int output;		/* Indicates which output device is to be used */
char *banner;		/* Points to string which is to be put at top of plot */
int fileopen;		/* One if output file is open */
int plot;		/* One if plot is to be made, zero to suppress */
int extractor;		/* Set if running extractor */
char *baseName;		/* base name for extractor to use */
extern char *fontfile;	/* Name of font file in '/usr/lib/vfont' to use for text */
int NoPixcels;		/* Number of pixcels on raster line */
int TextUp,TextDown;	/* Maximum extent of the text fonts above and below the base line */
int text;		/* one if text is to be plotted */
int printSymbolName;	/* one if symbol names are to be plotted */
int background;		/* one if the program is running in the background */
int symbox;		/* one if bounding boxes are to be drawn around symbols */
int grid; 		/* spacing in CIF units between grid lines; 0 means no grid lines */
int ask;		/* 1 if should ask whether to plot;0 otherwise*/
int depth;		/* depth to instanciate symbols; 0 = infinity */
int SetScale;		/* one if user has set the scale */
double scale;		/* the scale of the plot */

char *outfile;

int RetCmt;
int upToDate;		/* one if LineNo and CharNo represent the beginning of token
			   just sent to the parser */
int TokenLine,TokenChar; /* The line num & char num of token sent to parser */

typedef char *string;
int MoreToPlot;		/* Indicates whether there is another window to plot */

int SetStick;		/* for STIF plot wires constant width */
float Stickw;		/* for STIF the width of wires */
