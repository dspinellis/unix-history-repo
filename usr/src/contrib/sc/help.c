/*
 * Help functions for sc 
 * R. Bond, 1988
 * $Revision: 6.8 $
 */

#include <curses.h>
#include "sc.h"

char *intro[] = {
" ",
" Overview:",
" ",
" A:   This overview",
" B:   Options",
" C:   Cursor movement commands",
" D:   Cell entry and editing commands",
" E:   Line Editing",
" F:   File commands",
" G:   Row and column commands",
" H:   Range commands",
" I:   Miscellaneous commands",
" J:   Variable names/Expressions",
" K:   Range functions",
" L:   Numeric functions",
" M:   String functions",
" N:   Financial functions",
" O:   Time and date functions",
" ",
" Q:   Return to main spreadsheet",
(char *)0
};

char *options[] = {
" ",
" B: Options",
" ",
"     ^To  Toggle options. Toggle one option selected by o:",
" ",
"          a    Recalculate automatically or on ``@'' commands.",
"          c    Current cell highlighting enable/disable.",  
"          e    External function execution enable/disable.",
"          n    If enabled, a digit starts a numeric value.",
"          t    Top line display enable/disable.",
"          x    Encrypt/decrypt database and listing files.",
"          $    Dollar prescale.  If enabled, all numeric constants.",
"               (not expressions) entered are multipled by 0.01.",
" ",
"     S    Set options.  Options include:",
" ",
"          byrows        Recalculate in row order. (default)",
"          bycols        Recalculate in column order.",
"          iterations=n  Set the number of iterations allowed. (10)",
"          tblstyle=xx   Set ``T'' output style to:",
"                        0 (none), tex, latex, or tbl.",
(char *)0
};

char *cursor[] = {
" ",
" C: Cell cursor movement (always OK):",
" ",
"     ^N ^P ^B ^F Down, up, back, forward",
"     ^Ed         Go to end of range.  Follow ^E by a direction indicator",
"                 such as ^P or j.",
"     Arrow keys (if the terminal and termcap support them.)",
" ",
" Cell cursor movement if no prompt active:",
"     j,k,l,h    Down, up, right, left",
"     SPACE      Forward",
"     ^H         Back",
"     TAB        Forward, otherwise starts/ends a range",
"     ^          Up to row 0 of the current column.",
"     #          Down to the last valid row of the current column.",
"     0          Back to column A.  Preface with ^U if numeric mode.",
"     $          Forward to the last valid column of the current row.",
"     b          Back then up to the previous valid cell.",
"     w          Forward then down to the next valid cell.",
"     g          Go to a cell.  Cell name, range name, quoted string,",
"                or a number specify which cell.",
(char *)0
};


char *cell[] = {
" ",
" D: Cell entry and editing commands:",
" ",
"     =    Enter a numeric constant or expression.",
"     <    Enter a left justified string or string expression.",
"     \",>  Enter a right justified string or string expression.",
"     e    Edit the current cell's numeric value.",
"     E    Edit the current cell's string part.",
"     x    Clear the current cell.",
"     c    Copy the last marked cell to the current cell.",
"     m    Mark a cell to be used as the source for ``c''",
"     +    Increment numeric part",
"     -    Decrement numeric part",
" ",
"     In numeric mode, a decimal digit, ``+'', ``-'', and ``.'' all start",
"     a new numeric constant or expression.",
(char *)0
};


char *vi[] = {
" ",
" E: Line Editor",
" ",
"     Hitting the ESC key while entering any command on the top line",
"     will start a one-line vi-style editor.  Supported commands:",
" ",
"     ESC q        Abort command entry.",
"     h l          Move cursor forward, backward.",
"     0 $          Move cursor to the beginning, end of the line.",
"     b w          Move cursor forward/back one word.",
"     fc           Move cursor to character c.",
"     tc           Move the cursor the the character before c.",
"     i a          Enter insert mode before/after the cursor.",
"     I            Move to cursor column 0 and enter insert mode.",
"     x X          Delete the character under/before the cursor.",
"     rc           Replace the character under the cursor with c.",
"     cm           Change - m = b,f,h,l,t or w.",
"     dm           Delete - m = b,f,h,l,t or w.",
"     R            Enter replace (overstrike) mode.",
"     + j - k /    Forward/backward/search the command history.",
"     n            Repeat last history search.",
"     . u          Repeat/undo the last command.",
(char *)0
};

char *file[] = {
" ",
" F: File commands:",
" ",
"     G    Get a new database from a file. ",
"     M    Merge a new file into the current database.",
"     P    Put the current database into a file.",
"     W    Write a listing of the current database into a file in",
"          a form that matches its appearance on the screen.",
"     T    Write a listing of the current database to a file, but",
"          put delimiters between each pair of fields.",
"          Optionally brackets output with control lines for ``tbl'',",
"          ``LaTeX'', or ``TeX''.",
" ",
"     If encryption mode is set, file I/O will be encrypted/decrypted.",
"     ``\"| program\"'' for a file name will pipe (unencrypted) output to",
"     a program for Put, Write and Table.  If a cell name is used",
"     as the file name, the cell's string part will be used as the",
"     file name.",
(char *)0
};


char *row[] = {
" ",
" G: Row and column commands:",
" ",
"     ir, ic      Insert a new, empty row (column)",
"     ar, ac      Append a new copy of the current row (column)",
"     dr, dc      Delete the current row (column)",
"     pr, pc, pm  Pull deleted cells back into the spreadsheet",
"                 Insert rows, columns or merge the cells.",
"     vr, vc      Remove expressions from the affected rows (columns),",
"                 leaving only the values.",
"     zr, zc      Hide (``zap'') the current row (column)",
"     sr, sc      Show hidden rows (columns)",
"     f           Set the output format to be used with the values of",
"                 each cell in this column.  Enter field width and",
"                 number of fractional digits.  A preceding count can be",
"                 used to change more than one column.",
" ",
"     Commands which move or copy cells also modify the row and column ",
"     references in the new cell expressions.  Use ``fixed'' or the",
"     ``$'' style cell reference to supress the change.",
(char *)0
};


char *range[] = {
" ",
" H: Range commands:",
" ",
"     /x   Clear a range. ",
"     /v   Remove the expressions from a range of cells, leaving ",
"          just the values.",
"     /c   Copy a source range to a destination range.",
"     /f   Fill a range with constant values starting with a given",
"          value and increasing by a given increment.",
"     /d   Assign a name to a cell or a range of cells.  Give the",
"          the name, surrounded by quotes, and either a cell name such",
"          as ``A10'' or a range such as ``a1:b20''.",
"     /s   Shows the currently defined range names.  Pipe output to",
"          sort, then to less.",
"     /u   Use this command to undefine a previously defined range",
"          name.",
" ",
"     Range operations affect a rectangular region on the screen",
"     defined by the upper left and lower right cells in the region.",
"     A range is specified by giving the cell names separated by ``:'',",
"     such as ``a20:k52''.  Another way to refer to a range is to use",
"     a name previously defined using ``/d''.",
(char *)0
};


char *misc[] = {
" ",
" I: Miscellaneous commands:",
" ",
"     Q q ^C   Exit from the program.",
"     ^G ESC   Abort entry of the current command.",
"     ?        Help",
"     !        Shell escape.  Enter a command to run.  ``!!'' repeats",
"              the last command.  Just ``!'' starts an interactive shell.",
"     ^L       Redraw the screen.",
"     ^R       Redraw the screen.  Highlight cells with values but no",
"              expressions.",
"     ^X       Redraw the screen.  Show formulas, not values.",
"     @        Recalculate the spreadsheet.",
"     ^V       Type, in the command line, the name of the current cell.",
"     ^W       Type, in the command line, the current cell's expression.",
"     ^A       Type, in the command line, the current cell's numeric value.",
"     TAB      When the character cursor is on the top line TAB can be used",
"              to start or stop the display of the default range.",
(char *)0
};

char *var[] = {
" ",
" J: Variable names:",
" ",
"     K20    Row and column can vary on copies.",
"     $K$20  Row and column stay fixed on copies.",
"     $K20   Row can vary; column stays fixed on copies.",
"     K$20   Row stays fixed; column can vary on copies.",
"     fixed  holds following expession fixed on copies.",
"     Cells and ranges can be given a symbolic name via ``/d''.",
" ",
" Expressions:",
"     -e      Negation                e<=e  Less than or equal",
"     e+e     Addition                e=e   Equal",
"     e-e     Subtraction             e!=e  Not Equal",
"     e*e     Multiplication          e>=e  Greater than or equal",
"     e/e     Division                e>e  Greater than",
"     e%e     Modulo                  e<e  Less than",
"     e^e     Exponentiation          e&e  Boolean operator AND.",
"     ~e      Boolean operator NOT    e|e  Boolean operator OR",
"     e?e1:e2  or @if(e,e1,e2)",
"             Conditional: If e is non zero then then e1, else e2.",
"     Terms may be constants, variables, and parenthesized expressions.",
(char *)0
};

char *rangef[] = {
" ",
" K: Range functions:",
" ",
"     @sum(r)           Sum all valid cells in the range.",
"     @prod(r)          Multiply together all valid cells in the range.",
"     @avg(r)           Average all valid cells in the range.",
"     @count(r)         Count all valid cells in the range.",
"     @max(r)           Return the maximum value in the range.",
"     @min(r)           Return the minimum value in the range.",
"     @stddev(r)        Return the sample standard deviation of ",
"                       the cells in the range.",
"     @index(e,r) @stindex(e,r)",
"                       Return the numeric (string) value of the cell at",
"                       index e into range r.",
"     @lookup(e,r) @hlookup(e,r,n) @vlookup(e,r,n)",
"                       Search through the range r for a value that",
"                       matches e.  If e is numeric, the last value <= e",
"                       matches; if string, an exact match is required.",
"                       @lookup searches a single row (column) and returns",
"                       the value from the next column (row); @hlookup",
"                       (@vlookup) searches the first row (column) in r and",
"                       returns the value n columns (rows) from the match.",
(char *)0
};

char *numericf[] = {
" ",
" L: Numeric functions:",
" ",
"     @atan2(e1,e2)     Arc tangent of e1/e2.",
"     @ceil(e)          Smallest integer not less than e.",
"     @eqs(se1,se2)     1 if string expr se1 has the same value as se2.",
"     @exp(e)           Exponential function of e.",
"     @abs(e) @fabs(e)  Absolute value of e.",
"     @floor(e)         The largest integer not greater than e.",
"     @hypot(x,y)       Sqrt(x*x+y*y).",
"     @max(e1,e2,...)   The maximum of the values of the e's.",
"     @min(e1,e2,...)   The minimum of the values of the e's",
"     @nval(se,e)       The numeric value of a named cell.",
"     pi                A constant quite close to pi.",
"     @pow(e1,e2)       e1 raised to the power of e2.",
"     @rnd(e)           Round e to the nearest integer.",
"     @round(e,n)       Round e to n decimal places.",
"     @sqrt(e)          Square root of e.",
"     @ston(se)         Convert string expr se to a numeric",
"     @ln(e)   @log(e)           Natural/base 10 logarithm of e.",
"     @dtr(e)  @rtd(e)           Convert degrees to/from radians.",
"     @cos(e)  @sin(e)  @tan(e)  Trig functions of radian arguments.",
"     @asin(e) @acos(e) @atan(e) Inverse trig function.",
(char *)0
};

char *stringf[] = {
" ",
" M: String functions:",
" ",
"     #                 Concatenate strings.  For example, the",
"                       string expression ``A0 # \"zy dog\"'' yields",
"                       ``the lazy dog'' if A0 is ``the la''.",
"     @substr(se,e1,e2) Extract characters e1 through e2 from the",
"                       string expression se.  For example,",
"                       ``@substr(\"Nice jacket\" 4, 7)'' yields ",
"                       ``e jac''.",
"     @fmt(se,e)        Convert a number to a string using sprintf(3).",
"                       For example,  ``@fmt(\"*%6.3f*\",10.5)'' yields",
"                       ``*10.500*''.  Use formats are e, E, f, g, and G.", 
"     @sval(se,e)       Return the string value of a cell selected by name.",
"     @ext(se,e)        Call an external function (program or",
"                       script).  Convert e to a string and append it",
"                       to the command line as an argument.  @ext yields",
"                       a string: the first line printed to standard",
"                       output by the command.",
"     String expressions are made up of constant strings (characters",
"     surrounded by quotes), variables, and string functions.",
(char *)0
};


char *finf[] = {
" ",
" N: Financial functions:",
" ",
"     @pmt(e1,e2,e3)    @pmt(60000,.01,360) computes the monthly",
"                       payments for a $60000 mortgage at 12%",
"                       annual interest (.01 per month) for 30",
"                       years (360 months).",
" ",
"     @fv(e1,e2,e3)     @fv(100,.005,36) computes the future value",
"                       of 36 monthly payments of $100 at 6%",
"                       interest (.005 per month).  It answers the",
"                       question:  ``How much will I have in 36",
"                       months if I deposit $100 per month in a",
"                       savings account paying 6% interest com-",
"                       pounded monthly?''",
" ",
"     @pv(e1,e2,e3)     @pv(1000,.015,36) computes the present",
"                       value of an ordinary annuity of 36",
"                       monthly payments of $1000 at 18% annual",
"                       interest.  It answers the question: ``How",
"                       much can I borrow at 18% for 30 years if I",
"                       pay $1000 per month?''",
(char *)0
};


char *timef[] = {
" ",
" O: Time and date functions:",
" ",
"     @now              Return the time encoded in seconds since 1970.",
"     @dts(m,d,y)       Return m/d/y encoded in seconds since 1970.",
"     @tts(h,m,s)       Return h:m:s encoded in seconds since midnight.",
" ",
"     All of the following take an argument expressed in seconds:",
" ",
"     @date(e)          Convert the time in seconds to a date",
"                       string 24 characters long in the following",
"                       form: ``Sun Sep 16 01:03:52 1973''.  Note",
"                       that you can extract pieces of this fixed format",
"                       string with @substr.",
"     @year(e)          Return the year.  Valid years begin with 1970.",
"     @month(e)         Return the month: 1 (Jan) to 12 (Dec).",
"     @day(e)           Return the day of the month: 1 to 31.",
"     @hour(e)          Return the number of hours since midnight: 0 to 23.",
"     @minute(e)        Return the number of minutes since the",
"                       last full hour: 0 to 59.",
"     @second(e)        Return the number of seconds since the",
"                       last full minute: 0 to 59.",
(char *)0
};
void
help()
{
    int option;
    char **ns = intro;

    while((option = pscreen(ns)) != 'q' && option != 'Q') {
    	switch (option) {
	case 'a': case 'A': ns = intro; break;
	case 'b': case 'B': ns = options; break;
	case 'c': case 'C': ns = cursor; break;
	case 'd': case 'D': ns = cell; break;
	case 'e': case 'E': ns = vi; break;
	case 'f': case 'F': ns = file; break;
	case 'g': case 'G': ns = row; break;
	case 'h': case 'H': ns = range; break;
	case 'i': case 'I': ns = misc; break;
	case 'j': case 'J': ns = var; break;
	case 'k': case 'K': ns = rangef; break;
	case 'l': case 'L': ns = numericf; break;
	case 'm': case 'M': ns = stringf; break;
	case 'n': case 'N': ns = finf; break;
	case 'o': case 'O': ns = timef; break;
	default: ns = intro; break;
	}
    }
    FullUpdate++;
    (void) move(1,0);
    (void) clrtobot();
}

pscreen(screen)
char *screen[];
{
    int line;
    int dbline;

    (void) move(1,0);
    (void) clrtobot();
    dbline = 1;
    for (line = 0; screen[line]; line++) {
	(void) move(dbline++, 4);
	(void) addstr (screen[line]);
	(void) clrtoeol();
    }
    (void) move(0,0);
    (void) printw("Which Screen? [a-n, q]");
    (void) clrtoeol();
    (void) refresh();
    return(nmgetch());
}
