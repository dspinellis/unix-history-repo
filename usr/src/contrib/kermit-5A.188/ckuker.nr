.TH KERMIT 1C LOCAL
.SH NAME
kermit \- file transfer, terminal connection, character-set translation,
and script programming
.SH SYNOPSIS
.B kermit
[ options ...] [files ...]
.SH DESCRIPTION
.I Kermit
is a family of file transfer, management, and communication software programs
available for most computers and operating systems.  The UNIX version of
Kermit, called C-Kermit, supports serial connections (direct or dialed) and,
in most UNIX implementations, also TCP/IP connections.  On SunOS systems
equipped with SunLink X.25, C-Kermit also supports X.25 connections.
.PP
C-Kermit's major functions are file transfer using the Kermit protocol,
automatic dialing, terminal connection, translation of national and
international character sets, and script programming for automated
communications.  This manual gives a brief overview of C-Kermit version
5A(188).  C-Kermit is fully documented in the book
.IR "Using C-Kermit"
by Frank da Cruz and Christine M. Gianone, Digital Press, Burlington, MA,
USA.  Digital Press ISBN: 1-55558-108-0; Prentice-Hall ISBN: 0-13-037490-3.
Price: US $34.95.  In USA, call DECdirect at 1-800-344-4825, refer to order
number EY-J896E-DP.  Available: January 1993.
.PP
C-Kermit can be used in two "modes" -- remote and local.  In remote mode,
you connect to the UNIX system from a desktop PC, Macintosh, or workstation
and transfer files between your desktop computer and UNIX C-Kermit.  In that
case, connection establishment (dialing, TELNET connection, etc) is handled
by the Kermit program on your desktop computer.
.PP
In local mode, C-Kermit establishes a connection to another computer
by direct serial connection, by dialing a modem, or by making a network
connection.  When used in local mode, C-Kermit gives you a terminal connection
to the remote computer, using your actual terminal, emulator, or UNIX
workstation window or console driver for specific terminal emulation.
.PP
C-Kermit also has two types of commands: the familiar UNIX-style command-line
arguments, and an interactive dialog with prompt.  Command-line arguments give
you access to a small but useful subset of C-Kermit's features for terminal
connection and file transfer, plus the ability to pipe files into or out of
Kermit for transfer.
.PP
Interactive commands give you access to dialing, script programming, 
character-set translation, and in general, detailed control and display of all
C-Kermit's features.  Interactive commands can also be collected into command
files or collected into macros.
.PP
.SH "STARTING C-KERMIT"
.PP
C-Kermit should be available as "kermit" somewhere in your PATH.  To start
C-Kermit simply type "kermit", possibly followed by command-line options.  If
there are no "action options" on the command line, C-Kermit starts in
interactive mode; you will see a greeting message and then the "C-Kermit>"
prompt.  If you include action options on the command line, C-Kermit takes the
indicated actions and then exits back to UNIX.  Either way, C-Kermit 
executes the commands in its initialization file, 
.IR ".kermrc" ,
in your home directory before it executes any other commands, unless you have
included the -Y (uppercase) command-line option, which means to skip the
initialization file.
.PP
.SH "USING C-KERMIT TO TRANSFER FILES"
.PP
Here is a common scenario for Kermit file transfer:
.PP
.in +0.5i
.ll -0.5i
.ta +0.2i
.ti -0.2i
\(bu	Start Kermit on local computer and establish a connection to the
remote computer.  If C-Kermit is on your local
computer, use the sequence SET MODEM <modem-name>, SET LINE, SET SPEED, and
DIAL <number> if you are dialing; SET LINE and SPEED for direct connections,
SET NETWORK and SET HOST (or, for TCP/IP, simply TELNET) for network
connections.

.ti -0.2i
\(bu	SET any other necessary communication parameters, such as PARITY,
DUPLEX, and FLOW-CONTROL.  If you will be communicating with an IBM mainframe
in line-mode (not full-screen mode), give the command DO IBM.

.ti -0.2i
\(bu	Give the CONNECT command.

.ti -0.2i
\(bu	Log in to the remote computer.

.ti -0.2i
\(bu	Start Kermit on the remote computer, give it any desired SET commands
for file, communication, or protocol-related parameters.  If you will be
transferring binary files, give the command SET FILE TYPE BINARY to the remote
Kermit.

.ti -0.2i
\(bu	Give the remote Kermit the SERVER command.

.ti -0.2i
\(bu	Escape back to the Kermit program on your local (desktop) computer.
If your local computer is running C-Kermit, type Ctrl-\\ c (Control-backslash
followed by the letter 'c') (on NeXT workstations, use Ctrl-] c).  If MS-DOS
or OS/2 Kermit, use Alt-X or Ctrl-] c.  Now you should see your local Kermit
program's prompt.

.ti -0.2i
\(bu	If you will be transferring binary files, give the command SET FILE
TYPE BINARY to your local Kermit program.

.ti -0.2i
\(bu	To get files from the remote computer to your local computer, use GET
followed by the filename.   Use SEND followed by the filename to put files
from your local computer to the remote system.  You may use "wildcard"
characters such as * and ? in the filenames to specify groups of files.

.ti -0.2i
\(bu	When you finish transferring files, and you need to continue your work
on the remote remote computer, type FINISH and then CONNECT.  You're now
talking Kermit on the remote computer again.  Type EXIT to get back to the
command prompt on the remote system.  At the end, logout normally from the
remote computer.  Then escape back to Kermit on your local computer, and then
EXIT from the local Kermit program.

.ti -0.2i
\(bu	If you don't need to go back to the remote computer again, type the
BYE command to the local Kermit program.  This will shut down the remote
Kermit server and terminate (log out) your remote session.
.ll +0.5i
.in -0.5i
.fi
.PP
.SH HELP
.PP
Kermit has extensive built-in help.  You can find out what commands exist by
typing ? at the beginning of a line.  You can type HELP followed by the name
of a command for information on it.  For commands that take multiple
arguments, you can type ? in the middle of the command for more information,
and you can also use help with several arguments.  E.g. SET has many different
things you can set, like SET RECEIVE PACKET-LENGTH 1000.  You can type 'SET
?', or 'SET RECEIVE ?', etc.  'HELP SET' talks about the 'SET' command in
general, 'HELP SET RECEIVE talks about the SET RECEIVE options.
.SH BACKSLASH NOTATION
Within an interactive command, \\ is a special character to help you enter
special quantities, or ordinary characters that would otherwise be illegal or
hard to type.  At the end of a line, \\ (backslash) (or - dash) makes the next
line a continuation of the current line.  Other than that, the character
following the \\ identifies what the thing is:
.nf

  % A user-defined simple (scalar) variable such as \\%a or \\%1
  & an array reference such as \\&a[3]
  $ an environment variable such as \\$(TERM)
  v (or V) a built-in variable such as \\v(time)
  f (or F) a function such as \\fSubstring(\\%a,3,2)
  d (or D) a decimal (base 10) number (1 to 3 digits, 0..255) such as \\d27
  o (or O) an octal (base 8) number (1 to 3 digits, 0..377) such as \\o33
  x (or X) a hexadecimal (base 16) number (2 digits, 00..ff) like \\x1b
  \\ the backslash character itself
  b (or B) the BREAK signal (OUTPUT command only)
  l (or L) a Long BREAK signal (OUTPUT only)
  a decimal digit (a 1-3 digit decimal number) such as \\27
  anything else: following character taken literally.

.fi
Note that numbers turn into the character with that binary code, so
you can use \\7 for a bell, \\13 for carriage return, \\10 for linefeed.

.SH COMMANDS
.PP
You can use upper- or lower case for commands.  Either one will work (but
remember that UNIX filenames are case-sensitive).  Also, you can abbreviate
commands as long as the abbreviation matches only one possibility.  This is a
summary of C-Kermit's commands.  For more information on each, see
.IR "Using C-Kermit" ,
or type HELP and then the command name.
.nf
.in 0
.ll 80
.ta 15

;	Introduce a full-line or trailing comment (also #).
!	Run a system command or enter system command interpreter.
ASK	Prompt the user, store user's reply in a variable.
ASKQ	Like ASK, but, but doesn't echo (useful for passwords).
ASSIGN	Assign an evaluated string to a variable or macro.
BYE	Terminate and log out a remote Kermit server.
C	Special abbreviation for CONNECT.
CD	Change Working Directory (also, CWD).
CHECK	See if a particular feature is built in.
CLEAR	Clear communication device input buffer.
CLOSE	Close a log or other local file.
COMMENT	Introduce a full-line comment.
CONNECT	Establish a terminal connection to a remote computer.
DECLARE	Declare an array.
DECREMENT	Subtract one (or other number) from a variable.
DEFINE	Define a variable or macro.
DELETE	Delete a file or files.
DIAL	Dial a telephone number.
DIRECTORY	Display a directory listing.
DISABLE	Disallow access to selected features during server operation.
DO	Execute a macro ("DO" can be omitted).
E-PACKET	Send an Error packet.
ECHO	Display text on the screen.
ELSE	Used with IF.
ENABLE	Allow access to selected features during server operation.
END	A command file or macro.
EXIT	Exit from the program, closing all open files and devices.
FINISH	Instruct a remote Kermit server to exit, but not log out.
FOR	Execute commands repeatedly in a counted loop.
G	Special abbreviation for GET.
GET	Get files from a remote Kermit server.
GETOK	Ask question, get Yes or No answer, set SUCCESS or FAILURE.
GOTO	Go to a labeled command in a command file or macro.
HANGUP	Hang up the phone or network connection.
HELP	Display a help message for a given command.
IF	Conditionally execute the following command.
INCREMENT	Add one (or other number) to a variable.
INPUT	Match characters from another computer against a given text.
INTRO	Print a brief introduction to C-Kermit.
LOG	Open a log file -- debugging, packet, session, transaction.
MAIL	Send a file to other Kermit, to be delivered as e-mail.
MSEND	Send a list of files to the other Kermit.
MSLEEP	Sleep for given number of milliseconds.
OPEN	Open a local file for reading or writing.
O	Special abbreviation for OUTPUT.
OUTPUT	Send text to another computer.
PAD	Command for X.25 PAD (SunLink X.25 only)
PAUSE	Do nothing for a given number of seconds.
PING	Check status of remote TCP/IP host.
PRINT	Print a file on a printer.
PUSH	Invoke host system interactive command interpreter.
PWD	Display current working directory.
QUIT	Same as EXIT.
R	Special abbreviation for RECEIVE.
READ	Read a line from a local file.
RECEIVE	Passively wait for files to arrive.
REDIAL	The the most recently DIALed number again.
REINPUT	Reexamine text previously received from another computer.
REMOTE	Issue file management commands to a remote Kermit server.
RENAME	Change the name of a file.
RETURN	Return from a user-defined function.
RUN	Run a program or system command.
S	Special abbreviation for SEND.
SCRIPT	Execute a UUCP-style login script.
SEND	Send files.
SERVER	Begin server operation.
SET	Set various parameters.
SHOW	Display values of SET parameters.
SLEEP	Sleep for given number of seconds.
SPACE	Display current disk space usage.
STATISTICS	Display statistics about most recent file transfer.
STOP	Stop executing macro or command file, return to prompt.
SUSPEND	Suspend Kermit (use only if shell supports job control!).
TAKE	Execute commands from a file.
TELNET	Make a TCP/IP TELNET connection to a remote network host.
TRANLATE	Translate a file's character set.
TRANSMIT	Upload a file with no error checking.
TYPE	Display a file on the screen.
VERSION	Display the program version number on the screen.
WAIT	Wait for the specified modem signals.
WHILE	Execute commands repeatedly while a condition is true.
WRITE	Write text to a local file.
XIF	Extended IF command.
XLATE	Synonym for TRANSLATE.
XMIT	Synonym for TRANSMIT.
.ll
.in
.fi
.SH PARAMETERS YOU CAN SET
Here are some of the parameters you can change with the SET command.  Note
that many of them require further specification, e.g. there are several
RECEIVE parameters.  So commands using SET RECEIVE
look like "SET RECEIVE PACKET-LENGTH 1000".  For parameters like that,
the possible second words are listed below them. 
.nf
.in 0
.ll 80
.ta 15 

ATTRIBUTES	Turn Attribute packet processing on or off.
     BLOCKSIZE, CHARACTER-SET, DATE, DISPOSITION,
     LENGTH, OS-SPECIFIC, SYSTEM-ID, ALL 
BLOCK-CHECK	Level of file transfer error detection. 
BUFFERS	Size of send and receive packet buffers.
CARRIER	Treatment of carrier on terminal connections.
CASE	Treatment of alphabetic case in string comparisons.
COMMAND	BYTESIZE used between C-Kermit and your keyboard and screen.
COUNT	For counted loops.
DEBUG	Log or display debugging information.
DELAY	How long to wait before sending first packet.
DIAL	Parameters related to dialing.
     DIAL-COMMAND, DIRECTORY, DISPLAY, HANGUP, INIT-STRING,
     KERMIT-SPOOF, MNP-ENABLE, SPEED-MATCHING, TIMEOUT	
DUPLEX	Specify which side echoes during CONNECT.
ESCAPE	Prefix for "escape commands" during CONNECT.
FILE	Set transfer file parameters:
     BYTESIZE, CHARACTER-SET, COLLISION, DISPLAY, 
     INCOMPLETE, NAMES, TYPE
FLOW-CONTROL	Communication line full-duplex flow control.
HANDSHAKE	Communication line half-duplex turnaround character.
HOST	Specify network host name.
INPUT	Control behavior of INPUT command.  
KEY	Key mapping and macros for use in CONNECT mode.
LANGUAGE	Enable language-specific character-set translations.
LINE	Serial communication device name.
MACRO	Control aspects of macro execution.
MODEM-DIALER	Type of modem-dialer on communication line.
NETWORK	Network type, e.g. TCP/IP, X.25.
PAD	X.25 X.3 PAD parameters (SunLink X.25 only).
PARITY	Communication line character parity.  
PROMPT	The C-Kermit program's interactive command prompt.
RECEIVE	Parameters for inbound packets.
     END-OF-PACKET, PACKET-LENGTH, PAD-CHARACTER, PADDING
     START-OF-PACKET, TIMEOUT 
RETRY	Packet retransmission limit.
SEND	Parameters for outbound packets.
	See RECEIVE for subparameters.  Normally you set only
	RECEIVE parameters.  SEND parameters come automatically
	from the Kermit on the other end.  
SERVER	Parameters for server operation.
     TIMEOUT 
SESSION-LOG	File type for session log, text or binary.
SPEED	Communication line speed, e.g. 2400, 9600.
TAKE	Control aspects of TAKE file execution.
TERMINAL	Terminal parameters:
     BYTESIZE, CHARACTER-SET, CR-DISPLAY, ECHO,
     LOCKING-SHIFT, NEWLINE-MODE
TRANSFER	File transfer parameters:
     CHARACTER-SET, LOCKING-SHIFT
TRANSMIT	Control aspects of TRANSMIT command execution:
     ECHO, EOF, FILL, LINEFEED, LOCKING-SHIFT, PAUSE, PROMPT
UNKNOWN	Specify handling of unknown character sets.
WINDOW	File transfer packet window size. 
X.25	Specify X.25 connection parameters (SunLink X.25 only).
.in
.ll
.SH "MACROS AND VARIABLES"
C-Kermit allows macros and variables.  A macro is a command
that you define, composed of one or more other C-Kermit commands.
A typical macro is a list of Kermit commands, separated
by commas.  For example:
.nf

  define sun set speed 9600, set parity none, set duplex full,-
  set flow xon/xoff

.fi
You call a macro by using its name, just like normal commands.  You can also
call them using the DO command.  If you have given the DEFINE command above
(or have it in your .kermrc file), then you can type SUN or DO SUN to execute
all the commands in the definition.
.PP
C-Kermit also lets you define variables.  Normal variables look like \\%i,
where i is a single letter.  The alphabetic case doesn't matter.  \\%a and
\\%A are the same.  Like macros, they are defined by DEFINE or ASSIGN.  All
values are strings.  DEFINE gives a variable a fixed value.  ASSIGN computes a
value and assigns it to the variable.  To see the difference, look at
.nf

  def \\%a Monday
  def \\%b Today is \\%a
  assign \\%c Today is \\%a
  def \\%a Tuesday
  echo \\%b
  echo \\%c

.fi
This will print "Today is Tuesday" then "Today is Monday".  The difference is
that when defining \\%c, the \\%a is evaluated at the time of the definition,
whereas when defining \\%b, the variable name \\%a itself is put in the
definition.  It isn't evaluated until the echo \\%b.
.PP
There are also arrays, which use \\& instead of \\%.  They are declared by
DECLARE, e.g. DECLARE \\&A[100].  Elements are referenced with subscripts,
which may themselves be variables, and act like simple variables, e.g.
.nf

  DEFINE \\&A[3] Tuesday

.fi
You can destroy the array by making it zero size, DECLARE \\&A[0].
The first element of an array is [1].
.PP
Macros have normal names.  No \\.  You call them by using the name like a
command.  If you put additional words on the same line as the macro
invocation, they become arguments.  Inside the macro, you can refer to the
arguments as \\%1, \\%2, etc.  For example:
.nf

  C-Kermit>define bsend set file type binary, send \\%1
  C-Kermit>define tsend set file type text, send \\%1
  C-Kermit>bsend kermit
  C-Kermit>tsend kermit.doc

.fi
The number of arguments supplied can be referred to as \\v(argc).  If you call
another macro from a macro, the new one gets its own set of arguments, which
do not interfere with the previous set.
.PP
There are a number of built-in variables, which are referred to by \\v(name).
They cannot be changed.  Type SHOW VARIABLES for a complete list.
.nf

  \\v(argc)      number of arguments in current macro
  \\v(args)      number of program command-line arguments
  \\v(cmdfile)   name of current command file, if any
  \\v(cmdlevel)  current command level
  \\v(cmdsource) where command are currently coming from, macro, file, etc.
  \\v(count)     current COUNT value
  \\v(cpu)       CPU type C-Kermit was built for
  \\v(date)      date as 8 Feb 1992
  \\v(day)       day of week
  \\v(directory) current/default directory
  \\v(exitstatus)current EXIT status (0 = good, nonzero = something failed)
  \\v(filespec)  filespec given in most recent SEND/RECEIVE/GET command
  \\v(fsize)     size of file most recently transferred
  \\v(home)      home directory
  \\v(host)      computer host name
  \\v(input)     current INPUT buffer contents
  \\v(inchar)    character most recently INPUT
  \\v(incount)   how many characters arrived during last INPUT
  \\v(line)      current communications device, set by LINE or HOST
  \\v(local)     0 if in remote mode, 1 if in local mode
  \\v(macro)     name of currently executing macro, if any
  \\v(ndate)     Current date as 19920208 (yyyymmdd)
  \\v(ntime)     Current local time in seconds since midnight
  \\v(platform)  Specific machine and/or operating system
  \\v(program)   Name of this program ("C-Kermit")
  \\v(return)    Most recent RETURN value
  \\v(speed)     Current speed, if known, or "unknown"
  \\v(status)    0 or 1 (SUCCESS or FAILURE of previous command)
  \\v(system)    UNIX
  \\v(time)      time as 13:45:23 (hh:mm:ss)
  \\v(ttyfd)     file descriptor of current communication device
  \\v(version)   numeric version of Kermit
.fi
.PP
There are builtin functions, invoked as \\Fname(args).  Type SHOW FUNCTIONS for
a complete list.
.nf

   \\Fcharacter(arg)   convert numeric arg to character
   \\Fcode(char)       numeric code for character
   \\Fcontents(v)      return current definition of variable
   \\Fdefinition(m)    return current definition of macro    
   \\Feval(expr)       evaluate arithmetic expression
   \\Fexecute(m a)     execute macro "m" with parameters "a"
   \\Ffiles(f)         number of files matching file spec
   \\Findex(a1,a2,a3)  position of string a2 in a1, starting at pos a3
   \\Flength(arg)      length of the string "arg"
   \\Fliteral(arg)     copy argument literally, no evaluation
   \\Flower(arg)       convert to lower case
   \\Flpad(text,n,c)   left pad text to length n with char c
   \\Fmax(a1,a2)       max of two numbers
   \\Fmin(a1,a2)       min of two numbers
   \\Fnextfile()       next file name from list in last \\Ffiles
   \\Frepeat(a1,a2)    repeat a1 a2 times
   \\Freverse(arg)     reverse character in arg
   \\Fright(a1,a2)     rightmost a2 characters of string a1
   \\Frpad(text,n,c)   right pad text to length n with char c
   \\Fsubstr(a1,a2,a3) substring of a1, starts at a2, length a3
   \\Fupper(arg)       convert to upper case

.fi
Eval allows the following operators in the expression.  The expression
can contain variables.  Precedences are shown as numbers, 1 is highest
precedence, 6 is lowest.
.in +.2i
.nf
.ta \w'n @ n  'u +\w'5   'u

( )	1	parentheses
n !	2	factorial       
~ n	3	logical NOT
- n	4	negative
n ^ n	4	power
n * n	5	times
n / n	5	division
n % n	5	modulus
n & n	5	logical AND
n + n	6	plus
n - n	6	minus
n | n	6	logical OR
n # n	6	exclusive OR
n @ n	6	greatest common divisor

.fi
.SH "OPTIONS AND COMMAND LINE ARGUMENTS"
.PP
Typically you run Kermit without any arguments, and use a combination
of .kermrc and interactive commands.  However it is possible to put options on
the command line.  This is normally used for scripts.  In this case, Kermit is
invoked as follows:
.nf
.ll 80

  kermit [-x arg [-x arg]...[-yyy]..]]

  -x is an option requiring an argument, 
  -y an option with no argument.
.ta 15

Actions:
  -s files	send files
  -s -	send files from stdin
  -r	receive files
  -k	receive files to stdout
  -x	enter server mode
  -f	finish remote server
  -g files	get remote files from server (quote wildcards)
  -a name	alternate file name, used with -s, -r, -g
  -c	connect (before file transfer), used with -l and -b
  -n	connect (after file transfer), used with -l and -b

Settings:
  -l line	communication line device
  -j host	network host name
  -q	quiet during file transfer
  -i	binary file transfer
  -b bps	line speed, e.g. 1200
  -m name	modem type
  -p x	parity, x = e,o,m,s, or n
  -t	half duplex, xon handshake
  -e n	receive packet length
  -v n  window size
  -w	write over files

Other:
  -y name	alternate init file name
  -Y	Skip init file
  -d	log debug info to debug.log
  -S	Stay, don't exit, after action
  -C "cmds"	Interactive-mode commands

If no action command is included, enter interactive dialog.
.ll
.in
.fi
.SH FILES
$HOME/.kermrc	\fIKermit\fR initialization commands
.PD
.SH AUTHORS
Frank da Cruz, Columbia University, with contributions from hundreds of
other volunteer programmers all over the world; "man page" mostly courtesy of
Charles Hedrick, Rutgers University.
.SH SEE ALSO
.TP
Frank da Cruz and Christine Gianone
.IR "Using C-Kermit" ,
Digital Press, Burlington, MA, USA (1993)
.TP
Frank da Cruz,
.IR "Kermit, A File Transfer Protocol" ,
Digital Press, Bedford, MA, USA (1987)
.SH DIAGNOSTICS
The diagnostics produced by
.I Kermit
itself are intended to be self-explanatory.  In addition, every command
returns a SUCCESS or FAILURE status that can be tested by IF FAILURE or
IF SUCCESS.  In addition, the program itself returns an exit status code of
0 upon successful operation or nonzero if any of various operations failed.
.SH BUGS
See recent issues of the Info-Kermit digest (on BITNET/EARN, the Internet, or
the comp.protocols.kermit newsgroup on Usenet) for discussion, or the files
ckcker.bwr and ckuker.bwr, for a list of bugs.  Report
bugs via e-mail to Info-Kermit-Request@columbia.edu or KERMIT@CUVMA.BITNET.
Subscribe to Info-Kermit by sending e-mail to I$KERMIT@CUVMA.BITNET or
I$KERMIT@CUVMA.CC.COLUMBIA.EDU containing the text "subscribe i$kermit"
followed by your name.
.br
