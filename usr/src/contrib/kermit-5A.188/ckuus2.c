/*  C K U U S 2  --  User interface STRINGS & help module for C-Kermit  */
 
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/
 
/*
 This module separates long strings from the body of the other ckuus* modules.
*/

#include "ckcdeb.h"
#include "ckcnet.h"
#include "ckcasc.h"
#include "ckcker.h"
#include "ckuusr.h"
#include "ckcxla.h"
 
extern char *ccntab[];

#ifndef NOICP
#ifdef DCMDBUF
extern char *cmdbuf;
#else
extern char cmdbuf[];
#endif /* DCMDBUF */
#endif /* NOICP */

#ifdef DEBUG
extern char debfil[];
#endif /* DEBUG */
#ifdef TLOG
extern char trafil[];
#endif

extern char *xarg0;
extern int nrmt, nprm, dfloc, local, parity, duplex, escape;
extern int turn, flow;
extern int binary, warn, quiet, keep;
extern int success;

extern long speed;
extern char *dftty, *versio, *ckxsys;
extern struct keytab prmtab[];
extern struct keytab remcmd[];
 
/* Command-Line help (Unix command-line arguments) */

#ifndef NOCMDL
static
char *hlp1[] = {
#ifndef NOICP
" [cmdfile] [-x arg [-x arg]...[-yyy]..] [ = text ] ]\n",
#else
"[-x arg [-x arg]...[-yyy]..]\n",
#endif
"  -x is an option requiring an argument, -y an option with no argument.\n",
#ifndef NOICP
#ifndef NOSPL
"     = means ignore following words, but place in array \\&@[].\n",
#else
"     = means ignore following material.\n",
#endif /* NOSPL */
#else
"     = means ignore following material.\n",
#endif /* NOICP */
"actions:\n",
"  -s files  send files                    -r  receive files\n",
"  -s -      send files from stdin         -k  receive files to stdout\n",
#ifndef NOSERVER
"  -x        enter server mode             -f  finish remote server\n\n",
#else
"  -f        finish remote server\n\n",
#endif /* NOSERVER */
"  -g files  get remote files from server (quote wildcards)\n",
"  -a name   alternate file name, used with -s, -r, -g\n",
"  -c        connect (before file transfer), used with -l and -b\n",
"  -n        connect (after file transfer), used with -l and -b\n\n",
"settings:\n",
"  -l line   communication line device     -q   quiet during file transfer\n",
#ifdef NETCONN
"  -j host   network host name             -i   binary file transfer\n",
#else
"  -i        binary file transfer\n",
#endif /* NETCONN */
"  -b bps    line speed, e.g. 2400         -t   half duplex, xon handshake\n",
#ifdef DEBUG
"  -p x      parity, x = e,o,m,s, or n     -d   log debug info to debug.log\n",
#else
"  -p x      parity, x = e,o,m,s, or n\n",
#endif /* DEBUG */
#ifndef NOICP
"  -y name   alternate init file name      -w   write over files\n",
#else
"  -w        write over files\n",
#endif /* NOICP */
"  -e n      receive packet length         -v n window size\n",
#ifndef NODIAL
"  -m name   modem type                    -z   force foreground\n",
#else
"  -z        force foreground\n",
#endif /* NODIAL */
#ifdef SUNX25
"  -X  X.25 address              -o index  X.25 closed user group call\n",
"  -u  X.25 reverse charge call  -U string X.25 call user data\n",
#endif /* SUNX25 */
#ifdef NOICP
"Interactive command parser removed.\n",
#else
"If no action command is included, or -S is, enter interactive dialog.\n",
#endif
""
};
 
/*  U S A G E */
 
VOID
usage() {
#ifndef MINIX
#ifdef NOICP
#endif /* NOICP */    
    conol("Usage: ");
    conol(xarg0);
    conola(hlp1);
#else
    conol("Usage: ");
    conol(xarg0);
    conol(" [-x arg [-x arg]...[-yyy]..] ]\n");
#endif /* MINIX */
}
#endif /* NOCMDL */

#ifndef NOICP

/*  Interactive help strings  */
 
static char *tophlp[] = { 
"Trustees of Columbia University in the City of New York.\n",

#ifndef NOHELP
"Type INTRO for an introduction to C-Kermit, press ? for a list of commands.",
"Type HELP followed by a command name for help about a specific command.",
#else
"Type ? for a list of commands; see documentation for detailed descriptions.",
#endif /* NOHELP */
"While typing commands, you may use the following special characters:",
" DEL, RUBOUT, BACKSPACE, CTRL-H: Delete the most recent character typed.",
" CTRL-W:  Delete the most recent word typed.",
" CTRL-U:  Delete the current line.",
" CTRL-R:  Redisplay the current line.",
" ?        (question mark) Display a menu for the current command field.",
" ESC      (or TAB) Attempt to complete the current field.",
" \\        (backslash) include the following character literally",
#ifndef NOSPL
"          or introduce a backslash code, variable, or function.\n",
#else
"          or introduce a numeric backslash code.\n",
#endif /* NOSPL */

"Command words other than filenames can be abbreviated in most contexts.",
#ifndef NOCMDL
"From system level, type \"kermit -h\" for help about command-line options.",
#endif /* NOCMDL */
" ",
#ifdef MAC
"Documentation for Command Window: \"Using C-Kermit\" by Frank da Cruz and",
"Christine M. Gianone. Digital Press ISBN: 1-55558-108-0; Prentice-Hall ISBN:",
"0-13-037490-3.  DECdirect:+1-800-344-4825, Order # EY-J896E-DP, US $34.95,",
"January 1993.  Macintosh-specific documentation is in preparation.",
#else
"DOCUMENTATION: \"Using C-Kermit\" by Frank da Cruz and Christine M. Gianone,",
"Digital Press.  DP ISBN: 1-55558-108-0; Prentice-Hall ISBN: 0-13-037490-3.",
"DECdirect: +1-800-344-4825, Order Number EY-J896E-DP, US $34.95, Jan 1993.",
#endif /* MAC */
""
};
 
#ifndef NOHELP
char *introtxt[] = {
"Welcome to C-Kermit communications software for:",
" . Error-free file transfer",
" . Terminal connection",
#ifndef NOSPL
" . Script programming",
#endif /* NOSPL */
#ifndef NOICS
" . International character set conversion",
#endif /* NOICS */
"\nSupporting:",
" . Serial connections, direct or dialed.",
#ifndef NODIAL
" . Automatic modem dialing",
#endif /* NODIAL */
#ifdef TCPSOCKET
" . TCP/IP network connections",
#endif /* TCPSOCKET */
#ifdef SUNX25
" . X.25 network connections",
#endif /* SUNX25 */
#ifdef OS2
#ifdef DECNET
" . DECnet/PATHWORKS LAT Ethernet connections",
"   (if you have PATHWORKS installed on your OS/2 system)",
#endif /* DECNET */
#ifdef NPIPE
" . Microsoft LAN Manager named-pipe network connections",
"   (if you have LAN Manager installed on your OS/2 system)",
#endif /* NPIPE */
#endif /* OS2 */
" . UNIX, VAX/VMS, OS/2, AOS/VS, OS-9, Commodore Amiga, Atari ST.",
"\nBasic C-Kermit commands:",
"  EXIT          exit from C-Kermit",
"  HELP          request help about a command",
"  TAKE          execute commands from a file",

"\nCommands for file transfer:",
"  SEND          send files",
"  RECEIVE       receive files",
"  SERVER        be a file transfer server",

"\nEssential settings:",
"  SET PARITY    communications parity",
"  SET FLOW      communications flow control, such as XON/XOFF",
"  SET FILE      file settings, for example TYPE TEXT or TYPE BINARY",

"\nTo make a direct serial connection:",
"  SET LINE      select serial communication device",
"  SET SPEED     select communication speed   ",
"  CONNECT       begin terminal connection",

#ifndef NODIAL
"\nTo dial out with a modem:",
"  SET MODEM     select modem type",
"  SET LINE      select serial communication device",
"  SET SPEED     select communication speed   ",
"  DIAL          dial ",
"  CONNECT       begin terminal connection",
#endif /* NODIAL */

#ifdef NETCONN
"\nTo make a network connection:",
"  SET NETWORK   select network type",
"  SET HOST      select network host",
"  CONNECT       begin terminal connection",
#ifdef TNCODE
"  TELNET        select a TCP/IP host and CONNECT to it",
#endif /* TNCODE */
#endif /* NETCONN */

"\nTo return from a terminal connection to the C-Kermit prompt:",
"  Type your escape character followed by the letter C.",

"\nTo display your escape character:",
"  SHOW ESCAPE",

"\nTo display other settings:",
"  SHOW COMMUNICATIONS, SHOW TERMINAL, SHOW FILE, SHOW PROTOCOL, etc.",

"\nFor further information about a particular command, type HELP xxx,",
"where xxx is the name of the command.  For documentation, news of new",
"releases, and information about other Kermit software, contact:\n",
"  Kermit Distribution        E-mail:",
"  Columbia University        kermit@columbia.edu (Internet)",
"  612 West 115th Street      KERMIT@CUVMA (BITNET/EARN)",
"  New York, NY 10025 USA",
"  Phone: (212) 854-3703      Fax: (212) 662-6442",
""
};

static char *hmxxbye = "Syntax: BYE\n\
Shut down and log out a remote Kermit server";
 
static char *hmxxclo[] = {
"Syntax:  CLOSE name",
"Example: CLOSE SESSION\n",
"Close one of the following logs or files:",
"  SESSION",
#ifdef TLOG
"  TRANSACTION",
#endif /* TLOG */
"  PACKET",
#ifdef DEBUG
"  DEBUGGING",
#endif /* DEBUG */
#ifndef NOSPL
"  READ",
"  WRITE",
#endif /* NOSPL */
"Type HELP LOG and HELP OPEN for further info.", "" };
 
static char *hmxxcon = "Syntax: CONNECT (or C)\n\n\
Connect to a remote computer via the tty device given in the most recent\n\
SET LINE command, or the network host named in the most recent SET HOST\n\
command.  Type the escape character followed by C to get back, or followed\n\
by ? for a list of CONNECT-mode escape commands.";
 
static char *hmxxget = "Syntax: GET filespec\n\
Tell the remote Kermit server to send the named file or files.\n\
If the filespec is omitted, then you are prompted for the remote and\n\
local filenames separately.";
 
static char *hmxxlg[] = { "Syntax: LOG (or L) name [ { NEW, APPEND } ]",
"Record information in a log file:\n",
#ifdef DEBUG
"DEBUGGING     Debugging information, to help track down bugs in the C-Kermit",
"              program (default log name is debug.log).\n",
#endif /* DEBUG */
"PACKETS       Kermit packets, to help with protocol problems (packet.log)",
"SESSION       Terminal session, during CONNECT command (session.log)",
#ifdef TLOG
"TRANSACTIONS  Names and statistics about files transferred (transact.log)\n",
#endif /* TLOG */
"If you include the APPEND keyword after the filename, the existing log file,",
"if any, is appended to; otherwise a new file is created.",
"" } ;
 
#ifndef NOSCRIPT
static char *hmxxlogi[] = { "\
Syntax: SCRIPT text\n",
"Login to a remote system using the text provided.  The login script",
"is intended to operate similarly to uucp \"L.sys\" entries.",
"A login script is a sequence of the form:\n",
"  expect send [expect send] . . .\n",
"where 'expect' is a prompt or message to be issued by the remote site, and",
"'send' is the names, numbers, etc, to return.  The send may also be the",
"keyword EOT, to send control-d, or BREAK (or \\\\b), to send a break signal.",
"Letters in send may be prefixed by ~ to send special characters.  These are:",
"~b backspace, ~s space, ~q '?', ~n linefeed, ~r return, ~c don\'t",
"append a return, and ~o[o[o]] for octal of a character.  As with some",
"uucp systems, sent strings are followed by ~r unless they end with ~c.\n",
"Only the last 7 characters in each expect are matched.  A null expect,",
"e.g. ~0 or two adjacent dashes, causes a short delay.  If you expect",
"that a sequence might not arrive, as with uucp, conditional sequences",
"may be expressed in the form:\n",
"  -send-expect[-send-expect[...]]\n",
"where dashed sequences are followed as long as previous expects fail.",
"" };
#endif
 
static char *hmxxrc[] = { "Syntax: RECEIVE (or R) [filespec]\n",
"Wait for a file to arrive from the other Kermit, which must be given a",
"SEND command.  If the optional filespec is given, the (first) incoming",
"file will be stored under that name, otherwise it will be stored under",
"the name it arrives with.",
"" } ;
 
static char *hmxxsen = "\
Syntax: SEND (or S) filespec [name]\n\n\
Send the file or files specified by filespec.\n\
filespec may contain wildcard characters '*' or '?'.  If no wildcards,\n\
then 'name' may be used to specify the name 'filespec' is sent under; if\n\
'name' is omitted, the file is sent under its own name.";
 
#ifndef NOMSEND
static char *hmssmse = "\
Syntax: MSEND filespec [ filespec [ ... ] ]\n\n\
Send the files specified by the filespecs.  One or more filespecs may be\n\
listed, separated by spaces.  Any or all filespecs may contain wildcards\n\
and they may be in different directories.  An alternate name cannot be given.";
#endif /* NOMSEND */

#ifndef NOSERVER
static char *hmxxser = "Syntax: SERVER\n\n\
Enter server mode on the currently selected line.  All further commands\n\
will be taken in packet form from the other Kermit program.  Use FINISH\n\
or BYE to get C-Kermit out of server mode.";
#endif /* NOSERVER */
 
static char *hmhset[] = { "\
The SET command is used to establish various communication or file",
"parameters.  The SHOW command can be used to display the values of",
"SET parameters.  Help is available for each individual parameter;",
"type HELP SET ? to see what's available.",
"" } ;
 
#ifndef NOSETKEY
static char *hmhskey[] = { "Syntax: SET KEY k text\n",
"Map the key k to send 'text' when pressed during CONNECT mode.",
"K can be expressed as decimal number or backslash code, 'text'",
"can also contain any backslash code.  If 'text' has the length 1",
"it is treated specially.  In some environments (OS/2, for example)",
"that single character may be wider than 8 bits, if specified in",
"backslash notation.  In this case, a scan code mapping takes place,",
"i.e. key k takes over the function of the key whose scan code is",
"assigned to k.  This may even be a controlling key for the CONNECT",
"mode.  If 'text' is empty, the default key binding is restored for",
"the key k.  SET KEY mappings take place before terminal character-set",
"translation.",
""};
#endif /* NOSETKEY */

static char *hmxychkt[] = { "Syntax: SET BLOCK-CHECK type\n",
"Type of packet block check to be used for error detection, 1, 2, 3, or",
"BLANK-FREE-2.  Type 1 is standard, and catches most errors.  Types 2 and 3",
"specify more rigorous checking at the cost of higher overhead.  The",
"BLANK-FREE-2 type is the same as Type 2, but is guaranteed to contain no",
"blanks.",
"" } ;
 
#ifndef NODIAL
static char *hmxydial[] = {
"SET DIAL DIAL-COMMAND [ text ]",
"The 'text' replaces C-Kermit's built-in modem dialing command.  It must",
"include '%s' (percent s) as a place-holder for the telephone numbers",
"given in your DIAL commands.  If the 'text' is omitted, C-Kermit uses its",
"built-in modem-specific dialing command.\n",
"SET DIAL DIRECTORY filename",
"Name of dialing directory file.  Type HELP DIAL for further info.\n",
"SET DIAL DISPLAY {ON, OFF}",
"Whether to display dialing progress on the screen.\n",
"SET DIAL HANGUP {ON, OFF}",
"Whether the DIAL command should hang up the phone before dialing.\n",
"SET DIAL INIT-STRING [ text ]",
"The 'text' is a replacement for C-Kermit's built-in initialization command",
"for the modem.  If 'text' omitted, use built-in initialization command.\n",
"SET DIAL KERMIT-SPOOF {ON, OFF}",
"If the selected modem type supports the Kermit protocol directly,",
"use this command to turn its Kermit protocol function on or off.\n",
"SET DIAL MODEM-HANGUP {ON, OFF}",
"Governs how the HANGUP, <esc>H, and similar operations work when you have",
"a dialed connection (in local mode, and a specific modem type is set).",
"ON means to use modem commands to hang up the phone, e.g. ATH0.",
"OFF means to hang up by attempting to turn off the DTR signal.",
"ON is not necessarily supported for all modem types.\n",
"SET DIAL MNP-ENABLE {ON, OFF}",
"Enable or disable MNP negotiation by the modem.\n",
"SET DIAL PREFIX [ text ]",
"Establish a prefix to be applied to the phone numbers given in the DIAL",
"command or read from the dialing directory.\n",
"SET DIAL SPEED-MATCHING {ON, OFF}",
"ON (the default) means that C-Kermit changes its serial interface speed to",
"agree with the speed reported by the modem's CONNECT message, if any.  OFF",
"means that C-Kermit should not change its interface speed.\n",
"SET DIAL TIMEOUT number",
"How many seconds to wait for a dialed call to complete.  Use this command",
"to override the DIAL command's automatic timeout calculation.  A value",
"of 0 turns off this feature and returns to Kermit's automatic dial",
"timeout calculation.\n",
"Also see DIAL and SET MODEM.  Use SHOW DIAL to display dial-related",
"settings.\n",
"" } ;
#endif /* NODIAL */

static char *hmxyflo[] = { "Syntax: SET FLOW value\n",
"Type of flow control to use during file transfer and CONNECT mode.",
"Choices: KEEP (don't change device's current setting), XON/XOFF (software",
"flow control, the default), NONE (no flow control at all), and possibly",
"others including RTS/CTS (hardware) depending on the capabilities of your",
"system.  Type SET FLOW ? for a list.",
""};

static char *hmxyf[] = { "Syntax: SET FILE parameter value",
"Parameters:\n",
"TYPE: How file contents are to be treated during file transfer.",
"TYPE is normally TEXT, with conversion of record format and character set.",
"BINARY means to do no conversion.  Use BINARY for executable programs or",
"binary data.  Example: SET FILE TYPE BINARY.\n",

#ifdef VMS
"For VAX/VMS, you may include an optional record-format after the word",
"BINARY.  This may be FIXED (the default) or UNDEFINED.",
"Two additional VMS file types are also supported: IMAGE and LABELED.  IMAGE",
"means raw block i/o, no interference from RMS, and applies to file transmis-",
"sion only.  LABELED means to send or interpret RMS attributes with the file.",
"\n",
#endif /* VMS */

"BYTESIZE { 7, 8 }: normally 8.  If 7, truncate the 8th bit of all file \
bytes.\n",

#ifndef NOCSETS
"CHARACTER-SET: tells the encoding of the local file, ASCII by default.",
"The names ITALIAN, PORTUGUESE, NORWEGIAN, etc, refer to 7-bit ISO-646",
"national character sets.  LATIN1 is the 8-bit ISO 8859-1 Latin Alphabet 1",
"for Western European languages.",
"NEXT is the 8-bit character set of the NeXT workstation.",
"The CPnnn sets are for IBM PCs.  MACINTOSH-LATIN is for the Macintosh.",
#ifndef NOLATIN2
"LATIN2 is ISO 8859-2 for Eastern European languages that are written with",
"Roman letters.",
#endif /* NOLATIN2 */
#ifdef CYRILLIC
"KOI-CYRILLIC, CYRILLIC-ISO, and CP866 are 8-bit Cyrillic character sets.",
"SHORT-KOI is a 7-bit ASCII coding for Cyrillic.",
#endif /* CYRILLIC */
#ifdef KANJI
"JAPANESE-EUC, JIS7-KANJI, DEC-KANJI, and SHIFT-JIS-KANJI are Japanese",
"Kanji character sets.",
#endif /* KANJI */
"Type SET FILE CHAR ? for a complete list of file character sets.\n",
#endif /* NOCSETS */

"INCOMPLETE - what to do with an incompletely received file: DISCARD",
"(default), or KEEP.\n",

"NAMES are normally CONVERTED to 'common form' during transmission; LITERAL",
"means use filenames literally (useful between like systems).\n",

"COLLISION tells what to do when a file arrives that has the same name as",
"an existing file.  The options are:",
"  BACKUP (default) - Rename the old file to a new, unique name and store",
"    the incoming file under the name it was sent with.",
"  OVERWRITE - Overwrite (replace) the existing file.",
"  APPEND - Append the incoming file to the end of the existing file.",
"  DISCARD - Refuse and/or discard the incoming file.",
"  RENAME - Give the incoming file a unique name.",
"  UPDATE - Accept the incoming file only if it is newer than the existing",
"    file.",
"Example: SET FILE COLLISION UPDATE\n",

#ifdef VMS
"RECORD-LENGTH sets the record length for received files of type BINARY. Use",
"this to receive VMS BACKUP savesets or other fixed-format files. The default",
"of 512 is suitable for executable (.EXE) files, etc.\n",
"Example: SET FILE REC 8192\n",
#endif /* VMS */

"SET FILE DISPLAY selects the format of the file transfer display for",
"local-mode file transfer.  The choices are:",
"  SERIAL (the default).  One dot is printed for every K bytes transferred.",
"    This format works on any kind of terminal, even a hardcopy.",
"  CRT.  Numbers are continuously updated on a single screen line.  This",
"    format can be used on any video display terminal.",
#ifdef CK_CURSES
"  FULLSCREEN.  A fully formatted 24x80 screen showing lots of information.",
"    This requires a video display terminal whose control sequences are",
"    understood by Kermit.",
#endif /* CK_CURSES */
"  NONE.  No file transfer display at all.\n",

"WARNING.  SET FILE WARNING is superseded by the newer command, SET FILE",
"COLLISION.  SET FILE WARNING ON is equivalent to SET FILE COLLISION RENAME",
"and SET FILE WARNING OFF is equivalent to SET FILE COLLISION OVERWRITE.\n",

"" };
 
static char *hmxyhsh[] = { "Syntax: SET HANDSHAKE value\n",
"Character to use for half duplex line turnaround handshake during file",
"transfer.  C-Kermit waits for this character from the other computer before",
"sending its next packet.  Default is NONE, others are XON, LF, BELL, ESC,",
"etc.  SET HANDSHAKE CODE <n> lets you specify the numeric ASCII value of the",
"handshake character.  Type SET HANDSH ? for a list.",
"" };

#ifndef NOSERVER
static char *hsetsrv[] = {"\
SET SERVER DISPLAY {ON,OFF}",
"Tell whether local-mode C-Kermit during server operation should put a",
"file transfer display on the screen.  Default is OFF.\n",
"SET SERVER TIMEOUT n",
"Server command wait timeout interval, how often the C-Kermit server issues",
"a NAK while waiting for a command packet.  Specify 0 for no NAKs at all.",
"Default is 0.",
"" };
#endif /* NOSERVER */

static char *hmhrmt[] = { "\
The REMOTE command is used to send file management instructions to a",
"remote Kermit server.  There should already be a Kermit running in server",
"mode on the other end of the currently selected line.  Type REMOTE ? to",
"see a list of available remote commands.  Type HELP REMOTE x to get",
"further information about a particular remote command 'x'.",
"" } ;

#ifndef NOSPL
static char *ifhlp[] = { "Syntax: IF [NOT] condition command\n",
"If the condition is (is not) true, do the command.  Only one command may",
"be given, and it must appear on the same line as the IF.  Conditions are:\n",
"  SUCCESS    - the previous command succeeded",
"  FAILURE    - the previous command failed",
"  BACKGROUND - C-Kermit is running in the background",
"  FOREGROUND - C-Kermit is running in the foreground\n",
"  DEFINED variablename or macroname - The named variable or macro is defined",
"  NUMERIC variable or constant      - The variable or constant is numeric",
"  EXIST filename                    - The named file exists\n",
"  COUNT   - subtract one from COUNT, execute the command if the result is",
"            greater than zero (see SET COUNT)\n",
"  EQUAL s1 s2 - s1 and s2 (character strings or variables) are equal",
"  LLT s1 s2   - s1 is lexically (alphabetically) less than s2",
"  LGT s1 s1   - s1 is lexically (alphabetically) greater than s2\n",
"  = n1 n1 - n1 and n2 (numbers or variables containing numbers) are equal",
"  < n1 n2 - n1 is arithmetically less than n2",
"  > n1 n2 - n1 is arithmetically greater than n2\n",
"The IF command may be followed on the next line by an ELSE command. Example:",
"  IF < \\%x 10 ECHO It's less",
"  ELSE echo It's not less\n",
"See also XIF.",
"" };
#endif /* NOSPL */

#ifndef NOSPL
static char *ifxhlp[] = { "\
Syntax: XIF condition { commandlist } [ ELSE { commandlist } ]\n",
"Extended IF command.  The conditions are the same as for IF (type HELP IF)",
"but multiple comma-separated commands may be grouped within braces in both", 
"the IF and ELSE parts.  The ELSE part, if any, must be on the same line as",
"the XIF (or use dash for line continuation).  Example:\n",
"  XIF equal \\%a YES { echo OK, goto begin } ELSE { echo Not OK, stop }",
"" };
#endif /* NOSPL */

#ifndef NOSPL
static char *forhlp[] = { "\
Syntax: FOR variablename initial-value final-value increment { commandlist }",
"\nFOR loop.  Execute the comma-separated commands in the commandlist the",
"number of times given by the initial value, final value and increment.",
"Example:  FOR \\%i 10 1 -1 { pause 1, echo \\%i }", "" };

static char *whihlp[] = { "\
Syntax: WHILE condition { commandlist }",
"\nWHILE loop.  Execute the comma-separated commands in the commandlist while",
"the condition is true.  Conditions are the same as for IF commands.", "" };
#endif /* NOSPL */

#ifndef NOSPL
static char *openhlp[] = {
"Syntax:  OPEN mode filename\n",
"For use with READ and WRITE commands.  Open the local file in the specified",
"mode: READ, WRITE, or APPEND.  !READ and !WRITE mean to read from or write",
"to a system command rather than a file.  Examples:\n",
"  OPEN READ oofa.txt",
"  OPEN !READ sort foo.bar",
"" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxaskq[] = {
"Syntax:  ASKQ variablename prompt",
"Example: ASKQ %p { Password:}\n",
"Issues the prompt and defines the variable to be whatever you type in.",
"The characters that you type do not echo on the screen.",
"Use braces to preserve leading and/or trailing spaces in the prompt.",
"To include a question mark, precede it by backslash (\\).","" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxask[] = {
"Syntax:  ASK variablename prompt",
"Example: ASK %n { What is your name\\? }\n",
"Issues the prompt and defines the variable to be whatever you type in.",
"Use braces to preserve leading and/or trailing spaces in the prompt.",
"To include a question mark, precede it by backslash (\\).","" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxdef[] = {
"Syntax: DEFINE name [ definition ]\n",
"Defines a macro or variable.  Its value is the definition, taken literally.",
"No expansion or evaluation of the definition is done.  Thus if the", 
"definition includes any variable or function references, their names are",
"included, rather than their values (compare with ASSIGN).  If the definition",
"is omitted, then the named variable or macro is undefined.\n",
"A typical macro definition looks like this:\n",
"  DEFINE name command, command, command, ...\n",
"for example:\n",
"  DEFINE vax set parity even, set duplex full, set flow xon/xoff\n",
"which defines a Kermit command macro called 'vax'.  The definition is a",
"comma-separated list of Kermit commands.  Use the DO command to execute",
"the macro, or just type its name, followed optionally by arguments.\n",
"The definition of a variable can be anything at all, for example:\n",
"  DEFINE \\%a Monday",
"  DEFINE \\%b 3\n",
"These variables can be used almost anywhere, for example:\n",
"  ECHO Today is \\%a",
"  SET BLOCK-CHECK \\%b",
"" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxass[] = {
"Syntax:  ASSIGN variablename string.",
"Example: ASSIGN \\%a My name is \\%b.\n",
"Assigns the current value of the string to the variable (or macro).",
"The definition string is fully evaluated before it is assigned, so that",
"the values of any variables are contained are used, rather than their",
"names.  Compare with DEFINE.  To illustrate the difference, try this:\n",
"  DEFINE \\%a hello",
"  DEFINE \\%x \\%a",
"  ASSIGN \\%y \\%a",
"  DEFINE \\%a goodbye",
"  ECHO \\%x \\%y\n",
"This will print 'goodbye hello'.", "" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxdec[] = {
"Syntax: DECREMENT variablename [ number ]\n",
"Decrement (subtract one from) the value of a variable if the current value",
"is numeric.  If the number argument is given, subtract that number instead.",
"\nExamples: DECR \\%a, DECR \\%a 7, DECR \\%a \\%n", "" };
#endif /* NOSPL */

#ifndef NOSPL
static char *hxxinc[] = {
"Syntax: INCREMENT variablename [ number ]\n",
"Increment (add one to) the value of a variable if the current value is",
"numeric.  If the number argument is given, add that number instead.\n",
"Examples: INCR \\%a, INCR \\%a 7, INCR \\%a \\%n", "" };
#endif /* NOSPL */

#ifdef SUNX25
static char *hxxpad[] = {
"Syntax: PAD command",
"X.25 PAD commands:\n",
"    PAD CLEAR     - Clear the virtual call",
"    PAD STATUS    - Return the status of virtual call",
"    PAD RESET     - Send a reset packet",
"    PAD INTERRUPT - Send an interrupt packet",
""};

static char *hxyx25[] = {
"Syntax: SET X.25 option { ON [ data ], OFF }\n",
"X.25 call options:",
"  CLOSED-USER-GROUP { ON index, OFF }",
"    Enable or disable closed user group call, where index is the group",
"    index, 0 to 99.",
"  REVERSE-CHARGE { ON, OFF }",
"    Tell whether you want to reverse the charges for the call.",
"  CALL-USER-DATA { ON string, OFF }",
"    Specify call user-data for the X.25 call.",
""};
#endif /* SUNX25 */

static char *hxxpau[] = {
"Syntax:  PAUSE [ number ]",
"Example: PAUSE 3\n",
"Do nothing for the specified number of seconds; if no number given, one",
"second.  If interrupted from the keyboard, set FAILURE, otherwise SUCCESS.",
"" };

static char *hxxmsl[] = {
"Syntax:  MSLEEP [ number ]",
"Example: MSLEEP 500\n",
"Do nothing for the specified number of milliseconds; if no number given,",
"100 milliseconds.","" };

#ifndef NOPUSH
static char *hxxshe[] = {
"Syntax: ! [ command ] or RUN [ command ] or PUSH [ command ]\n",
"Give a command to the local operating system's command processor, and",
"display the results on the screen.\n",
"If the command is omitted, enter interactive mode; return to Kermit",
"by exiting from the system's command parser.  The command is usually",
"EXIT or QUIT or LOGOUT.",  "" };
#endif /* NOPUSH */

#ifndef NOXMIT
static char *hxxxmit[] = {
"Syntax: TRANSMIT file\n",
"The TRANSMIT command is used for sending single files to other computers",
"that don't have Kermit.  Text files are sent a line at a time; binary files",
"are sent a character at a time.  There is no guarantee that the other",
"computer will receive the file correctly and completely.  Before you start",
"the TRANSMIT command, you must put the other computer in data collection",
"mode, for example by starting a text editor.  TRANSMIT may be interrupted by",
"Ctrl-C.  Synonym: XMIT.",
"" };
#endif /* NOXMIT */

#ifndef NOCSETS
static char *hxxxla[] = {
"Syntax: TRANSLATE file1 cs1 cs2 [ file2 ]\n",
"Translates file1 from the character set cs1 into the character set cs2",
"and stores the result in file2.  The character sets can be any of",
"C-Kermit's file character sets.  If file2 is omitted, the translation",
"is displayed on the screen.  Uses Latin-1 as intermediate character set",
"unless LANGUAGE is set to RUSSIAN, in which case it uses Latin-Cyrillic.",
"Synonym: XLATE.  Example:\n",
"TRANSLATE lasagna.lat latin1 italian lasagna.nrc",
"" };
#endif /* NOCSETS */

#ifndef NOSPL
static char *hxxwai[] = {
"Syntax:  WAIT number [modem-signal(s)]",
"Example: WAIT 5 \\cd\\cts\n",
"Waits up to the given number of seconds for all of the specified signals.",
"Sets FAILURE if signals do not appear in given time or if interrupted by",
"typing anything at the keyboard during the waiting period.\n",
"Signals: \\cd = Carrier Detect, \\dsr = Dataset Ready, \\cts = Clear To Send",
"Warning: This command does not work yet, signals are ignored.", "" };
#endif /* NOSPL */

static char *hxxwri[] = {
"Syntax: WRITE name text\n",
"Writes the given text to the named log or file.  The text text may include",
"backslash codes, and is not terminated by a newline unless you include the",
"appropriate code.  The name parameter can be any of the following:\n",
"  DEBUG-LOG",
"  ERROR (standard error)",
#ifndef NOSPL
"  FILE (the OPEN WRITE, OPEN !WRITE, or OPEN APPEND file, see HELP OPEN)",
#endif /* NOSPL */
"  PACKET-LOG",
"  SCREEN (compare with ECHO)",
"  SESSION-LOG",
"  TRANSACTION-LOG", "" };

#ifndef NODIAL
static char *hxxdial[] = { "Syntax:  DIAL phonenumber",
"Example: DIAL 7654321\n",
"Dial a number using an autodial modem.  First you must SET MODEM, then",
"SET LINE, then SET SPEED.  Then give the DIAL command, including the phone",
"number, for example:\n",
"  DIAL 7654321\n",
#ifdef NETCONN
"If the modem is on a network modem server, SET HOST first, then SET MODEM,",
"then DIAL.  See also SET DIAL, SET MODEM, SET LINE, SET HOST, SET SPEED,",
"and REDIAL.\n",
#else
"See also SET DIAL, SET MODEM, SET LINE, SET SPEED, REDIAL.\n",
#endif /* NETCONN */
"The 'phonenumber' can also the name of an entry from your dialing directory,",
"which is a plain text file, one entry per line:\n",
"  name  phonenumber  speed   parity   comments\n",
"for example:\n",
"  e-mail  765-4321   2400    even     My electronic mailbox\n",
"The fields are separated by spaces, and all fields after the 'phonenumber'",
"are optional.  If the speed or parity are present, they replace your current",
"SET SPEED and SET PARITY settings.  Specify your dialing directory file with",
"the SET DIAL DIRECTORY command.",
 "" };
#endif /* NODIAL */

#endif /* NOHELP */

/*  D O H L P  --  Give a help message  */
 
_PROTOTYP( int dohset, (int) );

int
dohlp(xx) int xx; {
    int x,y;
 
    debug(F101,"DOHELP xx","",xx);
    if (xx < 0) return(xx);
    switch (xx) {
 
#ifdef NOHELP

case XXHLP:
    if ((x = cmcfm()) < 0)
      return(x);
    printf("%s, Copyright (C) 1985, 1992,",versio);
    return(hmsga(tophlp));

#else /* help is available */

#ifndef NOSPL
case XXASS:				/* assign */
    return(hmsga(hxxass));

case XXASK:				/* ask */
    return(hmsga(hxxask));

case XXASKQ:
    return(hmsga(hxxaskq));
#endif /* NOSPL */

#ifndef NOFRILLS
case XXBUG:
    return(hmsg("Describe how to report C-Kermit bugs."));
#endif /* NOFRILLS */

case XXBYE:				/* bye */
    return(hmsg(hmxxbye));
 
case XXCHK:				/* check */
    return(hmsg("\
Syntax: CHECK name\n\
Check to see if the named feature is included in this version of C-Kermit.\n\
To list the features you can check, type \"check ?\"."));

#ifndef NOFRILLS
case XXCLE:				/* clear */
    return(hmsg("\
Syntax: CLEAR\n\
Clear the serial port input buffer."));
#endif /* NOFRILLS */

case XXCLO:				/* close */
    return(hmsga(hmxxclo));
 
case XXCOM:				/* comment */
    return(hmsg("\
Syntax: COMMENT text\n\
Example: COMMENT - this is a comment.\n\n\
Introduce a comment.  Beginning of command line only.  Commands may also\n\
have trailing comments, introduced by ; (semicolon)."));

case XXCON:				/* connect */
    hmsg(hmxxcon);
    printf("Your escape character is Ctrl-%c (ASCII %d, %s)\r\n",
	   ctl(escape), escape, (escape == 127 ? "DEL" : ccntab[escape]));
    return(0);
 
case XXCWD:				/* cd / cwd */
#ifdef vms
    return(hmsg("Syntax: CD [ directory or device:directory ]\n\
Change Working Directory, equivalent to VMS SET DEFAULT command"));
#else
#ifdef datageneral
    return(hmsg("Change Working Directory, equivalent to DG 'dir' command"));
#else
#ifdef OS2
  return(hmsg("Change Working Directory, equivalent to OS/2 'chdir' command"));
#else
    return(hmsg("Syntax: CD [ directoryname ]\n\n\
Change Working Directory, equivalent to UNIX cd command."));
#endif /* OS2 */
#endif /* datageneral */
#endif /* vms */
 
#ifndef NOSPL
case XXDCL:
    return(hmsg("Syntax:  DECLARE arrayname[size]\n\
Example: DECLARE \\&a[20]\n\n\
Declares an array of the given size.  Array elements can be used just like\n\
any other variables."));

case XXDEF:				/* define */
    return(hmsga(hxxdef));
#endif /* NOSPL */

#ifndef NOFRILLS
case XXDEL:				/* delete */
    return(hmsg("Syntax: DELETE filespec\n\n\
Delete a local file or files.  RM is a synonym for DELETE."));
#endif /* NOFRILLS */
 
#ifndef NODIAL
case XXDIAL:				/* dial */
    return(hmsga(hxxdial));
#endif
 
case XXDIR:				/* directory */
    return(hmsg("Syntax: DIRECTORY [ filespec ]\n\
Display a directory listing of local files."));
 
#ifndef NOSERVER
#ifndef NOFRILLS
case XXDIS:
    return(hmsg("Syntax: DISABLE command\n\n\
Security for the C-Kermit server.  Prevent the client Kermit program from\n\
executing the named REMOTE command, such as CD, DELETE, RECEIVE, etc."));
#endif /* NOFRILLS */
#endif /* NOSERVER */

#ifndef NOSPL
case XXDO:				/* do */
    return(hmsg("Syntax: [ DO ] macroname [ arguments ]\n\n\
Execute a macro that was defined by the DEFINE command.  The word DO\n\
can be omitted.  Trailing argument words, if any, are automatically\n\
assigned to the macro argument variables \\%1, \\%2, etc."));
#endif /* NOSPL */

#ifndef NOSPL
case XXDEC:
    return(hmsga(hxxdec));
#endif /* NOSPL */

case XXECH:				/* echo */
    return(hmsg("Syntax: ECHO text\n\
Display the text on the screen, followed by a newline.  The ECHO text may\n\
contain backslash codes.  Example: ECHO \\7Wake up!\\7")); 
 
#ifndef NOSERVER
#ifndef NOFRILLS
case XXENA:
    return(hmsg("Syntax: ENABLE capability\n\n\
For use with server mode.  Allow the client Kermit program access to the\n\
named capability, such as CD, DELETE, RECEIVE, etc.  Opposite of DISABLE."));
#endif /* NOFRILLS */
#endif /* NOSERVER */

#ifndef NOSPL
case XXEND:				/* end */
    return(hmsg("Syntax: END [ number [ message ] ]\n\
Exit from the current macro or TAKE file, back to wherever invoked from.\n\
Number is return code.  Message, if given, is printed."));
#endif /* NOSPL */

#ifndef NOFRILLS
case XXERR:				/* error */
    return(hmsg("Syntax: ERROR\n\
Send an Error packet to the other Kermit to get it out of packet mode."));
#endif /* NOFRILLS */

case XXEXI:				/* exit */
case XXQUI:
    return(hmsg("Syntax: QUIT (or EXIT)\n\
Exit from the Kermit program, closing all open files and devices."));
 
case XXFIN:
    return(hmsg("Syntax: FINISH\n\
Tell the remote Kermit server to shut down without logging out."));
 
#ifndef NOSPL
case XXFOR:
    return(hmsga(forhlp));
#endif /* NOSPL */

case XXGET:
    return(hmsg(hmxxget));
 
#ifndef NOSPL
#ifndef NOFRILLS
  case XXGOK:
    return(hmsg("Syntax: GETOK prompt\n\
Print the prompt, make user type 'yes', 'no', or 'ok', and set SUCCESS or\n\
FAILURE accordingly."));
#endif /* NOFRILLS */
#endif /* NOSPL */

#ifndef NOSPL
case XXGOTO:
    return(hmsg("Syntax: GOTO label\n\
In a TAKE file or macro, go to the given label.  A label is a word on the\n\
left margin that starts with a colon (:).  Example:\n\n\
:oofa\n\
echo Hello!\n\
goto oofa"));
#endif /* NOSPL */

case XXHAN:
    return(hmsg("Syntax: HANGUP\n\
Hang up the phone or network connection."));    

case XXHLP:
/*
  We get confirmation here, even though we do it again in hmsga(), to prevent
  the Copyright message from being printed prematurely.  This doesn't do any
  harm, because the first call to cmcfm() sets cmflgs to 1, making the second
  call return immediately.
*/
    if ((x = cmcfm()) < 0)
      return(x);
    printf("%s, Copyright (C) 1985, 1992,",versio);
    return(hmsga(tophlp));

case XXINT:
    return(hmsg("Give a brief introduction to C-Kermit."));	

#ifndef NOSPL
case XXIF:
    return(hmsga(ifhlp));

case XXINC:
    return(hmsga(hxxinc));

case XXINP:
   return(hmsg("\
Syntax:  INPUT n [ text ]\n\
Example: INPUT 5 Login:\n\n\
Wait up to n seconds for the given text to arrive on the communication line.\n\
If no text, waits for any character.  For use in script programs with\n\
IF FAILURE and IF SUCCESS.  Also see SET INPUT."));
#endif /* NOSPL */

#ifndef NODIAL
case XXRED:
    return(hmsg("Redial the number given in the most recent DIAL commnd."));
#endif /* NODIAL */

#ifndef NOSPL
case XXREI:
    return(hmsg("Syntax: REINPUT n string\n\n\
Look for the string in the text that has recently been INPUT, set SUCCESS\n\
or FAILURE accordingly.  Timeout, n, must be specified but is ignored."));
#endif /* NOSPL */

#ifndef NOFRILLS
case XXREN:
    return(hmsg("Syntax: RENAME oldname newname\n\n\
Change the name of file 'oldname' to 'newname'."));
#endif /* NOFRILLS */

#ifndef NOSPL
case XXLBL:
    return(hmsg("\
Introduce a label, like :loop, for use with GOTO in TAKE files or macros.\n\
See GOTO."));
#endif /* NOSPL */

case XXLOG:
    return(hmsga(hmxxlg));
 
#ifndef NOSCRIPT
case XXLOGI:
    return(hmsga(hmxxlogi));
#endif
 
#ifndef NOFRILLS
case XXMAI:
    return(hmsg("Syntax: MAIL filename address\n\n\
Send the file to the remote Kermit, which must be in RECEIVE or SERVER mode,\n\
and request that the remote host deliver the file as electronic mail to the\n\
given address.  Example: MAIL BUG.TXT KERMIT@CUVMA"));
#endif /* NOFRILLS */

#ifndef NOMSEND
case XXMSE:
    return(hmsg(hmssmse));
#endif /* NOMSEND */

#ifndef NOSPL
case XXOPE:
    return(hmsga(openhlp));
#endif /* NOSPL */

#ifndef NOSPL
case XXOUT:
    return(hmsg("Syntax: OUTPUT text\n\n\
Send the text out the currently selected line, as if you had typed it\n\
during CONNECT mode.  The text may contain backslash codes.  Example:\n\n\
  OUTPUT help\\13"));
#endif /* NOSPL */

#ifdef SUNX25
case XXPAD:
    return(hmsga(hxxpad));
#endif /* SUNX25 */

#ifndef NOSPL
case XXPAU:
    return(hmsga(hxxpau));

case XXMSL:
    return(hmsga(hxxmsl));
#endif /* NOSPL */

#ifdef TCPSOCKET
case XXPNG:
    return(hmsg("Syntax: PING [ IP-hostname-or-number ]\n\n\
Check if given IP network host is reachable.  Default host from most\n\
recent SET HOST or TELNET command.  Runs system PING program, if any."));
#endif /* TCPSOCKET */

#ifndef NOFRILLS
case XXPRI:
    return(hmsg("Syntax: PRINT file [ options ]\n\n\
Print the local file on a local printer with the given options."));
#endif /* NOFRILLS */

case XXPWD:
    return(hmsg("Syntax: PWD\n\
Print the name of the current working directory."));

#ifndef NOSPL
case XXREA:
    return(hmsg("Syntax: READ variablename\n\
Read a line from the currently open READ or !READ file into the variable\n\
(see OPEN)."));
#endif /* NOSPL */

case XXREC:
    return(hmsga(hmxxrc));
 
case XXREM:
    y = cmkey(remcmd,nrmt,"Remote command","",xxstring);
    return(dohrmt(y));
 
#ifndef NOSPL
case XXRET:
    return(hmsg("Syntax: RETURN [ value ]\n\
Return from a macro.  An optional return value can be given for use with\n\
with \\fexecute(macro), which allows macros to be used like functions."));
case XXSEN:
    return(hmsg(hmxxsen));
#endif /* NOSPL */
 
#ifndef NOSERVER
case XXSER:
    return(hmsg(hmxxser));
#endif /* NOSERVER */
 
#ifndef NOJC
case XXSUS:
    return(hmsg("Syntax: SUSPEND or Z\n\
Suspend Kermit.  Continue Kermit with the appropriate system command,\n\
such as fg."));
#endif /* NOJC */

case XXSET:
    y = cmkey(prmtab,nprm,"Parameter","",xxstring);
    debug(F101,"HELP SET y","",y);
    return(dohset(y));
 
#ifndef NOPUSH
case XXSHE:
    return(hmsga(hxxshe));
#endif /* NOPUSH */
 
#ifndef NOSHOW
case XXSHO:
    return(hmsg("\
Display current values of various items (SET parameters, variables, etc).\n\
Type SHOW ? for a list of categories."));
#endif /* NOSHOW */
 
case XXSPA:
#ifdef datageneral
    return(hmsg("\
Display disk usage in current device, directory,\n\
or return space for a specified device, directory."));
#else
    return(hmsg("Syntax: SPACE\n\
Display disk usage in current device and/or directory"));
#endif
 
case XXSTA:
    return(hmsg("Syntax: STATISTICS\n\
Display statistics about most recent file transfer"));
 
#ifndef NOSPL
case XXSTO:
    return(hmsg("Syntax: STOP [ number [ message ] ]\n\
Stop executing the current macro or TAKE file and return immediately to\n\
the C-Kermit prompt.  Number is a return code.  Message printed if given."));
#endif /* NOSPL */

case XXTAK:
    return(hmsg("Syntax: TAKE filename\n\
Take Kermit commands from the named file.  Kermit command files may\n\
themselves contain TAKE commands, up to a reasonable depth of nesting."));
 
#ifdef NETCONN
case XXTEL:
    return(hmsg("Syntax: TELNET [ host ]\n\
Equivalent to SET HOST host, followed by CONNECT.  If hostname omitted,\n\
previous connection (if any) is resumed."));
#endif /* NETCONN */


#ifndef NOXMIT
case XXTRA:
    return(hmsga(hxxxmit));
#endif /* NOXMIT */

#ifndef NOFRILLS
case XXTYP:
    return(hmsg("Syntax: TYPE file\n\
Display a file on the screen.  Pauses if you type Ctrl-S, resumes if you\n\
type Ctrl-Q, returns immediately to C-Kermit prompt if you type Ctrl-C."
));
#endif /* NOFRILLS */

#ifndef NOSPL
case XXWHI:
    return(hmsga(whihlp));
#endif /* NOSPL */

#ifndef NOCSETS
case XXXLA:
    return(hmsga(hxxxla));
#endif /* NOCSETS */

case XXVER:
    return(hmsg("Syntax: VERSION\nDisplays the program version number."));

#ifndef NOSPL
case XXWAI:
    return(hmsga(hxxwai));
#endif /* NOSPL */

#ifndef NOFRILLS
case XXWHO:
    return(hmsg("Syntax: WHO [ user ]\nDisplays info about the user."));

case XXWRI:
    return(hmsga(hxxwri));
#endif /* NOFRILLS */

#ifndef NOSPL
case XXIFX:
    return(hmsga(ifxhlp));
#endif /* NOSPL */

#endif /* NOHELP */

default:
    if ((x = cmcfm()) < 0) return(x);
    printf("Help not available - %s\n",cmdbuf);
    break;
    }
    return(success = 0);
}

#ifdef NOHELP 

int					/* Print an array of lines, */
hmsga(s) char *s[]; {			/* cheap version. */
    int i;
    if ((i = cmcfm()) < 0) return(i);
    printf("\n");			/* Start off with a blank line */
    for (i = 0; *s[i]; i++) {		/* Print each line. */
	printf("%s\n",s[i]);
    }
    printf("\n");
    return(0);
}

#else

int					/* Print an array of lines, */
hmsga(s) char *s[]; {			/* pausing at end of each screen. */
    int x, y, i, j, k, n;
    if ((x = cmcfm()) < 0) return(x);

    printf("\n");			/* Start off with a blank line */
    n = 1;				/* Line counter */
    for (i = 0; *s[i]; i++) {
	printf("%s\n",s[i]);		/* Print a line. */
        y = (int)strlen(s[i]);
        k = 1;
        for (j = 0; j < y; j++)		/* See how many newlines were */
          if (s[i][j] == '\n') k++;	/* in the string... */
        n += k;
	if (n > 21)			/* After a screenful, give them */
          if (!askmore()) return(-9);
          else n = 0;
    }
    printf("\n");
    return(0);
}

/*  H M S G  --  Get confirmation, then print the given message  */
 
int
hmsg(s) char *s; {
    int x;
    if ((x = cmcfm()) < 0) return(x);
    printf("\n%s\n\n",s);
    return(0);
}
 
#ifndef NOXMIT
static char *hsetxmit[] = {
"Syntax: SET TRANSMIT parameter value\n",
"Controls the behavior of the TRANSMIT command, used for uploading files",
"to computers that don't have Kermit programs.  Parameters are:\n",
"ECHO ON/OFF:     Whether to echo text as it is being transmitted.",
"EOF text:        Text to send after end of file is reached.",
"FILL number:     ASCII value of character to insert into blank lines.",
"LINEFEED ON/OFF: Transmit LF as well as CR at the end of each line.",
"                 Normally, only CR is sent.",
"LOCKING-SHIFT ON/OFF: Whether to use SO/SI for transmitting 8-bit data",
"                 when PARITY is not NONE.",
"PAUSE number:    How many milliseconds to pause after transmitting each line",
"                 (text mode), or each character (binary mode).",
"PROMPT number:   ASCII value of character to look for from host before",
"                 sending next line, normally LF (10).",
"Synonym: SET XMIT.",
"" };
#endif /* NOXMIT */

static char *hsetbkg[] = {
"Syntax: SET BACKGROUND { OFF, ON }\n",
"SET BACKGROUND OFF forces prompts and messages to appear on your screen",
"even though Kermit thinks it is running in the background.", "" };

#ifdef DYNAMIC
static char *hsetbuf[] = {
"Syntax: SET BUFFERS n1 n2\n",
"Change the overall amount of memory allocated for SEND and RECEIVE packet",
"buffers, respectively.  Bigger numbers let you have longer packets and more",
"window slots", "" };
#endif /* DYNAMIC */

static char *hsetcmd[] = {
"Syntax: SET COMMAND BYTESIZE { 7, 8 }\n",
"SET COMMAND BYTE 8 allows you to use an 8-bit (international) character set",
"in the commands you type at the C-Kermit> prompt.  7 is the default.", "" };

static char *hsetcar[] = {
"Syntax: SET CARRIER ON, AUTO, or OFF\n",
"Attempts to control treatment of carrier on the communication device.",
"ON means that carrier is required at all times except during the DIAL",
"command.  OFF means that carrier is never required.  AUTO (the default)",
"means that carrier is required only during CONNECT.", "" };

static char *hsetat[] = {
"Syntax: SET ATTRIBUTES name ON or OFF\n",
"Use this command to enable (ON) or disable (OFF) the transmission of",
"selected file attributes along with each file, and to handle or ignore",
"selected incoming file attributes, including:\n",
"  CHARACTER-SET:  The transfer character set for text files",
"  DATE:           The file's creation date",
"  DISPOSITION:    Unusual things to do with the file, like MAIL or PRINT",
"  LENGTH:         The file's length",
"  SYSTEM-ID:      Machine/Operating system of origin",
"  TYPE:           The file's type (text or binary)\n",
"You can also specify ALL to select all of them.  Examples:\n",
"  SET ATTR DATE OFF\n  SET ATTR SIZE ON\n  SET ATTR ALL OFF", ""
};

#ifndef NOSPL
static char *hxxinp[] = {
"Syntax: SET INPUT parameter value\n",
"The SET INPUT command controls the behavior of the INPUT command:\n",
"SET INPUT CASE { IGNORE, OBSERVE }",
"Tells whether alphabetic case is to be significant in string comparisons.\n",
"SET INPUT ECHO { ON, OFF }",
"Tells whether to display arriving characters read by INPUT on the screen.\n",
"SET INPUT SILENCE <number>",
"The maximum number to seconds of silence (no input at all) before the INPUT",
"command times out, 0 for no maximum.\n",
"SET INPUT TIMEOUT-ACTION { PROCEED, QUIT }",
"Tells whether to proceed or quit from a script program if an INPUT command",
"fails.  PROCEED (default) allows use of IF SUCCESS and IF FAILURE commands.",
"" };
#endif /* NOSPL */

static char *hxytak[] = {
"Syntax: SET TAKE parameter value\n",
"Controls behavior of TAKE command.\n",
"SET TAKE ECHO { ON, OFF } tells whether commands read from a TAKE file",
"should be displayed on the screen.\n",
"SET TAKE ERROR { ON, OFF } tells whether a TAKE command file should be",
"automatically terminated when a command fails.", "" };

static char *hxyterm[] = {
"Syntax: SET TERMINAL parameter value\n",
#ifdef OS2
"SET TERMINAL ARROW-KEYS { APPLICATION, CURSOR }",
"sets the mode for the arrow keys during VT terminal emulation.\n", 
#endif /* OS2 */
"SET TERMINAL BYTESIZE 7 or 8, to use 7- or 8-bit terminal characters",
"between C-Kermit and the remote host during CONNECT.\n",
"SET TERMINAL CHARACTER-SET <remote-cs> [ <local-cs> ] to specify the",
"character set used by the remote host, <remote-cs>, and the character",
"set used by C-Kermit locally, <local-cs>.  If you don't specify the",
"local character set, the current FILE CHARACTER-SET is used.  When you",
"specify two different character sets, C-Kermit translates between them",
"during CONNECT.  By default, both character sets are TRANSPARENT.\n",
#ifdef OS2
"SET TERMINAL COLOR <screenpart> <foreground> <background>, to set",
"the colors of the terminal emulation screen. <screenpart> may be one",
"of NORMAL, REVERSE, UNDERLINED, STATUS and HELP. <foreground> and",
"<background> may be one of BLACK, BLUE, GREEN, CYAN, RED, MAGENTA,",
"BROWN, LGRAY, DGRAY, LBLUE, LGREEN, LCYAN, LRED, LMAGENTA, YELLOW",
"or WHITE.  The L prefix for the color names means Light.\n",
#endif /* OS2 */
"SET TERMINAL CR-DISPLAY { CRLF, NORMAL } to specify how incoming",
"carriage return characters are to be displayed on your screen.\n",
"SET TERMINAL ECHO { LOCAL, REMOTE } to specify which side does the",
"echoing during terminal connection.\n",
#ifdef OS2
"SET TERMINAL KEYPAD-MODE { APPLICATION, NUMERIC } to specify the keypad",
"mode for VT terminal emulation.\n",
#endif /* OS2 */
"SET TERMINAL LOCKING-SHIFT { OFF, ON } tells C-Kermit whether to use",
"Shift-In/Shift-Out (Ctrl-O and Ctrl-N) to switch between 7-bit and 8-bit",
"characters during CONNECT.  OFF by default.\n",
"SET TERMINAL NEWLINE-MODE { OFF, ON } tells whether to send CRLF when you",
"type CR during CONNECT mode.\n",
#ifdef OS2
"SET TERMINAL TYPE { VT102, VT52 } to select which terminal to emulate.\n",
"SET TERMINAL WRAP { OFF, ON } to tell whether the VT terminal emulator",
"should automatically wrap long lines on your screen.\n",
#endif /* OS2 */
"Type SHOW TERMINAL to see current terminal settings.",
"" };

#ifdef NETCONN
static char *hxyhost[] = {
"Syntax:  SET HOST hostname-or-number\n",
"Select a network host.  Use this command instead of SET LINE.",
"After SET HOST give the host name or number.  TCP/IP Examples:\n",
"  SET HOST watsun.cc.columbia.edu",
"  SET HOST 128.59.39.2",
"" };

#ifdef TNCODE
static char *hxytel[] = {
"Syntax: SET TELNET parameter value\n",
"For TCP/IP SET HOST and TELNET connections:\n",
"SET TELNET ECHO { LOCAL, REMOTE }",
"  C-Kermit's initial echoing state for TELNET connections, LOCAL by default.",
"  After the connection is made, TELNET negotiations determine the echoing",
"  state.",
"SET TELNET NEWLINE-MODE { OFF, ON }",
"  ON (the default) means send CRLF when user types CR.",
"  OFF means send CR alone.",
"SET TELNET TERMINAL-TYPE name",
"  The terminal type to send to the remote TELNET host.  If none is given,",
"  your local terminal type is sent.\n",
"Type SHOW NETWORK to see the current values of these parameters.",
"" };
#endif /* TNCODE */

static char *hxynet[] = {
"Syntax: SET NETWORK network-type\n",
"Select the type of network to be used with SET HOST connections:\n",
#ifdef TCPSOCKET
"  SET NETWORK TCP/IP",
#endif /* TCPSOCKET */
#ifdef SUNX25
"  SET NETWORK X.25",
#endif /* SUNX25 */
#ifdef DECNET
"  SET NETWORK DECNET",
#endif /* DECNET */
""};
#endif /* NETCONN */
 
#ifndef NOSPL
static char *hxymacr[] = {
"Syntax: SET MACRO parameter value\n",
"Controls the behavior of macros.\n",
"SET MACRO ECHO { ON, OFF } tells whether commands executed from a macro",
"definition should be displayed on the screen.\n",
"SET MACRO ERROR { ON, OFF } tells whether a macro should be automatically",
"terminated upon a command error.", "" };
#endif /* NOSPL */

#ifndef NODIAL
static char *hxymodm[] = {
"Syntax: SET MODEM-DIALER name\n",
"Type of modem for dialing remote connections.  Selects the dialing protocol,",
#ifdef MINIDIAL
"such as HAYES, to be used by the DIAL command.  Several dialing protocols",
#else
"such as HAYES, to be used by the DIAL command.  Many modem dialing protocols",
#endif /* MINIDIAL */
"are supported; type SET MODEM ? for a list.  DIRECT or NONE means a direct",
"connection, with no modem at all.  Also see HELP DIAL, HELP SET CARRIER.",
"" };
#endif /* NODIAL */

static char *hmxyprm[] = {
"Syntax: SET PROMPT [ text ]\n",
#ifdef MAC
"Prompt text for this program, normally 'C-Kermit>'.  May contain backslash",
#else
"Prompt text for this program, normally 'Mac-Kermit>'.  May contain backslash",
#endif /* MAC */
"codes for special effects.  Surround by { } to preserve leading or trailing",
#ifdef MAC
"spaces.  If text omitted, prompt reverts to C-Kermit>.",
#else
"spaces.  If text omitted, prompt reverts to Mac-Kermit>.",
#endif /* MAC */
"" };

static char *hxywind[] = {
"Syntax: SET WINDOW-SIZE number\n",
"Specify number of window slots for sliding windows, the number of packets",
"that can be transmitted before pausing for acknowledgement.  The default",
"is one, the maximum is 31.  Increased window size may result in reduced",
"maximum packet length.  Use sliding windows for improved efficiency on",
"connections with long delays.  A full duplex connection is required.",
"" };

static char *hxyrcv[] = { 
"Syntax: SET RECEIVE parameter value\n",
"Specify parameters for inbound packets:\n",
"END-OF-PACKET number",
" ASCII value of control character that terminates incoming packets,",
" normally 13 (carriage return).\n",
"PACKET-LENGTH number",
" Maximum length packet the other Kermit should send.\n",
"PADDING number",
" Number of prepacket padding characters to ask for (normally 0).\n",
"PAD-CHARACTER number",
" ASCII value of control character to use for padding (normally 0).\n",
"START-OF-PACKET number",
" ASCII value of character that marks start of inbound packet.\n",
"TIMEOUT number",
" Number of seconds other Kermit should wait for a packet before sending",
" NAK or retransmitting.",
"" };

static char *hxysnd[] = {
"Syntax: SET SEND parameter value\n",
"Specify parameters for outbound packets.  This command should be used only",
"to override the normal negotiated parameters and is rarely needed:\n",
"END-OF-PACKET number",
" ASCII value of control character to terminate an outbound packet,",
" normally 13 (carriage return).\n",
"PACKET-LENGTH number",
" Maximum length packet to send, even if other Kermit asks for longer ones.\n",
"PADDING number",
" Number of prepacket padding characters to send.\n",
"PAD-CHARACTER number",
" ASCII value of control character to use for padding.\n",
"START-OF-PACKET number",
" ASCII value of character to mark start of outbound packet.\n",
"TIMEOUT number",
" Number of seconds to wait for a packet before sending NAK or",
" retransmitting.",
"" };

static char *hxyxfer[] = {
"Syntax: SET TRANSFER LOCKING-SHIFT { OFF, ON, FORCED }\n",
"Tell whether locking-shift protocol should be used during file transfer",
"to achieve 8-bit transparency on a 7-bit connection.  ON means to request",
"its use if PARITY is not NONE and to use it if the other Kermit agrees,",
"OFF means not to use it, FORCED means to use it even if the other Kermit",
"does not agree.",
#ifndef NOCSETS
"\nSyntax: SET TRANSFER CHARACTER-SET name\n",
"Select the character set used to represent textual data in Kermit packets.",
"Text characters are translated to/from the FILE CHARACTER-SET.",
"The choices are TRANSPARENT (no translation, the default), ASCII,",
"LATIN1 (ISO Latin Alphabet 1), LATIN2 (ISO Latin Alphabet 2),",
#ifdef CYRILLIC
#ifdef KANJI
"CYRILLIC-ISO (ISO Latin/Cyrillic),",
"and JAPANESE-EUC.",
#else
"and CYRILLIC-ISO (ISO Latin/Cyrillic).",
#endif /* KANJI */
#else /* No CYRILLIC */
#ifdef KANJI
"and JAPANESE-EUC.",
#endif /* KANJI */
#endif /* CYRILLIC */
"Synonym: SET XFER CHARACTER-SET.", 
#endif /* NOCSETS */
"" };

/*  D O H S E T  --  Give help for SET command  */
 
int
dohset(xx) int xx; {
    int x;

    if (xx == -3) return(hmsga(hmhset));
    if (xx < 0) return(xx);
    if ((x = cmcfm()) < 0) return(x);
    switch (xx) {
 
case XYATTR:
    return(hmsga(hsetat));

case XYBACK:
    return(hmsga(hsetbkg));

#ifdef DYNAMIC
case XYBUF:
    return(hmsga(hsetbuf));
#endif /* DYNAMIC */

case XYCARR:
    return(hmsga(hsetcar));

#ifndef NOSPL
case XYCASE:
    return(hmsg("Syntax: SET CASE { ON, OFF }\n\
Tells whether alphabetic case is significant in string comparisons\n\
done by INPUT, IF, and other commands."));
#endif /* NOSPL */

case XYCMD:
    return(hmsga(hsetcmd));

case XYIFD:
    return(hmsg("Syntax: SET INCOMPLETE { DISCARD, KEEP }\n\
Discard or Keep incompletely received files, default is DISCARD."));

#ifndef NOSPL
case XYINPU:
    return(hmsga(hxxinp));
#endif /* NOSPL */

case XYCHKT:
    return(hmsga(hmxychkt));
 
#ifndef NOSPL
case XYCOUN:
    return(hmsg("Syntax:  SET COUNT number\n\
Example: SET COUNT 5\n\
Set up a loop counter, for use with IF COUNT."));
#endif /* NOSPL */

case XYDEBU:
#ifdef DEBUG    
    return(hmsg("Syntax: SET DEBUG { SESSION, ON, OFF }\n\
SESSION means display control and 8-bit characters symbolically during\n\
CONNECT mode.  ON means log debugging information to file debug.log."));
#else
    return(hmsg("Syntax: SET DEBUG { SESSION, OFF }\n\
SESSION means display control and 8-bit characters symbolically during\n\
CONNECT mode."));
#endif /* DEBUG */

case XYDFLT:
    return(hmsg("Syntax: SET DEFAULT directory\n\
Change directory.  Equivalent to CD command."));

case XYDELA: 
    return(hmsg("Syntax: SET DELAY number\n\
Number of seconds to wait before sending first packet after SEND command."));
 
#ifndef NODIAL
case XYDIAL:
    return(hmsga(hmxydial));
#endif /* NODIAL */

#ifdef UNIX
case XYSUSP:
    return(hmsg("Syntax: SET SUSPEND { OFF, ON }\n\n\
Disables SUSPEND command, suspend signals, and <esc-char>Z during CONNECT."));
#endif

#ifndef NOSCRIPT
case XYSCRI:
    return(hmsg("Syntax: SET SCRIPT ECHO { OFF, ON }\n\n\
Disables/Enables echoing of SCRIPT command operation."));
#endif /* NOSCRIPT */

case XYTAKE:
    return(hmsga(hxytak));

case XYTERM:
    return(hmsga(hxyterm));

case XYDUPL:
    return(hmsg("Syntax: SET DUPLEX { FULL, HALF }\n\n\
During CONNECT: FULL means remote host echoes, HALF means C-Kermit\n\
does its own echoing."));
 
case XYESC:
    return(hmsg("Syntax: SET ESCAPE number\n\n\
Decimal ASCII value for escape character during CONNECT, normally 28\n\
(Control-\\).  Type the escape character followed by C to get back to the\n\
C-Kermit prompt."));
 
case XYFILE:
    return(hmsga(hmxyf));
 
case XYFLOW:
    return(hmsga(hmxyflo));
 
case XYHAND:
   return(hmsga(hmxyhsh));

#ifdef NETCONN
case XYHOST:
return(hmsga(hxyhost));
case XYNET:
return(hmsga(hxynet));

#ifdef SUNX25
case XYX25:
    return(hmsga(hxyx25));

case XYPAD:
    return(hmsg("Syntax: SET PAD name value\n\
Set a PAD X.3 parameter with a desired value."));
#endif /* SUNX25 */ 
#endif /* NETCONN */

#ifndef NOSETKEY
case XYKEY:				/* SET KEY */
    return(hmsga(hmhskey));
#endif /* NOSETKEY */

#ifndef NOCSETS
case XYLANG:
    return(hmsg("Syntax: SET LANGUAGE name\n\
Selects language-specific translation rules for text-mode file transfers.\n\
Used with SET FILE CHARACTER-SET and SET TRANSFER CHARACTER-SET when one\n\
of these is ASCII."));
#endif /* NOCSETS */

case XYLINE:
    printf("\nSyntax: SET LINE devicename\n\n\
Select communication device to use.  Normally %s.\n",dftty);
    if (!dfloc) {
	printf("\
If you SET LINE to other than %s, then Kermit\n",dftty);
	printf("\
will be in 'local' mode; SET LINE alone will reset Kermit to remote mode.\n\
To use the modem to dial out, first SET MODEM-DIALER (e.g., to HAYES), then");
puts("\nSET LINE xxx, next issue the DIAL command, and finally CONNECT.\n");
    }
    return(0);
 
#ifndef NOSPL
case XYMACR:
    return(hmsga(hxymacr));
#endif /* NOSPL */

#ifndef NODIAL
case XYMODM:
    return(hmsga(hxymodm));
#endif /* NODIAL */
 
case XYPARI:
    return(hmsg("Syntax: SET PARITY name\n\n\
Parity to use during terminal connection and file transfer: EVEN, ODD, MARK,\n\
SPACE, or NONE.  Normally NONE."));
 
case XYPROM:
    return(hmsga(hmxyprm));
 
case XYQUIE:
    return(hmsg("Syntax: SET QUIET {ON, OFF}\n\n\
Normally OFF.  ON disables most information messages during interactive\n\
operation."));

case XYRETR:
    return(hmsg("Syntax: SET RETRY number\n\n\
How many times to retransmit a particular packet before giving up."));

#ifdef UNIX
case XYSESS:
    return(hmsg("Syntax: SET SESSION-LOG { BINARY, TEXT }\n\n\
If BINARY, record all CONNECT characters in session log.  If TEXT, strip\n\
out carriage returns."));
#endif /* UNIX */

case XYSPEE:
    return(hmsg("Syntax: SET SPEED number\n\n\
Communication line speed for external tty line specified in most recent\n\
SET LINE command, in bits per second.  Type SET SPEED ? for a list of\n\
possible speeds."));

case XYRECV:
    return(hmsga(hxyrcv));
case XYSEND:
    return(hmsga(hxysnd));

#ifndef NOSERVER
case XYSERV:
    return(hmsga(hsetsrv));
#endif /* NOSERVER */

#ifdef TNCODE
case XYTEL:
    return(hmsga(hxytel));
#endif /* TNCODE */

#ifndef NOXMIT
case XYXMIT:
    return(hmsga(hsetxmit));
#endif /* NOXMIT */

#ifndef NOCSETS
case XYUNCS:
    return(hmsg("Syntax: SET UNKNOWN-CHAR-SET action\n\n\
DISCARD (default) means reject any arriving files encoded in unknown\n\
character sets.  KEEP means to accept them anyway."));
#endif /* NOCSETS */

#ifdef UNIX
case XYWILD:
    return(hmsg("Syntax: SET WILDCARD-EXPANSION { KERMIT, SHELL }\n\n\
KERMIT (the default) means C-Kermit expands filename wildcards in SEND\n\
and MSEND commands and incoming GET commands.  SHELL means your shell does \
it."));
#endif /* UNIX */

case XYWIND:
    return(hmsga(hxywind));

case XYXFER:
    return(hmsga(hxyxfer));

default:
    printf("Not available yet - %s\n",cmdbuf);
    return(0);
    }
}

 
/*  D O H R M T  --  Give help about REMOTE command  */
 
static char *hrset[] = {
"Syntax:  REMOTE SET parameter value",
"Example: REMOTE SET FILE TYPE BINARY\n",
"Ask the remote Kermit server to set the named parameter to the given value.",
"Equivalent to typing the corresponding SET command directly to the other",
"Kermit if it were in interactive mode.", "" };

int
dohrmt(xx) int xx; {
    int x;
    if (xx == -3) return(hmsga(hmhrmt));
    if (xx < 0) return(xx);
    if ((x = cmcfm()) < 0) return(x);
    switch (xx) {
 
case XZCWD:
    return(hmsg("Syntax: REMOTE CD [ name ]\n\n\
Ask remote Kermit server to change its working directory or device.\n\
If the device or directory name is omitted, restore the default."));
 
case XZDEL:
    return(hmsg("Syntax: REMOTE DELETE filespec\n\n\
Ask the remote Kermit server to delete the named file(s)."));
 
case XZDIR:
    return(hmsg("Syntax: REMOTE DIRECTORY [ filespec ]\n\n\
Ask the remote Kermit server to provide a directory listing of the named\n\
file(s) or if no file specification is given, of all files in the current\n\
directory."));
 
case XZHLP:
    return(hmsg("Syntax: REMOTE HELP\n\n\
Ask the remote Kermit server to list the services it provides."));
 
case XZHOS:
    return(hmsg("Syntax: REMOTE HOST command\n\n\
Send a command to the remote host computer in its own command language\n\
through the remote Kermit server."));
 
#ifndef NOFRILLS
case XZKER:
    return(hmsg("Syntax: REMOTE KERMIT command\n\n\
Send a command to the remote Kermit server in its own command language."));

case XZLGI:
    return(hmsg("Syntax: REMOTE LOGIN user password [ account ]\n\n\
Log in to a remote Kermit server that requires you login."));

case XZLGO:
    return(hmsg("Syntax: REMOTE LOGOUT\n\n\
Log out from a remote Kermit server to which you have previously logged in."));

case XZPRI:
    return(hmsg("Syntax: REMOTE PRINT filespec [ options ]\n\n\
Send the specified file(s) to the remote Kermit and ask it to have the\n\
file printed on the remote system's printer, using any specified options."));
#endif /* NOFRILLS */

case XZSET:
    return(hmsga(hrset));

case XZSPA:
    return(hmsg("Syntax: REMOTE SPACE [ name ]\n\n\
Ask the remote Kermit server to tell you about its disk space on the current\n\
disk or directory, or in the one that you name."));
 
#ifndef NOFRILLS
case XZTYP:
    return(hmsg("Syntax: REMOTE TYPE file\n\n\
Ask the remote Kermit server to type the named file(s) on your screen."));
 
case XZWHO:
    return(hmsg("Syntax: REMOTE WHO [ name ]\n\n\
Ask the remote Kermit server to list who's logged in, or to give information\n\
about the named user."));
#endif /* NOFRILLS */
 
default:
    if ((x = cmcfm()) < 0) return(x);
    printf("not working yet - %s\n",cmdbuf);
    return(-2);
    }
}

#endif /* NOHELP */
#endif /* NOICP */
