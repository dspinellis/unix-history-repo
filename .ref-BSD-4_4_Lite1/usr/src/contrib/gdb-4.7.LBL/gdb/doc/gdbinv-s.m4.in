_dnl__								-*- Texinfo -*-
_dnl__ Copyright (c) 1990 1991 1992 Free Software Foundation, Inc.
_dnl__ This file is part of the source for the GDB manual.
_dnl__ M4 FRAGMENT $Id: gdbinv-s.m4.in,v 2.13 1992/10/23 08:50:19 grossman Exp $
_dnl__ This text diverted to "Remote Debugging" section in general case;
_dnl__ however, if we're doing a manual specifically for one of these, it
_dnl__ belongs up front (in "Getting In and Out" chapter).
_if__(_REMOTESTUB__)
@node Remote Serial
@subsection The _GDBN__ remote serial protocol

@cindex remote serial debugging, overview
To debug a program running on another machine (the debugging
@dfn{target} machine), you must first arrange for all the usual
prerequisites for the program to run by itself.  For example, for a C
program, you need

@enumerate
@item
A startup routine to set up the C runtime environment; these usually
have a name like @file{crt0}.  The startup routine may be supplied by
your hardware supplier, or you may have to write your own.

@item 
You probably need a C subroutine library to support your program's
subroutine calls, notably managing input and output.

@item
A way of getting your program to the other machine---for example, a
download program.  These are often supplied by the hardware
manufacturer, but you may have to write your own from hardware
documentation.
@end enumerate

The next step is to arrange for your program to use a serial port to
communicate with the machine where _GDBN__ is running (the @dfn{host}
machine).  In general terms, the scheme looks like this:

@table @emph
@item On the host,
_GDBN__ already understands how to use this protocol; when everything
else is set up, you can simply use the @samp{target remote} command
(@pxref{Targets,,Specifying a Debugging Target}).

@item On the target,
you must link with your program a few special-purpose subroutines that
implement the _GDBN__ remote serial protocol.  The file containing these
subroutines is called  a @dfn{debugging stub}.
@end table

The debugging stub is specific to the architecture of the remote
machine; for example, use @file{sparc-stub.c} to debug programs on
@sc{sparc} boards.

@cindex remote serial stub list
These working remote stubs are distributed with _GDBN__:

@c FIXME! verify these...
@table @code
@item sparc-stub.c
@kindex sparc-stub.c
For @sc{sparc} architectures.

@item m68k-stub.c
@kindex m68-stub.c
For Motorola 680x0 architectures.

@item i386-stub.c
@kindex i36-stub.c
For Intel 386 and compatible architectures.
@end table

The @file{README} file in the _GDBN__ distribution may list other
recently added stubs.

@menu
* stub contents::       What the stub can do for you
* bootstrapping::       What you must do for the stub
* debug session::       Putting it all together
* protocol::            Outline of the communication protocol
@end menu

@node stub contents
@subsubsection What the stub can do for you

@cindex remote serial stub
The debugging stub for your architecture supplies these three
subroutines:

@table @code
@item set_debug_traps
@kindex set_debug_traps
@cindex remote serial stub, initialization
This routine arranges to transfer control to @code{handle_exception}
when your program stops.  You must call this subroutine explicitly near
the beginning of your program.

@item handle_exception
@kindex handle_exception
@cindex remote serial stub, main routine
This is the central workhorse, but your program never calls it
explicitly---the setup code arranges for @code{handle_exception} to
run when a trap is triggered.

@code{handle_exception} takes control when your program stops during
execution (for example, on a breakpoint), and mediates communications
with _GDBN__ on the host machine.  This is where the communications
protocol is implemented; @code{handle_exception} acts as the _GDBN__
representative on the target machine; it begins by sending summary
information on the state of your program, then continues to execute,
retrieving and transmitting any information _GDBN__ needs, until you
execute a _GDBN__ command that makes your program resume; at that point,
@code{handle_exception} returns control to your own code on the target
machine. 

@item breakpoint
@cindex @code{breakpoint} subroutine, remote
Use this auxiliary subroutine to make your program contain a
breakpoint.  Depending on the particular situation, this may be the only
way for _GDBN__ to get control.  For instance, if your target
machine has some sort of interrupt button, you won't need to call this;
pressing the interrupt button will transfer control to
@code{handle_exception}---in efect, to _GDBN__.  On some machines,
simply receiving characters on the serial port may also trigger a trap;
again, in that situation, you don't need to call @code{breakpoint} from
your own program---simply running @samp{target remote} from the host
_GDBN__ session will get control.  

Call @code{breakpoint} if none of these is true, or if you simply want
to make certain your program stops at a predetermined point for the
start of your debugging session.
@end table

@node bootstrapping
@subsubsection What you must do for the stub

@cindex remote stub, support routines
The debugging stubs that come with _GDBN__ are set up for a particular
chip architecture, but they have no information about the rest of your
debugging target machine.  To allow the stub to work, you must supply
these special low-level subroutines:

@table @code
@item int getDebugChar()
@kindex getDebugChar
Write this subroutine to read a single character from the serial port.
It may be identical to @code{getchar} for your target system; a
different name is used to allow you to distinguish the two if you wish.

@item void putDebugChar(int)
@kindex putDebugChar
Write this subroutine to write a single character to the serial port.
It may be identical to @code{putchar} for your target system; a 
different name is used to allow you to distinguish the two if you wish.

@item void flush_i_cache()
@kindex flush_i_cache
Write this subroutine to flush the instruction cache, if any, on your
target machine.  If there is no instruction cache, this subroutine may
be a no-op.

On target machines that have instruction caches, _GDBN__ requires this
function to make certain that the state of your program is stable.
@end table

@noindent
You must also make sure this library routine is available:

@table @code
@item void *memset(void *, int, int)
@kindex memset
This is the standard library function @code{memset} that sets an area of
memory to a known value.  If you have one of the free versions of
@code{libc.a}, @code{memset} can be found there; otherwise, you must
either obtain it from your hardware manufacturer, or write your own.
@end table

If you do not use the GNU C compiler, you may need other standard
library subroutines as well; this will vary from one stub to another,
but in general the stubs are likely to use any of the common library
subroutines which @code{gcc} generates as inline code.


@node debug session
@subsubsection Putting it all together

@cindex remote serial debugging summary
In summary, when your program is ready to debug, you must follow these
steps.

@enumerate
@item
Make sure you have the supporting low-level routines:
@code{getDebugChar}, @code{putDebugChar}, @code{flush_i_cache},
@code{memset}.

@item
Insert these lines near the top of your program:

@example
set_debug_traps();
breakpoint();
@end example

@item
Compile and link together: your program, the _GDBN__ debugging stub for
your target architecture, and the supporting subroutines.

@item
Make sure you have a serial connection between your target machine and
the _GDBN__ host, and identify the serial port used for this on the host.

@item
Download your program to your target machine (or get it there by
whatever means the manufacturer provides), and start it.

@item
To start remote debugging, run _GDBN__ on the host machine, and specify
as an executable file the program that is running in the remote machine.
This tells _GDBN__ how to find your program's symbols and the contents
of its pure text.

Then establish communication using the @code{target remote} command.
Its argument is the name of the device you're using to control the
target machine.  For example:

@example
target remote /dev/ttyb
@end example

@noindent
if the serial line is connected to the device named @file{/dev/ttyb}.  
@ignore
@c this is from the old text, but it doesn't seem to make sense now that I've
@c seen an example...  pesch 4sep1992
This will stop the remote machine if it is not already stopped.
@end ignore

@end enumerate

Now you can use all the usual commands to examine and change data and to
step and continue the remote program.

To resume the remote program and stop debugging it, use the @code{detach}
command.

@node protocol
@subsubsection Outline of the communication protocol

@cindex debugging stub, example
@cindex remote stub, example
@cindex stub example, remote debugging
The stub files provided with _GDBN__ implement the target side of the
communication protocol, and the _GDBN__ side is implemented in the
_GDBN__ source file @file{remote.c}.  Normally, you can simply allow
these subroutines to communicate, and ignore the details.  (If you're
implementing your own stub file, you can still ignore the details: start
with one of the existing stub files.  @file{sparc-stub.c} is the best
organized, and therefore the easiest to read.)

However, there may be occasions when you need to know something about
the protocol---for example, if there is only one serial port to your
target machine, you might want your program to do something special if
it recognizes a packet meant for _GDBN__.

@cindex protocol, _GDBN__ remote serial
@cindex serial protocol, _GDBN__ remote
@cindex remote serial protocol
All _GDBN__ commands and responses (other than acknowledgements, which
are single characters) are sent as a packet which includes a
checksum.  A packet is introduced with the character @samp{$}, and ends
with the character @samp{#} followed by a two-digit checksum:

@example
$@var{packet info}#@var{checksum}
@end example

@cindex checksum, for _GDBN__ remote
@noindent
@var{checksum} is computed as the modulo 256 sum of the @var{packet
info} characters.

When either the host or the target machine receives a packet, the first
response expected is an acknowledgement: a single character, either
@samp{+} (to indicate the package was received correctly) or @samp{-}
(to request retransmission).

The host (_GDBN__) sends commands, and the target (the debugging stub
incorporated in your program) sends data in response.  The target also
sends data when your program stops.

Command packets are distinguished by their first character, which
identifies the kind of command.

These are the commands currently supported:

@table @code
@item g
Requests the values of CPU registers.

@item G
Sets the values of CPU registers.

@item m@var{addr},@var{count}
Read @var{count} bytes at location @var{addr}.

@item M@var{addr},@var{count}:@dots{}
Write @var{count} bytes at location @var{addr}.

@item c
@itemx c@var{addr}
Resume execution at the current address (or at @var{addr} if supplied).

@item s
@itemx s@var{addr}
Step the target program for one instruction, from either the current
program counter or from @var{addr} if supplied.

@item k
Kill the target program.

@item ?
Report the most recent signal.  To allow you to take advantage of the
_GDBN__ signal handling commands, one of the functions of the debugging
stub is to report CPU traps as the corresponding POSIX signal values.
@end table

@kindex set remotedebug
@kindex show remotedebug
@cindex packets, reporting on stdout
@cindex serial connections, debugging
If you have trouble with the serial connection, you can use the command
@code{set remotedebug}.  This makes _GDBN__ report on all packets sent
back and forth across the serial line to the remote machine.  The
packet-debugging information is printed on the _GDBN__ standard output
stream.  @code{set remotedebug off} turns it off, and @code{show
remotedebug} will show you its current state.
_fi__(_REMOTESTUB__)

_if__(_I960__)
@node i960-Nindy Remote
@subsection _GDBN__ with a Remote i960 (Nindy)

@cindex Nindy
@cindex i960
@dfn{Nindy} is a ROM Monitor program for Intel 960 target systems.  When
_GDBN__ is configured to control a remote Intel 960 using Nindy, you can
tell _GDBN__ how to connect to the 960 in several ways:

@itemize @bullet
@item
Through command line options specifying serial port, version of the
Nindy protocol, and communications speed;

@item
By responding to a prompt on startup;

@item
By using the @code{target} command at any point during your _GDBN__
session.  @xref{Target Commands, ,Commands for Managing Targets}.

@end itemize

@menu
* Nindy Startup::               Startup with Nindy
* Nindy Options::               Options for Nindy
* Nindy reset::                 Nindy Reset Command
@end menu

@node Nindy Startup
@subsubsection Startup with Nindy

If you simply start @code{_GDBP__} without using any command-line
options, you are prompted for what serial port to use, @emph{before} you
reach the ordinary _GDBN__ prompt:

@example
Attach /dev/ttyNN -- specify NN, or "quit" to quit:  
@end example

@noindent
Respond to the prompt with whatever suffix (after @samp{/dev/tty})
identifies the serial port you want to use.  You can, if you choose,
simply start up with no Nindy connection by responding to the prompt
with an empty line.  If you do this, and later wish to attach to Nindy,
use @code{target} (@pxref{Target Commands, ,Commands for Managing Targets}).

@node Nindy Options
@subsubsection Options for Nindy

These are the startup options for beginning your _GDBN__ session with a
Nindy-960 board attached:

@table @code
@item -r @var{port}
Specify the serial port name of a serial interface to be used to connect
to the target system.  This option is only available when _GDBN__ is
configured for the Intel 960 target architecture.  You may specify
@var{port} as any of: a full pathname (e.g. @samp{-r /dev/ttya}), a
device name in @file{/dev} (e.g. @samp{-r ttya}), or simply the unique
suffix for a specific @code{tty} (e.g. @samp{-r a}).

@item -O
(An uppercase letter ``O'', not a zero.)  Specify that _GDBN__ should use
the ``old'' Nindy monitor protocol to connect to the target system.
This option is only available when _GDBN__ is configured for the Intel 960
target architecture.

@quotation
@emph{Warning:} if you specify @samp{-O}, but are actually trying to
connect to a target system that expects the newer protocol, the connection
will fail, appearing to be a speed mismatch.  _GDBN__ will repeatedly
attempt to reconnect at several different line speeds.  You can abort
this process with an interrupt.
@end quotation

@item -brk
Specify that _GDBN__ should first send a @code{BREAK} signal to the target
system, in an attempt to reset it, before connecting to a Nindy target.

@quotation
@emph{Warning:} Many target systems do not have the hardware that this
requires; it only works with a few boards.
@end quotation
@end table

The standard @samp{-b} option controls the line speed used on the serial
port.

@c @group
@node Nindy reset
@subsubsection Nindy Reset Command

@table @code
@item reset
@kindex reset
For a Nindy target, this command sends a ``break'' to the remote target
system; this is only useful if the target has been equipped with a
circuit to perform a hard reset (or some other interesting action) when
a break is detected.
@end table
@c @end group
_fi__(_I960__)

_if__(_AMD29K__)
@node EB29K Remote
@subsection _GDBN__ with a Remote EB29K

@cindex EB29K board
@cindex running 29K programs

To use _GDBN__ from a Unix system to run programs on AMD's EB29K
board in a PC, you must first connect a serial cable between the PC
and a serial port on the Unix system.  In the following, we assume
you've hooked the cable between the PC's @file{COM1} port and
@file{/dev/ttya} on the Unix system.

@menu
* Comms (EB29K)::               Communications Setup
* _GDBP__-EB29K::                   EB29K cross-debugging
* Remote Log::                  Remote Log
@end menu

@node Comms (EB29K)
@subsubsection Communications Setup

The next step is to set up the PC's port, by doing something like the
following in DOS on the PC:

_0__@example
C:\> MODE com1:9600,n,8,1,none
_1__@end example

@noindent
This example---run on an MS DOS 4.0 system---sets the PC port to 9600
bps, no parity, eight data bits, one stop bit, and no ``retry'' action;
you must match the communications parameters when establishing the Unix
end of the connection as well.
@c FIXME: Who knows what this "no retry action" crud from the DOS manual may
@c       mean?  It's optional; leave it out? ---pesch@cygnus.com, 25feb91 

To give control of the PC to the Unix side of the serial line, type
the following at the DOS console:

_0__@example
C:\> CTTY com1
_1__@end example

@noindent
(Later, if you wish to return control to the DOS console, you can use
the command @code{CTTY con}---but you must send it over the device that
had control, in our example over the @file{COM1} serial line).

From the Unix host, use a communications program such as @code{tip} or
@code{cu} to communicate with the PC; for example,

@example
cu -s 9600 -l /dev/ttya
@end example

@noindent
The @code{cu} options shown specify, respectively, the linespeed and the
serial port to use.  If you use @code{tip} instead, your command line
may look something like the following:

@example
tip -9600 /dev/ttya
@end example

@noindent
Your system may define a different name where our example uses
@file{/dev/ttya} as the argument to @code{tip}.  The communications
parameters, including which port to use, are associated with the
@code{tip} argument in the ``remote'' descriptions file---normally the
system table @file{/etc/remote}.
@c FIXME: What if anything needs doing to match the "n,8,1,none" part of
@c the DOS side's comms setup?  cu can support -o (odd
@c parity), -e (even parity)---apparently no settings for no parity or
@c for character size.  Taken from stty maybe...?  John points out tip
@c can set these as internal variables, eg ~s parity=none; man stty
@c suggests that it *might* work to stty these options with stdin or
@c stdout redirected... ---pesch@cygnus.com, 25feb91

@kindex EBMON
Using the @code{tip} or @code{cu} connection, change the DOS working
directory to the directory containing a copy of your 29K program, then
start the PC program @code{EBMON} (an EB29K control program supplied
with your board by AMD).  You should see an initial display from
@code{EBMON} similar to the one that follows, ending with the
@code{EBMON} prompt @samp{#}---

_0__@example
C:\> G:

G:\> CD \usr\joe\work29k

G:\USR\JOE\WORK29K> EBMON
Am29000 PC Coprocessor Board Monitor, version 3.0-18
Copyright 1990 Advanced Micro Devices, Inc.
Written by Gibbons and Associates, Inc.

Enter '?' or 'H' for help

PC Coprocessor Type   = EB29K
I/O Base              = 0x208
Memory Base           = 0xd0000

Data Memory Size      = 2048KB
Available I-RAM Range = 0x8000 to 0x1fffff
Available D-RAM Range = 0x80002000 to 0x801fffff

PageSize              = 0x400
Register Stack Size   = 0x800
Memory Stack Size     = 0x1800

CPU PRL               = 0x3
Am29027 Available     = No
Byte Write Available  = Yes

# ~.
_1__@end example

Then exit the @code{cu} or @code{tip} program (done in the example by
typing @code{~.} at the @code{EBMON} prompt).  @code{EBMON} will keep
running, ready for _GDBN__ to take over.

For this example, we've assumed what is probably the most convenient
way to make sure the same 29K program is on both the PC and the Unix
system: a PC/NFS connection that establishes ``drive @code{G:}'' on the
PC as a file system on the Unix host.  If you do not have PC/NFS or
something similar connecting the two systems, you must arrange some
other way---perhaps floppy-disk transfer---of getting the 29K program
from the Unix system to the PC; _GDBN__ will @emph{not} download it over the
serial line.

@node _GDBP__-EB29K
@subsubsection EB29K cross-debugging

Finally, @code{cd} to the directory containing an image of your 29K
program on the Unix system, and start _GDBN__---specifying as argument the
name of your 29K program:

@example
cd /usr/joe/work29k
_GDBP__ myfoo
@end example

Now you can use the @code{target} command:

@example
target amd-eb /dev/ttya 9600 MYFOO
@c FIXME: test above 'target amd-eb' as spelled, with caps!  caps are meant to
@c emphasize that this is the name as seen by DOS (since I think DOS is
@c single-minded about case of letters).  ---pesch@cygnus.com, 25feb91
@end example

@noindent
In this example, we've assumed your program is in a file called
@file{myfoo}.  Note that the filename given as the last argument to
@code{target amd-eb} should be the name of the program as it appears to DOS.
In our example this is simply @code{MYFOO}, but in general it can include
a DOS path, and depending on your transfer mechanism may not resemble
the name on the Unix side.

At this point, you can set any breakpoints you wish; when you are ready
to see your program run on the 29K board, use the _GDBN__ command
@code{run}.

To stop debugging the remote program, use the _GDBN__ @code{detach}
command.

To return control of the PC to its console, use @code{tip} or @code{cu}
once again, after your _GDBN__ session has concluded, to attach to
@code{EBMON}.  You can then type the command @code{q} to shut down
@code{EBMON}, returning control to the DOS command-line interpreter.
Type @code{CTTY con} to return command input to the main DOS console,
and type @kbd{~.} to leave @code{tip} or @code{cu}.

@node Remote Log
@subsubsection Remote Log
@kindex eb.log
@cindex log file for EB29K

The @code{target amd-eb} command creates a file @file{eb.log} in the
current working directory, to help debug problems with the connection.
@file{eb.log} records all the output from @code{EBMON}, including echoes
of the commands sent to it.  Running @samp{tail -f} on this file in
another window often helps to understand trouble with @code{EBMON}, or
unexpected events on the PC side of the connection.

_fi__(_AMD29K__)

_if__(_ST2000__)
@node ST2000 Remote
@subsection _GDBN__ with a Tandem ST2000

To connect your ST2000 to the host system, see the manufacturer's
manual.  Once the ST2000 is physically attached, you can run

@example
target st2000 @var{dev} @var{speed}
@end example

@noindent
to establish it as your debugging environment.  

The @code{load} and @code{attach} commands are @emph{not} defined for
this target; you must load your program into the ST2000 as you normally
would for standalone operation.  _GDBN__ will read debugging information
(such as symbols) from a separate, debugging version of the program
available on your host computer.
@c FIXME!! This is terribly vague; what little content is here is
@c basically hearsay.

@cindex ST2000 auxiliary commands
These auxiliary _GDBN__ commands are available to help you with the ST2000
environment:

@table @code
@item st2000 @var{command}
@kindex st2000 @var{cmd}
@cindex STDBUG commands (ST2000)
@cindex commands to STDBUG (ST2000)
Send a @var{command} to the STDBUG monitor.  See the manufacturer's
manual for available commands.

@item connect
@cindex connect (to STDBUG)
Connect the controlling terminal to the STDBUG command monitor.  When
you are done interacting with STDBUG, typing either of two character
sequences will get you back to the _GDBN__ command prompt:
@kbd{@key{RET}~.} (Return, followed by tilde and period) or
@kbd{@key{RET}~@key{C-d}} (Return, followed by tilde and control-D).
@end table
_fi__(_ST2000__)

_if__(_VXWORKS__)
@node VxWorks Remote
@subsection _GDBN__ and VxWorks
@cindex VxWorks

_GDBN__ enables developers to spawn and debug tasks running on networked
VxWorks targets from a Unix host.  Already-running tasks spawned from
the VxWorks shell can also be debugged.  _GDBN__ uses code that runs on
both the UNIX host and on the VxWorks target.  The program
@code{_GDBP__} is installed and executed on the UNIX host.

The following information on connecting to VxWorks was current when
this manual was produced; newer releases of VxWorks may use revised
procedures.

The remote debugging interface (RDB) routines are installed and executed
on the VxWorks target.  These routines are included in the VxWorks library
@file{rdb.a} and are incorporated into the system image when source-level
debugging is enabled in the VxWorks configuration.

@kindex INCLUDE_RDB
If you wish, you can define @code{INCLUDE_RDB} in the VxWorks
configuration file @file{configAll.h} to include the RDB interface
routines and spawn the source debugging task @code{tRdbTask} when
VxWorks is booted.  For more information on configuring and remaking
_if__(_FSF__)
VxWorks, see the manufacturer's manual.
_fi__(_FSF__)
_if__(!_FSF__)
VxWorks, see the @cite{VxWorks Programmer's Guide}.
_fi__(!_FSF__)

Once you have included the RDB interface in your VxWorks system image
and set your Unix execution search path to find _GDBN__, you are ready
to run _GDBN__.  From your UNIX host, type:

@smallexample
% _GDBP__
@end smallexample

_GDBN__ will come up showing the prompt:

@smallexample
(_GDBP__)
@end smallexample

@menu
* VxWorks connection::          Connecting to VxWorks
* VxWorks download::            VxWorks Download
* VxWorks attach::              Running Tasks
@end menu

@node VxWorks connection
@subsubsection Connecting to VxWorks

The _GDBN__ command @code{target} lets you connect to a VxWorks target on the
network.  To connect to a target whose host name is ``@code{tt}'', type:

@smallexample
(_GDBP__) target vxworks tt
@end smallexample

_GDBN__ will display a message similar to the following:

@smallexample
Attaching remote machine across net... Success!
@end smallexample

_GDBN__ will then attempt to read the symbol tables of any object modules
loaded into the VxWorks target since it was last booted.  _GDBN__ locates
these files by searching the directories listed in the command search
path (@pxref{Environment, ,Your Program's Environment}); if it fails
to find an object file, it will display a message such as:

@smallexample
prog.o: No such file or directory.
@end smallexample

This will cause the @code{target} command to abort.  When this happens,
you should add the appropriate directory to the search path, with the
_GDBN__ command @code{path}, and execute the @code{target} command
again.

@node VxWorks download
@subsubsection VxWorks Download

@cindex download to VxWorks
If you have connected to the VxWorks target and you want to debug an
object that has not yet been loaded, you can use the _GDBN__ @code{load}
command to download a file from UNIX to VxWorks incrementally.  The
object file given as an argument to the @code{load} command is actually
opened twice: first by the VxWorks target in order to download the code,
then by _GDBN__ in order to read the symbol table.  This can lead to
problems if the current working directories on the two systems differ.
It is simplest to set the working directory on both systems to the
directory in which the object file resides, and then to reference the
file by its name, without any path.  Thus, to load a program
@file{prog.o}, residing in @file{wherever/vw/demo/rdb}, on VxWorks type:

@smallexample
-> cd "wherever/vw/demo/rdb"
@end smallexample

On _GDBN__ type:

@smallexample
(_GDBP__) cd wherever/vw/demo/rdb 
(_GDBP__) load prog.o
@end smallexample

_GDBN__ will display a response similar to the following:

@smallexample
Reading symbol data from wherever/vw/demo/rdb/prog.o... done.
@end smallexample

You can also use the @code{load} command to reload an object module
after editing and recompiling the corresponding source file.  Note that
this will cause _GDBN__ to delete all currently-defined breakpoints,
auto-displays, and convenience variables, and to clear the value
history.  (This is necessary in order to preserve the integrity of
debugger data structures that reference the target system's symbol
table.)

@node VxWorks attach
@subsubsection Running Tasks

@cindex running VxWorks tasks
You can also attach to an existing task using the @code{attach} command as
follows:

@smallexample
(_GDBP__) attach @var{task}
@end smallexample

@noindent
where @var{task} is the VxWorks hexadecimal task ID.  The task can be running
or suspended when you attach to it.  If running, it will be suspended at
the time of attachment.
_fi__(_VXWORKS__)

_if__(_H8__)
@node Hitachi H8/300 Remote
@subsection _GDBN__ and the Hitachi H8/300
_GDBN__ needs to know these things to talk to your H8/300: 

@enumerate
@item
that you want to use @samp{target hms}, the remote debugging
interface for the H8/300 (this is the default when
GDB is configured specifically for the H8/300);

@item
what serial device connects your host to your H8/300 (the first serial
device available on your host is the default);

@ignore
@c this is only for Unix hosts, not currently of interest.
@item
what speed to use over the serial device.
@end ignore
@end enumerate

@kindex device
@cindex serial device for H8/300
@ignore
@c only for Unix hosts
Use the special @code{gdb83} command @samp{device @var{port}} if you
need to explicitly set the serial device.  The default @var{port} is the
first available port on your host.  This is only necessary on Unix
hosts, where it is typically something like @file{/dev/ttya}.

@kindex speed
@cindex serial line speed for H8/300
@code{gdb83} has another special command to set the communications speed
for the H8/300: @samp{speed @var{bps}}.  This command also is only used
from Unix hosts; on DOS hosts, set the line speed as usual from outside
GDB with the DOS @kbd{mode} command (for instance, @w{@samp{mode
com2:9600,n,8,1,p}} for a 9600 bps connection).
@end ignore

_GDBN__ depends on an auxiliary terminate-and-stay-resident program
called @code{asynctsr} to communicate with the H8/300 development board
through a PC serial port.  You must also use the DOS @code{mode} command
to set up the serial port on the DOS side.

The following sample session illustrates the steps needed to start a
program under _GDBN__ control on your H8/300.  The example uses a sample
H8/300 program called @file{t.x}.

First hook up your H8/300 development board.  In this example, we use a
board attached to serial port @code{COM2}; if you use a different serial
port, substitute its name in the argument of the @code{mode} command.
When you call @code{asynctsr}, the auxiliary comms program used by the
degugger, you give it just the numeric part of the serial port's name;
for example, @samp{asyncstr 2} below runs @code{asyncstr} on
@code{COM2}.

@smallexample
(eg-C:\H8300\TEST) mode com2:9600,n,8,1,p

Resident portion of MODE loaded

COM2: 9600, n, 8, 1, p

(eg-C:\H8300\TEST) asynctsr 2
@end smallexample

@quotation
@emph{Warning:} We have noticed a bug in PC-NFS that conflicts with
@code{asynctsr}.  If you also run PC-NFS on your DOS host, you may need to
disable it, or even boot without it, to use @code{asynctsr} to control
your H8/300 board.
@end quotation

Now that serial communications are set up, and the H8/300 is connected,
you can start up _GDBN__.  Call @code{_GDBP__} with the name of your
program as the argument.  @code{_GDBP__} prompts you, as usual, with the
prompt @samp{(_GDBP__)}.  Use two special commands to begin your debugging
session: @samp{target hms} to specify cross-debugging to the Hitachi board,
and the @code{load} command to download your program to the board.
@code{load} displays the names of the
program's sections, and a @samp{*} for each 2K of data downloaded.  (If
you want to refresh _GDBN__ data on symbols or on the executable file
without downloading, use the _GDBN__ commands @code{file} or
@code{symbol-file}.  These commands, and @code{load} itself, are
described in @ref{Files,,Commands to Specify Files}.)

@smallexample
(eg-C:\H8300\TEST) _GDBP__ t.x
GDB is free software and you are welcome to distribute copies
 of it under certain conditions; type "show copying" to see 
 the conditions.
There is absolutely no warranty for GDB; type "show warranty" 
for details.
GDB _GDB_VN__, Copyright 1992 Free Software Foundation, Inc...
(gdb) target hms
Connected to remote H8/300 HMS system.
(gdb) load t.x
.text   : 0x8000 .. 0xabde ***********
.data   : 0xabde .. 0xad30 *
.stack  : 0xf000 .. 0xf014 *
@end smallexample

At this point, you're ready to run or debug your program.  From here on,
you can use all the usual _GDBN__ commands.  The @code{break} command
sets breakpoints; the @code{run} command starts your program;
@code{print} or @code{x} display data; the @code{continue} command
resumes execution after stopping at a breakpoint.  You can use the
@code{help} command at any time to find out more about _GDBN__ commands.

Remember, however, that @emph{operating system} facilities aren't
available on your H8/300; for example, if your program hangs, you can't
send an interrupt---but you can press the @sc{reset} switch!  

Use the @sc{reset} button on the H8/300 board
@itemize @bullet
@item
to interrupt your program (don't use @kbd{ctl-C} on the DOS host---it has
no way to pass an interrupt signal to the H8/300); and

@item
to return to the _GDBN__ command prompt after your program finishes
normally.  The communications protocol provides no other way for _GDBN__
to detect program completion.
@end itemize

In either case, _GDBN__ will see the effect of a @sc{reset} on the
H8/300 board as a ``normal exit'' of your program.
_fi__(_H8__)
