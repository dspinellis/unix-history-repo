.(l C
/etc/connectd: Implementation Notes
Bill Jolitz
.l)
.uh "Introduction"
.pp
Many UNIX programs have duplicate mechanisms for obtaining serial ports
for use with modems and other devices. In addition, they have duplicate
and frequently inconsistant facilities for operating devices like modems
which are not very easy to debug, require program source to modify or
configure, and generally are a nusance when new devices become available
in the market. Finally, UNIX has typically required that lines be dedicated
for either outbound or inbound operation, and it would be convienent to
be able to use them for both in a sensible way.
.pp
Connectd is a daemon that intend to improve this situation by managing
outbound and switchable outbound/inbound lines. It receives requests via
UNIX domain IPC and arranges for physical connections to be made on behalf
of the requesting program, by in turn spawning another program to work the
given device. These connection makeing programs, or "connectors", are much
easier to write and debug than the special purpose subroutines that they
depricate that rely on the context of the program that they are written
into. Since the connectors are seperate UNIX programs, they can be created
or modified without altering the programs that use them. Since they are
centralized, all user programs can equally take advantage of program
improvements without being specifically rewritten each time a new device
comes on the market. Finally, connectd allows UNIX to sensibly manage
outbound/inbound devices to further enhance resource utilization.
.lp
An addtional advantage of organizing UNIX in this way is to improve
the security of the system interface to outbound devices, which are a
necessary evil. Since both the facility program and connector program
are isolated from access to the system resources hidden by connectd, we
again reduce the number of programs who need to be trusted with such
information to one relatively small program.
.lp
Two advantages to using IPC as the method of passing and recieving requests
are that we hide much of the implementation from those who desire to either
create programs needing outside access, or those wishing to create programs
providing more facilities for access. We also avoid the need to fork an
application process, as would be the case if we chose to have connector progams
arranged as filters (e.g. popen("/etc/dialer/hayes"), which also have problems
in being easy to debug. Thus, we ease the burden
for maintaining these facilities.
.lp
With the methodology we have chosen for connectd, we have modeled this service
on the Berkeley 4.3BSD networking system, which it will be used with. And
while we use the socket abstraction which we prefer, there is no intimate
requirement for it in our model, streams would work just as well.
.uh "Assumptions:"
.pp
User program wants to connect to a tty line which will be connected
to another system via a modem or other equipment transparently, without
regard to knowing what is done to make this happen.
.lp
User program supplies a name or a number to be dialed, plus an
option string or structure (primarily to allow a desired baud rate or rates).
.lp
User program needs to recieve a file descriptor of a serial port which has
been connected to the entity associated with the number passed,
or an explanation as to why the connection could not be made. 
.lp
For debugging purposes, the user of the user program should be able to
optionally recieve a running, real time account of the progress of the
connector program.
.uh "Implementation:"
.pp
User programs are modified to remove embedded dialing code, and the
following interface is added:
.uh " User Program Interface"
.pp
Names are mapped to absolute numbers by user program via
getphonenumberbyname(3C), which returns a structure that has phone number and
dialing domain. This request is satisfied either by the domain nameserver
or an phonenumber(5C) file as a last resort.
.lp
The connection request is made with serialconnect(3C).
It takes a number structure and option buffer, and will eventually return
either a file descriptor or -1 (error in errno). If filedes 2 is connected to
a "terminal", dial progress information, or error messages will be supplied
per whatever dialing process is called. The option buffer is value-result,
it contains a bunch of null-terminated strings, in turn terminated by a NULL.
These strings have the convention of being of the formats:
.(l R
	optionname#number
	optionname=string
	optionname
.l)
where optionname is some option name, and the formats encode numeric, string,
and boolean information. A well - behaved program should return all option
strings it is called with, and may return other information as well (such as
call termination sequence?). programs should look in the connector(5C) file
for generic modem configuration info.
.lp
The serialconnect(3C) request sends a request over the UNIX domain stream
socket bound to /dev/connect, containing the above info. Connectd(8C)
recieves the request, and uses the file lines(5C) to decide on suitable
lines to use, if none, sends back service unavailable error. If it finds
suitable ones, and if unused by a getty, then it opens the line, forks, changes
back to user uid of caller, dups to fd 0 & 1 & 2 closing others,  and execs
appropriate program /etc/connectors/<modemtype> with fd0&1 opened to tty line,
fd2 to users tty.
.lp
The modemtype is obtained from the file lines(5C), and the
both the options from the user's program and any added by lines(5C) are passed
to the program as argv strings. The program can optionally return additional
options, and should return all (how). Connector programs

.lp
