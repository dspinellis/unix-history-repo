.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	5.1 (Berkeley) %G%
.\"
.NH 1
Setting up
.PP
The 4.2BSD release comes with the necessary programs 
installed and with the default line printer queue
created.  If the system must be modified, the
makefile in the directory /usr/src/usr.lib/lpr
should be used in recompiling and reinstalling
the necessary programs.
.PP
The real work in setting up is to create the
.I printcap
file and any printer filters for printers not supported
in the distribution system.
.NH 2
Creating a printcap file
.PP
The 
.I printcap
database contains one or more entries per printer.
A printer should have a separate spooling directory;
otherwise, jobs will be printed on
different printers depending on which printer daemon starts first.
This section describes how to create entries for printers which do not
conform to the default printer description (an LP-11 style interface to a
standard, band printer).
.NH 3
Printers on serial lines
.PP
When a printer is connected via a serial communication line
it must have the proper baud rate and terminal modes set.
The following example is for a DecWriter III printer connected
locally via a 1200 baud serial line.
.DS
.DT
lp|LA-180 DecWriter III:\e
	:lp=/dev/lp:br#1200:fs#06320:\e
	:tr=\ef:of=/usr/lib/lpf:lf=/usr/adm/lpd-errs:
.DE
The
.B lp
entry specifies the file name to open for output. In this case it could
be left out since ``/dev/lp'' is the default.
The
.B br
entry sets the baud rate for the tty line and the
.B fs
entry sets CRMOD, no parity, and XTABS (see \fItty\fP\|(4)).
The
.B tr
entry indicates a form-feed should be printed when the queue
empties so the paper can be torn off without turning the printer off-line and
pressing form feed.
The
.B of
entry specifies the filter program
.I lpf
should be used for printing the files;
more will be said about filters later.
The last entry causes errors
to be written to the file ``/usr/adm/lpd-errs''
instead of the console.
.NH 3
Remote printers
.PP
Printers which reside on remote hosts should have an empty
.B lp
entry.
For example, the following printcap entry would send output to the printer
named ``lp'' on the machine ``ucbvax''.
.DS
.DT
lp|default line printer:\e
	:lp=:rm=ucbvax:rp=lp:sd=/usr/spool/vaxlpd:
.DE
The
.B rm
entry is the name of the remote machine to connect to; this name must
appear in the /etc/hosts database, see \fIhosts\fP\|(5).
The
.B rp
capability indicates
the name of the printer on the remote machine is ``lp'';
in this case it could be left out since this is the default value.
The
.B sd
entry specifies ``/usr/spool/vaxlpd''
as the spooling directory instead of the
default value of ``/usr/spool/lpd''.
.NH 2
Output filters
.PP
Filters are used to handle device dependencies and to
perform accounting functions.  The output filter
.B of
is used to filter text data to the printer device when accounting is
not used or when all text data must be passed through a filter.
It is not intended to perform accounting since it is started only once,
all text files are filtered through it, and no provision is made for passing
owners' login name, identifying the begining and ending of jobs, etc.
The other filters (if specified) are started for each file
printed and perform accounting if there is an
.B af
entry.
If entries for both
.B of
and one of the other filters are specified,
the output filter is used only to print the banner page;
it is then stopped to allow other filters access to the printer.
An example of a printer which requires output filters
is the Benson-Varian.
.DS
.DT
va|varian|Benson-Varian:\e
	:lp=/dev/va0:sd=/usr/spool/vad:of=/usr/lib/vpf:\e
	:tf=/usr/lib/rvcat:mx#2000:pl#58:tr=\ef:
.DE
The
.B tf
entry specifies ``/usr/lib/rvcat'' as the filter to be
used in printing \fItroff\fP\|(1) output.
This filter is needed to set the device into print mode
for text, and plot mode for printing
.I troff
files and raster images (see \fIva\fP\|(4V)).
Note that the page length is set to 58 lines by the
.B pl
entry for 8.5" by 11" fan-fold paper.
To enable accounting, the varian entry would be
augmented with an
.B af
filter as shown below.
.DS
.DT
va|varian|Benson-Varian:\e
	:lp=/dev/va0:sd=/usr/spool/vad:of=/usr/lib/vpf:\e
	:if=/usr/lib/vpf:tf=/usr/lib/rvcat:af=/usr/adm/vaacct:\e
	:mx#2000:pl#58:tr=\ef:
.DE
