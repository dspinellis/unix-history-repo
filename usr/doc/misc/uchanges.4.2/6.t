.SH
.LG
.ce
Section 5
.SM
.sp
.PP
.BP dir
Reflects the new directory format.
.BP disktab
Is a new file for maintaining disk geometry information.
This is a temporary scheme until the information stored
in this file for each disk is recorded on the disk
pack itself.
.BP dump
Is a superset of that used in 4.1BSD.
.BP fs
Reflects the new file system organization.
.BP gettytab
Is a new file which idescribes terminal characteristics.
Each entry in the file describes one of the possible arguments
to the getty program.
.BP hosts
Is a database for mapping between host names
and DARPA Internet host addresses.
.BP mtab
Has been modified to include a ``type'' field indicating
whether the file system is mounted read-only, read-write,
or read-write with disk quotas enabled.
.BP networks
Is a database for mapping between network names
and DARPA standard network numbers.
.BP phones
Is a phone number data base for tip.
.BP printcap
Is a termcap clone for configuring printers.
.BP protocols
Is a database for mapping between protocol names
and DARPA Internetwork standard protocol numbers.
.BP remote
Is a database of remote hosts for use with tip.
.BP services
Is a database in which DARPA Internet services
are recorded.  The information contained in this
file indicates the name of the service, the protocol
which is required to access it, and the port number
at which a client should connect to utilize the service.
.BP tar
Is a new entry describing the format of a tar tape.
.BP utmp
Has been augmented to include a remote host from which
a login session originates.  The wtmp file is also used
to record FTP sessions.
.BP vgrindefs
Is a file describing how to vgrind programs written
in many languages.
