.th STARTUP FILES
.sh NAME
\&.../files/startup \- \*(II startup file
.sh DESCRIPTION
This file is read by the monitor
at login time.
It is read before the user startup file
specified in the users file.
The primary purpose is to define
a new editor and/or shell to call
with the
.bd \ee
or
.bd \es
commands.
.sh "SEE ALSO"
monitor(quel),
users(files)
