#! /bin/csh -f
#
# Read all files with names of the form "system.list" and send any
# notesfiles listed in that file to "system". Allows us to add another
# feed by merely adding a file "system.list" in the .utilities directory.
#
echo -n "	LOCAL NOTESFILE UPDATES begin: " ; date
echo " Lock files existing before updates: "; /bin/ls -l /usr/spool/notes/.locks
set nfutil = /usr/spool/notes/.utilities
cd ${nfutil}
foreach i ( *.list)
	set sys = `echo ${i} | sed 's/\(.*\).list/\1/'`
	echo " ------ Sending to system ${sys}"
	time /usr/bin/nfxmit -d${sys} -f ${nfutil}/${sys}.list -f ${nfutil}/short.names
end
echo " Lock files existing after updates: "; /bin/ls -l /usr/spool/notes/.locks
echo -n "	LOCAL NOTESFILES UPDATES completed: " ; date
