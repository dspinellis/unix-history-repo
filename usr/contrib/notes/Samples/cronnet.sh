#! /bin/csh -f
echo -n "	USENET NOTESFILE UPDATES begin: " ; date
echo " Lock files existing before updates: "; /bin/ls -l /usr/spool/notes/.locks
set nfutil = /usr/spool/notes/.utilities
cd ${nfutil}
foreach i ( *.netlist)
	set sys = `echo ${i} | sed 's/\(.*\).netlist/\1/'`
	echo " ------ Sending to system ${sys}"
	time /usr/bin/nfxmit -d${sys} -a -f ${nfutil}/${sys}.netlist -f ${nfutil}/short.names
end
echo " Lock files existing after updates: "; /bin/ls -l /usr/spool/notes/.locks
echo -n "	USENET NOTESFILES UPDATES completed: " ; date
