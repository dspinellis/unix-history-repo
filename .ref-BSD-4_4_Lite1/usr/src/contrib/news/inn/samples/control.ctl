##  $Revision: 1.4 $
##  control.ctl - access control for control messages
##  Format:
##	<message>:<from>:<newsgroups>:<action>
##  The last match found is used.
##	<message>	Control message or "all" if it applies
##			to all control messages.
##	<from>		Pattern that must match the From line.
##	<newsgroups>	Pattern that must match the newsgroup being
##			newgroup'd or rmgroup'd (ignored for other messages).
##	<action>	What to do:
##			    doit	Perform action (usually sends mail too)
##			    doifarg	Do if command has an arg (see sendsys)
##			    doit=xxx	Do action; log to xxx (see below)
##			    drop	Ignore message
##			    log		One line to error log
##			    log=xxx	Log to xxx (see below)
##			    mail	Send mail to admin
##			xxx=mail to mail; xxx= (empty) to toss; xxx=/full/path
##			to log to /full/path; xxx=foo to log to ${LOG}/foo.log

##	DEFAULT
all:*:*:mail

##	CHECKGROUPS MESSAGES
checkgroups:*:*:mail

##	IHAVE/SENDME MESSAGES
ihave:*:*:drop
sendme:*:*:drop

##	NEWGROUP MESSAGES
##  Any newsgroups
newgroup:*:*:log=newgroup
newgroup:tale@*.uu.net:comp.*|misc.*|news.*|rec.*|sci.*|soc.*|talk.*:doit=newgroup
newgroup:fair@apple.com:comp.*|misc.*|news.*|rec.*|sci.*|soc.*|talk.*|ddn.*:mail
##  Australia newsgroups
newgroup:kre@*mu*au:aus.*|melb.*:mail
##  BITNET newsgroups
newgroup:jim@*american.edu:bit.*:mail
##  BIONET newsgroups
newgroup:shibumi@*.bio.net:bionet.*:mail
newgroup:kristoff@genbank.bio.net:bionet.*:mail
##  CLARINET newsgroups
newgroup:brad@clarinet.com:clari.*:mail
newgroup:grant@clarinet.com:clari.*:mail
##  GNU newsgroups
newgroup:usenet@*ohio-state.edu:gnu.*:mail
newgroup:tower@prep.ai.mit.edu:gnu.*:mail
newgroup:news@ai.mit.edu:gnu.*:mail
newgroup:karl.kleinpaste@osc.edu:gnu.*:mail
##  K12 newsgroups
newgroup:nerd@percival.rain.com:k12.*:mail
##  IEEE newsgroups
newgroup:burt@ieee.org:ieee.*:mail
##  VMSNET newsgroups
newgroup:tp@mccall.com:vmsnet.*:mail
##  ALT newsgroups
newgroup:*:alt.*:log=newgroup

##	RMGROUP MESSAGES
##  Any newsgroups
rmgroup:*:*:mail
rmgroup:tale@*.uu.net:comp.*|misc.*|news.*|rec.*|sci.*|soc.*|talk.*:doit=rmgroup
rmgroup:kre@*mu*au:comp.*|misc.*|news.*|rec.*|sci.*|soc.*|talk.*:mail
rmgroup:fair@apple.com:comp.*|misc.*|news.*|rec.*|sci.*|soc.*|talk.*|ddn.*:mail
##  Australia newsgroups
rmgroup:kre@*mu*au:aus.*|melb.*:mail
##  BITNET newsgroups
rmgroup:jim@*american.edu:bit.*:mail
##  BIONET newsgroups
rmgroup:shibumi@*.bio.net:bionet.*:mail
rmgroup:kristoff@genbank.bio.net:bionet.*:mail
##  CLARINET newsgroups
rmgroup:brad@clarinet.com:clari.*:mail
rmgroup:grant@clarinet.com:clari.*:mail
##  GNU newsgroups
rmgroup:usenet@*ohio-state.edu:gnu.*:mail
rmgroup:tower@prep.ai.mit.edu:gnu.*:mail
rmgroup:news@ai.mit.edu:gnu.*:mail
rmgroup:karl.kleinpaste@osc.edu:gnu.*:mail
##  K12 newsgroups
rmgroup:nerd@percival.rain.com:k12.*:mail
##  IEEE newsgroups
rmgroup:burt@ieee.org:ieee.*:mail
##  VMSNET newsgroups
rmgroup:tp@mccall.com:vmsnet.*:mail
##  ALT newsgroups
rmgroup:*:alt.*:log=rmgroup

##	SENDSYS
sendsys:*@uunet.uu.net:*:doit=miscctl
sendsys:*:*:doifarg

##	SENDUUNAME
senduuname:*@uunet.uu.net:*:doit=miscctl
senduuname:*:*:mail

##	VERSION
version:*@uunet.uu.net:*:doit=miscctl
version:*:*:mail
