##  $Revision: 1.8 $
##  expire.ctl - expire control file
##  Format:
##	/remember/:<keep>
##	<patterns>:<modflag>:<keep>:<default>:<purge>
##  First line gives history retention; other lines specify expiration
##  for newsgroups.  Must have a "*:A:..." line which is the default.
##	<patterns>	wildmat-style patterns for the newsgroups
##	<modflag>	Pick one of M U A -- modifies pattern to be only
##			moderated, unmoderated, or all groups
##	<keep>		Mininum number of days to keep article
##	<default>	Default number of days to keep the article
##	<purge>		Flush article after this many days
##  <keep>, <default>, and <purge> can be floating-point numbers or the
##  word "never."  Times are based on when received unless -p is used;
##  see expire.8

##  If article expires before 14 days, we still remember it for 14 days in
##  case we get offered it again.  Depending on what you use for the innd
##  -c flag and how paranoid you are about old news, you might want to
##  make this 28, 30, etc.
/remember/:14

##  Keep for 1-10 days, allow Expires headers to work.
*:A:1:10:never

##  Some particular groups stay forever.
dc.dining*:A:never:never:never
uunet*:A:never:never:never
