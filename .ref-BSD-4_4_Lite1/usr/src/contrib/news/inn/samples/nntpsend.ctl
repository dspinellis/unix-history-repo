##  $Revision: 1.2 $
##  Control file for nntpsend.
## Format:
##	site:fqdn:max_size:[<args...>]
##	<site>		The name used in the newsfeeds file for this site;
##			this determines the name of the batchfile, etc.
##	<fqdn>		The fully-qualified domain name of the site,
##			passed as the parameter to innxmit.
##	<size>		Size to truncate batchfile if it gets too big;
##			see trunc(1).
##	<args>		Other args to pass to innxmit
##  Everything after the pound sign is ignored.
nsavax:erehwon.nsavax.gov::-S -t60
walldrug:walldrug.com:1m:-T1800 -t300
