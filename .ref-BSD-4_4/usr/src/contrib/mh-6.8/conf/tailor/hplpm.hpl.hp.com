mmdfldir:       /usr/mail
mmdflfil:
# If you have a cluster, put the name of the root server in the line below
# servers:
# If you have a cluster, and make this file a CDF, put your hostname here
# Don't do it if you want "address hiding"; leaving localname blank will
# let outgoing mail be user@server.
localname:
sendmail:       /usr/lib/sendmail
