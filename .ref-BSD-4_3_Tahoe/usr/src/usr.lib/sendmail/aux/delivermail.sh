#! /bin/sh
# delivermail hack thanks to Jay Lepreau
echo "Subject: delivermail $*" >/tmp/foo.$$
echo "---------------------------------" >>/tmp/foo.$$
cat /tmp/foo.$$ - | /bin/mail postmaster
rm /tmp/foo.$$
