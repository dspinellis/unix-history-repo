: run this script through /bin/sh

while true
do
    (echo -n "Directory listing as of "; date; ls [A-Z]*) > DIRLIST
    sleep 30
done
