setenv BLOCKSIZE 1k

tset -Q \?$TERM
echo 'erase ^H, kill ^U, intr ^C status ^T'
stty erase '^H' kill '^U' intr '^C' status '^T' crt

umask 022

echo "Don't login as root, use the su command."
