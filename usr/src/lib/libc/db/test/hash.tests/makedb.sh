#!/bin/sh
awk '{i++; print $0; print i;}' /usr/share/dict/words > words
