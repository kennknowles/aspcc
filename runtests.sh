#!/bin/bash

files=`find tests -iname '*.asp'`

for file in $files; do
	echo -n "Trying $file"
	if ./aspcc $file > /dev/null; then
		echo ' Success!';
	else	
		echo ' FAILED';
	fi
done
