#!/bin/sh

echo -n $1 | md5sum | cut -d ' ' -f 1
