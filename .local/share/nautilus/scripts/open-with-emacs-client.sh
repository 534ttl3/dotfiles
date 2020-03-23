#!/bin/bash

emacsclient -a "" -c -n -e "(progn (raise-frame) (x-focus-frame (selected-frame)) (with-selected-frame (find-file \"${PWD}/$*\")))" 
