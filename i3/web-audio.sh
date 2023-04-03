#!/bin/bash

function get {
    playerctl -p playerctld metadata | grep $1 | awk '{$1=$2=""; print $0}'
}

echo $(get title) • $(get artist)