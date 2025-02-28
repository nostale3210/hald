#!/bin/sh

type getarg >/dev/null 2>&1 || . /lib/dracut-lib.sh

[ -z "$1" ] && printf "No sysroot specified! Exiting!\n" && exit 0
sysroot="$1"

ald_boot=$(getarg ald.boot)
[ -z "$ald_boot" ] && printf "No deployment specified! Exiting!\n" && exit 0

mount -o remount,rw "$sysroot"
[ ! -d "$sysroot/usr" ] && mkdir -p "$sysroot/usr"
[ ! -d "$sysroot/etc" ] && mkdir -p "$sysroot/etc"

printf "Activating deployment: $ald_boot"

hald activate "$ald_boot" --rootd "$sysroot"
