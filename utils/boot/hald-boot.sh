#!/bin/sh

type getarg >/dev/null 2>&1 || . /lib/dracut-lib.sh

[ -z "$1" ] && printf "No sysroot specified! Exiting!\n" && exit 0
sysroot="$1"

hald_boot=$(getarg hald.boot)
[ -z "$hald_boot" ] && printf "No deployment specified! Exiting!\n" && exit 0

hald_digest=$(getarg hald.digest)

mount -o remount,rw "$sysroot"
[ ! -d "$sysroot/usr" ] && chattr -i "$sysroot/" && mkdir -p "$sysroot/usr"
[ ! -d "$sysroot/etc" ] && chattr -i "$sysroot/" && mkdir -p "$sysroot/etc"

if [ -n "$hald_digest" ]; then
  hald activate "$hald_boot" --digest "$hald_digest" --rootd "$sysroot"
else
  hald activate "$hald_boot" --rootd "$sysroot"
fi
