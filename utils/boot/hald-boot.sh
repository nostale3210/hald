#!/bin/sh

type getarg >/dev/null 2>&1 || . /lib/dracut-lib.sh

[ -z "$1" ] && printf "No sysroot specified! Exiting!\n" && exit 0
sysroot="$1"

hald_boot=$(getarg hald.boot)
[ -z "$hald_boot" ] && hald_boot=$(getarg ald.boot)  # LEGACY: fallback for old cmdline
[ -z "$hald_boot" ] && printf "No deployment specified! Exiting!\n" && exit 0

mount -o remount,rw "$sysroot"
[ ! -d "$sysroot/usr" ] && chattr -i "$sysroot/" && mkdir -p "$sysroot/usr"
[ ! -d "$sysroot/etc" ] && chattr -i "$sysroot/" && mkdir -p "$sysroot/etc"

hald activate "$hald_boot" --rootd "$sysroot"
