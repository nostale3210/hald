#!/bin/bash

check() {
    if [[ -x /usr/libexec/hald-boot.sh ]]; then
        return 255
    fi

    return 1
}

depends() {
    echo bash

    return 0
}

install() {

    inst_multiple /usr/libexec/hald-boot.sh \
        hald \
        move-mount

    inst_hook pre-pivot 40 "$moddir/hald-hook.sh"

    # Dependencies
}
