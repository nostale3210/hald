#!/bin/bash

check() {
    if [[ -x "$systemdutildir"/systemd ]] && [[ -x /usr/libexec/hald-boot.sh ]]; then
        return 255
    fi

    return 1
}

depends() {
    echo bash systemd

    return 0
}

install() {

    inst_multiple /usr/libexec/hald-boot.sh \
        "$systemdsystemunitdir"/hald-boot.service \
        hald \
        move-mount

    $SYSTEMCTL -q --root "$initdir" enable hald-boot.service

    # Dependencies
}
