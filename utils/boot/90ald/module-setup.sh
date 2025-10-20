#!/bin/bash

check() {
    if [[ -x "$systemdutildir"/systemd ]] && [[ -x /usr/libexec/ald-boot.sh ]]; then
        return 255
    fi

    return 1
}

depends() {
    echo bash systemd

    return 0
}

install() {

    inst_multiple /usr/libexec/ald-boot.sh \
        "$systemdsystemunitdir"/ald-boot.service \
        hald

    $SYSTEMCTL -q --root "$initdir" enable ald-boot.service

    # Dependencies
}
