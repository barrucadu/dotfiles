#!/bin/sh

install -d -m755 ${DESTDIR}/etc/rc.d || exit 1
install -d -m755 ${DESTDIR}/etc/conf.d || exit 1
install -d -m755 ${DESTDIR}/etc/rc.d/functions.d/ || exit 1

for i in rc.conf; do
  install -D -m644 $i ${DESTDIR}/etc/$i || exit 1
done
for i in rc.local rc.local.shutdown; do
  install -D -m755 $i ${DESTDIR}/etc/$i || exit 1
done
for i in rc.multi rc.shutdown rc.sysinit runsystem; do
  install -D -m755 $i ${DESTDIR}/libexec/$i || exit 1
done

install -D -m644 functions ${DESTDIR}/etc/rc.d/functions || exit 1

gcc $CFLAGS -o minilogd minilogd.c || exit 1
install -D -m755 minilogd ${DESTDIR}/sbin/minilogd || exit 1

ln -s /libexec/rc.shutdown ${DESTDIR}/sbin/halt
ln -s /libexec/rc.shutdown ${DESTDIR}/sbin/reboot
