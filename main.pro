TEMPLATE = subdirs
SUBDIRS = MitsukoGridWM lisp/mitsuko_lib lisp/core lisp/float lisp/ion4 mitsuko

desktop.files = mitsuko.desktop
desktop.path = $$[QT_HOST_PREFIX]/share/wayland-sessions

INSTALLS += desktop

mitsuko.depends = lisp/mitsuko_lib