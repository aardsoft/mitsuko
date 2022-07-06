TEMPLATE = subdirs
SUBDIRS = MitsukoGridWM mitsukosettings lisp/mitsuko_lib lisp/core lisp/float lisp/ion4 lisp/minimal mitsuko libraryloader

desktop.files = mitsuko.desktop
desktop.path = $$[QT_HOST_PREFIX]/share/wayland-sessions

INSTALLS += desktop

mitsuko.depends = lisp/mitsuko_lib