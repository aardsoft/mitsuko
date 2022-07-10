TEMPLATE = subdirs
LISP_SRC = lisp/mitsuko_lib lisp/core lisp/float lisp/ion4 lisp/minimal
PLUGIN_SRC = mitsukosettings mitsukocompleter libraryloader
SUBDIRS = MitsukoGridWM $$PLUGIN_SRC $$LISP_SRC mitsuko

desktop.files = mitsuko.desktop
desktop.path = $$[QT_HOST_PREFIX]/share/wayland-sessions

INSTALLS += desktop

mitsuko.depends = lisp/mitsuko_lib