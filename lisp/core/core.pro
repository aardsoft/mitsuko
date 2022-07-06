TEMPLATE = subdirs

lisp.files = *.lisp
lisp.path = $$[QT_HOST_PREFIX]/share/mitsuko/lisp/core
INSTALLS += lisp

SUBDIRS = qml