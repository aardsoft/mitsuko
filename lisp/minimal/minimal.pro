TEMPLATE = subdirs

lisp.files = *.lisp
lisp.path = $$[QT_HOST_PREFIX]/share/mitsuko/lisp/minimal
INSTALLS += lisp

SUBDIRS = qml