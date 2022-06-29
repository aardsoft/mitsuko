TEMPLATE = subdirs

lisp.files = *.lisp
lisp.path = $$[QT_HOST_PREFIX]/share/mitsuko/lisp/float
INSTALLS += lisp

SUBDIRS = qml