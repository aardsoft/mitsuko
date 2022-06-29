TEMPLATE = subdirs

lisp.files = *.lisp
lisp.path = $$[QT_HOST_PREFIX]/share/mitsuko/lisp/ion4
INSTALLS += lisp

SUBDIRS = qml