QT += gui waylandcompositor
LIBS += -lecl -L. -L../lisp/mitsuko_lib -lmitsuko_lib -leql5
TARGET = mitsuko

#HEADERS +=

SOURCES += main.cpp

INSTALLS += target
target.path = $$[QT_HOST_PREFIX]/bin