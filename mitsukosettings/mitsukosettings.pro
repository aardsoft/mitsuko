QT += core qml
QT -= gui
#LIBS += -lecl -L. -L../lisp/mitsuko_lib -lmitsuko_lib -leql5
TEMPLATE = lib
TARGET = MitsukoSettings
CONFIG += plugin release


HEADERS += ini.h mitsukosettings.h

SOURCES += ini.cpp mitsukosettings.cpp

INSTALLS += target
target.path = $$[QT_HOST_PREFIX]/bin