QT += core qml
QT -= gui
TEMPLATE = lib
TARGET = MitsukoCompleter
CONFIG += plugin release


HEADERS += ini.h mitsukocompleter.h

SOURCES += ini.cpp mitsukocompleter.cpp

INSTALLS += target
target.path = $$[QT_HOST_PREFIX]/bin