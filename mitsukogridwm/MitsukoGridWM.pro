TEMPLATE = lib
CONFIG += qmltypes plugin file_copies
QT += qml quick waylandcompositor

QML_IMPORT_NAME = MitsukoGridWM
QML_IMPORT_MAJOR_VERSION = 1

TARGET = mitsukogridwmplugin #$$qtLibraryTarget(mitsukogridwmplugin)

qmldir.files = qmldir
qmldir.path = $$[QT_INSTALL_QML]/MitsukoGridWM

INSTALLS += target qmldir
target.path = $$[QT_INSTALL_QML]/MitsukoGridWM

# this is required for being able to run from inside the build directory
COPIES += qmldir_build
qmldir_build.files = qmldir
qmldir_build.path = $$OUT_PWD

HEADERS += mitsukogridwm.h \
        scenewindow.h \
        scenewindowmanager.h

SOURCES += mitsukogridwm.cpp \
        scenewindow.cpp \
        scenewindowmanager.cpp
