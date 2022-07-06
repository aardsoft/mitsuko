TEMPLATE = subdirs

qml.files = *.qml qmldir
qml.path = $$[QT_HOST_PREFIX]/share/mitsuko/qml/MitsukoCore
INSTALLS += qml