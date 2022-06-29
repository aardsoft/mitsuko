#include "mitsukogridwm.h"
#include "scenewindow.h"
#include "scenewindowmanager.h"
#include <QDebug>

void MitsukoGridWM::registerTypes(const char* uri)
{
    qmlRegisterType<SceneWindow>(uri, 1, 0, "SceneWindow");
    qmlRegisterType<SceneWindowManager>(uri, 1, 0, "SceneWindowManager");
}
