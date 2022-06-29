/**
 * @file scenewindowmanager.h
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#ifndef _SCENEWINDOWMANAGER_H
#define _SCENEWINDOWMANAGER_H

#include "scenewindow.h"

// Manages all the SceneWindows, positioning (and perhaps stacking) them as necessary.
class SceneWindowManager : public QQuickItem
{
    Q_OBJECT
public:
    SceneWindowManager(QQuickItem* parent = nullptr);

protected:
    void itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) override;
    void updatePolish() override;
private:
    QList<QPointer<SceneWindow>> m_windows;
};

#endif
