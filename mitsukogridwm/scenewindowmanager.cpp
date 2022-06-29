/**
 * @file scenewindowmanager.cpp
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#include "scenewindowmanager.h"

SceneWindowManager::SceneWindowManager(QQuickItem* parent)
    : QQuickItem(parent)
{
}

void SceneWindowManager::itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value)
{
    SceneWindow* windowItem = nullptr;

    // Detect if a SceneWindow was added, or removed, and trigger a layout appropriately.
    // We also monitor the SceneWindow for geometry changes, and trigger a layout if those happen.
    switch (change) {
    case QQuickItem::ItemChildAddedChange:
        windowItem = qobject_cast<SceneWindow*>(value.item);
        if (windowItem) {
            connect(windowItem, &QQuickItem::xChanged, this, &QQuickItem::polish);
            connect(windowItem, &QQuickItem::yChanged, this, &QQuickItem::polish);
            connect(windowItem, &QQuickItem::widthChanged, this, &QQuickItem::polish);
            connect(windowItem, &QQuickItem::heightChanged, this, &QQuickItem::polish);
            m_windows.append(windowItem);
            polish();
        }
        break;
    case QQuickItem::ItemChildRemovedChange:
        windowItem = qobject_cast<SceneWindow*>(value.item);
        if (windowItem) {
            disconnect(windowItem, &QQuickItem::xChanged, this, &QQuickItem::polish);
            disconnect(windowItem, &QQuickItem::yChanged, this, &QQuickItem::polish);
            disconnect(windowItem, &QQuickItem::widthChanged, this, &QQuickItem::polish);
            disconnect(windowItem, &QQuickItem::heightChanged, this, &QQuickItem::polish);
            m_windows.removeOne(windowItem);
            polish();
        }
        break;
    default:
        break;
    }

    QQuickItem::itemChange(change, value);
}

// Called by QQuickItem, delayed, when polish() is invoked.
// Can recurse, if it needs to.
void SceneWindowManager::updatePolish()
{
    // Remove anything that was destroyed since we last laid out
    m_windows.removeAll(nullptr);
    QPointF origin;
    int i = 0;

    for (const auto& window : qAsConst(m_windows)) {
        window->setIndex(i++);
        qDebug() << "Position " << i << " at " << origin;
        window->setPosition(origin);
        origin.setX(origin.x() + window->width());
        origin.setY(origin.y() + window->height());
    }
}
