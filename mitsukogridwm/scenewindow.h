/**
 * @file scenewindow.h
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#ifndef _SCENEWINDOW_H
#define _SCENEWINDOW_H

#include <QtQuick/qquickview.h>
#include <QtQuick/qquickitem.h>
#include <QWaylandShellSurface>

// Acts as the "top level" we will wrap around our windows.
// We have this as a C++ class so we can identify it C++ side.
class SceneWindow : public QQuickItem
{
    Q_OBJECT
    Q_PROPERTY(int index READ index WRITE setIndex NOTIFY indexChanged) // this is just for fun, but you could add some real properties to it.
    Q_PROPERTY(QWaylandShellSurface *shellSurface READ shellSurface WRITE setShellSurface NOTIFY shellSurfaceChanged)
public:
    using QQuickItem::QQuickItem;
    QWaylandShellSurface *shellSurface() const;
    int index() const;
    void setIndex(int i);
    void setShellSurface(QWaylandShellSurface *shellSurface);
signals:
    void indexChanged(int);
    void shellSurfaceChanged(QWaylandShellSurface *);
private:
    int m_index = 0;
    QWaylandShellSurface *m_surface = 0;
    QString m_cachedTitle="";
};

#endif
