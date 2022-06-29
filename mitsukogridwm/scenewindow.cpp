/**
 * @file scenewindow.cpp
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#include <QWaylandWlShellSurface>
#include <QWaylandXdgSurface>
#include "scenewindow.h"

int SceneWindow::index() const {
  return m_index;
}

void SceneWindow::setIndex(int i) {
  if (i == m_index) { return; } m_index = i; emit indexChanged(i);
}

void SceneWindow::setShellSurface(QWaylandShellSurface *shellSurface){
  QWaylandWlShellSurface *wlShellSurface=qobject_cast<QWaylandWlShellSurface*>(shellSurface);
  QWaylandXdgSurface *xdgSurface=qobject_cast<QWaylandXdgSurface*>(shellSurface);
  if (wlShellSurface){
    m_cachedTitle=wlShellSurface->title();
    qDebug()<<"Using WlShellSurface"<<shellSurface<<"for"<<m_cachedTitle;
  } else if (xdgSurface){
    m_cachedTitle=xdgSurface->toplevel()->title();
    qDebug()<<"Using XdgSurface"<<shellSurface<<"for"<<m_cachedTitle;
  } else{
    qDebug()<<"Setting shell surface"<<shellSurface;
    m_cachedTitle="";
  }

  m_surface = shellSurface;
}

QWaylandShellSurface* SceneWindow::shellSurface() const{
  qDebug()<<"getting shell surface"<<m_surface;
  return m_surface;
}
