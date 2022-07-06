import QtQuick 2.15
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtWayland.Compositor 1.3
import QtQuick.Controls 2.12
import Liri.XWayland 1.0 as LXW
import EQL5 1.0

Column {
  id: chrome

  property var shellSurface

  width: shellSurfaceItem.implicitWidth

  Rectangle {
    id: decoration
    height: titleText.height
    color: "blue"
    width: parent.width
    Text {
      id: titleText
      text: modelData.toplevel.title
      anchors.centerIn: parent
    }
  }

  ShellSurfaceItem {
    id: shellSurfaceItem
    objectName: "shellSurfaceItem"
    autoCreatePopupItems: true
    shellSurface: chrome.shellSurface
    onSurfaceDestroyed: shellSurfaces.remove(index)
  }
}