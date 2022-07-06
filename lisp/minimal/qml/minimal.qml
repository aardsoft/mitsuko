import QtQuick 2.12
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtWayland.Compositor 1.15
import QtQuick.Controls 2.12
import EQL5 1.0
import MitsukoCore 1.0

WaylandCompositor {
  id: compositor
  objectName: "compositor"

  WaylandOutput {
    id: output
    objectName: "output"
    sizeFollowsWindow: true
    window:

    Window {
      width: 500
      height: 500
      visible: true
      id: window
      objectName: "window"

      MouseArea {
        anchors.fill: parent
        onClicked: {
          wmMenu.popup(mouseX, mouseY);
        }
      }

      MitsukoMenu { id: wmMenu }

      MitsukoDesktop {
        id: desktop
      }
    }
  }

  XdgDecorationManagerV1 {
    preferredMode: XdgToplevel.ServerSideDecoration
  }

  WlScaler { }

  WlShell {
    onWlShellSurfaceCreated: shellSurfaces.append({shellSurface: shellSurface});
    id: wlShell
    objectName: "wlShell"
  }

  XdgShellV6 {
    onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});
    id: xdgShellV6
    objectName: "xdgShellV6"
  }

  XdgShell {
    onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});
    id: xdgShell
    objectName: "xdgShell"
  }

  ListModel {
    id: shellSurfaces
    objectName: "shellSurfaces"
  }
}
