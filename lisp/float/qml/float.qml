import QtQuick 2.12
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtWayland.Compositor 1.3
import QtQuick.Controls 2.12
import Liri.XWayland 1.0 as LXW
import EQL5 1.0

WaylandCompositor {
  id: compositor
  objectName: "compositor"

  onCreatedChanged: {
    xwayland.startServer();
  }

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

      Menu {
        id: wmMenu
        MenuItem {
          text: "Terminal"
          onClicked: {
            Lisp.call("mitsuko-core:run-terminal")
          }
        }
        MenuItem {
          text: "Exit"
          onClicked: {
            Lisp.call("mitsuko-core:shutdown")
          }
        }
      }

      Repeater {
        id: repeater
        objectName: "repeater"
        model: shellSurfaces

        Column {
          id: chrome
          width: shellSurfaceItem.implicitWidth
          Rectangle {
            visible: modelData.toplevel.decorationMode === XdgToplevel.ServerSideDecoration
            width: parent.width
            height: 30
            gradient: "HeavyRain";
            Text {
              text: modelData.toplevel.title
              anchors.centerIn: parent
            }
            Item {
              anchors.right: parent.right
              width: 30
              height: 30
              Text { text: "X"; anchors.centerIn: parent }
              TapHandler {
                onTapped: modelData.toplevel.sendClose()
              }
            }
            DragHandler {
              target: chrome
            }
          }
          ShellSurfaceItem {
            id: shellSurfaceItem
            objectName: "shellSurfaceItem"
            autoCreatePopupItems: true
            shellSurface: modelData
            onSurfaceDestroyed: shellSurfaces.remove(index)
          }
        }
      }
    }
  }

  XdgDecorationManagerV1 {
    preferredMode: XdgToplevel.ServerSideDecoration
  }

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

  Component {
    id: shellSurfaceComponent

    LXW.XWaylandShellSurface {}
  }

  LXW.XWayland {
    id: xwayland

    //enabled: liriCompositor.settings.shell.enableXwayland
    enabled: true
    manager: LXW.XWaylandManager {
      id: manager
      onShellSurfaceRequested: {
        var shellSurface = shellSurfaceComponent.createObject(manager);
        shellSurface.initialize(manager, window, geometry, overrideRedirect, parentShellSurface);
        console.info("Shell surface requested for " + window);
      }
      onShellSurfaceCreated: {
        shellSurfaces.append({"shellSurface": shellSurface});
      }
    }
    onServerStarted: {
      console.info("Xwayland server started on " + displayName);
      //Session.SessionManager.setEnvironment("DISPLAY", displayName);
    }
  }

  ListModel {
    id: shellSurfaces
    objectName: "shellSurfaces"
  }
}
