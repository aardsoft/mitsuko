import QtQuick 2.6
import QtQuick.Window 2.2
import QtWayland.Compositor 1.3

WaylandCompositor {
  id: compositor
  objectName: "compositor"
  Shortcut {
    id: sc5
    objectName: "sc5"
    onActivated: console.log("sc4")
  }

  WaylandOutput {
    id: output
    objectName: "output"
    sizeFollowsWindow: true
    Shortcut {
      id: sc4
      objectName: "sc4"
      onActivated: console.log("sc4")
    }
    window:

    Window {
      width: 500
      height: 500
      visible: true
      id: window
      objectName: "window"

      Repeater {
        id: repeater
        objectName: "repeater"
        model: shellSurfaces

        ShellSurfaceItem {
          id: shellSurfaceItem
          objectName: "shellSurfaceItem"
          autoCreatePopupItems: true
          shellSurface: modelData
          onSurfaceDestroyed: shellSurfaces.remove(index)
        }
      }
      Shortcut {
        id: sc1
        objectName: "sc1"
        sequence: ""
        onActivated: console.log("sc1")
      }
      Shortcut {
        id: sc2
        objectName: "sc2"
        onActivated: console.log("sc2")
      }
      Shortcut {
        id: sc3
        objectName: "sc3"
        sequence: "F10"
        onActivated: console.log("sc3")
      }
    }
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

  ListModel {
    id: shellSurfaces
    objectName: "shellSurfaces"
  }
}
