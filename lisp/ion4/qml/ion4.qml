import QtQuick 2.12
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtWayland.Compositor 1.3
import MitsukoGridWM 1.0
import Liri.XWayland 1.0 as LXW

/*
key handling:
- listen for modifier (or possibly even modifier combinations - like alt-f ..)
- pop up input bar when valid modifier pressed
- keep input bar until valid sequence entered
- ctrl, ... are usable as secondary modifiers when entering the sequence
*/
WaylandCompositor {
  id: compositor
  objectName: "compositor"

  onCreatedChanged: {
    console.debug("Compositor created");
    xwayland.startServer();
  }

  Shortcut {
    id: sc5
    objectName: "sc5"
    onActivated: console.log("sc4")
    sequence: "F10"
  }

  WaylandOutput {
    id: output
    objectName: "output"
    sizeFollowsWindow: true
    Shortcut {
      id: sc4
      objectName: "sc4"
      sequence: "F7"
      onActivated: console.log("sc4")
    }
    window:

    Window {
      width: 500
      height: 500
      visible: true
      id: window
      objectName: "window"

      Component {
        id: winComp
        SceneWindow {
          id: windowRoot
          height: 20
          width: 20

          Rectangle {
            anchors.fill: parent
            color: {
              if (windowRoot.index % 2 == 0) {
                return "red"
              } else {
                return "blue"
              }
            }
          }
        }
      }

      SceneWindowManager {
      Shortcut {
      id: sc99
      objectName: "sc99"
      sequence: "F8"
      onActivated: console.log("sc99")
      }
        id: winManager
        anchors.fill: parent
    }

    Component.onCompleted: {
        for (let i = 0; i < 10; i++) {
            // Create a fake window, parented to the window manager.
            let window = winComp.createObject(winManager);
        }
    }

      Repeater {
        id: repeater
        objectName: "repeater"
        model: shellSurfaces

        Column {
          id: chrome
          objectName: "chrome"

            HoverHandler {
              id: hoverHandler
            }

          Rectangle {
            visible: modelData.toplevel.decorationMode === XdgToplevel.ServerSideDecoration
            width: parent.width
            height: 30
            //gradient: "HeavyRain";
            color: (hoverHandler.hovered? "green":"red")
            id: decoration
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
            //Rectangle {
            //  visible: modelData.toplevel.decorationMode === XdgToplevel.ServerSideDecoration
            //  width: parent.width
            //  height: 30
            //  id: bottom
            //  color: "green"
            //}
          }

          ShellSurfaceItem {
            focus: hoverHandler.hovered

            id: shellSurfaceItem
            objectName: "shellSurfaceItem"
            autoCreatePopupItems: true
            shellSurface: modelData
            onSurfaceDestroyed: shellSurfaces.remove(index)
          }
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
        onActivated: console.log("sc3")
      }
    }
  }

  Component {
    id: tilingWindow
    SceneWindow {
      id: windowRoot
      Component.onCompleted: console.log(shellSurface)
      HoverHandler {
        id: hoverHandler
      }
      Rectangle {
        anchors.fill: parent
        id: decoratorContainer
        objectName: "decoratorContainer"
        // idea here is that we have a decorator object which typically is
        // just as big as the display item, but also may be bigger on each side
        // exact properties as well as relative sizing should come from lisp
        // when a new surface is added the size of the decorator (unless there are
        // size constraints in the frame) is calculated based on the surface.
        // later on, the size of the surface is set based on the decorator size
        color: {
          if (windowRoot.index % 2 == 0) {
            return "red"
          } else {
            return "blue"
          }
        }
        ShellSurfaceItem {
          focus: hoverHandler.hovered
          id: shellSurfaceItem
          objectName: "shellSurfaceItem"
          autoCreatePopupItems: true
          shellSurface: windowRoot.shellSurface
          //onSurfaceDestroyed: shellSurfaces.remove(index)
        }
      }
    }
  }

  WlShell {
    onWlShellSurfaceCreated: {
      let window = tilingWindow.createObject(winManager, {"shellSurface": shellSurface});
    }
    id: wlShell
    objectName: "wlShell"
  }

  XdgDecorationManagerV1 {
    preferredMode: XdgToplevel.ServerSideDecoration
    id: xdgDecorationManager
    objectName: "xdgDecorationManager"
  }

  XdgShellV6 {
    onToplevelCreated: {
      let window = tilingWindow.createObject(winManager, {"shellSurface": xdgSurface});
    }
    id: xdgShellV6
    objectName: "xdgShellV6"
  }

  XdgShell {
    onToplevelCreated: {
      let window = tilingWindow.createObject(winManager, {"shellSurface": xdgSurface});
    }
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
        let window = tilingWindow.createObject(winManager, {"shellSurface": shellSurface});
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
