import QtQuick.Layouts 1.3
import QtQuick 2.12
import EQL5 1.0
import MitsukoCore 1.0

ColumnLayout {
  id: desktopArea
  objectName: "desktopArea"
  anchors.fill: parent
  spacing: 0

  MitsukoStatusBar {
    visible: false
    id: topStatusBar
    objectName: "topStatusBar"
    height: 40
    Layout.fillWidth: true
    z: 90000
  }

  RowLayout {
    spacing: 0

    MitsukoStatusBar {
      visible: false
      id: leftStatusBar
      objectName: "leftStatusBar"
      width: 10
      Layout.fillHeight: true
      z: 90000
    }

    Rectangle {
      objectName: "desktop"
      Layout.fillWidth: true
      Layout.fillHeight: true
      color: "black"

      Repeater {
        id: repeater
        objectName: "repeater"
        model: shellSurfaces

        MitsukoWindow {
          shellSurface: modelData
        }
      }
    }

    MitsukoStatusBar {
      visible: false
      id: rightStatusBar
      objectName: "rightStatusBar"
      width: 10
      Layout.fillHeight: true
      z: 90000
    }
  }

  MitsukoStatusBar {
    id: bottomStatusBar
    objectName: "bottomStatusBar"
    height: 10
    color: "black"
    Layout.fillWidth: true
    z: 90000
  }
}