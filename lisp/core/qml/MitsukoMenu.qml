import QtQuick 2.12
import QtQuick.Controls 2.12
import EQL5 1.0

Menu {
  id: mitsukoMenu

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