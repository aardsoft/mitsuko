import QtQuick 2.12
import EQL5 1.0
import MitsukoCore 1.0
import Mitsuko.Settings 1.0

Item {
  Shortcut {
    id: sc_infoPrompt
    sequence: MitsukoSettings.settingString("shortcuts/infoPrompt", "F1")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_openTerminal
    sequence: MitsukoSettings.settingString("shortcuts/openTerminal", "F2")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_commandPrompt
    sequence: MitsukoSettings.settingString("shortcuts/commandPrompt", "F3")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_sshPrompt
    sequence: MitsukoSettings.settingString("shortcuts/sshPrompt", "F4")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_editFilePrompt
    sequence: MitsukoSettings.settingString("shortcuts/editFilePrompt", "F5")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_viewFilePrompt
    sequence: MitsukoSettings.settingString("shortcuts/viewFilePrompt", "F6")
    onActivated: {
    }
  }
  Shortcut {
    id: sc_mainMenu
    sequence: MitsukoSettings.settingString("shortcuts/mainMenu", "F12")
    onActivated: {
      wmMenu.popup()
    }
  }

  Shortcut {
    id: sc_toggleLispRepl
    sequence: MitsukoSettings.settingString("shortcuts/toggleLispRepl", "")
    onActivated: {
      console.log("Toggling LISP console");
    }
  }
}