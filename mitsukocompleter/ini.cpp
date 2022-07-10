/**
 * @file ini.cpp
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#include <QDebug>
#include <QQmlEngine>
#include "ini.h"
#include "mitsukocompleter.h"

QT_BEGIN_NAMESPACE

// TODO: this probably will blow up if we try to reload this plugin
QObject* ini(){
  static MitsukoCompleter *instance = MitsukoCompleter::instance();
  qmlRegisterSingletonInstance("Mitsuko.Completer", 1, 0,"MitsukoCompleter", MitsukoCompleter::instance());
  return instance;
}

QT_END_NAMESPACE
