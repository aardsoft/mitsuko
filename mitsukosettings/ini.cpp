/**
 * @file ini.cpp
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#include <QDebug>
#include <QQmlEngine>
#include "ini.h"
#include "mitsukosettings.h"

QT_BEGIN_NAMESPACE

// TODO: this probably will blow up if we try to reload this plugin
QObject* ini(){
  qDebug() << "Legacy init";
  static MitsukoSettings *instance = MitsukoSettings::instance();
  qmlRegisterSingletonInstance("Mitsuko.Settings", 1, 0,"MitsukoSettings", MitsukoSettings::instance());
  return instance;
}

QT_END_NAMESPACE
