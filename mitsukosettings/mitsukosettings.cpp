/**
 * @file mitsukosettings.cpp
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#include <QDebug>
#include <QUuid>
#include <QStandardPaths>
#include "mitsukosettings.h"

MitsukoSettings *MitsukoSettings::mitsukoSettings=0;

MitsukoSettings *MitsukoSettings::instance(){
  if (!mitsukoSettings){
    qDebug() << "Creating settings instance";
    mitsukoSettings=new MitsukoSettings();
  }

  qDebug() << "Returning settings instance";
  return mitsukoSettings;
}

MitsukoSettings::MitsukoSettings():
  QSettings(
    QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation)+"/core.ini",
    QSettings::IniFormat){
  m_initialized=true;
  m_notify=true;
  m_initializeDefaults=true;
}

bool MitsukoSettings::notify(){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->m_notify;
}

void MitsukoSettings::setNotify(const bool value){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  _instance->m_notify=value;
}

QVariant MitsukoSettings::setting(const QString &key, const QVariant &defaultValue){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->value(key, defaultValue);
}

QVariant MitsukoSettings::value(const QString &key, const QVariant &defaultValue){
  if (QSettings::value(key) == QVariant() &&
      defaultValue != QVariant() &&
      m_initializeDefaults == true){
    m_notify=false;
    setValue(key, defaultValue);
    m_notify=true;
  }
  return QSettings::value(key, defaultValue);
}

bool MitsukoSettings::settingBool(const QString &key, const bool defaultValue){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->value(key, defaultValue).toBool();
}

QString MitsukoSettings::settingString(const QString &key, const QString &defaultValue){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->value(key, defaultValue).toString();
}

QStringList MitsukoSettings::settingStringList(const QString &key, const QStringList &defaultValue){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->value(key, defaultValue).toStringList();
}

int MitsukoSettings::settingInt(const QString &key, const int defaultValue){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  return _instance->value(key, defaultValue).toInt();
}

void MitsukoSettings::setSetting(const QString &key, const QVariant &value){
  MitsukoSettings *_instance=MitsukoSettings::instance();
  _instance->setValue(key, value);
}

void MitsukoSettings::setSettingBool(const QString &key, const bool value){
  MitsukoSettings::setSetting(key, value);
}

void MitsukoSettings::setSettingInt(const QString &key, const int value){
  MitsukoSettings::setSetting(key, value);
}

void MitsukoSettings::setSettingString(const QString &key, const QString &value){
  MitsukoSettings::setSetting(key, value);
}

void MitsukoSettings::setSettingStringList(const QString &key, const QStringList &value){
  MitsukoSettings::setSetting(key, value);
}

void MitsukoSettings::setValue(const QString &key, const QVariant &value){
  if (QSettings::value(key) == QVariant()){
    qDebug()<< "setting initial value on " << key;
    QSettings::setValue(key, value);
    if (m_notify)
      emit settingChanged(key);
  } else if (QSettings::value(key) == value){
    qDebug()<< "value didn't change for " << key;
  } else {
    QSettings::setValue(key, value);
    qDebug()<< "new value for " << key;
    if (m_notify)
      emit settingChanged(key);
  }
}
