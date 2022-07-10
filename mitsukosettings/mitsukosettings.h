/**
 * @file mitsukosettings.h
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#ifndef _MITSUKOSETTINGS_H
#define _MITSUKOSETTINGS_H

#include <QSettings>
#include <QObject>

class MitsukoSettings: public QSettings{
    Q_OBJECT

    Q_PROPERTY(QString settingChanged NOTIFY settingChanged)

  public:
    Q_INVOKABLE static MitsukoSettings *instance();
    Q_INVOKABLE static bool notify();
    Q_INVOKABLE void setNotify(const bool value);

    Q_INVOKABLE void setValue(const QString &key, const QVariant &value);
    Q_INVOKABLE static void setSetting(const QString &key, const QVariant &value);
    Q_INVOKABLE static void setSettingBool(const QString &key, const bool value);
    Q_INVOKABLE static void setSettingInt(const QString &key, const int value);
    Q_INVOKABLE static void setSettingString(const QString &key, const QString &value);
    Q_INVOKABLE static void setSettingStringList(const QString &key, const QStringList &value);

    Q_INVOKABLE QVariant value(const QString &key, const QVariant &defaultValue=QVariant());
    Q_INVOKABLE static QVariant setting(const QString &key, const QVariant &defaultValue=QVariant());
    Q_INVOKABLE static bool settingBool(const QString &key, const bool defaultValue=false);
    Q_INVOKABLE static QString settingString(const QString &key, const QString &defaultValue=QString());
    Q_INVOKABLE static QStringList settingStringList(const QString &key, const QStringList &defaultValue=QStringList(""));
    Q_INVOKABLE static int settingInt(const QString &key, const int defaultValue=-1);

  signals:
    void settingChanged(const QString &key);

  private:
    MitsukoSettings();
    MitsukoSettings(const MitsukoSettings&);
    static MitsukoSettings *mitsukoSettings;
    bool m_initialized=false;
    bool m_notify=false;
    bool m_initializeDefaults=false;
};

#endif
