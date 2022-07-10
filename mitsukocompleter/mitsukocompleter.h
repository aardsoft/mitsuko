/**
 * @file mitsukocompleter.h
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#ifndef _MITSUKOCOMPLETER_H
#define _MITSUKOCOMPLETER_H

#include <QObject>

class MitsukoCompleter: public QObject{
    Q_OBJECT

    Q_PROPERTY(QString settingChanged NOTIFY settingChanged)

  public:
    Q_INVOKABLE static MitsukoCompleter *instance();
    Q_INVOKABLE static bool notify();
    Q_INVOKABLE void setNotify(const bool value);

    /*
     * Request completion of type key, identified by instance id id
     * Prefill with default values, if desired
     */
    Q_INVOKABLE static QStringList getCompletionList(
      const QString &id,
      const QString &key,
      const QStringList &defaultValue=QStringList(""));

  signals:
    void completionChanged(const QString &key);

  private:
    MitsukoCompleter();
    MitsukoCompleter(const MitsukoCompleter&);
    static MitsukoCompleter *mitsukoCompleter;
    bool m_initialized=false;
    bool m_notify=false;
};

#endif
