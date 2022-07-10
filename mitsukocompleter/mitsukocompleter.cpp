/**
 * @file mitsukocompleter.cpp
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#include <QDebug>
#include <QUuid>
#include <QStandardPaths>
#include "mitsukocompleter.h"

MitsukoCompleter *MitsukoCompleter::mitsukoCompleter=0;

MitsukoCompleter *MitsukoCompleter::instance(){
  if (!mitsukoCompleter){
    qDebug() << "Creating completer instance";
    mitsukoCompleter=new MitsukoCompleter();
  }

  qDebug() << "Returning completer instance";
  return mitsukoCompleter;
}

MitsukoCompleter::MitsukoCompleter():
  QObject(){
  m_initialized=true;
  m_notify=true;
}

bool MitsukoCompleter::notify(){
  MitsukoCompleter *_instance=MitsukoCompleter::instance();
  return _instance->m_notify;
}

void MitsukoCompleter::setNotify(const bool value){
  MitsukoCompleter *_instance=MitsukoCompleter::instance();
  _instance->m_notify=value;
}

// cache the initial completion, and just narrow, unless completion type experts complete re-evaluation
QStringList MitsukoCompleter::getCompletionList(const QString &id,
                                         const QString &key,
                                         const QStringList &defaultValue){
  MitsukoCompleter *_instance=MitsukoCompleter::instance();
}
