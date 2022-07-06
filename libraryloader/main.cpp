/**
 * @file main.cpp
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

// run with LD_LIBRARY_PATH pointing to the library directory
#include <QDebug>
#include <QLibrary>

int main(int argc, char **argv){
  QLibrary lib;
  QString libraryName="libMitsukoSettings";

  qDebug() << "Looking for " << libraryName;


  lib.setFileName(libraryName);
  lib.load();

  if (lib.isLoaded()){
    typedef QObject* (*Ini)();
    Ini ini = (Ini)lib.resolve("ini");
    if(ini) {
      QObject* main = ini();
    } else {
      qDebug() << "Unable to locate ini: " << lib.errorString();
    }

  } else {
    qDebug() << "Didn't load library: " << lib.errorString();
  }

}
