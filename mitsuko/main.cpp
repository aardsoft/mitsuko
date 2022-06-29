/**
 * @file main.cpp
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QTextCodec>
#include <QGuiApplication>
#include <QDebug>
extern "C" void init_lib_MITSUKO__ALL_SYSTEMS(cl_object);

int main(int argc, char **argv){
  EQL::ini(argv);

  qputenv("QT_QPA_PLATFORM", "wayland;xcb;eglfs");

  QGuiApplication app(argc, argv);

  QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
  QTextCodec::setCodecForLocale(utf8);

  // TODO: consider adding translations here

  EQL eql;

  eql.exec(init_lib_MITSUKO__ALL_SYSTEMS);

  qDebug()<<"EQL core init finished.";
  int ret=0;
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ret = app.exec(); }
  CL_CATCH_ALL_END;
  return ret;
}
