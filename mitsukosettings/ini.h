/**
 * @file ini.h
 * @copyright 2022 Aardsoft Oy
 * @author Bernd Wachter <bwachter@aardsoft.fi>
 * @date 2022
 */

#ifndef _INI_H
#define _INI_H

#include <QObject>

QT_BEGIN_NAMESPACE

extern "C" {
  QObject* ini();
}

QT_END_NAMESPACE

#endif
