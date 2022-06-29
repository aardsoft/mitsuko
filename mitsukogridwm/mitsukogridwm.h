/**
 * @file mitsukogridwm.h
 * @copyright 2020 Aardsoft Oy
 * @author Bernd Wachter <bwachter@lart.info>
 * @date 2020
 */

#ifndef _MITSUKOGRIDWM_H
#define _MITSUKOGRIDWM_H

#include <QQmlExtensionPlugin>

class MitsukoGridWM: public QQmlExtensionPlugin{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org.qt-project.Qt.QQmlExtensionInterface")
protected:
    void registerTypes(const char* uri);
};

#endif
