#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
graphical user interface
"""
#! /usr/bin/python
# -*- coding: utf-8 -*-
import sys
from PyQt4.QtGui import QLabel, QApplication, QWidget, QLineEdit, QProgressBar
from PyQt4.QtGui import QPushButton, QGridLayout, QComboBox, QMessageBox, QIcon
from PyQt4.QtCore import QObject, SIGNAL, QTimer
import webbrowser
import platform
from queue import *
import threading
import os
import math
import subprocess

class MainWindow (QWidget):
    """
    the main window of the GUI.
    """
    def __init__ (self):
        """
        constructor of the main window
        """
        QWidget.__init__ (self)
        self.layout = QGridLayout(self)
        self.setLayout(self.layout)
        self.edit = QLineEdit("http://en.wikipedia.org/wiki/Homomorphism", self)
        self.layout.addWidget(self.edit, 0, 0, 1, 2)
        self.queue = Queue()

        self.timer = QTimer(self) 
        QObject.connect(self.timer, SIGNAL("timeout()"), self.tick)
        self.timer.start(10)
        self.combo = QComboBox(self)
        self.combo.addItem("Print")
        self.combo.addItem("MediaWiki")
        self.combo.addItem("normal")
        self.layout.addWidget(self.combo, 1, 1, 1, 1)
        self.layout.addWidget(QLabel("Template Expansion"), 1, 0, 1, 1)
        

        self.papercombo = QComboBox(self)
        self.papercombo.addItem("A4")
        self.papercombo.addItem("A5")
        self.papercombo.addItem("B5")
        self.papercombo.addItem("letter")
        self.papercombo.addItem("legal")
        self.papercombo.addItem("executive")
       
        self.layout.addWidget(self.papercombo, 2, 1, 1, 1)
        self.layout.addWidget(QLabel("Paper"), 2, 0, 1, 1)


        self.rastercombo = QComboBox(self)
        self.rastercombo.addItem("Rasterize")
        self.rastercombo.addItem("Keep vector form")
        self.layout.addWidget(self.rastercombo, 3, 1, 1, 1)
        self.layout.addWidget(QLabel("Vector Graphics"), 3, 0, 1, 1)



        self.inccombo = QComboBox(self)
        if platform.system() == "Linux": 
            self.inccombo.addItem("Included")
            self.inccombo.addItem("Excluded")
        else:
            self.inccombo.addItem("excluded")
        self.layout.addWidget(self.inccombo, 4, 1, 1, 1)
        self.layout.addWidget(QLabel("LaTeX Source"), 4, 0, 1, 1)

        self.button = QPushButton("Run", self)
        self.layout.addWidget(self.button, 5, 0, 1, 2)
        self.counter = 0
        QObject.connect(self.button, SIGNAL("clicked()"), self.callbackrun)

        self.pbar = QProgressBar(self)
        self.pbar.setGeometry(30, 40, 200, 25)
        self.pbar.setValue(0)
        self.layout.addWidget(self.pbar, 6, 0, 1, 2)

 
        self.resize (340, -1)
        self.setWindowTitle("MediaWiki to LaTeX")
 
    def callbackrun(self):
        """
        Callback function that is called when the user clicks the run button.
        """
        try:
            if os.path.exists("main.pdf"):
                mainfile = open("main.pdf", "ab")
                mainfile.close()
        except:
            QMessageBox.about(self, "File still open", 
              "Please close your PDF viewer before pressing the run button!\n" +
              " This program need writing permission on the PDF file!")
            return

        self.counter = 0
        self.button.setEnabled(False)

        thread = threading.Thread()
        thread.run = self.callbackthreadrun
        thread.start()

    def tick(self):
        """
        callback function for the times. It is called every 10 milliseconds. 
        """
        got = False
        try:
            item = self.queue.get(False)
            got = True
        except:
            pass
        if got:
            self.callbacklog(item)
        self.timer.start(10)
    def getSwitch(self, switch):
        if switch=="MediaWiki": return " -m "
        if switch=="normal": return " -i "
        return ""
    def getVector(self, switch):
        if switch=="Rasterize": return ""
        return "-g"

    def callbackthreadrun(self):
        """
        callback for a thread. When the used clicks the run button. The callbackrun function is
        called back. This starts a thread that called this functions. So this function runs in
        multithreaded context. So it cannot interact with the gui because of multithreading hazards 
        """
        p=os.system ("mediawiki2latex -u "+self.edit.text()+" "+self.getSwitch(self.combo.currentText())+self.getVector(self.rastercombo.currentText())+" -o main.pdf")
        self.queue.put(["Done"])
        
    def callbacklog(self, item):
        """
        called back when the logger of the main program log a message. Used to update the progress
        bar. And to display the resulting pdf file in a pdf viewer if the main program has finished.
        This function is called from the main GUI loop. Essentially from the timer. So here are
        not multithreading problems and you can directly interact with all components of the GUI.
        """
        self.counter += 1
        self.pbar.setValue(int(100.0 - 100 * math.exp(-self.counter / 5000.0)))
        if item[0] == "Done":
            self.pbar.setValue(100)
            webbrowser.open('file://%s' % os.path.abspath(
                "main.pdf"))
            self.button.setEnabled(True)


    def closeEvent(self, event):
        """
        callback function called when this window is closes. Disable the callback function from the
        logger of the main program to this window to avoid calling callbacks on this windows in 
        future since it will be destroyed 
        """
        pass

    def callbackthread(self, logmessage):
        """
        callback called from the logger of the main program. This is called back from the tread of the
        main program. So it can not directly interact with the GUI because of multithreading hazards
        this way it just puts the message into a synchronized queue which will be polled by the timer
        and essentially call the callbacklog function in the context of the GUIs main thread
        """
        self.queue.put(logmessage)

 
#--------------- main ------------------
if __name__ == '__main__':
 
    APP = QApplication(sys.argv)
    # style = QStyleFactory.create('Cleanlooks')
    # APP.setStyle(style)

    MAINWINDOW = MainWindow ()
    MAINWINDOW.setWindowIcon(QIcon('logo.ico'))
    MAINWINDOW.show ()
    APP.exec_ ()
  
