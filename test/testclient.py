#!/usr/bin/python

import pygtk
pygtk.require('2.0')
import gtk,sys

Wid = 0L
if len(sys.argv) == 2:
    Wid = long(sys.argv[1])

plug = gtk.Plug(Wid)
print "Plug ID=", plug.get_id()

def embed_event(widget):
    print "I (", widget, ") have just been embedded!"

plug.connect("embedded", embed_event)

entry2 = gtk.Entry()
entry2.set_text("Munghi")

entry = gtk.Entry()
entry.set_text("hello")
def entry_point(widget):
    print "You've changed my text to '%s'" % widget.get_text()

box = gtk.VBox(False, 0)
box.pack_end(entry2, True, True, 0)
box.pack_end(entry, True, True, 0)
entry2.connect("changed", entry_point)
entry.connect("changed", entry_point)
plug.connect("destroy", lambda w: gtk.main_quit())

plug.add(box)
plug.show_all()


gtk.main()
