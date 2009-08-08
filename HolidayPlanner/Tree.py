from java.awt.event import *
from javax.swing import *
from javax.swing.event import *
from javax.swing.tree import *
# Holiday Planner Java library imports
from HolidayDefs import *
# Holiday Planner Python library imports
from Menu import *

class TabContainer:
    def __init__(self, dictName):
        self.__name = dictName
        self.__containerPanel = JPanel()
        self.__containerPanel.setBorder( BorderFactory.createEtchedBorder() )
        self.__containerPanel.add( JLabel( dictName ) )
                                   
    def getPanel(self):
        return self.__containerPanel
    
        
class DictionaryTree( DefaultTreeModel ):
    def __init__( self, rootNode ):
        DefaultTreeModel.__init__( self, rootNode  )
        self.dictionary = { rootNode : { } }
    
    def insertNodeInto( self, newNode, parentNode, position, tabbedPane ):
        DefaultTreeModel.insertNodeInto( self, newNode, parentNode, position )
        print "Adding tabbed pane:", str( newNode )
        # We don't want to add the pane every time we add a node???
        tabbedPane.addTab ( str( newNode ), JScrollPane( JPanel( ) ) )
        self.__dictionaryWalker( self.dictionary, parentNode, newNode )
    
        print "THE DICTIONARY:"
        print self.dictionary
            
    def getContainer( rootDict, nodeName ):
        if rootDict.has_key( nodeName ):
            pass
        
        
    def __dictionaryWalker( self, rootDict, desiredParentDict, newDict ):
        print "Walker called"
        if rootDict.has_key( desiredParentDict ):
            print "Have the desired parent, adding child"
            testContainer = TabContainer( str( newDict ) )
            rootDict[ desiredParentDict ][ newDict ] = { "Container" : testContainer  }
            return True
        elif rootDict == { }:
            print "rootNode is empty dictionary"
            return False
        else:
            print "Not found desired parent, looping over rootDicts children"
            for key in rootDict:
                print "Looping over", key
                if self.__dictionaryWalker( rootDict[key], desiredParentDict, newDict ):
                    print "Walker Process returned TRUE"
                    return True
 
            
        
class treeMouseAdapter( MouseAdapter ):
    def __init__( self, popup, tabbedPane ):
        self.__popup = popup
        self.__tabbedPane = tabbedPane
        
    def mousePressed( self, e ):
        jtree = e.getSource()
        x = e.getX()
        y = e.getY()
        selectedPath =  jtree.getClosestPathForLocation( x, y )
        if e.isMetaDown():
             jtree.setSelectionPath( selectedPath  )
             self.__popup.show( jtree, x, y )
        elif e.getClickCount( ) == 2:
            print "Double Click"
            #print "Looking for this tab:", selectedPath.getLastPathComponent( )
            #tabIndex = self.__tabbedPane.indexOfTab( str ( selectedPath.getLastPathComponent( ) ) )
            #self.__tabbedPane.setSelectedIndex ( tabIndex )
            #if self.__tabbedPane.getSelectedComponent !=
            #self.__tabbedPane.setSelectedComponent( )
        else:
            print "Click type not registered"
            
             
class TreeProcessor( JTree ):
    def __init__( self, popup, tabbedPane ):
        JTree.__init__( self )        
        JTree.addMouseListener( self, treeMouseAdapter( popup, tabbedPane ) )
        
    
        