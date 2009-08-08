# Java library imports
from java.lang import *
from java.awt import *
from java.awt.event import *
from javax.swing import *
from javax.swing.tree import *
# Holiday Planner Java library imports
from HolidayDefs import *
from MenuDefs import *
from TreeDefs import *
# Python library imports

# Holiday Planner Python library imports
from Menu import *
from Tree import *

class HolidayPlanner:
    
    def __init__(self):
        
        MENU_BAR.add( Menu( "File", 
                            [ "New Holiday", "Load Holiday", "Save Holiday", "Quit" ], 
                            [ { SHORTCUT : KeyEvent.VK_N, ACTION : self.__new },
                              { SHORTCUT : KeyEvent.VK_L, ACTION : self.__load },
                              { SHORTCUT : KeyEvent.VK_S, ACTION : self.__save},
                              { SHORTCUT : KeyEvent.VK_Q, ACTION : self.__quit} ]  ) )
       
        MENU_BAR.add( Menu( "Help", 
                            [ "About" ], 
                            [ { SHORTCUT : KeyEvent.VK_A, ACTION : self.__about } ] ) )
                            
        
        self.tabbedPane = JTabbedPane()
        self.tabbedPane.setBorder( BorderFactory.createEtchedBorder() )
    
        
        WEST_PANEL.setLayout( GridLayout( 1, 1 ) )

        self.xHOLIDAY_TREE = TreeProcessor( PopupMenu( "Edit",
                                                       [ "Add", "Edit", "Delete" ],
                                                       [ {  SHORTCUT : KeyEvent.VK_N, ACTION : self.__popNew },
                                                         {  SHORTCUT : KeyEvent.VK_E, ACTION : self.__popEdit },
                                                         {  SHORTCUT : KeyEvent.VK_D, ACTION : self.__popDelete } ] ), self.tabbedPane )
        
        self.xHOLIDAY_TREE.setModel( DictionaryTree( ROOT ) )    
        self.xHOLIDAY_TREE.setBorder( BorderFactory.createEtchedBorder() )
        
        WEST_PANEL.add( JScrollPane( self.xHOLIDAY_TREE ) )
        
        FRAME.setLayout( BorderLayout() )
        FRAME.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
        FRAME.setSize( 800, 800 )
        FRAME.setJMenuBar( MENU_BAR )
        FRAME.add( WEST_PANEL, BorderLayout.WEST )
        FRAME.add( self.tabbedPane, BorderLayout.CENTER )
        FRAME.setVisible( True )
       
    def __popNew(self): 
        print "Pop New"
        path = self.xHOLIDAY_TREE.getSelectionPath()
        #.getParentPath()
        pathCount = path.getPathCount()
        if pathCount == 1:
            self.__new()
        elif pathCount == 2:
            print "Adding new generic node to specifc Holiday"
        elif pathCount == 3:
            print "Adding new Itinerary, Accomodation, Transport, Activity OR new generic item"
        else:
            print "Don't know what to add"
            
        
        
    def __popEdit(self): 
        print "Pop Edit"
        selectedPath = self.xHOLIDAY_TREE.getSelectionPath()
        print "Looking for this tab:", selectedPath.getLastPathComponent( )
        tabIndex = self.tabbedPane.indexOfTab( str ( selectedPath.getLastPathComponent( ) ) )
        if tabIndex == -1:
            print "This tab is not currently displayed, retrieving...."
            # Need to create a class, and store references to it in tree for each node
            # The class will contain the JPanel that needs to be brought
            # into focus and all the data (if not stored in JPanel) to put into it
            # We'll need to reference each node by it's full path!
        else:
            print "Bringing tab into focus...."
            self.tabbedPane.setSelectedIndex ( tabIndex )
        
    def __popDelete(self): print "Pop Delete" 
        
    def __new(self):
        myNewRootHolidayNode = DefaultMutableTreeNode( JOptionPane.showInputDialog( FRAME, "Enter Holiday Name" ) )
        
        #print "Adding tabbed pane:", str( myNewRootHolidayNode )
        #newPanel = JPanel()
        #self.tabbedPane.addTab ( str( myNewRootHolidayNode ), JScrollPane( newPanel ) )
        
        self.xHOLIDAY_TREE.getModel().insertNodeInto( myNewRootHolidayNode, ROOT, 0, self.tabbedPane )
        self.xHOLIDAY_TREE.getModel().insertNodeInto( DefaultMutableTreeNode( "Itinerary" ),    myNewRootHolidayNode, 0, self.tabbedPane )
        self.xHOLIDAY_TREE.getModel().insertNodeInto( DefaultMutableTreeNode( "Accomodation" ), myNewRootHolidayNode, 1, self.tabbedPane )
        self.xHOLIDAY_TREE.getModel().insertNodeInto( DefaultMutableTreeNode( "Transport" ),    myNewRootHolidayNode, 2, self.tabbedPane )
        self.xHOLIDAY_TREE.getModel().insertNodeInto( DefaultMutableTreeNode( "Activities" ),   myNewRootHolidayNode, 3, self.tabbedPane )
        # There must be a better way of doing this!!!
        # Ideally you'd override the insertNodeInto method to always expand an inserted class
        # however if you keep the data and tree separate this isn't possible as 
        # insertNodeInto is part of the TreeModel and has no knowledge of the JTree.
        # You can probably catch the insert event and expand there, but will you
        # have the tree path and/or row at this point?
        self.xHOLIDAY_TREE.expandRow( 0 )
        self.xHOLIDAY_TREE.expandRow( 1 ) 
        
    def __load(self): print "load called"
    def __save(self): print "save called"
    
    def __quit(self):
        System.exit(0) 
    
    def __about(self): print "about called"
    
if __name__ == "__main__":
    print "Starting..."
    holidayPlanner = HolidayPlanner()