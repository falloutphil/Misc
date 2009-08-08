
from java.awt.event import *
from javax.swing import *

from MenuDefs import *

class MenuProcessor:
    def __init__(self, menuList, menuFunctions):
        iterMenuFunctions = iter ( menuFunctions )
        menuBehaviourDict = dict ( [ ( name, iterMenuFunctions.next() ) for name in menuList ] )

        # Item list is used so that menu items are created in order.
        # We loop over each item in the list creating a JMenuItem for each
        for optionName in menuList:
            # First create the Menu item to be stored our new menu  
            newItem = JMenuItem( optionName )
            # Now get the unordered dictionary entry equivalent to the ordered list.
            menuActions =  menuBehaviourDict[ optionName ]
            # Loop through the predefined 'menu item processes' (eg 'Shortcut' and 'Action') to see if the
            # current menu item has registered itself against these.
            for controlItem in self.__menuControl.keys():
                if menuActions.has_key( controlItem ):
                    # If it has registered itself against the process, call the associated
                    # function in the menuControl dictionary to register with Swing.
                    # A reference to the JMenu is passed to aid registration.
                    # menuControl 'values' are references to helper functions in the MenuProcessor class.
                    # menuActions[ controlItem ] will either store the keystroke (addShortcut) to add
                    # or a reference to the function to be called when the ActionListener
                    # event is raised (addAction)
                    self.__menuControl[ controlItem ]( self, menuActions[ controlItem ], newItem )
            self.add( newItem )
            
    # Adds the defined keystroke to the referanced JMenu
    def __addShortcut( self, keystroke, jmenuRef ):
        jmenuRef.setMnemonic( keystroke )
    # Adds an ActionListener to the referanced JMenu
    # and specifies the action to be taken
    def __addAction( self, action, jmenuRef ):
        # New ActionListener must be created
        class menuItemActionListener( ActionListener ):
            def __init__(self, action):
                # Store the function reference passed in
                # as a class member.
                self.__theAction = action    
            def actionPerformed( self, e ):
                #print e.getActionCommand()
                # When the menu item is clicked, execute registered function
                self.__theAction()
         # Add the ActionListener to the JMenu item       
        jmenuRef.addActionListener( menuItemActionListener( action ) )
              
    # Dictionary containing all possible behaviours as 'Keys' and function
    # references to implement these behaviours as 'Values'
    # NOTE: This MUST go at the bottom of the class definition
    #       otherwise Jython cannot see the function defintions first
    #       and throws a 'NameError'.  Silly - I know.
    __menuControl = { SHORTCUT : __addShortcut, ACTION : __addAction }    
        
        
class Menu( JMenu, MenuProcessor ):
    def __init__( self, menuName, menuList, menuFunctions ):
        JMenu.__init__( self, menuName )
        MenuProcessor.__init__( self, menuList, menuFunctions )
        
class PopupMenu ( JPopupMenu, MenuProcessor ):
    def __init__( self, menuName, menuList, menuFunctions ):
        JPopupMenu.__init__( self, menuName )
        MenuProcessor.__init__( self, menuList, menuFunctions )
        
        