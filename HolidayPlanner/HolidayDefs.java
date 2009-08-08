// Class to contain app wide GUI contants for the Holiday Planner
// Written in Java rather than Jython to utilize static final
// methods for efficiency and code robustness

import javax.swing.*;
import javax.swing.tree.*;

public class HolidayDefs
{
	public static final JFrame     FRAME        = new JFrame( "Holiday Planner" );
	public static final JPanel     WEST_PANEL   = new JPanel( );
	public static final JMenuBar   MENU_BAR     = new JMenuBar( );		
	public static final JTree      HOLIDAY_TREE = new JTree( );
	public static final JPopupMenu TREE_POPUP   = new JPopupMenu( "Edit" );
}