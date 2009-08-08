// Class to contain contants for the Tree *model*
// This is an attempt to separate the visible JTree from the model
// that drives it.
// Written in Java rather than Jython to utilize static final
// methods for efficiency and code robustness

import javax.swing.tree.*;

public class TreeDefs
{
	public static final DefaultMutableTreeNode ROOT = new DefaultMutableTreeNode( "My Holidays" );
}