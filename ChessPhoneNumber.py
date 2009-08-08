#!/usr/bin/env python

# Calculate possible valid phone numbers using
# chess moves on a telephone keypad.
# EDFT Programming Problems, Question 1.
# Written by Philip Beadling 03/04/08.

# Should work fine with Python 2.3+

# All our functionality for generating phone numbers
# is handled by this iterable class
class PhoneNumber:

	# Dictionaries of allowed next moves from all legal points
    __King   = { 0 : [0,7,8,9], 
                 1 : [1,2,4,5], 
                 2 : [2,1,3,4,5,6], 
                 3 : [3,2,5,6], 
                 4 : [4,1,7,2,5,8], 
                 5 : [5,1,2,3,4,6,7,8,9], 
                 6 : [6,3,2,5,8,9], 
                 7 : [7,4,5,8,0], 
                 8 : [8,0,7,4,5,6,9],
                 9 : [9,6,5,8,0] }
    
    __Queen =  { 0 : [0,7,8,5,2,9], 
                 1 : [1,2,3,5,9,4,7], 
                 2 : [2,1,3,4,5,8,0,6], 
                 3 : [3,2,1,5,7,6,9], 
                 4 : [4,1,7,2,5,6,8], 
                 5 : [5,1,2,3,4,6,7,8,0,9], 
                 6 : [6,3,2,5,4,8,9], 
                 7 : [7,4,1,5,3,8,9,0], 
                 8 : [8,0,7,4,5,2,6,9], 
                 9 : [9,6,3,5,1,8,7,0] }
    
    __Bishop = { 0 : [0,7,9], 
                 1 : [1,5,9], 
                 2 : [2,4,6], 
                 3 : [3,5,7], 
                 4 : [4,2,8], 
                 5 : [5,1,3,7,9], 
                 6 : [6,2,8], 
                 7 : [7,5,3], 
                 8 : [8,4,6], 
                 9 : [9,5,1,0] }
                                          
    __Knight = { 0 : [0,4,6], 
                 1 : [1,8,6], 
                 2 : [2,7,9], 
                 3 : [3,4,8], 
                 4 : [4,3,9,0], 
                 5 : [5], 
                 6 : [6,1,7,0], 
                 7 : [7,2,6], 
                 8 : [8,1,3], 
                 9 : [9,2,4] }
      
    __Rook   = { 0 : [0,8,5,2], 
                 1 : [1,2,3,4,7], 
                 2 : [2,1,3,5,8,0], 
                 3 : [3,2,1,6,9], 
                 4 : [4,1,7,5,6], 
                 5 : [5,2,4,6,8,0], 
                 6 : [6,3,5,4,9], 
                 7 : [7,4,1,8,9], 
                 8 : [8,0,7,5,2,9], 
                 9 : [9,6,3,8,7] }
                 
    # From positions on the bottom 2 rows it can only jump 2 positions if it is it's first go, we'll handle this later.
    # The values for 1,2,3 are arguably irrelevent as functionality below will convert the pawn in these positions
    # I've left them in for illustrative completeness
    __Pawn   = { 0 : [0,8,5], 
                 1 : [1], 
                 2 : [2], 
                 3 : [3], 
                 4 : [4,1], 
                 5 : [5,2], 
                 6 : [6,3], 
                 7 : [7,4,1], 
                 8 : [8,5,2], 
                 9 : [9,6,3] }
    
    def __init__(self, startPoint, chessPiece, numberLength):
        # Where we'll store an individual phone number - as a list
        self.__soFar = []
        # All numbers will start with the start point given
        self.__soFar.append( startPoint )
        # Length of desired phone number
        self.__phoneNumberLength = numberLength
        
        # Referance to the dictionary of legal moves we want
        self.__legalMoveDict = getattr( self, "_PhoneNumber__" + chessPiece )
        
        # Pawn needs some extra attention
        self.__pieceIsPawn = False
        if chessPiece == 'Pawn':
        	# A quick shortcut - if we start at positions 1,2,3 
        	# as a pawn then I'm assuming we instantly become a queen.
        	if startPoint in [1,2,3]:
        		self.__legalMoveDict = self.__Queen
        	else:
        		# Our next() generator will need
        		# to test for a pawn to handle the
        		# case when has become a queen
        		self.__pieceIsPawn = True
        		# The other requested special case
        		if startPoint != 8:
        			# We can only allow a pawn to move two
        			# spaces from position 8 if it is the first move
        			# i.e. we are starting from position 8.  If this
        			# is not the case, remove this as a possibility.
        			# The case is simpler for 0,7,9 because
        			# a pawn can never *move* there. The posibility
        			# of starting from position 0 dictates this requirment.
        			# One final possibility of the first 2 moves being (0,0), (7,7), (8,8) or (9,9)
        			# is dealt with in the next() below
        			self.__legalMoveDict[8] = [8,5]
        	

    
    # We want this class to be an iterator
    def __iter__(self):
        return self.next()
    
    # Note: The below iterator is itself a recursive generator.
    # Calling next() will return an new iteratable object which
    # itself can be .next()'ed.  This way Python will retain state
    # information for us, saving us the hassle.
    # The code to complete the exercise using this method is very
    # short, but warrants a fair bit of commenting.
    
    # Simple Case:
    # On first call if we have an N-digit number (eg 1 digit)
    # we *yield* that. Execution of next() freezes here
    # and the complete N-digit number is passed back.  
    # When the "generator's (instance of) next" is called by the parent ( the "main" for loop
    # in this simple case ) we will continue next()'s execution, popping the last digit 
    # and looping over the remaining possible moves in it's place:
    #   - appending, yeilding the answer to main, and then popping the leaf when execution resumes.  
    # When the nextPossibleMove loop completes
    # we *return* and any subsequent call to next() will result in
    # a stopIteration as we have no more yield points to hit.
    # In this simple case this will break the "main" client loop and exit.
    
    # Recusrive Case:
    # We have a loop similar to that in main in the else clause
    # below, which is hit when we have not yet reached N-digits.
    # The 'for v' loop construct will control execution over the child instance of
    # 'for nextPossibleMove'. As with the simple case, this will return a valid phone number (or enter
    # another level of recursion) until the child's loop has no more *yield* 
    # points to hit (that is, it has exhausted all possible moves from this point).
    # Thus we are doing the same thing as above - appending, yeilding (or recursing), and popping,
    # with the *parent* instance generated by next(self) controlling the process flow
    # of the child instance via the 'for v' loop.
    # On exhaustion of a child's self.next() instance the last call will find
    # no subsequent 'yield' calls, but it will pop the last outer leaf before returning,
    # leaving the stem as it was.  The parent's parent will then call next() on the parent
    # which will pop the parent and continue looping, appending the next possible (parent) move
    # from this point or if no further move exists for the parent, 
    # the parent will return having popped it's own last move, and so on....
    
    # Visually we are creating a tree structure which will recurse from base to leaf, and then sweep
    # across the tree structure, with each node either automatically returning itself when it reaches 
    # the required number of hops from the base, or spawing another set of legal leaves which will
    # continue the process.

    def next(self):
        # Return a list of possible moves from our current position (last list member)
        # and loop over them.
        for nextPossibleMove in self.__legalMoveDict[ self.__soFar[-1] ]:
            # Append the next possible position given 
            self.__soFar.append( nextPossibleMove )
            # Pawns need special treatment
            if self.__pieceIsPawn:
                if len( set([1,2,3]).intersection(self.__soFar) ):
                    # If the pawn and has *ever* touched the top row
                    # in this stem, treat it as a queen
                    self.__legalMoveDict =  self.__Queen
                # Have to cater for the case when we start at 
                # bottom 2 rows, but our subsequent move.is to remain at 
                # in the same position.
                # From the wording of the specification as this is our second move
                # and thus we can no longer jump to 2 positions from herein
                elif self.__soFar == [0,0]:
                     self.__legalMoveDict[0] = [0,8]
                elif self.__soFar == [7,7]:
                     self.__legalMoveDict[7] = [7,4]
                elif self.__soFar == [8,8]:
                     self.__legalMoveDict[8] = [8,5]
                elif self.__soFar == [9,9]:
                     self.__legalMoveDict[9] = [9,6]
            # If our number has reached the required N
            # digits return it to the parent
            if len( self.__soFar ) == self.__phoneNumberLength:
                # We have a valid number to yield to
                # the client *via* our nested recursive calls.  Execution will resume with
                # the pawn test and pop below, making way for the next "nextPossibleMove"
                # as controlled by the parent.
                # Stringify the list of ints before yielding as result will be printed
                yield ''.join([`num` for num in self.__soFar])
            else:
                # The phone number isn't long enough yet.
                # We can't just recurse directly by looping over "self"
                # as next() itself returns a new generator instance.
                # We must iterate over the instance returned by self.next()
                # collecting all the next possible positions yielded (one per iteration) 
                # passed back by the child nextPossibleMove loop.
                # Each next() call will ultimately return a "yield self.__soFar" complete result (above)
                # until there are no more (child's nextPossbleMove loop ends) and the child returns, 
                # after popping itself.  The yeild-ed results in this way are propagated back up the tree.
                # Each of these is passed back to the parent which then calls next on *this* instance,
                # to pop this object's leaf and replaces it with the next legal digit (or returns).
                for v in self.next():
                   yield v
            
            # Execution resumes here.
            # Revert a potentially converted pawn back to a pawn 
            # (perhaps only our leaf caused the conversion so we can't retain this setting)
            # the next stem will set this again if needs be
            if self.__pieceIsPawn:
                 self.__legalMoveDict = self.__Pawn
            # pop the last successful number (thus retaining the stem's original structure) and 
            # continue the loop to get the next possible move.
            # If there are no more possible moves we return control to the parent.
            self.__soFar.pop()
            


# Our main Function containing our outer loop
if __name__ == '__main__':
	# Handle command line parameters
	from optparse import OptionParser
	pieceChoice = ['King', 'Queen', 'Bishop', 'Knight', 'Rook', 'Pawn' ]
	defaultNumberLength = 10
	parser = OptionParser()
	parser.add_option("-s", "--start",  action="store", type="int",    dest="startValue", help="Start value on the telephone keypad")
	parser.add_option("-p", "--piece",  action="store", type="choice", dest="chessPiece", choices=pieceChoice, help="Name of the chess piece to use: " + ", ".join(pieceChoice))
	parser.add_option("-l", "--length", action="store", type="int",    dest="numberLength", default=defaultNumberLength, help="Length of the phone number to generate (defaults to %d)" % defaultNumberLength)
	
	(options,args) = parser.parse_args()
	if (options.startValue == None) or (options.chessPiece == None):
		parser.print_help()
	else:
		print "List of possible phone numbers:"
		# Our "main" loop and entry point to our recursion
		# Each iteration will produce a valid phone number
		for possibleNumber in PhoneNumber( options.startValue, options.chessPiece, options.numberLength ):
			print possibleNumber
	


     