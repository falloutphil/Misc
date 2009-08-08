# Java Imports
from java.io import File

# Lucene Imports
from org.apache.lucene.document import Document
from org.apache.lucene.document import Field
from org.apache.lucene.index import IndexWriter
from org.apache.lucene.analysis.standard import StandardAnalyzer

# Python Imports
import os

class FileIndexer:
    
    __fileList = []
    
    def __init__( self, dataDir, fileExtension, indexDir ):
        os.path.walk( dataDir, self.__fileSearcher, fileExtension )
        self.__writer = IndexWriter( indexDir, StandardAnalyzer(), True )
        self.__writer.setUseCompoundFile( False )
        
    def __fileSearcher( self, fileExtension, dirname, filenames ):
        #print "Directory:", dirname
        for filename in filenames:
            if filename.split('.')[-1] == fileExtension:
                self.__fileList.append( os.path.join( dirname, filename ) )
    
    def getNames( self ):
        return self.__fileList
    
    def indexFiles( self ):
        for filename in self.__fileList:
            print filename
            print File(filename).getCanonicalPath()
            doc = Document()
            doc.add( Field( "contents", open( filename, 'r' ).read(), Field.Store.YES, Field.Index.TOKENIZED ) )
            doc.add( Field( "path", File(filename).getCanonicalPath(), Field.Store.YES, Field.Index.UN_TOKENIZED ) )
            self.__writer.addDocument(doc)
            
    def optimizeAndClose( self ):
        docCount = self.__writer.docCount()
        self.__writer.optimize()
        self.__writer.close()
        return docCount
         
if __name__ == "__main__":
    print "Starting..."
    dataDir  = "/Users/philipbeadling/dev/DocSearch/TestDir/Data"
    indexDir = "/Users/philipbeadling/dev/DocSearch/TestDir/index"
    
    fileIndexer = FileIndexer( dataDir, "txt", indexDir )
    print fileIndexer.getNames()
    fileIndexer.indexFiles()
    print "Wrote out", str( fileIndexer.optimizeAndClose() ), "documents."
    print "Done..."
    
    


