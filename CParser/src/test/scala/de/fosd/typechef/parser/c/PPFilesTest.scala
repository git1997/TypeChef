package de.fosd.typechef.parser.c

import junit.framework.Assert._
import de.fosd.typechef.featureexpr._
import org.kiama.rewriting.Rewriter._
import org.junit.{Assert, Ignore, Test}
import java.util.Collections

class PPFilesTest {
    def parseFile(fileName: String) {
        val inputStream = getClass.getResourceAsStream("/" + fileName)
        assertNotNull("file not found " + fileName, inputStream)
        val p = new CParser()
        val result = p.translationUnit(
            CLexer.lexStream(inputStream, fileName, Collections.singletonList("testfiles/boa/"), null), FeatureExprFactory.True)
        def printResult(result:p.MultiParseResult[Any], fexpr:FeatureExpr): Unit =
        (result: @unchecked) match {
            case p.Success(ast, unparsed) => {
                val emptyLocation = checkPositionInformation(ast.asInstanceOf[Product])
                assertTrue(fexpr+": found nodes with empty location information", emptyLocation.isEmpty)
                assertTrue(fexpr+": parser did not reach end of token stream: " + unparsed, unparsed.atEnd)
                //succeed
            }
            case p.NoSuccess(msg, unparsed, inner) =>
                println(unparsed.context)
                Assert.fail(msg + " at " + unparsed + " " + inner)
            case p.SplittedParseResult(f,a,b) =>
                printResult(a, fexpr and f)
                printResult(b, fexpr andNot f)
        }
        printResult(result, FeatureExprFactory.True)

    }

    def checkPositionInformation(ast: Product): List[Product] = {
        assert(ast != null)
        var nodeswithoutposition: List[Product] = List()
        val checkpos = everywherebu(query {
            case a: AST => if (!a.hasPosition) nodeswithoutposition ::= a
        })
        checkpos(ast)
        nodeswithoutposition
    }

    //

    @Test
    def testEscapePi() {
        parseFile("boa/escape.pi")
    }

    @Test
    def testAliasPi() {
        parseFile("boa/alias.pi")
    }

    @Test
    @Ignore("not checked?")
    def testIpPi() {
        parseFile("boa/ip.pi")
    }

    @Test
    def testLineEditPi() {
        parseFile("other/lineedit.pi")
    }
    @Test
    def testUCLibCpi() {
        parseFile("other/_ppfs_setargs.pi")
    }

    @Test
    def testUCLibC() {
        parseFile("other/_ppfs_setargs.i")
    }

}