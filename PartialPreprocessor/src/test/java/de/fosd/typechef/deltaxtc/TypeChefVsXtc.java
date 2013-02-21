package de.fosd.typechef.deltaxtc;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import de.fosd.typechef.lexer.*;
import de.fosd.typechef.xtclexer.XtcPreprocessor;
import junit.framework.Assert;
import org.junit.Ignore;
import org.junit.Test;
import xtc.LexerInterface;
import xtc.lang.cpp.Stream;
import xtc.lang.cpp.Syntax;

import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

/**
 * the goal of this test is to compare the lexers of
 * TypeChef and Xtc/SuperC
 * <p/>
 * needs to run with -XX:-UseSplitVerifier
 *
 * @author kaestner
 */
public class TypeChefVsXtc {


    @Test
    public void testNestingDead() throws LexerException, IOException {
        testFile("nestingdead.c");
    }

    @Test
    public void testDeadElse() throws LexerException, IOException {
        testFile("deadelse.h");
    }

    @Test
    public void testIncludeGuard() throws LexerException, IOException {
        testFile("in1.c");
    }

    @Test
    public void testUnlikely() throws LexerException, IOException {
        testFile("unlikely.h");
    }

    @Test
    @Ignore("intended solution inclear. probably both correct")
    public void testByteOrder() throws LexerException, IOException {
        testFile("byteorder.h");
    }

//	@Test
//	@Ignore
//	public void testIf() throws LexerException, IOException {
//		testFile("if.h");
//	}

    @Test
    public void testAlternativeMacros() throws LexerException, IOException {
        testFile("macro2.c");
    }

    @Test
    public void testIncludeGuards() throws LexerException, IOException {
        testFile("includeguards.c");
    }

    @Test
    public void testIncludeGuards2() throws LexerException, IOException {
        testFile("includeguards2.h");
    }

    @Test
    @Ignore("this test case is not valid c code")
    public void testDefDefined() throws LexerException, IOException {
        testFile("defdefined.c");
    }

    @Test
    public void testAlternativeDef() throws LexerException, IOException {
        testFile("alternativedef.c");
    }

    @Test
    public void testHiddenBaseAndDead() throws LexerException, IOException {
        testFile("hiddenDeadAndBase.c");
    }

//	@Test
//	@Ignore
//	public void testMultiInclude() throws LexerException, IOException {
//		// XXX this is not supported right now. let's see whether we will need
//		// it.
//		testFile("multiinclude.c");
//	}

    @Test
    @Ignore("apparently Xtc does not handle nonboolean expressions, at least it does not simplify them")
    public void testIfCondition() throws LexerException, IOException {
        testFile("ifcondition.c");
    }

    @Test
    public void testBeispielJoerg() throws LexerException, IOException {
        testFile("beispielJoerg.c");
    }

    @Test
    public void testNumericIfAlternative() throws LexerException, IOException {
        testFile("ifdefnumeric.c");
    }

    @Test
    public void testLinuxTestFLock() throws LexerException, IOException {
        testFile("linuxtestflock.c");
    }

    @Test
    public void testElIfChain() throws LexerException, IOException {
        testFile("elifchain.c");
    }

    @Test
    public void testSelfDef() throws LexerException, IOException {
        testFile("selfdef.c");
    }

    @Test
    public void testNonTautologicExpansions() throws LexerException,
            IOException {
        testFile("non_tautologic.c");
    }

    @Test
    public void testVariadic() throws LexerException, IOException {
        testFile("variadic.c");
    }

    @Test
    @Ignore("expects an error in either case")
    public void testIncompMacroExp() throws LexerException, IOException {
        testFile("incompatibleMacroExp.c");
    }

    @Test
    public void testRedef() throws LexerException, IOException {
        testFile("redef.h");
    }

    //jiffies contains complex calculations; from the linux kernel headers
    @Test
    public void testJiffies() throws LexerException, IOException {
        testFile("jiffiesTest.h");
    }

    @Test
    public void testIncludeMacros() throws LexerException, IOException {
        testFile("includemacro.c");
    }

    @Test
    public void testRecursiveMacro() throws LexerException, IOException {
        testFile("recursivemacro.h");
    }

    @Test
    public void testStringifyNl() throws LexerException, IOException {
        testFile("stringifyNl.c");
    }

    @Test
    public void testUseCondDef() throws LexerException, IOException {
        testFile("useconddef.c");
    }

    @Test
    public void testDivByZero() throws LexerException, IOException {
        testFile("test_div_by_zero.c");
    }

    @Test
    @Ignore("noncritical bug in Xtc")
    public void testDivByZero2() throws LexerException, IOException {
        testFile("test_div_by_zero2.c");
    }

    @Test
    public void testMacroPNF() throws LexerException, IOException {
        testFile("macroPFN.c");
    }

    @Test
    public void testParametricMacro() throws LexerException, IOException {
        testFile("parametricmacro.h");
    }

    @Test
    public void testParametricMacro2() throws LexerException, IOException {
        testFile("parametricmacro2.h");
    }

    @Test
    public void testKBuildStr() throws LexerException, IOException {
        testFile("kbuildstr.c");
    }

    @Test
    @Ignore("bug in typechef")
    public void testStringify() throws LexerException, IOException {
        testFile("stringify.c");
    }

    @Test
    @Ignore("typechef problem?")
    public void testAlternativeDifferentArities1() throws LexerException, IOException {
        testFile("alternDiffArities1.c");
    }

    @Test
    public void testAlternativeDifferentArities2() throws LexerException, IOException {
        testFile("alternDiffArities2.c");
    }

    @Test
    @Ignore("Xtc does not seem to have buildins for __date__ and __time__. not important")
    public void testDateTime() throws LexerException, IOException {
        testFile("dateTime.c");
    }

    @Test
    public void testNumbers() throws LexerException, IOException {
        testFile("numbers.c");
    }

    @Test
    public void testConcatVarargs() throws LexerException, IOException {
        testFile("concatVarargs.c");
    }

    @Test
    public void testDeadcomparison() throws LexerException, IOException {
        testFile("deadcomparison.c");
    }

    @Test
    public void testExpandWithinExpand() throws LexerException, IOException {
        testFile("expandWithinExpand.c", false, true);
    }

    @Test
    @Ignore //TODO fix
    public void testLinebreaks() throws LexerException, IOException {
        testFile("linebreaks.c", false, true);
    }

    @Test
    @Ignore("cpp warning. reported as error by Xtc. not so important I guess")
    public void testLinebreaks2() throws LexerException, IOException {
        testFile("linebreaks2.c", false, true);
    }

    @Test
    @Ignore("absolute vs relative path. does not matter")
    public void testFileBaseFile() throws LexerException, IOException {
        testFile("filebasefile.c", false, true);
    }

    @Test
    public void testBnx2() throws LexerException, IOException {
        testFile("bnx2.c", false, true);
    }

    @Test
    @Ignore("bug in lexer, see issue #10")
    public void testBnx() throws LexerException, IOException {
        testFile("bnx.c", false, true);
    }

    @Test
    public void testVarargs() throws LexerException, IOException {
        testFile("varargs.c", false, true);
    }


    /**
     * parses a file and checks the result against the results specified in the
     * filename.check file
     *
     * @param filename
     * @throws LexerException
     * @throws IOException
     */
    protected void testFile(String filename) throws LexerException, IOException {
        testFile(filename, false);
    }


    protected void testFile(String filename, boolean debug) throws LexerException, IOException {
        testFile(filename, debug, false);
    }

    String folder = "tc_data/";

    protected void testFile(String filename, boolean debug, boolean ignoreWarning) throws LexerException, IOException {
        File file = getFile(filename);

        List<Token> xtcTokens = null, typechefTokens = null;
        Exception xtcException = null, typechefException = null;
        try {
            xtcTokens = getXtcTokens(filename);
        } catch (Exception e) {
            e.printStackTrace();
            xtcException = e;
        }


        try {
            typechefTokens = new PartialPPLexer().parseStream(getClass().getResourceAsStream(
                    "/" + folder + filename), filename, Collections.singletonList(file.getParent()), FeatureExprLib.featureModelFactory().empty());
        } catch (Exception e) {
            e.printStackTrace();
            typechefException = e;
        }

        if (xtcException != null && typechefException != null) return;
        Assert.assertTrue("either both succeed or both should fail " + xtcException + " vs " + typechefException, (xtcException == null) == (typechefException == null));
        Assert.assertEquals(xtcTokens.size(), typechefTokens.size());
        for (int i = 0; i < xtcTokens.size(); i++) {
            Assert.assertEquals(xtcTokens.get(i).getText(), typechefTokens.get(i).getText());
            Assert.assertTrue("feature expressions mismatch: " + xtcTokens.get(i).getFeature() + " - " + typechefTokens.get(i).getFeature(), xtcTokens.get(i).getFeature().equivalentTo(typechefTokens.get(i).getFeature()));
        }


//        LexerInterface.print(lexer);
    }

    private File getFile(String filename) {
        URL fileURL = getClass().getResource("/" + folder + filename);
        File file = null;
        try {
            file = new File(fileURL.toURI());
        } catch (URISyntaxException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
        return file;
    }


    private List<Token> getXtcTokens(String filename) throws IOException {
        InputStream inputStream = getClass().getResourceAsStream(
                "/" + folder + filename);
        Assert.assertNotNull("cannot load file /" + folder + filename + ".check", inputStream);
        BufferedReader checkFile = new BufferedReader(new InputStreamReader(
                new ExtraLinebreakInputStream(inputStream)));


        List<Stream> lexers = LexerInterface.createLexer("", checkFile, getFile(filename), new LexerInterface.ExceptionErrorHandler(),
                Collections.EMPTY_LIST, Collections.singletonList(getFile(filename).getParent()), Collections.EMPTY_LIST, null);

        //create TypeChef style token stream
        List<Token> result = new ArrayList<Token>();

        Stack<FeatureExpr> stack = new Stack<FeatureExpr>();
        stack.push(FeatureExprFactory.True());
        for (Stream lexer : lexers) {
            Syntax s = lexer.scan();
            while (s.kind() != Syntax.Kind.EOF) {
                if (s.kind() == Syntax.Kind.CONDITIONAL) {
                    Syntax.Conditional c = s.toConditional();
                    if (c.tag() == Syntax.ConditionalTag.START)
                        stack.push(stack.peek().and(XtcPreprocessor.translate(c.presenceCondition())));
                    else if (c.tag() == Syntax.ConditionalTag.NEXT) {
                        stack.pop();
                        stack.push(stack.peek().and(XtcPreprocessor.translate(c.presenceCondition())));
                    } else stack.pop();
                }

                if (s.kind() == Syntax.Kind.LANGUAGE)
                    result.add(new XtcPreprocessor.XtcToken(s, stack.peek()));

                s = lexer.scan();
            }
        }
        return result;

    }


    static class ExtraLinebreakInputStream extends InputStream {
        private final InputStream that;
        private boolean read = false;

        public ExtraLinebreakInputStream(InputStream that) {
            this.that = that;
        }

        public int read() throws IOException {
            int r = that.read();
            if (r == -1 && !read) {
                read = true;
                return 13;
            }
            return r;

        }
    }

}
