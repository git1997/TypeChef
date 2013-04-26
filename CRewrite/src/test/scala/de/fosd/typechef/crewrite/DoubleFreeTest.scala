package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c.{Id, TestHelper}
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import de.fosd.typechef.featureexpr.FeatureExprFactory

class DoubleFreeTest extends TestHelper with ShouldMatchers {

    // check allocated memory locations
    private def getDynAllocatedMem(code: String) = {
        val a = parseCompoundStmt(code)
        val df = new DoubleFree(CASTEnv.createASTEnv(a), null, null)
        df.gen(a)
    }

    // check freed pointers
    private def getFreedMem(code: String) = {
        val a = parseCompoundStmt(code)
        val df = new DoubleFree(CASTEnv.createASTEnv(a), null, null)
        df.kill(a)
    }

    // check double free pointers
    private def getDoubleFree(code: String) = {
        val a = parseCompoundStmt(code)
        val df = new DoubleFree(CASTEnv.createASTEnv(a), null, null)
        df.in(a)
    }

    @Test def test_pointers() {
        getDynAllocatedMem("{ int *a = malloc(2); }") should be(Map(FeatureExprFactory.True -> Set(Id("a"))))
        getDynAllocatedMem("{ void *a,*b = malloc(2); }") should be(Map(FeatureExprFactory.True -> Set(Id("b"))))
        // tricky example: a and b are aliases for each other
        getDynAllocatedMem("{ void *a,*b; a = b = malloc(2); } ") should be(Map(FeatureExprFactory.True -> Set(Id("a"), Id("b"))))
        getDynAllocatedMem("{ void *a = malloc(2),*b; }") should be(Map(FeatureExprFactory.True -> Set(Id("a"))))
        getDynAllocatedMem("{ a = malloc(2); }") should be(Map(FeatureExprFactory.True -> Set(Id("a"))))
        getDynAllocatedMem(
            """{
              |struct expr {
              |  int a;
              |};
              |struct expr *e = malloc(sizeof(*e));
              |}""".stripMargin) should be(Map(FeatureExprFactory.True -> Set(Id("e"))))
    }

    @Test def test_free_simple() {
        getFreedMem("{ free(a); } ") should be(Map(FeatureExprFactory.True -> Set(Id("a"))))
    }

    @Test def test_doublefree_simple() {
        getDoubleFree("{ int *a = malloc(2); free(a); free(a); } ") should be(Map(Id("a") -> FeatureExprFactory.True))
        getDoubleFree("{ int *a = malloc(2); free(a); } ") should be(Map(Id("a") -> FeatureExprFactory.True))
        getDoubleFree("{ int *a = malloc(2); } ") should be(Map(Id("a") -> FeatureExprFactory.True))
    }
}
