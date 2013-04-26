package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c.{Id, TestHelper}
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import de.fosd.typechef.featureexpr.FeatureExprFactory

class DoubleFreeTest extends TestHelper with ShouldMatchers {

    // runs an example
    // parses code as compound statement so we can parse declarations, expressions, and statements
    // without having to call different parse functions
    private def getDynAllocatedMem(code: String) = {
        val a = parseCompoundStmt(code)
        val df = new DoubleFree(CASTEnv.createASTEnv(a), null, null)
        df.in(a)
    }

//    // checks a whole compound statement for double free errors
//    private def checkCompoundStatement(code: String) = {
//        val a = parseCompoundStmt(code)
//        check(a, FeatureExprFactory.empty).isEmpty
//    }

    @Test def test_pointers() {
        getDynAllocatedMem("{ int *a = malloc(2); }") should be(Set(Id("a")))
        getDynAllocatedMem("{ void *a,*b = malloc(2); }") should be(Set(Id("b")))
        // tricky example: a and b are aliases for each other
        getDynAllocatedMem("{ void *a,*b; a = b = malloc(2); } ") should be(Set(Id("a"), Id("b")))
        getDynAllocatedMem("{ void *a = malloc(2),*b; }") should be(Set(Id("a")))
        getDynAllocatedMem("{ a = malloc(2); }") should be(Set(Id("a")))
        getDynAllocatedMem(
            """{
              |struct expr {
              |  int a;
              |};
              |struct expr *e = malloc(sizeof(*e));
              |}""".stripMargin) should be(Set(Id("e")))
    }

//    @Test def test_doublefree_simple() {
//        checkCompoundStatement("{ int *a = malloc(2); free(a); free(a); } ") should be(false)
//        checkCompoundStatement("{ int *a = malloc(2); free(a); } ") should be(true)
//    }
}
