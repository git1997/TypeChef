package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c.{FunctionDef, Id, TestHelper}
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import de.fosd.typechef.featureexpr.FeatureExprFactory

class DoubleFreeTest extends TestHelper with ShouldMatchers with CFGHelper {

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
    private def hasDoubleFree(code: String): Boolean = {
        val f = parseFunctionDef(code)
        var res = false
        val df = new DoubleFree(CASTEnv.createASTEnv(f), null, null)

        val env = CASTEnv.createASTEnv(f)
        val ss = getAllSucc(f.stmt.innerStatements.head.entry, FeatureExprFactory.empty, env)

        val nss = ss.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])
        for (s <- nss) {
            val g = df.gen(s)
            val pd = df.out(s)

            for ((i, _) <- pd)
                for ((_, j) <- g)
                    if (j.contains(i))
                        res = true

        }
        res
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
        hasDoubleFree("""void foo() {
                 free(a);
                 #ifdef A
                 free(a);
                 #endif
                 } """) should be(true)
        hasDoubleFree(
            """
              void foo() {
              int *a = malloc(2);
              free(a);
              }
            """.stripMargin) should be(false)
        // take from: https://www.securecoding.cert.org/confluence/display/seccode/MEM31-C.+Free+dynamically+allocated+memory+exactly+once
        hasDoubleFree(
            """
              int f(int n) {
                int error_condition = 0;

                int *x = (int*)malloc(n * sizeof(int));
                if (x == NULL)
                  return -1;

                /* Use x and set error_condition on error. */

                if (error_condition == 1) {
                  /* Handle error condition*/
                  free(x);
                }

                /* ... */
                free(x);
                return error_condition;
              }
            """.stripMargin) should be(true)
    }
}
