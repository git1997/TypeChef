package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c.{Id, TestHelper}
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

class DoubleFreeTest extends TestHelper with ShouldMatchers with DoubleFree {

    // runs an example
    // parses code as compoundstatement so we can parse declarations, expressions, and statements
    // without having to call different parse functions
    private def runExample(code: String) = {
        val a = parseCompoundStmt(code)
        getHeapPointers(a)
    }

    @Test def test_pointers() {
        runExample("{ int *a = malloc(2); }") should be(Set(Id("a")))
        runExample("{ void *a,*b = malloc(2); }") should be(Set(Id("b")))
        // tricky example: a and b are aliases for each other
        runExample("{ void *a,*b; a = b = malloc(2); } ") should be(Set(Id("a"), Id("b")))
        runExample("{ void *a = malloc(2),*b; }") should be(Set(Id("a")))
        runExample("{ a = malloc(2); }") should be(Set(Id("a")))
        runExample(
            """{
              |struct expr {
              |  int a;
              |};
              |struct expr *e = malloc(sizeof(*e));
              |}""".stripMargin) should be(Set(Id("e")))
    }
}
