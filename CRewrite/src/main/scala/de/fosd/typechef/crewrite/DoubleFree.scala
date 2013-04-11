package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._

import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c.Id

// implements a simple analysis of double-free
// freeing memory multiple times [dblfree]
// see http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1669.pdf 5.22
//
// major limitations:
//   - without an alias analysis we are not capable of
//     detecting double frees called on different pointers
//     directing to the same memory location
//   - we use intraprocedural control flow (IntraCFG) which
//     is a conservative analysis for program flow
//     so the analysis will likely produce a lot
//     of false positives
trait DoubleFree extends IntraCFG with ASTNavigation {

    // determine whether a given AST element a
    // contains a memory allocation call (malloc, calloc, or realloc)
    // TODO: malloc, calloc, realloc may not be from stdlib.h
    //       using string equality here only!!!
    private def containsMemoryAllocationCall(a: AST): Boolean = {
        var res = false
        val memalloc = manytd(query {
            case PostfixExpr(Id("malloc"), _) => res = true
            case PostfixExpr(Id("calloc"), _) => res = true
            case PostfixExpr(Id("realloc"), _) => res = true
        })
        memalloc(a)
        res
    }

    // returns a list of IDs with names of variables that point to
    // dynamically created memory regions (malloc, calloc, realloc)
    def getHeapPointers(a: AST): Set[Id] = {
        var res = Set[Id]()
        val mempointers = manytd(query {
            case InitDeclaratorI(declarator, _, Some(init)) => {
                if (containsMemoryAllocationCall(init)) res += declarator.getId
            }
            case AssignExpr(target@Id(_), "=", source) => {
                if (containsMemoryAllocationCall(source)) res += target
            }
        })

        mempointers(a)
        res
    }
}
