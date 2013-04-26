package de.fosd.typechef.crewrite

import org.kiama.rewriting.Rewriter._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.UseDeclMap
import de.fosd.typechef.featureexpr.FeatureModel

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
class DoubleFree(env: ASTEnv, udm: UseDeclMap, fm: FeatureModel) extends MonotoneFW[Id](env, udm, fm) with IntraCFG with CFGHelper with ASTNavigation {

    // determine whether a given AST element a
    // contains a memory allocation call (malloc, calloc, or realloc)
    // we ensure that malloc, calloc, realloc are from /usr/include/stdlib.h (see comment)
    private def containsMemoryAllocationCall(a: AST): Boolean = {
        var res = false
        val memalloc = manytd(query {
            case PostfixExpr(i@Id(s), _) => {
                if ((s.equals("malloc") || s.equals("calloc") || s.equals("realloc"))
                    // && i.hasPosition
                    // && i.getPositionFrom.getFile.contains("/usr/include/stdlib.h")
                ) res = true
            }
        })
        memalloc(a)
        res
    }

    def id2SetT(i: Id) = Set(i)

    def kill(a: AST, env: ASTEnv) = Map()

    // returns a list of Ids with names of variables that point to
    // dynamically created memory regions (malloc, calloc, realloc)
    def gen(a: AST, env: ASTEnv): Set[Id] = {
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

    // returns a list of Ids with names of variables that a freed
    // by call to free
    // we ensure (see comment) that call to free belongs to system free function
    // (see /usr/include/stdlib.h)
    def getFreedPointers(a: AST): Set[Id] = {
        var res = Set[Id]()
        val freedpointers = manytd(query {
            case PostfixExpr(i@Id("free"), FunctionCall(l)) => {
                // if (i.hasPosition && i.getPositionFrom.getFile.contains("/usr/include/stdlib.h"))
                for (e <- l.exprs)
                    for (ni <- filterAllASTElems[Id](e))
                        res += ni
            }
        })

        freedpointers(a)
        res
    }

    // returns true if f is free of double-free errors; returns false otherwise
    def check(f: CompoundStatement, fm: FeatureModel): List[AnalysisError] = {
        var res = List[AnalysisError]()

        // maintain a map of dynamically created memory locations and their free calls.
        var mres = Map[Id, List[Id]]()

        // basic idea is to get all successor elements of f
        // iterate over the list, fetch pointers of malloc, ... calls
        // and check subsequent control flow statements for double free
        val env = CASTEnv.createASTEnv(f)
        val wlist = getAllSucc(f, fm, env).map(_._1)
        for (s <- wlist) {
            val hp = gen(s, env)
            for (hpelem <- hp)
                mres = mres.+((hpelem, List()))

            val fp = getFreedPointers(s)
            for (fpelem <- fp) {
                if (mres.isDefinedAt(fpelem)) {
                    val clist = fpelem::mres.get(fpelem).get
                    mres = mres.+((fpelem, clist))
                    if (clist.size > 1)
                        res ::= new AnalysisError(env.featureExpr(fpelem), "Potential double free error!", clist.head)
                }
            }
        }

        res
    }
}
