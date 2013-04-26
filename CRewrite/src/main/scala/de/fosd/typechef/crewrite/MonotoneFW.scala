package de.fosd.typechef.crewrite

import org.kiama.attribution.AttributionBase

import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.UseDeclMap
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExpr}

// this trait provides standard routines of the monotone framework
// (a general framework) for dataflow analyses such as liveness,
// available expression, ...
// in contrast to the original idea this implementation
// is variability-aware; for more information about monotone frameworks
// see "Principles of Program Analysis" by (Nielson, Nielson, Hankin)
abstract class MonotoneFW[T](val env: ASTEnv, val udm: UseDeclMap, val fm: FeatureModel) extends AttributionBase with IntraCFG {

    // since C allows variable shadowing we need to track variable usages
    // to their corresponding declarations

    protected def id2SetT(i: Id): Set[T]

    protected val entry_cache = new IdentityHashMapCache[Map[T, FeatureExpr]]()
    protected val exit_cache = new IdentityHashMapCache[Map[T, FeatureExpr]]()

    def gen(a: AST, env: ASTEnv): Map[FeatureExpr, Set[T]]
    def kill(a: AST, env: ASTEnv): Map[FeatureExpr, Set[T]]

    // while monotone framework usually works on Sets
    // we use maps here for efficiency reasons:
    //   1. the obvious shift from non-variability-aware monotone framework to
    //      a variability-aware version is to change the type of the result set
    //      from Set[Id] to Set[Opt[Id]]. However this changes involves many lookups
    //      and changes to the set.
    //   2. We use Map[T, FeatureExpr] since T is our basic element of interest.
    //      FeatureExpr do not matter so far (they are prominent when using Opt
    //      nodes with List or Set). Since T matters operations on feature expression
    //      are easy and can be delayed to the point at which we *really* need
    //      the result. The delay also involves simplifications of feature
    //      expressions such as "a or (not a) => true".
    type ResultMap = Map[T, FeatureExpr]

    private def diff(map: ResultMap, d: Set[T]) = {
        var curmap = map
        for (e <- d)
            curmap = curmap.-(e)
        curmap
    }

    private def join(map: ResultMap, fexp: FeatureExpr, j: Set[T]) = {
        var curmap = map
        for (e <- j) {
            curmap.get(e) match {
                case None    => curmap = curmap.+((e, fexp))
                case Some(x) => curmap = curmap.+((e, fexp or x))
            }
        }
        curmap
    }

    private val analysis_entry: AST => Map[T, FeatureExpr] = {
        circular[AST, Map[T, FeatureExpr]](Map[T, FeatureExpr]()) {
            case FunctionDef(_, _, _, _) => Map()
            case t => {
                val g = gen(t, env)
                val k = kill(t, env)

                var res = exit(t)
                for ((_, v) <- k)
                    for (d <- v)
                        if (udm.containsKey(d))
                            for (nd <- udm.get(d))
                                res = diff(res, id2SetT(nd))
                for ((fexp, v) <- g)
                    for (u <- v)
                        if (udm.containsKey(u))
                            for (ud <- udm.get(u))
                                res = join(res, fexp, id2SetT(ud))


                res
            }
        }
    }

    private val analysis_exit: AST => Map[T, FeatureExpr] =
        circular[AST, Map[T, FeatureExpr]](Map[T, FeatureExpr]()) {
            case e => {
                val ss = succ(e, fm, env).filterNot(x => x.entry.isInstanceOf[FunctionDef])
                var res = Map[T, FeatureExpr]()
                for (s <- ss) {
                    for ((r, f) <- entry(s.entry))
                        res = join(res, f and s.feature, Set(r))
                }
                res
            }
        }

    def exit(a: AST) = {
        exit_cache.lookup(a) match {
            case Some(v) => v
            case None => {
                val r = analysis_exit(a)
                exit_cache.update(a, r)
                r
            }
        }
    }

    def out(a: AST) = exit(a)

    def entry(a: AST) = {
        entry_cache.lookup(a) match {
            case Some(v) => v
            case None => {
                val r = analysis_entry(a)
                entry_cache.update(a, r)
                r
            }
        }
    }

    def in(a: AST) = entry(a)
}
