package de.fosd.typechef.crewrite

import de.fosd.typechef.featureexpr._
import java.util
import de.fosd.typechef.conditional._
import de.fosd.typechef.parser.c._
import org.kiama.attribution.AttributionBase
import de.fosd.typechef.conditional.Opt

// defines and uses we can jump to using succ
// beware of List[Opt[_]]!! all list elements can possibly have a different annotation
trait Variables {
    // add annotation to elements of a Set[Id]
    // used for uses, defines, and declares
    private def addAnnotation2ResultSet(in: Set[Id], env: ASTEnv): Map[FeatureExpr, Set[Id]] = {
        var res = Map[FeatureExpr, Set[Id]]()

        for (r <- in) {
            val rfexp = env.featureExpr(r)

            val key = res.find(_._1 equivalentTo rfexp)
            key match {
                case None => res = res.+((rfexp, Set(r)))
                case Some((k, v)) => res = res.+((k, v ++ Set(r)))
            }
        }

        res
    }

    // returns all used variables with their annotation
    val usesVar: PartialFunction[(Any, ASTEnv), Map[FeatureExpr, Set[Id]]] = {
        case (a, env) => addAnnotation2ResultSet(uses(a, dataflowUses = false), env)
    }

    // returns all used variables (apart from declarations) with their annotation
    val dataflowUsesVar: PartialFunction[(Any, ASTEnv), Map[FeatureExpr, Set[Id]]] = {
        case (a, env) => addAnnotation2ResultSet(uses(a, dataflowUses = true), env)
    }

    // returns all used Ids independent of their annotation
    def uses(a: Any, dataflowUses: Boolean): Set[Id] = {
        a match {
            case ForStatement(expr1, expr2, expr3, _) => uses(expr1, dataflowUses) ++ uses(expr2, dataflowUses) ++ uses(expr3, dataflowUses)
            case ReturnStatement(Some(x)) => uses(x, dataflowUses)
            case WhileStatement(expr, _) => uses(expr, dataflowUses)
            case DeclarationStatement(d) => uses(d, dataflowUses)
            case Declaration(_, init) => init.flatMap(uses(_, dataflowUses)).toSet
            case InitDeclaratorI(_, _, Some(i)) => uses(i, dataflowUses)
            case AtomicNamedDeclarator(_, id, _) => Set(id)
            case NestedNamedDeclarator(_, nestedDecl, _) => uses(nestedDecl, dataflowUses)
            case Initializer(_, expr) => uses(expr, dataflowUses)
            case i@Id(name) => Set(i)
            case FunctionCall(params) => params.exprs.map(_.entry).flatMap(uses(_, dataflowUses)).toSet
            case ArrayAccess(expr) => uses(expr, dataflowUses)
            case PostfixExpr(Id(_), f@FunctionCall(_)) => uses(f, dataflowUses)
            case PostfixExpr(p, s) => uses(p, dataflowUses) ++ uses(s, dataflowUses)
            case UnaryExpr(_, ex) => uses(ex, dataflowUses)
            case SizeOfExprU(expr) => uses(expr, dataflowUses)
            case CastExpr(_, expr) => uses(expr, dataflowUses)
            case PointerDerefExpr(castExpr) => uses(castExpr, dataflowUses)
            case PointerCreationExpr(castExpr) => uses(castExpr, dataflowUses)
            case UnaryOpExpr(kind, castExpr) => uses(castExpr, dataflowUses)
            case NAryExpr(ex, others) => uses(ex, dataflowUses) ++ others.flatMap(uses(_, dataflowUses)).toSet
            case NArySubExpr(_, ex) => uses(ex, dataflowUses)
            case ConditionalExpr(condition, _, _) => uses(condition, dataflowUses)
            case ExprStatement(expr) => uses(expr, dataflowUses)
            case AssignExpr(target, op, source) => uses(source, dataflowUses) ++ ({
                op match {
                    case "=" if (!dataflowUses) => Set()
                    case _ => uses(target, dataflowUses)
                }
            })
            case Opt(_, entry) => uses(entry, dataflowUses)
            case _ => Set()
        }
    }

    // returns all defined Ids independent of their annotation
    val defines: PartialFunction[Any, Set[Id]] = {
        case i@Id(_) => Set(i)
        case AssignExpr(target, _, source) => defines(target)
        case DeclarationStatement(d) => defines(d)
        case Declaration(_, init) => init.flatMap(defines).toSet
        case InitDeclaratorI(a, _, _) => defines(a)
        case AtomicNamedDeclarator(_, i, _) => Set(i)
        case ExprStatement(_: Id) => Set()
        case ExprStatement(expr) => defines(expr)
        case PostfixExpr(i@Id(_), SimplePostfixSuffix(_)) => Set(i) // a++; or a--;
        case UnaryExpr(_, i@Id(_)) => Set(i) // ++a; or --a;
        case Opt(_, entry) => defines(entry)
        case _ => Set()
    }

    // returns all declared Ids independent of their annotation
    val declares: PartialFunction[Any, Set[Id]] = {
        case DeclarationStatement(decl) => declares(decl)
        case Declaration(_, init) => init.flatMap(declares).toSet
        case InitDeclaratorI(declarator, _, _) => declares(declarator)
        case AtomicNamedDeclarator(_, id, _) => Set(id)
        case Opt(_, entry) => declares(entry)
        case _ => Set()
    }

    // returns all defined variables with their annotation
    val definesVar: PartialFunction[(Any, ASTEnv), Map[FeatureExpr, Set[Id]]] = {
        case (a, env) => addAnnotation2ResultSet(defines(a), env)
    }

    // returns all declared variables with their annotation
    val declaresVar: PartialFunction[(Any, ASTEnv), Map[FeatureExpr, Set[Id]]] = {
        case (a, env) => addAnnotation2ResultSet(declares(a), env)
    }
}

class IdentityHashMapCache[A] {
    private val cache: java.util.IdentityHashMap[Any, A] = new util.IdentityHashMap[Any, A]()
    def update(k: Any, v: A) { cache.put(k, v) }
    def lookup(k: Any): Option[A] = {
        val v = cache.get(k)
        if (v != null) Some(v)
        else None
    }
}

trait Liveness extends AttributionBase with Variables with IntraCFG with MonotoneFW {

    // cf. http://www.cs.colostate.edu/~mstrout/CS553/slides/lecture03.pdf
    // page 5
    //  in(n) = uses(n) + (out(n) - defines(n))
    // out(n) = for s in succ(n) r = r + in(s); r
    // insimple and outsimple are the non variability-aware in and out versiosn
    // of liveness determination
    val insimple: AST => Set[Id] = {
        circular[AST, Set[Id]](Set[Id]()) {
            case _: FunctionDef => Set()
            case e => {
                val u = uses(e, dataflowUses = false)
                val d = defines(e)
                var res = outsimple(e)

                res = u.union(res.diff(d))
                res
            }
        }
    }

    val outsimple: AST => Set[Id] = {
        circular[AST, Set[Id]](Set[Id]()) {
            case e => {
                val ss = succ(e, fm, env).filterNot(x => x.entry.isInstanceOf[FunctionDef])
                var res: Set[Id] = Set()
                for (s <- ss.map(_.entry)) res = res.union(insimple(s))
                res
            }
        }
    }

     // in and out variability-aware versions
    val inrec: AST => Map[Id, FeatureExpr] = {
        circular[AST, Map[Id, FeatureExpr]](Map[Id, FeatureExpr]()) {
            case FunctionDef(_, _, _, _) => Map()
            case t => {
                val uses = usesVar(t, env)
                val defines = definesVar(t, env)

                var res = out(t)
                for ((k, v) <- defines) res = explodeIdUse(v, k, udr, res, op = true)
                for ((k, v) <- uses) res = explodeIdUse(v, k, udr, res, op = false)
                res
            }
        }
    }

    val outrec: AST => Map[Id, FeatureExpr] =
        circular[AST, Map[Id, FeatureExpr]](Map[Id, FeatureExpr]()) {
            case e => {
                val ss = succ(e, fm, env).filterNot(x => x.entry.isInstanceOf[FunctionDef])
                var res = Map[Id, FeatureExpr]()
                for (s <- ss) {
                    for ((r, f) <- in(s.entry))
                        res = join(res, f and s.feature, Set(r))
                }
                res
            }
        }

    def out(a: AST) = {
        outcache.lookup(a) match {
            case Some(v) => v
            case None => {
                val r = outrec(a)
                outcache.update(a, r)
                r
            }
        }
    }

    def in(a: AST) = {
        incache.lookup(a) match {
            case Some(v) => v
            case None => {
                val r = inrec(a)
                incache.update(a, r)
                r
            }
        }
    }
}
