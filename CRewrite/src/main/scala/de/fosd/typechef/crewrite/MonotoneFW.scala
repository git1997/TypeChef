package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel, FeatureExpr}

// this trait provides standard routines of the monotone framework
// (a general framework) for dataflow analyses such as liveness,
// available expression, ...
// in contrast to the original implementation this implementation
// is variability-aware
trait MonotoneFW extends Variables {

    // since C allows variable shadowing we need to track variable usages
    // to their corresponding declarations; may get eventually replaced by
    // CDeclUse from CTypeSystem
    type UsesDeclaresRel = java.util.IdentityHashMap[Id, Option[Conditional[Option[Id]]]]

    protected val incache = new IdentityHashMapCache[Map[Id, FeatureExpr]]()
    protected val outcache = new IdentityHashMapCache[Map[Id, FeatureExpr]]()
    protected var env: ASTEnv = null
    protected var udr: UsesDeclaresRel = null
    protected var fm: FeatureModel = null

    def setEnv(newenv: ASTEnv) {
        env = newenv
    }

    def setUdr(newudr: UsesDeclaresRel) {
        udr = newudr
    }

    def setFm(newfm: FeatureModel) {
        fm = newfm
    }

    // TypeChef does not enforce us to be type-uniform,
    // so a variable use may belong to different variable declarations
    // e.g.:
    // void foo() {
    //   int a = 0; // 3
    //   int b = a;
    //   if (b) {
    //     #if A
    //     int a = b; // 2
    //     #endif
    //     a;  // 1
    //   }
    // }
    // a (// 1) has two different declarations: Choice(A, One(// 3), One(// 2))
    // in presence of A (// 2) shadows declaration (// 3)
    // we compute the relation between variable uses and declarations per function
    def determineUseDeclareRelation(func: FunctionDef): UsesDeclaresRel = {
        // we use a working stack to maintain scoping of nested compound statements
        // each element of the list refers to a block; if we enter a compound statement then we
        // add a Map to the stack; if we leave a compound statement we return the tail of wstack
        // current block is head
        // Map[Id, Conditional[Option[Id]]] maintains all variable declarations in the block that are visible
        type BlockDecls = Map[Id, Conditional[Option[Id]]]
        val res: java.util.IdentityHashMap[Id, Option[Conditional[Option[Id]]]] =
            new java.util.IdentityHashMap[Id, Option[Conditional[Option[Id]]]]()
        var curIdSuffix = 1

        def handleElement(e: Any, curws: List[BlockDecls]): List[BlockDecls] = {
            def handleCFGInstruction(i: AST) = {
                var curblock = curws.head
                val declares = declaresVar(i, env)
                val uses = dataflowUsesVar(i, env)

                // first check uses then update curws using declares (and update defines accordingly)
                for ((k, v) <- uses) {
                    for (id <- v) {
                        val prevblockswithid = curws.flatMap(_.get(id))
                        if (prevblockswithid.isEmpty) res.put(id, None)
                        else res.put(id, Some(ConditionalLib.findSubtree(k, prevblockswithid.head)))
                    }
                }

                for ((k, v) <- declares) {
                    for (id <- v) {
                        // adding the declaration itself
                        res.put(id, Some(One(Some(Id(id.name + curIdSuffix.toString)))))

                        // look for alternative types
                        if (curblock.get(id).isDefined) {
                            curblock = curblock.+((id, ConditionalLib.insert[Option[Id]](curblock.get(id).get,
                                FeatureExprFactory.True, k, Some(Id(id.name + curIdSuffix.toString)))))
                            curIdSuffix += 1
                        } else {
                            // get previous block with declaring id and embed that block in a choice
                            val prevblockswithid = curws.tail.flatMap(_.get(id))
                            if (prevblockswithid.isEmpty) {
                                curblock = curblock.+((id, Choice(k, One(Some(Id(id.name + curIdSuffix.toString))), One(None))))
                                curIdSuffix += 1
                            } else {
                                curblock = curblock.+((id, Choice(k, One(Some(Id(id.name + curIdSuffix.toString))), prevblockswithid.head).simplify))
                                curIdSuffix += 1
                            }
                        }
                    }
                }

                curblock :: curws.tail
            }

            e match {
                // add map to ws when entering a {}; remove when leaving {}
                case CompoundStatement(innerStatements) => handleElement(innerStatements, Map[Id, Conditional[Option[Id]]]() :: curws); curws
                case l: List[_] => {
                    var newws = curws
                    for (s <- l)
                        newws = handleElement(s, newws)
                    newws
                }

                // statements with special treatment of statements with compound statements in it
                case s: IfStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws
                case s: ForStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws
                case s: ElifStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws
                case s: WhileStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws
                case s: DoStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws
                case s: SwitchStatement => s.productIterator.toList.map(x => handleElement(x, Map[Id, Conditional[Option[Id]]]() :: curws)); curws

                case s: Statement => handleCFGInstruction(s)
                case e: Expr => handleCFGInstruction(e)

                case Opt(_, entry) => handleElement(entry, curws)
                case Choice(_, thenBranch, elseBranch) => handleElement(thenBranch, curws); handleElement(elseBranch, curws)
                case One(value) => handleElement(value, curws)
                case Some(x) => handleElement(x, curws)
                case None => curws

                case _: FeatureExpr => curws
                case x => println("not handling: " + x); curws
            }
        }
        handleElement(func.stmt, List())
        res
    }

    // this method internally explodes the use of a variable in case it has multiple declarations
    // e.g.:
    // int a = 0;
    // {
    //   #if A    int a = 1;
    //   a;
    // }
    // the use of a either has "int a = 0;" or "int a = 1;" as declaration
    // udr holds rename versions of both variables and runs the analysis with it (e.g., int a = 0; -> a1
    // and int a = 1; -> a2)
    protected def explodeIdUse(s: Set[Id], sfexp: FeatureExpr, udr: UsesDeclaresRel, res: Map[Id, FeatureExpr], diff: Boolean) = {
        var curres = res
        for (i <- s) {
            val newname = udr.get(i)
            newname match {
                case null => curres = updateMap(curres, sfexp, Set(i), diff)
                case None => curres = updateMap(curres, sfexp, Set(i), diff)
                case Some(c) => {
                    val leaves = ConditionalLib.items(c)
                    for ((nfexp, nid) <- leaves)
                        if (nid.isDefined) curres = updateMap(curres, sfexp and nfexp, Set(nid.get), diff)
                        else curres = updateMap(curres, sfexp, Set(i), diff)
                }
            }
        }
        curres
    }

    protected def updateMap(map: Map[Id, FeatureExpr],
                            fexp: FeatureExpr,
                            difun: Set[Id],
                            diff: Boolean): Map[Id, FeatureExpr] = {
        var curmap = map

        if (diff) {
            for (v <- difun) curmap = curmap.-(v)
        } else {
            for (v <- difun) {
                curmap.get(v) match {
                    case None => curmap = curmap.+((v, fexp))
                    case Some(x) => curmap = curmap.+((v, fexp or x))
                }
            }
        }

        curmap
    }

}
