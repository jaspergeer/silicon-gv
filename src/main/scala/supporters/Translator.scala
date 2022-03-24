package viper.silicon.supporters

import viper.silver.ast
import viper.silicon.decider.RecordedPathConditions
import viper.silicon.state.{terms, State, Store, BasicChunk, Identifier}
import viper.silicon.resources.{FieldID, PredicateID}

// should we use the path conditions from the state?
final class Translator(s: State, pcs: RecordedPathConditions) {
  private var translatingVars: Seq[terms.Term] = Seq()
  // this is, to some extent, a stub method currently
  def translate(t: terms.Term): Option[ast.Exp] = {
    t match {
      case terms.Null()        => Some(ast.NullLit()())
      case terms.False()       => Some(ast.FalseLit()())
      case terms.True()        => Some(ast.TrueLit()())
      case terms.IntLiteral(i) => Some(ast.IntLit(i)())
      case terms.Plus(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Add(e1, e2)())
          case _                    => None
        }
      case terms.Minus(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Sub(e1, e2)())
          case _                    => None
        }
      case terms.Div(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Div(e1, e2)())
          case _                    => None
        }
      case terms.Times(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Mul(e1, e2)())
          case _                    => None
        }
      case terms.Mod(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Mod(e1, e2)())
          case _                    => None
        }
      case terms.Not(t) =>
        (translate(t)) match {
          case Some(e) => Some(ast.Not(e)())
          case _       => None
        }
      // exhaustiveness warnings are suppressed by the following four cases; do
      // these employ match guards via the seq matching?
      case terms.Or(ts) =>
        ts match {
          case t +: Seq() =>
            translate(t) match {
              case None    => None
              case Some(e) => Some(ast.Or(e, ast.FalseLit()())())
            }
          case t +: vs =>
            (translate(t), translate(terms.Or(vs))) match {
              case (Some(e1), Some(e2)) => Some(ast.Or(e1, e2)())
              case _                    => None
            }
        }
      case terms.And(ts) =>
        ts match {
          case t +: Seq() =>
            translate(t) match {
              case None    => None
              case Some(e) => Some(ast.And(e, ast.TrueLit()())())
            }
          case t +: vs =>
            (translate(t), translate(terms.And(vs))) match {
              case (Some(e1), Some(e2)) => Some(ast.And(e1, e2)())
              case _                    => None
            }
        }
      case terms.Implies(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.Implies(e1, e2)())
          case _                    => None
        }
      case terms.Iff(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) =>
            Some(ast.And(ast.Implies(e1, e2)(), ast.Implies(e2, e1)())())
          case _ => None
        }
      case terms.Equals(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.EqCmp(e1, e2)())
          case _                    => None
        }
      case terms.Less(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.LtCmp(e1, e2)())
          case _                    => None
        }
      case terms.AtMost(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.LeCmp(e1, e2)())
          case _                    => None
        }
      case terms.AtLeast(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.GeCmp(e1, e2)())
          case _                    => None
        }
      case terms.Greater(t0, t1) =>
        (translate(t0), translate(t1)) match {
          case (Some(e1), Some(e2)) => Some(ast.GtCmp(e1, e2)())
          case _                    => None
        }
      case terms.Ite(t0, t1, t2) =>
        (translate(t0), translate(t1), translate(t2)) match {
          case (Some(e1), Some(e2), Some(e3)) => Some(ast.CondExp(e1, e2, e3)())
          case _                              => None
        }
      case terms.Var(name, sort) =>
        sort match {
          case terms.sorts.Snap => {
            println(
              "WARNING: We encountered a snapshot variable, but this should not"
                + "happen! Returning None"
            )
            None
          }
          case _ => variableResolver(terms.Var(name, sort))
        }
      case terms.SortWrapper(t, sort) =>
        variableResolver(terms.SortWrapper(t, sort))
      // how do we deal with snapshots? we need not {
      //
      // snapshots only exist in the path condition because the latter is
      // already passed around everywhere; translated snapshot terms should
      // never be returned from diff as part of a necessary runtime check, so
      // we need not translate them
      //
      // these cases only exist to prevent the translator from crashing during
      // testing; the translator is tested on the path condition, which
      // includes snapshot terms
      // case terms.Unit          => None
      // case terms.First(_)      => None
      // case terms.Second(_)     => None
      // case terms.Combine(_, _) => None
      // }
      case _ => sys.error(s"Unable to translate ${t}")
    }
  }

  private def resolveType(variable: terms.Term): ast.Type = {
    variable match {
      case terms.Var(_, terms.sorts.Int) |
          terms.SortWrapper(_, terms.sorts.Int) =>
        ast.Int
      case terms.Var(_, terms.sorts.Bool) |
          terms.SortWrapper(_, terms.sorts.Bool) =>
        ast.Bool
      case terms.Var(_, terms.sorts.Ref) |
          terms.SortWrapper(_, terms.sorts.Ref) =>
        ast.Ref
      case terms.Var(_, terms.sorts.Perm) |
          terms.SortWrapper(_, terms.sorts.Perm) =>
        ast.Perm
      case _ => sys.error(s"Unable to match type for ${variable}!")
    }
  }

  // TODO: the invocation of getAccessibilityPredicates seems a bit wrong
  // TODO: make brancher translate its input (at the branch site)
  private def variableResolver(variable: terms.Term): Option[ast.Exp] = {
    if (translatingVars.contains(variable)) None

    // Retrieve aliasing information; add our
    // input variable to it
    val heapAliases: Seq[(terms.Term, String)] =
      (s.h + s.optimisticHeap).getChunksForValue(variable)
    val pcsEquivalentVariables: Seq[terms.Term] =
      variable +: pcs.getEquivalentVariables(variable)

    pcsEquivalentVariables.foldRight[Option[ast.Exp]](None)(
      (term, potentialResolvedVariable) =>
        potentialResolvedVariable match {
          case Some(_) => potentialResolvedVariable
          case None    => {
            if (translatingVars.contains(term)) None
            translatingVars = translatingVars :+ term
            // Attempt normal variable resolution (looking in
            // both heaps, followed by the store)
            regularVariableResolver(term) match {
              case Some(resolvedVariable) => Some(resolvedVariable)
              // Attempt regex variable resolution (we only look
              // in the store; the rest is constructed via a regex)
              //
              // Use caution here!
              case None => {
                translatingVars = translatingVars.filter(v => v != term)
                regexVariableResolver(term)
              }
            }
          }
        }
    ) match {
      case None => {
        translatingVars = translatingVars :+ variable
        heapAliases.foldRight[Option[ast.Exp]](None)(
          (alias, potentialResolvedVariable) =>
            potentialResolvedVariable match {
              case Some(_) => potentialResolvedVariable
              case None    => {
                if (translatingVars.contains(alias._1)) None
                translatingVars = translatingVars :+ alias._1
                // Attempt normal variable resolution (looking in
                // both heaps, followed by the store)
                regularVariableResolver(alias._1) match {
                  case Some(resolvedVariable) => Some(ast.FieldAccess(resolvedVariable, ast.Field(alias._2, resolveType(alias._1))())())
                  // Attempt regex variable resolution (we only look
                  // in the store; the rest is constructed via a regex)
                  case None => {
                    translatingVars = translatingVars.filter(v => v != alias._1)
                    regexVariableResolver(alias._1) match {
                      case Some(resolvedVariable) => Some(ast.FieldAccess(resolvedVariable, ast.Field(alias._2, resolveType(alias._1))())())
                      case None => None
                    }
                  }
                }
              }
            }
        ) match {
          case Some(e) => translatingVars = Seq(); Some(e)
          case None => {
            translatingVars = translatingVars.filter(v => v != variable)
            None
          }
        }
      }
      case Some(e) => translatingVars = Seq(); Some(e)
    }
  }

  private def regularVariableResolver(variable: terms.Term): Option[ast.Exp] = {

    // The old store is for resolving variables from the context of a pre- or
    // post condition
    val store: Store = s.oldStore match {
      case None           => s.g
      case Some(oldStore) => oldStore
    }

    val varType = resolveType(variable)

    // TODO: Make this handle predicates
    // Although, do we ever need to resolve predicates? And do they even appear
    // in the heap? They do, but I'm pretty sure we only emit runtime checks
    // for predicates when we attempt to consume a predicate instance
    //
    // I guess we may attempt to translate predicate instance arguments...?

    // TODO: The case where both the regular heap and optimistic heap have the
    // variable should never happen, maybe
    //
    // Ask Jenna about this?

    store.getKeyForValue(variable) match {
      case None =>
        // Search both heaps for the variable
        s.h.getChunkForValue(variable) match {
          case None =>
            s.optimisticHeap.getChunkForValue(variable) match {
              case None => None
              case Some((symVar, id)) =>
                variableResolver(symVar) match {
                  case None => None
                  case Some(astVar) =>
                    Some(ast.FieldAccess(astVar, ast.Field(id, varType)())())
                }
            }
          case Some((symVar, id)) =>
            variableResolver(symVar) match {
              case None => None
              case Some(astVar) =>
                Some(ast.FieldAccess(astVar, ast.Field(id, varType)())())
            }
        }
      case Some(v) => Some(v)
    }
  }

  private def regexVariableResolver(variable: terms.Term): Option[ast.Exp] =
    variable match {

      // This is the last resort for translating a variable. It's not as robust
      // as the previous methods, and should be inspected carefully
      //
      // Unfortunately, it's also the only way we can translate nested fields
      //
      // This may be the source of bugs!

      case term_variable @ terms.Var(
            identifier: Identifier,
            termType
          ) => {

        // We must check that we have the top level receiver in the symbolic store
        //
        // The "top level receiver" is the Ref variable without any fields
        //
        // symbolic values may point to separate variable names (with different names
        // than the value, since the thing the symbolic value points to may change)

        val store: Store = s.oldStore match {
          case None           => s.g
          case Some(oldStore) => oldStore
        }

        val varType = resolveType(variable)

        // This regex removes the non-concrete-variable parts of the
        // identifier
        // TODO: Can't this be simplified to '@[0-9]+'?
        val fieldCleanupPattern = """@[0-9][0-9]*""".r

        // This extracts the identifier into an array of concrete
        // names
        val identifierArray: Array[String] =
          fieldCleanupPattern
            .replaceAllIn(identifier.name, "")
            .split('.')

        // This will be the name of the field for the receiver
        val fieldName: String =
          identifierArray
            .slice(1, identifierArray.length)
            .mkString(".")

        // This regex extracts the part of the identifier that
        // should be in the symbolic store
        //
        // For 'p@01@08.field@01@09.anotherField@01@10', it should
        // yield 'p@01@08'
        //
        // TODO: Can't this be simplified to '\..*@[0-9].*'?
        val receiverCleanupPattern = """\..*@[0-9][0-9]*.*""".r

        // This will be the receiver
        // Some complications come up because we were unable to find
        // it from the heap (we're constructing this ourselves)
        // Mostly, we'll need to specially search the store
        val receiver: terms.Var =
          terms.Var(
            Identifier(
              receiverCleanupPattern.replaceAllIn(identifier.name, "")
            ),
            if (identifierArray.length != 0) {
              terms.sorts.Ref
            } else {
              termType
            }
          )

        // We need to enable "lenient mode" for the store search
        // here, because we've constructed our own identifier
        // (identifiers appear to be compared non-structurally)
        val astVar = store.getKeyForValue(receiver, true) match {
          case None                   => return None
          case Some(concreteVariable) => concreteVariable
        }

        // Return the translated variable
        Some(ast.FieldAccess(astVar, ast.Field(fieldName, varType)())())

      }

      case _ => None
    }

  def getAccessibilityPredicates: Seq[ast.Exp] = {

    (s.h.values ++ s.optimisticHeap.values)
      .map(chunk =>
        chunk match {
          case BasicChunk(resourceId, id, args, snap, perm) =>
            resourceId match {
              // TODO: Can we use translate for this case (or at least variableResolver?)
              //
              // TODO: Does this need to access the old store?
              //
              // TODO: Does this need to look in the path condition for equivalent
              //       variables? probably not, since it doesn't look in the heap (that is,
              //       it gets its value directly from the heap...); the case where
              //       we need to look in the path condition may only be for
              //       predicates
              case FieldID => {

                val varType = resolveType(args.head)

                val potentialAstVar = variableResolver(args.head) match {
                  case None => {
                    println(s"Warning: unable to translate ${args.head}")
                    None
                  }
                  case Some(astVar) => Some(astVar)
                }

                potentialAstVar match {
                  case None => None
                  case Some(astVar) =>
                    Some(
                      ast.FieldAccess(
                        astVar,
                        ast.Field(id.toString, varType)()
                      )()
                    )
                }
              }

              case PredicateID => {

                val predicateArgs: Option[Seq[ast.Exp]] =
                  args.foldRight[Option[Seq[ast.Exp]]](Some(Seq[ast.Exp]()))(
                    (term, rest) => {
                      (translate(term), rest) match {
                        case (Some(exp), Some(exprs)) => Some(exp +: exprs)
                        case (None, _)                => None
                        case (_, None)                => None
                      }
                    }
                  )

                predicateArgs match {
                  case None => {
                    println(s"Warning: unable to translate predicate ${chunk}")
                    None
                  }
                  case Some(exprs) =>
                    Some(ast.PredicateAccess(exprs, id.toString)())
                }
              }
            }
          // Remove the parts of the heap we were unable to translate
        }
      )
      .foldRight(Seq[ast.Exp]())(
        (potentialPredicate: Option[ast.Exp], rest: Seq[ast.Exp]) =>
          potentialPredicate match {
            case None            => rest
            case Some(predicate) => predicate +: rest
          }
      )
  }
}
