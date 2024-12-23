package viper.silicon.rules

import scala.collection.mutable
import viper.silver.ast
import viper.silver.verifier.PartialVerificationError
import viper.silicon.interfaces.VerificationResult
import viper.silicon.state.State
import viper.silicon.state.terms._
import viper.silicon.verifier.Verifier
// import viper.silicon.{WellformednessRecord, SymbExLogger}



trait WellFormednessRules extends SymbolicExecutionRules {
  def wellformed(s: State,
                 sf: (Sort, Verifier) => Term,
                 e: Seq[ast.Exp],
                 pve: PartialVerificationError,
                 v: Verifier)
                (Q: (State, Verifier) => VerificationResult)
                : VerificationResult
  
  def isEquiImp(as: Seq[ast.Exp])
               : Boolean

  def isIsoImp(as: Seq[ast.Exp])
              : Boolean
}

object wellFormedness extends WellFormednessRules with Immutable {
  import producer._
  private var visitedPreds: Seq[String] = Seq() // assumes pred names unique; silver + C0's typecheckers/parsers ensure this

  def wellformed(s: State,
                 sf: (Sort, Verifier) => Term,
                 e: Seq[ast.Exp],
                 pve: PartialVerificationError,
                 v: Verifier)
                (Q: (State, Verifier) => VerificationResult)
                : VerificationResult = {

    // TODO Wellformedness
    // val sepIdentifier = SymbExLogger.currentLog().insert(new WellformednessRecord(viper.silicon.utils.ast.BigAnd(e), s, v.decider.pcs))
    produce(s, sf, viper.silicon.utils.ast.BigAnd(e), pve, v)((s1, v1) =>
      produce(s, sf, viper.silicon.utils.ast.BigAnd(e), pve, v1)((s2, v2) => {
        // SymbExLogger.currentLog().collapse( viper.silicon.utils.ast.BigAnd(e), sepIdentifier) //TODO: fix type mismatch
        Q(s2, v2)}))
  }

  /** @inheritdoc */
  def isEquiImp(as: Seq[ast.Exp])
               : Boolean = {

    val allTlcs = mutable.ListBuffer[ast.Exp]()
    
    as.foreach(a => {
      val tlcs = a.topLevelConjuncts
      allTlcs ++= tlcs
    })

    isEquiImpTlcs(allTlcs.result())
  
  }

  private def isEquiImpTlcs(tlcs: Seq[ast.Exp])
                          : Boolean = {
    if (tlcs.isEmpty)
      false 
    else {
      tlcs.foldLeft(false) { (b, a) => 
        b || isEquiImpTlc(a)
      }
    }
  }

  private def isEquiImpR(a: ast.Exp)
                        : Boolean = {
    val tlcs = a.topLevelConjuncts
    
    isEquiImpTlcs(tlcs)
  }

  private def isEquiImpTlc(a: ast.Exp) 
                         : Boolean = {
    a match {
      case impr @ ast.ImpreciseExp(e) => true
      
      case ite @ ast.CondExp(e0, a1, a2) => isEquiImpR(a1) || isEquiImpR(a2)

      case ast.PredicateAccessPredicate(ast.PredicateAccess(eArgs, predicateName), perm) =>
        if (visitedPreds.contains(predicateName))
          false
        else {
          visitedPreds = visitedPreds :+ predicateName
          val predicate = Verifier.program.findPredicate(predicateName)
          var res = false
          res = isEquiImpR(predicate.body.get)     
          visitedPreds = visitedPreds.filter(p => p != predicateName)
          res
        }
      case _ => false
    }
  }

  /** @inheritdoc */
  def isIsoImp(as: Seq[ast.Exp])
              : Boolean = {
    
    val allTlcs = mutable.ListBuffer[ast.Exp]()
    as.foreach(a => {
      val tlcs = a.topLevelConjuncts
      allTlcs ++= tlcs
    })

    isIsoImpTlcs(allTlcs.result())
  }

  private def isIsoImpTlcs(tlcs: Seq[ast.Exp])
                          : Boolean = {
    if (tlcs.isEmpty)
      false 
    else {
      tlcs.foldLeft(false) { (b, a) => 
        b || isIsoImpTlc(a)
      }
    }
  }

  // This function assumes conditionals will never contain ? in their paths
  // This is a fair assumption because ? && (x > 2) ? ... : ... can give the same
  // affect as if ? was inside the conditional
  private def isIsoImpTlc(a: ast.Exp) 
                         : Boolean = {
    a match {
      case impr @ ast.ImpreciseExp(e) => true
      case _ => false
    }
  }
}
