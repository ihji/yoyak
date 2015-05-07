package com.simplytyped.yoyak.il.opt

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.MapDom
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.CommonILHelper
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}
import com.simplytyped.yoyak.il.opt.analysis.DefReachability
import com.simplytyped.yoyak.il.opt.analysis.DefReachability.SetCoreStmt
import scala.collection.mutable.{Set => MSet}

class VarSplitting {
  var nameCounter = 0
  private def getNewLocal() = {nameCounter += 1; Local("$v"+nameCounter)}

  var renameMap = Map.empty[CoreStmt,Local]
  private def generateRenameMap(defMap: MapDom[BasicBlock,GaloisIdentity[MapDom[Local,SetCoreStmt]]]) {
    val defGroup = defMap.foldLeft(Map.empty[CoreStmt,MSet[CoreStmt]]) {
      case (defGroupMap,(_,m)) => m.foldLeft(defGroupMap) {
        case (defGroupMap,(_,stmtSet)) =>
          val existingStmt = stmtSet.find{s => defGroupMap.get(s).nonEmpty}
          val existingSet = if(existingStmt.nonEmpty) {
            defGroupMap(existingStmt.get) ++= stmtSet
          } else {
            MSet.empty[CoreStmt] ++ stmtSet
          }
          stmtSet.foldLeft(defGroupMap) {(x,y) => x + (y -> existingSet)}
      }
    }.values.toList
    val renameMap = defGroup.flatMap{set =>
      val newLocal = getNewLocal()
      set.map{s => (s,newLocal)}
    }.toMap
    this.renameMap = renameMap
  }
  private def findNewLocal(defMap: MapDom[Local,SetCoreStmt])(old: Local) : Local = {
    val stmts = defMap.get(old)
    if(stmts.isEmpty) old
    else {
      val stmt = stmts.head
      val newLocalOpt = renameMap.get(stmt)
      if(newLocalOpt.nonEmpty) newLocalOpt.get.setType(Type.lub(old.ty,newLocalOpt.get.ty))
      else {
        assert(stmts.size == 1)
        val newLocal = getNewLocal().setType(old.ty)
        renameMap += (stmt -> newLocal)
        newLocal
      }
    }
  }
  private def renameLocal(defMap: MapDom[Local,SetCoreStmt])(stmt: CoreStmt) : (MapDom[Local,SetCoreStmt],CoreStmt) = {
    import DefReachability.absTransfer
    val newStmt1 = CommonILHelper.substituteLocalUse(findNewLocal(defMap))(stmt)
    val nextMap = absTransfer.transfer(defMap,stmt)
    val newStmt2 = CommonILHelper.substituteLocalDef(findNewLocal(nextMap))(newStmt1)
    (nextMap,newStmt2)
  }
  def run(cfg: CFG) : CFG = {
    val defreachAnalysis = new DefReachability
    val defreach = defreachAnalysis.run(cfg)
    generateRenameMap(defreach)
    cfg.nodes.foreach { bb =>
      val prevs = cfg.getPrevs(bb)
      val defMap = prevs.foldLeft(DefReachability.ops.bottom){(x,y) => DefReachability.ops.\/(x,defreach.get(y))}
      val stmts = bb.data.getStmts
      val newStmts = stmts.foldLeft(defMap,List.empty[CoreStmt]){
        case ((map,list),s) =>
          val (newMap,newS) = renameLocal(map)(s)
          (newMap,newS::list)
      }._2.reverse
      bb.data.setStmts(newStmts)
    }
    cfg
  }
}
