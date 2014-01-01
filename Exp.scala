abstract class Exp {

  def isFinal : Boolean


  val label = { Exp.maxLabel += 1 ; Exp.maxLabel }

  override def equals (that : Any) =
    this.label == that.asInstanceOf[Exp].label

  override def hashCode = label
}


object Exp {

  private var maxLabel = 0 

  def from(sexp : SExp) : Exp = {
    sexp match {

      // References:
      case SSymbol(id) => RefExp(id)

      // Lambda terms:
      case SList(SSymbol("lambda"),params,body) => {
        val vars = params.toList map { case SSymbol(id) => id }
        LambdaExp(vars,from(body))
      }
       

      // Conditionals:
      case STrue() => BoolExp(true)

      case SFalse() => BoolExp(false)

      case SList(SSymbol("if"),cond,ifTrue,ifFalse) =>
       IfExp(from(cond), from(ifTrue), from(ifFalse))

      case SList(SSymbol("and"),a,b) =>
       AndExp(from(a),from(b))

      case SList(SSymbol("or"),a,b) =>
       OrExp(from(a),from(b))


      // Numerics:
      case SInt(value) => IntExp(value) 

      case SList(SSymbol("zero?"), arg) =>
       ZeroPExp(from(arg))

      case SList(SSymbol("-"),a,b) =>
       SubExp(from(a),from(b))

      case SList(SSymbol("+"),a,b) =>
       PlusExp(from(a), from(b))

      case SList(SSymbol("*"),a,b) =>
       TimesExp(from(a),from(b))

      case SList(SSymbol("="),a,b) =>
       EqExp(from(a), from(b))

      
     // Lists:
     case SList(SSymbol("cons"),car,cdr) =>
      ConsExp(from(car),from(cdr))

     case SList(SSymbol("car"),arg) =>
      CarExp(from(arg))

     case SList(SSymbol("cdr"),arg) =>
      CdrExp(from(arg))

     case SList(SSymbol("quote"),SList()) => 
      NullExp()

     case SList(SSymbol("pair?"),arg) => 
      PairPExp(from(arg))

     case SList(SSymbol("null?"),arg) => 
      NullPExp(from(arg))


     // Binding forms:
     case SList(SSymbol("let"),
                bindings,
                body) => {
        val varexps = 
         bindings.toList map {
           case SList(SSymbol(id),exp) =>
             (id,from(exp))
         }
        val (vars,exps) = varexps.unzip
        LetExp(vars,exps,from(body))
      }
    

     case SList(SSymbol("letrec"),
                SList(SList(SSymbol(fun),lambda)),
                body) =>
       LetRecExp(fun,from(lambda),from(body))
    

     // Set!
     case SList(SSymbol("set!"),SSymbol(id),exp) =>
       SetExp(id,from(exp))

     // Application
     case SCons(fun,args) => 
       AppExp(from(fun), args.toList map from)

    } 
  }
}




/* Core lambda calculus forms. */
case class RefExp(val id : String) extends Exp {

  def isFinal = true

  override def toString = id
}

case class LambdaExp(val params : List[String], val body : Exp) extends Exp {

  def isFinal = true

  override def toString = 
    "(lambda (" +params.mkString(" ")+ ") " +body+ ")"

}

case class AppExp(val fun : Exp, args : List[Exp]) extends Exp {

  def isFinal = false

  override def toString = 
    "(" +(fun :: args).mkString(" ")+ ")"
}


/* Scheme forms. */

// Conditionals:
case class BoolExp(val value : Boolean) extends Exp {

  def isFinal = true

}


case class IfExp(val cond : Exp, 
                 val ifTrue : Exp, 
                 val ifFalse : Exp) extends Exp {
  def isFinal = false                   

}

case class AndExp(val cond1 : Exp, val cond2 : Exp) extends Exp {
  def isFinal = false                   
}

case class OrExp(val cond1 : Exp, val cond2 : Exp) extends Exp {
  def isFinal = false                   
}



// Numerics:
case class IntExp(val value : Int) extends Exp {
  def isFinal = true
}


case class ZeroPExp(val test : Exp) extends Exp {
  def isFinal = false
}

case class SubExp(val exp1 : Exp, val exp2 : Exp) extends Exp {
  def isFinal = false
}

case class EqExp(val exp1 : Exp, val exp2 : Exp) extends Exp {
  def isFinal = false
}

case class PlusExp(val exp1 : Exp, val exp2 : Exp) extends Exp {
  def isFinal = false
}

case class TimesExp(val exp1 : Exp, val exp2 : Exp) extends Exp {
  def isFinal = false
}


// Binding and recursion:
case class LetExp(val vars : List[String],
                  val exps : List[Exp],
                  val body : Exp) extends Exp {
  def isFinal = false
}                    

case class LetRecExp(val fun : String,
                     val lambda : Exp,



                     val body : Exp) extends Exp {
  def isFinal = false                       
}                       



// Lists:
case class ConsExp(val car : Exp, val cdr : Exp) extends Exp {
  def isFinal = false
}

case class CarExp(val arg : Exp) extends Exp {
  def isFinal = false
}
 
case class CdrExp(val arg : Exp) extends Exp {
  def isFinal = false
}

case class PairPExp(val arg : Exp) extends Exp {
  def isFinal = false
}

case class NullPExp(val arg : Exp) extends Exp {
  def isFinal = false
}

case class NullExp() extends Exp {
  def isFinal = true
}



// Mutation:
case class SetExp(val id : String, val exp : Exp) extends Exp {
  def isFinal = false
}



// Extractors:
object Let1Exp {

  def unapply (exp : Exp) : Option[(String,Exp,Exp)] = exp match {
    case LetExp(List(v),List(e),body) => Some((v,e,body))
    case _ => None
  }
}








/**
 An abstract CESK-style interpreter for A-Normal Form.
 */

abstract class Addr 

case class BindAddr(val id : String) extends Addr

case class KontAddr(val exp : Exp) extends Addr

case object HaltAddr extends Addr


object CESKTypes {

  type Env = Map[String,Addr]

  type AValues = Set[AValue]

  type Store = Map[Addr,AValues]
}

import CESKTypes._

abstract class AValue {
  def isFalse : Boolean

  def isKont : Boolean

  def isProc : Boolean
}

case class AClosure(val lam : LambdaExp, val env : Env) extends AValue {
  def isFalse = false
  def isKont = false

  def isProc = true
}

case object AHalt extends AValue {
  def isFalse = false
  def isKont = false

  def isProc = false
}

case object AInt extends AValue {
  def isFalse = false
  def isKont = false

  def isProc = false
}

case class ABool(val value : Boolean) extends AValue {
  def isFalse = !value
  def isKont = false

  def isProc = false
}


sealed abstract class Kont extends AValue

case class LetK(val id : String, val body : Exp, val env : Env, val akont : Addr)
 extends Kont {
   def isKont = true

   def isProc = false
   def isFalse = false
}


/*
case class CondF(val ifTrue : Exp, val ifFalse : Exp, val env : Env)
 extends Frame
*/


case class State(
 val exp : Exp,
 val env : Env,
 val store : Store,
 val akont : Addr) {

  def isFinal = exp.isFinal && (akont == HaltAddr)
}




object AbstractCESKEvaluator {

  // alloc : returns a fresh address
  def alloc (id : String) : Addr = {
    BindAddr(id)
    //println("id is" + id)
  }

  def alloc (exp : Exp) : Addr = {
    KontAddr(exp)
  }

  // eval : inject exp into a state, step to completion

  def eval (exp : Exp) : Set[State] = {

    val init = inject(exp) 

    var seen : Set[State] = Set()

    var todo : List[State] = List(init)

    while (!todo.isEmpty) {

      val curr = todo.head
      todo = todo.tail

      if (!seen.contains(curr)) {
        val succs = next(curr)
        todo = succs.toList ++ todo

        seen = seen + curr
      }
    }

    seen
   }


  def explore (exp : Exp) : Map[State,Set[State]] = {

    val init = inject(exp) 

    var seen : Set[State] = Set()

    var todo : List[State] = List(init)

    var map : Map[State,Set[State]] = Map()

    while (!todo.isEmpty) {

      val curr = todo.head
      todo = todo.tail

      if (!seen.contains(curr)) {
        val succs = next(curr)
        todo = succs.toList ++ todo

        seen = seen + curr
        map = map + ((curr,succs))
      }
    }

    map
  }

  def query(id : String, states : Set[State]) : Set[Exp] = {

    var lambdas : Set[Exp] = Set() 

    for (s <- states) {
      (s.env get id) match {
        case Some(a) => {
          val values = s.store(a)
          val lams = for (v <- values if v.isProc) yield {
            v match { case AClosure(lam,_) => lam }
          }


          lambdas = lambdas ++ lams
        }
        case None => {}
      }
    }
    lambdas
  }


   private var maxMark : Int = 0

   private var marks : Map[State,Int] = Map()

   private def markOfBeast(state : State) : Int = {
     (marks get state) match {
       case Some(l) => l
       case None => { maxMark += 1 ; marks = marks + ((state,maxMark)) ; maxMark }
     }
   }

   def printGraph(graph : Map[State,Set[State]]) {
     println("digraph { ") 
     for ((node,succs) <- graph) {
       for (succ <- succs) {
         println(markOfBeast(node) + " -> " + markOfBeast(succ) + ";") ;
       }
     }
     println("}")
   }




  // a : takes an atomic expression, an env and a store 
  //     returns the value of the expression in that context
  def a (aexp : Exp, env : Env, store : Store) : AValues = aexp match {
    case BoolExp(value) => Set(ABool(value))
    case IntExp(n) => Set(AInt)
    case RefExp(id) => {  if (id == "halt") Set(AHalt) else store(env(id))}
    case lam @ LambdaExp(_,_) => Set(AClosure(lam, env))
  }

  // inject : converts an expression into an initial state
  def inject (exp : Exp) : State = 
    State(exp, Map(), Map(HaltAddr -> Set()), HaltAddr)

  def bucket(store : Store, addr : Addr, values : AValues) : Store = {
    (store get addr) match {
      case Some (existing) => store + ((addr,values ++ existing))
      case None => store + ((addr,values))
    }
  }


  def buquettes(store : Store, avals : List[(Addr,AValues)]) : Store = {
    var tmp = store
    for ((a,v) <- avals) {
      tmp = bucket(tmp, a, v)
    }
    tmp
  }

  // next : takes a state to the next state
  def next (state : State) : Set[State] = state match {
    case State(exp, env, store, akont) if exp.isFinal => {
       val values = store(akont)
       val value = a (exp,env,store)
       for (kont <- values if kont.isKont) yield {
        applyKont(kont, value, store) 
       }
    }
     
    case State(Let1Exp(v,arg,body),env,store,akont) => {
      val akont1 = alloc(arg)
      val kont = LetK(v,body,env,akont)
      Set(State(arg,env,bucket(store,akont1,Set(kont)),akont1))
    }

    case State(AppExp(f,args),env,store,akont) => {
      val argvals = args map (a (_, env, store))
      val funvals = a (f, env, store)
      for (funval <- funvals if funval.isProc) yield {
       apply(funval,argvals,store,akont)
      }
    }

    case State(IfExp(aexp,ifTrue,ifFalse),env,store,akont) => {
      Set(State(ifFalse, env, store, akont),
          State(ifTrue, env, store, akont))
    }

    case State(LetRecExp(f,lam,body),env,store,akont) => {
      val addr = alloc(f)
      val env1 = env + ((f,addr))
      val clo = a(lam,env1,store)
      val store1 = bucket(store, addr,clo)
      Set(State(body,env1,store1,akont))
    }

 
  }

  def apply (proc : AValue, args : List[AValues],
             store : Store, akont : Addr) : State = proc match {
    case AClosure(LambdaExp(params,body),env) => {
      val addrs = params map (v => alloc(v))
      val varaddrs = params zip addrs
      val addrvals = addrs zip args
      val env1 = env ++ varaddrs
      val store1 = buquettes(store, addrvals)
      State(body, env1, store1, akont)
    }
  }

  def applyKont (kont : AValue, retval : AValues, store : Store) : State = kont match {

    case LetK(v,body,env, arest) => {
      val addr = alloc(v)
      val env1 = env + ((v,addr))
      val store1 = bucket(store, addr,retval)
      State(body, env1, store1, arest) 
    }

    case _ => throw new ImpossibleException()
  }

  def main (args : Array[String]) {

    // Should read the file in args(1)
   // println ("in is \n")

    //val in = scala.io.Source.stdin.mkString 
    val in = scala.io.Source.fromFile(args(0))
    val inp = in.getLines mkString "\n" 

    val sexp = SExp.from(inp)  

    val exp = Exp.from(sexp)

    val ans = eval(exp)

  //  for(s<-ans)
  //   println("state is" + s )
     
//    val coded = "c"
    for (lam <- query(args(1) ,ans)) 
      println(lam)
    
    /*
    for ((c,s) <- ans) {
      println("from: " + c)

      for (t <- s) {
        println("to: " + t)
      }

      println()
    }
    

    // printGraph(ans)

    for (lam <- query(args(0)),ans))) {
      println(lam)
    } */
  }
}








