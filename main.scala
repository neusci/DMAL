import scala.collection.immutable.{Map => _, Set => _, _}  // Vector, List, Range
import scala.collection.mutable._  // ArrayBuffer, StringBuilder, HashMap or HashSet
import scala.collection.JavaConversions._  // Java ops
import scala.concurrent.duration._ // actor msg duration
import scala.io.Source._  // file ops
import scala.language._  // postfixOps
import scala.math._  // the math
import scala.reflect.runtime.universe._  // reflection
import scala.runtime._ // runtime things
import scala.sys.process._  // work with shell
import scala.util.continuations._  // cont...
import scala.util.control.Breaks._  // break loops
import scala.util.matching.Regex._  // regex
import scala.util.parsing._  // parsing toolbox
import scala.util.Random._  // random numbers
import scala.util.Sorting._  // quickSort etc
import akka.actor._ // use akka actor
import akka.event._
import akka.dispatch._
import akka.pattern._
import akka.util._
import java.io.File
import java.io.PrintWriter


case class Init()
case class Dest()
case class stim(from: Int, curr:Double, time:Double) // from which neuron, with what current, at what time
case class Done()
case class Fail()


class iLN(id: Int) extends Actor {
  val time_step:Double = 0.01 // ms
  var time_cur:Double = 0.0 // temporal reference system (TRS) current time (ms)
  //  ...
  val C_m:Double = 0.13 // membrance capacitance (uF)
  val E_K:Double = -72.0 // K+ reversal potential (mV)
  val E_Na:Double = 55.0 // Na+ reversal potential (mV)
  val E_L:Double = -75.0 // L reversal potential (mV)
  val g_K_iLN:Double = 11.76 // I_K maximal conductance (mS) for iLN
  val g_Na:Double = 1.96 // I_Na maximal conductance (mS)
  val g_L:Double = 0.01372 // L maximal conductance (mS)
  val th_w:Double = -44.0 // half activation for b (mV)
  val th_m:Double = -32.0 // half activation for m (mV)
  val k_w:Double = -28.34 // activation sensitivity for b (mV)
  val k_m:Double = -30.92 // activation sensitivity for m (mV)
  val k_tau:Double = 28.34 // activation sensitivity for tau (mV)
  val r_w:Double = 0.1 // basal rate for b (1/ms)
  val E_Ca:Double = 124.0 // Ca^{2+} reversal potential (mV)
  val th_Ca:Double = -10.0 // half activation for n (mV)
  val g_Ca:Double = 20.0 // I_Ca maximal conductance (mS)
  val g_KCa:Double = 0.03 // I_KCa maximal conductance (mS)
  val al_Ca:Double = 0.00005 // Ca conversion rate
  val r:Double = 0.01 // Ca removal rate
  val k_Ca:Double = -8.467 // activation sensitivity for n (mV)
  val g_K_PN:Double = 10.78 // I_K maximal conductance for PNs (mS)
  //  ...
  val data_writer = new PrintWriter(new File("iln"+id+".txt"))
  //  ...
  var V:Double = 0.0
  var w:Double = 0.0
  var V_next:Double = 0.0
  var w_next:Double = 0.0
  //  ...
  var I_app:Double = 0.0 // the applied current (nA)
  //  ...
  def dfdt_V() = {(-I_Na()-I_K()-I_L()+I_app)/C_m}
  def dfdt_w() = {(w_inf(V)-w)/tau_w(V)}
  def I_Na() = {g_Na*pow(m_inf(V),3)*(1-w)*(V-E_Na)} // inward sodium current
  def I_K() = {g_K_iLN*pow(w,4)*(V-E_K)} // next outward potassium current
  def I_L() = {g_L*(V-E_L)} // the next leak current
  def m_inf(v:Double):Double = {1/(1+ pow(al_m(v),2))}
  def w_inf(v:Double):Double = {1/(1+ pow(al_w(v),2))}
  def al_m(v:Double):Double = {exp((v-th_m)/k_m)}
  def al_w(v:Double):Double = {exp((v-th_w)/k_w)}
  def tau_w(v:Double):Double = {1/(r_w*cosh( (V-th_w)/k_tau ))}
  // ...
  def iterate() = {
    V_next = V + dfdt_V()*time_step
    w_next = w + dfdt_w()*time_step
    time_cur += time_step // update time.
    V = V_next
    w = w_next
    printf("iLN: %3d ; time (mS): %3.3f ; vol. (mV): %3.6f\n", id, time_cur, V)
  }
  // ...
  def receive() = {
    case Init() =>
      println("iLN["+id+"] is ready.")
      printf("iLN: %3d ; time (mS): %3.3f ; vol. (mV): %3.6f\n", id, time_cur, V)
      sender ! Done()
    case stim(i,x,t) =>
      I_app = x
      iterate()
      data_writer.println(V)
      sender ! Done()
    case Dest() =>
      println("iLN["+id+"] is done.")
      data_writer.close()
      sender ! Done()
    case _ =>
      sender ! Fail()
  }
}


class eLN(id: Int) extends Actor {
  val time_step:Double = 0.01 // ms
  var time_cur:Double = 0.0 // temporal reference system (TRS) current time (ms)
  //  ...
  val C_m:Double = 0.13 // membrance capacitance (uF)
  val E_K:Double = -72.0 // K+ reversal potential (mV)
  val E_Na:Double = 55.0 // Na+ reversal potential (mV)
  val E_L:Double = -75.0 // L reversal potential (mV)
  val g_K_iLN:Double = 11.76 // I_K maximal conductance (mS) for iLN
  val g_Na:Double = 1.96 // I_Na maximal conductance (mS)
  val g_L:Double = 0.01372 // L maximal conductance (mS)
  val th_w:Double = -44.0 // half activation for b (mV)
  val th_m:Double = -32.0 // half activation for m (mV)
  val k_w:Double = -28.34 // activation sensitivity for b (mV)
  val k_m:Double = -30.92 // activation sensitivity for m (mV)
  val k_tau:Double = 28.34 // activation sensitivity for tau (mV)
  val r_w:Double = 0.1 // basal rate for b (1/ms)
  val E_Ca:Double = 124.0 // Ca^{2+} reversal potential (mV)
  val th_Ca:Double = -10.0 // half activation for n (mV)
  val g_Ca:Double = 20.0 // I_Ca maximal conductance (mS)
  val g_KCa:Double = 0.03 // I_KCa maximal conductance (mS)
  val al_Ca:Double = 0.00005 // Ca conversion rate
  val r:Double = 0.01 // Ca removal rate
  val k_Ca:Double = -8.467 // activation sensitivity for n (mV)
  val g_K_PN:Double = 10.78 // I_K maximal conductance for PNs (mS)
  //  ...
  val data_writer = new PrintWriter(new File("eln"+id+".txt"))
  //  ...
  var V:Double = 0.0
  var w:Double = 0.0
  var F_Ca:Double = 0.0
  var V_next:Double = 0.0
  var w_next:Double = 0.0
  var F_Ca_next:Double = 0.0
  //  ...
  var I_app:Double = 0.0 // the applied current (nA)
  //  ...
  def dfdt_V() = {(-I_Na()-I_K()-I_Ca()-I_KCa()-I_L()+I_app)/C_m}
  def dfdt_w() = {(w_inf(V)-w)/tau_w(V)}
  def I_Na() = {g_Na*pow(m_inf(V),3)*(1-w)*(V-E_Na)} // inward sodium current
  def I_K() = {g_K_iLN*pow(w,4)*(V-E_K)} // next outward potassium current
  def I_L() = {g_L*(V-E_L)} // the next leak current
  def I_Ca() = {g_Ca * pow(n_inf(1-w), 2) * (V-E_Ca)}
  def I_KCa() = {g_KCa * (F_Ca/(0.04+F_Ca)) * (V-E_K)}
  def m_inf(v:Double):Double = { 1 / (1 + pow(al_m(v), 2)) }
  def n_inf(v:Double):Double = { 1 / (1 + exp( (v-th_Ca) / k_Ca )) }
  def w_inf(v:Double):Double = { 1 / (1 + pow(al_w(v), 2)) }
  def al_m(v:Double):Double = { exp((v-th_m) / k_m) }
  def al_w(v:Double):Double = { exp((v-th_w) / k_w) }
  def tau_w(v:Double):Double = {1/(r_w*cosh( (V-th_w)/k_tau ))}
  def dfdt_FCa() = {-al_Ca*I_Ca() - r*F_Ca}
  // ...
  def iterate() = {
    F_Ca_next = F_Ca + dfdt_FCa()*time_step
    V_next = V + dfdt_V()*time_step
    w_next = w + dfdt_w()*time_step
    time_cur += time_step // update time...
    F_Ca = F_Ca_next
    V = V_next
    w = w_next
    printf("eLN: %3d ; time (mS): %3.3f ; vol. (mV): %3.6f\n", id, time_cur, V)
  }
  // ...
  def receive() = {
    case Init() =>
      println("eLN["+id+"] is ready.")
      printf("eLN: %3d ; time (mS): %3.3f ; vol. (mV): %3.6f\n", id, time_cur, V)
      sender ! Done()
    case stim(i,x,t) =>
      I_app = x
      iterate()
      data_writer.println(V)
      sender ! Done()
    case Dest() =>
      println("eLN["+id+"] is done.")
      data_writer.close()
      sender ! Done()
    case _ =>
      sender ! Fail()
  }
}


object main extends App {
  val stagent = ActorSystem("agent-on-stage")
  val iLN_test = stagent.actorOf(Props( new iLN(1) ))
  val eLN_test = stagent.actorOf(Props( new eLN(1) ))
  implicit val timeout = Timeout(1 seconds)

/*
 iLN_test ? Init() // run 10S:
 for (i <- 1 to 100000) {
   iLN_test ? stim(0, 0.1, 0)
   Thread sleep 1
 }
 iLN_test ? Dest()
*/

  eLN_test ? Init() // run 10S:
  for (i <- 1 to 1000000) {
    eLN_test ? stim(0, 0.2, 0)
    Thread sleep 1
  }
  eLN_test ? Dest()

  Thread sleep 10000
  stagent shutdown
}
