package scurry.test

import scurry.rts._
import scurry.lib._

object SmallExamples {
  def main(args: Array[String]) = {
//     triple_not
//     single_isEmpty
//     nested_fail
//     projection
//     calculation
//     comparison
//     small_fibo
//     coin_tossing
//     small_choice
//     weird_choice
//     hnf_test
//     nondet_plus
//     shared_not
//     singleton_coin
//     shared_coin
    insert123
//     list_consumption /* big hydra */
//     large_fibo /* takes 20 seconds */
//     project_cycle /* cannot print hydra */
//     failing_cycle /* needs bfs */
//     infinite_choice /* needs cycle check (e.g. using fprints) */
  }

  private def eval_print(exp: Expression) {
    println(exp)
    // use LifoQ or depth-first evaluation of redexes, FifoQ for breadth-first
    val results = new SequentialEvaluator(new LifoQ())
    results.init(exp)
    while (Console.readLine != "n" && results.hasNext) {
      println(results.next)
    }
  }

  private def triple_not =
    eval_print(Bool.not(Bool.not(Bool.not(Bool.False))))

  private def single_isEmpty =
    eval_print(Lists.Cons(Bool.not(Lists.isEmpty(Lists.Empty)),Lists.Empty))

  private def project_cycle =
    eval_print(Lists.head(Lists.tail(Lists.repeat(Bool.True))))

  private def shared_not = {
    val x = Bool.not(Bool.False)
    eval_print(Lists.Cons(x,Lists.Cons(x,Lists.Empty)))
  }

  private def nested_fail =
    eval_print(Lists.Cons(Lists.head(Lists.Empty),Lists.Empty))

  // needs bfs
  private def failing_cycle = {
    val l = Lists.Cons(Lists.head(Lists.Empty),null)
    l.args.update(1,l)
    eval_print(l)
  }

  private def projection =
    eval_print(
      Bool.not(Lists.head(Lists.Cons(Bool.not(Bool.True),Exp.failure))))

  private def calculation =
    eval_print(
      Integer.leq(Integer.plus(Integer.value(35),Integer.value(6)),
                  Integer.times(Integer.value(6),Integer.value(7))))

  private def comparison =
    eval_print(
      Bool.eq(Lists.Cons(Integer.value(2),Lists.Empty),
              Lists.Cons(Integer.value(3),Lists.Empty)))

  private def small_fibo =
    eval_print(Integer.fibo(Integer.value(4)))

  private def large_fibo =
    eval_print(Integer.fibo(Integer.value(27)))

  private def list_consumption =
    eval_print(
      Lists.length(
        Lists.drop(
          Integer.value(300),
          Lists.enumFromTo(Integer.value(1),Integer.value(1300)))))

  private def coin_tossing =
    eval_print(Exp.bin_choice(Bool.True,Bool.False))

  private def small_choice =
    eval_print(Bool.not(Exp.bin_choice(Bool.True,Bool.False)))

  private def weird_choice =
    eval_print(
      Bool.not(Bool.not(Exp.bin_choice(
        Bool.not(Bool.True),
        Bool.not(Bool.False)))))

  private def infinite_choice = {
    val x: Expression = Exp.bin_choice(Bool.True,null)
    x.args.update(1,x)
    eval_print(x)
  }

  private def singleton_coin = {
    val coin = Exp.bin_choice(Bool.True,Bool.False)
    eval_print(Lists.Cons(coin,Lists.Empty))
  }

  private def nondet_plus = {
    val one2  = Exp.bin_choice(Integer.value(1),Integer.value(2))
    val ten20 = Exp.bin_choice(Integer.value(10),Integer.value(20))
    eval_print(Integer.plus(one2,ten20))
  }

  private def hnf_test =
    eval_print(Bool.not(Exp.bin_choice(Bool.True,Bool.not(Bool.True))))

  private def shared_coin = {
    val coin = Exp.bin_choice(Bool.True,Bool.False)
    eval_print(Lists.Cons(coin,Lists.Cons(coin,Lists.Empty)))
  }

  private def insert123 = {
    val two3 = Lists.Cons(Integer.value(2),  Lists.Empty)
//                           Lists.Cons(Integer.value(3),Lists.Empty))
    eval_print(Lists.insert(Integer.value(1),two3))
  }
}
