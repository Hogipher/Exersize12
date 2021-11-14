@main def main: Unit =
  object ex12 extends org.scalatest.FunSuite {

    // A Parser[A] takes a string and a starting index
    // and tries to parse a substring of the string beginning at
    // that starting index. If it fails, it returns None. If it
    // succeeds, it returns Some containing a tuple of a result of
    // type A (often an abstract syntax tree) and the end index,
    // which is one position after the last character used.
    type Parser[A] = (String, Int) => Option[(A,Int)]


    //////////////////// FUNCTIONS FROM HW12 ////////////////////

    // functions parse, epsilon any, char, alt, concat, and map
    // from ex11.scala have been placed here
    // (update solutions !!)

    def parse[A](p: Parser[A], str: String): Option[A] = {
      p(str,0).filter(tup => tup._2 == str.length).map(tup => tup._1)
    }

    def epsilon[A](a: A): Parser[A] = { (str,i) =>
      Some((a,i))
    }

    def any: Parser[Char] = { (str,i) =>
      if (i< str.length) Some((str(i),i+1))
      else None
    }

    def char(ch: Char): Parser[Char] = { (str,i) =>
      any(str,i).filter(_._1 == ch)
    }

    def alt[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = { (str,i) =>
      p1(str,i) orElse p2(str,i)
    }

    def concat[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = { (str,i) =>
      p1(str,i) match {
        case None => None
        case Some((a,i2)) => p2(str,i2).map(tup => ((a,tup._1),tup._2))
      }
    }

    def map[A,B](p: Parser[A], f: A => B): Parser[B] = { (str,i) =>
      p(str,i).map(tup => (f(tup._1),tup._2))
    }

    ////////////////// END FUNCTIONS FROM HW12 //////////////////

    // The new functions from HW13 start here...

    def fail: Parser[Nothing] = { (str,i) =>
      None
    }

    def filter[A](p: Parser[A], pred: A => Boolean): Parser[A] = { (str,i) =>
      val pOpt:Option[(A,Int)] = p(str,i)
      if (pOpt == None || !pred(pOpt.get._1)) None else pOpt
      //p(str,i).filter(tup=>pred(tup._1))  CLASS ANSWER
    }

    def flatMap[A,B](p: Parser[A], f: A => Parser[B]): Parser[B] = { (str,i) =>
      p(str,i) match {
        case pResult@Some((_,i2)) => pResult.flatMap(tup=> (f(tup._1))(str,i2))
        case None => None
      }
      //p(str,i).flatMap(tup=>f(tup._1)(str,tup._2)) CLASS ANSWER
    }

    // The remaining functions should all be derived parsers.
    // They should NEVER refer to (str,i)

    def char2(ch: Char): Parser[Char] =
      filter(any,nxtchar => nxtchar == ch)
    //filter(any,_ == ch)

    def optional[A](p: Parser[A]): Parser[Option[A]] =
      alt(map[A,Option[A]](p,a=>Some(a)),epsilon(None))
    //alt(map(p,(a:A)=>Some(a)),epsilon(None)) CLASS ANSWER

    def star[A](p: Parser[A]): Parser[List[A]] =
      alt(map[(A,List[A]),List[A]](concat(p,star(p)),tup=>tup._1::tup._2),epsilon(Nil))

    // And the last three functions should all be derived parsers that
    // use the flatMap function defined above.

    def concat2[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
      flatMap[A,(A,B)](p1,a=> flatMap[B,(A,B)](p2,a2=> epsilon((a,a2))))
    }

    def map2[A,B](p: Parser[A], f: A => B): Parser[B] =
      flatMap[A,B](p, a=> epsilon(f(a)))

    def filter2[A](p: Parser[A], pred: A => Boolean): Parser[A] =
      flatMap[A,A](p,a=>if (pred(a)) epsilon(a) else fail)

    // tests

    test("recheck everything from HW12 before the new stuff") {
      // this is just all the tests from HW12 shoved together in one big test

      assertResult(Some(17))(parse(epsilon(17),""))
      assertResult(None)(parse(epsilon(17),"a"))
      assertResult(None)(parse(epsilon(17),"abc"))

      assertResult(Some('a'))(parse(any, "a"))
      assertResult(Some('b'))(parse(any, "b"))
      assertResult(None)(parse(any, ""))
      assertResult(None)(parse(any, "ab"))

      assertResult(Some('a'))(parse(char('a'), "a"))
      assertResult(Some('b'))(parse(char('b'), "b"))
      assertResult(None)(parse(char('a'), "b"))
      assertResult(None)(parse(char('a'), ""))
      assertResult(None)(parse(char('a'), "ab"))

      assertResult(Some('a'))(parse(alt(char('a'), char('b')),"a"))
      assertResult(Some('b'))(parse(alt(char('a'), char('b')),"b"))
      assertResult(None)(parse(alt(char('a'), char('b')),"c"))
      assertResult(None)(parse(alt(char('a'), char('b')),""))
      assertResult(None)(parse(alt(char('a'), char('b')),"ab"))

      assertResult(Some('a'))(parse(alt(char('a'),epsilon('E')),"a"))
      assertResult(Some('E'))(parse(alt(char('a'),epsilon('E')),""))
      assertResult(None)(parse(alt(char('a'),epsilon('E')),"b"))
      assertResult(None)(parse(alt(char('a'),epsilon('E')),"E"))
      assertResult(None)(parse(alt(char('a'),epsilon('E')),"ab"))

      assertResult(Some(('a','b')))(parse(concat(char('a'), char('b')),"ab"))
      assertResult(None)(parse(concat(char('a'), char('b')),""))
      assertResult(None)(parse(concat(char('a'), char('b')),"a"))
      assertResult(None)(parse(concat(char('a'), char('b')),"b"))
      assertResult(None)(parse(concat(char('a'), char('b')),"ba"))
      assertResult(None)(parse(concat(char('a'), char('b')),"aba"))
      assertResult(None)(parse(concat(char('a'), char('b')),"aab"))
      assertResult(None)(parse(concat(char('a'), char('b')),"abb"))
      assertResult(None)(parse(concat(char('a'), char('b')),"abc"))

      assertResult(Some((5,('a','b'))))(
        parse(concat(epsilon(5),concat(char('a'),char('b'))),"ab"))
      assertResult(None)(
        parse(concat(epsilon(5),concat(char('a'),char('b'))),""))

      assertResult(Some((('a','b'),('a','b'))))(
        parse(concat(concat(char('a'), char('b')),concat(char('a'), char('b'))),"abab"))

      val pa = map[Char,Char](char('a'), _.toUpper)
      assertResult(Some('A'))(parse(pa, "a"))
      assertResult(None)(parse(pa, "b"))
      assertResult(None)(parse(pa, ""))
      assertResult(None)(parse(pa, "ab"))

      type CII = (Char, (Int, Int))
      def pexpr: Parser[Int] =
        alt(pdigit,
        alt(map[CII,Int](concat(char('+'), concat(pexpr, pexpr)), r => r._2._1 + r._2._2),
            map[CII,Int](concat(char('*'), concat(pexpr, pexpr)), r => r._2._1 * r._2._2)))

      def pdigit: Parser[Int] = {
        val ds: Seq[Parser[Int]] =
          ('0' to '9').map(d => map[Char,Int](char(d), _ => d - '0'))
        ds.tail.foldLeft[Parser[Int]](ds.head)((p1, p2) => alt(p1,p2))
      }

      def polish(str: String): Option[Int] = parse(pexpr, str)

      assertResult(Some(5))(polish("5"))
      assertResult(Some(7))(polish("7"))
      assertResult(Some(8))(polish("+53"))
      assertResult(Some(16))(polish("+79"))
      assertResult(Some(15))(polish("*53"))
      assertResult(Some(63))(polish("*79"))
      assertResult(Some(14))(polish("+2*34"))
      assertResult(Some(10))(polish("+*234"))
      assertResult(Some(60))(polish("*+23+57"))
      assertResult(None)(polish(""))
      assertResult(None)(polish("12"))
      assertResult(None)(polish("+"))
      assertResult(None)(polish("+1"))
      assertResult(None)(polish("+123"))
      assertResult(None)(polish("*"))
      assertResult(None)(polish("*3"))
      assertResult(None)(polish("*345"))
      assertResult(None)(polish("*+**1234"))
      assertResult(None)(polish("*+*23*1234"))
    }

    // and here are the new tests for HW13
    // activate each test by changing the word 'ignore' to say 'test'

    test("fail") {
      assertResult(None)(parse(fail, ""))
      assertResult(None)(parse(fail, "a"))
      assertResult(None)(parse(fail, "ab"))
      assertResult(Some('a'))(parse(alt(fail,any), "a"))
    }

    test("filter") {
      val p = filter[Char](any, _ > 'c')
      assertResult(None)(parse(p, ""))
      assertResult(None)(parse(p, "a"))
      assertResult(Some('m'))(parse(p, "m"))
      assertResult(None)(parse(p, "mx"))

      assertResult(Some(('N','O')))(
        parse(filter[(Char,Char)](concat(any,any), pair => pair._1 < pair._2), "NO"))
      assertResult(None)(
        parse(filter[(Char,Char)](concat(any,any), pair => pair._1 < pair._2), "ya"))
    }

    test("flatMap") {
      val p = flatMap[Char,Char](any, c => char(c.toUpper))

      assertResult(None)(parse(p, ""))
      assertResult(None)(parse(p, "a"))
      assertResult(None)(parse(p, "5"))
      assertResult(None)(parse(p, "a5"))
      assertResult(None)(parse(p, "aa"))
      assertResult(None)(parse(p, "bA"))
      assertResult(Some('A'))(parse(p, "aA"))
      assertResult(Some('Z'))(parse(p, "zZ"))
      assertResult(None)(parse(p, "aAa"))
      assertResult(None)(parse(p, "aAA"))
      assertResult(None)(parse(p, "aAb"))
    }

    test("char2") {
      assertResult(Some('a'))(parse(char2('a'), "a"))
      assertResult(Some('b'))(parse(char2('b'), "b"))
      assertResult(None)(parse(char2('a'), "b"))
      assertResult(None)(parse(char2('a'), ""))
      assertResult(None)(parse(char2('a'), "ab"))
    }

    test("optional") {
      val p = optional(char2('a'))
      assertResult(Some(None))(parse(p, ""))
      assertResult(Some(Some('a')))(parse(p, "a"))
      assertResult(None)(parse(p, "aa"))

      val pp = concat(p,p)
      assertResult(Some((None,None)))(parse(pp, ""))
      assertResult(Some((Some('a'),None)))(parse(pp, "a"))
      assertResult(Some((Some('a'),Some('a'))))(parse(pp, "aa"))
      assertResult(None)(parse(pp, "aaa"))
    }

    test("star") {
      val p = star[Char](filter[Char](any, _.isLetter))
      assertResult(Some(Nil))(parse(p,""))
      assertResult(Some(List('x')))(parse(p,"x"))
      assertResult(Some(List('x','y')))(parse(p,"xy"))
      assertResult(Some(List('x','y','z')))(parse(p,"xyz"))
      assertResult(None)(parse(p,"5"))
      assertResult(None)(parse(p,"ab9"))
    }

    test("concat2") {
      val p = concat2(char('a'), char('b'))
      assertResult(Some(('a','b')))(parse(p,"ab"))
      assertResult(None)(parse(p,""))
      assertResult(None)(parse(p,"a"))
      assertResult(None)(parse(p,"b"))
      assertResult(None)(parse(p,"ba"))
      assertResult(None)(parse(p,"aba"))
      assertResult(None)(parse(p,"aab"))
      assertResult(None)(parse(p,"abb"))
      assertResult(None)(parse(p,"abc"))

      val p2 = concat2(epsilon(5),p)
      assertResult(Some((5,('a','b'))))(parse(p2,"ab"))
      assertResult(None)(parse(p2,""))

      assertResult(Some((('a','b'),('a','b'))))(parse(concat(p,p),"abab"))
    }

    test("map2") {
      val pa = map2[Char,Char](char('a'), _.toUpper)
      assertResult(Some('A'))(parse(pa, "a"))
      assertResult(None)(parse(pa, "b"))
      assertResult(None)(parse(pa, ""))
      assertResult(None)(parse(pa, "ab"))
    }

    test("filter2") {
      val p = filter2[Char](any, _ > 'c')
      assertResult(None)(parse(p, ""))
      assertResult(None)(parse(p, "a"))
      assertResult(Some('m'))(parse(p, "m"))
      assertResult(None)(parse(p, "mx"))
    }

    test("Polish (using char2, concat2, map2)") {
      type CII = (Char, (Int, Int))
      def pexpr: Parser[Int] =
        alt(pdigit,
        alt(map2[CII,Int](concat2(char2('+'), concat2(pexpr, pexpr)), r => r._2._1 + r._2._2),
            map2[CII,Int](concat2(char2('*'), concat2(pexpr, pexpr)), r => r._2._1 * r._2._2)))

      def pdigit: Parser[Int] = {
        val ds: Seq[Parser[Int]] =
          ('0' to '9').map(d => map2[Char,Int](char2(d), _ => d - '0'))
        ds.tail.foldLeft[Parser[Int]](ds.head)((p1, p2) => alt(p1,p2))
      }

      def polish(str: String): Option[Int] = parse(pexpr, str)

      assertResult(Some(5))(polish("5"))
      assertResult(Some(7))(polish("7"))
      assertResult(Some(8))(polish("+53"))
      assertResult(Some(16))(polish("+79"))
      assertResult(Some(15))(polish("*53"))
      assertResult(Some(63))(polish("*79"))
      assertResult(Some(14))(polish("+2*34"))
      assertResult(Some(10))(polish("+*234"))
      assertResult(Some(60))(polish("*+23+57"))
      assertResult(None)(polish(""))
      assertResult(None)(polish("12"))
      assertResult(None)(polish("+"))
      assertResult(None)(polish("+1"))
      assertResult(None)(polish("+123"))
      assertResult(None)(polish("*"))
      assertResult(None)(polish("*3"))
      assertResult(None)(polish("*345"))
      assertResult(None)(polish("*+**1234"))
      assertResult(None)(polish("*+*23*1234"))
    }

    def main(args: Array[String]): Unit = {
      println(userName);
      execute()
    }
  }
