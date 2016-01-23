// These problems are extracted from "Programming Scala", by
// Dean Wampler.

object Problem1 {
    def main( args:Array[String] ) = {
      // Reverse each element of the args array and print out the
      // result. Note that the String class has a 'reverse' method.
      //
      // For example:
      //
      // scala Problem1 foo bar baz
      // oof rab zab

      //for each arg, reverse str and print 
      for (str <- args) yield print(str.reverse + ' ')
      //add new line 
      print('\n')
    }
}

object Problem2 {
  // A binary tree node.  The field `ord` is declared with
  // `var`, so it is mutable.  For example, you can do:
  //
  // val n = Node(...)
  // n.ord = (1 -> 2)
  //
  // Because we introduced the `var`, you may modify _this_ `var`.
  // You may not introduce any other `var`s.
  case class Node(var ord:(Int,Int), 
                  left:Option[Node],
                  right:Option[Node])

  def main( args:Array[String] ) = {
    // example tree
    val tree = Node( (-1,-1), 
      None,
      Some(Node( (-1,-1),
                Some(Node( (-1,-1), None, None )),
                Some(Node( (-1,-1), Some(Node( (-1,-1), None, None )), None ))
      ))
    )
    
    // set the tree nodes' labels and print the tree. note that case
    // classes are automatically given a toString method, so we don't
    // need to define our own.  Your solution must be general, in that
    // it can work with arbitrary trees.
    order( tree )
    println( tree )

    // For example:
    //
    // scala Problem2
    // Node((0,4),None,Some(Node((1,3),Some(Node((2,0),None,None)),Some(Node((3,2),Some(Node((4,1),None,None)),None)))))
  }

  def order( node:Node ) {
    // use a nested method inside this method as a helper function to
    // traverse the given tree and set each Node's 'ord' field to the
    // tuple '(preorder, postorder)', where 'preorder' is the Node's
    // preorder label and 'postorder' is the Node's postorder
    // label. For consistent numbers, visit left children before right
    // children. Labels should start at 0 (i.e., the root node's
    // preorder label should be 0).
    def labelpre(node: Option[Node], preorder: Int): Int = { // using Option here to allow pass None
        node match{
          case None => preorder
          case Some(node) => {
            //do preorder here 
            node.ord = (preorder, node.ord._2)
            //then left, get labeled value  
            val ac = labelpre(node.left, preorder+1)
            //then right 
            labelpre(node.right, ac)
          }
        }
    }

    def labelpost(node: Option[Node], postorder: Int): Int = { //using Option here to allow pass None
      node match {
          case None => postorder
          case Some(node) => {
            //fitst do left traversal, 
            val ac = labelpost(node.left, postorder)
            //the  do right traversal, and use ac from left traversal 
            val acc = labelpost(node.right, ac)
            
            //do increment here 
            node.ord = (node.ord._1, acc)  
            acc + 1
          }
      }
    }
    //use Some here--> node could be Null. 
    //variables that could be null have a type that is distinct
    // from variables that cannot be null
    labelpre(Some(node), 0)

    labelpost(Some(node), 0)

    // As a hint, you'll need to use recursion here.  The nested
    // method should have an auxilliary parameter, representing the
    // currently available ID.  The nested method should return the
    // next available ID.  This is equivalent to an approach of
    // having a mutable variable elsewhere and incrementing it
    // each time we need a new ID, which is likely a more obvious
    // solution if you're coming from an imperative background.  This
    // is equivalent, because the mutable variable sets up an implicit
    // data dependency between recursive calls, whereas with functional
    // purity we must make this data dependency explicit.
  }
}

object Problem3 {
  def main( args:Array[String] ) = {
    val list = args.toList

    // Use the foldLeft method of list to print the following:
    //
    // 1. the largest element in args (using string comparison)
    println(list.foldLeft("")((large:String, small:String) => if(large > small) large else small))


    // 2. args with no duplicate elements
    val newlist = list.foldLeft(List[String]())(  
      (inlist: List[String], str: String )=> 
        if(inlist.contains(str)) inlist //if contain, return inlist
        else str::inlist 
      )  // :: append to list 
    println(newlist.reverse)


    // 3. a run-length-encoded version of args
    val encodelist = list.foldLeft(List[(String, Int)]())(
      //foldLeft ==> keep adding to the empty List here 
      (inlist: List[(String, Int)], str: String) => 
      if(inlist.size > 0){
        if(inlist.head._1 == str){
          val new_tuple = (str, inlist.head._2+1) 
          new_tuple::inlist.drop(1)
        }
        else {
          val new_tuple = (str, 1)
          new_tuple:: inlist
        }
      }
      else {
        val new_tuple = (str, 1)
        new_tuple:: inlist
      }

    )
    println(encodelist.reverse)
    //if list.head exist current string, change the value inside head.tuple 

    //if list.head doesnt exit or does not contain current str, then append
    //new to the heead. 


    // NOTES
    //
    // If the initial value given to foldLeft is an empty List you
    // need to explicitly give the type of the List, e.g., List[Int]()
    // or List[String](), otherwise the compiler won't be able to
    // figure out the types.
    //
    // To determine if a string `s1` is greater than another string `s2`,
    // you can use `>` like so: `s1 > s2`.  The `compareTo` method on
    // `String` can also be used.
    // 
    // You may use reverse as part of your solution.
    //
    // For run-length-encoding specifics, see
    // http://en.wikipedia.org/wiki/Run_length_encoding.

    // For example:
    //
    // scala Problem3 foo bar bar baz moo moo moo cow
    // moo
    // List(foo, bar, baz, moo, cow)
    // List((foo,1), (bar,2), (baz,1), (moo,3), (cow,1))
  }
}
